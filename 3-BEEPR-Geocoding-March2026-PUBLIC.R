
##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset, all combined
#          GEOCODING
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: Aug 3rd 2026
#
##############################################################################################################
library(dplyr)
library(stringr)

# get occ
combined_df <- read.delim(
  "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-combined-last.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8",
  quote = ""
)
combined_df <- combined_df[rowSums(is.na(combined_df) | combined_df == "") < ncol(combined_df), ]

#Let's define PR bounds
lat_min <- 17.8
lat_max <- 18.6
lon_min <- -68.0
lon_max <- -65.1

#########################################################################################
#                                SUMMARIZING THE TABLE                                  #
#########################################################################################
#the text file created before has an empty line (normal behaviour of write.table), let's count the nrow without this empty line
# Rows with missing latitude or longitude
missing_gps <- combined_df[!complete.cases(combined_df[, c("decimalLatitude", "decimalLongitude")]), ]
nrow(missing_gps) # 752 lines do not have "decimalLatitude" or "decimalLongitude"

# number of families
num_families <- n_distinct(combined_df$family)
print(num_families) #4

# number of genus
num_genus <- n_distinct(combined_df$genus)
print(num_genus) #18

# number of species (specificEpithet)
num_se <- n_distinct(combined_df$specificEpithet[combined_df$specificEpithet != "" & !is.na(combined_df$specificEpithet)])
print(num_se) #38 # without counting the empty line

# number of scientificName (genus and genus+species)
num_sn <- n_distinct(combined_df$scientificName)
print(num_sn) #49

# number total of rows
nrow(combined_df) # 3987

# Count rows with both latitude and longitude present
count_GPSocc <- sum(complete.cases(combined_df[, c("decimalLatitude", "decimalLongitude")]))
print(count_GPSocc) # 3235 occurrences have GPS coordinates

# Count rows with any non-empty values in the columns: verbatimCoordinateSystem, verbatimLongitude, verbatimLatitude, verbatimCoordinates
# (Number of rows with a value in AT LEAST ONE of these four columns)

is_empty <- function(x) is.na(x) | trimws(x) == ""
combined_df %>%
  summarise(
    verbatimCoordinateSystem = sum(!is_empty(verbatimCoordinateSystem)),
    verbatimLongitude        = sum(!is_empty(verbatimLongitude)),
    verbatimLatitude         = sum(!is_empty(verbatimLatitude)),
    verbatimCoordinates      = sum(!is_empty(verbatimCoordinates))
  )

# verbatimCoordinateSystem verbatimLongitude verbatimLatitude verbatimCoordinates
# 1                       55              2331             2331                  74
combined_df %>%
  filter(
    !is_empty(verbatimCoordinateSystem) |
      !is_empty(verbatimLongitude) |
      !is_empty(verbatimLatitude) |
      !is_empty(verbatimCoordinates)
  ) %>%
  nrow()
# 2460 records contain information in at least one of the following fields:
# verbatimCoordinateSystem, verbatimLongitude, verbatimLatitude, or verbatimCoordinates.

combined_df %>%
  filter(
    is_empty(decimalLatitude),
    is_empty(decimalLongitude),
    !(
      is_empty(verbatimCoordinateSystem) &
        is_empty(verbatimLongitude) &
        is_empty(verbatimLatitude) &
        is_empty(verbatimCoordinates)
    )
  ) %>%
  nrow()
# 4 records do not have decimalLatitude/decimalLongitude but contain
# coordinate information in at least one verbatim coordinate field.

names(combined_df)

# Before geocoding, we need to transfer this info in decimalLatitude and decimalLongitude

#################################################
#
#              1. verbatimCoordinateSystem
#
#################################################
# Let's start with formatting the coordinates of verbatimCoordinateSystem
combined_df %>%
  filter(!is_empty(verbatimCoordinateSystem)) %>%
  pull(verbatimCoordinateSystem) %>%
  unique()
# [1] "decimal degrees"                "N17\"94.1302; W066\"95.241'"    "N18\"07.996W066\"49.002'"       "N18*08.626 W066*58.798'"        "N17\"54.496'; W066\"95.26.582'"
# [6] "N17\"97.0569, W 066\"87.4106'"  "N17\"58.816' W067\"10.231'"     "N17*59.705' W066*25.063'"       "N17\"58.632' W 066\"23.096'"    "N17*59.496' W066*26.582'"      
# [11] "N17*58.547 W066*25.063"         "N17\"58.547' W 066\"25.063'"    "N18*08.371 W066*49.230"         "N18*07.4616'W066*38.2638'"      "N17\"58.816; W067\"10.231'"    
# [16] "N17\"57.780' W066\"23.298'"     "N17\"59.737; W066'25.451'"      "N18\"07.724 W066\"53.386'"  
# I see here that one type of value in verbatimCoordinateSystem is "decimal degrees", I will omit this and only work with the other values 
# these other values have non decimal coordinates, let's change that

# -----------------------------
# Parser: extracts deg + decimal-minutes regardless of symbol used
# (", *, or nothing) and separator used (; , or space)
# -----------------------------
parse_ddm <- function(x) {
  
  # Extract 4 numeric groups: lat_deg, lat_min, lon_deg, lon_min
  m <- str_match(
    x,
    "N\\s*(\\d+)[^\\d]+(\\d+\\.\\d+).*?W\\s*(\\d+)[^\\d]+(\\d+\\.\\d+)"
  )
  
  lat_deg <- as.numeric(m[,2])
  lat_min <- as.numeric(m[,3])
  lon_deg <- as.numeric(m[,4])
  lon_min <- as.numeric(m[,5])
  
  lat <- lat_deg + lat_min / 60
  lon <- -(lon_deg + lon_min / 60)   # West = negative
  
  tibble(lat = lat, lon = lon, lat_min_raw = lat_min, lon_min_raw = lon_min)
}

# -----------------------------
# Apply to your unique verbatimCoordinateSystem strings
# -----------------------------
vcs_values <- combined_df %>%
  filter(!is_empty(verbatimCoordinateSystem)) %>%
  filter(verbatimCoordinateSystem != "decimal degrees") %>%   # this one is a label, not a coordinate
  pull(verbatimCoordinateSystem) %>%
  unique()

parsed <- parse_ddm(vcs_values) %>%
  mutate(raw = vcs_values, .before = 1)

# -----------------------------
# Flag invalid results: minutes must be 0-60
# -----------------------------
parsed <- parsed %>%
  mutate(
    valid = lat_min_raw < 60 & lon_min_raw < 60 &
      !is.na(lat) & !is.na(lon) &
      lat >= 17.8 & lat <= 18.6 & lon >= -68.0 & lon <= -65.1
  )

print(parsed, n = Inf)
# A tibble: 17 × 6
# raw                                lat   lon lat_min_raw lon_min_raw valid
# <chr>                            <dbl> <dbl>       <dbl>       <dbl> <lgl>
#   1 "N17\"94.1302; W066\"95.241'"     18.6 -67.6       94.1         95.2 FALSE
# 2 "N18\"07.996W066\"49.002'"        18.1 -66.8        8.00        49.0 TRUE 
# 3 "N18*08.626 W066*58.798'"         18.1 -67.0        8.63        58.8 TRUE 
# 4 "N17\"54.496'; W066\"95.26.582'"  17.9 -67.6       54.5         95.3 FALSE
# 5 "N17\"97.0569, W 066\"87.4106'"   18.6 -67.5       97.1         87.4 FALSE
# 6 "N17\"58.816' W067\"10.231'"      18.0 -67.2       58.8         10.2 TRUE 
# 7 "N17*59.705' W066*25.063'"        18.0 -66.4       59.7         25.1 TRUE 
# 8 "N17\"58.632' W 066\"23.096'"     18.0 -66.4       58.6         23.1 TRUE 
# 9 "N17*59.496' W066*26.582'"        18.0 -66.4       59.5         26.6 TRUE 
# 10 "N17*58.547 W066*25.063"          18.0 -66.4       58.5         25.1 TRUE 
# 11 "N17\"58.547' W 066\"25.063'"     18.0 -66.4       58.5         25.1 TRUE 
# 12 "N18*08.371 W066*49.230"          18.1 -66.8        8.37        49.2 TRUE 
# 13 "N18*07.4616'W066*38.2638'"       18.1 -66.6        7.46        38.3 TRUE 
# 14 "N17\"58.816; W067\"10.231'"      18.0 -67.2       58.8         10.2 TRUE 
# 15 "N17\"57.780' W066\"23.298'"      18.0 -66.4       57.8         23.3 TRUE 
# 16 "N17\"59.737; W066'25.451'"       18.0 -66.4       59.7         25.5 TRUE 
# 17 "N18\"07.724 W066\"53.386'"       18.1 -66.9        7.72        53.4 TRUE 

# 3 coordinates are still invalid (cf. "FALSE"), let's add the coordinates manually: 
# 'N17"94.1302; W066"95.241\'' are the given verbatim coordinates for Ensenada, Guanica, PR, with google, the decimals of the centroids are: 17.970888, -66.932582
# 'N17"54.496\'; W066"95.26.582\'' are the given verbatim coordinates for (SW River) and/or Santa Isabel, PR, with google, the decimals of the centroids are: 17.996152, -66.388746
# 'N17"97.0569, W066"87.4106\'' are the given verbatim coordinates for Guanica Dry Forest Guanica, PR, with google, the decimals of the centroids are: 17.971381, -66.868728

combined_df <- combined_df %>%
  mutate(
    decimalLatitude = decimalLatitude %>%
      trimws() %>%
      str_replace_all("−", "-") %>%
      na_if("") %>%
      as.numeric(),
    
    decimalLongitude = decimalLongitude %>%
      trimws() %>%
      str_replace_all("−", "-") %>%
      na_if("") %>%
      as.numeric()
  ) 
# this code was used to take out a warning :
# Warning message:
#   There was 1 warning in `mutate()`.
# ℹ In argument: `decimalLongitude = as.numeric(decimalLongitude)`.
# Caused by warning:
#   ! NAs introduced by coercion 
# Which the common causes are:
# Unicode minus sign (−) instead of ASCII -
#   Empty strings ("")
# "NA" stored as text
# Extra spaces
# Commas instead of decimal points (e.g. "66,1234")


manual_vcs <- tibble::tribble(
  ~verbatimCoordinateSystem,                  ~parsed_lat, ~parsed_lon,
  "N17\"94.1302; W066\"95.241'",               17.970888,   -66.932582,
  "N17\"54.496'; W066\"95.26.582'",            17.996152,   -66.388746,
  "N17\"97.0569, W 066\"87.4106'",             17.971381,   -66.868728
)

vcs_lookup <- bind_rows(
  parsed %>%
    filter(valid) %>%
    transmute(
      verbatimCoordinateSystem = raw,
      parsed_lat = lat,
      parsed_lon = lon
    ),
  manual_vcs
)

combined_df <- combined_df %>%
  mutate(
    decimalLatitude = as.numeric(decimalLatitude),
    decimalLongitude = as.numeric(decimalLongitude)
  ) %>%
  left_join(vcs_lookup, by = "verbatimCoordinateSystem") %>%
  mutate(
    needs_fix =
      !is.na(parsed_lat) &
      (
        is.na(decimalLatitude) |
          is.na(decimalLongitude) |
          decimalLatitude < lat_min |
          decimalLatitude > lat_max |
          decimalLongitude < lon_min |
          decimalLongitude > lon_max
      ),
    
    decimalLatitude = if_else(needs_fix, parsed_lat, decimalLatitude),
    decimalLongitude = if_else(needs_fix, parsed_lon, decimalLongitude),
    
    geodeticDatum = if_else(
      needs_fix,
      coalesce(verbatimSRS, "WGS84"),
      geodeticDatum
    ),
    coordinateUncertaintyInMeters = if_else(
      needs_fix,
      NA_real_,
      coordinateUncertaintyInMeters
    ),
    georeferencedBy = if_else(
      needs_fix,
      "Elif Kardas",
      georeferencedBy
    ),
    georeferencedDate = if_else(
      needs_fix,
      as.character(Sys.Date()),
      georeferencedDate
    ),
    georeferenceProtocol = if_else(
      needs_fix,
      "Parsed from verbatimCoordinateSystem field",
      georeferenceProtocol
    ),
    georeferenceSources = if_else(
      needs_fix,
      "Original collector-recorded coordinate notation",
      georeferenceSources
    ),
    georeferenceRemarks = if_else(
      needs_fix,
      "Coordinates parsed from verbatimCoordinateSystem and converted to decimal degrees.",
      georeferenceRemarks
    )
  ) %>%
  select(-parsed_lat, -parsed_lon, -needs_fix)



#################################################
#
#              verbatimLongitude
#              verbatimLatitude
#
#################################################
combined_df %>%
  filter(!is_empty(verbatimLatitude) | !is_empty(verbatimLongitude)) %>%
  select(full_address = verbatimLocality, verbatimLatitude, verbatimLongitude) %>%
  distinct() %>%
  arrange(verbatimLatitude) %>%
  View("verbatimLatitude / verbatimLongitude")

combined_df %>% filter(!is_empty(verbatimLatitude)) %>% pull(verbatimLatitude) %>% unique() %>% head(30)
combined_df %>% filter(!is_empty(verbatimLongitude)) %>% pull(verbatimLongitude) %>% unique() %>% head(30)

# They are clean coordinates, let's just put them in decimalLatitude and decimalLongitude columns in the combined_df

lat_min <- 17.8; lat_max <- 18.6
lon_min <- -68.0; lon_max <- -65.1

combined_df <- combined_df %>%
  mutate(
    verbatim_lat_num = as.numeric(verbatimLatitude),
    verbatim_lon_num = as.numeric(verbatimLongitude),
    
    needs_fill = (
      is.na(decimalLatitude) | is.na(decimalLongitude)
    ) & !is.na(verbatim_lat_num) & !is.na(verbatim_lon_num),
    
    decimalLatitude = if_else(needs_fill, verbatim_lat_num, decimalLatitude),
    decimalLongitude = if_else(needs_fill, verbatim_lon_num, decimalLongitude),
    geodeticDatum = if_else(needs_fill, coalesce(verbatimSRS, "WGS84"), geodeticDatum),
    coordinateUncertaintyInMeters = if_else(needs_fill, NA_real_, coordinateUncertaintyInMeters),
    georeferencedBy = if_else(needs_fill, "Elif Kardas", georeferencedBy),
    georeferencedDate = if_else(needs_fill, as.character(Sys.Date()), georeferencedDate),
    georeferenceProtocol = if_else(needs_fill, "Transcribed from verbatimLatitude/verbatimLongitude fields", georeferenceProtocol),
    georeferenceSources = if_else(needs_fill, "Original collector-recorded coordinates", georeferenceSources),
    georeferenceRemarks = if_else(needs_fill, "Coordinates transcribed directly from verbatim decimal lat/lon fields", georeferenceRemarks)
  ) %>%
  select(-verbatim_lat_num, -verbatim_lon_num, -needs_fill)

# -----------------------------
# Sanity check: any out-of-bounds after fill?
# -----------------------------
combined_df %>%
  filter(
    !is.na(decimalLatitude), !is.na(decimalLongitude),
    decimalLatitude < lat_min | decimalLatitude > lat_max |
      decimalLongitude < lon_min | decimalLongitude > lon_max
  ) %>%
  nrow()

# How many rows got filled
sum(
  !is.na(combined_df$decimalLatitude) &
    !is_empty(combined_df$verbatimLatitude)
)
# 2331 rows were filled based on verbatimLatitude and verbatimLongitude.



#################################################
#
#              verbatimCoordinates
#
#################################################
combined_df %>%
  filter(!is_empty(verbatimCoordinates)) %>%
  select(
    full_address = verbatimLocality,
    verbatimCoordinates
  ) %>%
  distinct() %>%
  arrange(verbatimCoordinates) %>%
  View("Unique verbatimCoordinates")

combined_df %>%
  filter(!is_empty(verbatimCoordinates)) %>%
  distinct(verbatimCoordinates) %>%
  arrange(verbatimCoordinates)

# to see only unique values:
combined_df %>%
  filter(!is_empty(verbatimCoordinates)) %>%
  pull(verbatimCoordinates) %>%
  unique() %>%
  sort()
# [1] "17°57'3.31N; 66°50'22.89W"       "17°58'18.45N; 66°52'4.87W"       "17°58'53.00N; 66°52'35.98W"      "18 2'11.53N; 66 22'26.80W"      
# [5] "18 24'9.97 N; 66 2'47.03W"       "18 26'51.64N; 66 53'44.92W"      "18.004532 -66.255518"            "18.004532 -66.255519"           
# [9] "18.07317 N -0.66.05656 W"        "18.07585 N -0.66.1077 W"         "18.08384N -0.66.10275W"          "18.08542N -0.66.10245W"         
# [13] "18.09506 N -0.66.08128"          "18.147429, -65.861511"           "18.380741, -65.625477"           "18.380802 -66.624182"           
# [17] "18.38538 066.72281"              "18.38679N -65.72537"             "18.38700N -65.72522"             "18.387N -65.72522"              
# [21] "18.409551 -66.057523"            "18.45, -65.97"                   "18.46177 -66.42376"              "18.46182 -66.42381"             
# [25] "18˚16' 48.96  -65˚52' 00.85"     "18° 2'11.53N, -66 22'26.80W"     "18° 24'13.26N, -66° 2' 53.22W"   "18° 24'14.45N, -66° 2'41.52W"   
# [29] "18°07'27.7N 66°38'15.8W"         "N 18.32407 W -065.82008"         "N 18.34942 W: 065.63804"         "N 18˚16' 48.96 W 65˚52' 00.85"  
# [33] "N: 18.27711 W: 066.25426"        "N: 18.34942 W: 065.63804"        "N: 18.4048 W: 066.6450"          "N: 18.46.186 W: 066.42380"      
# [37] "N:17.95164, W:-66.83558"         "N:17.95197, W:-66.83620"         "N:17.95393, W:-66.84741"         "N:17.95427, W:-66.84677"        
# [41] "N18 16'48.96'' W 65 52' 00.85''"

# They are not decimal coordinates, let's convert them into decimals coordinates into decimalLatitude and decimalLongitude columns in the combined_df
# Most of these are decimal degrees already (just messy formatting/typos), and a handful are DMS. 
# A single regex won't safely handle all 30 variants — some have corrupted numbers (-0.66.1077, 18.46.186) that need human judgment, not a blind parse. 
# Here's a tiered parser that handles the clean cases automatically and flags the ambiguous ones for manual review, rather than guessing:

parse_verbatim_coordinates <- function(x) {
  
  lat <- NA_real_
  lon <- NA_real_
  method <- NA_character_
  
  xs <- str_trim(x)
  
  # -----------------------------
  # PATTERN 1: DMS with degree/minute/second marks (˚, °, or space) + N/W
  # e.g. "18° 24'13.26N, -66° 2' 53.22W", "18 26'51.64N; 66 53'44.92W",
  #      "N18 16'48.96'' W 65 52' 00.85''"
  # -----------------------------
  m1 <- str_match(xs, "(\\d+)[°˚\\s]+(\\d+)['’]\\s*([\\d.]+)[\"'’]*\\s*N.*?(\\d+)[°˚\\s]+(\\d+)['’]\\s*([\\d.]+)[\"'’]*\\s*W")
  print(m1)
  if (!any(is.na(m1[1,2:7]))) {
    lat_d <- as.numeric(m1[2]); lat_m <- as.numeric(m1[3]); lat_s <- as.numeric(m1[4])
    lon_d <- as.numeric(m1[5]); lon_m <- as.numeric(m1[6]); lon_s <- as.numeric(m1[7])
    lat <- lat_d + lat_m/60 + lat_s/3600
    lon <- -(lon_d + lon_m/60 + lon_s/3600)
    method <- "DMS"
    return(tibble(raw = x, lat = lat, lon = lon, method = method))
  }
  
  # -----------------------------
  # PATTERN 2: "N: 18.xxxx W: 66.xxxx" or "N 18.xxxx W -65.xxxx" (clean decimal)
  # -----------------------------
  # Add [,\\s]* to allow a comma between lat and W
  m2 <- str_match(xs, "N:?\\s*(-?\\d+\\.\\d+)[,\\s]*W:?\\s*(-?\\d+\\.\\d+)")
  print(m2)
  if (!any(is.na(m2[1,2:3]))) {
    lat <- as.numeric(m2[2])
    lon <- -abs(as.numeric(m2[3]))   # force negative (West)
    method <- "labeled_decimal"
    return(tibble(raw = x, lat = lat, lon = lon, method = method))
  }
  
  # -----------------------------
  # PATTERN 3: Corrupted decimal like "-0.66.1077" -> intended "-66.1077"
  # Fix by removing the spurious "0." right after the minus sign
  # -----------------------------
  xs_fixed <- str_replace(xs, "-0\\.(\\d)", "-\\1")
  
  # -----------------------------
  # PATTERN 4: Plain "lat lon" or "lat, lon", optional N/W suffix
  # -----------------------------
  m4 <- str_match(xs_fixed, "(-?\\d+\\.\\d+)\\s*N?[,\\s]+(-?\\d+\\.\\d+)\\s*W?")
  print(m4)
  if (!any(is.na(m4[1,2:3]))) {
    lat <- as.numeric(m4[2])
    lon <- as.numeric(m4[3])
    if (lon > 0) lon <- -lon   # longitude should be negative (West)
    method <- "plain_decimal"
    return(tibble(raw = x, lat = lat, lon = lon, method = method))
  }
  
  # Nothing matched
  tibble(raw = x, lat = NA_real_, lon = NA_real_, method = "UNPARSED")
}

# -----------------------------
# Apply to unique verbatimCoordinates values
# -----------------------------
vc_values <- combined_df %>%
  filter(!is_empty(verbatimCoordinates)) %>%
  pull(verbatimCoordinates) %>%
  unique()

vc_parsed <- purrr::map_dfr(vc_values, parse_verbatim_coordinates)

# -----------------------------
# Validate against PR bounding box
# -----------------------------
vc_parsed <- vc_parsed %>%
  mutate(
    valid = !is.na(lat) & !is.na(lon) &
      lat >= 17.8 & lat <= 18.6 &
      lon >= -68.0 & lon <= -65.1
  )

print(vc_parsed, n = Inf)

# Let's see only those unparsed:
vc_parsed %>% filter(!valid | method == "UNPARSED")
# A tibble: 4 × 5
# raw                               lat   lon method   valid
# <chr>                           <dbl> <dbl> <chr>    <lgl>
# 1 N: 18.46.186 W: 066.42380          NA    NA UNPARSED FALSE
# 2 N 18˚16' 48.96 W 65˚52' 00.85      NA    NA UNPARSED FALSE
# 3 18˚16' 48.96  -65˚52' 00.85        NA    NA UNPARSED FALSE
# 4 N18 16'48.96'' W 65 52' 00.85''    NA    NA UNPARSED FALSE

# 4 are still invalid, let's do them manually. 
# IN ADDITION: 
# there are also coordinates from verbatimCoordinates that are not well written from the datasetProvider:
# verbatimCoordinates 18.004532 -66.255518 with decimalLatitude 18.00453 and decimalLongitude 18.00453,
# in place of decimalLatitude 18.00453 and decimalLongitude -66.255518

# -----------------------------
# Manual coordinate assignments
# Fill in decimalLatitude / decimalLongitude for each address
# Leave NA for any you haven't resolved yet
# -----------------------------
manual_coords <- tibble::tribble(
  ~verbatimCoordinates,                 ~decimalLatitude, ~decimalLongitude, ~notes,
  "N: 18.46.186 W: 066.42380",            18.769767,        -66.706333,        "",
  "N 18˚16' 48.96 W 65˚52' 00.85",        18.280267,        -65.866903,        "",
  "18˚16' 48.96  -65˚52' 00.85",          18.280267,        -65.866903,        "",
  "N18 16'48.96'' W 65 52' 00.85''",      18.280267,        -65.866903,        "",
  "18.004532 -66.255518",                 18.004532,        -66.255518,        "Longitude corrected manually from verbatimCoordinates"
)

combined_df <- combined_df %>%
  left_join(
    manual_coords %>%
      rename(
        manual_lat = decimalLatitude,
        manual_lon = decimalLongitude,
        manual_notes = notes
      ),
    by = "verbatimCoordinates"
  ) %>%
  mutate(
    needs_fix =
      (is.na(decimalLatitude) |
         is.na(decimalLongitude) |
         decimalLatitude < lat_min |
         decimalLatitude > lat_max |
         decimalLongitude < lon_min |
         decimalLongitude > lon_max) &
      !is.na(manual_lat),
    
    decimalLatitude = if_else(needs_fix, manual_lat, decimalLatitude),
    decimalLongitude = if_else(needs_fix, manual_lon, decimalLongitude),
    
    geodeticDatum = if_else(needs_fix, "WGS84", geodeticDatum),
    coordinateUncertaintyInMeters = if_else(
      needs_fix,
      NA_real_,
      coordinateUncertaintyInMeters
    ),
    georeferencedBy = if_else(
      needs_fix,
      "Elif Kardas",
      georeferencedBy
    ),
    georeferencedDate = if_else(
      needs_fix,
      as.character(Sys.Date()),
      georeferencedDate
    ),
    georeferenceProtocol = if_else(
      needs_fix,
      "Manually assigned from verbatim locality/coordinates",
      georeferenceProtocol
    ),
    georeferenceSources = if_else(
      needs_fix,
      "Manual review of verbatimCoordinates / verbatimCoordinateSystem fields",
      georeferenceSources
    ),
    georeferenceRemarks = if_else(
      needs_fix,
      dplyr::if_else(
        !is.na(manual_notes) & manual_notes != "",
        manual_notes,
        "Coordinates transcribed from verbatimCoordinates and validated."
      ),
      georeferenceRemarks
    )
  ) %>%
  select(-manual_lat, -manual_lon, -manual_notes, -needs_fix)


###################################################
# COORDINATE CLEANING
###################################################

#loading packages

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


sort(unique(combined_df$decimalLatitude))
sort(unique(combined_df$decimalLongitude)) # there are coordinates that are not negative (West should be negative all!) and there are 1 latitude value "18.00453" inside those.

combined_df %>%
  filter(
    decimalLongitude > 0 |
      decimalLongitude > -18
  ) %>%
  select(
    decimalLatitude,
    decimalLongitude,
    verbatimLatitude,
    verbatimLongitude,
    verbatimCoordinates,
    verbatimCoordinateSystem,
    verbatimLocality
  )

# -----------------------------
# Clean numeric coordinates
# -----------------------------
clean_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}

combined_df$decimalLatitude  <- clean_numeric(combined_df$decimalLatitude)
combined_df$decimalLongitude <- clean_numeric(combined_df$decimalLongitude)

# -----------------------------
# Fix non negative Longitudes 
# -----------------------------
combined_df <- combined_df %>%
  mutate(
    decimalLongitude = if_else(
      !is.na(decimalLongitude) & decimalLongitude > 0,
      -decimalLongitude,
      decimalLongitude
    )
  )
# was it fixed? 
sort(unique(combined_df$decimalLongitude)) # yes
# -----------------------------
# Fix inverted coordinates
# -----------------------------
# Identify inverted rows
index_inverse <- combined_df$decimalLatitude < 10 | combined_df$decimalLatitude > 20

# Replace NAs with FALSE
index_inverse[is.na(index_inverse)] <- FALSE

# Swap latitude and longitude safely
tmp <- combined_df$decimalLatitude[index_inverse]
combined_df$decimalLatitude[index_inverse] <- combined_df$decimalLongitude[index_inverse]
combined_df$decimalLongitude[index_inverse] <- tmp

# Verify if it worked
unique(combined_df$decimalLatitude) 

# -----------------------------
# Fix UTF-8 typos
# -----------------------------
unique(combined_df$municipality)
unique(combined_df$verbatimLocality)

library(dplyr)
library(stringr)

replacements <- c(
  "√º" = "u"
)

cols_to_fix <- c(
  "continent", "waterBody", "islandGroup", "island", "country",
  "countryCode", "stateProvince", "county", "municipality",
  "locality", "verbatimLocality"
)

combined_df <- combined_df %>%
  mutate(
    across(
      all_of(cols_to_fix),
      ~ str_replace_all(.x, replacements)
    ),
    municipality = ifelse(island == "Mona", "Mayaguez", municipality)
  )


#View(combined_df)
#Create dataframe with good GPS coordinates (before geocoding)
write.table(combined_df, "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-combined-coordinates-inferredGPS-cleaned+MEBTtypo.txt", row.names = FALSE, sep = "\t", quote = FALSE, na = "")


######################################################################
# -----------------------------
# Full dataframe geocoding
# -----------------------------
######################################################################
# -----------------------------
# Load packages
# -----------------------------
library(dplyr)
library(stringr)
library(tidygeocoder)
library(lubridate)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(lubridate)
library(purrr)
# -----------------------------
# Build full address for geocoding
# -----------------------------

#> unique(combined_df$country)
# [1] "Puerto Rico"              "United States"            "Estados Unidos"           "United States "           "United States of America" ""                        
# > unique(combined_df$countryCode)
# [1] "PR"  "US"  "USA" "IS"
# let's transform country set as "United States" "United States of America" "United States " "Estados Unidos" into into Puerto Rico, as well as countryCode set as "US"  "USA" "IS" into "PR"  

combined_df <- combined_df %>%
  mutate(
    country = if_else(
      trimws(country) %in% c("United States", "United States of America", "Estados Unidos"),
      "Puerto Rico",
      country
    ),
    countryCode = if_else(
      countryCode %in% c("US", "USA", "IS"),
      "PR",
      countryCode
    ),
    # Ensure consistency
    country = if_else(countryCode == "PR", "Puerto Rico", country)
  )

#Verify if it worked:
# unique(combined_df$countryCode)
# unique(combined_df$country)

# combined_df <- combined_df %>%
#   mutate(
#     full_address = pmap_chr(
#       list(verbatimLocality, locality, municipality, county, stateProvince, island, country),
#       ~ c(...) %>%
#         discard(~ is.na(.x) || .x == "") %>%
#         str_c(collapse = ", ")
#     )
#   )
library(dplyr)
library(purrr)
library(stringr)

clean_address <- function(...) {
  
  x <- c(...)
  
  # Remove NA and blanks
  x <- x[!is.na(x) & trimws(x) != ""]
  x <- trimws(x)
  
  # Standardize PR
  x[x == "PR"] <- "Puerto Rico"
  
  # Remove duplicate elements, keeping first occurrence
  x <- x[!duplicated(x)]
  
  # Put Puerto Rico once at the end
  if ("Puerto Rico" %in% x) {
    x <- c(x[x != "Puerto Rico"], "Puerto Rico")
  }
  
  str_c(x, collapse = ", ")
}

combined_df <- combined_df %>%
  mutate(
    full_address = pmap_chr(
      list(
        verbatimLocality,
        locality,
        municipality,
        county,
        stateProvince,
        island,
        country
      ),
      clean_address
    )
  )

unique(combined_df$full_address)

combined_df <- combined_df %>%
  mutate(
    full_address = case_when(
      full_address == "Salinas, PR, Salinas, Puerto Rico, Salinas, Puerto Rico" ~
        "Salinas, Puerto Rico",
      
      full_address == "Pinones, Playa Linda,Pinones, Loiza, Playa Linda,Pinones, Loiza, Puerto Rico" ~
        "Playa Linda, Pinones, Loiza, Puerto Rico",
      
      full_address == "Parque del Centenario, UPRRP, Rio Piedras, Rio Piedras, San Juan, Puerto Rico" ~
        "Parque del Centenario, UPRRP, Rio Piedras, San Juan, Puerto Rico",
      
      full_address == "UPR-RP, Universidad de Puerto Rico-Recinto de Rio Piedras, San Juan, Universida de Puerto Rico- Recinto de Rio Piedras, San Juan, Puerto Rico" ~
        "UPR-RP, Universidad de Puerto Rico-Recinto de Rio Piedras, San Juan, Puerto Rico",
      
      full_address == "Coamo, PR, Banos de Coamo, Coamo, Puerto Rico" ~
        "Banos de Coamo, Coamo, Puerto Rico",
      
      full_address == "UPRRP, Universidad de Puerto Rico-Recinto de Rio Piedras, Rio Piedras, San Juan, Puerto Rico" ~
        "UPRRP, Universidad de Puerto Rico-Recinto de Rio Piedras, Rio Piedras, San Juan, Puerto Rico",
      
      full_address == "Puerto Rico, Loiza, Pinones, Pinones, PR, Loiza, Puerto Rico" ~
        "Pinones, Loiza, Puerto Rico",
      
      full_address == "S.W.River, Santa Isabel, Puerto Rico" ~
        "Santa Isabel, Puerto Rico",
      
      full_address == "Guanica, Guanica Dry Forest, Guanica Dry Forest, Guanica, Puerto Rico" ~
        "Guanica Dry Forest, Guanica, Puerto Rico",
      
      TRUE ~ full_address
    )
  )

# -----------------------------
# 6️⃣ Ensure Darwin Core metadata columns exist
# -----------------------------
dwc_cols <- c(
  "geodeticDatum",
  "coordinateUncertaintyInMeters",
  "georeferencedBy",
  "georeferencedDate",
  "georeferenceProtocol",
  "georeferenceSources",
  "georeferenceRemarks"
)

for(col in dwc_cols){
  if(!col %in% names(combined_df)) combined_df[[col]] <- NA
}
# -----------------------------
# 7️⃣ Identify rows needing geocoding
# Only rows where BOTH lat and lon are NA
# -----------------------------
rows_to_geo <- which(
  is.na(combined_df$decimalLatitude) &
    is.na(combined_df$decimalLongitude)
)
length(rows_to_geo)
# 748 rows need to be georeferenced
# -----------------------------
# 8️⃣ Geocode missing rows
# -----------------------------
library(dplyr)
library(tidygeocoder)

# -----------------------------
# 8️⃣ Geocode missing rows
# -----------------------------
if(length(rows_to_geo) > 0){
  
  geo_results <- combined_df[rows_to_geo, ] %>%
    geocode(
      address = full_address,
      method = "google",
      lat = "lat",    # use string names
      long = "long",
      full_results = FALSE
    )
  
  # successful geocodes
  success <- !is.na(geo_results$lat)
  success_rows <- rows_to_geo[success]
  
  # write coordinates
  combined_df$decimalLatitude[success_rows]  <- geo_results$lat[success]
  combined_df$decimalLongitude[success_rows] <- geo_results$long[success]
  
  # -----------------------------
  # 9️⃣ Darwin Core metadata
  # -----------------------------
  combined_df$geodeticDatum[success_rows] <- "WGS84"
  combined_df$coordinateUncertaintyInMeters[success_rows] <- 1000
  combined_df$georeferencedBy[success_rows] <- "Elif Kardas"
  combined_df$georeferencedDate[success_rows] <- Sys.Date()
  combined_df$georeferenceProtocol[success_rows] <- "Google Maps API geocoding"
  combined_df$georeferenceSources[success_rows] <- "Google Maps Platform"
  combined_df$georeferenceRemarks[success_rows] <- "Coordinates assigned from locality fields"
  
  message("✅ Geocoded ", length(success_rows), " rows successfully.")
  message("⚠️ ", length(rows_to_geo) - length(success_rows), " rows could not be geocoded.")
  
} else {
  message("✅ No rows needed geocoding.")
}

# Passing 236 addresses to the Google single address geocoder
# [===========================================================================================================================] 236/236 (100%) Elapsed:  7s Remaining:  0s
# ✅ Geocoded 748 rows successfully.
# ⚠️ 0 rows could not be geocoded.


# Verify that all georeferencing was done well by showing anything outside the PR polygon

lat_min <- 17.8
lat_max <- 18.6
lon_min <- -68.0
lon_max <- -65.1

hors_bornes <- combined_df %>%
  filter(
    !is.na(decimalLatitude), !is.na(decimalLongitude),
    decimalLatitude < lat_min | decimalLatitude > lat_max |
      decimalLongitude < lon_min | decimalLongitude > lon_max
  )

View(hors_bornes) # none are outside PR, good!

# -----------------------------
# 9️⃣ Remove temporary column
# -----------------------------
combined_df <- combined_df %>% select(-full_address)

#view new combined_df with the GPS coordinates (geocoded)
#View(combined_df)

# export
#Create dataframe with good GPS coordinates (before geocoding)
write.table(combined_df, "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-combined-Geocoded.txt", 
            row.names = FALSE, sep = "\t", quote = FALSE, na = "")

# -----------------------------
# 🔟 Summary
# -----------------------------
summary_df <- combined_df %>%
  summarize(
    total_rows = n(),
    rows_with_coordinates = sum(!is.na(decimalLatitude) & !is.na(decimalLongitude)),
    rows_without_coordinates = sum(is.na(decimalLatitude) & is.na(decimalLongitude))
  )

print(summary_df)
# total_rows rows_with_coordinates rows_without_coordinates
# 1       3987                  3987                        0


####################################################################################################
