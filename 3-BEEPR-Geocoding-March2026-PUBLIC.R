##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset, all combined
#          GEOCODING
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: February 17th 2026
#
##############################################################################################################


library(dplyr)
library(stringr)
# get occ
combined_df <- read.delim(
  "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-combined-last.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)


#########################################################################################
#                               SETTING YOUR GOOGLE API KEY                             #
#########################################################################################

# Replace YOUR_GOOGLE_API_KEY with your actual key
Sys.setenv(GOOGLEGEOCODE_API_KEY = "fdhghjgjfhqlkrgnsjehbwrslfkbsbfdkgb") # replace with yours

# Test that R sees it
Sys.getenv("GOOGLEGEOCODE_API_KEY")
# Should print your key


#########################################################################################
#                                SUMMARIZING THE TABLE                                  #
#########################################################################################

# number total of rows
nrow(combined_df) # 3990

# Count rows with both latitude and longitude present
count_GPSocc <- sum(complete.cases(combined_df[, c("decimalLatitude", "decimalLongitude")]))
print(count_GPSocc) # 3604 occurrences have GPS coordinates

# Rows with missing latitude or longitude
missing_gps <- combined_df[!complete.cases(combined_df[, c("decimalLatitude", "decimalLongitude")]), ]
# View them
# View(missing_gps)
# unique(missing_gps$institutionID) # all the MEBT occ have missing GPS coordinates + other occurrences from MZU

# number of families
num_families <- n_distinct(combined_df$family)
print(num_families) #4

# number of genus
num_genus <- n_distinct(combined_df$genus)
print(num_genus) #18

# number of species (specificEpithet)
num_se <- n_distinct(combined_df$specificEpithet)
print(num_se) #40

# number of scientificName (genus and genus+species)
num_sn <- n_distinct(combined_df$scientificName)
print(num_sn) #50





###################################################
# COORDINATE CLEANING
###################################################

#loading packages

library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


unique(combined_df$decimalLatitude)

# -----------------------------
# Clean numeric coordinates
# -----------------------------
clean_numeric <- function(x) {
  as.numeric(gsub("[^0-9.-]", "", x))
}

combined_df$decimalLatitude  <- clean_numeric(combined_df$decimalLatitude)
combined_df$decimalLongitude <- clean_numeric(combined_df$decimalLongitude)


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
unique(combined_df$decimalLatitude) # yes it did

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
#Create dataframe with good GPS coordinates
write.table(combined_df, "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-combined-GPSMEBT-typo.txt", row.names = FALSE, sep = "\t", quote = FALSE, na = "")


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
# -----------------------------
# Build full address for geocoding
# -----------------------------
library(dplyr)
library(stringr)
library(purrr)

combined_df <- combined_df %>%
  mutate(
    full_address = pmap_chr(
      list(verbatimLocality, locality, municipality, stateProvince, island, country),
      ~ c(...) %>%
        discard(~ is.na(.x) || .x == "") %>%
        str_c(collapse = ", ")
    )
  )

unique(combined_df$full_address)

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




# -----------------------------
# 9️⃣ Remove temporary column
# -----------------------------
combined_df <- combined_df %>% select(-full_address)

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
# 1       3991                  3989                        2
