##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset, all combined
#          SUMMARIZING AND MAPS
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: March 26th 2026
#
##############################################################################################################


######################################################################
# -----------------------------
# Separate valid/missing coordinates
# -----------------------------
######################################################################
# Count rows with georeferences
combined_df %>%
  summarize(
    total_rows = n(),
    rows_with_coordinates = sum(!is.na(decimalLatitude) & !is.na(decimalLongitude)),
    rows_without_coordinates = sum(is.na(decimalLatitude) | is.na(decimalLongitude))
  )
occ_with <- combined_df[!is.na(combined_df$decimalLatitude) & !is.na(combined_df$decimalLongitude), ]
occ_missing <- combined_df[is.na(combined_df$decimalLatitude) | is.na(combined_df$decimalLongitude), ]

# -----------------------------
# Get Puerto Rico polygon
# -----------------------------
pr <- ne_countries(scale = "large", returnclass = "sf") %>%
  filter(admin == "Puerto Rico")

# define the bounds
lat_min <- 17.8
lat_max <- 18.6
lon_min <- -68.5
lon_max <- -65.2

# Keep only coordinates inside Puerto Rico
occ_with <- occ_with %>%
  filter(decimalLatitude  >= lat_min &
           decimalLatitude  <= lat_max &
           decimalLongitude >= lon_min &
           decimalLongitude <= lon_max)
# use polygon check (more precise)

library(sf)

# -----------------------------
# Convert to sf object for plotting
# -----------------------------
occ_with_sf <- st_as_sf(occ_with,
                        coords = c("decimalLongitude", "decimalLatitude"),
                        crs = 4326)


# Keep only points inside the PR polygon
occ_with_sf <- occ_with_sf[st_within(occ_with_sf, pr, sparse = FALSE), ]


# -----------------------------
# Plot map
# -----------------------------
#Plot occurrences in general
ggplot() +
  geom_sf(data = pr, fill = "grey90", color = "black") +
  geom_sf(data = occ_with_sf, color = "darkblue", size = 2, alpha = 0.7) +
  coord_sf(xlim = c(-68.2, -65.2), ylim = c(17.6, 18.8)) +
  labs(title = "Bee occurrences over the Archipelago of Puerto Rico (all databases)")+
  theme_minimal()

#Plot occurrences per institutionCode
ggplot() +
  geom_sf(data = pr, fill = "grey90", color = "black") +
  geom_sf(data = occ_with_sf,
          aes(color = institutionCode),
          size = 2,
          alpha = 0.7) +
  coord_sf(xlim = c(-68.2, -65.2), ylim = c(17.6, 18.8)) +
  labs(title = "Bee occurrences over the Archipelago of Puerto Rico (per museum)")+
  theme_minimal()

#Plot occurrences per databaseSource
ggplot() +
  geom_sf(data = pr, fill = "grey90", color = "black") +
  geom_sf(data = occ_with_sf,
          aes(color = databaseSource),
          size = 2,
          alpha = 0.7) +
  coord_sf(xlim = c(-68.2, -65.2), ylim = c(17.6, 18.8)) +
  labs(title = "Bee occurrences over the Archipelago of Puerto Rico (per data source)")+
  theme_minimal()

#Plot occurrences per databaseSource, excluding Bees_Of_SJ

# filter out Bees_Of_SJ
library(dplyr)

BEEPR_wtSJ <- occ_with_sf %>%
  filter(databaseSource != "Bees_Of_SJ" | is.na(databaseSource))


ggplot() +
  geom_sf(data = pr, fill = "grey90", color = "black") +
  geom_sf(data = BEEPR_wtSJ,
          aes(color = databaseSource),
          size = 2,
          alpha = 0.7) +
  coord_sf(xlim = c(-68.2, -65.2), ylim = c(17.6, 18.8)) +
  labs(title = "Bee occurrences over the Archipelago of Puerto Rico (excluding Bees_Of_SJ)")+
  theme_minimal()





############################################################
# PUBLICATION-QUALITY MAP – PUERTO RICO OCCURRENCES
############################################################

# ---- 1. PACKAGES ----
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthhires)

# ---- 2. BASE MAP (HIGH RES, INCLUDES CULEBRA) ----
pr <- ne_countries(
  scale = "large",
  returnclass = "sf"
) %>%
  filter(admin == "Puerto Rico")

# ---- 3. ENSURE CRS MATCH ----
# occ_with_sf must be sf with lon/lat
occ_with_sf <- st_transform(occ_with_sf, st_crs(pr))

#################################
# ---- 4. PUBLICATION MAP 1 ----
#################################
# Keep only rows with non-missing databaseSource
occ_with_sf_clean <- occ_with_sf %>%
  filter(!is.na(databaseSource) & databaseSource != "")

p <- ggplot() +
  
  # Land
  geom_sf(
    data = pr,
    fill = "grey95",
    color = "grey30",
    linewidth = 0.4
  ) +
  
  # Occurrence points
  geom_sf(
    data = occ_with_sf,
    color = "#1f4e79",
    size = 2,
    alpha = 0.8
  ) +
  
  # Map extent (Puerto Rico + Vieques + Culebra)
  coord_sf(
    xlim = c(-68.05, -65.2),
    ylim = c(17.5, 18.8),
    expand = FALSE
  ) +
  
  # Titles
  labs(
    title = "Bee occurrence records in Puerto Rico",
    subtitle = "Including Mona, Vieques and Culebra islands",
    caption = "Base map: Natural Earth (1:10m). 
Occurrence data: Bees_Of_SJ, GBIF (clean), MEBT (georeferenced), MZUPRRP, SaraPrado_PC"
  ) +
  
  # Clean theme
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 8, hjust = 0),
    axis.text = element_text(size = 9)
  )

p

#export
ggsave(
  "maps/puerto_rico_occurrences_map1.pdf",
  plot = p,
  width = 15,
  height = 10,
  units = "cm"
)

#################################
# ---- 4. PUBLICATION MAP 2 ----
#################################
############################################################
# FACET MAP BY dataSource (PUBLICATION RECOMMENDED)
############################################################

# Then plot
p2 <- ggplot() +
  geom_sf(data = pr, fill = "grey95", color = "grey30", linewidth = 0.4) +
  geom_sf(data = occ_with_sf_clean, color = "#1f4e79", size = 1.8, alpha = 0.8) +
  facet_wrap(~ databaseSource, nrow=3, ncol=2) +
  coord_sf(xlim = c(-68.05, -65.2), ylim = c(17.5, 18.8), expand = FALSE) +
  labs(
    title = "Bee occurrence records in Puerto Rico",
    subtitle = "Faceted by database source",
    caption = "Base map: Natural Earth (1:10m)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

p2


#export
ggsave(
  "maps/puerto_rico_occurrences_map2.pdf",
  plot = p2,
  width = 25,
  height = 12.5,
  units = "cm"
)



############################################################################################
############################################################################################
############################################################################################
############################################################################################
############################################################################################
install.packages(c("dplyr", "ggplot2", "sf", "lubridate", "viridis"))

############################################################
# OCCURRENCE ACCESS (occAccess-style) BIAS ANALYSIS
# For data paper publication
############################################################

# ========================
# 1. PACKAGES
# ========================

library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(viridis)

# ========================
# 2. DATA CHECKS
# ========================

# Ensure sf object
stopifnot(inherits(occ_with_sf_clean, "sf"))

# Remove records without coordinates
occ_with_sf_clean <- occ_with_sf_clean |> 
  filter(!st_is_empty(geometry))

# ========================
# 3. SPATIAL BIAS Correct hex density (NO distortion)
# ========================

############################################################
# 3. SPATIAL BIAS (PROJECTED CORRECTLY)
############################################################

# Reproject to UTM zone 20N (meters)
occ_utm <- st_transform(occ_with_sf_clean, 32620)
pr_utm  <- st_transform(pr, 32620)

# Extract coordinates in meters
occ_coords <- occ_utm |>
  mutate(
    x = st_coordinates(geometry)[,1],
    y = st_coordinates(geometry)[,2]
  )

p_spatial_bias <- ggplot() +
  
  geom_sf(
    data = pr_utm,
    fill = "grey95",
    color = "grey40",
    linewidth = 0.3
  ) +
  
  stat_bin_hex(
    data = occ_coords,
    aes(x = x, y = y, fill = after_stat(count)),
    bins = 40,
    alpha = 0.9
  ) +
  
  scale_fill_viridis(
    option = "C",
    name = "Record count"
  ) +
  
  labs(
    title = "Spatial sampling bias",
    subtitle = "Density of bee occurrence records in Puerto Rico",
    caption = "Projected in UTM zone 20N (EPSG:32620)"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

p_spatial_bias
#export
#export
ggsave(
  "maps/puerto_rico_spatial_bias.pdf",
  plot = p_spatial_bias,
  width = 15,
  height = 10,
  units = "cm"
)

# ========================
# 4. TEMPORAL BIAS
# ========================

# ---- A. Records per year ----

occ_year <- occ_with_sf_clean |>
  mutate(year = as.numeric(year)) |>
  filter(!is.na(year)) |>
  count(year)

p_temporal_year <- ggplot(occ_year, aes(x = year, y = n)) +
  geom_col() +
  labs(
    title = "Temporal sampling bias",
    subtitle = "Number of occurrence records per year",
    x = "Year",
    y = "Number of records"
  ) +
  theme_minimal()

p_temporal_year
#export
ggsave(
  "maps/puerto_rico_temporal_year.pdf",
  plot = p_temporal_year,
  width = 15,
  height = 10,
  units = "cm"
)

# ---- B. Seasonal bias (month) ----

occ_month <- occ_with_sf_clean |>
  mutate(month = as.numeric(month)) |>
  filter(!is.na(month)) |>
  count(month)

p_temporal_month <- ggplot(occ_month, aes(x = month, y = n)) +
  geom_col() +
  scale_x_continuous(breaks = 1:12) +
  labs(
    title = "Seasonal sampling bias",
    subtitle = "Number of records per month",
    x = "Month",
    y = "Number of records"
  ) +
  theme_minimal()

p_temporal_month
#export
#export
ggsave(
  "maps/puerto_rico_temporal_month.pdf",
  plot = p_temporal_month,
  width = 15,
  height = 10,
  units = "cm"
)

# ========================
# 5. TAXONOMIC BIAS
# ========================
# Bias among families

occ_family <- occ_with_sf_clean |>
  filter(!is.na(family)) |>
  count(family, sort = TRUE)

p_taxonomic_bias <- ggplot(
  occ_family,
  aes(x = reorder(family, n), y = n)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Taxonomic sampling bias",
    subtitle = "Number of occurrence records per bee family",
    x = "Family",
    y = "Number of records"
  ) +
  theme_minimal()

p_taxonomic_bias
#export
ggsave(
  "maps/puerto_rico_taxonomic_bias.pdf",
  plot = p_taxonomic_bias,
  width = 15,
  height = 10,
  units = "cm"
)


############################################################
# SEASONAL BIAS ANALYSIS
# For data paper publication
############################################################

library(dplyr)
library(ggplot2)

# Ensure month is numeric and valid
occ_with_sf_clean <- occ_with_sf_clean |>
  mutate(month = as.numeric(month)) |>
  filter(!is.na(month), month >= 1, month <= 12)

# Convert month to ordered factor with labels
occ_with_sf_clean <- occ_with_sf_clean |>
  mutate(
    month_factor = factor(
      month,
      levels = 1:12,
      labels = month.abb
    )
  )

############################################################
# 1. SEASONAL BIAS PER FAMILY
############################################################

season_family <- occ_with_sf_clean |>
  filter(!is.na(family)) |>
  count(family, month_factor)

p_season_family <- ggplot(season_family,
                          aes(x = month_factor, y = n, fill = family)) +
  geom_col(position = "stack") +
  labs(
    title = "Seasonal sampling bias per bee family in Puerto Rico",
    x = "Month",
    y = "Number of records",
    fill = "Family"
  ) +
  theme_minimal()

p_season_family

#export
ggsave(
  "maps/puerto_rico_season_family_bias.pdf",
  plot = p_season_family,
  width = 15,
  height = 10,
  units = "cm"
)

############################################################
# 2. SEASONAL BIAS PER DATABASE
############################################################

season_database <- occ_with_sf_clean |>
  filter(!is.na(databaseSource)) |>
  count(databaseSource, month_factor)

p_season_database <- ggplot(season_database,
                            aes(x = month_factor, y = n, fill = databaseSource)) +
  geom_col(position = "stack") +
  labs(
    title = "Seasonal sampling bias per data source",
    x = "Month",
    y = "Number of records",
    fill = "Database"
  ) +
  theme_minimal()

p_season_database

#export
ggsave(
  "maps/puerto_rico_season_database_bias.pdf",
  plot = p_season_database,
  width = 15,
  height = 10,
  units = "cm"
)


############################################################
# 3. CIRCULAR SEASONAL PLOT (Ecology-style)
############################################################

season_total <- occ_with_sf_clean |>
  count(month_factor)

p_circular <- ggplot(season_total,
                     aes(x = month_factor, y = n)) +
  geom_col(fill = "#1f4e79") +
  coord_polar(start = 0) +
  labs(
    title = "Circular seasonal sampling pattern",
    subtitle = "Monthly distribution of occurrence records"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  )

p_circular



############################################################
# SAMPLING EFFORT & TAXONOMIC COMPLETENESS
# For data paper publication
############################################################

library(dplyr)
library(ggplot2)

############################################################
# 1. SAMPLING EFFORT THROUGH TIME BY DATA SOURCE
############################################################

# Clean year
occ_time <- occ_with_sf_clean |>
  mutate(year = as.numeric(year)) |>
  filter(!is.na(year), !is.na(databaseSource)) |>
  count(year, databaseSource)

# Plot: stacked bars
p_sampling_time <- ggplot(
  occ_time,
  aes(x = year, y = n, fill = databaseSource)
) +
  geom_col() +
  labs(
    title = "Sampling effort through time",
    subtitle = "Number of occurrence records per year and data source",
    x = "Year",
    y = "Number of records",
    fill = "Data source"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )

p_sampling_time

#export
ggsave(
  "maps/puerto_rico_sampling_time_biases.pdf",
  plot = p_sampling_time,
  width = 15,
  height = 10,
  units = "cm"
)

############################################################
# 2. TAXONOMIC COMPLETENESS CURVE
############################################################
# (Species accumulation curve)

# --- IMPORTANT ---
# Replace `species` below if your column has a different name
# (e.g. scientificName)

occ_tax <- occ_with_sf_clean |>
  filter(!is.na(scientificName)) |>
  arrange(year) |>
  mutate(record_id = row_number())

# Cumulative number of unique scientificName
occ_completeness <- occ_tax |>
  mutate(
    cumulative_species = cumsum(!duplicated(scientificName))
  ) |>
  group_by(record_id) |>
  summarise(
    records = max(record_id),
    scientificName = max(cumulative_species)
  )

# Plot completeness curve
p_completeness <- ggplot(
  occ_completeness,
  aes(x = records, y = scientificName)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Taxonomic completeness curve",
    subtitle = "Cumulative number of species as a function of sampling effort",
    x = "Number of records",
    y = "Cumulative species richness"
  ) +
  theme_minimal()

p_completeness

#export
ggsave(
  "maps/puerto_rico_species_rarefaction_curve.pdf",
  plot = p_completeness,
  width = 15,
  height = 10,
  units = "cm"
)





############################################################
# FAMILY-LEVEL TAXONOMIC COMPLETENESS CURVES
############################################################

library(dplyr)
library(ggplot2)

# ---- IMPORTANT ----
# Replace `species` if your column is called differently
# (e.g. scientificName)

occ_family_comp <- occ_with_sf_clean |>
  filter(!is.na(family), !is.na(scientificName)) |>
  mutate(year = as.numeric(year)) |>
  arrange(year) |>
  group_by(family) |>
  mutate(
    record_order = row_number(),
    cumulative_species = cumsum(!duplicated(scientificName))
  ) |>
  ungroup()

# Plot curves
p_family_completeness <- ggplot(
  occ_family_comp,
  aes(x = record_order,
      y = cumulative_species,
      color = family)
) +
  geom_line(linewidth = 1) +
  labs(
    title = "Family-level taxonomic completeness",
    subtitle = "Cumulative species richness per bee family",
    x = "Number of records (per family)",
    y = "Cumulative species richness",
    color = "Family"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right"
  )

p_family_completeness

#export
ggsave(
  "maps/puerto_rico_family_raref.pdf",
  plot = p_family_completeness,
  width = 15,
  height = 10,
  units = "cm"
)

############################################################
# END OF SCRIPT
############################################################


#CHAO1 ANALYSIS

############################################################
# CHAO1 RICHNESS ESTIMATION
############################################################

library(dplyr)
library(vegan)

############################################################
# 1. PREPARE SPECIES ABUNDANCE VECTOR
############################################################

# Replace 'species' if needed => scientificName involves genera as well. Consider only species
species_counts <- occ_with_sf_clean |>
  filter(!is.na(specificEpithet)) |>
  count(specificEpithet)

abundance_vector <- species_counts$n

############################################################
# 2. CHAO1 ESTIMATOR
############################################################

chao_result <- estimateR(abundance_vector)

observed_richness <- chao_result["S.obs"]
chao1_estimate    <- chao_result["S.chao1"]
se_chao1          <- chao_result["se.chao1"]

############################################################
# 3. SAMPLING COMPLETENESS
############################################################

sampling_completeness <- observed_richness / chao1_estimate * 100

############################################################
# 4. PRINT RESULTS
############################################################

cat("Observed richness:", observed_richness, "\n") #Observed richness: 39
cat("Chao1 estimate:", round(chao1_estimate, 2), "\n") # Chao1 estimate: 42.75 
cat("Standard error:", round(se_chao1, 2), "\n") # Standard error: 4.2
cat("Sampling completeness (%):",
    round(sampling_completeness, 1), "\n") # Sampling completeness (%): 91.2