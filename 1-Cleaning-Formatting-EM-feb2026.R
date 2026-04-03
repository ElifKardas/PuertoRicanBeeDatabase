##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset: Cleaning & formatting
#          Emily Morris database
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: February 2nd 2026
#
##############################################################################################################
library(dplyr)

EMdf <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/EmilyMorrisUrbanBeeSpecimenList.csv", header = TRUE, fileEncoding = "UTF-8")
nrow(EMdf) # 2385


EMloc <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/EmilyMorris-Locations-GPS.csv", header = TRUE, fileEncoding = "UTF-8")
#View(EMloc)

# change the name of some columns in EMdf to correspond them to the DarwinCore:
names(EMdf)[names(EMdf) == "Location"] <- "location"
EMdf$location <- trimws(EMdf$location)
#delete empty columns (X, X.1, X.2 etc)
EMdf[, c("X", "X.1", "X.2")] <- NULL

#merge/join all the values of lat long from EMloc in EMdf
# first create the column lat and long in EMdf and habitat:
EMdf$latitude <- NA
EMdf$longitude <- NA
EMdf$habitat <- NA

#join
EMdf <- EMdf %>%
  left_join(
    EMloc %>% select(location, latitude, longitude, habitat),
    by = "location"
  )
#View(EMdf)

# clean column names

EMdf <- EMdf %>%
  mutate(
    latitude  = coalesce(latitude.x, latitude.y),
    longitude = coalesce(longitude.x, longitude.y),
    habitat = coalesce(habitat.x, habitat.y)
  ) %>%
  select(-latitude.x, -latitude.y, -longitude.x, -longitude.y, -habitat.x, -habitat.y)

# missing_locations_EMdf <- EMdf %>%
#   filter(is.na(latitude) | is.na(longitude)) %>%
#   distinct(location)
# missing_locations_EMdf

# creating new columns:

# df$year <- 2026
EMdf$type <- "PhysicalObject"
EMdf$modified <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
# language ("en")
EMdf$language <- "en"
# rightsHolder ("")
EMdf$rightsHolder <- "University of Puerto Rico"
# institutionID ("University of Puerto Rico")
EMdf$institutionID <- "Museum of Zoology at the University of Puerto Rico at Río Piedras"
# institutionCode ("MZUPRRP")
EMdf$institutionCode <- "MZUPRRP"
# collectionID ("collectionID")
EMdf$collectionID <- "Insect"
# collectionCode ("In")
EMdf$collectionCode <- "In"
# Where.the.bee.currently.is
names(EMdf)[names(EMdf) == "Where.the.bee.currently.is"] <- "ownerInstitutionCode"
EMdf$ownerInstitutionCode <- "UPRRP"

EMdf$causeOfDeath <- "trap"
EMdf$eventType <- "Survey"
# basisOfRecord ("preserved_specimen")
EMdf$basisOfRecord <- "Preserved_Specimen"

# creating a column occurenceID ("")
EMdf$occurrenceID  <- ""
# creating a column catalogNumber ("")
EMdf$catalogNumber  <- ""
# Bee.ID.Number = recordNumber
names(EMdf)[names(EMdf) == "Bee.ID.Number"] <- "recordNumber"

# Collector = recordedBy
names(EMdf)[names(EMdf) == "Collector"] <- "recordedBy"
library(stringr)
EMdf <- EMdf %>%
  mutate(
    # Remove leading/trailing spaces and collapse multiple spaces inside
    recordedBy = str_squish(recordedBy),
    
    # Convert empty strings to NA
    recordedBy = na_if(recordedBy, "")
  )

EMdf <- EMdf %>%
  mutate(
    # Move "Bowl trap" to samplingProtocol
    samplingProtocol = if_else(recordedBy == "Bowl trap", "Bowl trap", NA_character_),
    
    # Keep only names in recordedBy
    recordedBy = if_else(recordedBy == "Bowl trap", NA_character_, recordedBy),
    
    # Fill missing samplingProtocol with "insect net"
    samplingProtocol = if_else(is.na(samplingProtocol), "Insect net", samplingProtocol)
  )

# individualCount ("1")
EMdf$individualCount <- "1"

#organismQuantityType ("Individual")
EMdf$organismQuantityType <- "Individual"
# Sex = sex
names(EMdf)[names(EMdf) == "Sex"] <- "sex"

# lifeStage ("adult")
EMdf$lifeStage <- "adult"

# projectTitle ("A Checklist of the Bee (Hymenoptera: Anthophila) Species of the San Juan Metropolitan Area, Puerto Rico")
EMdf$projectTitle <- "A Checklist of the Bee (Hymenoptera: Anthophila) Species of the San Juan Metropolitan Area, Puerto Rico"

# preparations ("dry")
EMdf$preparations <- "dry"

# disposition ("In_Collection")
EMdf$disposition <- "In_Collection"

#dataset
EMdf$datasetName <- "SanJuanBees-EM"
EMdf$datasetID <- "SJB-EM"

# month
names(EMdf)[names(EMdf) == "Month"] <- "month"

# year
names(EMdf)[names(EMdf) == "Year"] <- "year"


# add verbatimEventDate
names(EMdf)[names(EMdf) == "Date..mm.dd.yyyy."] <- "verbatimEventDate"
EMdf$verbatimEventDate[EMdf$verbatimEventDate == "late april/early may"] <- "5/1/22"

#clean column
EMdf$verbatimEventDate <- trimws(EMdf$verbatimEventDate)       # remove spaces
EMdf$verbatimEventDate[EMdf$verbatimEventDate == ""] <- NA     # empty → NA


library(lubridate)
# Convert MM/DD/YYYY text to Date
EMdf$verbatimEventDate <- mdy(EMdf$verbatimEventDate)


##  extract day

library(dplyr)

EMdf <- EMdf %>%
  mutate(
    day = day(verbatimEventDate)   # extract day of month
  )


#habitat 
# Create multiple DC columns:
  
EMdf$continent <- "America"
EMdf$islandGroup <- "Puerto Rico Archipelago"
EMdf$island <- "Puerto Rico"
EMdf$country <- "Puerto Rico"
EMdf$countryCode <- "PR"
EMdf$municipality <- ""
names(EMdf)[names(EMdf) == "location"] <- "locationID"

# Latitude
names(EMdf)[names(EMdf) == "latitude"] <- "verbatimLatitude"
EMdf$decimalLatitude <- EMdf$verbatimLatitude 
# Longitude
names(EMdf)[names(EMdf) == "longitude"] <- "verbatimLongitude"
EMdf$decimalLongitude <- EMdf$verbatimLongitude 


# Create multiple DC columns:
EMdf$kingdom <- "Animalia"
EMdf$phylum <- "Arthropoda"
EMdf$class <- "Insecta"
EMdf$order <- "Hymenoptera"
EMdf$superfamily <- "Apoidea"
EMdf$family <- "" # this column will be filled after the genus column is created
EMdf$subfamily <- ""
EMdf$tribe <- ""
EMdf$subtribe <- ""

# EMdf$Bee.Genus = genus
names(EMdf)[names(EMdf) == "Bee.Genus"] <- "genus"

EMdf$subgenus <- ""

# Bee.Species = specificEpithet
names(EMdf)[names(EMdf) == "Bee.Species"] <- "specificEpithet"

names(EMdf)[names(EMdf) == "Species"] <- "verbatimIdentification"
EMdf <- EMdf %>%
  mutate(
    # Remove leading/trailing spaces and collapse multiple spaces inside
    verbatimIdentification = str_squish(verbatimIdentification),
    
    # Convert empty strings to NA
    verbatimIdentification = na_if(verbatimIdentification, "")
  )
unique(EMdf$verbatimIdentification)
unique(EMdf$genus)
EMdf$genus[EMdf$genus == "Megachile?"] <- "Apis"
EMdf$genus[EMdf$genus == "unknown too destroyed"] <- NA
EMdf$genus[EMdf$genus == "lost while IDing"] <- NA
EMdf$genus[EMdf$genus == "Lasoglossum"] <- "Lasioglossum"
EMdf$genus[EMdf$genus == "Lasioglossun"] <- "Lasioglossum"
EMdf <- EMdf %>%
  mutate(
    # Remove leading/trailing spaces and collapse multiple spaces inside
    genus = str_squish(genus),
    
    # Convert empty strings to NA
    genus = na_if(genus, "")
  )

unique(EMdf$genus)

unique(EMdf$specificEpithet)
EMdf$specificEpithet[EMdf$specificEpithet == "sp."] <- ""
EMdf$specificEpithet[EMdf$specificEpithet == "unknown"] <- ""
EMdf$specificEpithet[EMdf$specificEpithet == "ferreri"] <- "ferrerii"
EMdf$specificEpithet[EMdf$specificEpithet == "x.enatum"] <- "enatum"
EMdf$specificEpithet[EMdf$specificEpithet %in% c("gundlacchi", "gundlachhi", "gunlachii")] <- "gundlachii"
EMdf$specificEpithet[EMdf$specificEpithet == "trifasciata"] <- "trifasciatus"
EMdf$specificEpithet[EMdf$specificEpithet == "disperum"] <- "dispersum"

EMdf <- EMdf %>%
  mutate(
    # Remove leading/trailing spaces and collapse multiple spaces inside
    specificEpithet = str_squish(specificEpithet),
    
    # Convert empty strings to NA
    specificEpithet = na_if(specificEpithet, "")
  )
unique(EMdf$genus)


# I would like to add the correct family per genus

genus_to_family <- c(
  "Apis" = "Apidae",
  "Augochlora" = "Halictidae",
  "Centris" = "Apidae",
  "Exomalopsis" = "Apidae",
  "Hylaeus" = "Colletidae",
  "Lasioglossum" = "Halictidae",
  "Megachile" = "Megachilidae",
  "Melissodes" = "Apidae",
  "Nomada" = "Apidae",
  "Sphecodes" = "Halictidae",
  "Xylocopa" = "Apidae"
)

EMdf$family <- genus_to_family[EMdf$genus]
EMdf$family

unique(EMdf$specificEpithet)

unique(EMdf$verbatimIdentification)

# EMdf$taxonRank
library(stringr)  # for str_squish or str_trim
EMdf <- EMdf %>%
  mutate(
    # Remove leading/trailing spaces and treat empty strings as NA
    specificEpithet = str_squish(specificEpithet),
    specificEpithet = na_if(specificEpithet, ""),
    
    genus = str_squish(genus),
    genus = na_if(genus, ""),
    
    taxonRank = case_when(
      !is.na(genus) & !is.na(specificEpithet) ~ "species",
      !is.na(genus) & is.na(specificEpithet)  ~ "genus",
      is.na(genus) & is.na(specificEpithet)   ~ "superfamily"
    )
  )

EMdf$taxonomicStatus <- "ACCEPTED"

EMdf$scientificNameAuthorship

unique(EMdf$specificEpithet)
# Setting the authorship depending on the name
authorship_lookup <- data.frame(
  specificEpithet = c("mellifera", "decolorata", "lanipes", "similis", "analis", "pulchella", "dispersum", "enatum", "gundlachii", "ferrerii", "rufopanticis", "concinna", "lanata", "trifasciatus", "krugii", "mordax"),
  scientificNameAuthorship = c("Linnaeus, 1758", "Lepeletier, 1841", "(Fabricius, 1775)", "Cresson, 1865", "Spinola, 1853", "(Cresson, 1865)", "Gibbs, 2018", "Gibbs, 2018", "(Baker, 1906)", "(Baker, 1906)", "(Engel, 2001)", "Smith, 1879", "(Fabricius, 1775)", "Cresson, 1879", "Cresson, 1878", "Smith, 1874")
)


# Join to your main dataframe
EMdf <- EMdf %>%
  left_join(authorship_lookup, by = "specificEpithet")


EMdf <- EMdf %>%
  rowwise() %>%
  mutate(
    scientificName = paste(c(genus, specificEpithet, scientificNameAuthorship)[!is.na(c(genus, specificEpithet, scientificNameAuthorship))],
                           collapse = " ")
  ) %>%
  ungroup()


# verify if all the columns are ok according to DC

# get the DC template column list
DCtpl <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/DWC-2026-template.csv", header = TRUE)
# get the column list 
listDC <- colnames(DCtpl)
listDC #198


# names of EMdf
# Columns in listDC that are NOT present in EMdf at all
missing_cols <- setdiff(listDC, names(EMdf))
missing_cols #139 columns missing

setdiff(names(EMdf), listDC) # checking which column is additional in the df we want to reduce


for (col in setdiff(listDC, names(EMdf))) {
  EMdf[[col]] <- ""
}

library(dplyr)

EMdf <- EMdf %>%
  mutate(across(all_of(setdiff(listDC, names(EMdf))), ~ ""))
# Reorder EMdf columns to match listDC
EMdf <- EMdf[, listDC]
EMdf <- EMdf[order(EMdf$family, EMdf$verbatimIdentification), ]
write.table(EMdf, "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-EM-clean-forMuseum-beforeAccNb.txt", 
            row.names = FALSE, sep = "\t", quote = FALSE, na = "", fileEncoding = "UTF-8")
nrow(EMdf) #2385
# TAKING OUT ALL THE VALUES NOT ID TO GENUS AT LEAST:
EMdf <- EMdf[!(is.na(EMdf$genus) | EMdf$genus == ""), ]
nrow(EMdf) #2370
write.table(EMdf, "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-EM-clean-forMuseum-beforeAccNb-genus.txt", 
            row.names = FALSE, sep = "\t", quote = FALSE, na = "", fileEncoding = "UTF-8")


# # TAKING OUT ALL THE VALUES NOT ID TO SPECIES:
# EMdf <- EMdf[!(is.na(EMdf$specificEpithet) | EMdf$specificEpithet == ""), ]
# 
# 
# write.table(EMdf, "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-EM-clean.txt", row.names = FALSE, sep = "\t", quote = FALSE, na = "")
