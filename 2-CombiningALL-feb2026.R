##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset: COMBINING ALL
#          GBIF (cleaned), MZUPRRP, MEBT, SP, EM
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: February 13th 2026
#
##############################################################################################################

setwd("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/") 

# dfMEBT <- read.table("BEEPR-MEBT-clean.csv", header = TRUE,   fileEncoding = "UTF-8",
#                    stringsAsFactors = FALSE)
dfMEBT <- read.delim(
  "BEEPR-MEBT-clean.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)

nrow(dfMEBT) #369
#dfMZU <- read.csv("BEEPR-MZU-clean.csv", header = TRUE)
dfMZU <- read.delim(
  "BEEPR-MZU-clean.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
nrow(dfMZU) #513
#dfGBIF <- read.csv("BEEPR-GBIF-clean-genus.csv", header = TRUE)
dfGBIF <- read.delim(
  "BEEPR-GBIF-clean-genus.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
nrow(dfGBIF) #716
#dfSP <- read.csv("BEEPR-SaraPrado-clean.csv", header = TRUE)
dfSP <- read.delim(
  "BEEPR-SaraPrado-clean.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
nrow(dfSP) #22
#dfEM <- read.csv("BEEPR-EM-clean-forMuseum-beforeAccNb-genus.csv", header = TRUE)
dfEM <- read.delim(
  "BEEPR-EM-clean-forMuseum-beforeAccNb-genus.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
nrow(dfEM) #2370


library(dplyr)

convert_to_char <- function(df) {
  df[] <- lapply(df, as.character)
  df
}

# combined_df <- bind_rows(
#   convert_to_char(dfMEBT),
#   convert_to_char(dfMZU),
#   convert_to_char(dfGBIF),
#   convert_to_char(dfSP),
#   convert_to_char(dfEM)
# )

combined_df <- bind_rows(
  convert_to_char(dfMEBT)  %>% mutate(databaseSource = "MEBT"),
  convert_to_char(dfMZU)   %>% mutate(databaseSource = "MZUPRRP"),
  convert_to_char(dfGBIF)  %>% mutate(databaseSource = "GBIF"),
  convert_to_char(dfSP)    %>% mutate(databaseSource = "SaraPrado_PC"),
  convert_to_char(dfEM)    %>% mutate(databaseSource = "Bees_Of_SJ")
)


nrow(combined_df) #3990
#View(combined_df) 

unique(combined_df$specificEpithet) # ok
unique(combined_df$scientificName) # 84, we need to take out duplicates

###################################################
# SET ALL THE SPECIES NAMES AS THE RIGHT ONES
###################################################
name_mapping <- c(
  "Anthophora tricolor" = "Anthophora tricolor (Fabricius, 1775)",
  "Apis mellifera" = "Apis mellifera Linnaeus, 1758",
  "Centris decolorata" = "Centris decolorata Lepeletier, 1841",
  "Centris haemorrhoidalis" = "Centris haemorrhoidalis (Fabricius, 1775)",
  "Centris lanipes" = "Centris lanipes (Fabricius, 1775)",
  "Centris  lanipes" = "Centris lanipes (Fabricius, 1775)",
  "Centris  haemorrhoidalis" = "Centris haemorrhoidalis (Fabricius, 1775)",
  "Centris smithii" = "Centris smithii Cresson, 1879",
  "Exomalopsis pulchella" = "Exomalopsis pulchella Cresson, 1865",
  "Exomalopsis similis" = "Exomalopsis similis Cresson, 1865",
  "Exomalopsis bahamica" = "Exomalopsis bahamica Timberlake, 1980",
  "Exomalopsis aff. bahamica" = "Exomalopsis aff. bahamica",
  "Exomalopsis analis" = "Exomalopsis analis Spinola, 1853",
  "Melissodes trifasciatus" = "Melissodes trifasciatus Cresson, 1879",
  "Melissodes trifasciata" = "Melissodes trifasciatus Cresson, 1879",
  "Mesoplia bifrons" = "Mesoplia bifrons (Fabricius, 1804)",
  "Mesoplia aff. rufipes" = "Mesoplia aff. rufipes",
  "Nomada pilipes" = "Nomada pilipes (Cresson, 1865)",
  "Nomada krugii" = "Nomada krugii Cresson, 1878",
  "Xylocopa mordax" = "Xylocopa mordax Smith, 1874",
  "Agapostemon viequesensis" = "Agapostemon viequesensis Cockerell, 1918",
  "Augochlora buscki" = "Augochlora buscki Cockerell, 1910",
  "Augochlora busckii" = "Augochlora buscki Cockerell, 1910",
  "Augochlora boriquena" = "Augochlora boriquena Genaro, 2016",
  "Lasioglossum enatum" = "Lasioglossum enatum Gibbs, 2018",
  "Lasioglossum ferrerii" = "Lasioglossum ferrerii (Baker, 1906)",
  "Lasioglossum genaroi" = "Lasioglossum genaroi Gibbs, 2018",
  "Lasioglossum dispersum" = "Lasioglossum dispersum Gibbs, 2018",
  "Lasioglossum rufopanticis" = "Lasioglossum rufopanticis (Engel, 2001)",
  "Lasioglossum gundlachii" = "Lasioglossum gundlachii (Baker, 1906)",
  "Lasioglossum monense" = "Lasioglossum monense Gibbs, 2018",
  "Coelioxys abdominalis" = "Coelioxys abdominalis Guérin-Méneville, 1844",
  "Coelioxys spinosus" = "Coelioxys spinosus Dewitz, 1881",
  "Megachile concinna" = "Megachile concinna Smith, 1879",
  "Megachile holosericea" = "Megachile holosericea (Fabricius, 1793)",
  "Megachile lanata" = "Megachile lanata (Fabricius, 1775)",
  "Megachile luctifera" = "Megachile luctifera Spinola, 1841",
  "Brachymelecta tibialis" = "Brachymelecta tibialis (Fabricius, 1793)",
  "Nesosphecodes anthracinus" = "Nesosphecodes anthracinus Engel, 2006",
  "Sphecodes tainoi" = "Sphecodes tainoi Engel, 2006",
  "Ceratina guarnacciana" = "Ceratina guarnacciana Genaro, 1998",
  "Anthophora krugii" = "Anthophora krugii Cresson, 1879",
  "Lasioglossum eickwortellum" = "Lasioglossum eickwortellum (Engel, 2001)",
  "Augochlora buski" = "Augochlora buscki Cockerell, 1910",
  "Apis" = "Apis Linnaeus, 1758",
  "Apis " = "Apis Linnaeus, 1758",
  "Agapostemon" = "Agapostemon Guérin-Méneville, 1844",
  "Agapostemon " = "Agapostemon Guérin-Méneville, 1844",
  "Augochlora" = "Augochlora Smith, 1853",
  "Centris" = "Centris Fabricius, 1804",
  "Centris " = "Centris Fabricius, 1804",
  "Centris  " = "Centris Fabricius, 1804",
  "Exomalopsis" = "Exomalopsis Spinola, 1853",
  "Exomalopsis " = "Exomalopsis Spinola, 1853",
  "Hylaeus" = "Hylaeus Fabricius, 1793",
  "Hylaeus " = "Hylaeus Fabricius, 1793",
  "Lasioglossum" = "Lasioglossum Curtis, 1833",
  "Lasioglossum " = "Lasioglossum Curtis, 1833",
  "Megachile" = "Megachile Latreille, 1802",
  "Megachile " = "Megachile Latreille, 1802",
  "Sphecodes" = "Sphecodes Latreille, 1804",
  "Sphecodes " = "Sphecodes Latreille, 1804"
)



combined_df$scientificName <- ifelse(
  combined_df$scientificName %in% names(name_mapping),
  name_mapping[combined_df$scientificName],  # replace if mapped
  combined_df$scientificName                 # keep original if not
)
combined_df$scientificName[combined_df$scientificName == "Coelioxys abdominalis Gu√©rin-M√©neville, 1844"] <- "Coelioxys abdominalis Guérin-Méneville, 1844"
combined_df$scientificName[combined_df$scientificName == "Agapostemon Gu√©rin-M√©neville, 1844"] <- "Agapostemon Guérin-Méneville, 1844"

unique(combined_df$scientificName) # 50 ok


#########################################################################################
#    SPECIFIC CASE OF CENTRIS LANIPES => TRANSFORMING IT INTO CENTRIS CF. LANIPES       #
#########################################################################################

old_name <- "Centris lanipes (Fabricius, 1775)"
new_name <- "Centris cf. lanipes"

condition <- combined_df$scientificName == old_name

timestamp <- format(
  as.POSIXct(Sys.time(), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ"
)

# Store previous identification
combined_df$verbatimIdentification[condition] <- old_name

# Update taxonomy
combined_df$scientificName[condition]  <- new_name
combined_df$specificEpithet[condition] <- "cf. lanipes"

# Update metadata
combined_df$modified[condition]       <- timestamp
combined_df$identifiedBy[condition]   <- "Elif Kardas"
combined_df$dateIdentified[condition] <- "2026-02-06"
combined_df$taxonRemarks[condition]   <- paste(
  "The taxonomic history of the 'Centris cf. lanipes group' is very confusing.",
  "A deeper taxonomic study is needed (Felipe Vivallo, pers. comm. 2026).",
  "This group probably contains multiple cryptic species (Elif Kardas, 2026)."
)


unique(combined_df$scientificName) # 50 values

nrow(combined_df) #3990 OK



sort(unique(combined_df$scientificName))



#Details: adding the right name in institutionCode:

library(dplyr)
library(stringr)


# If text might vary slightly, use str_detect() instead of exact matching:
str_detect(combined_df$rightsHolder, "University of Michigan")
str_detect(combined_df$rightsHolder, "Natural History Museum of Los Angeles County")
str_detect(combined_df$rightsHolder, "Sara Guiti-Prado")



combined_df <- combined_df %>%
  mutate(
    institutionCode = case_when(
      
      # 1️⃣ If references starts with ENA URL (put first so it has priority)
      !is.na(references) & str_starts(references, "https://www.ebi.ac.uk/ena") ~ "ENA",
      
      # 2️⃣ The Regents of the University of Michigan
      rightsHolder == "The Regents of the University of Michigan" ~ "UMMNH",
      
      # 3️⃣ Natural History Museum of Los Angeles County
      rightsHolder == "Natural History Museum of Los Angeles County" ~ "NHMLAC",
      
      # 4️⃣ Sara Guiti-Prado
      rightsHolder == "Sara Guiti-Prado, University of North Carolina" ~ "SP-PC",
      
      # 5️⃣ Otherwise keep existing value
      TRUE ~ institutionCode
    )
  )

nrow(combined_df) #3990
#View(combined_df)

# write table
write.table(combined_df, "/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/BEEPR-combined-last.txt", 
            row.names = FALSE, sep = "\t", quote = FALSE, na = "", fileEncoding = "UTF-8")
