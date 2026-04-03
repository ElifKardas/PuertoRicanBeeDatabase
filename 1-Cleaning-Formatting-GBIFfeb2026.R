##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset: Cleaning & formatting
#          GBIF downloaded in January 2026
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: February 12th 2026
#
##############################################################################################################
########################################
# A. Libraries
########################################
library(dplyr)

########################################
# B. Load data
########################################

library(readr)
GBIFDBallfam <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/2026/combining/GBIFoccurrence-allF.csv", 
                         header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
nrow(GBIFDBallfam) # 1832

GBIFDBBT <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/2026/combining/GBIFoccurrence_Brachymelecta_tibialis.csv", 
                     header = TRUE, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
nrow(GBIFDBBT) # 8

GBIF.BEEPR <- bind_rows(GBIFDBallfam, GBIFDBBT)
nrow(GBIF.BEEPR) # 1840


library(dplyr)
library(stringr)
library(stringi)

# 1️⃣ Identify all character columns
char_cols <- names(GBIF.BEEPR)[sapply(GBIF.BEEPR, is.character)]

# 2️⃣# Detect non-ASCII or mojibake sequences
bad_chars <- list()
for(col in char_cols){
  bad_rows <- which(stri_detect_regex(GBIF.BEEPR[[col]], "[^\\p{ASCII}]"))
  if(length(bad_rows) > 0){
    bad_chars[[col]] <- unique(GBIF.BEEPR[[col]][bad_rows])
  }
}
bad_chars

#Apply automatic transliteration
GBIF.BEEPR <- GBIF.BEEPR %>%
  mutate(across(all_of(char_cols), ~ stringi::stri_trans_general(.x, "Latin-ASCII")))


#Manually create a generalized replacement map for common mojibake
utf_replacements <- c(
  "√°" = "á",
  "√±" = "ñ",
  "√≥" = "ó",
  "√©" = "é",
  "√¶" = "ö",
  "√§" = "ç",
  "√ª" = "ê",
  "√•" = "í",
  "√∏" = "ô",
  "√®" = "ü",
  "√≠" = "è",
  "√≤" = "ú",
  "√∂" = "á"
  # add more as you see in your dataset
)

# 3️⃣ Apply the replacements across all character columns
GBIF.BEEPR <- GBIF.BEEPR %>%
  mutate(across(all_of(char_cols), ~ str_replace_all(.x, utf_replacements)))

# 4️⃣ Convert everything explicitly to UTF-8
GBIF.BEEPR <- GBIF.BEEPR %>% mutate(across(all_of(char_cols), ~ stri_enc_toutf8(.x)))

# ✅ Optional: check if any remaining non-ASCII characters
non_ascii_check <- sapply(GBIF.BEEPR[char_cols], function(x) any(stri_detect_regex(x, "[^\\p{ASCII}]")))
non_ascii_check


#check for duplicates (multiple methods):
nrow(dplyr::intersect(GBIFDBallfam, GBIFDBBT))
nrow(inner_join(GBIFDBallfam, GBIFDBBT))
nrow(intersect(GBIFDBallfam, GBIFDBBT)) # => there are no duplicates

################################################################################
# C. Helper functions (can also be put as a separate file taxon_helpers.R)
################################################################################
update_taxon <- function(df, condition, 
                         new_genus = NULL,
                         new_species = NULL,
                         new_scientificName = NULL,
                         verbatim = NULL,
                         remarks = NULL,
                         rank = NULL,
                         taxonomicStatus = NULL,
                         identifiedBy = "Elif Kardas",
                         dateIdentified = Sys.Date()) {
  
  # UTC timestamp for modification
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Update columns if values are provided
  if (!is.null(verbatim)) df$verbatimIdentification[condition] <- verbatim
  if (!is.null(new_genus)) df$genus[condition] <- new_genus
  if (!is.null(new_species)) df$specificEpithet[condition] <- new_species
  if (!is.null(new_scientificName)) df$scientificName[condition] <- new_scientificName
  if (!is.null(remarks)) df$taxonRemarks[condition] <- remarks
  if (!is.null(rank)) df$taxonRank[condition] <- rank
  if (!is.null(taxonomicStatus)) df$taxonomicStatus[condition] <- taxonomicStatus
  
  # Always update identification metadata
  if (!is.null(identifiedBy)) df$identifiedBy[condition] <- identifiedBy
  if (!is.null(dateIdentified)) df$dateIdentified[condition] <- dateIdentified
  
  # Update modification timestamp
  df$modified[condition] <- timestamp
  
  return(df)
}




########################################################################################################
# Taking out all iNaturalist values
########################################################################################################
nrow(GBIF.BEEPR) #1840
# take out all the ones from iNaturalist
# I would like to know how many time "iNaturalist" appears:
sum(GBIF.BEEPR$institutionCode == "iNaturalist", na.rm = TRUE) #1055 times
GBIF.BEEPR <- GBIF.BEEPR[GBIF.BEEPR$institutionCode != "iNaturalist" |
                           is.na(GBIF.BEEPR$institutionCode), ] #to prevent accidental removal of NA

nrow(GBIF.BEEPR) #785 values without iNaturalist


#create df
df <- GBIF.BEEPR
# get values for "order"
unique(df$order) # 1 single value
# get values for "family"
unique(df$family) # 3
# get values for "specificEpithet"
unique(df$specificEpithet) # 34
unique(df$scientificName) #60 # there is a lot of BOLD name ID, we need to clean that

# Create working copy
BEEPR_df <- df


###############################################################
# Defining timestamp once 
###############################################################
timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

###############################################################
#  Screen for unidentified genera
###############################################################

# Subset rows where genus is NA or ""
genus_missing <- BEEPR_df[is.na(BEEPR_df$genus) | BEEPR_df$genus == "", ]
#View(genus_missing) # there is only one specimen, an Anthophorini, that is not identified to the genus. Let's take it out
BEEPR_df <- BEEPR_df[!(is.na(BEEPR_df$genus) | BEEPR_df$genus == ""), ]

# Check results
unique(BEEPR_df$genus) # 21 values
nrow(BEEPR_df) #784 values of occurrence with genus at least ID and no iNaturalist

###############################################################
# Take out BOLD results (repeated from my personal dataset)
###############################################################
unique(BEEPR_df$scientificName) # there are a few bold ID as scientificName from those I tried to sequence
# list them: 
BOLD_refs <- BEEPR_df[grepl("^https://boldsystems\\.org/index\\.php/", BEEPR_df$references), ]
# View results
#View(BOLD_refs)
#nrow(BOLD_refs) # 36 values, including those I sequenced and were deposited on BOLD while I also have them in my DB

# Keep only rows where occurrenceID does NOT start with "BEEPR..." which are mines (cf. BOLD project "BEEPR...")
nrow(BEEPR_df) #784 values
BEEPR_df <- BEEPR_df[!grepl("^BEEPR", BEEPR_df$occurrenceID), ]
# Check how many rows remain
nrow(BEEPR_df) # 768 values non redundant between DB

# list other BOLD data:
###########################
BOLD_refs <- BEEPR_df[grepl("^https://boldsystems\\.org/index\\.php/", BEEPR_df$references), ]
# View results
#View(BOLD_refs) 
nrow(BOLD_refs) # 20 values are from BOLD

unique(BOLD_refs$family) #3
unique(BOLD_refs$genus) #4
unique(BOLD_refs$specificEpithet) #2: nothing or ferrerii
unique(BOLD_refs$scientificName) #2: nothing or ferrerii
unique(BEEPR_df$scientificName) # it has a lot of BOLD names, let's change that !

# Show rows where scientificName contains "BOLD:"
BOLD_rows <- BEEPR_df[grep("^BOLD:", BEEPR_df$scientificName), ] # not all specimens coming from bold are wrongly named.

# View the result
#View(BOLD_rows)


# #############################################################################################################
# #############################################################################################################
# #############################################################################################################
# # WE WILL CHANGE THE FOLLOWING NAMES:
# 
# # 1. Change genus "Dialictus" to subgenus of Lasioglossum
# # 2. Lasioglossum rufopantex → Lasioglossum rufopanticis (Engel, 2001)
# # 3. Centris versicolor → Centris decolorata (cf. Vivallo 2020)
# # 4. Xylocopa brasilianorum → Xylocopa mordax (cf. Genaro and Franz 2008)
# # 5. Take out Meliponi tribe (does not exist in Puerto Rico)
# # 6. Centris nigerrima (Spinola, 1851) → Centris decolorata Lepeletier, 1841
# # 7. Halictus poeyi Lepeletier, 1841 → Lasioglossum (Dialictus) spp.
# # 8. Exomalopsis globosa (Fabricius, 1793) → Exomalopsis similis Cresson, 1865
# # 9. Brachymelecta californica (Cresson, 1878) → Brachymelecta tibialis (Fabricius, 1793)
# # 10. Anthophora krugii Cresson, 1879 → Anthophora tricolor (Fabricius, 1775)
# # 11. Change Gu√©rin-M√©neville, 1844 → Guérin-Méneville, 1844
# # 12. Dialictus Robertson, 1902 → Lasioglossum Curtis, 1833
# # 13. Halictus Latreille, 1804 (verified as Halictus proangularis) → Lasioglossum ferrerii (Baker, 1906)
# # 14. Xeromelecta tibialis → Brachymelecta tibialis (Fabricius, 1793)
# #############################################################################################################
# #############################################################################################################
# #############################################################################################################




###################################################################################################
# 1. Change genus "Dialictus" to subgenus of Lasioglossum
###################################################################################################

unique(BEEPR_df$genus)  # 21 values

# Move Dialictus to subgenus and update genus
##########################################################
# Condition for Dialictus genus
cond_genus <- BEEPR_df$genus == "Dialictus"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_genus,
  new_genus = "Lasioglossum",
  remarks = "Dialictus (as a genus) was modified as a subgenus of Lasioglossum, cf. Gibbs 2018 - Elif Kardas"
)

# Keep former genus as subgenus
BEEPR_df$subgenus[cond_genus] <- "Dialictus"


unique(BEEPR_df$genus)  # 20 values => OK Dialictus got changed into Lasioglossum

# Clean scientificName and update specificEpithet & taxonRank
##############################################################

# Condition for BOLD entries (example)
cond_bold <- grepl("^BOLD:", BEEPR_df$scientificName)

# Extract specificEpithet from species
BEEPR_df$specificEpithet[cond_bold] <- sapply(
  strsplit(BEEPR_df$species[cond_bold], " "),
  function(x) if (length(x) >= 2) x[2] else NA_character_
)

# Update taxonRank for rows with specificEpithet
cond_species <- !is.na(BEEPR_df$specificEpithet) &
  BEEPR_df$specificEpithet != ""

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_species,
  rank = "SPECIES"
)

# Condition for BOLD references with valid genus
cond_bold <- grepl("^https://boldsystems\\.org/index\\.php/", BEEPR_df$references) &
  !is.na(BEEPR_df$genus) &
  BEEPR_df$genus != ""

# Rebuild scientificName
new_sciname <- ifelse(
  !is.na(BEEPR_df$specificEpithet[cond_bold]) &
    BEEPR_df$specificEpithet[cond_bold] != "",
  paste(BEEPR_df$genus[cond_bold], BEEPR_df$specificEpithet[cond_bold]),
  BEEPR_df$genus[cond_bold]
)

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_bold,
  new_scientificName = new_sciname
)


####################################################################################################
# 2. Lasioglossum rufopantex → Lasioglossum rufopanticis (Engel, 2001)
####################################################################################################
# Condition for specificEpithet correction
cond_se <- BEEPR_df$specificEpithet == "rufopantex"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_se,
  verbatim = "Lasioglossum rufopantex (Engel, 2001)",
  new_species = "rufopanticis",
  remarks = "Corrected original spelling: gender agreement correction according to the International Code of Zoological Nomenclature (ICZN)."
)

# Condition for scientificName update (with or without authority)
cond_sn <- BEEPR_df$scientificName %in% c(
  "Lasioglossum rufopantex (Engel, 2001)",
  "Lasioglossum rufopantex"
)

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_sn,
  new_scientificName = "Lasioglossum rufopanticis (Engel, 2001)"
)


unique(BEEPR_df$scientificName) # 51 values and Lasioglossum rufopanticis (Engel, 2001) is the only name for this species

# Final checks
nrow(BEEPR_df)      # 768 - nothing changed, good
unique(BEEPR_df$genus)  # 20 values



########################################################################################################
# 3. Centris versicolor → Centris decolorata (cf. Vivallo 2020) 
########################################################################################################
# Count how many times "versicolor" appears (the only species with this name - i.e. no other GENUS versicolor)
sum(BEEPR_df$specificEpithet == "versicolor", na.rm = TRUE)  # 39

# Condition for specificEpithet correction
cond_se1 <- BEEPR_df$specificEpithet == "versicolor"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_se1,
  verbatim = "Centris versicolor (Fabricius, 1775)",
  new_species = "decolorata",
  remarks = "Centris versicolor (Fabricius, 1775) in Puerto Rico is a misidentification for Centris decolorata Lepeletier, 1841. cf. Vivallo 2020",
  rank = "SPECIES",
  # optional: can set identifiedBy and dateIdentified manually, but update_taxon already sets these
)

# Condition for scientificName correction
cond_sn1 <- BEEPR_df$scientificName == "Centris versicolor (Fabricius, 1775)"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_sn1,
  new_scientificName = "Centris decolorata Lepeletier, 1841"
)


# Check results
unique(BEEPR_df$specificEpithet) # 34
unique(BEEPR_df$scientificName) #50 ok 


####################################################################################################
# 4. Xylocopa brasilianorum → Xylocopa mordax (cf. Genaro and Franz 2008)
####################################################################################################
# Condition for specificEpithet correction
cond_se <- BEEPR_df$specificEpithet == "brasilianorum"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_se,
  verbatim = "Xylocopa brasilianorum (Linnaeus, 1767)",
  new_species = "mordax",
  remarks = "The only Xylocopa present in Puerto Rico is Xylocopa mordax Smith, 1874, cf. Genaro and Franz 2008"
)

# Condition for scientificName correction
cond_sn <- BEEPR_df$scientificName == "Xylocopa brasilianorum (Linnaeus, 1767)"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_sn,
  new_scientificName = "Xylocopa mordax Smith, 1874"
)

unique(BEEPR_df$scientificName) # 49 values
unique(BEEPR_df$genus) # 20 values

###############################################################################################
# 5. Take out Meliponi tribe (does not exist in Puerto Rico)
###############################################################################################
# take out the genera that are not present in Puerto Rico (like Meliponii, likely a misID)
nrow(BEEPR_df) #768 values before Trigona or Tetragonisca are retrieved
BEEPR_df <- BEEPR_df[
  !(BEEPR_df$genus %in% c("Trigona", "Tetragonisca", "Scaptotrigona")), ]

nrow(BEEPR_df) #716 values WITHOUT Trigona or Tetragonisca or Scaptotrigona

unique(BEEPR_df$genus) # 17 values
unique(BEEPR_df$scientificName) # 45 values


####################################################################################################
# 6. Centris nigerrima (Spinola, 1851) → Centris decolorata Lepeletier, 1841 
# Verified via specimen image (ecdysis record 4120825)
####################################################################################################
# Condition for specificEpithet correction
cond_se3 <- BEEPR_df$specificEpithet == "nigerrima"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_se3,
  verbatim = "Centris nigerrima (Spinola, 1851)",
  new_species = "decolorata",
  remarks = paste(
    "Specimen images (https://ecdysis.org/collections/individual/index.php?occid=4120825)",
    "were reviewed and verified as Centris decolorata Lepeletier, 1841,",
    "not Centris nigerrima (Spinola, 1851)."
  )
)

# Condition for scientificName correction
cond_sn3 <- BEEPR_df$scientificName == "Centris nigerrima (Spinola, 1851)"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_sn3,
  new_scientificName = "Centris decolorata Lepeletier, 1841"
)


unique(BEEPR_df$scientificName) # 44 values
nrow(BEEPR_df) # 716 values

####################################################################################################
# 7. Halictus poeyi Lepeletier, 1841 → Lasioglossum (Dialictus) spp.
####################################################################################################

# Update specificEpithet and related metadata
#############################################
# Condition for species-level removal of "poeyi"
cond_se4 <- BEEPR_df$specificEpithet == "poeyi"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_se4,
  verbatim = "Halictus poeyi Lepeletier, 1841",
  new_genus = "Lasioglossum",
  new_species = "",           # remove species-level ID
  new_scientificName = "Lasioglossum Curtis, 1833",
  remarks = "Likely one of the Lasioglossum (Dialictus) species listed by Gibbs (2018); cf. Genaro and Franz (2008).",
  rank = "GENUS"
)


unique(BEEPR_df$scientificName) #43 values



####################################################################################################
# 8. Exomalopsis globosa (Fabricius, 1793) → Exomalopsis similis Cresson, 1865
####################################################################################################

# Optional check
# View(BEEPR_df[BEEPR_df$scientificName ==
#      "Exomalopsis globosa (Fabricius, 1793)", ])


# Condition for "globosa" → "similis"
cond_se5 <- BEEPR_df$specificEpithet == "globosa"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_se5,
  verbatim = "Exomalopsis globosa (Fabricius, 1793)",
  new_species = "similis",
  new_scientificName = "Exomalopsis similis Cresson, 1865",
  remarks = "Synonym updated to accepted name: Exomalopsis similis Cresson, 1865.",
  taxonomicStatus = "ACCEPTED"
)



unique(BEEPR_df$scientificName) #43 values
nrow(BEEPR_df)  # 716

####################################################################################################
# 9. Brachymelecta californica (Cresson, 1878) → Brachymelecta tibialis (Fabricius, 1793)
####################################################################################################

# Optional check
# View(BEEPR_df[BEEPR_df$scientificName ==
#      "Brachymelecta californica (Cresson, 1878)", ])

# Condition for "californica" → "tibialis"
cond_se6 <- BEEPR_df$specificEpithet == "californica"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_se6,
  verbatim = "Brachymelecta californica (Cresson, 1878)",
  new_species = "tibialis",
  new_scientificName = "Brachymelecta tibialis (Fabricius, 1793)",
  remarks = paste(
    "Brachymelecta tibialis is the easternmost species in the genus",
    "and the only species known to occur in Puerto Rico and the U.S. Virgin Islands;",
    "cf. Onuferko et al. (2021)."
  )
)


unique(BEEPR_df$scientificName) # 43 values



# Clean trailing institutional variant "Brachymelecta tibialis (Fabricius, 1793) Uprm"
##################################################

# Remove extraneous "Uprm" from scientificName
cond_clean <- BEEPR_df$scientificName == "Brachymelecta tibialis (Fabricius, 1793) Uprm"

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = cond_clean,
  new_scientificName = "Brachymelecta tibialis (Fabricius, 1793)"
)

unique(BEEPR_df$scientificName) # 42 values - fine!
nrow(BEEPR_df)  # 716

####################################################################################################
# 10. Anthophora krugii Cresson, 1879 → Anthophora tricolor (Fabricius, 1775)
####################################################################################################

# Optional check
# View(BEEPR_df[BEEPR_df$scientificName ==
#      "Anthophora krugii Cresson, 1879", ])

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = BEEPR_df$scientificName == "Anthophora krugii Cresson, 1879",
  new_species = "tricolor",
  new_scientificName = "Anthophora tricolor (Fabricius, 1775)",
  verbatim = "Anthophora krugii Cresson, 1879",
  remarks = "Anthophora krugii Cresson, 1879 is a synonym of Anthophora tricolor (Fabricius, 1775); cf. Genaro and Franz (2008).",
  rank = "SPECIES",
  taxonomicStatus = "ACCEPTED"
)



unique(BEEPR_df$scientificName) #41 values - ok!
unique(BEEPR_df$specificEpithet) #27 values - ok!
nrow(BEEPR_df)  # 716



####################################################################################################
# 11. change Gu√©rin-M√©neville, 1844 into Guérin-Méneville, 1844
####################################################################################################

# change Gu√©rin-M√©neville, 1844 into Guérin-Méneville, 1844

BEEPR_df$scientificName <- gsub(
  "Gu√©rin-M√©neville",
  "Guérin-Méneville",
  BEEPR_df$scientificName
)


unique(BEEPR_df$scientificName) # 41 values ok



####################################################################################################
# 12. Dialictus Robertson, 1902 → Lasioglossum Curtis, 1833
####################################################################################################

# Optional check
# View(BEEPR_df[BEEPR_df$scientificName ==
#      "Dialictus Robertson, 1902", ])


BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = BEEPR_df$scientificName == "Dialictus Robertson, 1902",
  new_genus = "Lasioglossum",
  new_scientificName = "Lasioglossum Curtis, 1833",
  verbatim = "Dialictus Robertson, 1902",
  remarks = "Dialictus Robertson, 1902 is treated as a subgenus of Lasioglossum Curtis, 1833; cf. Gibbs (2018).",
  rank = "GENUS"
)


unique(BEEPR_df$scientificName) # 40 values - ok!
unique(BEEPR_df$genus) # 17 - but it has Halictus, let's change this


####################################################################################################
# 13. Halictus Latreille, 1804 (verified as Halictus proangularis) → Lasioglossum ferrerii (Baker, 1906)
####################################################################################################
# The only Halictus can be found via the occurenceID USNMENT00535160 of the USNM 
# see also: https://fr.bionomia.net/Q4820667/specimens?action=collected&family=Halictidae
# it has been verified, and this is actually Halictus proangularis.
# the accepted name is now Lasioglossum ferrerii (Baker, 1906)

# Optional check
# View(BEEPR_df[BEEPR_df$scientificName == "Halictus Latreille, 1804", ])

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = BEEPR_df$scientificName == "Halictus Latreille, 1804",
  new_genus = "Lasioglossum",
  new_species = "ferrerii",
  new_scientificName = "Lasioglossum ferrerii (Baker, 1906)",
  verbatim = "Halictus Latreille, 1804",
  remarks = paste(
    "Original identification Halictus Latreille, 1804 verified as Halictus proangularis;",
    "accepted name updated to Lasioglossum ferrerii (Baker, 1906);",
    "cf. USNM occurrence USNMENT00535160, Bionomia."
  ),
  rank = "SPECIES",
  taxonomicStatus = "ACCEPTED"
)


####################################################################################################
# 14. Xeromelecta tibialis → Brachymelecta tibialis (Fabricius, 1793)
# Set genus to Brachymelecta and taxonomicStatus to ACCEPTED
####################################################################################################


# Identify records to update
#############################

BEEPR_df <- update_taxon(
  df = BEEPR_df,
  condition = BEEPR_df$scientificName == "Brachymelecta tibialis (Fabricius, 1793)",
  new_genus = "Brachymelecta",
  verbatim = "Xeromelecta tibialis",
  remarks = paste(
    "The specimen was first identified as Xeromelecta tibialis, a synonym of Brachymelecta tibialis.",
    "It was subsequently updated to Brachymelecta tibialis and the taxonomic status was set to ACCEPTED.",
    "cf. Onuferko et al. 2021"
  ),
  taxonomicStatus = "ACCEPTED"
)



# Checks
unique(BEEPR_df$genus) #15 values
unique(BEEPR_df$taxonomicStatus)


#####################################################
# SET ALL THE SPECIES NAMES WITH THE AUTHORSHIP
#####################################################
#unique(BEEPR_df$scientificName) # 40 values
name_mapping <- c(
  "Apis mellifera" = "Apis mellifera Linnaeus, 1758",
  "Lasioglossum" = "Lasioglossum Curtis, 1833",
  "Lasioglossum ferrerii" = "Lasioglossum ferrerii (Baker, 1906)",
  "Lasioglossum dispersum" = "Lasioglossum dispersum Gibbs, 2018",
  "Lasioglossum enatum" = "Lasioglossum enatum Gibbs, 2018",
  "Lasioglossum genaroi" = "Lasioglossum genaroi Gibbs, 2018")


BEEPR_df$scientificName <- 
  dplyr::recode(BEEPR_df$scientificName, !!!name_mapping)


sort(unique(BEEPR_df$scientificName))








##########################################################################################################
# VERIFY IF ALL THE COLUMNS FROM THE DC ARE PRESENT IN THE FILE:
##########################################################################################################
# get the DC template column list
DCtpl <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/DWC-2026-template.csv", header = TRUE)
# get the column list 
listDC <- colnames(DCtpl)
listDC #198


# BEEPR_df <- BEEPR_df[, listDC, drop = FALSE]
setdiff(listDC, names(BEEPR_df)) # checking which column is not present in the df we want to reduce
setdiff(names(BEEPR_df), listDC) # checking which column is additional in the df we want to reduce

missing_cols <- setdiff(listDC, names(BEEPR_df))

# create missing columns (filled with NA)
for (col in missing_cols) {
  BEEPR_df[[col]] <- NA
}

# now keep only the requested columns (in the right order)
BEEPR_df <- BEEPR_df[, listDC, drop = FALSE]
#View(BEEPR_df)
listGBIF <- colnames(BEEPR_df)
listGBIF # the same 198 as in DC
setdiff(listDC, names(BEEPR_df)) # 0 
setdiff(names(BEEPR_df), listDC) # 0 
# OK
#to export 

str(BEEPR_df$modified)

write.table(BEEPR_df, "BEEPR-GBIF-clean-genus.txt", 
            sep = "\t", row.names = FALSE, quote = FALSE, na = "", fileEncoding = "UTF-8")


nrow(BEEPR_df) #716
unique(BEEPR_df$family) # 3
unique(BEEPR_df$genus) #15
unique(BEEPR_df$scientificName) #33




#
#we want to know how many rows are unidentified up to the species:
#


BEEPR_df %>%
  summarise(missing_or_blank = sum(is.na(specificEpithet) | specificEpithet == "")) # only 72 line is without identification up to the species.

