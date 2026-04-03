##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset: Cleaning & formatting
#          Museo de Entomologia y Biodiversidad Tropical (MEBT), San Juan, Puerto Rico
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: February 2nd 2026
#
##############################################################################################################
library(readr)
#set wd:
setwd("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES")
guess_encoding("jan2026MEBT.csv")
#load data
DBBEEPR <- read.csv("jan2026MEBT.csv", header = TRUE,  fileEncoding = "UTF-8",      # or "UTF-8-BOM" if exported from Excel
                    stringsAsFactors = FALSE)
DBBEEPR <- DBBEEPR %>%
  mutate(across(where(is.character), ~ stri_enc_toutf8(.x)))
# DBBEEPR$modified <- as.Date(DBBEEPR$modified)
#View(DBBEEPR)
nrow(DBBEEPR) # 369

DBBEEPR$modified[DBBEEPR$modified == "2021"] <- "2021-01-01T00:00:00Z"
str(DBBEEPR$modified)
unique(DBBEEPR$modified)
#DBBEEPR$modified <- as.Date(DBBEEPR$modified)
#View(DBBEEPR)

# # all the modifications brought from this script will be noted inside the script itself with the date of the day
# class(DBBEEPR$modified)
# inherits(DBBEEPR$modified, "Date")
# format(DBBEEPR$modified)

#create df
df <- DBBEEPR
# get values for "order"
unique(df$order) # only 1

hym_df <- df

# get values for "country"
unique(hym_df$country) # only 1 ok
#all are already for Puerto Rican data
hymPR_df <- hym_df

# get values for "family"
unique(hymPR_df$family) # 3 values ok

BEEPR_df <- hymPR_df

# to get value for genus
unique(BEEPR_df$genus) # 14
#modify anthophora typo:
#create logical condition
condition <- BEEPR_df$genus == "Anthophora "
#modify with the condition
BEEPR_df$genus[condition] <- "Anthophora"
#date only the modified lines
BEEPR_df$modified[condition] <- format(
  as.POSIXct(Sys.time(), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ"
)
unique(BEEPR_df$genus) # 13 ok


#how many species
unique(BEEPR_df$specificEpithet) # 23
# # take out the undetermined species
# BEEPR_df <- BEEPR_df[!(is.na(BEEPR_df$specificEpithet) | BEEPR_df$specificEpithet == ""), ]
# unique(BEEPR_df$specificEpithet) # 22
# nrow(BEEPR_df) # 363

#change parvum into enatum
# first, we will add in the column "verbatimIdentification" the previous ID:
BEEPR_df$verbatimIdentification[BEEPR_df$specificEpithet == "parvum"] <- "Lasioglossum parvum"
unique(BEEPR_df$verbatimIdentification) # 2: NA and "Lasioglossum parvum" -> OK
# now change parvum into enatum in specificEpithet
BEEPR_df$specificEpithet[BEEPR_df$specificEpithet == "parvum"] <- "enatum"
unique(BEEPR_df$specificEpithet) 
# now change Lasioglossum parvum into Lasioglossum enatum in scientificName
#create logical condition
condition <- BEEPR_df$scientificName == "Lasioglossum parvum"
#modify with the condition
BEEPR_df$scientificName[condition] <- "Lasioglossum enatum"
#date only the modified lines
BEEPR_df$modified[condition] <- format(
  as.POSIXct(Sys.time(), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ"
)
unique(BEEPR_df$scientificName) # 26 => mistake for Anthophora  tricolor

#change the scientificName of Anthophora tricolor
#create logical condition
condition <- BEEPR_df$scientificName == "Anthophora  tricolor"
#modify with the condition
BEEPR_df$scientificName[condition] <- "Anthophora tricolor"
#date only the modified lines
BEEPR_df$modified[condition] <- format(
  as.POSIXct(Sys.time(), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ"
)
unique(BEEPR_df$scientificName) # 25 ok

BEEPR_df$identifiedBy[BEEPR_df$scientificName == "Lasioglossum enatum"] <- "Elif Kardas"
BEEPR_df$dateIdentified[BEEPR_df$scientificName == "Lasioglossum enatum"] <- "2026-02-01"

unique(BEEPR_df$taxonRemarks) 

BEEPR_df$taxonRemarks[BEEPR_df$taxonRemarks == "ID to verify, cf. Gibbs, 2018: \"All specimens belonging to the parvum group examined from Puerto Rico were L. enatum\nsp. nov. or L. monense sp. nov. from Mona Island.\""] <- 
  "From the parvum group. cf. Gibbs, 2018: 'All specimens belonging to the parvum group examined from Puerto Rico were L. enatum nsp. nov. or L. monense sp. nov. from Mona Island.'"

unique(BEEPR_df$taxonRemarks) 
#View(BEEPR_df)




# now do the same for L. busckiellum:
#change parvum into enatum
# first, we will add in the column "verbatimIdentification" the previous ID:
BEEPR_df$verbatimIdentification[BEEPR_df$specificEpithet == "busckiellum"] <- "Lasioglossum busckiellum"
unique(BEEPR_df$verbatimIdentification) # 2: NA, "Lasioglossum parvum", and "Lasioglossum busckiellum" -> OK
# now change busckiellum into enatum in specificEpithet
BEEPR_df$specificEpithet[BEEPR_df$specificEpithet == "busckiellum"] <- "enatum"
unique(BEEPR_df$specificEpithet) # 21
# now change Lasioglossum busckiellum into Lasioglossum enatum in scientificName
#create logical condition
condition <- BEEPR_df$scientificName == "Lasioglossum busckiellum"
#modify with the condition
BEEPR_df$scientificName[condition] <- "Lasioglossum enatum"
#date only the modified lines
BEEPR_df$modified[condition] <- format(
  as.POSIXct(Sys.time(), tz = "UTC"),
  "%Y-%m-%dT%H:%M:%SZ"
)
unique(BEEPR_df$scientificName) # 24 => pas de L busckiellum ni L parvum => OK


BEEPR_df$identifiedBy[BEEPR_df$scientificName == "Lasioglossum enatum"] <- "Elif Kardas"
BEEPR_df$dateIdentified[BEEPR_df$scientificName == "Lasioglossum enatum"] <- "2026-02-01"

unique(BEEPR_df$taxonRemarks) 

BEEPR_df$taxonRemarks[BEEPR_df$verbatimIdentification == "Lasioglossum busckiellum"] <- 
  "From the parvum group. cf. Gibbs, 2018: 'All specimens belonging to the parvum group examined from Puerto Rico were L. enatum nsp. nov. or L. monense sp. nov. from Mona Island.'"

unique(BEEPR_df$taxonRemarks) 
#View(BEEPR_df)

nrow(BEEPR_df) #369

#verify if the dates are set as dates
str(BEEPR_df$modified)
unique(BEEPR_df$modified)


# VERIFY IF ALL THE COLUMNS FROM THE DC ARE PRESENT IN THE FILE:

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
listMEBT <- colnames(BEEPR_df)
listMEBT # the same 198 as in DC
setdiff(listDC, names(BEEPR_df)) # 0 
setdiff(names(BEEPR_df), listDC) # 0 

#to export 
str(BEEPR_df$modified)

# detect where UTF issues arise

library(stringi)

# List all character columns
char_cols <- names(BEEPR_df)[sapply(BEEPR_df, is.character)]

# Detect rows with non-ASCII characters
for(col in char_cols){
  bad_rows <- which(stri_enc_isutf8(BEEPR_df[[col]]) == FALSE |
                      stri_detect_regex(BEEPR_df[[col]], "[^\\p{ASCII}]"))
  if(length(bad_rows) > 0){
    cat("⚠ Column:", col, "has UTF issues at rows:", bad_rows, "\n")
    print(unique(BEEPR_df[[col]][bad_rows]))
  }
}

#use a replacement map:

replacements <- c(
  "‚àö¬∫" = "u",
  "‚àö¬±" = "n",
  "‚àö¬∞" = "a",
  "‚àö‚â†" = "i",
  "‚àö‚â•" = "o",
  "‚àö¬©" = "e",
  "‚Äö√†√∂¬¨‚àû" = "a",
  "‚Äö√†√∂¬¨‚à´" = "u",
  "‚Äö√†√∂‚Äö√¢‚Ä¢" = "o"
)

BEEPR_df <- BEEPR_df %>%
  mutate(across(all_of(char_cols), ~ str_replace_all(.x, replacements)))

BEEPR_df <- BEEPR_df %>%
  mutate(across(all_of(char_cols), ~ stringi::stri_trans_general(.x, "Latin-ASCII")))

write.table(BEEPR_df, "BEEPR-MEBT-clean.txt", row.names = FALSE, sep = "\t", quote = FALSE, na = "", fileEncoding = "UTF-8")


nrow(BEEPR_df) # 369
unique(BEEPR_df$family) # 3
unique(BEEPR_df$genus) #13
unique(BEEPR_df$scientificName) #24



