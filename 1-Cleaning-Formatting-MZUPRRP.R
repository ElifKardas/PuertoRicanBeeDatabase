##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset: Cleaning & formatting
#          Museum of Zoology of the University of Puerto Rico, Rio Piedras campus (MZUPRRP), Puerto Rico 
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: Feb 2nd 2026
#
##############################################################################################################

#set wd:
setwd("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES")
#load data
DBBEEPR <- read.csv("jan2026MZU.csv", header = TRUE, fileEncoding = "UTF-8")
# DBBEEPR$modified <- as.Date(DBBEEPR$modified)
#View(DBBEEPR)
nrow(DBBEEPR) # 15546


# # all the modifications brought from this script will be noted inside the script itself with the date of the day
# class(DBBEEPR$modified)
# inherits(DBBEEPR$modified, "Date")
# format(DBBEEPR$modified)

#create df
df <- DBBEEPR
# get values for "order"
unique(df$order) # 37 values

#change all the Hymenoptera without typo MODIF
#create logical condition
condition <- df$order == "Hymenoptera "
#modify with the condition
df$order[condition] <- "Hymenoptera"
unique(df$order) # 36 values
#date onlu the modified lines
df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
#View(df)

#Subset DB with only Hymenoptera
hym_df <- df[df$order == "Hymenoptera", ]
#View(hym_df)
nrow(hym_df) # 2816


# get values for "stateProvince"
unique(hym_df$stateProvince) # 52 values
unique(hym_df$county) # 194 values

#getting only Puerto Rican data
hymPR_df <- hym_df[hym_df$stateProvince == "Puerto Rico" | hym_df$stateProvince == "Puerto Rico " |hym_df$stateProvince == "PR" | hym_df$county == "Puerto Rico", ]
unique(hymPR_df$stateProvince) # 4
unique(hymPR_df$county) # 102

#MODIF StateProvince and county

#1. #create logical condition
condition <- hymPR_df$stateProvince == "Puerto Rico "
#modify with the condition
hymPR_df$stateProvince[condition] <- "Puerto Rico"
#date only the modified lines
hymPR_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
#View(hymPR_df)

#2. #create logical condition
condition <- hymPR_df$stateProvince == "PR"
#modify with the condition
hymPR_df$stateProvince[condition] <- "Puerto Rico"
#date only the modified lines
hymPR_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
#View(hymPR_df)

#3. #create logical condition
condition <- hymPR_df$stateProvince == "US"
#modify with the condition
hymPR_df$stateProvince[condition] <- "Puerto Rico"
#date only the modified lines
hymPR_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
#View(hymPR_df)

unique(hymPR_df$stateProvince) # 1
unique(hymPR_df$county) # 102
#View(hymPR_df)
nrow(hymPR_df) # 1556

#only get bees
# get values for "family"
unique(hymPR_df$family) # 39 values
#fix typos
#create logical condition
condition <- hymPR_df$family == "Apidae "
#modify with the condition
hymPR_df$family[condition] <- "Apidae"
#date only the modified lines
hymPR_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
#View(hymPR_df)

#only select families of bees:
BEEPR_df <- hymPR_df[hymPR_df$family == "Apidae" | hymPR_df$family == "Colletidae" | hymPR_df$family =="Halictidae" | hymPR_df$family == "Megachilidae", ]
unique(BEEPR_df$family) # 4 values
#View(BEEPR_df)
nrow(BEEPR_df) # 523


# to get value for genus
unique(BEEPR_df$genus) # 22


#take out rows for lines with empty vaues for genus
BEEPR2_df <- BEEPR_df[!(is.na(BEEPR_df$genus) | BEEPR_df$genus == ""), ]
unique(BEEPR2_df$genus) # 21

#############################################################
#change all the genus names without typo
#############################################################

#BEEPR2_df$genus[BEEPR2_df$genus == "Xylocpa"] <- "Xylocopa"
#create logical condition
condition <- BEEPR2_df$genus == "Xylocpa"
#modify with the condition
BEEPR2_df$genus[condition] <- "Xylocopa"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#BEEPR2_df$genus[BEEPR2_df$genus == "Xylocopa "] <- "Xylocopa"
#create logical condition
condition <- BEEPR2_df$genus == "Xylocopa "
#modify with the condition
BEEPR2_df$genus[condition] <- "Xylocopa"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#BEEPR2_df$genus[BEEPR2_df$genus == " Xylocopa "] <- "Xylocopa"
#create logical condition
condition <- BEEPR2_df$genus == " Xylocopa "
#modify with the condition
BEEPR2_df$genus[condition] <- "Xylocopa"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#BEEPR2_df$genus[BEEPR2_df$genus == "Apis "] <- "Apis"
#create logical condition
condition <- BEEPR2_df$genus == "Apis "
#modify with the condition
BEEPR2_df$genus[condition] <- "Apis"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#BEEPR2_df$genus[BEEPR2_df$genus == "Centris "] <- "Centris"
#create logical condition
condition <- BEEPR2_df$genus == "Centris "
#modify with the condition
BEEPR2_df$genus[condition] <- "Centris"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#BEEPR2_df$genus[BEEPR2_df$genus == "Lasioglossum "] <- "Lasioglossum"
#create logical condition
condition <- BEEPR2_df$genus == "Lasioglossum "
#modify with the condition
BEEPR2_df$genus[condition] <- "Lasioglossum"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#take out aberrant genusnames
BEEPR2_df <- BEEPR2_df[BEEPR2_df$genus != "Bombus", ]
unique(BEEPR2_df$genus) # 14

########################################################################
#how many species
########################################################################

unique(BEEPR2_df$specificEpithet) # 34
# # take out the undetermined species
# BEEPR2_df <- BEEPR2_df[!(is.na(BEEPR2_df$specificEpithet) | BEEPR2_df$specificEpithet == ""), ]
# unique(BEEPR2_df$specificEpithet) # 33

#MODIF all the specificEpithet names with typo
#BEEPR2_df$specificEpithet[BEEPR2_df$specificEpithet == "melifera"] <- "mellifera"
#create logical condition
condition <- BEEPR2_df$specificEpithet == "melifera"
#modify with the condition
BEEPR2_df$specificEpithet[condition] <- "mellifera"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")

#BEEPR2_df$specificEpithet[BEEPR2_df$specificEpithet == "ferreri"] <- "ferrerii"
#create logical condition
condition <- BEEPR2_df$specificEpithet == "ferreri"
#modify with the condition
BEEPR2_df$specificEpithet[condition] <- "ferrerii"
#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
unique(BEEPR2_df$specificEpithet) # 32

#get all scientificName values
unique(BEEPR2_df$scientificName) # 42

#change all the scientificName names without typo
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Apis  mellifera"] <- "Apis mellifera"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Apis  melifera"] <- "Apis mellifera"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == " Xylocopa  mordax"] <- "Xylocopa mordax"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Lasioglossum  dispersum"] <- "Lasioglossum dispersum"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Lasioglossum  gundlachii"] <- "Lasioglossum gundlachii"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Lasioglossum  enatum"] <- "Lasioglossum enatum"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Lasioglossum ferreri"] <- "Lasioglossum ferrerii"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Coelioxys spinosa"] <- "Coelioxys spinosus"
BEEPR2_df$scientificName[BEEPR2_df$scientificName == "Centris  decolorata"] <- "Centris decolorata"
unique(BEEPR2_df$scientificName) # 34

#create the corrections and condition
corrections <- c(
  "Apis  mellifera"            = "Apis mellifera",
  "Apis  melifera"             = "Apis mellifera",
  " Xylocopa  mordax"          = "Xylocopa mordax",
  "Lasioglossum  dispersum"    = "Lasioglossum dispersum",
  "Lasioglossum  gundlachii"   = "Lasioglossum gundlachii",
  "Lasioglossum  enatum"       = "Lasioglossum enatum",
  "Lasioglossum ferreri"       = "Lasioglossum ferrerii",
  "Coelioxys spinosa"          = "Coelioxys spinosus",
  "Centris  decolorata"        = "Centris decolorata"
)

condition <- BEEPR2_df$scientificName %in% names(corrections)
#modify with the condition
BEEPR2_df$scientificName[condition] <-
  corrections[BEEPR2_df$scientificName[condition]]

#date only the modified lines
BEEPR2_df$modified[condition] <- format(as.POSIXct(Sys.time(), tz = "UTC"),"%Y-%m-%dT%H:%M:%SZ")

unique(BEEPR2_df$scientificName) # 39

#take out uncertain species names
BEEPR2_df <- BEEPR2_df[BEEPR2_df$scientificName != "Lasioglossum enatum?", ]
unique(BEEPR2_df$scientificName) # 38 OK

nrow(BEEPR2_df) #513

str(BEEPR2_df$modified)

unique(BEEPR2_df$modified)



# VERIFY IF ALL THE COLUMNS FROM THE DC ARE PRESENT IN THE FILE:

# get the DC template column list
DCtpl <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/DWC-2026-template.csv", header = TRUE)
# get the column list 
listDC <- colnames(DCtpl)
listDC #198


# BEEPR_df <- BEEPR_df[, listDC, drop = FALSE]
setdiff(listDC, names(BEEPR2_df)) # checking which column is not present in the df we want to reduce
setdiff(names(BEEPR2_df), listDC) # checking which column is additional in the df we want to reduce

missing_cols <- setdiff(listDC, names(BEEPR2_df))

# create missing columns (filled with NA)
for (col in missing_cols) {
  BEEPR2_df[[col]] <- NA
}

# now keep only the requested columns (in the right order)
BEEPR2_df <- BEEPR2_df[, listDC, drop = FALSE]
#View(BEEPR2_df)
listMZU <- colnames(BEEPR2_df)
listMZU # the same 198 as in DC
setdiff(listDC, names(BEEPR2_df)) # 0 
setdiff(names(BEEPR2_df), listDC) # 0 


# verify coordinates

unique(BEEPR2_df$decimalLatitude)

#to export 

#to export dates as character to avoid conversion
# BEEPR2_df$modified <- format(BEEPR2_df$modified, "%Y-%m-%d")
# str(BEEPR2_df$modified)





# detect where UTF issues arise

library(stringi)

# List all character columns
char_cols <- names(BEEPR2_df)[sapply(BEEPR2_df, is.character)]

# Detect rows with non-ASCII characters
for(col in char_cols){
  bad_rows <- which(stri_enc_isutf8(BEEPR2_df[[col]]) == FALSE |
                      stri_detect_regex(BEEPR2_df[[col]], "[^\\p{ASCII}]"))
  if(length(bad_rows) > 0){
    cat("⚠ Column:", col, "has UTF issues at rows:", bad_rows, "\n")
    print(unique(BEEPR2_df[[col]][bad_rows]))
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

BEEPR2_df <- BEEPR2_df %>%
  mutate(across(all_of(char_cols), ~ str_replace_all(.x, replacements)))

BEEPR2_df <- BEEPR2_df %>%
  mutate(across(all_of(char_cols), ~ stringi::stri_trans_general(.x, "Latin-ASCII")))




write.table(BEEPR2_df, "BEEPR-MZU-clean.txt", row.names = FALSE, sep = "\t", quote = FALSE, na = "", fileEncoding = "UTF-8")

nrow(BEEPR2_df) #513
# get values for "family"
unique(BEEPR2_df$family) # 4
unique(BEEPR2_df$genus) #14
unique(BEEPR2_df$scientificName) #38
