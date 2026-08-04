##############################################################################################################
#
#          BEEPR - Creating a database of bees from Puerto Rico
#          Museum collection dataset: Cleaning & formatting
#          Sara Prado
#          Elif Kardas (elif.kardas@umons.ac.be) - last update: February 1st 2026
#
##############################################################################################################


#set wd:
setwd("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES")
#load data
DBBEEPR <- read.csv("jan2026SP.csv", header = TRUE, fileEncoding = "UTF-8")
# DBBEEPR$modified <- as.Date(DBBEEPR$modified)
#View(DBBEEPR)
nrow(DBBEEPR) # 22

unique(DBBEEPR$scientificName)
#how many species
unique(DBBEEPR$scientificName) # 17
# # take out the undetermined species
# DBBEEPR <- DBBEEPR[!(is.na(DBBEEPR$specificEpithet) | DBBEEPR$specificEpithet == ""), ]
# unique(DBBEEPR$scientificName) # 16
# nrow(DBBEEPR) # 20




# VERIFY IF ALL THE COLUMNS FROM THE DC ARE PRESENT IN THE FILE:

# get the DC template column list
DCtpl <- read.csv("/Users/elifka/Library/CloudStorage/OneDrive-UMONS/PhDThesis_Elif-2020-2024/BEE-DATABASES/DWC-2026-template.csv", header = TRUE)
# get the column list 
listDC <- colnames(DCtpl)
listDC #198



setdiff(listDC, names(DBBEEPR)) # checking which column is not present in the df we want to reduce
setdiff(names(DBBEEPR), listDC) # checking which column is additional in the df we want to reduce

missing_cols <- setdiff(listDC, names(DBBEEPR))

# create missing columns (filled with NA)
for (col in missing_cols) {
  DBBEEPR[[col]] <- NA
}

# now keep only the requested columns (in the right order)
DBBEEPR <- DBBEEPR[, listDC, drop = FALSE]
#View(DBBEEPR)
listSP<- colnames(DBBEEPR)
listSP # the same 198 as in DC
setdiff(listDC, names(DBBEEPR)) # 0 
setdiff(names(DBBEEPR), listDC) # 0 



#to export 
write.table(DBBEEPR, "BEEPR-SaraPrado-clean.txt", row.names = FALSE, sep = "\t", quote = FALSE, na = "", fileEncoding = "UTF-8")



nrow(DBBEEPR) #22
# get values for "family"
unique(DBBEEPR$family) # 4
unique(DBBEEPR$genus) #11
unique(DBBEEPR$scientificName) #17
