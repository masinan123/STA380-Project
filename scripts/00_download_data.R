#### Preamble ####
# purpose: downloads and saves the un world population prospects 2024 
#          complete xlsx file locally
# pre-requisites: none

#### Workspace setup ####
library(httr)

#### Paths ####
dir.create("submissions/Data/Raw", showWarnings = FALSE)

file_path <- "submissions/Data/Raw/WPP2024_Complete.xlsx"

#### Data download ####
# note: replace `download_url` with the DIRECT download link for:
# "Complete (estimates and all projection scenarios) (XLSX)"
download_url <- "https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/1_General/WPP2024_GEN_F01_DEMOGRAPHIC_INDICATORS_FULL.xlsx"

resp <- GET(download_url, write_disk(file_path, overwrite = TRUE))
