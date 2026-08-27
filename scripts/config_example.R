#########################################################################
# Name of file - config.R
#
# Type - Reproducible Analytical Pipeline (RAP)
# Written/run on - RStudio Desktop
# Version of R - 4.4.2
#
# Description - Specifies file paths, file names.
# This is the only file which requires manual changes before the 
# RAP process is run. It is not pushed to git as it contains 
# sensitive information.
#########################################################################

### 1 - Sample year - TO UPDATE ----

# initiate list 
config <- list()

### 2 - Survey years - TO UPDATE ----

# Survey years

config$year_1 <- "20XX_XX"
config$year_1s <- "XX" # weighting folder in datashare the survey weights are saved in

config$year_2 <- "20XX_XX"
config$year_2s <- "XX" # weighting folder in datashare the survey weights are saved in

### 3 - File paths - TO UPDATE ----

# This section may need to be updated if the data storage location has changed

# Path to data share
config$datashare.path <- '//path/to/datashare/'

# Path to SAS data
config$sasdata.path <- '//path/to/SAS data/'

# the following path is for the data
# this path doesn't usually change, only if reruns are required
config$y1_path <- paste0(config$datashare.path, 20, config$year_1s, " Weighting/SCJS ", gsub("_", "-", config$year_1), '/V2/')
config$y2_path <- paste0(config$datashare.path, 20, config$year_2s, " Weighting/SCJS ", gsub("_", "-", config$year_2), '/')

### 4 - Calibration files - TO UPDATE ----

config$hh_totals_file <- "//path/to/hhtotals"
config$hh_sc_totals_file <- "//path/to/hhsctotals"

config$ind_totals_file <- "//path/to/indtotals"
config$ind_sc_totals_file <- "//path/to/indsctotals"
  
# 5 - Population totals - TO UPDATE ----

# get population totals from population construction workbook
config$hh_total <- XXX
config$ind_total <- XXX
