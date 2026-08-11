# =============================================================
# 00_setup.R
# Loads all data required libraries and functions for the
# SCJS multiyear weighting process
# =============================================================

# 1 - Load packages ----

library(tidyverse)
library(here)
library(fs)
library(janitor)
library(readr)
library(survey)
library(srvyr)
library(ReGenesees)

# 2 - Load functions from functions folder of SHS Weighting RAP ----

walk(list.files(here("functions"), pattern = "\\.R$", full.names = TRUE), 
     source)

# 3 - Load config file from code folder of SHS Weighting RAP ----

source(here("scripts", "config.R"))

# 4 - Create folders ----

folders <- paste0(here("output"))

walk(folders, ~ if(!file.exists(.x)) dir.create(.x, recursive = TRUE))

# 5 - Non-sensitive file paths needed for SCJS multi-year weighting RAP ----

# initiate list 
setup <- list()

### Paths ----

### Input files (single-year weights) ----

setup$hh_file_y1 <- paste0(config$y1_path, "SCJShh.csv")
setup$hh_file_y2 <- paste0(config$y2_path, "SCJShh.csv")

setup$ind_file_y1 <- paste0(config$y1_path, "SCJSind.csv")
setup$ind_file_y2 <- paste0(config$y2_path, "SCJSind.csv")

setup$hh_sc_file_y1 <- paste0(config$y1_path, "SCJShh_sc.csv")
setup$hh_sc_file_y2 <- paste0(config$y2_path, "SCJShh_sc.csv")

setup$ind_sc_file_y1 <- paste0(config$y1_path, "SCJSind_sc.csv")
setup$ind_sc_file_y2 <- paste0(config$y2_path, "SCJSind_sc.csv")



