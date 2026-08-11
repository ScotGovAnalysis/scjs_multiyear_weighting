#' Import and standardise SCJS input data
#'
#' Imports an SCJS data file and applies a set of standard cleaning and
#' harmonisation steps used throughout the multi-year weighting process.
#' These include standardising variable names, converting numeric-like
#' character variables to numeric format and harmonising household
#' reference person gender categories.
#'
#' @param path File path to the input dataset.
#'
#' @return A tibble containing the imported and cleaned data.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Imports a CSV file using `readr::read_csv()`.
#'   \item Converts all column names to lower case.
#'   \item Converts character columns containing only digits to numeric.
#'   \item Creates a standardised gender variable, `hrpgender1`, with
#'   values `"Male"` and `"Female"`.
#'   \item Removes the original `hrpgender` variable.
#' }
#'
#' This function is intended to provide a consistent starting point for all
#' SCJS multi-year weighting scripts.
#'
#' @export

scjs_import_data <- function(path){
  
  file <- read_csv(path, 
                   show_col_types = FALSE, 
                   guess_max = 100000,
                   name_repair = "unique_quiet") %>% 
    
    # ensure all column names are lower case
    rename_with(tolower) %>%
    
    # mutate columns which only contains digits to numeric
    mutate(across(where(~ all(grepl("^\\d+$", .x))), ~ as.numeric(.x))) %>%
    
    # only import required variables
    select(serial, la_code, laa, age_sex, hhtype, hrpageband, urbrur,
           matches("hh|ind"))
  
  return(file)
  
}
