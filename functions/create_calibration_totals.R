#' Create calibration totals for year-based calibration
#'
#' Reads a calibration totals file, removes any existing year totals,
#' calculates new year totals based on the sample distribution across years,
#' and appends them to the calibration totals.
#'
#' This function is primarily used for weights where year calibration totals
#' are required to be proportional to the achieved sample size in each year
#' rather than being fixed population totals.
#'
#' Existing year controls (e.g. `y22`, `y23`, `y24`, `y25`) are removed from
#' the calibration totals file and recalculated using the distribution of
#' records in the supplied dataset.
#'
#' A validation check is then performed to ensure the calculated year totals
#' sum to the reference year population total.
#'
#' @param totals_file Character string containing the path to the calibration
#'   totals file.
#'
#' @param data Data frame containing the survey records being calibrated.
#'   Must contain a variable called `year`.
#'
#' @param pop_total Vector containing population totals for most recent year.
#'
#' @returns A data frame containing:
#' \itemize{
#'   \item The original calibration totals (excluding existing year totals).
#'   \item Recalculated year totals proportional to the achieved sample
#'         distribution.
#' }
#'
#' @examples
#' \dontrun{
#' totals <- create_calibration_totals(
#'   totals_file = "SHeSadulttotals_sc.csv",
#'   data = adult_sc,
#'   pop_total = config$adult_pop
#' )
#' }

scjs_create_calibration_totals <- function(totals_file, data, pop_total) {
  
  # Read calibration totals ----
  
  totals <- read_csv(totals_file, show_col_types = FALSE,
                     name_repair = "unique_quiet") %>%
    filter(!grepl("^y.{2}$", name)) %>%
    rename(name = 1,
           total = 2) %>%
    select(name, total)
  
  # check if calibration totals align with population total (their sum should be a total of the population total)
  # if the check is failed, check if the population totals value is correct
  if((sum(totals$total) %% pop_total == 0) == TRUE){
    print('Sum of calibration totals is multiple of population total, continuing...')
  } else {
    stop(paste0('Sum of calibration totals (', sum(totals$total), 
                ') is NOT multiple of population total (', pop_total, '). Investigate.'))
  }
  
  # Calculate proportional year totals ----
  
  year_totals <- data %>%
    count(survey_year) %>%
    mutate(
      name = paste0("y", survey_year),
      total = n / sum(n) * pop_total
    ) %>%
    select(name, total)
  
  # Validate year totals ----
  
  if (sum(totals$total) %% sum(year_totals$total) == 0) {
    print("Calibration totals validated successfully, continuing...")
  } else {
    stop(paste0(
      "Calibration total check failed: Proportional totals do not ",
      "align with calibration totals. Please verify the population ",
      "total input."
    )
    )
  }
  
  # Return combined calibration totals ----
  
  bind_rows(totals, year_totals)
  
}
