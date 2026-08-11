#' Collapse household reference person age bands
#'
#' Harmonises household reference person (HRP) age bands across survey
#' years by combining older age categories into a single "60 plus" group.
#' This ensures consistency of weighting variables when producing
#' multi-year SCJS weights.
#'
#' @param data A data frame containing the variable `hrpageband`.
#'
#' @return The input data frame with harmonised values in `hrpageband`.
#'
#' @details
#' Recodes the following categories to `"60 plus"`:
#' \itemize{
#'   \item `60 to 74`
#'   \item `60plus`
#'   \item `75 plus`
#' }
#'
#' All other values are left unchanged.
#'
#' @export

scjs_collapse_household_age_band <- function(data) {
  
  data %>%
    mutate(
      hrpageband = case_when(
        hrpageband %in% c(
          "60 to 74",
          "60plus",
          "75 plus"
          ) ~ "60 plus",
        TRUE ~ hrpageband
        )
    )
  
}
