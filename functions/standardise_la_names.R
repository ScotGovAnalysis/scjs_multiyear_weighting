#' Standardise local authority names
#'
#' Harmonises local authority names used in SCJS weighting by replacing
#' selected local authority codes with a consistent set of names. This
#' ensures geographic variables are comparable across survey years and
#' align with the categories used in calibration.
#'
#' @param data A data frame containing the variables `la_code` and `laa`.
#'
#' @return The input data frame with standardised values in `laa`.
#'
#' @details
#' The following local authority codes are recoded:
#' \itemize{
#'   \item `180` → `"Dundee"`
#'   \item `230` → `"City of Edinburgh"`
#'   \item `235` → `"Na h-Eileanan Siar"`
#' }
#'
#' All other local authority names are left unchanged.
#'
#' @export

scjs_standardise_la_names <- function(data) {
  
  data %>%
    mutate(
      laa = case_when(
        la_code == 180 ~ "Dundee",
        la_code == 230 ~ "City of Edinburgh",
        la_code == 235 ~ "Na h-Eileanan Siar",
        TRUE ~ laa
        )
    )
  
}
