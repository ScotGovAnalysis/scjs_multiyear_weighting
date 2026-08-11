#' Summarise weight distribution
#'
#' Produces summary statistics for a weight variable to support quality
#' assurance checks during the SCJS weighting process.
#'
#' @param data A data frame containing the weight variable.
#' @param weight_col Weight variable to be summarised. This can be supplied
#'   unquoted using tidy evaluation.
#'
#' @return A one-row tibble containing:
#' \describe{
#'   \item{count}{Number of records.}
#'   \item{sum}{Sum of weights.}
#'   \item{mean}{Mean weight.}
#'   \item{sd}{Standard deviation of weights.}
#'   \item{IQR}{Interquartile range of weights.}
#' }
#'
#' @details
#' Any existing grouping is removed before calculation to ensure a single
#' set of summary statistics is returned for the full dataset.
#'
#' @export

scjs_distribution_check <- function(data, weight_col) {
  
  weight_col <- rlang::ensym(weight_col)
  
  data %>%
    dplyr::ungroup() %>%   # <- removes grouping
    summarise(
      count = n(),
      sum   = sum(!!weight_col, na.rm = TRUE),
      mean  = mean(!!weight_col, na.rm = TRUE),
      sd    = sd(!!weight_col, na.rm = TRUE),
      IQR   = IQR(!!weight_col, na.rm = TRUE)
    )
}
