#' Create multi-year entry weights
#'
#' Calculates the initial weight used in multi-year weighting by scaling
#' annual survey pre-weights to the reference population of the most recent
#' survey year. This reproduces the entry weight calculation used in the
#' legacy SCJS multi-year SAS weighting process.
#'
#' @param data A data frame containing the pre-weight variable.
#' @param preweight_var Character string giving the name of the pre-weight
#'   variable to be used in the calculation.
#' @param year_1_population Estimated population total represented by the
#'   first survey year.
#' @param year_2_population Estimated population total represented by the
#'   second (reference) survey year.
#'
#' @return The input data frame with an additional variable,
#'   `preweight`, containing the calculated multi-year entry weight.
#'
#' @details
#' Entry weights are calculated as:
#'
#' \deqn{
#' preweight = \frac{input\_weight}
#' {(year\_1\_population + year\_2\_population)}
#' \times year\_2\_population
#' }
#'
#' This scales the combined sample to the population size of the most
#' recent survey year before calibration.
#'
#' @export

scjs_create_preweight <- function(data, 
                                     preweight_var, 
                                     year_1_population,
                                     year_2_population) {
  
  output <- data %>%
    mutate(preweight = .data[[preweight_var]] /
             (year_1_population + year_2_population) *
             year_2_population)
  
  return(output)
  
}
