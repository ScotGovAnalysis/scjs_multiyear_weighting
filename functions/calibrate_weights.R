#' Calibrate survey weights
#'
#' Applies raking calibration to SCJS survey data using a specified
#' calibration model and external population control totals.
#'
#' @param rf.data Survey microdata.
#' @param df.population Population control totals.
#' @param ids Survey identifier variable(s).
#' @param strata Survey strata variable(s).
#' @param model Calibration model formula.
#' @param preweight Input weight variable.
#' @param calfun Calibration method.
#' @param bounds Calibration bounds.
#' @param aggregate.stage Optional aggregation stage.
#' @param sigma2 Optional variance parameter.
#'
#' @return A list containing the calibrated data, population template and
#' population totals used in calibration.


scjs_calibrate_weights <- function(rf.data,
                              df.population,
                              ids = ~serial,
                              strata = NULL,
                              model = NULL,
                              preweight = ~preweight,
                              calfun = "raking",
                              bounds = c(-Inf, Inf),
                              aggregate.stage = NULL,
                              sigma2 = NULL) {
  
  # Ensure variables are factors
  rf.data <- rf.data %>%
    mutate(across(where(is.character), as.factor))
  
  # Create population template
  pop <- pop.template(rf.data, calmodel = model)
  
  # Transpose and clean up
  pop2 <- t(pop)
  colnames(pop2) <- "NA"
  pop.names <- data.frame(name = rownames(pop2))
  
  # Merge population totals
  merge1 <- merge(pop.names, df.population, by = "name", all.x = TRUE, sort = FALSE)
  merge1[!complete.cases(merge1), "total"] <- 0
  
  merge2 <- data.frame(merge1[, "total"])
  rownames(merge2) <- merge1[, "name"]
  merge3 <- t(merge2)
  final_pop <- data.frame(merge3)
  colnames(final_pop) <- colnames(merge3)
  
  rf.data <- as.data.frame(rf.data)  # Force base R data.frame
  rf.data$preweight <- as.numeric(as.character(rf.data$preweight))  # Force clean numeric
  
  # Create survey design object
  des <- e.svydesign(data = rf.data, ids = ids, strata = strata, weights = ~preweight)
  
  # Calibrate
  calr <- e.calibrate(design = des, 
                      df.population = final_pop,
                      calmodel = model,
                      calfun = calfun,
                      bounds = bounds,
                      aggregate.stage = aggregate.stage,
                      sigma2 = sigma2)
  
  # This then creates a separate data file with the output weights added  
  calrdata <- calr$variables					
  list(data=calrdata, poptemp=pop, poptot=final_pop)
}
