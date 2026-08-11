#' Create police division grouping variable (PD2)
#'
#' Derives the police division geography used in SCJS multi-year weighting
#' from local authority codes. This function harmonises police division
#' groupings across survey years, including the updated North East grouping
#' introduced from the 2024-25 survey onwards.
#'
#' @param data A data frame containing the variable `la_code`.
#'
#' @return The input data frame with an additional variable, `pd2`,
#' containing the derived police division group.
#'
#' @details
#' Local authority codes are mapped to the police division groupings used in
#' household and individual calibration. For survey years prior to 2025-26,
#' Aberdeen City and Aberdeenshire/Moray are treated as separate groups.
#' From 2025-26 onwards, these areas are combined into a single
#' `"North East"` group.
#'
#' If a local authority code cannot be matched to a police division,
#' `pd2` is assigned `NA`.
#'
#' @export

scjs_create_pd2 <- function(data) {
  
  data %>%
    mutate(pd2 = case_when(
      config$year_2s >= 25 & la_code %in% 100 ~ "North East",
      config$year_2s < 25 & la_code %in% 100 ~ "Aberdeen City",
      config$year_2s >= 25 & la_code %in% c(110, 300) ~ "North East",
      config$year_2s < 25 & la_code %in% c(110, 300) ~ "Aberdeenshire and Moray",
      la_code %in% c(130, 395) ~ "Argyll and West Dunbartonshire",
      la_code %in% c(190, 310, 370) ~ "Ayrshire",
      la_code %in% 170 ~ "Dumfries and Galloway",
      la_code %in% 230 ~ "Edinburgh City",
      la_code %in% 250 ~ "Fife",
      la_code %in% c(150,240,390) ~ "Forth Valley",
      la_code %in% c(200,220,260) ~ "Greater Glasgow",
      la_code %in% c(235,270,330,360) ~ "Highlands and Islands",
      la_code %in% c(320,380) ~ "Lanarkshire",
      la_code %in% c(280,350) ~ "Renfrewshire and Inverclyde",
      la_code %in% c(120,180,340) ~ "Tayside",
      la_code %in% c(210,290,355,400) ~ "The Lothians and Scottish Borders",
      TRUE ~ NA_character_
      )
    )
  
}
