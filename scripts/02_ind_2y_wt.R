# ==================================================
# Individual two-year weight
# ==================================================
#
# Purpose:
# Create the SCJS two-year individual weight by combining two years of
# survey data and calibrating the resulting dataset to adult population
# control totals.
#
# This script:
#   - imports single-year individual weighting datasets
#   - combines the two survey years
#   - creates a multi-year preweight
#   - harmonises calibration variables
#   - calibrates the combined dataset to adult population totals
#   - performs quality assurance checks
#   - exports final individual weights
#
# Inputs:
#   - Individual weighting data for year 1
#   - Individual weighting data for year 2
#   - Individual calibration totals
#
# Outputs:
#   - output/SCJSind.csv
#
# Weight variables produced:
#   - SCJS_ind_wt
#   - SCJS_ind_wt_sc
#
# Calibration model:
#   y(year 1) +
#   y(year 2) +
#   PD2:age_sex - 1
#
# Dependencies:
#   - scripts/00_setup.R
#   - scjs_import_data()
#   - scjs_create_calibration_totals()
#   - scjs_create_preweight()
#   - scjs_create_pd2()
#   - scjs_standardise_la_names()
#   - scjs_calibrate_weights()
#   - scjs_distribution_check()
#
# Quality assurance checks:
#   - Preweights sum to the Scotland adult population estimate
#   - Record count unchanged following calibration
#   - Calibrated weights sum to adult population totals
#
# ==================================================

# Setup -------------------------

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute 2y individual weight script")


# Import data -------------------------

# Add message to inform user about progress
message("Import data")

# import single-year weights
y1 <- scjs_import_data(setup$ind_file_y1)
y2 <- scjs_import_data(setup$ind_file_y2)

# combine years
combined <- bind_rows(
  y1 %>%
    mutate(survey_year = config$year_1s,
           !!paste0("y", config$year_1s) := 1,
           !!paste0("y", config$year_2s) := 0),
  y2 %>%
    mutate(survey_year = config$year_2s,
           !!paste0("y", config$year_1s) := 0,
           !!paste0("y", config$year_2s) := 1)
)


# Import calibration totals -------------------------

# import population totals for calibration

totals <- scjs_create_calibration_totals(
  totals_file = config$ind_totals_file,
  data = combined,
  pop_total = config$ind_total
)

# Pre-calibration -------------------------

# Add message to inform user about progress
message("Prepare data for calibration")

# get population totals
y1_pop <- sum(y1$scjs_ind_wt)
y2_pop <- sum(y2$scjs_ind_wt)

# calculate entry weight
combined <- scjs_create_preweight(
  combined,
  "ind_preweight",
  y1_pop,
  y2_pop
)

# harmonise variables for calibration
combined <- combined %>%
  scjs_create_pd2() %>%
  scjs_standardise_la_names()

# ensure capitalisation matched population totals (otherwise calibration will fail)
combined <- combined %>%
  rename(PD2 = pd2,
         LAA = laa,
         HHtype = hhtype,
         HRPAgeBand = hrpageband,
         UrbRur = urbrur)


# check if preweights sum to pop totals
pre_calib <- scjs_distribution_check(combined, preweight)
pre_calib_check <- pre_calib$sum

# Compare the sum to a target (e.g., 100)
if (all.equal(pre_calib_check, config$ind_total) == TRUE) {
  print("Sum of preweight matches total adult population of Scotland, continuing...")
} else {
  stop(paste("Sum of preweight is", pre_calib_check, " and doesn't match pop total (", config$ind_total, 
             "). Halting execution. Check if latest SCJS single-year weights used correct ind totals."))
}

# Calibration -------------------------

# Add message to inform user about progress
message("Calibration")

# recode sex variable to match pop totals
combined <- combined %>%
  mutate(age_sex = trimws(gsub("\\s+", " ", age_sex)),
         age_sex = str_replace(age_sex, "sex\\s*1$", "sexMale"),
         age_sex = str_replace(age_sex, "sex\\s*2$", "sexFemale"))

model1 <- as.formula(paste0("~", 
                            paste0(
                              paste0('y', config$year_1s), 
                              '+', 
                              paste0('y', config$year_2s), 
                              '+ PD2:age_sex -1')))

result <- scjs_calibrate_weights(
  rf.data = combined,
  df.population = totals,
  model = model1
)

names(result$data)[names(result$data)=='preweight.cal'] <- 'SCJS_ind_wt'
result$data$SCJS_ind_wt_sc <- result$data$SCJS_ind_wt * (nrow(combined)/y2_pop)


# QA -------------------------

# Add message to inform user about progress
message("Weight checking")

wt_precheck <- scjs_distribution_check(result$data, "SCJS_ind_wt")
wt_check_count <- wt_precheck$count
wt_check_sum <- wt_precheck$sum

if (all.equal(wt_check_count, nrow(combined)) == TRUE) {
  print("Number of observations correct, continuing...")
} else {
  stop(paste("Number of observations", wt_check_count, "incorrect. Halting execution."))
}

if (all.equal(wt_check_sum, config$ind_total) == TRUE) {
  print("Sum of weights matches population totals, continuing...")
} else {
  stop(paste0("Sum of weights (", wt_check_sum, ") incorrect. Halting execution."))
}


# Export -------------------------

# Add message to inform user about progress
message("Exporting 2y individual weights")

indwts <- result$data %>%
  select(serial, SCJS_ind_wt, SCJS_ind_wt_sc) %>%
  arrange(serial)

write_csv(indwts, here("output", "SCJSind.csv"))

