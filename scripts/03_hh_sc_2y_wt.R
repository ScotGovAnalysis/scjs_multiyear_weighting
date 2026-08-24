# ==================================================
# Household self-completion two-year weight
# ==================================================
#
# Purpose:
# Create the SCJS two-year household self-completion weight by combining
# two years of self-completion survey data and calibrating the resulting
# dataset to household population control totals.
#
# This script:
#   - imports single-year household self-completion weighting datasets
#   - combines the two survey years
#   - creates a multi-year preweight
#   - harmonises calibration variables
#   - calibrates the combined dataset to household population totals
#   - performs quality assurance checks
#   - exports final household self-completion weights
#
# Inputs:
#   - Household self-completion weighting data for year 1
#   - Household self-completion weighting data for year 2
#   - Household self-completion calibration totals
#
# Outputs:
#   - output/SCJShhsc.csv
#
# Weight variables produced:
#   - SCJS_sc_hh_wt
#   - SCJS_sc_hh_wt_sc
#
# Calibration model:
#   y(year 1) +
#   y(year 2) +
#   PD2:HHtype +
#   PD2:HRPAgeBand +
#   LAA:UrbRur - 1
#
# Dependencies:
#   - scripts/00_setup.R
#   - scjs_import_data()
#   - scjs_create_preweight()
#   - scjs_create_pd2()
#   - scjs_standardise_la_names()
#   - scjs_collapse_household_age_band()
#   - scjs_calibrate_weights()
#   - scjs_distribution_check()
#
# Quality assurance checks:
#   - Preweights sum to the Scotland household population estimate
#   - Record count unchanged following calibration
#   - Calibrated weights sum to household population totals
#
# ==================================================

# Setup -------------------------

# Run setup script which loads all required packages and functions and 
# executes the config.R script.

source(here::here("scripts", "00_setup.R"))

# Add message to inform user about progress
message("Execute 2y household self-completion weight script")


# Import data -------------------------

# Add message to inform user about progress
message("Import data")

# import single-year weights
y1 <- scjs_import_data(setup$hh_sc_file_y1)
y2 <- scjs_import_data(setup$hh_sc_file_y2)

# import population totals for calibration
totals <- read_csv(config$hh_sc_totals_file,
                   show_col_types = FALSE,
                   name_repair = "unique_quiet") %>%
  rename(name = 1,
         total = 2) %>%
  select(name, total)

# check if calibration totals align with hh total (their sum should be a total of the hh total)
# if the check is failed, check if the hh totals value is correct
if((sum(totals$total) %% config$hh_total == 0) == TRUE){
  print('Sum of calibration totals is multiple of hh total, continuing...')
} else {
  stop(paste0('Sum of calibration totals (', sum(totals$total), 
              ') is NOT multiple of hh total (', config$hh_total, '). Investigate.'))
}

# Create dataset -------------------------

# Add message to inform user about progress
message("Create dataset")

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


# Pre-calibration -------------------------

# Add message to inform user about progress
message("Prepare data for calibration")

# get population totals
y1_pop <- sum(y1$scjs_sc_hh_wt)
y2_pop <- sum(y2$scjs_sc_hh_wt)

# calculate entry weight
combined <- scjs_create_preweight(
  combined,
  "hhsc_preweight",
  y1_pop,
  y2_pop
)

# harmonise variables for calibration
combined <- combined %>%
  scjs_create_pd2() %>%
  scjs_standardise_la_names() %>%
  scjs_collapse_household_age_band()

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
if (all.equal(pre_calib_check, config$hh_total) == TRUE) {
  print("Sum of preweight matches total household population of Scotland, continuing...")
} else {
  stop(paste0("Sum of preweight is ", pre_calib_check, " and doesn't match pop total (", config$hh_total, 
              "). Halting execution. Check if latest SCJS single-year weights used correct hh totals."))
}

# Calibration -------------------------

# Add message to inform user about progress
message("Calibration")

model1 <- as.formula(paste0("~", 
                            paste0(
                              paste0('y', config$year_1s), 
                              '+', 
                              paste0('y', config$year_2s), 
                              '+ PD2:HHtype+PD2:HRPAgeBand+LAA:UrbRur-1')))

result <- scjs_calibrate_weights(
  rf.data = combined,
  df.population = totals,
  model = model1
)

names(result$data)[names(result$data)=='preweight.cal'] <- 'SCJS_sc_hh_wt'
result$data$SCJS_sc_hh_wt_sc <- result$data$SCJS_sc_hh_wt * (nrow(combined)/y2_pop)

# QA -------------------------

# Add message to inform user about progress
message("Weight checking")

wt_precheck <- scjs_distribution_check(result$data, "SCJS_sc_hh_wt")
wt_check_count <- wt_precheck$count
wt_check_sum <- wt_precheck$sum

if (all.equal(wt_check_count, nrow(combined)) == TRUE) {
  print("Number of observations correct, continuing...")
} else {
  stop(paste("Number of observations", wt_check_count, "incorrect. Halting execution."))
}

if (all.equal(wt_check_sum, config$hh_total) == TRUE) {
  print("Sum of weights matches population totals, continuing...")
} else {
  stop(paste0("Sum of weights (", wt_check_sum, ") incorrect. Halting execution."))
}

# Export -------------------------

# Add message to inform user about progress
message("Exporting 2y household self-completion weights")

hhwts_sc <- result$data %>%
  select(serial, SCJS_sc_hh_wt, SCJS_sc_hh_wt_sc) %>%
  arrange(serial)

write_csv(hhwts_sc, here("output", "SCJShhsc.csv"))

