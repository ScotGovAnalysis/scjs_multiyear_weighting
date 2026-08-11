# ==================================================
# Combine SCJS two-year weights
# ==================================================
#
# Purpose:
# Combine the final SCJS two-year weight files into a single dataset
# containing household, individual, household self-completion and
# individual self-completion weights.
#
# This script:
#   - imports the final weight files produced by each weighting script
#   - merges all weight variables using serial number
#   - exports a consolidated file for dissemination and analysis
#
# Inputs:
#   - output/SCJShh.csv
#   - output/SCJSind.csv
#   - output/SCJShhsc.csv
#   - output/SCJSindsc.csv
#
# Outputs:
#   - output/AllMYWeights_YYYYMMDD.csv
#
# Weight variables combined:
#   - SCJS_hh_wt
#   - SCJS_hh_wt_sc
#   - SCJS_ind_wt
#   - SCJS_ind_wt_sc
#   - SCJS_sc_hh_wt
#   - SCJS_sc_hh_wt_sc
#   - SCJS_ind_sc_wt
#   - SCJS_ind_sc_wt_sc
#
# Dependencies:
#   - scripts/00_setup.R
#
# Quality assurance checks:
#   - Verify all input weight files exist
#   - Check for duplicate serial numbers
#   - Confirm expected weight variables are present
#   - Validate row counts following merge
#
# ==================================================

# Setup -----------------------

source(here::here("scripts", "00_setup.R"))

# Import weights -----------------------

# Add message to inform user about progress
message("Import weights")

hh <- read_csv(here("output", "SCJShh.csv"), show_col_types = FALSE)
ind <- read_csv(here("output", "SCJSind.csv"), show_col_types = FALSE)
hh_sc <- read_csv(here("output", "SCJShhsc.csv"), show_col_types = FALSE)
ind_sc <- read_csv(here("output", "SCJSindsc.csv"), show_col_types = FALSE)

# Combine -----------------------

# Add message to inform user about progress
message("Combine weights")

combined_wts <- hh %>%
  full_join(ind, by = "serial") %>%
  full_join(hh_sc, by = "serial") %>%
  full_join(ind_sc, by = "serial") %>%
  select(serial, SCJS_hh_wt, SCJS_ind_wt, SCJS_sc_hh_wt, SCJS_ind_sc_wt,
         SCJS_hh_wt_sc, SCJS_ind_wt_sc, SCJS_sc_hh_wt_sc, SCJS_ind_sc_wt_sc)

# Export -----------------------

# Add message to inform user about progress
message("Export weights")

write_csv(
  combined_wts,
  here("output",
       paste0("SCJSMYWeights_",
              gsub('_', '', config$year_1),
              '_',
              gsub('_', '', config$year_2),
              '_',
              format(Sys.Date(),"%Y%m%d"),
              ".csv")
       )
  )
