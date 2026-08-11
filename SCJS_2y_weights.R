
### 0 - Setup ----

# Run setup script which loads all required packages and functions and 
# executes the config.R script.
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}

library(here)

### 1 - Household 2y weights ----

# Runs the household weights script
source(here("scripts", "01_hh_2y_wt.R"))


### 2 - Individual 2y weights ----

# Runs the random adult weights script
source(here("scripts", "02_ind_2y_wt.R"))


### 3 - Household self-completion 2y weights ----

# Runs the random school child weights script
source(here("scripts", "03_hh_sc_2y_wt.R"))


### 4 - Individual self-completion 2y weights ----

# Runs the travel weights script
source(here("scripts", "04_ind_sc_2y_wt.R"))


### 5 - Collate the weights ----

# Runs the manual weight checking script
source(here("scripts", "05_combine_wts.R"))

