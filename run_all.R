# Run the full synthetic observational treatment-effects workflow.

source("R/00_packages.R")
source("R/01_simulate_data.R")
source("R/02_describe_cohort.R")
source("R/03_estimate_iptw.R")
source("R/04_estimate_longitudinal_ate.R")
source("R/05_analyze_time_to_event.R")
source("R/06_sensitivity_analyses.R")

