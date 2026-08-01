# Describe the simulated cohort -------------------------------------------

cohort <- readr::read_csv("data/synthetic_cohort.csv", show_col_types = FALSE)

cohort_summary <- cohort %>%
  group_by(strategy) %>%
  summarise(
    n = n(),
    mean_age = mean(age_years),
    mean_comorbidity_burden = mean(comorbidity_burden),
    high_severity = mean(baseline_severity == "high"),
    prior_intervention = mean(prior_intervention),
    anatomical_complexity = mean(anatomical_complexity),
    .groups = "drop"
  )

readr::write_csv(cohort_summary, "outputs/cohort_summary_unadjusted.csv")

