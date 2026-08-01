# Simulate a confounded, longitudinal observational cohort ----------------
#
# This script creates entirely synthetic data. Parameter values were chosen
# for demonstration and are not calibrated to a real study or population.

set.seed(20260801)

n_patients <- 2000L
follow_up_days <- c(30L, 180L, 365L)

# Baseline covariates measured before treatment assignment.
cohort <- tibble(
  patient_id = seq_len(n_patients),
  age_years = pmin(pmax(round(rnorm(n_patients, 66, 13)), 18), 95),
  comorbidity_burden = rpois(n_patients, lambda = 1.4),
  baseline_severity = sample(
    c("low", "moderate", "high"), n_patients,
    replace = TRUE, prob = c(0.30, 0.46, 0.24)
  ),
  prior_intervention = rbinom(n_patients, 1, 0.34),
  anatomical_complexity = rbinom(n_patients, 1, 0.42),
  urgent_presentation = rbinom(n_patients, 1, 0.28)
) %>%
  mutate(
    severity_score = case_when(
      baseline_severity == "low" ~ 0,
      baseline_severity == "moderate" ~ 1,
      TRUE ~ 2
    ),
    age_10y = (age_years - 65) / 10
  )

# Treatment assignment is intentionally confounded by baseline risk.
cohort <- cohort %>%
  mutate(
    propensity_true = plogis(
      -0.25 + 0.25 * age_10y + 0.42 * comorbidity_burden +
        0.58 * severity_score + 0.62 * prior_intervention +
        0.48 * anatomical_complexity + 0.36 * urgent_presentation
    ),
    strategy_a = rbinom(n_patients, 1, propensity_true),
    strategy = factor(if_else(strategy_a == 1, "Strategy A", "Strategy B"))
  )

# Counterfactual hazards. Strategy A is beneficial on average but the benefit
# is smaller among individuals with high baseline severity.
cohort <- cohort %>%
  mutate(
    baseline_hazard = exp(
      -7.45 + 0.13 * age_10y + 0.18 * comorbidity_burden +
        0.46 * severity_score + 0.38 * prior_intervention +
        0.30 * anatomical_complexity + 0.24 * urgent_presentation
    ),
    log_hazard_ratio_a = log(0.76) + 0.10 * severity_score,
    hazard_b = baseline_hazard,
    hazard_a = baseline_hazard * exp(log_hazard_ratio_a),
    potential_event_free_a_365 = exp(-hazard_a * 365),
    potential_event_free_b_365 = exp(-hazard_b * 365),
    observed_hazard = if_else(strategy_a == 1, hazard_a, hazard_b),
    event_time_days = rexp(n_patients, rate = observed_hazard)
  )

# Administrative censoring and informative follow-up availability are included
# to make the repeated-outcome data realistic. The core analysis reports the
# observed-data GEE result and explicitly documents this limitation.
cohort <- cohort %>%
  mutate(
    dropout_hazard = exp(
      -7.20 + 0.18 * comorbidity_burden + 0.20 * severity_score +
        0.16 * urgent_presentation - 0.08 * strategy_a
    ),
    dropout_time_days = rexp(n_patients, rate = dropout_hazard),
    censor_time_days = pmin(dropout_time_days, 365),
    observed_time_days = pmin(event_time_days, censor_time_days, 365),
    event_observed = as.integer(event_time_days <= censor_time_days & event_time_days <= 365)
  )

longitudinal_outcomes <- tidyr::crossing(
  patient_id = cohort$patient_id,
  follow_up_days = follow_up_days
) %>%
  left_join(
    cohort %>%
      select(patient_id, strategy, strategy_a, observed_time_days, event_observed,
             dropout_time_days, censor_time_days),
    by = "patient_id"
  ) %>%
  mutate(
    # Use named scheduled visits rather than days / 30, since 365 / 30 is
    # not exactly 12 and would otherwise become an unintended missing factor.
    follow_up_month = dplyr::case_when(
      follow_up_days == 30L ~ 1L,
      follow_up_days == 180L ~ 6L,
      follow_up_days == 365L ~ 12L
    ),
    assessment_observed = dropout_time_days >= follow_up_days |
      (event_observed == 1 & observed_time_days <= follow_up_days),
    event_free = case_when(
      !assessment_observed ~ NA_integer_,
      event_observed == 1 & observed_time_days <= follow_up_days ~ 0L,
      TRUE ~ 1L
    )
  )

simulation_truth <- tibble(
  estimand = "ATE: Strategy A minus Strategy B in 365-day event-free probability",
  true_value = mean(cohort$potential_event_free_a_365 - cohort$potential_event_free_b_365),
  n_patients = n_patients,
  seed = 20260801L
)

readr::write_csv(
  cohort %>%
    select(-baseline_hazard, -hazard_a, -hazard_b, -observed_hazard,
            -potential_event_free_a_365, -potential_event_free_b_365),
  "data/synthetic_cohort.csv"
)
readr::write_csv(longitudinal_outcomes, "data/synthetic_longitudinal_outcomes.csv")
readr::write_csv(simulation_truth, "data/simulation_truth.csv")

message("Synthetic cohort and outcome data written to data/.")
