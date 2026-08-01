# Compare untrimmed and trimmed weights ------------------------------------

cohort <- readr::read_csv("data/synthetic_cohort.csv", show_col_types = FALSE) %>%
  mutate(baseline_severity = factor(baseline_severity, levels = c("low", "moderate", "high")))

ps_formula <- strategy_a ~ age_10y + comorbidity_burden + baseline_severity +
  prior_intervention + anatomical_complexity + urgent_presentation

weight_models <- list(
  stabilized_ate = WeightIt::weightit(ps_formula, data = cohort, method = "ps", estimand = "ATE", stabilize = TRUE),
  stabilized_ate_trimmed = WeightIt::weightit(ps_formula, data = cohort, method = "ps", estimand = "ATE", stabilize = TRUE, trim = 0.01)
)

weight_summary <- dplyr::bind_rows(lapply(names(weight_models), function(model_name) {
  weights <- weight_models[[model_name]]$weights
  tibble(
    specification = model_name,
    min_weight = min(weights),
    p01_weight = unname(stats::quantile(weights, 0.01)),
    median_weight = stats::median(weights),
    p99_weight = unname(stats::quantile(weights, 0.99)),
    max_weight = max(weights),
    effective_sample_size = (sum(weights)^2) / sum(weights^2)
  )
}))

readr::write_csv(weight_summary, "outputs/weight_sensitivity_summary.csv")

