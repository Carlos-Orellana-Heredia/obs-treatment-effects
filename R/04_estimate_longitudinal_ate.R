# Estimate weighted marginal outcome trajectories with GEE -----------------

cohort <- readr::read_csv("data/synthetic_cohort_weighted.csv", show_col_types = FALSE)
outcomes <- readr::read_csv("data/synthetic_longitudinal_outcomes.csv", show_col_types = FALSE)
truth <- readr::read_csv("data/simulation_truth.csv", show_col_types = FALSE)

analysis_long <- outcomes %>%
  # `outcomes` already contains strategy and strategy_a from the simulation.
  # Join only IPTW to avoid generating strategy_a.x / strategy_a.y columns.
  inner_join(cohort %>% select(patient_id, iptw), by = "patient_id") %>%
  filter(!is.na(event_free)) %>%
  mutate(
    follow_up_month = factor(follow_up_month, levels = c(1, 6, 12)),
    id = factor(patient_id)
  )

# `geeglm()` supports binomial but not quasibinomial. With fractional IPTW,
# binomial() emits the familiar non-integer-frequency warning; it does not
# prevent estimation, so suppress it locally rather than changing the family.
gee_fit <- suppressWarnings(
  geepack::geeglm(
    event_free ~ strategy_a * follow_up_month,
    id = id,
    weights = iptw,
    family = binomial(),
    corstr = "exchangeable",
    data = analysis_long
  )
)

prediction_grid <- tidyr::crossing(
  strategy_a = c(0L, 1L),
  follow_up_month = factor(c(1, 6, 12), levels = c(1, 6, 12))
)

estimated_rates <- emmeans::emmeans(gee_fit, ~ strategy_a | follow_up_month) %>%
  emmeans::regrid(transform = "response") %>%
  summary(infer = TRUE) %>%
  as.data.frame() %>%
  mutate(strategy = if_else(strategy_a == 1, "Strategy A", "Strategy B"))

estimated_rd <- emmeans::emmeans(gee_fit, ~ strategy_a | follow_up_month) %>%
  emmeans::regrid(transform = "response") %>%
  emmeans::contrast(method = "revpairwise") %>%
  summary(infer = TRUE) %>%
  as.data.frame()

readr::write_csv(estimated_rates, "outputs/weighted_longitudinal_rates.csv")
readr::write_csv(estimated_rd, "outputs/weighted_longitudinal_risk_differences.csv")

trajectory_plot <- ggplot2::ggplot(
  estimated_rates,
  ggplot2::aes(x = follow_up_month, y = prob, color = strategy, group = strategy)
) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2.4) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lower.CL, ymax = upper.CL), width = 0.08) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
  ggplot2::labs(
    title = "IPTW-adjusted event-free outcome trajectory",
    subtitle = "Marginal estimates from a weighted GEE model",
    x = "Follow-up month", y = "Estimated event-free probability", color = NULL
  ) +
  ggplot2::theme_classic(base_size = 12)
ggplot2::ggsave("outputs/adjusted_outcome_trajectory.png", trajectory_plot,
                width = 8, height = 5, dpi = 300)

message("Known 365-day simulation ATE: ", round(truth$true_value, 3))
