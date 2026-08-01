# Estimate stabilized ATE weights and assess overlap and balance -----------

cohort <- readr::read_csv("data/synthetic_cohort.csv", show_col_types = FALSE) %>%
  mutate(
    strategy_a = as.integer(strategy_a),
    baseline_severity = factor(baseline_severity, levels = c("low", "moderate", "high"))
  )

ps_formula <- strategy_a ~ age_10y + comorbidity_burden + baseline_severity +
  prior_intervention + anatomical_complexity + urgent_presentation

weights_ate <- WeightIt::weightit(
  formula = ps_formula,
  data = cohort,
  method = "ps",
  estimand = "ATE",
  stabilize = TRUE
)

cohort <- cohort %>% mutate(iptw = weights_ate$weights)
readr::write_csv(cohort, "data/synthetic_cohort_weighted.csv")

balance <- cobalt::bal.tab(weights_ate, un = TRUE, m.threshold = 0.10)
capture.output(balance, file = "outputs/balance_diagnostics.txt")

balance_plot <- cobalt::love.plot(
  balance,
  stat = "mean.diffs",
  abs = TRUE,
  thresholds = c(m = 0.10),
  title = "Covariate balance before and after IPTW"
)
ggplot2::ggsave("outputs/love_plot.png", balance_plot, width = 8, height = 5, dpi = 300)

weight_plot <- ggplot2::ggplot(cohort, ggplot2::aes(x = iptw, fill = strategy)) +
  ggplot2::geom_histogram(bins = 40, alpha = 0.55, position = "identity") +
  ggplot2::labs(
    title = "Distribution of stabilized ATE weights",
    x = "Stabilized IPTW", y = "Number of individuals", fill = "Observed strategy"
  ) +
  ggplot2::theme_classic(base_size = 12)
ggplot2::ggsave("outputs/weight_distribution.png", weight_plot, width = 8, height = 5, dpi = 300)

