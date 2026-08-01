# Estimate weighted survival curves and a marginal Cox model ---------------

cohort <- readr::read_csv("data/synthetic_cohort_weighted.csv", show_col_types = FALSE) %>%
  mutate(strategy = factor(strategy, levels = c("Strategy B", "Strategy A")))

weighted_km <- survival::survfit(
  survival::Surv(observed_time_days, event_observed) ~ strategy,
  data = cohort,
  weights = iptw
)

weighted_cox <- survival::coxph(
  survival::Surv(observed_time_days, event_observed) ~ strategy_a,
  data = cohort,
  weights = iptw,
  robust = TRUE
)

readr::write_csv(
  broom::tidy(weighted_cox, exponentiate = TRUE, conf.int = TRUE),
  "outputs/weighted_cox_model.csv"
)

km_summary <- summary(weighted_km)
km_data <- tibble(
  time = km_summary$time,
  survival = km_summary$surv,
  lower = km_summary$lower,
  upper = km_summary$upper,
  strategy = sub("strategy=", "", km_summary$strata)
)

km_plot <- ggplot2::ggplot(km_data, ggplot2::aes(x = time, y = survival, color = strategy)) +
  ggplot2::geom_step(linewidth = 0.9) +
  ggplot2::scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1)) +
  ggplot2::labs(
    title = "IPTW-weighted event-free survival",
    x = "Days from treatment assignment", y = "Event-free probability", color = NULL
  ) +
  ggplot2::theme_classic(base_size = 12)
ggplot2::ggsave("outputs/weighted_survival_curve.png", km_plot, width = 8, height = 5, dpi = 300)

