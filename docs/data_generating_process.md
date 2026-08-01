# Data-generating process

## Unit, time zero, and treatments

The simulated unit is an individual eligible for either of two strategies at a common baseline (`time zero`). Strategy A and Strategy B are mutually exclusive. The observed strategy is not randomized.

## Baseline covariates

The simulation generates age, comorbidity burden, baseline severity, prior intervention, anatomical complexity, and urgency. These variables are measured before strategy assignment.

They influence both the probability of receiving Strategy A and the risk of the event. This deliberately creates confounding in the crude comparison.

## Outcomes

The primary outcome is event-free status at 1, 6, and 12 months. The survival outcome is time from treatment assignment to event, with administrative censoring at 365 days. Follow-up availability can vary by baseline risk and observed strategy.

## Known causal truth

The code generates counterfactual hazards under both strategies for every individual. The true 365-day ATE is the sample average difference between the two counterfactual event-free probabilities. It is written to `data/simulation_truth.csv` after each run.

## Deliberate exclusions

No actual patients, site identifiers, dates, treatment brands, observed event rates, sample sizes, or results from an external study are used. The numerical parameters are illustrative and should not be interpreted as clinical evidence.

