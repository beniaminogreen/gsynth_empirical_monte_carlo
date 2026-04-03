library(targets)

tar_source()
tar_option_set(packages = c("ggplot2", "dplyr", "tidyr", "gsynth", "estimatr", "broom"))

set.seed(0)

list(
  tar_target(data, get_data()),
  tar_target(outcomes, get_outcomes(data)),
  tar_target(datasets, slice_sample(create_datasets_df(data, outcomes), n = 30)),
  tar_target(datasets_with_sims, add_sims_to_df(datasets, n = 200))
)

