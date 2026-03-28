library(targets)

tar_source()
tar_option_set(packages = c("tidyverse", "gsynth", "estimatr", "broom"))

set.seed(0)

list(
  tar_target(file, "correlates2-6.csv", format = "file"),
  tar_target(data, get_data(file)),
  tar_target(outcomes, get_outcomes(data)),
  tar_target(datasets, slice_sample(create_datasets_df(data, outcomes), n = 30)),
  tar_target(datasets_with_sims, add_sims_to_df(datasets, n = 50))
)

