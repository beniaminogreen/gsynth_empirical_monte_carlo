get_data <- function(file) {
  read_csv(file)
}

#' Select outcomes that can be used for gsynth
get_outcomes <- function(dataset) {
  outcomes <- names(dataset)[-(1:50)]
  outcomes <- dataset[,-(1:50)] %>% 
    select(where(is.numeric)) %>% 
    select(where(function(x){
      mean(is.na(x)) < .8
    })) %>% 
    names()

  return(outcomes)
}

create_datasets_df <- function(dataset, outcomes, max_time_periods = 20, min_time_periods = 12) {
  sim_df <- tibble(
    outcomes = outcomes
  )  %>% 
    mutate(
      data = map(outcomes, function(x){
      df <- dataset %>% 
          select(state, year, all_of(x)) 
      names(df) <- c("state", "year", "outcome")
      df
      })
    ) %>% 
    mutate(
      min_year = map_dbl(data, ~min(.x$year[!is.na(.x$outcome)], na.rm = T)),
      max_year = map_dbl(data, ~max(.x$year[!is.na(.x$outcome)], na.rm = T))
    )

# subset datasets to only rows where we have data (no time periods before / after data is avaliable)
  sim_df <- sim_df %>% 
    mutate(
      data = pmap(list(data, min_year, max_year), function(data, min_year, max_year){ 
        data %>% 
          filter(year > min_year, year < max_year)
      })
    )

  datasets <- sim_df %>% 
    mutate(
      mean_na = map_dbl(data, function(data){mean(is.na(data$outcome))}), 
      n_distinct = map_dbl(data, function(data){n_distinct(data$outcome)}), 
    ) %>% 
    filter(max_year - min_year > min_time_periods) %>% 
    filter(
      mean_na < .02,
      n_distinct > 300
    )
  datasets <- datasets %>% 
    mutate(
      small_data = pmap(list(data, min_year, max_year), function(data, min_year, max_year) {
        filter_min_year = max(min_year, max_year - max_time_periods)
        data %>% 
          filter(year > filter_min_year) %>% 
          drop_na()
      })
    )

  return(datasets)
}
