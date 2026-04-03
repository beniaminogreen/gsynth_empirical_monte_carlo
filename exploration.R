library(gsynth)
library(tidyverse)
library(panelView)

# must use old gsynth version
# devtools::install_github("xuyiqing/gsynth@e4525ad")

# read in old 
dataset <- read_csv("correlates2-6.csv")

states <- unique(dataset$state)

outcomes <- names(dataset)[-(1:50)]
outcomes <- dataset[,-(1:50)] %>% 
  select(where(is.numeric)) %>% 
  select(where(function(x){
    mean(is.na(x)) < .8
  })) %>% 
  names()


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
  filter(max_year - min_year > 12) %>% 
  filter(
    mean_na < .02,
    n_distinct > 300
  )
datasets <- datasets %>% 
  mutate(
    small_data = pmap(list(data, min_year, max_year), function(data, min_year, max_year) {
      filter_min_year = max(min_year, max_year - 20)
      data %>% 
        filter(year > filter_min_year)
    })
  )


run_one_sim <- function(data) {
  states <- unique(data$states)
  data$treat <- as.numeric(data$state %in% sample(states, 5) & max(data$year) - data$year < 8)

  gsc_out <- gsynth(
      outcome ~ treat, data = data, index = c("state","year"), 
      inference = "parametric", EM = T, CV = T, se = T, 
      force = "two-way"
  )

  out_df <- as_tibble(gsc_out$est.avg) %>% 
    rename_with(tolower)
  out_df$r <- gsc_out$r.cv
  out_df$gsc <- list(gsc_out)

  return(out_df)
}

sims <- map(1:30, function(x){
    data_idx <- sample(1:nrow(datasets), 1)
    data <- datasets$small_data[[data_idx]] %>% 
      drop_na()

    sims_out <- replicate(30, run_one_sim(data), simplify = F)  
    sims_df <- bind_rows(sims_out)
    sims_df$idx <- data_idx

    sims_df 
  }
)

out <- sims %>% 
  bind_rows() 

mini_out <- out %>% 
  filter(p.value < .01) 


mean(out$p.value < .05)

coverage <- sims %>% 
  bind_rows() %>% 
  summarize(coverage_rate = mean(p.value < .5), .by = idx) 

coverage %>% 
  print(n=100)





