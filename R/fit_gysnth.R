# data <- targets::tar_read(datasets)$small_data[[1]]
# n_treated <- 5
# n_treat_time_periods <- 8

run_one_sim <- function(data, n_treated = 5, n_treat_time_periods = 8 ) {
  states <- unique(data$state)
  data$treat <- as.numeric(data$state %in% sample(states, n_treated) & max(data$year) - data$year < n_treat_time_periods)

  out <- tryCatch({
    gsc_out <- gsynth(
      outcome ~ treat, data = data, index = c("state","year"),
      inference = "parametric", EM = FALSE, CV = TRUE, se = TRUE,
      force = "two-way"
    )

    em_gsc_out <- gsynth(
      outcome ~ treat, data = data, index = c("state","year"),
      inference = "parametric", EM = TRUE, CV = TRUE, se = TRUE,
      force = "two-way"
    )

    lm_rob_out <- lm_robust(
      outcome ~ treat + as.factor(state) + as.factor(year),
      cluster = state, data = data
    )

    lm_df <- head(as_tibble(broom::tidy(lm_rob_out)), 2) %>%
      tail(1) %>%
      rename_with(~ paste0("lm_", tolower(.x)))

    em_gsc_df <- as_tibble(em_gsc_out$est.avg) %>%
      rename_with(~ paste0("em_gsc_", tolower(.x)))
    em_gsc_df$em_gsc_r <- em_gsc_out$r.cv

    gsc_df <- as_tibble(gsc_out$est.avg) %>%
      rename_with(~ paste0("gsc_", tolower(.x)))
    gsc_df$gsc_r <- gsc_out$r.cv

    bind_cols(lm_df, gsc_df, em_gsc_df)
  }, error = function(e) tibble())

  return(out)
}


add_sims_to_df <- function(datasets, n = 1, n_treated = 5) {
  datasets %>% 
    mutate(
      sims = map(small_data, function(df){
        print("Starting Replicate Loop")
        sims_out <- replicate(n, run_one_sim(df, n_treated = n_treated), simplify = F)  
        print("Ended Replicate Loop")
        sims_df <- bind_rows(sims_out)
      })
    )
}
