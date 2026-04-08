library(tidyverse)
library(MASS)
library(panelView)
library(gsynth)

make_kernel_matrix2 <- function(n, bw = 10, multiplier = 3.0, sigma = 1.0) {
  out <- matrix(0, n, n)

  for (i in 1:n) {
    for (j in 1:i) {
      cor <- multiplier * exp(-abs(i - j)^2 / bw)
      out[i, j] <- cor
      out[j, i] <- cor
    }
  }

  out
}

n_treat <- 40
n_control <- 200
n_pre <- 40
n_post <- 40
n_time <- n_pre + n_post
n_units <- n_treat + n_control
multiplier <-  4
bw <- 300
k_mat <- make_kernel_matrix2(n_time, bw = bw, multiplier = multiplier)

generate_dataset <- function(){
  y_no_noise <- mvrnorm(n_treat + n_control, rep(0, n_time), k_mat)
  N <- n_time*n_units
  y <- numeric(N)
  units <- numeric(N)
  times <- numeric(N)
  idx <- 1
  for (i in 1:n_units){
    for (t in 1:n_time){
      y[idx] <- y_no_noise[i,t]
      units[idx] <- i
      times[idx] <- t
      idx <- idx + 1
    }
  }

  df <- tibble(
    y=y, 
    id = units, 
    t = times
  ) %>% 
    mutate(
      y_obs = y + rnorm(n(),0,.1), 
      treat = as.numeric(id <= n_treat & t > n_pre)
  )
}

set.seed(1)
datasets  <- replicate(100, generate_dataset(), simplify = FALSE)

flagship_ds <- datasets[[1]]

gs_no_em <- gsynth(y_obs ~ treat, index = c("id", "t"), data = flagship_ds, 
  CV = T,EM = F, se=T, inference = "parametric", r=c(0,15),
)
gs_em <- gsynth(y_obs ~ treat, index = c("id", "t"), data = flagship_ds, 
  CV = T,EM = T, se=T, inference = "parametric", r=c(0,15),
)
gs_em_fix <- gsynthEMfix::gsynth(y_obs ~ treat, index = c("id", "t"), data = flagship_ds, 
  CV = T,EM = T, se=T, inference = "parametric", r=c(0,15),
)


resids <- gs_em$res.co
res <- c(gs_em$res.co)
y <- c(gs_em$Y.co)
RSS <- sum(res^2)
TSS <- sum((y-mean(y))^2)
1 - RSS/TSS



gs_em_df <- as_tibble(gs_em$est.avg) %>% 
  mutate(method = "IFE-EM (Original)")
gs_em_fix_df <- as_tibble(gs_em_fix$est.avg) %>% 
  mutate(method = "IFE-EM (Corrected)")
gs_no_em_df <- as_tibble(gs_no_em$est.avg) %>% 
  mutate(method = "GSC")


bind_rows(gs_em_df, gs_no_em_df, gs_em_fix_df) %>% 
  rename_with(tolower) %>% 
  mutate(method = factor(method, levels = c("IFE-EM (Original)", "IFE-EM (Corrected)", "GSC"))) %>% 
  ggplot(aes(x= method, y = estimate, ymin = ci.lower, ymax = ci.upper)) + 
  geom_pointrange() + 
  geom_hline(yintercept = 0, linetype = 2) + 
  theme_bw()  + 
  ggtitle("Point Estimates + 95% CI") + 
  xlab("Method") + 
  ylab("ATT Estimate")  + 
  scale_x_discrete(breaks = c("IFE-EM (Original)", "IFE-EM (Corrected)", "GSC"))
ggsave("gsc_with_without_em.png")


plot(gs_em) + 
  xlab("Time") + 
  ylab("ATT Estimate")
ggsave("gsc_estimate_long_run_correlation.png")

panelview(y_obs ~ treat, index = c("id", "t"), data = flagship_ds, type = "outcome")
ggsave("raw_data.png")

# gsc_estimates <- map(datasets, function(ds) {
#   gs <- gsynth(y_obs ~ treat, index = c("id", "t"), data = ds, 
#     CV = T,EM = T, se=T, inference = "parametric", r=c(0,10),
#   )
#   return(gs)
# })
#
#
# # rss <- sum(c(gs$res.co)^2)
# # ctrl_data <- flagship_ds %>%
# #   mutate(ever_treat = any(treat ==1), .by = id) %>% 
# #   filter(!ever_treat)
# # tss <- var(ctrl_data$y_obs)*nrow(ctrl_data)
# # 1- rss/tss
#
# #
# #
# # df <- flagship_ds
# # outer_boots <- function(df, M = 100) {
# #   lm_model <- lm(y_obs ~ as.factor(id) + as.factor(t), data = df)
# #   residuals <- residuals(lm_model)
# #   preds <- predict(lm_model)
# #
# #   df$resids <- residuals
# #   df$preds <- preds
# #
# #   replicate(M, {
# #     multiplier_df <- tibble(
# #       id = unique(df$id), 
# #     ) %>% 
# #     mutate(mutiplier = rnorm(n()))
# #
# #     out <- inner_join(df,multiplier_df, by = "id") %>% 
# #       mutate(
# #         a = preds + (resids * multiplier)
# #       )
# #     out
# #   }, simplify = F)
# # }
# #
# # outer_boots(flagship_ds, 100)
# #
# #
# #
# # gsc_df <- as_tibble(gs_em$est.avg) %>% 
# #   rename_with(tolower) %>% 
# #   mutate(method = "gsynth")
# #
# # #
# # tidy_rob_df <- broom::tidy(estimatr::lm_robust(y_obs ~ treat + as.factor(id) + as.factor(t), data = flagship_ds, cluster = id)) %>% 
# #   as_tibble()
# # tidy_lm_df <- broom::tidy(lm(y_obs ~ treat + as.factor(id) + as.factor(t), data = flagship_ds)) %>% 
# #   as_tibble()
# #
# #
# # rob_lm_df <- tibble(
# #   estimate = tidy_rob_df$estimate[2],
# #   ci.lower = tidy_rob_df$conf.low[2],
# #   ci.upper = tidy_rob_df$conf.high[2],
# #   method = "TWFE + Clustered Standard Errors"
# # )
# # lm_df <- tibble(
# #   estimate = tidy_lm_df$estimate[2],
# #   ci.lower = estimate - 2*tidy_lm_df$std.error[2],
# #   ci.upper = estimate +2*tidy_lm_df$std.error[2],
# #   method = "TWFE"
# # )
# #
# # bind_rows(gsc_df, lm_df, rob_lm_df) %>% 
# #   ggplot(aes(x= method, y = estimate, ymin = ci.lower, ymax=ci.upper)) + 
# #   geom_pointrange() + 
# #   theme_bw() + 
# #   geom_hline(yintercept = 0, linetype = 2) + 
# #   ggtitle("Comparison of GSC and TWFE Point Estimates") + 
# #   xlab("Error") + 
# #   ylab("Point Estimate + 95% CI")
# # ggsave("comparison.png")
# # #
# # # panelview(y_obs ~ treat, index = c("id", "t"), data = flagship_ds, type = "outcome") + 
# # #   theme_bw() + 
# # #   theme(legend.position = "none") + 
# # #   xlab("Time") +
# # #   ylab("Outcome") + 
# # #   ggtitle("Simulated Data, Serially-Correlated Noise") 
# # # ggsave("raw_data.png")
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # #
# # # ggplot() + 
# # #   geom_line(data = flagship_ds, aes(x=t, y=y_obs, group = id), alpha = .13) + 
# # #   theme_bw() + 
# # #   geom_line(data = group_mean_df, aes(x=t, y = group_mean, group = ever_treat))
# # #
# # #
# # #
# # #
# # # gsynth_objects <- map(datasets, function(data){
# # #   gsynth(y_obs ~ treat, index = c("id", "t"), data = data, CV = T,EM = T, se=T, inference = "parametric", r = c(0,10))
# # # })
# # #
# # # result_df <- gsynth_objects %>% 
# # #   map(~as_tibble(.x$est.avg))  %>% 
# # #   bind_rows()  %>% 
# # #   rename_with(tolower)
# # #
# # # sd(result_df$estimate)
# # # mean(result_df$s.e.)
# # #
# # # #
# # # #
# # # # pvals <- small_n_replications %>% 
# # # #   map(~as_tibble(.x$est.avg)) 
# # # #
# # # # mean(pvals < .05)
# # # #
# # # # alternate_n_factors <- map(0:8, function(r){
# # # #   out <- gsynth(y_obs ~ treat, r = r, CV = F, EM = T, inference = "parametric", index = c("id", "t"), data=df, se = T, force = "none")
# # # #
# # # #    as_tibble(out$est.avg)
# # # #   }
# # # # )
# # # #
# # # # alternate_n_factors %>%  
# # # #   bind_rows() %>% 
# # # #   rename_with(tolower) %>% 
# # # #   mutate(
# # # #     r = row_number()-1, 
# # # #     highlight = r!=8,
# # # #   )  %>% 
# # # #   ggplot(aes(x=r, y = estimate, ymin = ci.lower, ymax = ci.upper, col = highlight)) + 
# # # #   scale_color_manual(values = c("FALSE" = "black", "TRUE" = "#666666")) +
# # # #   geom_pointrange() + 
# # # #   geom_hline(yintercept = 0, linetype = 2) + 
# # # #   xlab("Number of Factors Fit") + 
# # # #   ylab("Estimate") + 
# # # #   ggtitle("Analysis of Gilens (2021), Varying Number of Latent Factors") + 
# # # #   theme_bw() + 
# # # #   theme(legend.position = "none") 
# # # #
# # # #
# # # #
# # # #
# # # #
# # # #
# # # # #
# # # # # make_gp_df2 <- function(n_factors, n_pre, n_post, n_ctrl, n_treat, bw = 8, multiplier = 4, w = .8, noise = 1, id = NULL) {
# # # # #   n_time <- n_pre + n_post
# # # # #   n_units <- n_treat + n_ctrl
# # # # #
# # # # #   k_mat <- make_kernel_matrix2(n_time, bw = bw, multiplier = multiplier)
# # # # #   factors <- mvrnorm(n_factors, rep(0, n_time), k_mat)
# # # # #
# # # # #   loadings <- matrix(runif(n_units * n_factors), nrow = n_units)
# # # # #
# # # # #   y_no_noise <- loadings %*% factors
# # # # #   unit <- integer(n_time * n_units)
# # # # #   time <- integer(n_time * n_units)
# # # # #   y <- numeric(n_time * n_units)
# # # # #
# # # # #   idx <- 1
# # # # #   for (i in 1:n_units) {
# # # # #     for (t in 1:n_time) {
# # # # #       unit[idx] <- i
# # # # #       time[idx] <- t
# # # # #       y[idx] <- y_no_noise[i, t]
# # # # #       idx <- idx + 1
# # # # #     }
# # # # #   }
# # # # #
# # # # #   df <- tibble(
# # # # #     y_no_noise = y,
# # # # #     y = y + rnorm(n_time * n_units, 0, noise),
# # # # #     unit = unit,
# # # # #     time = time
# # # # #   )
# # # # #
# # # # #   df$treat <- as.integer((df$unit <= n_treat) & (df$time > n_pre))
# # # # #
# # # # #   return(df)
# # # # # }
# # # # #
# # # # # noise <- .25
# # # # # set.seed(14)
# # # # # df <- make_gp_df2(2, 15, 10, 24, 8, bw = 30, multiplier = 6, noise = noise)
# # # # # df %>%
# # # # #   group_by(unit) %>%
# # # # #   mutate(ever_treat = any(treat == 1)) %>%
# # # # #   group_by(ever_treat, time) %>%
# # # # #   summarize(mean_y = mean(y)) %>%
# # # # #   ggplot(aes(x = time, y = mean_y, col = ever_treat)) +
# # # # #   geom_line()
# # # # #
# # # # # panelview(y ~ treat, data = df, index = c("unit", "time"), type = "outcome")
# # # # #
# # # # # df$y <- df$y_no_noise + rnorm(nrow(df), 0, noise)
# # # # # gsynth_out <- gsynth(
# # # # #   y ~ treat,
# # # # #   data = df, index = c("unit", "time"),
# # # # #   se = T, nboots = 1000, cores = 5,
# # # # #   inference = "parametric",
# # # # #   force = "two-way"
# # # # # )
# # # # # print(gsynth_out)
# # # # #
# # # # # get_att_est <- function(data) {
# # # # #   data$y <- data$y_no_noise + rnorm(nrow(data), 0, .3)
# # # # #
# # # # #   gsynth_out <- gsynth(
# # # # #     y ~ treat,
# # # # #     data = data, index = c("unit", "time"),
# # # # #     se = F, nboots = 1000, cores = 5,
# # # # #     inference = "parametric",
# # # # #     force = "two-way"
# # # # #   )
# # # # #   est <- gsynth_out$att.avg
# # # # #   return(est)
# # # # # }
# # # # #
# # # # # replicate(1000, get_att_est(df)) %>%
# # # # #   mean()
# # # # #
# # # # #
# # # # # n_time <- 300
# # # # # bw <- 1000
# # # # # multiplier <- 10
# # # # # n_factors <- 3
# # # # # k_mat <- make_kernel_matrix2(n_time, bw = bw, multiplier = multiplier)
# # # # # factors <- mvrnorm(n_factors, rep(0, n_time), k_mat)
# # # # # tibble(
# # # # #   time = 1:n_time,
# # # # #   f_1 = t(factors)[, 1],
# # # # #   f_2 = t(factors)[, 2],
# # # # #   f_3 = t(factors)[, 3]
# # # # # ) %>%
# # # # #   pivot_longer(starts_with("f")) %>%
# # # # #   ggplot(aes(x = time, y = value, col = name)) +
# # # # #   geom_line() +
# # # # #   xlab("Time") +
# # # # #   ylab("Factor Level") +
# # # # #   theme_bw() +
# # # # #   labs(col = "Factor")
# # # # # ggsave("factors.png", width = 8, height = 1.5)
