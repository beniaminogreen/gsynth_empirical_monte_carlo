library(dplyr)
library(purrr)
library(ggplot2)
library(wesanderson)
library(stringr)
library(tidyr)
library(panelView)

data <- targets::tar_read(datasets_with_sims)

label_df <- tribble(
  ~new_outcome_number, ~outcomes, ~hr_name, 
  1,'anti_aid', "Too Much: Foreign Aid", 
  2,'anti_education', "Too Much: Education",
  3,'anti_environment', "Too Much: Environment", 
  4,'anti_defense', "Too Much: Defense", 
  5,'anti_race', "Too Much: Race", 
  6,'anti_welfare', "Too Much: Welfare", 
  7,'poptotal', "Population", 
  8,'nonwhite', "Share Nonwhite", 
  9,'incomepcap', "Income", 
  10,'gsppcap', "GDP Per Capita", 
  11,'gini_coef', "Gini Coefficient",  
  12,'vcrimerate', "Violent Crime Rate", 
  13,'union_density', "Union Density", 
  14,'inst6014_nom', "Government Ideology", 
  15,'citi6013', "Citizen Ideology", 
  16,'mood', "Policy Mood", 
)

data <- inner_join(data, label_df) %>% 
  arrange(new_outcome_number)

#  This code saves out 6 example panels
walk2(1:6, data$small_data[1:6], function(index, data) {
  panelview(outcome ~ 0, index = c("state", "year"), data = data, type = "outcome") 
  ggsave(str_glue("figures/time_series_{index}.png"), height = 8, width = 8)
  }
)



#  Calculate all the statistics for coverage, ci width, bias across simulations
df <- data %>% 
  unnest(sims) %>% 
  mutate( 
    outcome_sd = map_dbl(small_data, ~sd(.x$outcome)),
    gsc_covers = gsc_ci.lower < 0 & gsc_ci.upper > 0,
    em_gsc_covers = em_gsc_ci.lower < 0 & em_gsc_ci.upper > 0,
    lm_covers = lm_conf.low < 0 & lm_conf.high > 0,
    gsc_ci_width = gsc_ci.upper - gsc_ci.lower,
    em_gsc_ci_width = em_gsc_ci.upper - em_gsc_ci.lower,
    lm_ci_width = lm_conf.high - lm_conf.low
  ) %>% 
  summarize(
    gsc_bias =mean(gsc_estimate/outcome_sd), 
    em_gsc_bias =mean(em_gsc_estimate/outcome_sd), 
    lm_bias =mean(lm_estimate/outcome_sd), 
    gsc_se_ratio =mean(gsc_s.e.)/sd(gsc_estimate), 
    em_gsc_se_ratio =mean(em_gsc_s.e.)/sd(em_gsc_estimate), 
    lm_se_ratio =mean(lm_std.error)/sd(lm_estimate), 
    em_gsc_coverage =mean(em_gsc_covers), 
    lm_coverage =mean(lm_covers), 
    gsc_coverage =mean(gsc_covers), 
    em_gsc_coverage =mean(em_gsc_covers), 
    lm_coverage =mean(lm_covers), 
    gsc_ci_width =mean(gsc_ci_width/outcome_sd), 
    em_gsc_ci_width =mean(em_gsc_ci_width/outcome_sd), 
    lm_ci_width =mean(lm_ci_width/outcome_sd), 
    .by = c(new_outcome_number, hr_name)
  ) 


df %>% 
    pivot_longer(cols = c(gsc_coverage, lm_coverage, em_gsc_coverage)) %>% 
    mutate(
      name = recode(name, 
                    gsc_coverage = "GSC + Parametric Bootstrap", 
                    em_gsc_coverage = "IFE-EM + Parametric Bootstrap", 
                    lm_coverage = "TWFE + Clutstered Standard Errors"
                    ) %>% 
            factor() %>% 
            relevel("IFE-EM + Parametric Bootstrap")
    ) %>% 
    ggplot(aes(x=reorder(hr_name, -new_outcome_number), y=value)) +
    geom_bar(position="dodge", stat="identity") +
    theme_bw()  +
    scale_fill_manual(values = wes_palette("GrandBudapest2")) +
    xlab("Outcome") +
    ylab("Coverage Rate") +
    geom_hline(yintercept = .95, linetype = 2) +
    coord_flip() +
    facet_wrap(~name) + 
    ggtitle(
      "Coverage Rate by Method"
    )
  ggsave("figures/combined_coverage.png", width = 10)


# df %>% 
#   pivot_longer(cols = contains("bias"))   %>% 
#   ggplot(aes(x=reorder(other_name, -rn), y=value, fill = name)) + 
#   geom_hline(yintercept = 0, linetype = 2) + 
#   geom_bar(position="dodge", stat="identity")  + 
#   theme_bw()  +  
#   scale_fill_manual(values = wes_palette("GrandBudapest2")) + 
#   xlab("Outcome") + 
#   labs(fill = "Method") + 
#   coord_flip() + 
#   ggtitle(
#     "Bias by Method", 
#     subtitle = "GSC vs TWFE"
#   )  
# ggsave("figures/bias.png")
# 
# df %>% 
#   pivot_longer(cols = contains("se_ratio"))   %>% 
#   ggplot(aes(x=reorder(other_name, -rn), y=value, fill = name)) + 
#   geom_hline(yintercept = 1, linetype = 2) + 
#   geom_bar(position="dodge", stat="identity")  + 
#   theme_bw()  +  
#   scale_fill_manual(values = wes_palette("GrandBudapest2")) + 
#   xlab("Outcome") + 
#   ylab("Estimated SE / True SE") +  
#   labs(fill = "Method") + 
#   coord_flip() + 
#   ggtitle(
#     "Estimated VS Actual SE", 
#     subtitle = "GSC vs TWFE"
#   )  
# ggsave("figures/se_ratio.png")
# 
# df %>% 
#   pivot_longer(cols = contains("ci_width"))   %>% 
#   ggplot(aes(x=reorder(other_name, -rn), y=value, fill = name)) + 
#   geom_bar(position="dodge", stat="identity") + 
#   theme_bw()  +  
#   scale_fill_manual(values = wes_palette("GrandBudapest2")) + 
#   xlab("Outcome") + 
#   ylab("Ci Width") +  
#   labs(fill = "Method") + 
#   coord_flip() + 
#   ggtitle(
#     "CI Width by Method", 
#     subtitle = "GSC vs TWFE"
#   )  
# ggsave("figures/ci_width.png")
# 
# df %>% 
#   #pivot_longer(cols = c(gsc_coverage, lm_coverage, em_gsc_coverage))   %>% 
#   pivot_longer(cols = c(em_gsc_coverage))   %>% 
#   mutate(
#     ci_lo = pmax(value - 2*sqrt(value * (1-value)/200),0),
#     ci_hi = pmin(value + 2*sqrt(value * (1-value)/200),1)
#   ) %>% 
#   ggplot(aes(x=reorder(other_name, -rn), y=value)) + 
#   geom_bar(position="dodge", stat="identity") + 
#   geom_linerange(aes(ymax = ci_hi, ymin = ci_lo), alpha = .5) + 
#   theme_bw()  +  
#   scale_fill_manual(values = wes_palette("GrandBudapest2")) + 
#   xlab("Outcome") + 
#   ylab("Coverage Rate") +  
#   geom_hline(yintercept = .95, linetype = 2) + 
#   coord_flip() + 
#   ggtitle(
#     "IFE-EM Coverage Rate"
#   )  
# ggsave("figures/coverage.png", width = 10)
# 
# df %>%
#   pivot_longer(cols = c(gsc_coverage, lm_coverage, em_gsc_coverage))   %>%
#   mutate(
#     ci_lo = pmax(value - 2*sqrt(value * (1-value)/200),0),
#     ci_hi = pmin(value + 2*sqrt(value * (1-value)/200),1),
#     compatible_with_coverage = ci_lo < .95 & ci_hi > .95,
#     compatible_with_conservative = ci_hi > .95
#   )  %>%
#   summarize(
#     compatible = mean(compatible_with_coverage),
#     cons = mean(compatible_with_conservative),
#     .by = name
#   )
# 
# 
# df
