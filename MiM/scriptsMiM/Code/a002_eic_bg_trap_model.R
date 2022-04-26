####Title####
# Main script for EIC Barcelona modeling
## Written using R 4.0.2

rm(list=ls())

####Dependencies####
library(tidyverse)
library(rstanarm)

season_bounds = read_rds("data/proc/a000c_eic_season_bounds.Rds")


bg = read_rds("data/proc/a000_eic_bg.Rds") %>% mutate(season = (sea_days > season_bounds$season_start & sea_days < season_bounds$season_end)) 

unique(bg$trap_name)

Mbg4 = stan_glmer(females ~ season + mwi7*FT30 + (1 | BARRI) + (1 | code_2018) + (1 | trap_name), offset = log(trapping_effort), data= bg, family = neg_binomial_2(link = "log"), adapt_delta = 0.99, iter = 2000, chains=4, cores=4)

write_rds(Mbg4, "data/proc/a002_eic_bg_model_Mbg4.Rds")

View(round(posterior_interval(Mbg4),4))
View(as_tibble(posterior_interval(Mbg4)) %>% mutate(coef = rownames(posterior_interval(Mbg4))) %>% filter(str_detect(coef, "mwi")))

