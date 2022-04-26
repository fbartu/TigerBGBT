####Title####
# Main script for EIC Barcelona modeling
## Written using R 4.0.2

# R CMD BATCH --no-save --no-restore scripts/eic/a004_eic_combined_predictions.R scripts/eic/a004_eic_combined_predictions.out 


rm(list=ls())

####Dependencies####
library(tidyverse)
library(sf)
library(rgdal)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(RcppRoll)
library(parallel)
library(data.table)
library(rstanarm)
library(matrixStats)

# Loading Models ####

M = read_rds("data/proc/a001_eic_mosquito_alert_spatio_temporal_model_M10.Rds")

Mbg4 = read_rds("data/proc/a002_eic_bg_model_Mbg4.Rds")

Msts = read_rds("data/proc/a002_eic_smart_trap_model_Msts.Rds")

bcn_weather_daily = read_rds("data/proc/a001_eic_bcn_weather_daily.Rds")

bcn_weather_lags_7d = read_rds("data/proc/a001_eic_bcn_weather_lags_7d.Rds") 

bcn_weather_lags_14d = read_rds("data/proc/a001_eic_bcn_weather_lags_14d.Rds") 

bcn_weather_lags_30d = read_rds("data/proc/a001_eic_bcn_weather_lags_30d.Rds") 

mean_se = read_rds("data/proc/a001_eic_mosquito_alert_spatio_temporal_model_mean_se.Rds")

season_bounds = read_rds("data/proc/a000c_eic_season_bounds.Rds")

these_points_bcn_lc = read_rds("data/proc/barcelona_prediction_points_lc_sf_df_20mx20m.Rds")


pred_dates = c(seq.Date(as_date("2020-01-01"), as_date("2021-02-04"), by="day"),seq.Date(as_date("2018-01-01"), as_date("2019-12-31"), by="day") ) 

this_date = as_date("2020-09-01")

# saving memory
these_points_bcn_lc = these_points_bcn_lc %>% as_tibble() %>% select(-c(geometry, renta_bruta_media_por_persona, renta_bruta_media_por_hogar, average_age_of_the_population, percentage_of_the_population_under_the_age_of_18, distribucion_de_la_renta_p80_p20))

mclapply(pred_dates, function(this_date) {
  
  print(this_date)
  flush.console()
  
  these_pred_points = these_points_bcn_lc %>% mutate(date = this_date, year = factor(year(date))) %>% mutate(sea_days = yday(date), season = (sea_days > season_bounds$season_start & sea_days < season_bounds$season_end)) %>% left_join(bcn_weather_daily %>% select(date, mwi)) %>% left_join(bcn_weather_lags_7d %>% select(date, mwi7)) %>% left_join(bcn_weather_lags_30d %>% select(date, FT30))  %>% drop_na() %>% as.data.table()

  rm(these_points_bcn_lc)
  
  ma_prob = apply(posterior_predict(M, newdata = these_pred_points, draws = 1000), 2, function(x) mean(x))
  
  these_pred_points[, ma_prob:=ma_prob]
  
  rm(ma_prob)

  bg_prob = apply(posterior_predict(Mbg4, newdata = these_pred_points, draws = 1000), 2, function(x) mean(x>0))
  
 
  these_pred_points[, bg_prob:=bg_prob]
  
  rm(bg_prob)
  
  # here we just need to predict onto one row since there is no spatial variation.
  st_prob = apply(posterior_predict(Msts, newdata = these_pred_points[1,], draws = 1000), 2, function(x) mean(x))
  
  these_pred_points[, st_prob:=st_prob]
  
  rm(st_prob)
  
  these_pred_points = these_pred_points[, .(date, lon, lat, crs3035_x, crs3035_y, ma_prob, bg_prob, st_prob)]
  
  these_pred_points[, vri:=(ma_prob+bg_prob+st_prob)/3]
  
  fwrite(these_pred_points, paste0("data/proc/a004_eic_combined_predictions_dt_", this_date, ".csv"))

  return(NULL)
}, mc.cores = 5)

