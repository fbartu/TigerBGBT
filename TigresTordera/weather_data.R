# # # # # # # # # # #
# WEATHER DATA
# Jordi Pagès: Trying to make this script work (script given by Fede Bartumeus)

# # Dependencies # #
library(tidyverse)
library(lubridate)
library(janitor)
library(RcppRoll)
library(parallel)
library(RSocrata)

source("../auth/transparencia_cat.R")

#####WEATHER PALAFOLLS AREA ################3
# #Fogars de la Selva i Malgrat weather station codes from https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-estacions-meteorol-giques-autom-tiques/yqwd-vj5e/data : 

plf_station_codes = "'KP', 'WT'"
# Variable codes from https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-variables-meteorol-giques/4fb2-n3yi/data
weather_variables = read_csv("/home/fbartu/Research/Laura_Blanco/Data/Metadades_variables_meteorol_giques.csv") %>% clean_names()

wv_codes = weather_variables %>% filter(acronim %in% c("VV10", "HR", "T", "PPT")) %>% pull(codi_variable) 

these_dates = seq.Date(from = as_date("2020-01-01"), to=as_date("2020-03-31"), by = "day")
this_date = these_dates[1]

# Note that I am putting the variable codes in by hand here since it is easier than figuring out how to automatically generate the right syntax for them in the API filter
#meteo_cat_weather <- bind_rows(lapply(these_dates, function(this_date) read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura = '", this_date, "' AND codi_estacio in('KP','WT') AND codi_variable in('30', '32', '33', '35')"), app_token = tc_token, email = tc_email, password = tc_pw, stringsAsFactors = FALSE) %>% as_tibble() %>% mutate(date = this_date))) %>% left_join(weather_variables) %>% rename(weather_type = acronim) %>% filter(weather_type %in% c("PPT", "T", "VV10", "HR")) %>% mutate(weather_type = case_when(weather_type=="PPT"~"PPT24H", weather_type =="T"~"TM", weather_type=="VV10"~"VVM10", weather_type=="HR"~"HRM")) %>% rename(valor = valor_lectura) %>% select(date, codi_estacio, data_extrem, weather_type, valor)
meteo_cat_weather <- bind_rows(lapply(these_dates, function(this_date) read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura = '", this_date, "' AND codi_estacio in('KP','WT') AND codi_variable in('30', '33', '35')"), app_token = tc_token, password = tc_pw, email = tc_email, stringsAsFactors = FALSE) ))

meteo_cat_weather <- bind_rows(lapply(this_date, function(this_date) read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura = '", this_date, "' AND codi_estacio in('KP','WT')"), app_token = tc_token, password = tc_pw, email = tc_email, stringsAsFactors = FALSE) ))

################################################################


#### WEATHER BARCELONA ##########################################

# Barcelona weather station codes from https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-estacions-meteorol-giques-autom-tiques/yqwd-vj5e/data : 
bcn_station_codes = "'AN', 'X4', 'D5', 'X8', 'X2'"

# Variable codes from https://analisi.transparenciacatalunya.cat/Medi-Ambient/Metadades-variables-meteorol-giques/4fb2-n3yi/data
weather_variables = read_csv("/home/fbartu/research/BarcelonaTiger/data/weather/Metadades_variables_meteorol_giques.csv") %>% clean_names() %>% mutate(codi_variable = codi_variable)


#wv_codes = weather_variables %>% filter(acronim %in% c("VV10", "HR", "Tx", "PPT")) %>% pull(codi_variable) 
wv_codes = weather_variables %>% filter(acronim %in% c("VV10","T", "HR", "PPT","Tx","Tn")) %>% pull(codi_variable) 


#FOR SOME REASON IF I RUN THE CODE FOR THE · YEARS SOMETHING DOES NOT WORK

these_dates = seq.Date(from = as_date("2018-01-01"), to=as_date("2020-12-31"), by = "day")
this_date = these_dates[1]
meteo_cat_weather <- bind_rows(lapply(these_dates, function(this_date) read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura = '", this_date, "' AND codi_estacio in('AN', 'X4', 'D5', 'X8', 'X2') AND codi_variable in('30', '32', '33', '35', '40', '42')"), app_token = tc_token, email = tc_email, password = tc_pw, stringsAsFactors = FALSE) %>% as_tibble() %>% mutate(date = this_date))) %>% left_join(weather_variables) %>% rename(weather_type = acronim) %>% filter(weather_type %in% c("VV10","T", "HR", "PPT","Tx","Tn")) %>% mutate(weather_type = case_when(weather_type=="PPT"~"PPT24H", weather_type=="T"~"T",weather_type=="Tn"~"Tmin",weather_type =="Tx"~"Tmax", weather_type=="VV10"~"VVM10", weather_type=="HR"~"HRM")) %>% rename(valor = valor_lectura) %>% select(date, codi_estacio, data_extrem, weather_type, valor)


# Note that I am putting the variable codes in by hand here since it is easier than figuring out how to automatically generate the right syntax for them in the API filter
meteo_cat_weather_2018 <- bind_rows(lapply(these_dates, function(this_date) read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura = '", this_date, "' AND codi_estacio in('AN', 'X4', 'D5', 'X8', 'X2') AND codi_variable in('30', '32', '33', '35', '40', '42')"), app_token = tc_token, email = tc_email, password = tc_pw, stringsAsFactors = FALSE) %>% as_tibble() %>% mutate(date = this_date))) %>% left_join(weather_variables) %>% rename(weather_type = acronim) %>% filter(weather_type %in% c("VV10","T", "HR", "PPT","Tx","Tn")) %>% mutate(weather_type = case_when(weather_type=="PPT"~"PPT24H", weather_type=="T"~"T",weather_type=="Tn"~"Tmin",weather_type =="Tx"~"Tmax", weather_type=="VV10"~"VVM10", weather_type=="HR"~"HRM")) %>% rename(valor = valor_lectura) %>% select(date, codi_estacio, data_extrem, weather_type, valor)

#these_dates = seq.Date(from = as_date("2020-12-02"), to=as_date("2020-12-05"), by = "day")
these_dates = seq.Date(from = as_date("2019-01-01"), to=as_date("2019-12-31"), by = "day")
this_date = these_dates[1]
# Note that I am putting the variable codes in by hand here since it is easier than figuring out how to automatically generate the right syntax for them in the API filter
meteo_cat_weather_2019 <- bind_rows(lapply(these_dates, function(this_date) read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura = '", this_date, "' AND codi_estacio in('AN', 'X4', 'D5', 'X8', 'X2') AND codi_variable in('30', '32', '33', '35', '40', '42')"), app_token = tc_token, email = tc_email, password = tc_pw, stringsAsFactors = FALSE) %>% 
                                             as_tibble() %>% mutate(date = this_date)))%>% left_join(weather_variables) %>% rename(weather_type = acronim) %>% 
  filter(weather_type %in% c("VV10","T", "HR", "PPT","Tx","Tn")) %>% mutate(weather_type = case_when(weather_type=="PPT"~"PPT24H", weather_type=="T"~"T",weather_type=="Tn"~"Tmin",weather_type =="Tx"~"Tmax", weather_type=="VV10"~"VVM10", weather_type=="HR"~"HRM")) %>%
  rename(valor = valor_lectura) %>% select(date, codi_estacio, data_extrem, weather_type, valor)

###################################################################

#these_dates = seq.Date(from = as_date("2020-12-02"), to=as_date("2020-12-05"), by = "day")
weather_variables = read_csv("/home/fbartu/research/BarcelonaTiger/data/weather/Metadades_variables_meteorol_giques.csv") %>% clean_names() %>% mutate(codi_variable = as.integer(codi_variable))

these_dates = seq.Date(from = as_date("2020-02-01"), to=as_date("2020-02-02"), by = "day")
#this_date = these_dates[1]
# Note that I am putting the variable codes in by hand here since it is easier than figuring out how to automatically generate the right syntax for them in the API filter
meteo_cat_weather_feb2020A <- bind_rows(lapply(these_dates, function(this_date) read.socrata(paste0("https://analisi.transparenciacatalunya.cat/resource/nzvn-apee.csv?$where=data_lectura = '", this_date, "' AND codi_estacio in('AN', 'X4', 'D5', 'X8', 'X2') AND codi_variable in('30', '32', '33', '35', '40', '42')"), app_token = tc_token, email = tc_email, password = tc_pw, stringsAsFactors = FALSE) %>% as_tibble() %>% mutate(date = this_date))) %>% left_join(weather_variables) %>% rename(weather_type = acronim) %>% filter(weather_type %in% c("VV10","T", "HR", "PPT","Tx","Tn")) %>% mutate(weather_type = case_when(weather_type=="PPT"~"PPT24H", weather_type=="T"~"T",weather_type=="Tn"~"Tmin",weather_type =="Tx"~"Tmax", weather_type=="VV10"~"VVM10", weather_type=="HR"~"HRM")) %>% rename(valor = valor_lectura) %>% select(date, codi_estacio, data_extrem, weather_type, valor)

#meteo_cat_weather_2020 <- bind_rows(lapply(these_dates, function(this_date) A, this_date, "' AND codi_estacio in('AN', 'X4', 'D5', 'X8', 'X2') AND codi_variable in('30', '32', '33', '35', '40', '42')"), app_token = tc_token, email = tc_email, password = tc_pw, stringsAsFactors = FALSE) %>% as_tibble() %>% mutate(date = this_date))) %>% left_join(weather_variables) %>% rename(weather_type = acronim) %>% filter(weather_type %in% c("VV10","T", "HR", "PPT","Tx","Tn")) %>% mutate(weather_type = case_when(weather_type=="PPT"~"PPT24H", weather_type=="T"~"T",weather_type=="Tn"~"Tmin",weather_type =="Tx"~"Tmax", weather_type=="VV10"~"VVM10", weather_type=="HR"~"HRM")) %>% rename(valor = valor_lectura) %>% select(date, codi_estacio, data_extrem, weather_type, valor)


# Combining Everything ####
bcn_weather_daily = bind_rows(list(meteo_cat_weather_2018 %>% mutate(data_extrem = as.character(data_extrem)),meteo_cat_weather_2019 %>% mutate(data_extrem = as.character(data_extrem)))) %>% group_by(date, weather_type) %>% ungroup() 
write_rds(bcn_weather_daily, "bcn_weather_daily.Rds")

# Combining Everything ####
bcn_weather_daily2020 = bind_rows(list(meteo_cat_weather_jan2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_feb2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_abr2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_mai2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_jun2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_jul2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_ago2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_sep2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_oct2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_nov2020 %>% mutate(data_extrem = as.character(data_extrem)),
                                   meteo_cat_weather_des2020 %>% mutate(data_extrem = as.character(data_extrem)))) %>% group_by(date, weather_type) %>% ungroup() 
write_rds(bcn_weather_daily2020, "bcn_weather_daily2020.Rds")

