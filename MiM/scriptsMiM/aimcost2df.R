library("readxl")
library(tidyverse)
library(lubridate)
library(curl)
library(jsonlite)
library(sf)
library(rgdal)


setwd('/home/fbartu/research/SpainTiger/')
# Delete all variables.
rm(list = ls())
# Read xlsx and skip 2 first rows.
my_data <- read_excel("/home/fbartu/SpainTiger/dataMiM/abundmosq_marimutra_2020_21FB.xlsx",
                      skip = 3,col_names = TRUE , col_types = c("guess", "guess", "guess",
                                                                "guess", "guess", "guess", "guess","guess", "guess", 
                                                                "guess", "guess", "guess", "guess", "guess", "guess",
                                                                "date", "guess", "guess", "date","guess", "guess", 
                                                                "guess", "guess", "numeric", "guess", "guess","numeric", 
                                                                "guess", "guess", "guess", "guess"))


my_data$diff_date = as.numeric(difftime(my_data$endDate, my_data$startDate), units="days") 
my_data$year = year(my_data$endDate)
#data_albopictus = my_data %>% filter("Aedes albopictus" == my_data$species & "2020" == my_data$year)

data_albopictus = my_data %>% filter("Aedes albopictus" == my_data$species)

plot(data_albopictus$endDate,data_albopictus$nrperSpecies,is.na=TRUE)



#WEATHER AEMET DATA
SPAIN_CRS = 25830
spain_perimeter = st_read("data/cartography/SIGLIM_Publico_INSPIRE/SHP_ETRS89/recintos_autonomicas_inspire_peninbal_etrs89/recintos_autonomicas_inspire_peninbal_etrs89.shp") %>% st_transform(SPAIN_CRS) %>% summarize() 

station_points = st_read("data/cartography/1da1315b/Estaciones_Completas.shp") %>% bind_rows(st_read("data/cartography/b29c8d56/Estaciones_Termometricas.shp")) %>% bind_rows(st_read("data/cartography/2aa58725/Estaciones_Automaticas.shp")) %>% bind_rows(st_read("data/cartography/8892d9c9/Estaciones_Pluviometricas.shp")) %>% st_transform(SPAIN_CRS)


h <- new_handle()
handle_setheaders(h, 'api_key' = 'eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJqb2huLnBhbG1lckB1cGYuZWR1IiwianRpIjoiYWRjYTliNGItNmZkMC00MTlkLWI1MzMtNjRlNzQwMGY2MDAxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MTA0NTk4MTUsInVzZXJJZCI6ImFkY2E5YjRiLTZmZDAtNDE5ZC1iNTMzLTY0ZTc0MDBmNjAwMSIsInJvbGUiOiIifQ.JtTlq8QIaAEdte8Mn3JrgzGvkwrtboEpswfEK6Lb1Hc')

#all_dates = seq.Date(from = as_date("2018-01-01"), to=today(), by = "day")
all_dates = seq.Date(from = as_date("2020-01-01"), to=as_date("2020-01-09"), by = "day")

weather_daily = bind_rows(lapply(1:length(all_dates), function(i){
  
  start_date = all_dates[i]
  print(start_date)
  flush.console()
  if(i %% 10 == 0) Sys.sleep(30)
  
  req = curl_fetch_memory(paste0('https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/', start_date, 'T00%3A00%3A00UTC/fechafin/', start_date, 'T23%3A59%3A59UTC/todasestaciones'), handle=h)
  
  wurl = fromJSON(rawToChar(req$content))$datos
  
  req = curl_fetch_memory(wurl)
 
    wdia  = fromJSON(rawToChar(req$content)) %>% as_tibble()  %>% select(fecha, indicativo, velmedia, tmed)  %>% filter(indicativo =="4358X") %>% mutate(
    velmedia = as.numeric(str_replace(velmedia, ",", ".")),
    tmed = as.numeric(str_replace(tmed, ",", ".")),
    FW = as.integer(velmedia <= (6*3.6)*1000/(60*60)), 
    FT = case_when(tmed<=15~0, tmed>30~0, (tmed>15 & tmed <=20)~ (.2*tmed)-3, (tmed>20 & tmed<=25)~1, (tmed>25 & tmed <= 30)~ (-.2*tmed)+6),
    mwi = FW*FT
  ) %>% select(fecha, indicativo, mwi) %>% filter(!is.na(mwi))
  
  return(wdia)
}))


X=readRDS("/home/fbartu/research/SpainTiger/data/proc/aemet_weather_daily.Rds")


