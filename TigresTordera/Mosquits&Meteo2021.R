# Mosquits i Meteo

library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(pollen) # To calculate growing degree days: https://cran.r-project.org/web/packages/pollen/vignettes/gdd.html

# Data sets (mosquito's data set, the official one)
meteo <- read_csv("MeteoTorderaMaresme2021.csv")
# mosquits <- read_csv(file = "~/Documents/FEINA/PERSONAL_PROJECTS/TigresTordera/Jordistrap_until_june_2021.csv")
mosquits <- read_csv(file = "~/Documents/FEINA/PERSONAL_PROJECTS/TigresTordera/Jordistrap_until_june_2021.csv")

# Filtering mosquito data only for 2021 and only aedes albopictus
mosquits21 <- mosquits %>% 
  mutate(Date2 = dmy(Date),
         na = is.na(Abundance)) %>% 
  filter(Species == "Aedes albopictus") %>% 
  filter(Date2>"2020-12-31") %>% 
  filter(!is.na(Abundance))

# Polishing the format of meteo data set
meteo21 <- meteo %>% 
  select(Date, Temperature) %>% 
  filter(!is.na(Temperature)) %>% 
  separate(Date, into = c('Day', 'Time'), sep=' ', remove = TRUE) %>% 
  mutate(Date2 = dmy(Day)) %>% 
  filter(Date2 < dmy("31/12/2021") & Date2 > dmy("01/01/2021")) %>% 
  group_by(Date2) %>% 
  summarise(Max = max(Temperature),
            Min = min(Temperature)) %>% 
  pivot_longer(Max:Min, names_to = "Type", values_to = "Temperature") %>% 
  filter(Temperature<40)


# Growth-degree days
meteo21 %>% 
  pivot_wider(names_from = Type, values_from = Temperature) %>% 
  mutate(gdd = gdd(tmax = Max, tmin = Min, tbase = 15, tbase_max = 30),
         diff_gdd = c(NA, diff(gdd))) %>% 
  left_join(mosquits21) %>% 
  filter(!is.na(Abundance)) %>% 
  mutate(diff_gdd10 = 15*diff_gdd) %>% 
  pivot_longer(cols = c("diff_gdd10", "Abundance")) %>%
  ggplot(aes(Date2, value)) +
  geom_line(aes(col = name))


  


