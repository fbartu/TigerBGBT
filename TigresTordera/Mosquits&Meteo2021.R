# Mosquits i Meteo

library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(pollen) # To calculate growing degree days: https://cran.r-project.org/web/packages/pollen/vignettes/gdd.html

# Data sets (mosquito's data set, the official one)
meteo <- read_csv("Data/MeteoTorderaMaresme2021.csv")
# mosquits <- read_csv(file = "~/Documents/FEINA/PERSONAL_PROJECTS/TigresTordera/Jordistrap_until_june_2021.csv")
mosquits <- read_csv(file = "Data/Jordistrap_until_june_2021.csv")

# Filtering mosquito data only for 2021 and only aedes albopictus
mosquits21 <- mosquits %>% 
  mutate(Date2 = dmy(Date),
         na = is.na(Abundance),
         WeekNum = isoweek(Date2)) %>% 
  filter(Species == "Aedes albopictus") %>% 
  filter(Date2>"2020-12-31") %>% 
  filter(!is.na(Abundance))

# Polishing the format of meteo data set (right now we only select TEMPERATURE data)
meteo21 <- meteo %>% 
  select(Date, Temperature) %>% 
  filter(!is.na(Temperature)) %>%
  filter(Temperature < 40) %>% 
  separate(Date, into = c('Day', 'Time'), sep=' ', remove = TRUE) %>% 
  mutate(Date2 = dmy(Day)) %>% 
  filter(Date2 < dmy("31/12/2021") & Date2 > dmy("01/01/2021")) %>% 
  group_by(Date2) %>% 
  summarise(DailyMax = max(Temperature),
            DailyMin = min(Temperature),
            DailyMean = mean(Temperature))

# Plotting daily temperatures
meteo21 %>% 
  pivot_longer(cols = starts_with("Daily"), names_to = "Variable", values_to = "Value") %>% 
  ggplot(aes(x = Date2, y = Value)) +
  geom_point(aes(col = Variable), alpha = 0.5) +
  geom_smooth(aes(col = Variable, fill = Variable), span = 0.1) +
  labs(title = "Tordera BG Trap") +
  xlab("2021") +
  ylab("Daily temperature (ºC)") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        legend.position = c(0.13,0.85))


# Weekly temperatures (isoweek() starting on Monday, as our mosquito sampling)
meteo21_week <-  meteo %>% 
  select(Date, Temperature) %>% 
  filter(!is.na(Temperature)) %>%
  filter(Temperature < 40) %>% 
  separate(Date, into = c('Day', 'Time'), sep=' ', remove = TRUE) %>% 
  mutate(Date2 = dmy(Day)) %>% 
  filter(Date2 < dmy("31/12/2021") & Date2 > dmy("01/01/2021")) %>% 
  mutate(Weekday = weekdays(Date2),
         WeekNum = isoweek(Date2)) %>% 
  group_by(WeekNum) %>% 
  summarise(WeeklyMax = max(Temperature),
            WeeklyMin = min(Temperature),
            WeeklyMean = mean(Temperature))
  
# Plotting weekly max. temperatures (or mean or min, using select), along with mosquitoes
meteo21_week %>% 
  select(WeekNum, WeeklyMax) %>%
  left_join(mosquits21, by = "WeekNum") %>% 
  mutate(Abundance.corrected = Abundance/Trapping.effort) %>% 
  pivot_longer(cols = c("Abundance.corrected", starts_with("Weekly")), names_to = "Variable", values_to = "Value") %>% 
  ggplot(aes(x = Date2, y = Value)) +
  geom_line(aes(col = Variable), lwd = 0.8) +
  labs(title = "Tordera BG Trap") +
  xlab("2021") +
  ylab("Temperature (ºC)\nMosquitoes captured") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        legend.position = c(0.18,0.9))
# ggsave("Figs/WeeklyMaxTemp_Tordera.png")


# # # # # # # # # # # # # 
# GROWTH DEGREE DAYS ---- 
# # # # # # # # # # # # # 

# Growth-degree days accumulated per DAY
meteo21 %>% 
  mutate(gdd = gdd(tmax = DailyMax, tmin = DailyMin, tbase = 10, tbase_max = 30)) %>% 
  mutate(daily_acc_gdd = c(NA, diff(gdd))) %>% 
  ggplot(aes(x = Date2, y = daily_acc_gdd)) +
  geom_line() +
  labs(title = "Tordera BG Trap") +
  xlab("2021") +
  ylab("Daily accumulated degree days (ºC)") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        legend.position = c(0.2,0.8))


# Growth-degree days accumulated per WEEK, along with mosquitoes captured per week
meteo21 %>% 
  # pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(gdd = gdd(tmax = DailyMax, tmin = DailyMin, tbase = 10, tbase_max = 30)) %>% 
  left_join(mosquits21) %>% 
  filter(!is.na(Abundance)) %>% 
  mutate(weekly_acc_gdd = c(NA, diff(gdd))) %>% 
  pivot_longer(cols = c("weekly_acc_gdd", "Abundance"), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Date2, y = Value)) +
  geom_line(aes(col = Variable), lwd = 0.8) +
  scale_colour_manual(labels = c("Mosquitoes", "Degree days"), values = c("#1E6DA8","#FE7223")) +
  labs(title = "Tordera BG Trap") +
  xlab("2021") +
  ylab("Weekly mosquito capture (indiv.)\nWeekly accumulated degree days (ºC)") +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        legend.position = c(0.2,0.8))
# ggsave("Figs/WeeklyDD_Tordera.png")


  


