library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)

meteo <- read_csv("MeteoTorderaMaresme2021.csv")

meteo %>% 
  select(Date, Temperature) %>% 
  filter(!is.na(Temperature)) %>% 
  separate(Date, into = c('Day', 'Time'), sep=' ', remove = TRUE) %>% 
  mutate(Date2 = dmy(Day)) %>% 
  filter(Date2 < dmy("31/12/2021")) %>% 
  group_by(Date2) %>% 
  summarise(Max = max(Temperature),
            Min = min(Temperature)) %>% 
  pivot_longer(Max:Min, names_to = "Type", values_to = "Temperature") %>% 
  filter(Temperature<40) %>% 
  ggplot(aes(x = Date2, y = Temperature)) +
  geom_line(aes(colour = Type)) +
  scale_x_date(breaks = breaks_pretty(15)) +
  scale_y_continuous(breaks = breaks_pretty(8)) +
  coord_cartesian(ylim = c(-5,35)) +
  ylab("Temperature (ºC)") +
  xlab("2021") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust = 1))
# ggsave("TorderaTempMaxMin.png")
