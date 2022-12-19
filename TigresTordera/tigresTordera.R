library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)

mosquits <- read_csv(file = "Data/Jordistrap_EVERYTHING.csv")


mosquits2 <- mosquits %>%
  filter(Species == "Aedes albopictus") %>% 
  mutate(Date2 = dmy(Date),
         na = is.na(Abundance))

# to filter out dates where ants ate all the captured mosquitoes
mosquits2 <- mosquits2 %>% 
  mutate(Abundance2 = ifelse(Date2 == "2022-06-29" | Date2 == "2022-07-06" | Date2 == "2022-07-13", NA, Abundance))

ggplot(data = mosquits2, aes(x = Date2, y = Abundance2/Trapping.effort)) + 
  geom_line(colour = "#ff7f0d", lwd = 1) +
  geom_point(alpha = 0.3) +
  # geom_rect(aes(xmin = as.Date("2020-11-30"), xmax = as.Date("2021-02-15"), ymin = -1, ymax = 1), fill = "black", alpha = 0.002) +
  # geom_rect(aes(xmin = as.Date("2021-11-29"), xmax = as.Date("2022-04-27"), ymin = -1, ymax = 1), fill = "black", alpha = 0.002) +
  # geom_vline(xintercept = as.numeric(as.Date("2020-11-30")), lty = 3) +
  # geom_vline(xintercept = as.numeric(as.Date("2021-02-08")), lty = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-11-02")), lty = 2, lwd = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2021-11-02")), lty = 2, lwd = 0.3) +
  geom_vline(xintercept = as.numeric(as.Date("2022-11-02")), lty = 2, lwd = 0.3) +
  # geom_hline(yintercept = 255/7, lty = 2, lwd = 0.3) +
  # geom_label_repel(data = filter(mosquits2, Date2 %in% c(as.numeric(as.Date("2021-05-31")), 
  #                                                  as.numeric(as.Date("2020-06-01")), 
  #                                                  as.numeric(as.Date("2022-06-01")))),
  #                     
  #                          aes(label = paste("Captures setmana = ", Abundance, sep = "")),
  #                  min.segment.length = 0) +
  scale_x_date(breaks = breaks_pretty(15)) +
  ylab("Captures diàries de mosquit tigre") +
  xlab("") +
  coord_cartesian(ylim = c(0,60)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust = 1))
# ggsave(filename = "Figs/tigres_trampaTorderaCAT.png")

