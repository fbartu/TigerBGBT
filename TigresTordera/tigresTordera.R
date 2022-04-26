library(tidyverse)
library(lubridate)
library(scales)

mosquits <- read_csv(file = "~/Documents/FEINA/PERSONAL_PROJECTS/TigresTordera/Jordistrap_until_june_2021.csv")


mosquits %>%
  filter(Species == "Aedes albopictus") %>% 
  mutate(Date2 = dmy(Date),
         na = is.na(Abundance)) %>%
  ggplot(aes(x = Date2, y = Abundance/Trapping.effort)) + 
  geom_line(colour = "#ff7f0d", lwd = 1) +
  geom_point(alpha = 0.3) +
  geom_rect(aes(xmin = as.Date("2020-11-30"), xmax = as.Date("2021-02-15"), ymin = -10, ymax = 50), fill = "black", alpha = 0.005) +
  # geom_vline(xintercept = as.numeric(as.Date("2020-11-30")), lty = 3) +
  # geom_vline(xintercept = as.numeric(as.Date("2021-02-08")), lty = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2021-07-27")), lty = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2021-09-27")), lty = 3) +
  scale_x_date(breaks = breaks_pretty(15)) +
  ylab("Captures diàries de mosquit tigre") +
  xlab("") +
  coord_cartesian(ylim = c(0,30)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 18),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust = 1))
# ggsave(filename = "~/Documents/FEINA/PROJECTES PERSONALS/TigresTordera/tigres_trampaTorderaCAT.png")

