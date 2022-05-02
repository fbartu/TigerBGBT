# Mosquits i Meteo

library(tidyverse)
library(tidylog)
library(lubridate)
library(scales)
library(pollen) # To calculate growing degree days: https://cran.r-project.org/web/packages/pollen/vignettes/gdd.html
source("mcheck_function.R")

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
# ggsave("Figs/MaxMeanMinTempsTordera2021.png")

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
  scale_colour_manual(values = c("#1E6DA8","#FE7223")) +
  labs(title = "Tordera BG Trap") +
  xlab("2021") +
  scale_y_continuous("Weekly maximum temperature (ºC)", sec.axis = sec_axis(~., name = "Daily tiger mosquito capture")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        legend.position = "none")
# ggsave("Figs/WeeklyMaxTemp_Tordera.png")


# # # # # # # # # # # # # 
# GROWTH DEGREE DAYS ---- 
# # # # # # # # # # # # # 

# Sum of the growing degree days of 2021 along with mosquito abundance
meteo21 %>% 
  mutate(gdd = gdd(tmax = DailyMax, tmin = DailyMin, tbase = 10, tbase_max = 30)) %>% 
  left_join(mosquits21) %>% 
  mutate(Abundance = Abundance*10) %>% # To make the scale of Abundance be similar to the scale of accumulated growing degree days.
  filter(!is.na(Abundance)) %>% 
  select(Date2, gdd, Abundance) %>%  
  pivot_longer(cols = c("gdd", "Abundance"), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Date2, y = Value)) +
  geom_line(aes(colour = Variable), lwd = 0.8) +
  scale_colour_manual(labels = c("Mosquitoes", "Degree days"), values = c("#1E6DA8","#FE7223")) +
  labs(title = "Tordera BG Trap") +
  xlab("2021") +
  scale_y_continuous("Sum of growing degree days (ºC)", sec.axis = sec_axis(~./10, name = "Abundance")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        # legend.position = c(0.2,0.8),
        legend.position = "none")
# ggsave("Figs/SumGDDvsMosquitoesTordera.png")


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
# ggsave("Figs/GDDaccummDay.png")

# Growth-degree days accumulated per WEEK, along with mosquitoes captured per week
meteo21 %>% 
  # pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(gdd = gdd(tmax = DailyMax, tmin = DailyMin, tbase = 10, tbase_max = 30)) %>% 
  left_join(mosquits21) %>% 
  filter(!is.na(Abundance)) %>% 
  mutate(Abundance = Abundance) %>% 
  mutate(weekly_acc_gdd = c(NA, diff(gdd))) %>% 
  pivot_longer(cols = c("weekly_acc_gdd", "Abundance"), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = Date2, y = Value)) +
  geom_line(aes(col = Variable), lwd = 0.8) +
  scale_colour_manual(labels = c("Mosquitoes", "Degree days"), values = c("#1E6DA8","#FE7223")) +
  labs(title = "Tordera BG Trap") +
  xlab("2021") +
  scale_y_continuous("Weekly accumulated degree days (ºC)", sec.axis = sec_axis(~., name = "Weekly mosquito capture (indiv.)")) +
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(size = 16),
        legend.position = "none")
# ggsave("Figs/WeeklyDD_Tordera.png")



# # # # # # 
# Temporal trends using linear models ----
# # # # # #
library(nlme)

meteoMosq <- meteo21 %>% 
  # pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(gdd = gdd(tmax = DailyMax, tmin = DailyMin, tbase = 10, tbase_max = 30)) %>% 
  left_join(mosquits21) %>% 
  filter(!is.na(Abundance)) %>% 
  mutate(weekly_acc_gdd = c(NA, diff(gdd)))

M0 <- gls(Abundance ~  weekly_acc_gdd + WeekNum, na.action = na.omit, data = meteoMosq)
summary(M0)
car::Anova(M0)
plot(M0)

E<-as.numeric(residuals(M0,type="normalized"))
# Given there is an NA...
I<-!is.na(meteoMosq$weekly_acc_gdd)
Efull<-vector(length=length(meteoMosq$weekly_acc_gdd))
Efull<-NA
Efull[I]<-E

#Auto-correlation plot
acf(Efull,na.action = na.pass,
    main="Auto-correlation plot for residuals")

# First attempt to model the correlation structure
M1<-gls(Abundance ~ weekly_acc_gdd + WeekNum, na.action = na.omit,
        correlation = corCompSymm(form =~ WeekNum), data = meteoMosq)
summary(M1)

# Second attempt
M2<-gls(Abundance ~ weekly_acc_gdd + WeekNum, na.action = na.omit,
        correlation = corAR1(form =~ WeekNum), data = meteoMosq)  
summary(M2)

AIC(M0, M1, M2) # AR1 model is much better than M0 (linear model without correl structure) and M1.
anova(M0, M2) # M2 is clearly better.

# Let's check validation for M2
E<-as.numeric(residuals(M2,type="normalized"))
# Given there is an NA...
I<-!is.na(meteoMosq$weekly_acc_gdd)
Efull<-vector(length=length(meteoMosq$weekly_acc_gdd))
Efull<-NA
Efull[I]<-E
#Auto-correlation plot
acf(Efull,na.action = na.pass,
    main="Auto-correlation plot for residuals")
# NO AUTOCORRELATION LEFT! YAY!

car::Anova(M2)
# Analysis of Deviance Table (Type II tests)
# Response: Abundance
# Df  Chisq Pr(>Chisq)   
# weekly_acc_gdd  1 8.1833   0.004228 **
# WeekNum         1 0.4680   0.493894   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(M2)
# Generalized least squares fit by REML
# Model: Abundance ~ weekly_acc_gdd + WeekNum 
# Data: meteoMosq 
# AIC      BIC    logLik
# 383.1152 391.1697 -186.5576
# 
# Correlation Structure: ARMA(1,0)
# Formula: ~WeekNum 
# Parameter estimate(s):
#   Phi1 
# 0.7170292 
# 
# Coefficients:
# Value Std.Error   t-value p-value
# (Intercept)    -45.00346  38.54071 -1.167686  0.2504
# weekly_acc_gdd   1.06761   0.37321  2.860647  0.0069
# WeekNum          0.80617   1.17838  0.684129  0.4982
# 
# Correlation: 
#   (Intr) wkly__
# weekly_acc_gdd -0.367       
# WeekNum        -0.758 -0.186
# 
# Standardized residuals:
#   Min          Q1         Med          Q3         Max 
# -1.13831768 -0.49232145 -0.08703551  0.28566503  2.81774001 
# 
# Residual standard error: 42.32828 
# Degrees of freedom: 40 total; 37 residual

mcheck(M2)
# We see some increase in spread... we need weights!

# Third attempt
M2.w <-gls(Abundance ~ weekly_acc_gdd + WeekNum, na.action = na.omit,
        correlation = corAR1(form =~ WeekNum), data = meteoMosq,
        weights = varFixed(~weekly_acc_gdd))  
AIC(M2, M2.w)
# M2.w much better

mcheck2(M2.w) # ok... it's the zero abundances that mess things up.
car::Anova(M2.w)
# Analysis of Deviance Table (Type II tests)
# Response: Abundance
#               Df  Chisq Pr(>Chisq)  
# weekly_acc_gdd  1 5.5595    0.01838 *
# WeekNum         1 0.3895    0.53256  

summary(M2.w)
# Coefficients:
#   Value Std.Error    t-value p-value
# (Intercept)    -20.848122 24.837296 -0.8393878  0.4066
# weekly_acc_gdd   0.805259  0.341523  2.3578487  0.0238
# WeekNum          0.428497  0.686581  0.6241022  0.5364

visreg::visreg(M2.w)


# # # # # # 
# Temporal trends using additive models ----
# # # # # #
library(mgcv)

meteoMosq <- meteo21 %>% 
  # pivot_wider(names_from = Variable, values_from = Value) %>%
  mutate(gdd = gdd(tmax = DailyMax, tmin = DailyMin, tbase = 10, tbase_max = 30)) %>% 
  left_join(mosquits21) %>% 
  filter(!is.na(Abundance)) %>% 
  mutate(weekly_acc_gdd = c(NA, diff(gdd)))

# Additive model without temporal correlation structure
gam1 <- gamm(Abundance ~ weekly_acc_gdd + s(WeekNum), data = meteoMosq)
summary(gam1$gam)

# Let's add temporal autocorrelation
gam2 <- gamm(Abundance ~ weekly_acc_gdd + s(WeekNum), 
             correlation = corAR1(form = ~WeekNum),
             data = meteoMosq)

AIC(gam1$lme, gam2$lme)
anova(gam1$lme, gam2$lme) # The model with temporal correlation is better.

summary(gam2$gam)
anova(gam2$gam)
# Family: gaussian 
# Link function: identity 
# Formula:
#   Abundance ~ weekly_acc_gdd + s(WeekNum)
# 
# Parametric Terms:
#   df     F p-value
# weekly_acc_gdd  1 10.84 0.00219 **
# 
# Approximate significance of smooth terms:
#             edf Ref.df     F  p-value
# s(WeekNum)   1      1   1.021   0.319


plot(gam2$gam)
             
mcheck(gam2$gam) # more or less ok

gam2$gam$data <- meteoMosq
visreg::visreg(gam2$gam)
