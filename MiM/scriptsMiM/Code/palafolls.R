#setwd("C:/Users/lblan/OneDrive/Escritorio/CEAB/2020/RStudio/Modelling/Data")
setwd('/home/fbartu/Research/Laura_Blanco/Code/') #Fede

library(tidyverse)
library(sf)
library(rgdal)
library(tmap)
library(ggplot2)
library(lubridate)
library(readxl)
library(janitor)
library(RcppRoll)
Surveillance2020 <- read_excel("../Data/Surveillance2020.xlsx", 
                               col_types = c("numeric", "text", "text", 
                                             "text", "text", "numeric", "text", 
                                             "numeric", "numeric", "numeric", 
                                             "text", "text", "numeric", "text", 
                                             "text", "date", "numeric", "numeric", 
                                             "date", "numeric", "numeric", "text", 
                                             "text", "numeric", "numeric", "text", 
                                             "numeric", "text", "numeric", "numeric", 
                                             "text"))
ggplot(Surveillance2020, aes(x=End_date, y=Females/trapping_effort, color=trap_name)) + geom_line()

Palafolls <- Surveillance2020[ c(1:221), ]

ggplot(Palafolls, aes(x=End_date, y=Females/trapping_effort, color=trap_name)) + geom_point()
library(lattice)
library(rstanarm)
Mbg4 = stan_glmer(females ~ season + mwi7*FT30 + (1 | BARRI) + (1 | code_2018) + (1 | trap_name), offset = log(trapping_effort), data= bg, family = neg_binomial_2(link = "log"), adapt_delta = 0.99, iter = 2000, chains=4, cores=4)

M0 = stan_glmer(Females ~ (1 | trap_name), offset = log(trapping_effort), data= Palafolls, family = neg_binomial_2(link = "log"), adapt_delta = 0.99, iter = 2000, chains=4, cores=4)
M0_loo = loo(M0, save_psis = TRUE)
plot(M0)
launch_shinystan(M0)
M1 =stan_glm(nrperspecies ~ Start_date + trap_name, offset = log(trapping_effort), data = Palafolls, family = neg_binomial_2(link = "log"), adapt_delta = 0.99, iter =2000, chains= 4, cores = 4)
M1_loo = loo(M1, save_psis = TRUE)
plot(M1)
M2 = stan_glm(Females ~ trap_name, data = Palafolls, adapt_delta = 0.99, iter = 2000, chains=4, cores=4)
plot(M2)
m3 = stan_glm(Females~ Start_date*trap_name, data= Palafolls, adapt_delta = 0.99, iter = 2000, chains = 4, cores = 4)
m3_loo = loo(M1, save_psis = TRUE)
plot(m3)


#LINEAL REGRESSION MODELS (SIMPLE)
Palafolls$ftrampa <- as.factor(Palafolls$trap_name) #CREATING TRAP FACTOR
p <- xyplot (nrperspecies ~ Start_date | ftrampa, Palafolls, type = c("p", "r"), #Graphic with the relation between the number of mosquitoes and date/trap 
             index= function (x,y) coef(lm(y~x))[1],
             xlab = "Start date",
             ylab = "Number of mosquitoes",
             main = "Variaci?n en el n?mero de mosquitos por fecha y trampa",
             sub= "modelo mixto anidado",
             aspect ="xy")
update (p, panel = function (...){
  panel.xyplot(...)
  panel.abline(v=0, lty=3, col=2, lwd=0.5)
})

m1 <- lm (nrperspecies ~ Start_date, data = Palafolls) #Number of mosquitoes and date
plot (Palafolls$Start_date, Palafolls$nrperspecies, xlab = "start date", ylab ="number of mosquitoes", main= "relacion entre abundancia de mosquitos y fecha inicial de cada intervalo de captura", type = "n")
points (Palafolls$Start_date, Palafolls$nrperspecies, cex=1)
abline (m1)
m1
summary(m1)
anova (m1) 
#factors
Palafolls$trap <- Palafolls$trap_name 
trap[trap == "A_SP_PAL_1"] <- "1"
trap[trap == "A_SP_PAL_2"] <- "2"
trap[trap == "A_SP_PAL_3"] <- "3"
trap[trap == "A_SP_PAL_4"] <- "4"
trap[trap == "A_SP_PAL_5"] <- "5"
trap[trap == "A_SP_PAL_6"] <- "6"
Palafolls$trap <- as.factor(trap)

m2 <- lm(nrperspecies ~ Start_date * ftrampa, data =Palafolls)
plot (Palafolls$Start_date, Palafolls$nrperspecies, xlab= "start date", ylab= "number of mosquitoes", main="N?mero de mosquitos por inicio de intervalo de captura y por trampa", type = "n")   
points(Palafolls$Start_date, Palafolls$nrperspecies, cex =1)
for (i in 1:6) {
  index_trap <- Palafolls$trap ==i #correspondientes a las 5 trampas (i=2 a i=6)
  x1 <- Palafolls$Start_date [index_trap]
  y1 <- m2$fitted[index_trap]
  Ord <- order(x1)
  lines (x1[Ord], y1[Ord])
}
m3 <- lm(nrperspecies~Start_date + ftrampa, data= Palafolls) #EN ESTE MODELO, LA RELACI?N ENTRE EL N?MERO DE MOSQUITOS Y LA FECHA SE CONSIDERA IGUAL EN TODAS LAS TRAMPAS, PERO SE PERMITE QUE EL INTERCEPT (NUM MOSQUITOS EN CADA TRAMPA) VAR?E
plot (Palafolls$Start_date, Palafolls$nrperspecies, xlab= "Start date", ylab = "Number of mosquitoes", type ="n")
points (Palafolls$Start_date, Palafolls$nrperspecies, cex = 1)
for (i in 1:6){
  index_trap <- Palafolls$trap == i
  x1 <- Palafolls$Start_date [index_trap]
  y1 <- m3$fitted[index_trap]
  Ord <- order (x1)
  lines (x1[Ord], y1[Ord])
}
anova(m3)

m4 <- lm(nrperspecies~Start_date + ftrampa:Start_date, data= Palafolls) 
plot (Palafolls$Start_date, Palafolls$nrperspecies, xlab= "Start date", ylab = "Number of mosquitoes", type ="n")
points (Palafolls$Start_date, Palafolls$nrperspecies, cex = 1)
for (i in 1:6){
  index_trap <- Palafolls$trap == i
  x1 <- Palafolls$Start_date [index_trap]
  y1 <- m4$fitted[index_trap]
  Ord <- order (x1)
  lines (x1[Ord], y1[Ord])
}
anova(m4)

