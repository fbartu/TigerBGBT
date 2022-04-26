rm(list=ls()) #remove elements from the environment
setwd('/home/fbartu/Research/Laura_Blanco/Code/') #Fede


library(readxl)
library(lme4)
library(tidyverse)


J=read_excel('.././Data/Mar_i_Murtra_unabridged.xlsx') #relative path to the directory where you are

my_data <- read_excel('.././Data/Mar_i_Murtra_unabridged.xlsx',
                      skip = 2,col_names = TRUE , col_types = c("guess", "guess", "guess","guess",
                                                                "guess", "guess", "guess", "guess","guess", "guess",
                                                                "guess", "guess", "guess", "guess", "guess", "guess",
                                                                "date", "guess", "guess", "date","guess", "guess",
                                                                "guess", "guess", "guess", "guess", "guess","numeric",
                                                                "guess", "guess", "guess", "guess"))
J = my_data[2:length(my_data$Country),]



names(J)[16] <- "startdate" #change name of one column
names(J)[19] <- "enddate" 
names (J) [27] <- "numberaedes"
names (J) [7] <- "numbertrap"

library(ggplot2) #open ggplot2

ggplot(J, aes(x=enddate, y=numberaedes, colour=numbertrap,fill=numbertrap, shape=numbertrap)) + geom_point(size=1) + geom_smooth(method="lm")+ 
  xlab("Date(months)") + ylab("Number of Mosquitoes") #plotting all traps (and lm)

T2 <- J[which(J$numbertrap == "A_SP_BL_2"),] #filtering each trap
T3 <- J[which(J$numbertrap == "A_SP_BL_3"),]
T4 <- J[which(J$numbertrap == "A_SP_BL_4"),]
T5 <- J[which(J$numbertrap == "A_SP_BL_5"),]
T6 <- J[which(J$numbertrap == "A_SP_BL_6"),]

ggplot(T2, aes(y= numberaedes, x= enddate)) + geom_point()+ #plot trap2
  xlab("Date in months") + ylab("Number of mosquitoes")
ggplot(T3, aes(y= numberaedes, x= enddate)) + geom_point()+ #plot trap3
  xlab("Date in months") + ylab("Number of mosquitoes")
ggplot(T4, aes(y= numberaedes, x= enddate)) + geom_point()+ #plot trap4
  xlab("Date in months") + ylab("Number of mosquitoes")
ggplot(T5, aes(y= numberaedes, x= enddate)) + geom_point()+ #plot trap5
  xlab("Date in months") + ylab("Number of mosquitoes")
ggplot(T6, aes(y= numberaedes, x= enddate)) + geom_point()+ #plot trap6
  xlab("Date in months") + ylab("Number of mosquitoes")

DATE1<-sort(J$enddate)[1]
J$NUM_DAYS  <- difftime(J$enddate, DATE1 ,units="days")

T2$NUM_DAYS  <- difftime(T2$enddate, DATE1 ,units="days")
T3$NUM_DAYS  <- difftime(T3$enddate, DATE1 ,units="days")
T4$NUM_DAYS  <- difftime(T4$enddate, DATE1 ,units="days")
T5$NUM_DAYS  <- difftime(T5$enddate, DATE1 ,units="days")
T6$NUM_DAYS  <- difftime(T6$enddate, DATE1 ,units="days")


#we analyse all traps
J_all<-lm(numberaedes~NUM_DAYS,data=J)
summary(J_all)
#Call:
 # lm(formula = numberaedes ~ NUM_DAYS, data = J)

#Residuals:
 # Min     1Q Median     3Q    Max 
#-49.51 -37.59 -22.49   1.18 278.95 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  52.0367     6.7571   7.701 1.21e-12 ***
 # NUM_DAYS     -0.2627     0.1494  -1.758   0.0806 .  
#---
 # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 62.87 on 164 degrees of freedom
#Multiple R-squared:  0.0185,	Adjusted R-squared:  0.01251 
#F-statistic: 3.091 on 1 and 164 DF,  p-value: 0.08061

M2<-lm(numberaedes~NUM_DAYS,data=T2) #WE ANALYSE EACH TRAP NOW
M3<-lm(numberaedes~NUM_DAYS,data=T3)
M4<-lm(numberaedes~NUM_DAYS,data=T4)
M5<-lm(numberaedes~NUM_DAYS,data=T5)
M6<-lm(numberaedes~NUM_DAYS,data=T6)

#Then we see the summary
summary(M2)
summary(M3)
summary (M4)
summary (M5)
summary (M6)

#Modelo linear con pendientes iguales /que no tenemos
#lm2 <- lm(numberaedes ~ 1 + factor(numbertrap), data = J)
#lm1 <- glm(numberaedes ~ 1 + factor(numbertrap) + offset(as.numeric(NUM_DAYS)), data = J)

summary(lm1)


#MODELO LINEAR GENERALIZADO

glm1 <- glm(numberaedes ~ 1 + factor(numbertrap) + offset(as.numeric(NUM_DAYS)), data = J, family=poisson)

glm1 <- glm(numberaedes ~ 1 + offset(as.numeric(NUM_DAYS)), data = J,family = poisson)

summary(glm1)


#MODELO MIXTO LINEAL
lme1<- lmer(numberaedes~1 + (1| numbertrap) + offset (as.numeric(NUM_DAYS)) ,data=J)
summary(lme1)

library(esquisse)
esquisser()
