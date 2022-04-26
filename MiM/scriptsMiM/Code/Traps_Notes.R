rm(list=ls())
setwd('/home/fbartu/Research/Mosquito_Alert/Modelling/Traps/') #Fede
#setwd('C:\\jjjj\\\\') ##Laura

#Dependencies
#library (lattice)
#library (MASS)
library(readxl)
library(ggplot2)

#FUNCTIONS DECLARATION
#fit is the ouput object of lm function
ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

#D=read_excel('/home/fbartu/Research/Mosquito_Alert/Modelling/Traps/Data/Trampas.xlsx') ##absolute path
D=read_excel('./Data/Trampas.xlsx') #relative path to the directory where you are

#To understand what type of object are we dealing with
#class(D)
#str(D)
#head(D,20)
#tail(D)
#D2=D[1:10,1:10]

##ADDING other columns to D:we add a column with the number of days
DATE1<-sort(D$END_DATA)[1]
D$NUM_DAYS  <- difftime(D$END_DATA,DATE1,units="days")

#We do not need Julian DAYS for anything!!
#JEND_DATA<-julian(D$END_DATA)
#JSTART_DATA<-julian(D$START_DATA)
#D=cbind(D,JSTART_DATA,JEND_DATA)

#JUlian DAYS loop
#L=dimensiones_D[1]
#L=length(D$TRAMPA)
#for (i in 1:length(D$TRAMPA))  { 
#  if ( (julian(D$END_DATA[i])-julian(D$START_DATA[i])) == (D$END_DATA[i]-D$START_DATA[i])) {x[i]=1} else {x[i]=0}
#}
#V=which(x==0)

#FILTERING
D5=D[which(D$TRAP=="A_SP_BL_5"),]
D2=D[which(D$TRAP=="A_SP_BL_2"),]
D3=D[which(D$TRAP=="A_SP_BL_3"),]
D6=D[which(D$TRAP=="A_SP_BL_6"),]

#Simple graph with non corrected and corrected mosquito numbers
#plot (x=Trampa2$`END DATA`, y= Trampa2$NUMBER)
#ploting only Trap 5
#ggplot(D5, aes(x=END_DATA, y=NUMBER)) + geom_point() + geom_smooth(method="lm")
#+ xlab("Date (months)") + ylab("Number of Mosquitoes")

#ploting all traps
ggplot(D, aes(x=END_DATA, y=NUMBER)) + geom_point() + geom_smooth(method="lm")+ 
  xlab("Date(months)") + ylab("Number of Mosquitoes")

M_all<-lm(NUMBER~NUM_DAYS,data=D)
summary(M_all)
ggplotRegression(M_all)


ggplot(D, aes(x=END_DATA, y=NUMBER,colour=TRAP,fill=TRAP,shape=TRAP)) + geom_point(size=2) + geom_smooth(method="lm")+ 
  xlab("Date(months)") + ylab("Number of Mosquitoes")

M2<-lm(NUMBER~NUM_DAYS,data=D2)
M3<-lm(NUMBER~NUM_DAYS,data=D3)
M5<-lm(NUMBER~NUM_DAYS,data=D5)
M6<-lm(NUMBER~NUM_DAYS,data=D6)

summary(M2)
summary(M3)
summary(M5)
summary(M6)



Aedpoisson <- glm(Trampa2$NUMBER ~ 1, data = Trampa2, family = poisson)
Aedpoisson

AedpoissonC <- glm(Trampa2$CORREGIDO ~ 1, data = Trampa2, family = poisson)
AedpoissonC


#COMPARACI?N DEL MODELO ORIGINAL CON EL NULO, QUE PERMITIR? COMPROBAR LA SIGNIFICACI?N DEL MODELO EN SU CONJUNTO
Aedpoisson.nulo <- glm(Trampa2$NUMBER ~ 1, data = Trampa2, family = poisson)
anova(Aedpoisson, Aedpoisson.nulo, test = "Chisq")

#COMPARACI?N DEL MODELO CORREGIDO CON EL NULO, QUE PERMITIR? COMPROBAR LA SIGNIFICACI?N DEL MODELO EN SU CONJUNTO
Aedpoissonc.nulo <- glm(Trampa2$CORREGIDO ~ 1, data = Trampa2, family = poisson)
anova(AedpoissonC, Aedpoissonc.nulo, test = "Chisq")

anova(Aedpoisson, test = "Chisq")
anova(AedpoissonC, test = "Chisq")

#valores ajustados -> funci?n exponencial de Trampa2$CORREGIDO
par(mar = c(5, 5, 2, 2))
Trampa2 <- data.frame(Trampa2$CORREGIDO = seq(0.804, 4.865, length = 25))
P1 <- predict(AedpoissonC, newdata = Trampa2, type = "response")
plot(x = Trampa2$`END DATA`, y = Trampa2$CORREGIDO, ylim = c(0, 300),
     xlab = "Date (months)", ylab = "Total abundance Aedes albopictus",
     cex.lab = 1)
lines(Trampa2$CORREGIDO, P1, lwd = 3)

#residuos y valores ajustados
E1 <- resid(AedpoissonC, type = "pearson")
F1 <- fitted(AedpoissonC)
eta <- predict(AedpoissonC, type = "link")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
# el tipo de gr?fico m?s com?n por defecto en R (con 'eta')
plot(x = eta, y = E1, xlab = "Eta", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
plot(x = Trampa2$`END DATA`, y = E1, xlab = "Date (months)", ylab = "Pearson residuals",
     cex.lab = 1.5, pch = 16)
abline(h = 0, v = 0, lty = 2)
boxplot(E1 ~ Trampa2$NUMBER, ylab = "Pearson residuals", data = Trampa2,
        cex.lab = 1.5, xlab = "Period")
abline(h = 0, v = 0, lty = 2)

