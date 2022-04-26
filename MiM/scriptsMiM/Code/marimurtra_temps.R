setwd("C://Users//lblan//OneDrive//Escritorio//CEAB//2020//RStudio//Modelling//Data")
getwd()
library(readxl)
library (MASS)
library(easypackages)
libraries("nlme", "influence.ME", "merTools", "lattice", "ggplot2", "tidyverse")
mar_murtra_glm <- read_excel("mar_murtra.xlsx")
temp <- read_excel("temperaturas_mar_murtra.xlsx")
library(glmmML)
library(MCMCglmm)
library(DHARma)
library(GLMMadaptive)
library(gridExtra)
library(plotMCMC)
library(effects)

#creamos los distintos factores para nuestros futuros modelos. En este caso,
#las trampas y la temperatura son factores que influyen en la recogida de mosquitos
mar_murtra_glm$ftrap <- as.factor(mar_murtra_glm$trap_id)
mar_murtra_glm$ftmed <- as.factor(temp$tmed)
mar_murtra_glm$ftmin <- as.factor(temp$tmin)
mar_murtra_glm$fmax <- as.factor(temp$tmax)
mar_murtra_glm$tmed <- temp$tmed
mar_murtra_glm$tmin <- temp$tmin
mar_murtra_glm$tmax <- temp$tmax

#con la media de la temperatura
p <- xyplot(mar_murtra_glm$number_mosquitoes ~ mar_murtra_glm$tmed | mar_murtra_glm$ftrap, mar_murtra_glm, type = c("p","r"),
            index = function (x,y) coef(lm(y~x))[1],
            xlab = "start date",
            ylab = "number of mosquitoes",
            main = "Number of mosquitoes captured in different months, influenced by temperature",
            sub = "Un modelo mixto anidado",
            aspect = "xy")
update(p, panel = function(...) {
  panel.xyplot(...)
  panel.abline(v=0,lty=3,col=2,lwd=0.5)
})


tmp1 <- lm(number_mosquitoes ~ tmed, data = mar_murtra_glm) # este es el modelo
plot(mar_murtra_glm$tmed, mar_murtra_glm$number_mosquitoes, xlab = "temp med", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmed, mar_murtra_glm$number_mosquitoes, cex = 1)
abline(tmp)

#con las temperaturas maximas
p <- xyplot(mar_murtra_glm$number_mosquitoes ~ mar_murtra_glm$tmax | mar_murtra_glm$ftrap, mar_murtra_glm, type = c("p","r"),
            index = function (x,y) coef(lm(y~x))[1],
            xlab = "start date",
            ylab = "number of mosquitoes",
            main = "Number of mosquitoes captured in different months, influenced by temperature",
            sub = "Un modelo mixto anidado",
            aspect = "xy")
update(p, panel = function(...) {
  panel.xyplot(...)
  panel.abline(v=0,lty=3,col=2,lwd=0.5)
})

tmp2 <- lm(number_mosquitoes ~ tmax, data = mar_murtra_glm) # este es el modelo
plot(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, xlab = "temp max", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, cex = 1)
abline(tmp)

#con las temperaturas minimas
p <- xyplot(mar_murtra_glm$number_mosquitoes ~ mar_murtra_glm$tmin | mar_murtra_glm$ftrap, mar_murtra_glm, type = c("p","r"),
            index = function (x,y) coef(lm(y~x))[1],
            xlab = "start date",
            ylab = "number of mosquitoes",
            main = "Number of mosquitoes captured in different months, influenced by temperature",
            sub = "Un modelo mixto anidado",
            aspect = "xy")
update(p, panel = function(...) {
  panel.xyplot(...)
  panel.abline(v=0,lty=3,col=2,lwd=0.5)
})

tmp3 <- lm(number_mosquitoes ~ tmin, data = mar_murtra_glm) # este es el modelo
plot(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, xlab = "temp min", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, cex = 1)
abline(tmp)

anova(tmp)
trap <- mar_murtra_glm$trap_id
trap[trap == "A_SP_BL_2"] <- "2"
trap[trap == "A_SP_BL_3"] <- "3"
trap[trap == "A_SP_BL_4"] <- "4"
trap[trap == "A_SP_BL_5"] <- "5"
trap[trap == "A_SP_BL_6"] <- "6"
mar_murtra_glm$trap <- as.factor(trap)

#Probemos con una relación diferente (intercepta y pendiente) para cada trampa:
#temperatura media
tmpMED <- lm(number_mosquitoes ~ tmed * trap, data = mar_murtra_glm)
plot(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, xlab = "temp med", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <- mar_murtra_glm$trap == i 
  x1 <- mar_murtra_glm$tmin[index_trap]
  y1 <- tmpMED$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

#ahora con temperaturas maximas
tmpMAX <- lm(number_mosquitoes ~ tmax * trap, data = mar_murtra_glm)
plot(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, xlab = "temp max", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <- mar_murtra_glm$trap == i 
  x1 <- mar_murtra_glm$tmax[index_trap]
  y1 <- tmpMAX$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

#y con temperaturas minimas
tmpMMIN <- lm(number_mosquitoes ~ tmin * trap, data = mar_murtra_glm)
plot(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, xlab = "temp min", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <- mar_murtra_glm$trap == i 
  x1 <- mar_murtra_glm$tmin[index_trap]
  y1 <- tmpMMIN$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

#Probemos a ajustar un modelo intermedio, en el que la relación mosquitos-temperatura se considera igual en todas las trampas,
#pero se permite que la temperatura de cada trampa (la intercepta) varíe:

#TEMPERATURA MEDIA
Modelomed <- lm(number_mosquitoes ~ tmed + factor(trap), data = mar_murtra_glm)
plot(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, xlab = "temp med", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <-  mar_murtra_glm$trap == i
  x1 <-mar_murtra_glm$tmin[index_trap]
  y1 <- Modelomed$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

#TEMPERATURA MAXIMA
Modelomax <- lm(number_mosquitoes ~ tmax + factor(trap), data = mar_murtra_glm)
plot(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, xlab = "temp max", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <-  mar_murtra_glm$trap == i
  x1 <-mar_murtra_glm$tmax[index_trap]
  y1 <- Modelomax$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}
#TEMPERATURA MINIMA
Modelomin <- lm(number_mosquitoes ~ tmin + factor(trap), data = mar_murtra_glm)
plot(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, xlab = "temp min", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <-  mar_murtra_glm$trap == i
  x1 <-mar_murtra_glm$tmin[index_trap]
  y1 <- Modelomin$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

anova(Modelomed)
anova(Modelomin)
anova(Modelomax)

#Finalmente, probemos con otro modelo intermedio, en el que la temperatura de base se considera igual en todas
#las trampas, pero se deja variar la relación 'Mosquitos - Temperatura':

#TEMPERATURA MEDIA
tmp4 <- lm(number_mosquitoes ~ tmed + factor(trap):tmed, data = mar_murtra_glm)
plot(mar_murtra_glm$tmed, mar_murtra_glm$number_mosquitoes, xlab = "tmed", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <-  mar_murtra_glm$trap == i
  x1 <-mar_murtra_glm$tmed[index_trap]
  y1 <- tmp4$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

#TEMPERATURA MAXIMA
tmp5 <- lm(number_mosquitoes ~ tmax + factor(trap):tmax, data = mar_murtra_glm)
plot(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, xlab = "tmax", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmax, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <-  mar_murtra_glm$trap == i
  x1 <-mar_murtra_glm$tmax[index_trap]
  y1 <- tmp5$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

#TEMPERATURA MINIMA
tmp6 <- lm(number_mosquitoes ~ tmin + factor(trap):tmin, data = mar_murtra_glm)
plot(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, xlab = "tmin", ylab = "number of mosquitoes",
     type = "n")
points(mar_murtra_glm$tmin, mar_murtra_glm$number_mosquitoes, cex = 1)
for (i in 2:6) {
  index_trap <-  mar_murtra_glm$trap == i
  x1 <-mar_murtra_glm$tmin[index_trap]
  y1 <- tmp6$fitted[index_trap]
  Ord <- order(x1)
  lines(x1[Ord], y1[Ord])
}

anova(tmp5)
anova(tmp6)

AIC(tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, Modelomax, Modelomed, Modelomin)

#el tmp6 en este caso sería el más acertado (cogiendo temperaturas minimas)

#CONSTRUCCION DE UN MODELO DE INTERCEPTA ALEATORIA
modelomedio <- lme(number_mosquitoes ~ tmed, data= mar_murtra_glm, random = ~1|ftrap)
summary(modelomedio)
plot(modelomedio)
fitted(modelomedio, level = 0:1)
F0 <- fitted(modelomedio, level = 0)
F1 <- fitted(modelomedio, level = 1)
plot(mar_murtra_glm$tmed, F0, lwd = 4, type = "l", ylim = c(0, 22), ylab = "Mosquitoes",
     xlab = "tmed") # línea gruesa: efecto fijo
for (i in 2:6) {
  x1 <-mar_murtra_glm$tmed[mar_murtra_glm$trap == i]
  y1 <- F1[mar_murtra_glm$trap == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}

ctrl <- lmeControl(opt='optim'); #para solventar el error de "iteration limit reached without convergence
modelomedio2 <- lme(number_mosquitoes ~ tmed, random= ~tmed|ftrap, data = mar_murtra_glm, control = ctrl, method = "REML")
summary(modelomedio2)
VarCorr(modelomedio2)
cor(ranef(modelomedio2)[, "(Intercept)"], ranef(modelomedio2)[, "tmed"])
anova(modelomedio2)
plot(modelomedio2)

#ahora con temperaturas maximas
modelomaximas <- lme(number_mosquitoes ~ tmax, data= mar_murtra_glm, random = ~1|ftrap)
summary(modelomaximas)
plot(modelomaximas)
fitted(modelomaximas, level = 0:1)
F0 <- fitted(modelomaximas, level = 0)
F1 <- fitted(modelomaximas, level = 1)
plot(mar_murtra_glm$tmax, F0, lwd = 4, type = "l", ylim = c(0, 22), ylab = "Mosquitoes",
     xlab = "tmax") # línea gruesa: efecto fijo
for (i in 2:6) {
  x1 <-mar_murtra_glm$tmax[mar_murtra_glm$trap == i]
  y1 <- F1[mar_murtra_glm$trap == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}

ctrl <- lmeControl(opt='optim'); #para solventar el error de "iteration limit reached without convergence
modelomaximas2 <- lme(number_mosquitoes ~ tmax, random= ~tmax|ftrap, data = mar_murtra_glm, control = ctrl, method = "REML")
summary(modelomaximas2)
VarCorr(modelomaximas2)
cor(ranef(modelomaximas2)[, "(Intercept)"], ranef(modelomaximas2)[, "tmax"])
anova(modelomaximas2)
plot(modelomaximas2)
#temperaturas minimas...
modelominimas <- lme(number_mosquitoes ~ tmin, data= mar_murtra_glm, random = ~1|ftrap)
summary(modelominimas)
plot(modelominimas)
fitted(modelominimas, level = 0:1)
F0 <- fitted(modelominimas, level = 0)
F1 <- fitted(modelominimas, level = 1)
plot(mar_murtra_glm$tmin, F0, lwd = 4, type = "l", ylim = c(0, 22), ylab = "Mosquitoes",
     xlab = "tmin") # línea gruesa: efecto fijo
for (i in 2:6) {
  x1 <-mar_murtra_glm$tmin[mar_murtra_glm$trap == i]
  y1 <- F1[mar_murtra_glm$trap == i]
  K <- order(x1)
  lines(sort(x1), y1[K])
}

ctrl <- lmeControl(opt='optim'); #para solventar el error de "iteration limit reached without convergence
modelominimas2 <- lme(number_mosquitoes ~ tmin, random= ~tmin|ftrap, data = mar_murtra_glm, control = ctrl, method = "REML")
summary(modelominimas2)
VarCorr(modelominimas2)
cor(ranef(modelominimas2)[, "(Intercept)"], ranef(modelominimas2)[, "tmax"])
anova(modelominimas2)
plot(modelominimas2)

detach(package:nlme)
library(lme4)
lme5 <- lmer(number_mosquitoes ~ tmed + (1|ftrap), data =mar_murtra_glm)
lme6 <- lmer(number_mosquitoes ~ tmed + (tmed|ftrap), data = mar_murtra_glm)
summary(lme5)
library(visreg)
visreg(fit=lme5, xvar = "tmed", ylab = "number of mosquitoes", xlab= "tmed")

visreg(fit = lme5, by = "ftrap",
       re.form = ~ (1 | ftrap),  xvar = "tmed", ylab = "mosquitoes", xlab = "tmed")

plot(lme5)
plot(lme5, resid(., type = "response") ~ fitted(.) | ftrap,
     abline = 0, lty = 3)

plot(lme5, ftrap ~ resid(.))

par(mfrow = c(1, 1))
qqnorm(resid(lme5))
qqline(resid(lme5))


par(mfrow = c(1, 1))
qqnorm(unlist(ranef(lme5)))
qqline(unlist(ranef(lme5)))

library(nlme)
modelofinal <- lme(number_mosquitoes ~ 1, random = ~1 | ftmin, data =mar_murtra_glm)
lme7 <- lmer(number_mosquitoes ~ (1|ftmin), data = mar_murtra_glm)
summary(lme7)

par(mfrow = c(2, 2))
plot(modelofinal)



