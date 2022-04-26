setwd("C://Users//lblan//OneDrive//Escritorio//CEAB//2020//RStudio//Modelling//Data")
getwd()
library(readxl)
mar_murtra <- read_excel("mar_murtra.xlsx", 
                         col_types = c("numeric", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "text", "text", "text", 
                                       "date", "date", "text", "text", "numeric", 
                                       "text"))
View(mar_murtra)
#ya tenemos los datos del jardín cargados, con todas las trampas juntas.
library(easypackages)
libraries("nlme", "influence.ME", "merTools", "lattice", "ggplot2", "tidyverse")
mar_murtra$ftrampa <- as.factor(mar_murtra$trap_id) #creamos el factor trampa
p <- xyplot (number_mosquitoes ~ start_date | ftrampa, mar_murtra, type = c("p", "r"), #grafico de relación del numero de mosquitos por fecha y por trampa(factor)
              index= function (x,y) coef(lm(y~x))[1],
              xlab = "Start date",
              ylab = "Number of mosquitoes",
              main = "Variación en el número de mosquitos por fecha y trampa",
              sub= "modelo mixto anidado",
              aspect ="xy")
update (p, panel = function (...){
  panel.xyplot(...)
  panel.abline(v=0, lty=3, col=2, lwd=0.5)
})

#PRIMER MODELO, SIMPLE -> RELACIÓN ENTRE ABUNDANCIA DE NUMERO DE MOSQUITOS Y FECHA INICIAL DE CADA INTERVALO DE CAPTURA
m1 <- lm (number_mosquitoes ~ start_date, data = mar_murtra) #MODELO BÁSICO
plot (mar_murtra$start_date, mar_murtra$number_mosquitoes, xlab = "start date", ylab ="number of mosquitoes", main= "relacion entre abundancia de mosquitos y fecha inicial de cada intervalo de captura", type = "n")
points (mar_murtra$start_date, mar_murtra$number_mosquitoes, cex=1)
abline (m1)
m1
summary(m1)
anova (m1) #el intercepto y la pendiente son iguales para todas las trampas

#SEGUNDO MODELO, INCORPORAMOS EL FACTOR TRAMPA
m2 <- lm(number_mosquitoes ~ start_date * ftrampa, data =mar_murtra)
plot (mar_murtra$start_date, mar_murtra$number_mosquitoes, xlab= "start date", ylab= "number of mosquitoes", main="Número de mosquitos por inicio de intervalo de captura y por trampa", type = "n")   
points(mar_murtra$start_date, mar_murtra$number_mosquitoes, cex =1)
for (i in 2:6) {
  index_trap <- mar_murtra$trap ==i #correspondientes a las 5 trampas (i=2 a i=6)
  x1 <- mar_murtra$start_date [index_trap]
  y1 <- m2$fitted[index_trap]
  Ord <- order(x1)
  lines (x1[Ord], y1[Ord])
}
trap <- mar_murtra$trap_id #no podemos utilizar en la condición el trap_id debido a los nombres que tienen las trampas, así que:
#1 duplicamos la columna trap_id, llamándola trap
#sustituimos en cada caso el A_SP_BL... por los números correspondientes
#añadimos la nueva columna trap como factor a nuestro dataframe
trap[trap == "A_SP_BL_2"] <- "2"
trap[trap == "A_SP_BL_3"] <- "3"
trap[trap == "A_SP_BL_4"] <- "4"
trap[trap == "A_SP_BL_5"] <- "5"
trap[trap == "A_SP_BL_6"] <- "6"
mar_murtra$trap <- as.factor(trap)
anova (m2)
summary(m2)

m3 <- lm(number_mosquitoes~start_date + ftrampa, data= mar_murtra) #EN ESTE MODELO, LA RELACIÓN ENTRE EL NÚMERO DE MOSQUITOS Y LA FECHA SE CONSIDERA IGUAL EN TODAS LAS TRAMPAS, PERO SE PERMITE QUE EL INTERCEPT (NUM MOSQUITOS EN CADA TRAMPA) VARÍE
plot (mar_murtra$start_date, mar_murtra$number_mosquitoes, xlab= "Start date", ylab = "Number of mosquitoes", type ="n")
points (mar_murtra$start_date, mar_murtra$number_mosquitoes, cex = 1)
for (i in 2:6){
  index_trap <- mar_murtra$trap == i
  x1 <- mar_murtra$start_date [index_trap]
  y1 <- m3$fitted[index_trap]
  Ord <- order (x1)
  lines (x1[Ord], y1[Ord])
}
anova(m3)

m4 <- lm(number_mosquitoes~start_date + ftrampa:start_date, data= mar_murtra) #numero de mosquitos base se mantiene igual en cada trampa, pero se permite que la relación mosquitos ~ fecha varíe
plot (mar_murtra$start_date, mar_murtra$number_mosquitoes, xlab= "Start date", ylab = "Number of mosquitoes", type ="n")
points (mar_murtra$start_date, mar_murtra$number_mosquitoes, cex = 1)
for (i in 2:6){
  index_trap <- mar_murtra$trap == i
  x1 <- mar_murtra$start_date [index_trap]
  y1 <- m3$fitted[index_trap]
  Ord <- order (x1)
  lines (x1[Ord], y1[Ord])
}
anova(m4) #igual que el m3... ?????

#¿Qué método es más plausible?
AIC (m1, m2, m3, m4) #hay que quedarse con el que tenga menos AIC
anova(m3, m1, test = "F")
anova (m2, m3, test = "F")
anova (m2, m4, test = "F")
#nos quedamos con el MODELO 2

#AHORA VAMOS A HACER UN MODELO MIXTO, CON INTERCEPTA ALEATORIA
library(nlme)
m5 <- lme(number_mosquitoes ~ start_date, data=mar_murtra, random = ~1|ftrampa)
summary(m5)
#AIC <- 1716.004
anova(m5)
#pendiente significativa
plot(m5)
fitted(m5, level = 0:1) #valores ajustados por el modelo con la función
f0 <- fitted(m5, level = 0) #prediccion componente fijo (numero mosquitos)
f1 <- fitted (m5, level = 1) #predicción componente variable (trampa)


#ploteamos el modelo, la raya gruesa hace referencia al componente fijo y las rayas finas a los componentes variables (trampa). Cada punto
#tiene indicado su número de trampa
plot(mar_murtra$start_date, f0, lwd = 4, type = "l", ylim = c(0,22), ylab = "Number of mosquitoes captured", xlab = "start date")
for (i in 2:6){
  x1 <- mar_murtra$start_date[mar_murtra$trap == i]
  y1 <- f1[mar_murtra$trap == i]
  k <- order(x1)
  lines (sort(x1), y1[k]) #efectos de cada trampa
}
text (mar_murtra$start_date, mar_murtra$number_mosquitoes, mar_murtra$trap, cex = 0.9)

#MODELO 6
m6 <- lme(number_mosquitoes ~ start_date, random = ~start_date |ftrampa, data = mar_murtra,  method = "REML") # restricted maximum likelihood (REML)
summary(m6)
#AIC <- 1720.153
#VARIANZA DE INTERCEPTA ALEATORIA 3.345E+01
VarCorr(m6) #obtener las varianzas de forma aislada
cor(ranef(m6)[, "(Intercept)"], ranef(m6)[, "start_date"]) #Comprobar correlacion
anova(m6)
# correlación 0.9999983, alta y positiva. las trampas con mayor abundancia de mosquitos sufrirán una pérdida más pequeña conforme vaya avanzando el tiempo (pasando el pico)
plot(m6)

#valores ajustados para nuestro modelo
F0 <- fitted (m6, level = 0)
F1 <- fitted (m6, level = 1)
plot (mar_murtra$start_date, F0, lwd = 4, type = "l", ylim = c(0,22), ylab = "number of mosquitoes captured", xlab = "date")
for (i in 2:6){
  x1 <- mar_murtra$start_date [mar_murtra$trap == i]
  y1 <- F1[mar_murtra$trap == i]
  K <- order (x1)
  lines(sort(x1), y1[K])
}
text (mar_murtra$start_date, mar_murtra$number_mosquitoes, mar_murtra$trap, cex = 0.9)

#glm y lme mas complejos
detach(package:nlme)
library(lme4)
#install.packages("Matrix")
library(Matrix)
lme5 <- lmer(number_mosquitoes ~ start_date + (1|ftrampa), data = mar_murtra)
lme6 <- lmer(number_mosquitoes~start_date + (start_date|ftrampa), data = mar_murtra)
library(visreg)
?visreg
visreg(fit=lme5, xvar= "start_date", ylab = "mosquitoes", xlab = "date")
visreg(fit=lme5, by = "ftrampa",
       re.form = ~ (1|ftrampa),
       xvar = "start_date", ylab = "mosquitoes", xlab = "date") #TENIENDO EN CUENTA EFECTOS ALEATORIOS (trampas)

#modelo intercepta aleatoria (omitimos pendiente aleatoria)
library (nlme)
m55 <- lme(number_mosquitoes ~ start_date, random = ~ 1 | ftrampa, data = mar_murtra, method = "REML")
m66 <- lme (number_mosquitoes ~ start_date, random = ~ start_date - 1 | ftrampa, data = mar_murtra, method = "REML")
anova (m55, m66) #comparamos modelos para evaluar signif de pendiente aleatoria
anova (m66, m6) #evaluamos signif intercepta aleatoria, que sale p > 0.001
anova (lme5, lme6, refit = F)

#comparamos modelos con/sin efectos aleatorios
lmer5.1 <- lmer(number_mosquitoes ~ start_date + (1|ftrampa), data = mar_murtra)
lm5 <- lm (number_mosquitoes ~ start_date, data = mar_murtra)
as.numeric (2* (logLik(lmer5.1) - logLik(lm5, REML = TRUE))) #EL REML ES TRUE PORQUE SE COMPARAN MODELOS QUE DIFIEREN EN SUS EFECTOS ALEATORIOS

lrstat <- numeric(100)
for (i in 1:100) {
  mmosquitoes <- unlist(simulate(lm5)) #creacion de respuesta sobre modelo simple
  lmr5 <- lm(mmosquitoes~start_date, mar_murtra) #reconstruimos el modelo con la nueva respuesta
  lmer5.1r <- refit(lmer5.1, mmosquitoes)
  lrstat[i] <- 2*(logLik(lmer5.1r) - logLik (lm5, REML = TRUE))
}
mean (lrstat > 12.72075)

#La comparacion de modelos con la misma estructura de componente aleatorio pero distinta
#componente fija se debe hacer con estimadores de máxima verosimilitud (ML) en vez de REML
#entonces para comprobar la significacion de efectos fijos mediante LRT, REML = FALSE

#COMPROBAR SI NAP ES UN PREDICTOR INTERESANTE (REDUCE LA DESV RESIDUAL?)
lmer5.0 <- lmer(number_mosquitoes ~ 1 + (1|ftrampa), data = mar_murtra, REML = FALSE)
lmer5.1 <- lmer(number_mosquitoes ~ start_date + (1|ftrampa), data = mar_murtra, REML = FALSE)
anova (lmer5.1, lmer5.0)
#lmer5.1  0.0001061***
#el efecto de la fecha(el paso de los meses) sobre el numero de mosquitos capturados es SIGNIFICATIVO

#usamos la aproximación de Kenward-Roger para calculo de grados de libertad
library(pbkrtest)
KRmodcomp(largeModel=lmer5.0, smallModel =lmer5.1)

#aproximacion Satterthwaite 
library(lmerTest)
lmer5.1b <- lmerTest::lmer(number_mosquitoes ~ start_date + (1|ftrampa), data = mar_murtra, REML = TRUE)
summary(lmer5.1b)

#DIAGNOSIS DEL MODELO
plot(lme5)

#en modelos mixtos lineales type = "response" 
#y type = "pearson" generan el mismo resultado, pero esto no será asi
#en los GLM
plot (lme5, resid(., type = "response") ~fitted(.), abline = 0)
plot (lme5, resid(., type = "response") ~ fitted(.)|ftrampa, abline = 0, lty =3)
plot (lme5, ftrampa ~resid(.))

#normalidad de los residuos y los efectos aleatorios
par (mfrow = c(1,1))
qqnorm(resid(lme5))
qqline(resid(lme5))

par (mfrow= c(1,1))
qqnorm(unlist(ranef(lme5)))
qqline(unlist(ranef(lme5)))

#gráficos de residuos frente a la variable predictora y de normalidad de residuos
par(mfrow = c(2,2))
plot (x=predict(lme5, re.form = NA), y = resid (lme5), #re.form =NA para obtener las predicciones del componente fijo
      xlab = "valores ajustados con lme4",
      ylab = "residuos")
abline (h=0, lty = 3)

plot(x= mar_murtra$start_date, y= resid(lme5), xlab = "atart_date", ylab = "residuos")
abline(h=0, lty=3)

qqnorm(resid(lme5))
qqline(resid(lme5), lty=2, col =2)

#Ajustar los valores por el modelo
F0 <- predict(lme5, re.form = NA) #Predicciones del componente fijo (poblacional)
F1 <- predict(lme5) #predicciones que incluyen el componente aleatorio (grupo)
plot (mar_murtra$start_date, F0, lwd= 4, type = "l", ylim =c(0,22), ylab = "Number of mosquitoes captured", xlab = "start date") #linea gruesa, efecto medio
for (i in 2:6) {
  x1 <- mar_murtra$start_date[mar_murtra$ftrampa == i]
  y1 <- F1[mar_murtra$ftrampa ==1]
  K <- order(x1)
  lines(sort(x1), y1[K]) #efectos de cada trampa, lineas finas
}
text(mar_murtra$start_date, mar_murtra$number_mosquitoes, mar_murtra$trap, cex = 0.9)

#modelo de efectos aleatorios
library(nlme)
m7 <- lme (number_mosquitoes ~ 1, random = ~1 | ftrampa, data = mar_murtra)
summary (m7)
lme7 <- lmer(number_mosquitoes ~ (1 | ftrampa), data = mar_murtra)
summary (lme7)

par(mfrow= c(2,2))
plot(m7)

plot (m7, resid (., type = "p") ~fitted (.), abline = 0)
plot (m7, resid (., type = "p") ~ fitted (.) | ftrampa, abline = 0, lty = 3)
plot (m7, ftrampa ~ resid(.))

#normalidad de los residuos y de los efectos aleatorios
qqnorm (m7, ~resid(.))
qqnorm (m7, ~ranef(.))

#graficos lme7 -> residuos vs var predictora, y normalidad de residuos
par (mfrow = c(2,2))
plot(x = fitted(lme7, level = 0), y = resid(lme7), xlab = "valores ajustados", ylab = "residuos")
abline (h=0, lty =3)

plot(x= mar_murtra$start_date, y = resid(lme7), xlab = "dates of capture in bgtraps", ylab = "residuos")
abline (h=0, lty =3)

qqnorm(resid(lme7))
qqlline(resid(lme7), lty=2, col =2)

F0 <- fitted(m7, level =0)
F1 <- fitted(m7, level = 1)
plot(mar_murtra$start_date, F0, lwd= 4, type = "l", ylim = c(0,22), ylab="Mosquitoes", xlab = "date")
for (i in 2:6){
  x1 <- mar_murtra$start_date[mar_murtra$trap ==i]
  y1 <- F1[mar_murtra$trap == i]
  K <- order(x1)
  lines (sort(x1), y1[K])
}
text(mar_murtra$number_mosquitoes, mar_murtra$start_date, mar_murtra$trap, cex =0.9)

#analisis del modelo
P1 <- round(predict(lmer5.1),2) #prediccion que incluye componente fijo y aleatorio
P0 <- round(predict(lmer5.1, re.form = NA), 2) 
mi.tabla <- data.frame(Observacion = mar_murtra$number_mosquitoes, Prediccion = round(fitted(lmer5.1), 2), Pred1 = P1, Pred0 = P0)
head (mi.tabla, 10)
#pred0 es la estima para una trampa nueva, mientras que pred1 es la estima para una trampa dada

#BOOTSTRAP
mySumm <- function(.) {
  predict(., newdata = mar_murtra, re.form = NULL)
}
cuantos <- 250
tic <- proc.time() # para controlar el tiempo que tarda
boot1 <- bootMer(lmer5.1, mySumm, nsim = cuantos, use.u = FALSE,
                       type = "parametric")
tac <- proc.time() - tic
tac
dim(boot1$t)
head(boot1$t)

sumBoot2 <- function(mi.boot.t) {
  return(data.frame(fit = apply(mi.boot.t, 2, function(x) as.numeric(quantile(x,
                                                                              probs = 0.5, na.rm = TRUE))), lwr = apply(mi.boot.t,
                                                                                                                        2, function(x) as.numeric(quantile(x, probs = 0.025,
                                                                                                                                                           na.rm = TRUE))), upr = apply(mi.boot.t, 2, function(x) as.numeric(quantile(x,
                                                                                                                                                                                                                                      probs = 0.975, na.rm = TRUE)))))
}
(PI.boot1 <- sumBoot2(mi.boot.t = boot1$t))


plot(x = mar_murtra$start_date, y = mar_murtra$number_mosquitoes)
lines(x = mar_murtra$start_date[order(mar_murtra$start_date)], y = PI.boot1$fit[order(mar_murtra$start_date)],
      lty = 3)
# representa la predicción media del modelo (poblacional)
nuevos.datos <- data.frame(start_date = seq(from = min(mar_murtra$start_date), to = max(mar_murtra$start_date),
                                     length.out = 50))
lines(x = nuevos.datos$start_date, y = predict(lmer5.1, newdata = nuevos.datos,
                                        re.form = NA), lty = 1, lwd = 2, col = "blue")
# representa los límites del intervalo de confianza
# 'bootstrap'
lines(x = mar_murtra$start_date[order(mar_murtra$start_date)], y = PI.boot1$lwr[order(mar_murtra$start_date)],
      lty = 3, col = "grey", lwd = 2)
lines(x = mar_murtra$start_date[order(mar_murtra$start_date)], y = PI.boot1$upr[order(mar_murtra$start_date)],
      lty = 3, col = "grey", lwd = 2)

#con ggplot
library(ggplot2)
nuevos.datos <- data.frame(start_date = seq(from = min(mar_murtra$start_date), to = max(mar_murtra$start_date),
                                     length.out = 50))
PI.boot1o <- PI.boot1
PI.boot1o$start_date <- mar_murtra$start_date
PI.boot1o <- PI.boot1o[order(mar_murtra$start_date), ]
# primero representa los puntos observados usando la base de
# datos original
ggplot(data = mar_murtra, aes(x = start_date, y = number_mosquitoes)) + geom_point(shape = 1) + theme_light() +
  # representa la mediana de las predicciones 'bootstrap':
  geom_line(data = PI.boot1o, aes(y = fit), size = 0.75, lty = 2,
            col = "blue") + # representa la predicción media del modelo (poblacional):
  geom_line(data = nuevos.datos, aes(y = predict(lme5, newdata = nuevos.datos,
                                                 re.form = NA)), size = 1, lty = 1, col = "green") + # representa los límites del intervalo de confianza
  # 'bootstrap'
  geom_ribbon(data = PI.boot1o, aes(x = start_date, ymin = lwr, ymax = upr),
              alpha = 0.4, fill = "pink", inherit.aes = FALSE)

#Intervalos de confianza basados en simulaciones
library(merTools)
lmer5.1 <- lmer(number_mosquitoes ~ start_date + (1 | ftrampa), data = mar_murtra)
reSims <- REsim(lmer5.1, n.sims = cuantos)
plotREsim(reSims, stat = "median", sd = TRUE, labs = TRUE)
tic2 <- proc.time()
PI <- predictInterval(merMod = lmer5.1, newdata = mar_murtra, level = 0.95,
                      n.sims = 500, stat = "median")
tac2 <- proc.time() - tic2
tac2

#intervalos de confianza a mano
group.sd <- as.data.frame(VarCorr(lmer5.1))$sdcor[1]
resid.sd <- as.data.frame(VarCorr(lmer5.1))$sdcor[2]
# VarCorr(lmer5.1) da las estimas de varianza
pv <- matrix(nrow = 100, ncol = nrow(mar_murtra), data = -99) # mejor 1000
tic3 <- proc.time()
for (i in 1:100) {
  y <- unlist(simulate(lmer5.1))
  lmer5.1.bootstrapeado <- refit(lmer5.1, y)
  pv[i, ] <- predict(lmer5.1.bootstrapeado, re.form = ~0) +
    rnorm(n = 1, sd = group.sd) + rnorm(n = 1, sd = resid.sd)
  # no añadir estas dos últimas fuentes de variación si quieres
  # el intervalo de confianza para la predicción media en lugar
  # de para la predicción para un punto (el intervalo de
  # predicción)
}
tac3 <- proc.time() - tic3
(PI.boot2 <- sumBoot2(mi.boot.t = pv))
comp.data <- rbind(data.frame(Metodo = "predictInterval()", x = (1:nrow(PI)) -
                                0.1, PI), data.frame(Metodo = "bootMer()", x = (1:nrow(PI.boot1)) +
                                                       0.1, PI.boot1), data.frame(Metodo = "simulate-predict", x = (1:nrow(PI.boot2)) +
                                                                                    0.3, PI.boot2))
ggplot(aes(x = x, y = fit, ymin = lwr, ymax = upr, color = Metodo),
       data = comp.data) + geom_point() + geom_linerange() + labs(x = "Punto de muestreo",
                                                                  y = "Predicción w/ 95% PI") + theme_bw() + theme(legend.position = "bottom") +
  scale_color_brewer(type = "qual", palette = 2)
