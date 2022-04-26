library (lattice)
library (MASS)
#gráfico simple, tanto en número crudo como número corregido por días/trampa
plot (x=Trampa6$`END DATA`, y= Trampa6$NUMBER)
plot (x=Trampa6$`END DATA`, y= Trampa6$CORREGIDO)
#regresión lineal
Aed0 <- lm(Trampa6$`END DATA`~ Trampa6$NUMBER, data = Trampa6)
Aed0
#regresión lineal versión corregida
Aed0corregido <- lm(Trampa6$`END DATA`~ Trampa6$CORREGIDO, data = Trampa6)
Aed0corregido
#cálculo de residuo y valores ajustados
E0 <- resid(Aed0)
F0 <- fitted(Aed0)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))
plot(x = F0, y = E0, xlab = "Fitted values", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
plot(x = Trampa6$`END DATA`, y = Trampa6$NUMBER, xlab = "Date (months)",
     ylab = "Abundance of Aedes albopictus in Trap 6", cex.lab = 1.5, pch = 16)
abline(Aed0, lwd = 5)
#cálculo de residuo y valores ajustados (corregido)
E0c <- resid(Aed0corregido)
F0c <- fitted(Aed0corregido)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))
plot(x = F0c, y = E0c, xlab = "Fitted values", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
plot(x = Trampa6$`END DATA`, y = Trampa6$CORREGIDO, xlab = "Date (months)",
     ylab = "Abundance of Aedes albopictus in Trap 6", cex.lab = 1.5, pch = 16)
abline(Aed0corregido, lwd = 5)
#otras herramientas de evaluación
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))
plot(x = Trampa6$`END DATA`, y = E0, xlab = "Date (months)", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
boxplot(E0 ~ Trampa6$NUMBER, data = Trampa6, cex.lab = 1.5, xlab = "Trampa6",
        ylab = "Residuals")
#versión corregida
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))
plot(x = Trampa6$`END DATA`, y = E0c, xlab = "Date (months)", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
boxplot(E0c ~ Trampa6$CORREGIDO, data = Trampa6, cex.lab = 1.5, xlab = "Trampa6",
        ylab = "Residuals")
hist(E0c, main = "", breaks = 20, cex.lab = 1.5, xlab = "Residuals")
#abundancias esperadas bajo una regresión con valores normales
par(mfrow = c(1, 1), mar = c(5, 5, 3, 2))
plot(x = Trampa6$`END DATA`, y = Trampa6$NUMBER, xlab = "date (months)",
     ylab = "Total abundance of Aedes albopictus", cex.lab = 1.5, pch = 1, ylim = c(0,
                                                                                    300))
abline(Aed0, lwd = 5)
abline(h = 0, lty = 2)
range(Trampa6$`END DATA`)
#Lo mismo pero con la versión corregida
par(mfrow = c(1, 1), mar = c(5, 5, 3, 2))
plot(x = Trampa6$`END DATA`, y = Trampa6$CORREGIDO, xlab = "date (months)",
     ylab = "Total abundance of Aedes albopictus", cex.lab = 1.5, pch = 1, ylim = c(0,
                                                                                    300))
abline(Aed0corregido, lwd = 5)
abline(h = 0, lty = 2)
range(Trampa6$`END DATA`)

#En la regresión de Poisson asumimos que la VR se distribuye según una función de Poisson con un parámetro "mu" ?? (la "media") a determinar. La media es igual a la varianza.
par(mfrow = c(2, 2))
x <- seq(0, 50, 1)
plot(dpois(x, lambda = 1, log = FALSE), type = "h", xlab = "x",
     ylab = "curva de densidad de probabilidad", lwd = 2, main = "valores posibles con mu = 1")
plot(dpois(x, lambda = 5, log = FALSE), type = "h", xlab = "x",
     ylab = "curva de densidad de probabilidad", lwd = 2, main = "valores posibles con mu = 5")
plot(dpois(x, lambda = 15, log = FALSE), type = "h", xlab = "x",
     ylab = "curva de densidad de probabilidad", lwd = 2, main = "valores posibles con mu = 15")
plot(dpois(x, lambda = 25, log = FALSE), type = "h", xlab = "x",
     ylab = "curva de densidad de probabilidad", lwd = 2, main = "valores posibles con mu = 25")
#Modelo de Poisson
library(poisson)
Aedpoisson <- glm(Trampa6$NUMBER ~ 1, data = Trampa6, family = poisson)
Aedpoisson

AedpoissonC <- glm(Trampa6$CORREGIDO ~ 1, data = Trampa6, family = poisson)
AedpoissonC

Aedpoisson.nulo <- glm(Trampa6$NUMBER ~ 1, data = Trampa6, family = poisson)
anova(Aedpoisson, Aedpoisson.nulo, test = "Chisq")

#COMPARACIÓN DEL MODELO ORIGINAL CON EL NULO, QUE PERMITIRÁ COMPROBAR LA SIGNIFICACIÓN DEL MODELO EN SU CONJUNTO
Aedpoissonc.nulo <- glm(Trampa6$CORREGIDO ~ 1, data = Trampa6, family = poisson)
anova(AedpoissonC, Aedpoissonc.nulo, test = "Chisq")

anova(Aedpoisson, test = "Chisq")
anova(AedpoissonC, test = "Chisq")

#valores ajustados -> función exponencial de Trampa6$CORREGIDO
par(mar = c(5, 5, 2, 2))
Trampa6 <- data.frame(Trampa6$CORREGIDO = seq(0.804, 4.865, length = 25))
P1 <- predict(AedpoissonC, newdata = Trampa6, type = "response")
plot(x = Trampa6$`END DATA`, y = Trampa6$CORREGIDO, ylim = c(0, 300),
     xlab = "Date (months)", ylab = "Total abundance Aedes albopictus",
     cex.lab = 1)
lines(Trampa6$CORREGIDO, P1, lwd = 3)

#residuos y valores ajustados
E1 <- resid(AedpoissonC, type = "pearson")
F1 <- fitted(AedpoissonC)
eta <- predict(AedpoissonC, type = "link")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
# el tipo de gráfico más común por defecto en R (con 'eta')
plot(x = eta, y = E1, xlab = "Eta", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
plot(x = Trampa6$`END DATA`, y = E1, xlab = "Date (months)", ylab = "Pearson residuals",
     cex.lab = 1.5, pch = 16)
abline(h = 0, v = 0, lty = 2)
boxplot(E1 ~ Trampa6$NUMBER, ylab = "Pearson residuals", data = Trampa6,
        cex.lab = 1.5, xlab = "Period")
abline(h = 0, v = 0, lty = 2)

