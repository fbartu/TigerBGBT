library (lattice)
library (MASS)
#gr�fico simple, tanto en n�mero crudo como n�mero corregido por d�as/trampa
plot (x=Trampa5$`END DATA`, y= Trampa5$NUMBER)
plot (x=Trampa5$`END DATA`, y= Trampa5$CORREGIDO)
#regresi�n lineal
Aed0 <- lm(Trampa5$`END DATA`~ Trampa5$NUMBER, data = Trampa5)
Aed0
#regresi�n lineal versi�n corregida
Aed0corregido <- lm(Trampa5$`END DATA`~ Trampa5$CORREGIDO, data = Trampa5)
Aed0corregido
#c�lculo de residuo y valores ajustados
E0 <- resid(Aed0)
F0 <- fitted(Aed0)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))
plot(x = F0, y = E0, xlab = "Fitted values", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
plot(x = Trampa5$`END DATA`, y = Trampa5$NUMBER, xlab = "Date (months)",
     ylab = "Abundance of Aedes albopictus in Trap 5", cex.lab = 1.5, pch = 16)
abline(Aed0, lwd = 5)
#c�lculo de residuo y valores ajustados (corregido)
E0c <- resid(Aed0corregido)
F0c <- fitted(Aed0corregido)
par(mfrow = c(1, 2), mar = c(5, 5, 3, 2))
plot(x = F0c, y = E0c, xlab = "Fitted values", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
plot(x = Trampa5$`END DATA`, y = Trampa5$CORREGIDO, xlab = "Date (months)",
     ylab = "Abundance of Aedes albopictus in Trap 5", cex.lab = 1.5, pch = 16)
abline(Aed0corregido, lwd = 5)
#otras herramientas de evaluaci�n
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))
plot(x = Trampa5$`END DATA`, y = E0, xlab = "Date (months)", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
boxplot(E0 ~ Trampa5$NUMBER, data = Trampa5, cex.lab = 1.5, xlab = "Trampa5",
        ylab = "Residuals")
#versi�n corregida
par(mfrow = c(2, 2), mar = c(5, 5, 3, 2))
plot(x = Trampa5$`END DATA`, y = E0c, xlab = "Date (months)", ylab = "Residuals",
     cex.lab = 1.5)
abline(h = 0, lty = 2)
boxplot(E0c ~ Trampa5$CORREGIDO, data = Trampa5, cex.lab = 1.5, xlab = "Trampa5",
        ylab = "Residuals")
hist(E0c, main = "", breaks = 20, cex.lab = 1.5, xlab = "Residuals")
#abundancias esperadas bajo una regresi�n con valores normales
par(mfrow = c(1, 1), mar = c(5, 5, 3, 2))
plot(x = Trampa5$`END DATA`, y = Trampa5$NUMBER, xlab = "date (months)",
     ylab = "Total abundance of Aedes albopictus", cex.lab = 1.5, pch = 1, ylim = c(0,
                                                                300))
abline(Aed0, lwd = 5)
abline(h = 0, lty = 2)
range(Trampa5$`END DATA`)
#Lo mismo pero con la versi�n corregida
par(mfrow = c(1, 1), mar = c(5, 5, 3, 2))
plot(x = Trampa5$`END DATA`, y = Trampa5$CORREGIDO, xlab = "date (months)",
     ylab = "Total abundance of Aedes albopictus", cex.lab = 1.5, pch = 1, ylim = c(0,
                                                                                    300))
abline(Aed0corregido, lwd = 5)
abline(h = 0, lty = 2)
range(Trampa5$`END DATA`)

#En la regresi�n de Poisson asumimos que la VR se distribuye seg�n una funci�n de Poisson con un par�metro "mu" ?? (la "media") a determinar. La media es igual a la varianza.
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
Aedpoisson <- glm(Trampa5$NUMBER ~ 1, data = Trampa5, family = poisson)
Aedpoisson
#Call:  glm(formula = Trampa5$NUMBER ~ 1, family = poisson, data = Trampa5)

#Coefficients:
 # (Intercept)  
#4.435  

#Degrees of Freedom: 12 Total (i.e. Null);  12 Residual
#Null Deviance:	    363.3 
#Residual Deviance: 363.3 	AIC: 443.9
AedpoissonC <- glm(Trampa5$CORREGIDO ~ 1, data = Trampa5, family = poisson)
AedpoissonC
#Call:  glm(formula = Trampa5$CORREGIDO ~ 1, family = poisson, data = Trampa5)

#Coefficients:
 # (Intercept)  
#3.883  

#Degrees of Freedom: 12 Total (i.e. Null);  12 Residual
#Null Deviance:	    585.6 
#Residual Deviance: 585.6 	AIC: Inf

#COMPARACI�N DEL MODELO ORIGINAL CON EL NULO, QUE PERMITIR� COMPROBAR LA SIGNIFICACI�N DEL MODELO EN SU CONJUNTO
Aedpoisson.nulo <- glm(Trampa5$NUMBER ~ 1, data = Trampa5, family = poisson)
anova(Aedpoisson, Aedpoisson.nulo, test = "Chisq")

#COMPARACI�N DEL MODELO ORIGINAL CON EL NULO, QUE PERMITIR� COMPROBAR LA SIGNIFICACI�N DEL MODELO EN SU CONJUNTO
Aedpoissonc.nulo <- glm(Trampa5$CORREGIDO ~ 1, data = Trampa5, family = poisson)
anova(AedpoissonC, Aedpoissonc.nulo, test = "Chisq")

anova(Aedpoisson, test = "Chisq")
anova(AedpoissonC, test = "Chisq")

#valores ajustados -> funci�n exponencial de Trampa5$CORREGIDO
par(mar = c(5, 5, 2, 2))
Trampa5 <- data.frame(Trampa5$CORREGIDO = seq(0.804, 4.865, length = 25))
P1 <- predict(AedpoissonC, newdata = Trampa5, type = "response")
plot(x = Trampa5$`END DATA`, y = Trampa5$CORREGIDO, ylim = c(0, 300),
     xlab = "Date (months)", ylab = "Total abundance Aedes albopictus",
     cex.lab = 1)
lines(Trampa5$CORREGIDO, P1, lwd = 3)

#residuos y valores ajustados
E1 <- resid(AedpoissonC, type = "pearson")
F1 <- fitted(AedpoissonC)
eta <- predict(AedpoissonC, type = "link")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
# el tipo de gr�fico m�s com�n por defecto en R (con 'eta')
plot(x = eta, y = E1, xlab = "Eta", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
plot(x = Trampa5$`END DATA`, y = E1, xlab = "Date (months)", ylab = "Pearson residuals",
     cex.lab = 1.5, pch = 16)
abline(h = 0, v = 0, lty = 2)
boxplot(E1 ~ Trampa5$NUMBER, ylab = "Pearson residuals", data = Trampa5,
        cex.lab = 1.5, xlab = "Period")
abline(h = 0, v = 0, lty = 2)

