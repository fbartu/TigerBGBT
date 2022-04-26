library(Matrix)
difftime(Trampa4$`END DATA`, Trampa4$`START DATA`, units = "days") #calcular la "diferencia" de días entre una fecha y otra.
amountdays <- difftime(Trampa4$`END DATA`, Trampa4$`START DATA`, units = "days")
library (lattice) #GRÁFICOS CONDICIONADOS
library (MASS) # REGRESIÓN DE TIPO BINOMIAL NEGATIVO
plot(x = Trampa4$`END DATA`, y = Trampa4$NUMBER)
Aed0 <- lm(amountdays ~ Trampa4$NUMBER, data = Trampa4) #REGRESIÓN LINEAL, NÚMERO DE AEDES ALBOPICTUS Y TOTAL DIAS EN BOLSA
Aed0
#RESIDUOS Y VALORES AJUSTADOS:
E0 <- resid (Aed0)
F0 <- resid (Aed0)
par (mfrow = c(1,2), mar = c(5,5,3,2))
plot(x= F0, y=E0, xlab ="Valor ajustado", ylab="Residuo", cex.lab = 1.5)
abline (h=0, lty=2)
plot(x=Trampa4$`END DATA`, y= Trampa4$NUMBER, xlab = "Date (months)", ylab ="Aedes albopictus", cex.lab=1.5, pch= 16)
abline (Aed0, lwd=5)

par (mfrow = c(2,2), mar = c(5,5,3,2))
plot(x=Trampa4$`END DATA`, y=E0, xlab = "Date (months)", ylab = "Residuo", cex.lab = 1.5)
abline(h=0, lty =2)
#ABUNDANCIAS ESPERADAS BAJO UNA REGRESION CON ERRORES NORMALES
par(mfrow = c(1,1), mar = c(5,5,3,2))
plot(x=Trampa4$`END DATA`, y=Trampa4$NUMBER, xlab = "Date (months)", ylab = "Abundance of Aedes albopictus", cex.lab = 1.5, pch = 1, ylim = c(-300,1200))
abline(Aed0, lwd=5)
abline(h=0, lty=2)

range(Trampa4$`END DATA`)
#Generar secuencia de valores que comprenda el rango observado 
ed <- seq("2020-07-27 UTC", "2020-11-02 UTC", length = 10)
Beta <- coef(Aed0)
for (i in 1:10) {
  mu <- Beta[1] + Beta[2] * ed[i]
  yi <- rnorm(100, mean = mu, sd = summary(Aed0)$sigma)
  points(jitter(rep(ed[i], 100)), jitter(yi), col = grey(0.5),
         pch = 16, cex = 1)
}
#No tenía sentido, se esperaban abundancias NEGATIVAS. HACER REGRESIÓN POISSON PARA CORREGIR PROBLEMA DE VALORES NEGATIVOS Y HETEROCEDASTICIDAD.

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

#REGRESIÓN POISSON
#Predictor lineal, especificando las variables explicativas a incluir en el modelo
log(Trampa4$NUMBER, base = exp (1))

Aed1 <- glm(Trampa4$NUMBER ~ 1, data = Trampa4, family = poisson(link = "log"))
Aed1

#( Call:  glm(formula = Trampa4$NUMBER ~ 1, family = poisson(link = "log"), 
          # data = Trampa4)

#Coefficients:
 # (Intercept)  
#6.072  

#Degrees of Freedom: 12 Total (i.e. Null);  12 Residual
#Null Deviance:	    3094 
#Residual Deviance: 3094 	AIC: 3194

#null deviance
Aed1$null.deviance
#[1] 3094.349
1 - (deviance(Aed1)/Aed1$null.deviance)
range(Trampa4$NUMBER)
#[1]   17 1001
#Comparacion del modelo original con el nulo, lo que equivale a comprobar la significación del modelo en su conjunto
Aed1.nulo <- glm(Trampa4$NUMBER ~ 1, data = Trampa4, family = poisson)
anova(Aed1, Aed1.nulo, test = "Chisq")
anova(Aed1, test = "Chisq")
par(mar = c(5, 5, 2, 2))
Trap4 <- data.frame(Trampa4$NUMBER = seq(0.804, 4.865, length = 25))
P1 <- predict(Aed1, newdata = Trap4, type = "response")
plot(x = Trampa4$`END DATA`, y = Trampa4$NUMBER, ylim = c(0, 1300),
     xlab = "Date(months)", ylab = "Abundance of mosquitoes",
     cex.lab = 1.5)
#gráficos diagnóstico
E1 <- resid(Aed1, type = "pearson")
F1 <- fitted(Aed1)
eta <- predict(Aed1, type = "link")
par(mfrow = c(2, 2), mar = c(5, 5, 2, 2))
plot(x = F1, y = E1, xlab = "Fitted values", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
# el tipo de gráfico más común por defecto en R (con 'eta')
plot(x = eta, y = E1, xlab = "Eta", ylab = "Pearson residuals",
     cex.lab = 1.5)
abline(h = 0, v = 0, lty = 2)
plot(x = Trampa4$NUMBER, y = E1, xlab = "Abundance of mosquitoes", ylab = "Pearson residuals",
     cex.lab = 1.5, pch = 16)
abline(h = 0, v = 0, lty = 2)
boxplot(E1 ~ Trampa4$`END DATA`, ylab = "Pearson residuals", data = Trampa4,
        cex.lab = 1.5, xlab = "End data")
abline(h = 0, v = 0, lty = 2)

