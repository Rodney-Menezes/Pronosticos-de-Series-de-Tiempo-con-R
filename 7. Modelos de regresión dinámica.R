#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 9 - Modelos de regresión dinámica  ####
#     =====================================     #

#Paquetes que se utilizará
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("seasonal")


library(forecast)
library(ggplot2)
library(fpp2)
library(seasonal)


set.seed(30)  #ponemos semilla


## 9.2: Regresión con errores ARIMA en R ##
# *************************************** #

#La función R Arima() se ajustará a un modelo de regresión con errores ARIMA si xreg se usa 
#el argumento . El orderargumento especifica el orden del modelo de error ARIMA. Si se 
#especifica la diferenciación, entonces la diferenciación se aplica a todas las variables en 
#el modelo de regresión antes de estimar el modelo. 

fit <- Arima(y, xreg=x, order=c(1,1,0))

#La función auto.arima() también manejará los términos de regresión a través del argumento 
#xreg El usuario debe especificar las variables predictoras para incluir, pero auto.arima()
#seleccionará el mejor modelo ARIMA para los errores. Si se requiere la diferenciación, 
#todas las variables se diferencian durante el proceso de estimación, aunque el modelo final 
#se expresará en términos de las variables originales.


#Ejemplo: Consumo personal e ingresos de EE.UU.
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(uschange[,1:2], facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Quarterly changes in US consumption
    and personal income")

(fit <- auto.arima(uschange[,"Consumption"],
                   xreg=uschange[,"Income"]))

cbind("Regression Errors" = residuals(fit, type="regression"),
      "ARIMA errors" = residuals(fit, type="innovation")) %>%
  autoplot(facets=TRUE)

checkresiduals(fit)


## 9.3: Regresión con errores ARIMA en R ##
# *************************************** #

#Ejemplo 1: Consumo personal e ingresos de EE.UU.
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

fcast <- forecast(fit, xreg=rep(mean(uschange[,2]),8))
autoplot(fcast) + xlab("Year") +
  ylab("Percentage change")


#Ejemplo 2: pronóstico de la demanda de electricidad
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#En este ejemplo, ajustamos un modelo de regresión cuadrática con errores ARMA usando 
#la función auto.arima().
xreg <- cbind(MaxTemp = elecdaily[, "Temperature"],
              MaxTempSq = elecdaily[, "Temperature"]^2,
              Workday = elecdaily[, "WorkDay"])
fit <- auto.arima(elecdaily[, "Demand"], xreg = xreg)
checkresiduals(fit)

fcast <- forecast(fit,
                  xreg = cbind(MaxTemp=rep(26,14), MaxTempSq=rep(26^2,14),
                               Workday=c(0,1,0,0,1,1,1,1,1,0,0,1,1,1)))
autoplot(fcast) + ylab("Electricity demand (GW)")


## 9.4: Tendencias estocásticas y deterministas ##
# ********************************************** #

#Ejemplo: visitantes internacionales a Australia:
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(austa) + xlab("Year") +
  ylab("millions of people") +
  ggtitle("Total annual international visitors to Australia")

#El modelo de tendencia determinista se obtiene de la siguiente manera:

trend <- seq_along(austa)
(fit1 <- auto.arima(austa, d=0, xreg=trend))

#Alternativamente, se puede estimar el modelo de tendencia estocástica:

(fit2 <- auto.arima(austa, d=1))

#Comparacion grafica de ambas prediciones
fc1 <- forecast(fit1,
                xreg = length(austa) + 1:10)
fc2 <- forecast(fit2, h=10)
autoplot(austa) +
  autolayer(fc2, series="Stochastic trend") +
  autolayer(fc1, series="Deterministic trend") +
  ggtitle("Forecasts from trend models") +
  xlab("Year") + ylab("Visitors to Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

#Existe una suposición implícita con las tendencias deterministas de que la pendiente de la 
#tendencia no va a cambiar con el tiempo. Por otro lado, las tendencias estocásticas pueden 
#cambiar, y se supone que el crecimiento estimado es el crecimiento promedio durante el 
#período histórico, no necesariamente la tasa de crecimiento que se observará en el futuro. 
#En consecuencia, es más seguro pronosticar con tendencias estocásticas, especialmente para 
#horizontes de pronóstico más largos, ya que los intervalos de predicción permiten una mayor 
#incertidumbre en el crecimiento futuro.


## 9.5: Regresión armónica dinámica ##
# ********************************** #

#Ejemplo: gasto de comer fuera de Australia
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#En este ejemplo, demostramos la combinación de términos de Fourier para capturar 
#la estacionalidad con errores de ARIMA que capturan otras dinámicas en los datos.

cafe04 <- window(auscafe, start=2004)
plots <- list()
for (i in seq(6)) {
  fit <- auto.arima(cafe04, xreg = fourier(cafe04, K = i),
                    seasonal = FALSE, lambda = 0)
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg=fourier(cafe04, K=i, h=24))) +
    xlab(paste("K=",i,"   AICC=",round(fit[["aicc"]],2))) +
    ylab("") + ylim(1.5,4.7)
}
gridExtra::grid.arrange(
  plots[[1]],plots[[2]],plots[[3]],
  plots[[4]],plots[[5]],plots[[6]], nrow=3)

#La Figura muestra el patrón estacional proyectado hacia adelante a medida que k aumenta. 
#Observe que a medida que aumenta la captura de términos de Fourier y proyecte un patrón 
#estacional más "ondulado" y se requieren modelos ARIMA más simples para capturar otras 
#dinámicas. El valor de AIC se minimiza para K=5, con un salto significativo desde K=4 a K=5, 
#por lo tanto, los pronósticos generados a partir de este modelo serían los utilizados. 


## 9.6: Predictores rezagados ##
# **************************** #

#Ejemplo: publicidad televisiva y cotizaciones de seguros
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(insurance, facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Insurance advertising and quotations")

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(
  AdLag0 = insurance[,"TV.advert"],
  AdLag1 = stats::lag(insurance[,"TV.advert"],-1),
  AdLag2 = stats::lag(insurance[,"TV.advert"],-2),
  AdLag3 = stats::lag(insurance[,"TV.advert"],-3)) %>%
  head(NROW(insurance))

# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1],
                   stationary=TRUE)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2],
                   stationary=TRUE)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3],
                   stationary=TRUE)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4],
                   stationary=TRUE)
#elegimos la longitud de retraso óptima para la publicidad basada en el AIC
c(fit1[["aicc"]],fit2[["aicc"]],fit3[["aicc"]],fit4[["aicc"]])

#El mejor modelo (con el valor AICc más pequeño) tiene dos predictores rezagados; 
#es decir, incluye publicidad solo en el mes actual y el mes anterior. 
#Entonces ahora volvemos a estimar ese modelo, pero usando todos los datos disponibles.
(fit <- auto.arima(insurance[,1], xreg=Advert[,1:2],
                   stationary=TRUE))

#Pronosticamos el modelo elegido:
fc8 <- forecast(fit, h=20,
                xreg=cbind(AdLag0 = rep(8,20),
                           AdLag1 = c(Advert[40,1], rep(8,19))))
autoplot(fc8) + ylab("Quotes") +
  ggtitle("Forecast quotes with future advertising set to 8")

