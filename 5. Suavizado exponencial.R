#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 7 - Suavizado exponencial  ####
#     =============================     #


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



## 7.1: Suavizado exponencial simple ##
# *********************************** #

#Este método es adecuado para pronosticar datos sin tendencia clara o patrón estacional.

#cargamos y graficamos datos
oildata <- window(oil, start=1996)
autoplot(oildata) +
  ylab("Oil (millions of tonnes)") + xlab("Year")

# Ejemplo: producción de petróleo
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Cargamos los datos
oildata <- window(oil, start=1996)

# Estimate parameters
fc <- ses(oildata, h=5)

# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)

autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Oil (millions of tonnes)") + xlab("Year")


## 7.2: Métodos de tendencia ##
# *************************** #

# 7.2.1) Método de tendencia lineal de Holt:

# Ejemplo: pasajeros aéreos
# ,,,,,,,,,,,,,,,,,,,,,,,,,

air <- window(ausair, start=1990)
fc <- holt(air, h=5)

#El valor muy pequeño de ?? significa que la pendiente apenas cambia con el tiempo. 
#En este caso ??=0.0001

# 7.2.2) Métodos de tendencia amortiguadas:

#Ejemplo: Pasajeros aéreos (continuación)
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

fc <- holt(air, h=15)
fc2 <- holt(air, damped=TRUE, phi = 0.9, h=15)
autoplot(air) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

#Ejemplo: ovejas en Asia
#,,,,,,,,,,,,,,,,,,,,,,,

autoplot(livestock) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")

#Utilizamos la validación cruzada de series temporales para comparar la precisión del 
#pronóstico en un solo paso de los tres métodos.

e1 <- tsCV(livestock, ses, h=1)
e2 <- tsCV(livestock, holt, h=1)
e3 <- tsCV(livestock, holt, damped=TRUE, h=1)

# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)

# Compare MAE:
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)

#El método amortiguado de Holt es mejor si compara los valores de MAE o MSE. 
#Por lo tanto, procederemos a utilizar el método de Holt amortiguado y lo aplicaremos a todo 
#el conjunto de datos para obtener pronósticos para años futuros.
fc <- holt(livestock, damped=TRUE)

#Se estima que el parámetro de suavizado para la pendiente es esencialmente cero, 
#lo que indica que la tendencia no cambia con el tiempo. El valor de ?? está muy cerca de uno,
#lo que muestra que el nivel reacciona fuertemente a cada nueva observación. 

autoplot(fc) +
  xlab("Year") + ylab("Livestock, sheep in Asia (millions)")



## 7.3: Métodos de tendencia ##
# *************************** #

# 7.3.1) Método aditivo de Holt-Winters:

# 7.3.2) Método multiplicativo de Holt-Winters:

#Ejemplo: noches de turistas internacionales en Australia
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")
autoplot(aust) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Visitor nights (millions)") +
  ggtitle("International visitors nights in Australia") +
  guides(colour=guide_legend(title="Forecast"))

# 7.3.3) Método amortiguado de Holt-Winters:

#La amortiguación es posible con los métodos de Holt-Winters aditivos y multiplicativos. 
#Un método que a menudo proporciona pronósticos precisos y sólidos para datos estacionales 
#es el método Holt-Winters con una tendencia amortiguada y una estacionalidad multiplicativa:

hw(y, damped=TRUE, seasonal="multiplicative")

#Ejemplo: método Holt-Winters con datos diarios
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

fc <- hw(subset(hyndsight,end=length(hyndsight)-35),
         damped = TRUE, seasonal="multiplicative", h=35)
autoplot(hyndsight) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))


## 7.6: Estimación y selección de modelo ##
# *************************************** #

#7.6.1 La ets()función en R:

#Nota:
#   ets(y, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL,
#       gamma=NULL, phi=NULL, lambda=NULL, biasadj=FALSE,
#       additive.only=FALSE, restrict=TRUE,
#       allow.multiplicative.trend=FALSE)

#Ejemplo: noches de turistas internacionales en Australia
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

aust <- window(austourists, start=2005)
fit <- ets(aust)
summary(fit)

#Los intervalos de predicción estrechos indican que la serie es relativamente fácil 
#de pronosticar debido a la fuerte tendencia y estacionalidad. 

autoplot(fit)

#Debido a que este modelo tiene errores multiplicativos, los residuos no son equivalentes 
#a los errores de entrenamiento de un paso.

cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")


## 7.7: Predicción con modelos ETS ##
# ********************************* #

fit %>% forecast(h=8) %>%
  autoplot() +
  ylab("International visitor night in Australia (millions)")

# 7.7.1) Utilizando forecast()

# forecast(object, h=ifelse(object$m>1, 2*object$m, 10),
#          level=c(80,95), fan=FALSE, simulate=FALSE, bootstrap=FALSE,
#          npaths=5000, PI=TRUE, lambda=object$lambda, biasadj=NULL, ...)


