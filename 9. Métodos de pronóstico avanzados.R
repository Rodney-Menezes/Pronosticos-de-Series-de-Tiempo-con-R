#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 11 - M�todos de pron�stico avanzados  ####
#     ========================================     #


#Paquetes que se utilizar�
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("seasonal")
install.packages("hts")
install.packages("tidyverse")
install.packages("tibble")
install.packages("vars")


library(forecast)
library(ggplot2)
library(fpp2)
library(seasonal)
library(hts)
library(tidyverse)
library(tibble)
library(vars)


set.seed(30)  #ponemos semilla


## 11.1: Estacionalidad compleja ##
# ******************************* #

#Para tratar con tales series, usaremos la clase msts que maneja m�ltiples series temporales 
#de estacionalidad. Esto le permite especificar todas las frecuencias que podr�an ser 
#relevantes. Tambi�n es lo suficientemente flexible como para manejar frecuencias no enteras.

#graficamos la base de datos divida (una todos los periodos y otra solo cuatro)
p1 <- autoplot(calls) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(breaks=seq(1,33,by=2))
p2 <- autoplot(window(calls, end=4)) +
  ylab("Call volume") + xlab("Weeks") +
  scale_x_continuous(minor_breaks = seq(1,4,by=0.2))
gridExtra::grid.arrange(p1,p2)

#11.1.1) STL con m�ltiples per�odos estacionales

#La funci�n mstl() es una variaci�n stl() dise�ada para lidiar con la estacionalidad m�ltiple. 
#Devolver� m�ltiples componentes estacionales, as� como un componente de tendencia y resto.

calls %>% mstl() %>%
  autoplot() + xlab("Week")

#Se muestran dos patrones estacionales, uno para la hora del d�a (el tercer panel) y otro para
#la hora de la semana (el cuarto panel). Para interpretar correctamente este gr�fico, es 
#importante notar las escalas verticales.

#La descomposici�n tambi�n se puede utilizar en el pron�stico, con cada uno de los componentes
#estacionales pronosticados utilizando un m�todo ingenuo estacional, y los pron�sticos de 
#datos ajustados estacionalmente utilizando ETS (o alg�n otro m�todo especificado por el 
#usuario). La  funci�n stlf() har� esto autom�ticamente.

calls %>%  stlf() %>%
  autoplot() + xlab("Week")

#11.1.2) Regresi�n arm�nica din�mica con m�ltiples per�odos estacionales:

#Con estacionalidades m�ltiples, podemos usar t�rminos de Fourier. Debido a que hay m�ltiples
#estacionalidades, necesitamos agregar t�rminos de Fourier para cada per�odo estacional.

#Ajustaremos un modelo de regresi�n arm�nica din�mica con una estructura de error ARMA.
#Se ha elegido el n�mero total de t�rminos de Fourier para cada per�odo estacional para 
#minimizar el AICc. Utilizaremos una transformaci�n logar�tmica ( lambda=0) para garantizar 
#que los pron�sticos y los intervalos de predicci�n sigan siendo positivos.

fit <- auto.arima(calls, seasonal=FALSE, lambda=0,
                  xreg=fourier(calls, K=c(10,10)))
fit %>%
  forecast(xreg=fourier(calls, K=c(10,10), h=2*169)) %>%
  autoplot(include=5*169) +
  ylab("Call volume") + xlab("Weeks")

#Este es un modelo grande, que contiene 40 par�metros: 4 coeficientes ARMA, 
#20 coeficientes de Fourier para la frecuencia 169 y 16 coeficientes de Fourier para la 
#frecuencia 845. No utilizamos todos los t�rminos de Fourier para la frecuencia 845 porque 
#hay cierta superposici�n con los t�rminos de frecuencia 169

#11.1.3) Modelos TBATS:

#utiliza una combinaci�n de t�rminos de Fourier con un modelo de espacio de estado de 
#suavizado exponencial y una transformaci�n Box-Cox, de una manera completamente automatizada.

calls %>%
  subset(start=length(calls)-2000) %>%
  tbats() -> fit2
fc2 <- forecast(fit2, h=2*169)
autoplot(fc2, include=5*169) +
  ylab("Call volume") + xlab("Weeks")

#Aqu� los intervalos de predicci�n parecen ser demasiado amplios, algo que desafortunadamente
#sucede con frecuencia con los modelos TBATS.

#11.1.4) Estacionalidad compleja con covariables:

#Los modelos TBATS no permiten covariables, aunque pueden incluirse en modelos de
#regresi�n arm�nica din�mica.

autoplot(elecdemand[,c("Demand","Temperature")],
         facet=TRUE) +
  scale_x_continuous(minor_breaks=NULL,
                     breaks=2014+
                       cumsum(c(0,31,28,31,30,31,30,31,31,30,31,30))/365,
                     labels=month.abb) +
  xlab("Time") + ylab("")

#El trazado de la demanda de electricidad frente a la temperatura
elecdemand %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) + geom_point() +
  xlab("Temperature (degrees Celsius)") +
  ylab("Demand (GW)")

#Ajustaremos un modelo de regresi�n con una funci�n lineal de temperatura por partes 
#(que contiene un nudo a 18 grados) y t�rminos de regresi�n arm�nica para permitir el patr�n 
#estacional diario.

#Pronosticar con tales modelos es dif�cil porque requerimos valores futuros de las variables
#predictoras. 

temps <- subset(elecdemand[,"Temperature"],
                start=NROW(elecdemand)-2*48+1)
fc <- forecast(fit,
               xreg=cbind(fourier(temps, c(10,10,0)),
                          heating=temps, cooling=pmax(temps,18)))
autoplot(fc, include=14*48)

checkresiduals(fc)


## 11.2: Vector autorregresiVOS ##
# ****************************** #

#Si las series son estacionarias, las pronosticamos ajustando un VAR a los datos directamente 
#(conocido como "VAR en niveles"). Si las series no son estacionarias, tomamos las diferencias
#de los datos para hacerlas estacionarias, luego ajustamos un modelo VAR (conocido como "VAR 
#en diferencias"). En ambos casos, los modelos se estiman ecuaci�n por ecuaci�n utilizando 
#el principio de m�nimos cuadrados. 

#Ejemplo: un modelo VAR para pronosticar el consumo de EE.UU.
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Seleccionamos los rezagos optimos del VAR
VARselect(uschange[,1:2], lag.max=8,
          type="const")[["selection"]]

#probamos que los residuos no esten parcialmente correlacionados
var1 <- VAR(uschange[,1:2], p=1, type="const")
serial.test(var1, lags.pt=10, type="PT.asymptotic")
var2 <- VAR(uschange[,1:2], p=2, type="const")
serial.test(var2, lags.pt=10, type="PT.asymptotic")

#Tanto un VAR(1) como un VAR(2) tienen alguna correlaci�n serial residual y, por lo tanto, 
#ajustamos un VAR(3).
var3 <- VAR(uschange[,1:2], p=3, type="const")
serial.test(var3, lags.pt=10, type="PT.asymptotic")

#pronosticos del VAR(3)
forecast(var3) %>%
  autoplot() + xlab("Year")


## 11.3: Modelos de redes neuronales ##
# *********************************** #

#Las redes neuronales artificiales son m�todos de pron�stico basados En modelos matem�ticos 
#simples del cerebro. Permiten complejas relaciones no lineales entre la variable de 
#respuesta y sus predictores.

#Ejemplo: manchas solares:
#,,,,,,,,,,,,,,,,,,,,,,,,,

fit <- nnetar(sunspotarea, lambda=0)
autoplot(forecast(fit,h=30))

#Aqu�, las �ltimas 10 observaciones se usan como predictores, 
#y hay 6 neuronas en la capa oculta.

#11.3.1) Intervalos de predicci�n:

#A diferencia de la mayor�a de los m�todos considerados en este libro, las redes neuronales 
#no se basan en un modelo estoc�stico bien definido y, por lo tanto, no es sencillo derivar 
#intervalos de predicci�n para los pron�sticos resultantes. Sin embargo, a�n podemos calcular 
#los intervalos de predicci�n utilizando la simulaci�n en la que se generan rutas de muestreo 
#futuras utilizando residuos de arranque.

#Aqu� hay una simulaci�n de 9 posibles rutas de muestra futuras para los datos de manchas 
#solares. Cada ruta de muestra cubre los pr�ximos 30 a�os despu�s de los datos observados.

sim <- ts(matrix(0, nrow=30L, ncol=9L),
          start=end(sunspotarea)[1L]+1L)
for(i in seq(9))
  sim[,i] <- simulate(fit, nsim=30L)
autoplot(sunspotarea) + autolayer(sim)

#Si hacemos esto unos cientos o miles de veces, podemos obtener una buena imagen de las 
#distribuciones de pron�stico. As� es como la forecast()funci�n produce intervalos de 
#predicci�n para los modelos NNAR:

fcast <- forecast(fit, PI=TRUE, h=30)
autoplot(fcast)

## 11.4: Bootstrapping and bagging ##
# ********************************* #

#11.4.1) Series temporales de bootstrapping:

#En la secci�n anterior, iniciamos los residuos de una serie de tiempo para simular valores 
#futuros de una serie usando un modelo.

#En t�rminos m�s generales, podemos generar nuevas series de tiempo que son similares a 
#nuestras series observadas, utilizando otro tipo de bootstrap.

#Datos gasto mensual de tarjetas cred islandia (Ene2000-Ago2013)
bootseries <- bld.mbb.bootstrap(debitcards, 10) %>%
  as.data.frame() %>% ts(start=2000, frequency=12)
autoplot(debitcards) +
  autolayer(bootseries, colour=TRUE) +
  autolayer(debitcards, colour=FALSE) +
  ylab("Bootstrapped series") + guides(colour="none")

#Este tipo de bootstrapping puede ser �til de dos maneras. Primero, nos ayuda a obtener 
#una mejor medida de la incertidumbre del pron�stico, y segundo, proporciona una forma de 
#mejorar nuestros pron�sticos puntuales utilizando el "bagging".

#11.4.2) Intervalos de predicci�n de series bootstrapped:

#Casi todos los intervalos de predicci�n de los modelos de series temporales son demasiado 
#estrechos. Este es un fen�meno bien conocido y surge porque no tienen en cuenta todas las 
#fuentes de incertidumbre. 

#Cuando producimos intervalos de predicci�n para modelos de series de tiempo, 
#generalmente solo tomamos en cuenta la primera de estas fuentes de incertidumbre.

#Podemos usar series temporales de bootstrapped para superar este problema. 
nsim <- 1000L
sim <- bld.mbb.bootstrap(debitcards, nsim)

#Para cada una de estas series, ajustamos un modelo ETS y simulamos una ruta de 
#muestra a partir de ese modelo.

#Este es un proceso que lleva mucho tiempo ya que hay una gran cantidad de series de 
#tiempo para modelar.
h <- 36L
future <- matrix(0, nrow=nsim, ncol=h)
for(i in seq(nsim))
  future[i,] <- simulate(ets(sim[[i]]), nsim=h)

#Finalmente, tomamos las medias y cuantiles de estas rutas de muestra simuladas para formar 
#pron�sticos puntuales e intervalos de predicci�n.
start <- tsp(debitcards)[2]+1/12
simfc <- structure(list(
  mean = ts(colMeans(future), start=start, frequency=12),
  lower = ts(apply(future, 2, quantile, prob=0.025),
             start=start, frequency=12),
  upper = ts(apply(future, 2, quantile, prob=0.975),
             start=start, frequency=12),
  level=95),
  class="forecast")

#Estos intervalos de predicci�n ser�n mayores que los obtenidos de un modelo ETS aplicado 
#directamente a los datos originales.
etsfc <- forecast(ets(debitcards), h=h, level=95)
autoplot(debitcards) +
  ggtitle("Monthly retail debit card usage in Iceland") +
  xlab("Year") + ylab("million ISK") +
  autolayer(simfc, series="Simulated") +
  autolayer(etsfc, series="ETS")

#11.4.3) Pron�sticos de ETS en bolsas

#Otro uso para estas series temporales de arranque es mejorar la precisi�n del pron�stico.
#Si producimos pron�sticos de cada una de las series de tiempo adicionales y promediamos 
#los pron�sticos resultantes, obtendremos mejores pron�sticos que si simplemente 
#pronosticamos las series de tiempo originales directamente. Esto se llama "bagging", 
#que significa ""Bootstrap AGGregatING"".

#Usaremos ets()para pronosticar cada una de estas series.
sim <- bld.mbb.bootstrap(debitcards, 10) %>%
  as.data.frame() %>%
  ts(frequency=12, start=2000)
fc <- purrr::map(as.list(sim),
                 function(x){forecast(ets(x))[["mean"]]}) %>%
  as.data.frame() %>%
  ts(frequency=12, start=start)
autoplot(debitcards) +
  autolayer(sim, colour=TRUE) +
  autolayer(fc, colour=TRUE) +
  autolayer(debitcards, colour=FALSE) +
  ylab("Bootstrapped series") +
  guides(colour="none")

#El promedio de estos pron�sticos da los pron�sticos en bolsas de los datos originales. 
#Todo el procedimiento se puede manejar con la baggedETS()funci�n. De manera predeterminada, 
#se utilizan 100 series de bootstrapped, y la longitud de los bloques utilizados para obtener
#los residuos de bootstrapped se establece en 24 para los datos mensuales.

etsfc <- debitcards %>% ets() %>% forecast(h=36)
baggedfc <- debitcards %>% baggedETS() %>% forecast(h=36)
autoplot(debitcards) +
  autolayer(baggedfc, series="BaggedETS", PI=FALSE) +
  autolayer(etsfc, series="ETS", PI=FALSE) +
  guides(colour=guide_legend(title="Forecasts"))

