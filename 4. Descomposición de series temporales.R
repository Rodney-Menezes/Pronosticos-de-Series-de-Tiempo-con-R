#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 6 - Descomposición de series temporales  ####
#     ===========================================     #


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


## 6.2:  Promedios móviles (MA) ##
# ****************************** #

# 6.2.1) Alisado promedio móvil:

#Nota: Cada valor en la columna 5-MA es el promedio de las observaciones en la ventana de 
#      cinco años centrada en el año correspondiente. 
#      un MA es una estimacion de la tendencia de un ciclo

#graficamos la data
autoplot(elecsales) + xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia")

#calculando la MA
ma(elecsales, 5)

#graficamos la tendencia del ciclo (MA) con los datos reales
autoplot(elecsales, series="Data") +
  autolayer(ma(elecsales,5), series="5-MA") +
  xlab("Year") + ylab("GWh") +
  ggtitle("Annual electricity sales: South Australia") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

# Nota:  El orden del promedio móvil determina la suavidad de la estimación del ciclo 
#        de tendencia. En general, un orden mayor significa una curva más suave. 
#        Los promedios móviles simples como estos generalmente son de orden impar. 
#        Esto es para que sean simétricos.


# 6.2.2) Medias móviles de medias móviles:

# Una razón para hacerlo es hacer una media móvil de orden par simétrica.
# Por ejemplo, podríamos tomar un promedio móvil de orden 4 y luego aplicar otro promedio 
# móvil de orden 2 a los resultados, este seria un MA(2x4)
beer2 <- window(ausbeer,start=1992)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)


# 6.2.3) Estimando la tendencia del ciclo de con datos estacionales:

# El uso más común de promedios móviles centrados es para estimar el ciclo de 
# tendencia a partir de datos estacionales.

# Ejemplo: fabricación de equipos eléctricos:
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))


# 6.2.4) Promedios móviles ponderados:

# Una ventaja importante de los promedios móviles ponderados es que producen una 
# estimación más uniforme del ciclo de tendencia. 


## 6.3: Descomposición clásica ##
# ***************************** #

# Nota: Si bien la descomposición clásica todavía se usa ampliamente, 
# no se recomienda, ya que ahora hay varios métodos mucho mejores.

# 6.3.1) Descomposición aditiva:

# 6.3.2) Descomposición multiplicativa:

elecequip %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of electrical equipment index")


## 6.4: descomposición X11 ##
# ************************* #

#Este método se basa en la descomposición clásica, pero incluye muchos pasos, características
# y metodos sofisticados adicionales para superar las limitaciones de la descomposición clásica.

# El método X11 está disponible utilizando la función seas() del paquete estacional para R.

elecequip %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")

# Dado el resultado de la función seas(), seasonal() extraerá el componente estacional, 
# trendcycle() extraerá el componente tendencial del ciclo, remainder()extraerá el componente 
# restante y seasadj() calculará la serie temporal ajustada estacionalmente.

autoplot(elecequip, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#Puede ser útil utilizar gráficos estacionales y gráficos de sub-series estacionales 
#del componente estacional. Estos nos ayudan a visualizar la variación en el componente 
#estacional a lo largo del tiempo.

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

## 6.5: Descomposición de SEATS ##
# ****************************** #

#"SEATS" significa "Extracción estacional en la serie temporal de "ARIMA". 
#Este procedimiento fue desarrollado en el Banco de España. Se usa a travez del paquete seasonal

elecequip %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of electrical equipment index")

#Como con el método X11, podemos utilizar la funcioness seasonal(), trendcycle() y remainder()
#para extraer los componentes individuales, y seasadj() para calcular las series de tiempo 
#ajustados estacionalmente.


## 6.6: Descomposición STL ##
# ************************* #

#STL es un acrónimo de "Descomposición estacional y de tendencia usando Loess", 
#"Loess" es un método para estimar relaciones no lineales.

# STL tiene varias ventajas sobre los métodos de descomposición clásicos, SEATS y X11:
# a) A diferencia de SEATS y X11, STL manejará cualquier tipo de estacionalidad, 
#    no solo datos mensuales y trimestrales.
# b) El componente estacional puede cambiar con el tiempo, 
#    y el usuario puede controlar la tasa de cambio. 
# c) La suavidad del ciclo de tendencia también puede ser controlada por el usuario.
# d) Puede ser robusto para los valores atípicos. 

elecequip %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

#Notas adicionlaes sobre STL:
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Los dos parámetros principales que se elegirán al usar STL son la ventana tendencial
# del ciclo ( t.window) y la ventana estacional ( s.window). Estos controlan qué tan rápido 
#pueden cambiar el ciclo de tendencia y los componentes estacionales. 
#Los valores más pequeños permiten cambios más rápidos. Ambos deben ser números impares; 
#t.window es el número de observaciones consecutivas que se utilizarán al estimar la tendecia
#del ciclo; s.windowes el número de años consecutivos que se utilizarán para estimar cada valor
#en el componente estacional. 

#Al igual que con los otros métodos de descomposición discutidos en este libro, para obtener
#una grafica de los componentes separados , use la función seasonal() para el componente 
#estacional, la función trendcycle() para el componente de ciclo de tendencia y 
#la función remainder() para el componente restante. La función seasadj() se puede usar para 
#calcular la serie ajustada estacionalmente.


## 6.7 Medición de la fuerza de la tendencia y la estacionalidad ##
# *************************************************************** #

# Se puede utilizar una descomposición de series temporales para medir la fuerza de 
# la tendencia y la estacionalidad en una serie temporal (Wang, Smith y Hyndman, 2006 )


## 6.8 Predicción con descomposición ##
# *********************************** #

#Para pronosticar una serie temporal descompuesta, pronosticamos el componente estacional, 
#y el componente estacionalmente ajustado, por separado. Para pronosticar el componente 
#desestacionalizado, se puede utilizar cualquier método de pronóstico no estacional.

# Ejemplos de equipos electricos:
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#muestra pronósticos ingenuos ajustados estacionalmente
fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

#Debe especificar el método que se utiliza en los datos ajustados estacionalmente, 
#y la función stl hará la reestacionalización por usted.

#pronóstico ingenuo de los datos ajustados estacionalmente y un pronóstico ingenuo estacional
#del componente estacional, después de una descomposición STL de los datos.
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

#Un enfoque para reestacionalizarusar la stlf()función. El siguiente código descompondrá 
#las series de tiempo usando STL, pronosticará las series ajustadas estacionalmente y 
#devolverá los pronósticos reestacionalizados. 

fcast <- stlf(elecequip, method='naive')













































































