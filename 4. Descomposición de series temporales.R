#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 6 - Descomposici�n de series temporales  ####
#     ===========================================     #


#Paquetes que se utilizar�
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("seasonal")


library(forecast)
library(ggplot2)
library(fpp2)
library(seasonal)


set.seed(30)  #ponemos semilla


## 6.2:  Promedios m�viles (MA) ##
# ****************************** #

# 6.2.1) Alisado promedio m�vil:

#Nota: Cada valor en la columna 5-MA es el promedio de las observaciones en la ventana de 
#      cinco a�os centrada en el a�o correspondiente. 
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

# Nota:  El orden del promedio m�vil determina la suavidad de la estimaci�n del ciclo 
#        de tendencia. En general, un orden mayor significa una curva m�s suave. 
#        Los promedios m�viles simples como estos generalmente son de orden impar. 
#        Esto es para que sean sim�tricos.


# 6.2.2) Medias m�viles de medias m�viles:

# Una raz�n para hacerlo es hacer una media m�vil de orden par sim�trica.
# Por ejemplo, podr�amos tomar un promedio m�vil de orden 4 y luego aplicar otro promedio 
# m�vil de orden 2 a los resultados, este seria un MA(2x4)
beer2 <- window(ausbeer,start=1992)
ma4 <- ma(beer2, order=4, centre=FALSE)
ma2x4 <- ma(beer2, order=4, centre=TRUE)


# 6.2.3) Estimando la tendencia del ciclo de con datos estacionales:

# El uso m�s com�n de promedios m�viles centrados es para estimar el ciclo de 
# tendencia a partir de datos estacionales.

# Ejemplo: fabricaci�n de equipos el�ctricos:
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(elecequip, series="Data") +
  autolayer(ma(elecequip, 12), series="12-MA") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))


# 6.2.4) Promedios m�viles ponderados:

# Una ventaja importante de los promedios m�viles ponderados es que producen una 
# estimaci�n m�s uniforme del ciclo de tendencia. 


## 6.3: Descomposici�n cl�sica ##
# ***************************** #

# Nota: Si bien la descomposici�n cl�sica todav�a se usa ampliamente, 
# no se recomienda, ya que ahora hay varios m�todos mucho mejores.

# 6.3.1) Descomposici�n aditiva:

# 6.3.2) Descomposici�n multiplicativa:

elecequip %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of electrical equipment index")


## 6.4: descomposici�n X11 ##
# ************************* #

#Este m�todo se basa en la descomposici�n cl�sica, pero incluye muchos pasos, caracter�sticas
# y metodos sofisticados adicionales para superar las limitaciones de la descomposici�n cl�sica.

# El m�todo X11 est� disponible utilizando la funci�n seas() del paquete estacional para R.

elecequip %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of electrical equipment index")

# Dado el resultado de la funci�n seas(), seasonal() extraer� el componente estacional, 
# trendcycle() extraer� el componente tendencial del ciclo, remainder()extraer� el componente 
# restante y seasadj() calcular� la serie temporal ajustada estacionalmente.

autoplot(elecequip, series="Data") +
  autolayer(trendcycle(fit), series="Trend") +
  autolayer(seasadj(fit), series="Seasonally Adjusted") +
  xlab("Year") + ylab("New orders index") +
  ggtitle("Electrical equipment manufacturing (Euro area)") +
  scale_colour_manual(values=c("gray","blue","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))

#Puede ser �til utilizar gr�ficos estacionales y gr�ficos de sub-series estacionales 
#del componente estacional. Estos nos ayudan a visualizar la variaci�n en el componente 
#estacional a lo largo del tiempo.

fit %>% seasonal() %>% ggsubseriesplot() + ylab("Seasonal")

## 6.5: Descomposici�n de SEATS ##
# ****************************** #

#"SEATS" significa "Extracci�n estacional en la serie temporal de "ARIMA". 
#Este procedimiento fue desarrollado en el Banco de Espa�a. Se usa a travez del paquete seasonal

elecequip %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of electrical equipment index")

#Como con el m�todo X11, podemos utilizar la funcioness seasonal(), trendcycle() y remainder()
#para extraer los componentes individuales, y seasadj() para calcular las series de tiempo 
#ajustados estacionalmente.


## 6.6: Descomposici�n STL ##
# ************************* #

#STL es un acr�nimo de "Descomposici�n estacional y de tendencia usando Loess", 
#"Loess" es un m�todo para estimar relaciones no lineales.

# STL tiene varias ventajas sobre los m�todos de descomposici�n cl�sicos, SEATS y X11:
# a) A diferencia de SEATS y X11, STL manejar� cualquier tipo de estacionalidad, 
#    no solo datos mensuales y trimestrales.
# b) El componente estacional puede cambiar con el tiempo, 
#    y el usuario puede controlar la tasa de cambio. 
# c) La suavidad del ciclo de tendencia tambi�n puede ser controlada por el usuario.
# d) Puede ser robusto para los valores at�picos. 

elecequip %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot()

#Notas adicionlaes sobre STL:
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Los dos par�metros principales que se elegir�n al usar STL son la ventana tendencial
# del ciclo ( t.window) y la ventana estacional ( s.window). Estos controlan qu� tan r�pido 
#pueden cambiar el ciclo de tendencia y los componentes estacionales. 
#Los valores m�s peque�os permiten cambios m�s r�pidos. Ambos deben ser n�meros impares; 
#t.window es el n�mero de observaciones consecutivas que se utilizar�n al estimar la tendecia
#del ciclo; s.windowes el n�mero de a�os consecutivos que se utilizar�n para estimar cada valor
#en el componente estacional. 

#Al igual que con los otros m�todos de descomposici�n discutidos en este libro, para obtener
#una grafica de los componentes separados , use la funci�n seasonal() para el componente 
#estacional, la funci�n trendcycle() para el componente de ciclo de tendencia y 
#la funci�n remainder() para el componente restante. La funci�n seasadj() se puede usar para 
#calcular la serie ajustada estacionalmente.


## 6.7 Medici�n de la fuerza de la tendencia y la estacionalidad ##
# *************************************************************** #

# Se puede utilizar una descomposici�n de series temporales para medir la fuerza de 
# la tendencia y la estacionalidad en una serie temporal (Wang, Smith y Hyndman, 2006 )


## 6.8 Predicci�n con descomposici�n ##
# *********************************** #

#Para pronosticar una serie temporal descompuesta, pronosticamos el componente estacional, 
#y el componente estacionalmente ajustado, por separado. Para pronosticar el componente 
#desestacionalizado, se puede utilizar cualquier m�todo de pron�stico no estacional.

# Ejemplos de equipos electricos:
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#muestra pron�sticos ingenuos ajustados estacionalmente
fit <- stl(elecequip, t.window=13, s.window="periodic",
           robust=TRUE)
fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("New orders index") +
  ggtitle("Naive forecasts of seasonally adjusted data")

#Debe especificar el m�todo que se utiliza en los datos ajustados estacionalmente, 
#y la funci�n stl har� la reestacionalizaci�n por usted.

#pron�stico ingenuo de los datos ajustados estacionalmente y un pron�stico ingenuo estacional
#del componente estacional, despu�s de una descomposici�n STL de los datos.
fit %>% forecast(method="naive") %>%
  autoplot() + ylab("New orders index")

#Un enfoque para reestacionalizarusar la stlf()funci�n. El siguiente c�digo descompondr� 
#las series de tiempo usando STL, pronosticar� las series ajustadas estacionalmente y 
#devolver� los pron�sticos reestacionalizados. 

fcast <- stlf(elecequip, method='naive')













































































