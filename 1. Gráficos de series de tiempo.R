#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#



####  Cap 2 - Graficos de series temporales  ####
#     =====================================     #


#Paquetes que se utilizar�
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("GGally")


library(forecast)
library(ggplot2)
library(fpp2)
library(GGally)

## 2.1: ts objetos ##
# ***************** #

# convertimos los datos a una serie de tiempo:
y <- ts(c(123,39,78,52,110), start=2012) #con frecuencia anual

# Observaciones con frecuencia mensual
y <- ts(z, start=2003, frequency=12)

#Frecuendia de Datos: (los casos mas comunes)
#--------------------------------------------
#Datos	   frecuencia
#Anual	       1
#Trimestral	   4 
#Mensual	    12
#Semanal	    52



## 2.2: Gr�ficos de tiempo ##
# ************************* #

autoplot(melsyd[,"Economy.Class"]) +
  ggtitle("Economy class passengers: Melbourne-Sydney") +
  xlab("Year") +
  ylab("Thousands")

autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")


## 2.3: Patrones de series de tiempo ##
# *********************************** #

# * Tendencia
# * Estacional
# * C�clico


## 2.4: Parcelas estacionales ##
# **************************** #

# Grafica estacional.
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

# Grafica estacional en coordenadas polares. 
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")


## 2.5: Gr�ficos de subseries estacionales ##
# ***************************************** #

ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

#Nota: Las l�neas horizontales indican las medias para cada mes. 
# Es especialmente �til para identificar cambios dentro de estaciones particulares.


## 2.6: Diagramas de dispersi�n ##
# ****************************** #

# Son �tiles para explorar las relaciones entre series de tiempo.

# Comparar series individuales:
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

# Comparar series una contra la otra:
qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")

# Comparar varias series individuales:
autoplot(visnights[,1:5], facets=TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

# Matrices de diagrama de dispersi�n:
GGally::ggpairs(as.data.frame(visnights[,1:5]))


## 2.7: Gr�ficos de retraso ##
# ************************** #

beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

#Nota: Aqu� los colores indican el cuarto de la variable en el eje vertical. 
#Las l�neas conectan puntos en orden cronol�gico. La relaci�n es muy positiva 
#en los rezagos 4 y 8, lo que refleja la fuerte estacionalidad en los datos. 
#La relaci�n negativa observada para los rezagos 2 y 6 ocurre porque 
#los picos (en Q4) se trazan contra los canales (en Q2)


## 2.8: Autocorrelaci�n ##
# ********************** #

# funci�n de autocorrelaci�n o ACF o correlograma:
ggAcf(beer2)

# Tendencia y estacionalidad en parcelas ACF:
aelec <- window(elec, start=1980)      #cargamos datos
autoplot(aelec) + xlab("Year") + ylab("GWh")  #graficamos serie de tiempo

ggAcf(aelec, lag=48)   #ACF
#Nota: La lenta disminuci�n en el ACF a medida que aumentan los retrasos se debe a la tendencia, 
#mientras que la forma "festoneada" se debe a la estacionalidad.


## 2.9: Ruido blanco ##
# ******************* #

#Nota: Las series de tiempo que no muestran autocorrelaci�n se llaman ruido blanco

set.seed(30)  #ponemos semilla

y <- ts(rnorm(50))  #creamos data set
autoplot(y) + ggtitle("White noise") #graficamos datos

ggAcf(y)   #ACF

#Nota:
#Para las series de ruido blanco, esperamos que cada autocorrelaci�n sea cercana a cero.
#Para una serie de ruido blanco, esperamos que el 95% de los picos en el ACF 
#se encuentren dentro de donde es la longitud de la serie de tiempo.
#Si uno o m�s picos grandes est�n fuera de estos l�mites, o si sustancialmente 
#m�s del 5% de los picos est�n fuera de estos l�mites, 
#entonces la serie probablemente no sea ruido blanco.
