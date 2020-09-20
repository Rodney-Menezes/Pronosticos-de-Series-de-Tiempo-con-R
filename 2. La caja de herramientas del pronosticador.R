#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#



####  Cap 3 - La caja de herramientas del pronosticador  ####
#     =================================================     #

#Paquetes que se utilizará
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("GGally")


library(forecast)
library(ggplot2)
library(fpp2)
library(GGally)


set.seed(30)  #ponemos semilla



## 3.1: Algunos métodos de pronóstico simples ##
# ******************************************** #

y <- ts(rnorm(50)) # y contains the time series
h <-2 #h is the forecast horizon

# 3.1.1)Método promedio:
meanf(y,h) #pronostico metodo del promedio

# 3.1.2) Método ingenuo
naive(y, h)
rwf(y, h) # Equivalent alternative
#Nota: Para pronósticos ingenuos, simplemente establecemos que todos 
#los pronósticos sean el valor de la última observación

#3.1.3) Método ingenuo estacional:
snaive(y, h)

#3.1.4) Método ingenuo con deriva:
rwf(y, h, drift=TRUE)


#-----------------
#Ejemplos: :
#-----------------

# 1: Para datos trimestrales de la produccion de cerveza
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
  
# Set training data from 1992 to 2007
beer2 <- window(ausbeer,start=1992,end=c(2007,4))

# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))


# 2: Epara datos de precio de acciones de Google.
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))


## 3.2: Transformaciones y ajustes ##
# ********************************* #

# 3.2.1) Ajustes de calendario:
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

#Nota: La función monthdays() calculará el número de días en cada mes o trimestre.
#Por ejemplo, si está estudiando la producción mensual de leche en una granja, 
#habrá una variación entre los meses simplemente debido a los diferentes números de 
#días en cada mes, además de la variación estacional a lo largo del año.

# 3.2.2) Ajustes poblacionales:
# Cualquier dato que se vea afectado por los cambios de la población se puede ajustar 
#para proporcionar datos per cápita.

#3.2.3) Ajustes de inflación:
#Para ajustar la inflacion de los bienes de consumo se usa el IPC.

#3.2.4) Transformaciones matematicas:
#1) transformaciones logaritmicas
#2) transformacion de de poder (con raices cuadradas o cubicas
#3) las transformaciones de Box-Cox:
(lambda <- BoxCox.lambda(elec)) #elegirá un valor de lambda para usted.
autoplot(BoxCox(elec,lambda))


#Nota: Un buen valor de es aquel que hace que el tamaño de la variación estacional sea casi 
#igual en toda la serie, ya que eso simplifica el modelo de pronóstico. 


#Ejemplo: pronostico precio promedio anual del huevo con transf log y el metdodo de deriva:
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

#La línea azul en la Figura 3.4 muestra las medianas de pronóstico, mientras que la línea roja
#muestra los medios de pronóstico. Observe cómo la distribución de pronóstico sesgada levanta 
#el pronóstico puntual cuando usamos el ajuste de sesgo.

#El ajuste de sesgo no se realiza de forma predeterminada en el paquete de pronóstico. 
#Si desea que sus pronósticos sean medios en lugar de medianas, 
#use el argumento biasadj=TRUE cuando seleccione su parámetro de transformación Box-Cox.


## 3.3: Diagnostico de residuales ##
# ******************************** #

#Ejemplo: pronosticar el precio diario de cierre de acciones de Google
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#graficamos los datos
autoplot(goog200) + 
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

#graficamos los Los residuales obtenidos al pronosticar esta serie usando el método ingenuo 
res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

#histograma de los residuos al pronosticar con el metodo ingenuo
gghistogram(res) + ggtitle("Histogram of residuals")

#ACF de los residuos del método ingenuo
ggAcf(res) + ggtitle("ACF of residuals")


#3.3.1) Pruebas de portmanteau:

#a) prueba de Box-Pierce 
Box.test(res, lag=10, fitdf=0) # lag=h and fitdf=K

#b)prueba de Ljung-Box (más precisa) 
Box.test(res,lag=10, fitdf=0, type="Lj") # lag=h and fitdf=K

#Nota: Para ambos Q y Q*, los resultados no son significativos (es decir, los p-values 
#son relativamente grandes). Por lo tanto, podemos concluir que 
#los residuos no son distinguibles de una serie de ruido blanco.

#Todos estos métodos para verificar los residuos se empaquetan en la funcion checkresiduals()
checkresiduals(naive(goog200))
#Incluye: graf de tiempo, ACF, Histograma y 
#realiza una prueba de Ljung-Box con los grados correctos de libertad


## 3.4: Evaluación de la precisión del pronóstico ##
# ************************************************ #

#3.4.1) Funciones para subconjuntar una serie de tiempo:

#3.4.1.1) Funcion Window:
window(ausbeer, start=1995) #extrae todos los datos desde 1995 en adelante.
#La window()función presentada en el Capítulo 2 es útil cuando se extrae una parte 
#de una serie de tiempo, como la que necesitamos cuando se crean conjuntos de 
#entrenamiento y prueba. 

#3.4.1.2) Funcion subset()que permite más tipos de subconjuntos. 

#a)Una gran ventaja de esta función es que permite el uso de índices para elegir un subconjunto.
subset(ausbeer, start=length(ausbeer)-4*5) #extrae los últimos 5 años de observaciones de ausbeer

#b)También permite extraer todos los valores para una temporada específica.
subset(ausbeer, quarter = 1) #extrae los primeros trimestres de todos los años

# 3.4.1.3) Finalmente head y/O tail son útiles para extraer las primeras o últimas observaciones. 
tail(ausbeer, 4*5) #los últimos 5 años ausbeer


# 3.4.2)Errores de pronóstico:

#Un "error" de pronóstico es la diferencia entre un valor observado y su pronóstico.

#los errores de pronóstico son diferentes de los residuales de dos maneras:
#1) los residuos se calculan en el conjunto de entrenamiento mientras que 
#   los errores de pronóstico se calculan en el conjunto de prueba .
#2) los residuos se basan en pronósticos de un solo paso, mientras que los 
#   errores de pronóstico pueden involucrar pronósticos de varios pasos .

#Podemos medir la precisión del pronóstico resumiendo los errores de pronóstico 
#de diferentes maneras:

#1) Errores dependientes de la escala:
    #MAE : Error absoluto medio
    #RMSE: Error cuadratico medio

#2) Porcentaje de errores
    #MAPE : Error Porcentual absoluto medio
    #MAPEs : MAPE simetrico

#3) Errores escalados
    #MASE :error medio absoluto escalado


#Ejemplo: con datos de produccion de cerbeza
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

beer2 <- window(ausbeer,start=1992,end=c(2007,4)) #se extrae datos para entrenamiento
beerfit1 <- meanf(beer2,h=10) #pronostico metodo promedio
beerfit2 <- rwf(beer2,h=10) #pronostico metodo ingenuo
beerfit3 <- snaive(beer2,h=10) #pronostico metodo ingenuo estacional

autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) + #graficar pronostico metodo promedio
  autolayer(beerfit2, series="Naïve", PI=FALSE) + #graficar pronostico metodo ingenuo
  autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) + # graf p. metod ingenuo estacional
  xlab("Year") + ylab("Megalitres") + #etiquetas de las coordenadas
  ggtitle("Forecasts for quarterly beer production") + #nombre de la grafica
  guides(colour=guide_legend(title="Forecast")) #etiqueta y color de la leyenda

beer3 <- window(ausbeer, start=2008) #se extrae una parte de la base datos para prueba
accuracy(beerfit1, beer3) #aplicamos pronostico metodo promedio a los datos de prueba
accuracy(beerfit2, beer3) #aplicamos pronostico metodo ingenuo a los datos de prueba
accuracy(beerfit3, beer3)#aplicamos pronostico metodo ingenuo estacional a los datos de prueba

#Nota:l método ingenuo estacional es el mejor para estos datos, aunque aún se puede mejorar


#Ejemplo: con datos de precio de acciones de Google
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)

autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

#Aquí, el mejor método es el método de deriva 
#(independientemente de la medida de precisión utilizada).


# 3.4.3) Validación cruzada de series de tiempo:

#Una versión más sofisticada de los conjuntos de entrenamiento/prueba 
#es la validación cruzada de series temporales.

#La validación cruzada de series temporales se implementa con la función tsCV()


#Ejemplo:comparamos el RMSE obtenido mediante validación cruzada de series 
#temporales con el RMSE residual 
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

e <- tsCV(goog200, rwf, drift=TRUE, h=1) #RMSE de validación cruzada
sqrt(mean(e^2, na.rm=TRUE))

sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE)) #RMSE residual 

# Como se esperaba, el RMSE de los residuos es menor, ya que los "pronósticos" 
# correspondientes se basan en un modelo ajustado a todo el conjunto de datos, 
# en lugar de ser pronósticos verdaderos

# Una buena manera de elegir el mejor modelo de pronóstico es encontrar el modelo 
# con el RMSE más pequeño calculado utilizando la validación cruzada de series temporales.


# 3.4.4) Operador de tubería:

#La fealdad del código R anterior hace que esta sea una buena oportunidad para 
#introducir algunas formas alternativas de encadenar las funciones R juntas. 
# otra forma de escribir el ejemlo anterior es usando el operador de tubería %>% :

goog200 %>% tsCV(forecastfunction=rwf, drift=TRUE, h=1) -> e
e^2 %>% mean(na.rm=TRUE) %>% sqrt()

goog200 %>% rwf(drift=TRUE) %>% residuals() -> res #Por ejemplo, esta línea se leer como "Tomar la serie goog200, pasarlo a rwf()con drift=TRUE, calcular los residuos resultantes, y guardarlas como res".
res^2 %>% mean(na.rm=TRUE) %>% sqrt()

# El operador de tubería siempre que haga que el código sea más fácil de leer

# Ejemplo: precio de acciones de Google usando tsCV ()
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#El siguiente código evalúa el rendimiento de pronóstico de pronósticos ingenuos 
#de 1 a 8 pasos con tsCV(), utilizando MSE como la medida de error de pronóstico.

e <- tsCV(goog200, forecastfunction=naive, h=8)
mse <- colMeans(e^2, na.rm = T) # Compute the MSE values and remove missing values
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

#El gráfico muestra que el error de pronóstico aumenta a medida que aumenta 
#el horizonte de pronóstico, como era de esperar.


## 3.5: Intervalos de predicción ##
# ******************************* #

#un intervalo de predicción da un intervalo dentro del cual esperamos 
#medir y_t con una probabilidad especificada. 

#La siguiente tabla da el valor del valor critico c para un rango de probabilidades 
#de cobertura asumiendo errores de pronóstico distribuidos normalmente, aunque siempre
#usaremos 95%

#Porcentaje	Multiplicador
#,,,,,,,,,,,,,,,,,,,,,,,,
# 50           	0,67
# 55        	  0,76
# 60   	        0,84
# 65           	0,93
# 70	          1.04
# 75	          1,15
# 80	          1,28
# 85	          1,44
# 90	          1,64
# 95	          1,96
# 96	          2,05
# 97	          2,17
# 98	          2,33
# 99	          2,58

# 3.5.1) Intervalos de predicción de un paso

#Al pronosticar un paso adelante, la desviación estándar de la distribución 
#del pronóstico es casi la misma que la desviación estándar de los residuos.

# 3.5.2) Intervalos de predicción de varios pasos

#Una característica común de los intervalos de predicción es que aumentan en longitud 
#a medida que aumenta el horizonte de pronóstico. Cuanto más adelante pronostiquemos, 
#más incertidumbre se asocia con el pronóstico y mayores serán los intervalos de predicción. 

#Para producir un intervalo de predicción, es necesario tener una estimación de ??_h.
#Como ya se señaló, para pronósticos de un paso (h=1),la desviación estándar residual 
#proporciona una buena estimación de la desviación estándar pronosticada ??_1.

#Para pronósticos de varios pasos, se requiere un método de cálculo más complicado. 
#Estos cálculos suponen que los residuos no están correlacionados.

#3.5.3) Métodos de referencia

#Pronósticos medios 
#Pronósticos ingenuos
#Pronósticos ingenuos estacionales 
#Pronósticos de deriva 

#Los intervalos de predicción se calcularán para usted cuando utilice 
#cualquiera de los métodos de pronóstico de referencia.

# Ejemplo: intervalos de prediccion en los precios de acciones de Google con metodo ingenuo.
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

naive(goog200) # prediccion, resultados numericos
autoplot(naive(goog200)) #prediccion, graficos

#Cuando se trazan, los intervalos de predicción se muestran como una región sombreada, 
#con la intensidad del color que indica la probabilidad asociada con el intervalo al 80% y 95% 

# 3.5.4) Intervalos de predicción de los residuos de bootstrap:
#Cuando una distribución normal de los errores de pronóstico es una suposición irracional, 
#una alternativa es usar bootstrapping, que solo supone que 
#los errores de pronóstico no están correlacionados.

#Para generar tales intervalos, simplemente podemos agregar 
#el argumento bootstrap a nuestras funciones de pronóstico.
#Ejem: 
naive(goog200, bootstrap=TRUE)

#3.5.5) Predicción de intervalos con transformaciones:
#Si se ha utilizado una transformación, entonces el intervalo de predicción se debe calcular 
#en la escala transformada, y los puntos finales se transforman para dar un intervalo de 
#predicción en la escala original. Este enfoque conserva la cobertura de probabilidad 
#del intervalo de predicción, aunque ya no será simétrico alrededor del pronóstico del punto.


