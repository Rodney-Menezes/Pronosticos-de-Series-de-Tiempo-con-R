#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#



####  Cap 3 - La caja de herramientas del pronosticador  ####
#     =================================================     #

#Paquetes que se utilizar�
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("GGally")


library(forecast)
library(ggplot2)
library(fpp2)
library(GGally)


set.seed(30)  #ponemos semilla



## 3.1: Algunos m�todos de pron�stico simples ##
# ******************************************** #

y <- ts(rnorm(50)) # y contains the time series
h <-2 #h is the forecast horizon

# 3.1.1)M�todo promedio:
meanf(y,h) #pronostico metodo del promedio

# 3.1.2) M�todo ingenuo
naive(y, h)
rwf(y, h) # Equivalent alternative
#Nota: Para pron�sticos ingenuos, simplemente establecemos que todos 
#los pron�sticos sean el valor de la �ltima observaci�n

#3.1.3) M�todo ingenuo estacional:
snaive(y, h)

#3.1.4) M�todo ingenuo con deriva:
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
            series="Na�ve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal na�ve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))


# 2: Epara datos de precio de acciones de Google.
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Na�ve", PI=FALSE) +
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

#Nota: La funci�n monthdays() calcular� el n�mero de d�as en cada mes o trimestre.
#Por ejemplo, si est� estudiando la producci�n mensual de leche en una granja, 
#habr� una variaci�n entre los meses simplemente debido a los diferentes n�meros de 
#d�as en cada mes, adem�s de la variaci�n estacional a lo largo del a�o.

# 3.2.2) Ajustes poblacionales:
# Cualquier dato que se vea afectado por los cambios de la poblaci�n se puede ajustar 
#para proporcionar datos per c�pita.

#3.2.3) Ajustes de inflaci�n:
#Para ajustar la inflacion de los bienes de consumo se usa el IPC.

#3.2.4) Transformaciones matematicas:
#1) transformaciones logaritmicas
#2) transformacion de de poder (con raices cuadradas o cubicas
#3) las transformaciones de Box-Cox:
(lambda <- BoxCox.lambda(elec)) #elegir� un valor de lambda para usted.
autoplot(BoxCox(elec,lambda))


#Nota: Un buen valor de es aquel que hace que el tama�o de la variaci�n estacional sea casi 
#igual en toda la serie, ya que eso simplifica el modelo de pron�stico. 


#Ejemplo: pronostico precio promedio anual del huevo con transf log y el metdodo de deriva:
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

#La l�nea azul en la Figura 3.4 muestra las medianas de pron�stico, mientras que la l�nea roja
#muestra los medios de pron�stico. Observe c�mo la distribuci�n de pron�stico sesgada levanta 
#el pron�stico puntual cuando usamos el ajuste de sesgo.

#El ajuste de sesgo no se realiza de forma predeterminada en el paquete de pron�stico. 
#Si desea que sus pron�sticos sean medios en lugar de medianas, 
#use el argumento biasadj=TRUE cuando seleccione su par�metro de transformaci�n Box-Cox.


## 3.3: Diagnostico de residuales ##
# ******************************** #

#Ejemplo: pronosticar el precio diario de cierre de acciones de Google
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#graficamos los datos
autoplot(goog200) + 
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

#graficamos los Los residuales obtenidos al pronosticar esta serie usando el m�todo ingenuo 
res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from na�ve method")

#histograma de los residuos al pronosticar con el metodo ingenuo
gghistogram(res) + ggtitle("Histogram of residuals")

#ACF de los residuos del m�todo ingenuo
ggAcf(res) + ggtitle("ACF of residuals")


#3.3.1) Pruebas de portmanteau:

#a) prueba de Box-Pierce 
Box.test(res, lag=10, fitdf=0) # lag=h and fitdf=K

#b)prueba de Ljung-Box (m�s precisa) 
Box.test(res,lag=10, fitdf=0, type="Lj") # lag=h and fitdf=K

#Nota: Para ambos Q y Q*, los resultados no son significativos (es decir, los p-values 
#son relativamente grandes). Por lo tanto, podemos concluir que 
#los residuos no son distinguibles de una serie de ruido blanco.

#Todos estos m�todos para verificar los residuos se empaquetan en la funcion checkresiduals()
checkresiduals(naive(goog200))
#Incluye: graf de tiempo, ACF, Histograma y 
#realiza una prueba de Ljung-Box con los grados correctos de libertad


## 3.4: Evaluaci�n de la precisi�n del pron�stico ##
# ************************************************ #

#3.4.1) Funciones para subconjuntar una serie de tiempo:

#3.4.1.1) Funcion Window:
window(ausbeer, start=1995) #extrae todos los datos desde 1995 en adelante.
#La window()funci�n presentada en el Cap�tulo 2 es �til cuando se extrae una parte 
#de una serie de tiempo, como la que necesitamos cuando se crean conjuntos de 
#entrenamiento y prueba. 

#3.4.1.2) Funcion subset()que permite m�s tipos de subconjuntos. 

#a)Una gran ventaja de esta funci�n es que permite el uso de �ndices para elegir un subconjunto.
subset(ausbeer, start=length(ausbeer)-4*5) #extrae los �ltimos 5 a�os de observaciones de ausbeer

#b)Tambi�n permite extraer todos los valores para una temporada espec�fica.
subset(ausbeer, quarter = 1) #extrae los primeros trimestres de todos los a�os

# 3.4.1.3) Finalmente head y/O tail son �tiles para extraer las primeras o �ltimas observaciones. 
tail(ausbeer, 4*5) #los �ltimos 5 a�os ausbeer


# 3.4.2)Errores de pron�stico:

#Un "error" de pron�stico es la diferencia entre un valor observado y su pron�stico.

#los errores de pron�stico son diferentes de los residuales de dos maneras:
#1) los residuos se calculan en el conjunto de entrenamiento mientras que 
#   los errores de pron�stico se calculan en el conjunto de prueba .
#2) los residuos se basan en pron�sticos de un solo paso, mientras que los 
#   errores de pron�stico pueden involucrar pron�sticos de varios pasos .

#Podemos medir la precisi�n del pron�stico resumiendo los errores de pron�stico 
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
  autolayer(beerfit2, series="Na�ve", PI=FALSE) + #graficar pronostico metodo ingenuo
  autolayer(beerfit3, series="Seasonal na�ve", PI=FALSE) + # graf p. metod ingenuo estacional
  xlab("Year") + ylab("Megalitres") + #etiquetas de las coordenadas
  ggtitle("Forecasts for quarterly beer production") + #nombre de la grafica
  guides(colour=guide_legend(title="Forecast")) #etiqueta y color de la leyenda

beer3 <- window(ausbeer, start=2008) #se extrae una parte de la base datos para prueba
accuracy(beerfit1, beer3) #aplicamos pronostico metodo promedio a los datos de prueba
accuracy(beerfit2, beer3) #aplicamos pronostico metodo ingenuo a los datos de prueba
accuracy(beerfit3, beer3)#aplicamos pronostico metodo ingenuo estacional a los datos de prueba

#Nota:l m�todo ingenuo estacional es el mejor para estos datos, aunque a�n se puede mejorar


#Ejemplo: con datos de precio de acciones de Google
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)

autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Na�ve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

#Aqu�, el mejor m�todo es el m�todo de deriva 
#(independientemente de la medida de precisi�n utilizada).


# 3.4.3) Validaci�n cruzada de series de tiempo:

#Una versi�n m�s sofisticada de los conjuntos de entrenamiento/prueba 
#es la validaci�n cruzada de series temporales.

#La validaci�n cruzada de series temporales se implementa con la funci�n tsCV()


#Ejemplo:comparamos el RMSE obtenido mediante validaci�n cruzada de series 
#temporales con el RMSE residual 
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

e <- tsCV(goog200, rwf, drift=TRUE, h=1) #RMSE de validaci�n cruzada
sqrt(mean(e^2, na.rm=TRUE))

sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE)) #RMSE residual 

# Como se esperaba, el RMSE de los residuos es menor, ya que los "pron�sticos" 
# correspondientes se basan en un modelo ajustado a todo el conjunto de datos, 
# en lugar de ser pron�sticos verdaderos

# Una buena manera de elegir el mejor modelo de pron�stico es encontrar el modelo 
# con el RMSE m�s peque�o calculado utilizando la validaci�n cruzada de series temporales.


# 3.4.4) Operador de tuber�a:

#La fealdad del c�digo R anterior hace que esta sea una buena oportunidad para 
#introducir algunas formas alternativas de encadenar las funciones R juntas. 
# otra forma de escribir el ejemlo anterior es usando el operador de tuber�a %>% :

goog200 %>% tsCV(forecastfunction=rwf, drift=TRUE, h=1) -> e
e^2 %>% mean(na.rm=TRUE) %>% sqrt()

goog200 %>% rwf(drift=TRUE) %>% residuals() -> res #Por ejemplo, esta l�nea se leer como "Tomar la serie goog200, pasarlo a rwf()con drift=TRUE, calcular los residuos resultantes, y guardarlas como res".
res^2 %>% mean(na.rm=TRUE) %>% sqrt()

# El operador de tuber�a siempre que haga que el c�digo sea m�s f�cil de leer

# Ejemplo: precio de acciones de Google usando tsCV ()
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#El siguiente c�digo eval�a el rendimiento de pron�stico de pron�sticos ingenuos 
#de 1 a 8 pasos con tsCV(), utilizando MSE como la medida de error de pron�stico.

e <- tsCV(goog200, forecastfunction=naive, h=8)
mse <- colMeans(e^2, na.rm = T) # Compute the MSE values and remove missing values
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

#El gr�fico muestra que el error de pron�stico aumenta a medida que aumenta 
#el horizonte de pron�stico, como era de esperar.


## 3.5: Intervalos de predicci�n ##
# ******************************* #

#un intervalo de predicci�n da un intervalo dentro del cual esperamos 
#medir y_t con una probabilidad especificada. 

#La siguiente tabla da el valor del valor critico c para un rango de probabilidades 
#de cobertura asumiendo errores de pron�stico distribuidos normalmente, aunque siempre
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

# 3.5.1) Intervalos de predicci�n de un paso

#Al pronosticar un paso adelante, la desviaci�n est�ndar de la distribuci�n 
#del pron�stico es casi la misma que la desviaci�n est�ndar de los residuos.

# 3.5.2) Intervalos de predicci�n de varios pasos

#Una caracter�stica com�n de los intervalos de predicci�n es que aumentan en longitud 
#a medida que aumenta el horizonte de pron�stico. Cuanto m�s adelante pronostiquemos, 
#m�s incertidumbre se asocia con el pron�stico y mayores ser�n los intervalos de predicci�n. 

#Para producir un intervalo de predicci�n, es necesario tener una estimaci�n de ??_h.
#Como ya se se�al�, para pron�sticos de un paso (h=1),la desviaci�n est�ndar residual 
#proporciona una buena estimaci�n de la desviaci�n est�ndar pronosticada ??_1.

#Para pron�sticos de varios pasos, se requiere un m�todo de c�lculo m�s complicado. 
#Estos c�lculos suponen que los residuos no est�n correlacionados.

#3.5.3) M�todos de referencia

#Pron�sticos medios 
#Pron�sticos ingenuos
#Pron�sticos ingenuos estacionales 
#Pron�sticos de deriva 

#Los intervalos de predicci�n se calcular�n para usted cuando utilice 
#cualquiera de los m�todos de pron�stico de referencia.

# Ejemplo: intervalos de prediccion en los precios de acciones de Google con metodo ingenuo.
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

naive(goog200) # prediccion, resultados numericos
autoplot(naive(goog200)) #prediccion, graficos

#Cuando se trazan, los intervalos de predicci�n se muestran como una regi�n sombreada, 
#con la intensidad del color que indica la probabilidad asociada con el intervalo al 80% y 95% 

# 3.5.4) Intervalos de predicci�n de los residuos de bootstrap:
#Cuando una distribuci�n normal de los errores de pron�stico es una suposici�n irracional, 
#una alternativa es usar bootstrapping, que solo supone que 
#los errores de pron�stico no est�n correlacionados.

#Para generar tales intervalos, simplemente podemos agregar 
#el argumento bootstrap a nuestras funciones de pron�stico.
#Ejem: 
naive(goog200, bootstrap=TRUE)

#3.5.5) Predicci�n de intervalos con transformaciones:
#Si se ha utilizado una transformaci�n, entonces el intervalo de predicci�n se debe calcular 
#en la escala transformada, y los puntos finales se transforman para dar un intervalo de 
#predicci�n en la escala original. Este enfoque conserva la cobertura de probabilidad 
#del intervalo de predicci�n, aunque ya no ser� sim�trico alrededor del pron�stico del punto.


