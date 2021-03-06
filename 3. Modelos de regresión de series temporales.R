#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 5 - Modelos de regresi�n de series temporales  ####
#     =================================================     #


#Paquetes que se utilizar�
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("GGally")
install.packages("gridExtra")


library(forecast)
library(ggplot2)
library(fpp2)
library(GGally)
library(gridExtra)

set.seed(30)  #ponemos semilla


## 5.1:  El modelo lineal ##
# ************************ #

# 5.1.1) Regresi�n lineal simple

# Ejemplo: gasto de consumo de EE.UU:
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#graficamos las series:
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

#estimacion grafica:
uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) #> `geom_smooth()` using formula 'y ~ x'

#estimacion usando datos:
tslm(Consumption ~ Income, data=uschange)


## 5.2: El modelo lineal ##
# *********************** #

# 5.2.1) Regresi�n lineal m�ltiple

# Ejemplo: gasto de consumo de EE.UU:
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

# graficamos las series

autoplot(uschange[,c("Consumption","Income","Production","Savings","Unemployment")],facets=TRUE, colour=T) +
  ylab("% change") + xlab("Year")

# Diagrama de dispersion
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

# estimamos los coeficientes
fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings,
  data=uschange)
summary(fit.consMR)


# 5.2.2) Regresi�n lineal m�ltiple
# ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#grafica comparacion de datos reales y ajustados
autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

#estimacion grafica:
cbind(Data = uschange[,"Consumption"],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("Percent change in US consumption expenditure") +
  geom_abline(intercept=0, slope=1)


## 5.3: Evaluaci�n del modelo de regresi�n. ##
# ****************************************** #

#Despu�s de seleccionar las variables de regresi�n y ajustar un modelo de regresi�n, es 
#necesario trazar los residuos para verificar que se hayan cumplido los supuestos del modelo.

#Propiedades de "e":
#-------------------
#mean = 0
#corr(e,x)=0
# no autocorr serial: corr(et,et-1) != 1
# e ~ N -> no necesario para pronostico pero da mejores intervalos de predicci�n.

# 5.3.1) Gr�fico ACF de residuos (para ver autocorr):

#Los pron�sticos de un modelo con errores autocorrelacionados a�n no son imparciales, 
#por lo que no son "incorrectos", pero generalmente tendr�n intervalos de predicci�n m�s 
#largos de lo necesario. Por lo tanto, siempre debemos mirar un gr�fico ACF de los residuos.

# Otra prueba �til de autocorrelaci�n en los residuos dise�ados para tener en cuenta el modelo
#de regresi�n es la prueba de Breusch-Godfrey , tambi�n conocida como 
#prueba LM (multiplicador de Lagrange) para la correlaci�n en serie. Se utiliza para probar 
#la hip�tesis conjunta de que no hay autocorrelaci�n en los residuos hasta un cierto orden 
#especificado. Un valor p peque�o indica que hay una autocorrelaci�n significativa restante en
#los residuos. La prueba Breusch-Godfrey.

# 5.3.2) Histograma de residuos:
#verifica si los residuos se distribuyen normalmente.

# 5.3.3) con la funci�n checkresiduals(), podemos obtener todos los diagn�sticos 
#residuales �tiles mencionados anteriormente: (grafica residuos,ACF,histograma y prueba B-G)

checkresiduals(fit.consMR)

# 5.3.4) Gr�ficos residuales contra predictores:

#Esperar�amos que los residuos se dispersen al azar sin mostrar ning�n patr�n sistem�tico. 
#Una manera simple y r�pida de verificar esto es examinar diagramas de dispersi�n de los 
#residuos contra cada una de las variables predictoras. 
#Si estos diagramas de dispersi�n muestran un patr�n, entonces la relaci�n 
#puede ser no lineal y el modelo deber� modificarse en consecuencia.
#Tambi�n es necesario trazar los residuos contra cualquier predictor que no est� en el modelo.
#Si alguno de estos muestra un patr�n, entonces el predictor correspondiente puede necesitar 
#ser agregado al modelo (posiblemente en una forma no lineal).

df <- as.data.frame(uschange)
df[,"Residuals"]  <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) +
  geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) +
  geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) +
  geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) +
  geom_point()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow=2)

# 5.3.5) Gr�ficos residuales contra valores ajustados:

#Una gr�fica de los residuos contra los valores ajustados tampoco debe mostrar ning�n patr�n. 
#Si se observa un patr�n, puede haber "heterocedasticidad" en los errores, 
#lo que significa que la varianza de los residuos puede no ser constante.
#Si se produce este problema, puede ser necesaria una transformaci�n de la 
#variable de pron�stico, como un logaritmo o una ra�z cuadrada.

cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

# 5.3.6) Observaciones at�picas e influyentes:

#Las observaciones que toman valores extremos en comparaci�n con la mayor�a de los datos 
#se llaman valores at�picos . Las observaciones que tienen una gran influencia en los 
#coeficientes estimados de un modelo de regresi�n se denominan observaciones influyentes. 
#Por lo general, las observaciones influyentes tambi�n son valores at�picos 
#que son extremos en la direcci�n 

#Una fuente de valores at�picos es la entrada de datos incorrecta.  
#Si se identifica dicha observaci�n y se ha registrado incorrectamente, 
#debe corregirse o eliminarse de la muestra de inmediato.

#Los valores at�picos tambi�n ocurren cuando algunas observaciones son simplemente diferentes.
#En este caso, puede que no sea aconsejable eliminar estas observaciones. 
#Si se ha identificado una observaci�n como un valor at�pico probable, 
#es importante estudiarla y analizar las posibles razones detr�s de ella. 
#La decisi�n de eliminar o retener una observaci�n puede ser desafiante 
#(especialmente cuando los valores at�picos son observaciones influyentes). 
#Es aconsejable informar los resultados con y sin la eliminaci�n de tales observaciones.

# 5.3.7) Regresi�n espuria:

#La mayor�a de las veces, los datos de series temporales son "no estacionarios"
#La regresi�n de series de tiempo no estacionarias puede conducir a regresiones espurias. 
#la alta autocorrelaci�n residual pueden ser signos de regresi�n espuria.

#Ejemplo:
#,,,,,,,,

aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)

checkresiduals(fit)


## 5.4: Evaluaci�n del modelo de regresi�n. ##
# ****************************************** #

#Tendencia:
#Se puede especificar una variable de tendencia en la funci�n tslm() 
#utilizando el trend predictor.

#Variables ficticias : 
#tslm() manejar� autom�ticamente este caso si especifica una variable de factor como predictor.
#Por lo general, no es necesario crear manualmente las variables ficticias correspondientes.

#Variables ficticias estacionales: (variables ficticias para multiples categorias)
#La funci�n tslm() manejar� autom�ticamente esta situaci�n si especifica el predictor season.

#Ejemplo: producci�n de cerveza trimestral en Australia
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

beer2 <- window(ausbeer, start=1992) #cargamos datos 
autoplot(beer2) + xlab("Year") + ylab("Megalitres") #graficamos

fit.beer <- tslm(beer2 ~ trend + season) #estimamos
summary(fit.beer)

#comparacion de datos y ajustados
autoplot(beer2, series="Data") +
  autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")

#Producci�n real de cerveza trazada contra la producci�n de cerveza prevista.
cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

# 5.4.1) Variables de intervenci�n:

#A menudo es necesario modelar intervenciones que pueden haber afectado la variable a 
#pronosticar.

#Cuando el efecto dura solo un per�odo, usamos una variable de "pico". Esta es una variable 
#ficticia que toma el valor uno en el per�odo de la intervenci�n y cero en otro lugar. 
#Una variable de pico es equivalente a una variable ficticia para manejar un valor at�pico.

#Otras intervenciones tienen un efecto inmediato y permanente. Si una intervenci�n causa un 
#cambio de nivel (es decir, el valor de la serie cambia repentina y permanentemente desde el 
#momento de la intervenci�n), entonces usamos una variable de "paso". 
#Una variable de paso toma el valor cero antes de la intervenci�n y uno desde 
#el momento de la intervenci�n en adelante.

#Otra forma de efecto permanente es un cambio de pendiente. Aqu� la intervenci�n se maneja 
#utilizando una tendencia lineal por partes; Una tendencia que se dobla en el momento de la 
#intervenci�n y, por lo tanto, no es lineal.

# 5.4.2) D�as de negociaci�n:
#Para datos mensuales o trimestrales, la funci�n bizdays() calcular� el n�mero de d�as 
#de negociaci�n en cada per�odo.

# 5.4.2) Retrasos distribuidos:
#Es com�n requerir que los coeficientes disminuyan a medida que aumenta el retraso,

# 5.4.3) Pascua de Resurrecci�n:
#La Pascua difiere de la mayor�a de los d�as festivos porque no se celebra en la misma 
#fecha cada a�o, y su efecto puede durar varios d�as.La funci�n easter() calcular�
#la variable ficticia para usted.

# 5.4.4) series de Fourier:
#Una alternativa al uso de variables ficticias estacionales, especialmente durante largos 
#per�odos estacionales, es usar t�rminos de Fourier.Jean-Baptiste Fourier fue un matem�tico
#franc�s, nacido en la d�cada de 1700, que demostr� que una serie de t�rminos seno y coseno
#de las frecuencias correctas puede aproximarse a cualquier funci�n peri�dica. 
#Podemos usarlos para patrones estacionales.

fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)

#Si solo se utilizan los dos primeros t�rminos de Fourier (x1,t y x2,t) el patr�n estacional 
#seguir� una onda sinusoidal simple. Un modelo de regresi�n que contiene t�rminos de Fourier 
#a menudo se llama regresi�n arm�nica porque los t�rminos sucesivos de Fourier 
#representan arm�nicos de los primeros dos t�rminos de Fourier.


## 5.5 Seleccionar predictores. ##
# ****************************** #

# un enfoque com�n que tambi�n es inv�lido es hacer una regresi�n lineal m�ltiple en todos los
#predictores y no tener en cuenta todas las variables cuyos valores p son mayores que 0.05. 
#Para empezar, la significaci�n estad�stica no siempre indica un valor predictivo. 
#Incluso si el pron�stico no es el objetivo, esta no es una buena estrategia porque los 
#valores p pueden ser enga�osos cuando dos o m�s predictores est�n correlacionados entre s�.

## 5.5.1)*medidas de precisi�n predictiva:
#En cambio, utilizaremos una medida de precisi�n predictiva. Cinco de estas medidas 
#se introducen en esta secci�n. 

# 5.5.1.1) Se pueden calcular utilizando la funci�n CV()
CV(fit.consMR) # este obtiene las 5 medidas

# 5.5.1.2) R ajustado 

# 5.5.1.3) Validaci�n cruzada:
#hay metodos mas rapidos para calcular el CV

# 5.5.1.4) Criterio de informaci�n de Akaike
#La idea aqu� es penalizar el ajuste del modelo (SSE) con el n�mero de par�metros que deben estimarse.

# 5.5.1.5) Criterio de informaci�n bayesiano de Schwarz

# 5.5.2) Mejor regresi�n de subconjunto:
#Siempre que sea posible, se deben ajustar todos los modelos de regresi�n potenciales 
#y se debe seleccionar el mejor modelo en funci�n de una de las medidas discutidas. 
#Esto se conoce como regresi�n de "mejores subconjuntos" 
#o regresi�n de "todos los subconjuntos posibles".

# 5.5.3) Regresi�n gradual:

# 5.5.3.1) regresi�n gradual hacia atr�s:
# a) Comience con el modelo que contiene todos los predictores potenciales.
# b) Eliminar un predictor a la vez. Mantenga el modelo si mejora la medida de la precisi�n 
#    predictiva.
#    Iterar hasta que no haya m�s mejoras.

#Si el n�mero de predictores potenciales es demasiado grande, entonces la regresi�n por 
#pasos hacia atr�s no funcionar� y en su lugar se puede usar la regresi�n por pasos hacia 
#adelante .

# 5.5.3.2) regresi�n por pasos hacia adelante:
#a) Este procedimiento comienza con un modelo que incluye solo la intercepci�n. 
#b) Los predictores se agregan uno a la vez, y el que m�s mejora la medida de la precisi�n 
#   predictiva se conserva en el modelo. 
#c) El procedimiento se repite hasta que no se pueda lograr una mejora adicional.

#Alternativamente para la direcci�n hacia atr�s o hacia adelante, 
#un modelo inicial puede ser uno que incluya un subconjunto de predictores potenciales. 
#En este caso, se debe incluir un paso adicional. Para el procedimiento hacia atr�s tambi�n 
#deber�amos considerar agregar un predictor con cada paso, y para el procedimiento 
#hacia adelante tambi�n deber�amos considerar descartar un predictor con cada paso. 
#Estos se denominan procedimientos h�bridos .


## 5.6 Pron�sticos con regresi�n ##
# ******************************* #

# 5.6.1) Pron�sticos ex ante versus ex post:

#5.6.1.1) Los pron�sticos ex ante:
#son aquellos que se realizan utilizando solo la informaci�n disponible de antemano.
#Estos son pron�sticos genuinos, hechos de antemano usando cualquier 
#informaci�n disponible en ese momento.

#5.6.1.1) Los pron�sticos ex ante:
# son aquellos que se realizan utilizando informaci�n posterior sobre los predictores.
# Estos no son pron�sticos genuinos, pero son �tiles para estudiar 
#el comportamiento de los modelos de pron�stico.

#Ejemplo: producci�n de cerveza trimestral en Australia
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")

# 5.6.2) Pron�stico basado en escenarios:

fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange)
h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(uschange[, 1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))

# 5.6.3) Intervalos de predicci�n:

fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
                      newdata = data.frame(
                        Income = rep(mean(uschange[,"Income"]), h)))
fcast.up <- forecast(fit.cons,
                     newdata = data.frame(Income = rep(5, h)))

autoplot(uschange[, "Consumption"]) +
  ylab("% change in US consumption") +
  autolayer(fcast.ave, series = "Average increase",
            PI = TRUE) +
  autolayer(fcast.up, series = "Extreme increase",
            PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))


## 5.8 Regresi�n no lineal ##
# ************************* #

#Nota: Siguen siendo lineales en los parametros

# 5.8.1) Pronosticar con una tendencia no lineal:

#Ejemplo: tiempos de marat�n de Boston
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <- ts(pmax(0, t - t.break2), start = 1897)

fit.pw <- tslm(marathon ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(marathon ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)

autoplot(marathon) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.lin, series="Linear", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = " "))

#Los mejores pron�sticos parecen provenir de la tendencia lineal por partes, 
#mientras que la spline c�bica brinda el mejor ajuste a los datos hist�ricos, 
#pero los pron�sticos son pobres.

#Tambi�n hemos utilizado una transformaci�n logar�tmica ( lambda=0) 
#para manejar la heterocedasticidad.

marathon %>%
  splinef(lambda=0) %>% #spline c�bica
  autoplot()

#residuos:
marathon %>%
  splinef(lambda=0) %>%
  checkresiduals()

#Los residuos trazados muestran que este modelo ha capturado bien la tendencia, 
#aunque queda algo de heterocedasticidad. El amplio intervalo de predicci�n asociado 
#con los pron�sticos refleja la volatilidad observada en los tiempos ganadores hist�ricos.


## 5.9 Correlaci�n, causalidad y pron�stico ##
# ****************************************** #

# 5.9.1) La correlaci�n no es causalidad:

#Es importante no confundir la correlaci�n con la causalidad, 
#o la causalidad con el pron�stico.

#Una variable X puede ser �til para pronosticar una variable y, pero eso no significa que  
#X est� causando a y. 

#Es importante comprender que las correlaciones son �tiles para el pron�stico.

# Sin embargo, a menudo es posible un mejor modelo si se puede determinar un mecanismo causal. 

# 5.9.2) Predictores confundidos:

#Decimos que dos variables est�n confundidascuando sus efectos en la variable de pron�stico 
#no se pueden separar. Cualquier par de predictores correlacionados tendr� cierto nivel de 
#confusi�n, pero normalmente no los describir�amos como confundidos a menos que hubiera un 
#nivel relativamente alto de correlaci�n entre ellos.

#La confusi�n no es realmente un problema para pronosticar, ya que a�n podemos calcular 
#pron�sticos sin necesidad de separar los efectos de los predictores. Sin embargo, 
#se convierte en un problema con el pron�stico del escenario, ya que los escenarios deben 
#tener en cuenta las relaciones entre los predictores. Tambi�n es un problema si se requiere 
#alg�n an�lisis hist�rico de las contribuciones de varios predictores.

# 5.9.2) Multicolinealidad. (ya dominas esto xd)
