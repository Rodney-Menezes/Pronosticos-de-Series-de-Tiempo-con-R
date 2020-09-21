#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 5 - Modelos de regresión de series temporales  ####
#     =================================================     #


#Paquetes que se utilizará
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

# 5.1.1) Regresión lineal simple

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

# 5.2.1) Regresión lineal múltiple

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


# 5.2.2) Regresión lineal múltiple
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


## 5.3: Evaluación del modelo de regresión. ##
# ****************************************** #

#Después de seleccionar las variables de regresión y ajustar un modelo de regresión, es 
#necesario trazar los residuos para verificar que se hayan cumplido los supuestos del modelo.

#Propiedades de "e":
#-------------------
#mean = 0
#corr(e,x)=0
# no autocorr serial: corr(et,et-1) != 1
# e ~ N -> no necesario para pronostico pero da mejores intervalos de predicción.

# 5.3.1) Gráfico ACF de residuos (para ver autocorr):

#Los pronósticos de un modelo con errores autocorrelacionados aún no son imparciales, 
#por lo que no son "incorrectos", pero generalmente tendrán intervalos de predicción más 
#largos de lo necesario. Por lo tanto, siempre debemos mirar un gráfico ACF de los residuos.

# Otra prueba útil de autocorrelación en los residuos diseñados para tener en cuenta el modelo
#de regresión es la prueba de Breusch-Godfrey , también conocida como 
#prueba LM (multiplicador de Lagrange) para la correlación en serie. Se utiliza para probar 
#la hipótesis conjunta de que no hay autocorrelación en los residuos hasta un cierto orden 
#especificado. Un valor p pequeño indica que hay una autocorrelación significativa restante en
#los residuos. La prueba Breusch-Godfrey.

# 5.3.2) Histograma de residuos:
#verifica si los residuos se distribuyen normalmente.

# 5.3.3) con la función checkresiduals(), podemos obtener todos los diagnósticos 
#residuales útiles mencionados anteriormente: (grafica residuos,ACF,histograma y prueba B-G)

checkresiduals(fit.consMR)

# 5.3.4) Gráficos residuales contra predictores:

#Esperaríamos que los residuos se dispersen al azar sin mostrar ningún patrón sistemático. 
#Una manera simple y rápida de verificar esto es examinar diagramas de dispersión de los 
#residuos contra cada una de las variables predictoras. 
#Si estos diagramas de dispersión muestran un patrón, entonces la relación 
#puede ser no lineal y el modelo deberá modificarse en consecuencia.
#También es necesario trazar los residuos contra cualquier predictor que no esté en el modelo.
#Si alguno de estos muestra un patrón, entonces el predictor correspondiente puede necesitar 
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

# 5.3.5) Gráficos residuales contra valores ajustados:

#Una gráfica de los residuos contra los valores ajustados tampoco debe mostrar ningún patrón. 
#Si se observa un patrón, puede haber "heterocedasticidad" en los errores, 
#lo que significa que la varianza de los residuos puede no ser constante.
#Si se produce este problema, puede ser necesaria una transformación de la 
#variable de pronóstico, como un logaritmo o una raíz cuadrada.

cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

# 5.3.6) Observaciones atípicas e influyentes:

#Las observaciones que toman valores extremos en comparación con la mayoría de los datos 
#se llaman valores atípicos . Las observaciones que tienen una gran influencia en los 
#coeficientes estimados de un modelo de regresión se denominan observaciones influyentes. 
#Por lo general, las observaciones influyentes también son valores atípicos 
#que son extremos en la dirección 

#Una fuente de valores atípicos es la entrada de datos incorrecta.  
#Si se identifica dicha observación y se ha registrado incorrectamente, 
#debe corregirse o eliminarse de la muestra de inmediato.

#Los valores atípicos también ocurren cuando algunas observaciones son simplemente diferentes.
#En este caso, puede que no sea aconsejable eliminar estas observaciones. 
#Si se ha identificado una observación como un valor atípico probable, 
#es importante estudiarla y analizar las posibles razones detrás de ella. 
#La decisión de eliminar o retener una observación puede ser desafiante 
#(especialmente cuando los valores atípicos son observaciones influyentes). 
#Es aconsejable informar los resultados con y sin la eliminación de tales observaciones.

# 5.3.7) Regresión espuria:

#La mayoría de las veces, los datos de series temporales son "no estacionarios"
#La regresión de series de tiempo no estacionarias puede conducir a regresiones espurias. 
#la alta autocorrelación residual pueden ser signos de regresión espuria.

#Ejemplo:
#,,,,,,,,

aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)

checkresiduals(fit)


## 5.4: Evaluación del modelo de regresión. ##
# ****************************************** #

#Tendencia:
#Se puede especificar una variable de tendencia en la función tslm() 
#utilizando el trend predictor.

#Variables ficticias : 
#tslm() manejará automáticamente este caso si especifica una variable de factor como predictor.
#Por lo general, no es necesario crear manualmente las variables ficticias correspondientes.

#Variables ficticias estacionales: (variables ficticias para multiples categorias)
#La función tslm() manejará automáticamente esta situación si especifica el predictor season.

#Ejemplo: producción de cerveza trimestral en Australia
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

#Producción real de cerveza trazada contra la producción de cerveza prevista.
cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

# 5.4.1) Variables de intervención:

#A menudo es necesario modelar intervenciones que pueden haber afectado la variable a 
#pronosticar.

#Cuando el efecto dura solo un período, usamos una variable de "pico". Esta es una variable 
#ficticia que toma el valor uno en el período de la intervención y cero en otro lugar. 
#Una variable de pico es equivalente a una variable ficticia para manejar un valor atípico.

#Otras intervenciones tienen un efecto inmediato y permanente. Si una intervención causa un 
#cambio de nivel (es decir, el valor de la serie cambia repentina y permanentemente desde el 
#momento de la intervención), entonces usamos una variable de "paso". 
#Una variable de paso toma el valor cero antes de la intervención y uno desde 
#el momento de la intervención en adelante.

#Otra forma de efecto permanente es un cambio de pendiente. Aquí la intervención se maneja 
#utilizando una tendencia lineal por partes; Una tendencia que se dobla en el momento de la 
#intervención y, por lo tanto, no es lineal.

# 5.4.2) Días de negociación:
#Para datos mensuales o trimestrales, la función bizdays() calculará el número de días 
#de negociación en cada período.

# 5.4.2) Retrasos distribuidos:
#Es común requerir que los coeficientes disminuyan a medida que aumenta el retraso,

# 5.4.3) Pascua de Resurrección:
#La Pascua difiere de la mayoría de los días festivos porque no se celebra en la misma 
#fecha cada año, y su efecto puede durar varios días.La función easter() calculará
#la variable ficticia para usted.

# 5.4.4) series de Fourier:
#Una alternativa al uso de variables ficticias estacionales, especialmente durante largos 
#períodos estacionales, es usar términos de Fourier.Jean-Baptiste Fourier fue un matemático
#francés, nacido en la década de 1700, que demostró que una serie de términos seno y coseno
#de las frecuencias correctas puede aproximarse a cualquier función periódica. 
#Podemos usarlos para patrones estacionales.

fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)

#Si solo se utilizan los dos primeros términos de Fourier (x1,t y x2,t) el patrón estacional 
#seguirá una onda sinusoidal simple. Un modelo de regresión que contiene términos de Fourier 
#a menudo se llama regresión armónica porque los términos sucesivos de Fourier 
#representan armónicos de los primeros dos términos de Fourier.


## 5.5 Seleccionar predictores. ##
# ****************************** #

# un enfoque común que también es inválido es hacer una regresión lineal múltiple en todos los
#predictores y no tener en cuenta todas las variables cuyos valores p son mayores que 0.05. 
#Para empezar, la significación estadística no siempre indica un valor predictivo. 
#Incluso si el pronóstico no es el objetivo, esta no es una buena estrategia porque los 
#valores p pueden ser engañosos cuando dos o más predictores están correlacionados entre sí.

## 5.5.1)*medidas de precisión predictiva:
#En cambio, utilizaremos una medida de precisión predictiva. Cinco de estas medidas 
#se introducen en esta sección. 

# 5.5.1.1) Se pueden calcular utilizando la función CV()
CV(fit.consMR) # este obtiene las 5 medidas

# 5.5.1.2) R ajustado 

# 5.5.1.3) Validación cruzada:
#hay metodos mas rapidos para calcular el CV

# 5.5.1.4) Criterio de información de Akaike
#La idea aquí es penalizar el ajuste del modelo (SSE) con el número de parámetros que deben estimarse.

# 5.5.1.5) Criterio de información bayesiano de Schwarz

# 5.5.2) Mejor regresión de subconjunto:
#Siempre que sea posible, se deben ajustar todos los modelos de regresión potenciales 
#y se debe seleccionar el mejor modelo en función de una de las medidas discutidas. 
#Esto se conoce como regresión de "mejores subconjuntos" 
#o regresión de "todos los subconjuntos posibles".

# 5.5.3) Regresión gradual:

# 5.5.3.1) regresión gradual hacia atrás:
# a) Comience con el modelo que contiene todos los predictores potenciales.
# b) Eliminar un predictor a la vez. Mantenga el modelo si mejora la medida de la precisión 
#    predictiva.
#    Iterar hasta que no haya más mejoras.

#Si el número de predictores potenciales es demasiado grande, entonces la regresión por 
#pasos hacia atrás no funcionará y en su lugar se puede usar la regresión por pasos hacia 
#adelante .

# 5.5.3.2) regresión por pasos hacia adelante:
#a) Este procedimiento comienza con un modelo que incluye solo la intercepción. 
#b) Los predictores se agregan uno a la vez, y el que más mejora la medida de la precisión 
#   predictiva se conserva en el modelo. 
#c) El procedimiento se repite hasta que no se pueda lograr una mejora adicional.

#Alternativamente para la dirección hacia atrás o hacia adelante, 
#un modelo inicial puede ser uno que incluya un subconjunto de predictores potenciales. 
#En este caso, se debe incluir un paso adicional. Para el procedimiento hacia atrás también 
#deberíamos considerar agregar un predictor con cada paso, y para el procedimiento 
#hacia adelante también deberíamos considerar descartar un predictor con cada paso. 
#Estos se denominan procedimientos híbridos .


## 5.6 Pronósticos con regresión ##
# ******************************* #

# 5.6.1) Pronósticos ex ante versus ex post:

#5.6.1.1) Los pronósticos ex ante:
#son aquellos que se realizan utilizando solo la información disponible de antemano.
#Estos son pronósticos genuinos, hechos de antemano usando cualquier 
#información disponible en ese momento.

#5.6.1.1) Los pronósticos ex ante:
# son aquellos que se realizan utilizando información posterior sobre los predictores.
# Estos no son pronósticos genuinos, pero son útiles para estudiar 
#el comportamiento de los modelos de pronóstico.

#Ejemplo: producción de cerveza trimestral en Australia
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")

# 5.6.2) Pronóstico basado en escenarios:

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

# 5.6.3) Intervalos de predicción:

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


## 5.8 Regresión no lineal ##
# ************************* #

#Nota: Siguen siendo lineales en los parametros

# 5.8.1) Pronosticar con una tendencia no lineal:

#Ejemplo: tiempos de maratón de Boston
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

#Los mejores pronósticos parecen provenir de la tendencia lineal por partes, 
#mientras que la spline cúbica brinda el mejor ajuste a los datos históricos, 
#pero los pronósticos son pobres.

#También hemos utilizado una transformación logarítmica ( lambda=0) 
#para manejar la heterocedasticidad.

marathon %>%
  splinef(lambda=0) %>% #spline cúbica
  autoplot()

#residuos:
marathon %>%
  splinef(lambda=0) %>%
  checkresiduals()

#Los residuos trazados muestran que este modelo ha capturado bien la tendencia, 
#aunque queda algo de heterocedasticidad. El amplio intervalo de predicción asociado 
#con los pronósticos refleja la volatilidad observada en los tiempos ganadores históricos.


## 5.9 Correlación, causalidad y pronóstico ##
# ****************************************** #

# 5.9.1) La correlación no es causalidad:

#Es importante no confundir la correlación con la causalidad, 
#o la causalidad con el pronóstico.

#Una variable X puede ser útil para pronosticar una variable y, pero eso no significa que  
#X esté causando a y. 

#Es importante comprender que las correlaciones son útiles para el pronóstico.

# Sin embargo, a menudo es posible un mejor modelo si se puede determinar un mecanismo causal. 

# 5.9.2) Predictores confundidos:

#Decimos que dos variables están confundidascuando sus efectos en la variable de pronóstico 
#no se pueden separar. Cualquier par de predictores correlacionados tendrá cierto nivel de 
#confusión, pero normalmente no los describiríamos como confundidos a menos que hubiera un 
#nivel relativamente alto de correlación entre ellos.

#La confusión no es realmente un problema para pronosticar, ya que aún podemos calcular 
#pronósticos sin necesidad de separar los efectos de los predictores. Sin embargo, 
#se convierte en un problema con el pronóstico del escenario, ya que los escenarios deben 
#tener en cuenta las relaciones entre los predictores. También es un problema si se requiere 
#algún análisis histórico de las contribuciones de varios predictores.

# 5.9.2) Multicolinealidad. (ya dominas esto xd)
