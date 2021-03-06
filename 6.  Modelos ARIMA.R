#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 8 - Modelos ARIMA          ####
#     =============================     #


#Paquetes que se utilizar�
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("seasonal")


library(forecast)
library(ggplot2)
library(fpp2)
library(seasonal)
library(urca)


set.seed(30)  #ponemos semilla



## 8.1: Estacionariedad y diferenciaci�n ##
# *************************************** #

# 8.1.1) Diferenciaci�n:

Box.test(diff(goog200), lag=10, type="Ljung-Box")

# El test de Box-Ljung tiene un valor p de 0.355 (para h=10) Esto sugiere que el cambio diario
# en el precio de las acciones de Google es esencialmente una cantidad aleatoria que no est� 
#correlacionada con la de los d�as anteriores.

#8.1.2) Modelo de caminata aleatoria:

#8.1.3) Diferenciaci�n de segundo orden:

#8.1.4) Diferenciaci�n estacional:
# Una diferencia estacional es la diferencia entre una observaci�n 
# y la observaci�n previa de la misma estaci�n.

cbind("Sales ($million)" = a10,
      "Monthly log sales" = log(a10),
      "Annual change in log sales" = diff(log(a10),12)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Antidiabetic drug sales")

# A veces es necesario tomar una diferencia estacional y 
# una primera diferencia para obtener datos estacionarios.

cbind("Billion kWh" = usmelec,
      "Logs" = log(usmelec),
      "Seasonally\n differenced logs" =
        diff(log(usmelec),12),
      "Doubly\n differenced logs" =
        diff(diff(log(usmelec),12),1)) %>%
  autoplot(facets=TRUE) +
  xlab("Year") + ylab("") +
  ggtitle("Monthly US net electricity generation")

#8.1.5) Pruebas de ra�z unitaria: 

#En nuestro an�lisis, utilizamos la prueba Kwiatkowski-Phillips-Schmidt-Shin (KPSS)

#En esta prueba, la hip�tesis nula es que los datos son estacionarios, y buscamos evidencia 
#de que la hip�tesis nula sea falsa. En consecuencia, los valores p peque�os 
#(p. Ej., Menos de 0.05) sugieren que se requiere diferenciaci�n. La prueba se puede calcular 
#utilizando la ur.kpss()funci�n del paquete urca.

goog %>% ur.kpss() %>% summary()

#El estad�stico de prueba es mucho mayor que el valor cr�tico del 1%, lo que indica que 
#la hip�tesis nula es rechazada. Es decir, los datos no son estacionarios. 

#Podemos diferenciar los datos y aplicar la prueba nuevamente:

goog %>% diff() %>% ur.kpss() %>% summary()

#Esta vez, el estad�stico de prueba es peque�o y est� dentro del rango esperado. Entonces 
#podemos concluir que los datos diferenciados son estacionarios.

#La funci�n ndiffs() lleva a cabo este proceso de usar una secuencia de pruebas KPSS para 
#determinar el n�mero apropiado de primeras diferencias.

ndiffs(goog)

#Una funci�n similar para determinar si se requiere diferenciaci�n estacional es la nsdiffs()

usmelec %>% log() %>% nsdiffs()

usmelec %>% log() %>% diff(lag=12) %>% ndiffs()

#Debido a que nsdiffs() devuelve 1 (lo que indica que se requiere una diferencia estacional), 
#aplicamos la funci�n ndiffs() a los datos diferenciados estacionalmente. Estas funciones 
#sugieren que deber�amos hacer una diferencia estacional y una primera diferencia.


## 8.2: Operador de rezago ##
# ************************* #


## 8.3: Modelos autorregresivos (AR) ##
# *********************************** #


## 8.4: Modelos de media m�vil (MA)   ##
# *********************************** #


## 8.5: Modelos ARIMA no estacionales ARIMA(p,r,q)   ##
# *************************************************** #

#Ejemlo:Gasto de consumo de EE.UU.
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

autoplot(uschange[,"Consumption"]) +
  xlab("Year") + ylab("Quarterly percentage change")

#El siguiente c�digo R se utiliz� para seleccionar un modelo autom�ticamente.

fit <- auto.arima(uschange[,"Consumption"], seasonal=FALSE)
fit

#Este es un modelo ARIMA (1,0,3): donde y es ruido blanco 
#con una desviaci�n est�ndar de 0.592 = ???0.350

#Pronostico:
fit %>% forecast(h=10) %>% autoplot(include=80)


#8.5.1) Comprender los modelos ARIMA

#8.5.2) Parcelas ACF y PACF

#Por lo general, no es posible saber, simplemente a partir de un diagrama de tiempo, 
#qu� valores de y son apropiados para los datos. Sin embargo, a veces es posible usar 
#el gr�fico ACF, y el gr�fico PACF estrechamente relacionado, para determinar los valores 
#apropiados para y

#grafico autocorrelacion
ggAcf(uschange[,"Consumption"])

#grafico autocorrelacion parcial
ggPacf(uschange[,"Consumption"])

#El patr�n en los primeros tres picos es lo que esperar�amos de un ARIMA (3,0,0), ya que el 
#PACF tiende a disminuir. Entonces, en este caso, el ACF y el PACF nos llevan a pensar que 
#un modelo ARIMA (3,0,0) podr�a ser apropiado.

(fit2 <- Arima(uschange[,"Consumption"], order=c(3,0,0)))

#Este modelo es en realidad un poco mejor que el modelo identificado por auto.arima()
# La funci�n auto.arima() no encontr� este modelo porque no considera tod
#os los modelos posibles en su b�squeda. Puedes hacer que trabaje m�s duro usando 
#los argumentos stepwise=FALSE y approximation=FALSE:

(fit3 <- auto.arima(uschange[,"Consumption"], seasonal=FALSE,
                    stepwise=FALSE, approximation=FALSE))

#Esta vez, auto.arima()ha encontrado el mismo modelo que supusimos de las parcelas ACF y PACF. 

#Tambi�n usamos el argumento seasonal=FALSEpara evitar que busque modelos ARIMA estacionales


## 8.7: Modelado ARIMA en R   ##
# **************************** #

#8.7.1) Como funciona auto.arima():

#8.7.2) Procedimiento de modelado:

#Ejemplo: pedidos de equipos el�ctricos ajustados estacionalmente
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#vemos los datos, su estacionariedad y observaciones inusuales 
elecequip %>% stl(s.window='periodic') %>% seasadj() -> eeadj
autoplot(eeadj)

#vemos ACF y PACF en primera diferenca
eeadj %>% diff() %>% ggtsdisplay(main="")

#estimamos el modelo
(fit <- Arima(eeadj, order=c(3,1,1)))

#chequeamos los residuos
checkresiduals(fit)

#pronosticamos el modelo elegido:
autoplot(forecast(fit))

#8.7.3) Comprender las constantes en R:

#la inclusi�n de una constante en un modelo ARIMA no estacionario es equivalente a inducir 
#una tendencia polin�mica de orden en la funci�n de pron�stico. (Si se omite la constante, 
#la funci�n de pron�stico incluye una tendencia polin�mica de orden d-1).

#8.7.4) Trazando las ra�ces Unitarias:

autoplot(fit)

#la grafica de la izquierda muestra las raicez de los polinomios y de la derecha coresponde a
#la raiz de ??. Todas las raicez deben estar dentro del circulo unitario. Cualquier ra�z 
#cercana al c�rculo unitario puede ser num�ricamente inestable, y el modelo correspondiente 
#no ser� bueno para pronosticar. 


## 8.8: Pron�sticos ##
# ****************** #

## 8.9: Modelos ARIMA estacionales SARIMA ##
# **************************************** #

#El procedimiento de modelado es casi el mismo que para los datos no estacionales, 
#excepto que necesitamos seleccionar t�rminos AR y MA estacionales, as� como los componentes 
#no estacionales del modelo. El proceso se ilustra mejor a trav�s de ejemplos.

#Ejemplo 1: comercio minorista trimestral europeo
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#graficamos los datos y vemos posibles observaciones extra�as
autoplot(euretail) + ylab("Retail index") + xlab("Year")

#1era diferencia (asi tambien se desestacionaliza)
euretail %>% diff(lag=4) %>% ggtsdisplay()

#2da diferencia
euretail %>% diff(lag=4) %>% diff() %>% ggtsdisplay()

#estimamos el modelo que seleccionesmos
euretail %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()

#estimamos otro modelo que selecionemos una vez reparado problemas
fit3 <- Arima(euretail, order=c(0,1,3), seasonal=c(0,1,1))

#analizamos los residuales
checkresiduals(fit3)

#pronosticamos
fit3 %>% forecast(h=12) %>% autoplot()

#Nota: Podr�amos haber utilizado auto.arima() para que haga la mayor parte de este trabajo 
#por nosotros. Hubiera dado el mismo resultado.

auto.arima(euretail)


#Ejemlo 2: Ejemplo: venta de medicamentos con corticosteroides en Australia
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#graficamos los datos y vemos posibles observaciones extra�as
lh02 <- log(h02)
cbind("H02 sales (million scripts)" = h02,
      "Log H02 sales"=lh02) %>%
  autoplot(facets=TRUE) + xlab("Year") + ylab("")

#1era diferencia (asi tambien se desestacionaliza)
lh02 %>% diff(lag=12) %>%
  ggtsdisplay(xlab="Year",
              main="Seasonally differenced H02 scripts")

#estimamos el modelo que seleccionesmos
(fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2),
              lambda=0))

#analizamos los residuales
checkresiduals(fit, lag=36)

#pronosticamos
h02 %>%
  Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")


## 8.10: ARIMA vs ETS ##
# ******************** #

#Resumen: Siempre usa ARIMA we, ARIMA es mejor y ademas eres economista.

