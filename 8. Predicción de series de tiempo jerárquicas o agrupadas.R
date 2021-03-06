#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 10 - Predicci�n de series de tiempo jer�rquicas o agrupadas  ####
#     ===============================================================     #


#Paquetes que se utilizar�
install.packages("ggplot2")
install.packages("fpp2")
install.packages("forecast")
install.packages("seasonal")
install.packages("hts")
install.packages("tidyverse")
install.packages("tibble")


library(forecast)
library(ggplot2)
library(fpp2)
library(seasonal)
library(hts)
library(tidyverse)
library(tibble)


set.seed(30)  #ponemos semilla


## 10.1: Regresi�n con errores ARIMA en R ##
# **************************************** #

#Ejemplo: jerarqu�a tur�stica australiana
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Para crear una serie de tiempo jer�rquica, utilizamos la funci�n hts() como se muestra en el 
#siguiente c�digo. La funci�n requiere dos entradas: la serie de tiempo de nivel inferior e 
#informaci�n sobre la estructura jer�rquica. visnightses una matriz de series de tiempo que 
#contiene las series de nivel inferior. Hay varias formas de ingresar la estructura de la 
#jerarqu�a. En este caso estamos usando el charactersargumento. Los primeros tres caracteres 
#de cada nombre de columna visnightscapturan las categor�as en el primer nivel de la 
#jerarqu�a (Estados). Los siguientes cinco caracteres capturan las categor�as de nivel 
#inferior (Zonas).

tourism.hts <- hts(visnights, characters = c(3, 5))
tourism.hts %>% aggts(levels=0:1) %>%
  autoplot(facet=TRUE) +
  xlab("Year") + ylab("millions") + ggtitle("Visitor nights")

#La gr�fica superior en la Figura muestra el n�mero total de noches de visitantes para toda 
#Australia, mientras que las gr�ficas a continuaci�n muestran los datos desglosados por estado.
#Estos revelan din�micas diversas y ricas a nivel nacional agregado, y el primer nivel de 
#desagregaci�n para cada estado. La funci�n aggts() extrae series temporales de un objeto hts 
#para cualquier nivel de agregaci�n.

cols <- sample(scales::hue_pal(h=c(15,375),
                               c=100,l=65,h.start=0,direction = 1)(NCOL(visnights)))
as_tibble(visnights) %>%
  gather(Zone) %>%
  mutate(Date = rep(time(visnights), NCOL(visnights)),
         State = str_sub(Zone,1,3)) %>%
  ggplot(aes(x=Date, y=value, group=Zone, colour=Zone)) +
  geom_line() +
  facet_grid(State~., scales="free_y") +
  xlab("Year") + ylab("millions") +
  ggtitle("Visitor nights by Zone") +
  scale_colour_manual(values = cols)

#Las gr�ficas en la Figura muestran las series temporales de nivel inferior, es decir, las 
#noches de visitante para cada zona. Estos nos ayudan a visualizar la din�mica individual 
#diversa dentro de cada zona, y ayudan a identificar series de tiempo �nicas e importantes.


## 10.2: Series temporales agrupadas ##
# *********************************** #

#Las series de tiempo agrupadas implican estructuras de agregaci�n m�s generales que las series 
#de tiempo jer�rquicas. Con series de tiempo agrupadas, la estructura no se desagrega 
#naturalmente de una manera jer�rquica �nica y, a menudo, los factores de desagregaci�n est�n 
#anidados y cruzados.

#Ejemplo: poblaci�n carcelaria australiana
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

prison.gts <- gts(prison/1e3, characters = c(3,1,9),
                  gnames = c("State", "Gender", "Legal",
                             "State*Gender", "State*Legal",
                             "Gender*Legal"))

#Una forma de trazar los grupos principales es la siguiente.
prison.gts %>% aggts(level=0:3) %>% autoplot()

#Pero con un poco m�s de trabajo, podemos construir la siguiente Figura:
p1 <- prison.gts %>% aggts(level=0) %>%
  autoplot() + ggtitle("Australian prison population") +
  xlab("Year") + ylab("Total number of prisoners ('000)")
groups <- aggts(prison.gts, level=1:3)
cols <- sample(scales::hue_pal(h=c(15,375),
                               c=100,l=65,h.start=0,direction = 1)(NCOL(groups)))
p2 <- as_tibble(groups) %>%
  gather(Series) %>%
  mutate(Date = rep(time(groups), NCOL(groups)),
         Group = str_extract(Series, "([A-Za-z ]*)")) %>%
  ggplot(aes(x=Date, y=value, group=Series, colour=Series)) +
  geom_line() +
  xlab("Year") + ylab("Number of prisoners ('000)") +
  scale_colour_manual(values = cols) +
  facet_grid(.~Group, scales="free_y") +
  scale_x_continuous(breaks=seq(2006,2016,by=2)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
gridExtra::grid.arrange(p1, p2, ncol=1)

#Se pueden obtener gr�ficas de otras combinaciones de grupos de manera similar.


## 10.3: El enfoque de abajo hacia arriba o ascendente ##
# ***************************************************** #

#Un m�todo simple para generar pron�sticos coherentes es el enfoque ascendente. 
#Este enfoque implica primero generar pron�sticos para cada serie en el nivel inferior, 
#y luego sumarlos para producir pron�sticos para todas las series en la estructura.

#10.3.1) El enfoque de abajo hacia arriba

#Por ejemplo, supongamos que queremos pron�sticos de abajo hacia arriba utilizando modelos 
#ARIMA aplicados a los datos de la prisi�n. Entonces usar�amos
forecast(prison.gts, method="bu", fmethod="arima")
#que aplicar� la funci�n auto.arima() a cada serie de nivel inferior en nuestra colecci�n de 
#series de tiempo. Del mismo modo, los modelos ETS se usar�an si fmethod="ets"se usara.


## 10.4: Enfoques de arriba hacia abajo o descendente ##
# **************************************************** #

#Los enfoques descendentes solo funcionan con estructuras de agregaci�n estrictamente 
#jer�rquicas, y no con estructuras agrupadas. Implican primero generar pron�sticos para la 
#serie Total y_T , y luego desglosarlos en la jerarqu�a.

#Los dos enfoques descendentes m�s comunes especifican proporciones de desagregaci�n basadas
#en las proporciones hist�ricas de los datos. 

#10.4.1) Proporciones hist�ricas medias:

#Este enfoque se implementa en la funci�n forecast() mediante la configuraci�n 
#method="tdgsa", donde tdgsa significa "m�todo de Gross-Sohl de arriba hacia abajo A".

#10.4.2) Proporciones de los promedios hist�ricos.:

#Este enfoque se implementa en la forecast()funci�n estableciendo method="tdgsf", 
#donde tdgsf significa "m�todo de Gross-Sohl de arriba hacia abajo F".

#10.4.3) Proporciones de pron�stico:

#Debido a que las proporciones hist�ricas utilizadas para la desagregaci�n no tienen en cuenta
#c�mo esas proporciones pueden cambiar con el tiempo, los enfoques descendentes basados en 
#proporciones hist�ricas tienden a producir pron�sticos menos precisos en los niveles 
#inferiores de la jerarqu�a que los enfoques ascendentes.

#Este enfoque se implementa en la funci�n forecast() mediante la configuraci�n method="tdfp",
#donde tdfp significa "proporciones de pron�stico descendentes".


## 10.5: Enfoque intermedio ##
# ************************** #

#El enfoque intermedio combina enfoques ascendentes y descendentes. 

#Este enfoque se implementa en la funci�n forecast() estableciendo method="mo" y especificando
#el nivel medio apropiado mediante el argumento level. Para la desagregaci�n de arriba hacia 
#abajo por debajo del nivel medio, se utiliza el m�todo de proporciones de pron�stico de 
#arriba hacia abajo.


## 10.7: El enfoque �ptimo de reconciliaci�n ##
# ******************************************* #

#Se producir� una conciliaci�n de pron�stico �ptima si podemos encontrar la matriz G 
#que minimiza el error de pron�stico del conjunto de pron�sticos coherentes.

#Ejemplo: pron�stico de la poblaci�n carcelaria australiana:
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Calculamos los pron�sticos para la poblaci�n carcelaria australiana, Usando los argumentos 
#predeterminados para la funci�n forecast(), calculamos pron�sticos coherentes mediante el 
#enfoque de conciliaci�n �ptimo con el estimador WLS usando el escalado de varianza.
prisonfc <- forecast(prison.gts)

#Para obtener pron�sticos para cada nivel de agregaci�n, podemos usar la funci�n aggts().
fcsts <- aggts(prisonfc, levels=0:3)

#Se obtiene una trama simple usando
groups <- aggts(prison.gts, levels=0:3)
autoplot(fcsts) + autolayer(groups)

#Una trama m�s agradable est� disponible usando el siguiente c�digo:
prisonfc <- ts(rbind(groups, fcsts),
               start=start(groups), frequency=4)
p1 <- autoplot(prisonfc[,"Total"]) +
  ggtitle("Australian prison population") +
  xlab("Year") + ylab("Total number of prisoners ('000)") +
  geom_vline(xintercept=2017)
cols <- sample(scales::hue_pal(h=c(15,375),
                               c=100,l=65,h.start=0,direction = 1)(NCOL(groups)))
p2 <- as_tibble(prisonfc[,-1]) %>%
  gather(Series) %>%
  mutate(Date = rep(time(prisonfc), NCOL(prisonfc)-1),
         Group = str_extract(Series, "([A-Za-z ]*)")) %>%
  ggplot(aes(x=Date, y=value, group=Series, colour=Series)) +
  geom_line() +
  xlab("Year") + ylab("Number of prisoners ('000)") +
  scale_colour_manual(values = cols) +
  facet_grid(. ~ Group, scales="free_y") +
  scale_x_continuous(breaks=seq(2006,2018,by=2)) +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  geom_vline(xintercept=2017)
gridExtra::grid.arrange(p1, p2, ncol=1)

#El accuracy()comando es �til para evaluar la precisi�n del pron�stico en estructuras 
#jer�rquicas o agrupadas. 
