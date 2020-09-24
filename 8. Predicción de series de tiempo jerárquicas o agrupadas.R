#========================================#
#= Forecasting: Principles and Practice =#
#==   Jose Rodney Menezes De la Cruz   ==#
#========================================#


####  Cap 10 - Predicción de series de tiempo jerárquicas o agrupadas  ####
#     ===============================================================     #


#Paquetes que se utilizará
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


## 10.1: Regresión con errores ARIMA en R ##
# **************************************** #

#Ejemplo: jerarquía turística australiana
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Para crear una serie de tiempo jerárquica, utilizamos la función hts() como se muestra en el 
#siguiente código. La función requiere dos entradas: la serie de tiempo de nivel inferior e 
#información sobre la estructura jerárquica. visnightses una matriz de series de tiempo que 
#contiene las series de nivel inferior. Hay varias formas de ingresar la estructura de la 
#jerarquía. En este caso estamos usando el charactersargumento. Los primeros tres caracteres 
#de cada nombre de columna visnightscapturan las categorías en el primer nivel de la 
#jerarquía (Estados). Los siguientes cinco caracteres capturan las categorías de nivel 
#inferior (Zonas).

tourism.hts <- hts(visnights, characters = c(3, 5))
tourism.hts %>% aggts(levels=0:1) %>%
  autoplot(facet=TRUE) +
  xlab("Year") + ylab("millions") + ggtitle("Visitor nights")

#La gráfica superior en la Figura muestra el número total de noches de visitantes para toda 
#Australia, mientras que las gráficas a continuación muestran los datos desglosados por estado.
#Estos revelan dinámicas diversas y ricas a nivel nacional agregado, y el primer nivel de 
#desagregación para cada estado. La función aggts() extrae series temporales de un objeto hts 
#para cualquier nivel de agregación.

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

#Las gráficas en la Figura muestran las series temporales de nivel inferior, es decir, las 
#noches de visitante para cada zona. Estos nos ayudan a visualizar la dinámica individual 
#diversa dentro de cada zona, y ayudan a identificar series de tiempo únicas e importantes.


## 10.2: Series temporales agrupadas ##
# *********************************** #

#Las series de tiempo agrupadas implican estructuras de agregación más generales que las series 
#de tiempo jerárquicas. Con series de tiempo agrupadas, la estructura no se desagrega 
#naturalmente de una manera jerárquica única y, a menudo, los factores de desagregación están 
#anidados y cruzados.

#Ejemplo: población carcelaria australiana
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

prison.gts <- gts(prison/1e3, characters = c(3,1,9),
                  gnames = c("State", "Gender", "Legal",
                             "State*Gender", "State*Legal",
                             "Gender*Legal"))

#Una forma de trazar los grupos principales es la siguiente.
prison.gts %>% aggts(level=0:3) %>% autoplot()

#Pero con un poco más de trabajo, podemos construir la siguiente Figura:
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

#Se pueden obtener gráficas de otras combinaciones de grupos de manera similar.


## 10.3: El enfoque de abajo hacia arriba o ascendente ##
# ***************************************************** #

#Un método simple para generar pronósticos coherentes es el enfoque ascendente. 
#Este enfoque implica primero generar pronósticos para cada serie en el nivel inferior, 
#y luego sumarlos para producir pronósticos para todas las series en la estructura.

#10.3.1) El enfoque de abajo hacia arriba

#Por ejemplo, supongamos que queremos pronósticos de abajo hacia arriba utilizando modelos 
#ARIMA aplicados a los datos de la prisión. Entonces usaríamos
forecast(prison.gts, method="bu", fmethod="arima")
#que aplicará la función auto.arima() a cada serie de nivel inferior en nuestra colección de 
#series de tiempo. Del mismo modo, los modelos ETS se usarían si fmethod="ets"se usara.


## 10.4: Enfoques de arriba hacia abajo o descendente ##
# **************************************************** #

#Los enfoques descendentes solo funcionan con estructuras de agregación estrictamente 
#jerárquicas, y no con estructuras agrupadas. Implican primero generar pronósticos para la 
#serie Total y_T , y luego desglosarlos en la jerarquía.

#Los dos enfoques descendentes más comunes especifican proporciones de desagregación basadas
#en las proporciones históricas de los datos. 

#10.4.1) Proporciones históricas medias:

#Este enfoque se implementa en la función forecast() mediante la configuración 
#method="tdgsa", donde tdgsa significa "método de Gross-Sohl de arriba hacia abajo A".

#10.4.2) Proporciones de los promedios históricos.:

#Este enfoque se implementa en la forecast()función estableciendo method="tdgsf", 
#donde tdgsf significa "método de Gross-Sohl de arriba hacia abajo F".

#10.4.3) Proporciones de pronóstico:

#Debido a que las proporciones históricas utilizadas para la desagregación no tienen en cuenta
#cómo esas proporciones pueden cambiar con el tiempo, los enfoques descendentes basados en 
#proporciones históricas tienden a producir pronósticos menos precisos en los niveles 
#inferiores de la jerarquía que los enfoques ascendentes.

#Este enfoque se implementa en la función forecast() mediante la configuración method="tdfp",
#donde tdfp significa "proporciones de pronóstico descendentes".


## 10.5: Enfoque intermedio ##
# ************************** #

#El enfoque intermedio combina enfoques ascendentes y descendentes. 

#Este enfoque se implementa en la función forecast() estableciendo method="mo" y especificando
#el nivel medio apropiado mediante el argumento level. Para la desagregación de arriba hacia 
#abajo por debajo del nivel medio, se utiliza el método de proporciones de pronóstico de 
#arriba hacia abajo.


## 10.7: El enfoque óptimo de reconciliación ##
# ******************************************* #

#Se producirá una conciliación de pronóstico óptima si podemos encontrar la matriz G 
#que minimiza el error de pronóstico del conjunto de pronósticos coherentes.

#Ejemplo: pronóstico de la población carcelaria australiana:
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,

#Calculamos los pronósticos para la población carcelaria australiana, Usando los argumentos 
#predeterminados para la función forecast(), calculamos pronósticos coherentes mediante el 
#enfoque de conciliación óptimo con el estimador WLS usando el escalado de varianza.
prisonfc <- forecast(prison.gts)

#Para obtener pronósticos para cada nivel de agregación, podemos usar la función aggts().
fcsts <- aggts(prisonfc, levels=0:3)

#Se obtiene una trama simple usando
groups <- aggts(prison.gts, levels=0:3)
autoplot(fcsts) + autolayer(groups)

#Una trama más agradable está disponible usando el siguiente código:
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

#El accuracy()comando es útil para evaluar la precisión del pronóstico en estructuras 
#jerárquicas o agrupadas. 
