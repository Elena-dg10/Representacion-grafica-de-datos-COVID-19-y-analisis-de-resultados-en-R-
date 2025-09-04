#Descargo la librería ggplot para poder realizar gráficos
install.packages('tidyverse')
# install.packages("ggplot2")
# install.packages('dplyr')
# install.packages('readr')
# install.packages("csvread")
# 
# #Cargo la librería ggplot2
# library(ggplot2)
library(tidyverse)

#Importo mi base de datos de Covid19
estudio1<-read.csv("/Users/emmettdiez/Desktop/Covid19.csv", na.strings = "", fileEncoding = "UTF-8-BOM") %>%   mutate(date=as.Date(as.character(dateRep),"%d/%m/%Y"),month=as.factor(month), year=as.factor(year))

#EJERCICIO 1

#Filtro los datos por rango, indicando mínimo y máximo de casos
estudio1filter <- estudio1 %>%
  filter(estudio1$cases>0 & estudio1$cases<=1000)

#Hago el histograma para la distribución global de Covid19
ggplot(estudio1filter, aes(x=cases))+
  geom_histogram(fill="skyblue", color="black", binwidth = 100)+
  labs(title="Distribución casos Covid globales",subtitle = "Histograma de casos",
       x="Casos",
       y='Frecuencia') + 
  theme_minimal()

# EJERCICIO 2

#Filtro por territorio (Spain) y nº casos mayor que cero:
OlasCountry <- estudio1 %>%   filter(countriesAndTerritories=="Spain" & cases>0)

#Representación de las olas de Covid19 tras el verano (Septiembre-Diciembre):
 ggplot(OlasCountry, aes(x=date, y=cases)) +
  geom_line(color="blue", size=0.5) +
   labs(title = "Olas año 2020 Covid19 Spain", 
    subtitle = "Diagrama de lineas",
     x="Meses 2020", y="Casos Covid19 Spain") +
  theme_minimal()
# Como se observa en la gráfica tenemos dos olas:
 # Una a finales de marzo que tiene el pico en abril, con una duración aprox. de dos meses hasta mediados de mayo.
 # Otra que comienza a principios de agosto y tiene el pico en noviembre, con una duración aprox. de cinco meses hasta finales de diciembre.
 
#EJERCICIO 3

#Filtramos los datos por países y por fecha (a partir de verano):
Estudio1Selected <- estudio1 %>%   filter(countriesAndTerritories==c("Spain","Italy","France","Germany", "United_Kingdom") & cases>0)
#summary(Estudio1Selected)
Estudio1SelectedDate <- Estudio1Selected %>%
  filter(date >= ("2020-09-01") & date <= ("2020-12-31"))

#Construimos el gráfico global de casos en los 5 paises:
ggplot(Estudio1SelectedDate,aes(x=date, y=cases, color=countriesAndTerritories, group=countriesAndTerritories)) +
  geom_line(size=0.5)+
  labs(title="Paises mas afectados",
       subtitle="Diagrama de lineas",
       x="Fecha",
       y="Casos diarios") +
  theme_minimal()

# El pais que registró mayor número de casos fue Francia, llegando a los casi 60.000 casos en el mes de noviembre.

#Con la función facet_wrap() construimos el gráfico de cada pais:
ggplot(Estudio1SelectedDate,aes(x=date, y=cases, color=countriesAndTerritories, group=countriesAndTerritories)) +
  geom_line(size=0.5)+
  facet_wrap(~ countriesAndTerritories)+
  labs(title="Paises más afectados",
       subtitle="Diagrama de lineas",
       x="Fecha",
       y="Casos diarios") +
  theme_minimal()

#EJERCICIO 4
#Filtramos los datos para eliminar aquellos que sean NA:

estudio1continentes <- estudio1 %>% filter(!is.na(estudio1$continentExp))

#Hacemos el gráfico de barras indicando con fill que cada columna será un continente distinto:
ggplot(estudio1continentes, aes(continentExp, cases, fill=continentExp))+
  geom_bar(stat = "summary_bin", fun = mean )+
  labs(title="Casos medios por continente",
       subtitle = "Diagrama de barras Covid19 (2019 - 2020)",
       x="Continentes",
       y="Casos medios") +
  theme_minimal()
# El continente con mayor número de casos medios registrados en todo el periodo de tiempo recogido en el dataset fue América. 

options(repos = c(CRAN = "https://cloud.r-project.org"))

