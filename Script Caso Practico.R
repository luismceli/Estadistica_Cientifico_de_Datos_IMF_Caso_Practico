## Limpieza de entorno 
rm(list=ls())
options(scipen=999) 
##Importacion de Librerias 
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)
## Desarollo 
## Importacion de Base de Datos 
nacimientos <- read_csv("nacimientos.csv",locale = locale(encoding = "latin1"))
colnames(nacimientos)
glimpse(nacimientos)
## Creacion de una nueva variable donde se agrupe el tipo de nacimiento
nacimientos <- nacimientos %>%
  mutate(cesarea=case_when(tipocesarea=="Programada" ~ "Si",
                           tipocesarea=="Urgencia" ~ "Si",
                           tipocesarea=="No aplica" ~ "No"))
summary(nacimientos)
## Analisis Graficos
table(nacimientos$cesarea, nacimientos$sexo) ## Contamos la cantidad  
##Edad de la madre en años cumplidos

summary(nacimientos$edad)

etiqueta <- round(fivenum(nacimientos$edad), digits = 2)
boxplot(x=nacimientos$edad, col="red", horizontal = TRUE, xlab="edad", main="Distribución de la Edad")
text(x=etiqueta, y=c(1.15,1.25,1.25,1.25,1.15), labels = etiqueta)


##Cantidad de sexo del nacido vivo
ggplot(data = nacimientos) + geom_bar(mapping = aes(x = sexo) ,fill = "#BE81F7", color = "red")


##Peso promedio

etiqueta <- round(fivenum(nacimientos$peso), digits = 2)
boxplot(nacimientos$peso ~ nacimientos$sexo,
        col = rainbow(ncol(nacimientos)), xlab = "sexo", ylab = "peso", main = "Peso promedio")
text(y=etiqueta, x=c(1.15,1.25,1.25,1.25,1.15,2.3,2.5,2.5,2.5,2.3), labels = etiqueta)

##Correlación entre las variables peso-edadgestacional

data<-select(nacimientos, peso, edadgestacional)
chart.Correlation(data, histogram = TRUE, method = "pearson")

##Histograma
ggplot(na.omit(nacimientos))+
  geom_histogram(aes(x=talla), bins = 30, color = "green")


## Correlación Peso - Talla de hombre y mujer

ggplot(nacimientos)+geom_point(aes(x=peso, y=talla, color=sexo, shape=sexo))+
  scale_y_log10()+
  scale_x_log10()

##############################Analisis de Medias#######################################

## hipotesis ninos 
niños <- nacimientos %>%
  filter(sexo=="Hombre")
t.test(niños$peso, mu=3300, conf.level = .95)
t.test(niños$peso, mu=3300, conf.level = .95, alternative="less")
t.test(niños$peso, mu=3300, conf.level = .95, alternative="greater")

## hipotesis ninas 
niñas <- nacimientos %>%
  filter(sexo=="Mujer")
t.test(niñas$peso, mu=3200, conf.level = .95)
t.test(niñas$peso, mu=3200, conf.level = .95, alternative="less")
t.test(niñas$peso, mu=3200, conf.level = .95, alternative="greater")

## se Descartan los ninos prematuros 
nacimientos %>%
  group_by(edadgestacional,sexo) %>%
  summarise(peso_promedio=mean(peso), .groups = "drop") %>%
  spread(sexo, peso_promedio)


niños <- niños %>%
  filter(edadgestacional>=38)
t.test(niños$peso, mu=3300, conf.level = .95)

niñas <- niñas %>%
  filter(edadgestacional>=38)
t.test(niñas$peso, mu=3200, conf.level = .95)

## se filtran niños que fueron atendidos por el IMSS

unique(nacimientos$lugarnacimiento)
niñas <- niñas %>%
  filter(lugarnacimiento=="IMSS")
t.test(niñas$peso, mu=3200, conf.level = .95)

niños <- niños %>%
  filter(lugarnacimiento=="IMSS")
t.test(niños$peso, mu=3300, conf.level = .95)

##############################Analisis de Varianza #######################################


nacimientos %>%
  group_by(sexo) %>%
  summarise(media_peso=mean(peso),varianza_peso=var(peso),
            media_talla=mean(talla), varianza_talla=var(talla))
nacimientos %>%
  group_by(atencionprenatal) %>%
  summarise(media_peso=mean(peso),varianza_peso=var(peso),
            media_talla=mean(talla), varianza_talla=var(talla))

nacimientos %>%
  group_by(cesarea) %>%
  summarise(media_peso=mean(peso),varianza_peso=var(peso),
            media_talla=mean(talla),varianza_talla=var(talla))
m1 <- nacimientos %>%
  filter(sexo=="Hombre")
m2 <- nacimientos %>%
  filter(sexo=="Mujer")
var.test(m1$peso, m2$peso)
var.test(m1$talla, m2$talla)

m1 <- nacimientos %>%
  filter(atencionprenatal=="Sí")
m2 <- nacimientos %>%
  filter(atencionprenatal=="No")
var.test(m1$peso, m2$peso)

var.test(m1$talla, m2$talla)

m1 <- nacimientos %>%
  filter(cesarea=="No")
m2 <- nacimientos %>%
  filter(cesarea=="Si")
var.test(m1$peso, m2$peso)

var.test(m1$talla, m2$talla)

t.test(peso ~ cesarea, data=nacimientos, var.equal = TRUE)
t.test(talla ~ cesarea, data=nacimientos, var.equal = TRUE)
t.test(peso ~ sexo, data=nacimientos)
t.test(talla ~ sexo, data=nacimientos)
t.test(peso ~ atencionprenatal, data=nacimientos)
t.test(talla ~ atencionprenatal, data=nacimientos)
