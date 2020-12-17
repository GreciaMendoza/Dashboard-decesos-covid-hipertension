#Dashboard en R
#Grecia Mendoza Aviña
library(dplyr)
library(magrittr)
library(lubridate)
library(flexdashboard)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(plotly)
library(dygraphs)
library(xts)
library(TTR)
library(zoo)


# Dashboard

# Dashboard de covid 19
# Trabajaremos con la base y diccionario de Carranco, podemos encontrarlo aqui:

#Fijamosnuestro directorio de trabajo

setwd("C:/Users/Laptop/Desktop/Ingenieria_de_caracteristicas/dashboard")

# Descarga de archivos

#Base de Carranco
# download.file(url="https://github.com/carranco-sga/Mexico-COVID-19/raw/master/Open_data/COVID-19/202012/20201208.zip",destfile="covid_mexico.zip")
# unzip(zipfile = "covid_mexico.zip")

#Descarga del diccionario
# diccionario<-"https://github.com/carranco-sga/Mexico-COVID-19/raw/master/Open_data/COVID-19/202012/diccionario_20201208.zip"
# download.file(url=diccionario,destfile = "Diccionario.zip")
# unzip(zipfile = "Diccionario.zip")

#Descarga de la base de INEGI
#Inegi<-download.file("https://www.inegi.org.mx/contenidos/investigacion/pohd/2018/microdatos/por_estado_hipertension.csv",destfile="Inegi_hipertension.csv")


### Limpieza de la base de datos

base1<-read.csv("201208COVID19MEXICO.csv",header=TRUE,sep = ",", na.strings = "" )
inegi<-read.csv("Inegi_hipertension.csv",header=TRUE,sep = ",", na.strings = "")

#Con la función summary podemos notar algunas características de la base

#head(base1)
summary(base1)

# Al observar el summary notamos que variables que representan las fechas estan en  formato de caracter, por lo que deben convertirse a formato de fecha.


#### Corrección formato fecha

# Cambiamos a formato de fecha las variables: FECHA_ACTUALIZACION, FECHA_INGRESO,  FECHA_SINTOMAS y FECHA_DEF

#Con lubridate cambiamos a formato de fecha
base1$FECHA_ACTUALIZACION %<>% ymd()
base1$FECHA_INGRESO %<>% ymd()
base1$FECHA_SINTOMAS%<>% ymd() 
base1$FECHA_DEF%<>% ymd()

summary(base1)


#### Selección y filtro

#Seleccionamos las variables de interés usando  dplyr

#Selección de variables y filtro de hipertensos confirmados. 
covid19<-base1%>%
  select(FECHA_SINTOMAS,FECHA_DEF,ENTIDAD_RES,HIPERTENSION,CLASIFICACION_FINAL)%>%
  filter(CLASIFICACION_FINAL==3 & HIPERTENSION==1)
#head(covid19)  
#summary(covid19)

# #Filtramos por confirmados de covid 19 e hipertensión
# ConfHiperten<-covid19%>%
#   filter(CLASIFICACION_FINAL==3 & HIPERTENSION==1)
#    #filter(FECHA_DEF!=is.na(FECHA_DEF))
# head(ConfHiperten)

#Agrupamos por fecha y casos totales de hipertension por  dia
# fechaHiperten<-covid19%>%
#   group_by(FECHA_DEF)%>%
#   filter(FECHA_DEF!=is.na(FECHA_DEF))%>%
#   summarise(Hipertensos=sum(HIPERTENSION))%>%
#   mutate(Nuevos=abs(Hipertensos-lag(Hipertensos)),mm7=zoo::rollmean(Hipertensos,7,align="center",fill=NA),mm14=zoo::rollmean(Hipertensos,14,align="center",fill=NA),mm7_N=zoo::rollmean(Nuevos,7,align="center",fill=NA),mm14_N=zoo::rollmean(Nuevos,14,align="center",fill=NA))
# 

fechaHiperten<-covid19%>%
  group_by(FECHA_DEF)%>%
  filter(FECHA_DEF!=is.na(FECHA_DEF))%>%
  summarise(Hipertensos=sum(HIPERTENSION))%>%
  mutate(acumulados=cumsum(Hipertensos),Nuevos=abs(Hipertensos-lag(Hipertensos)),mm7=zoo::rollmean(Hipertensos,7,align="center",fill=NA),mm14=zoo::rollmean(Hipertensos,14,align="center",fill=NA),mm7_N=zoo::rollmean(Nuevos,7,align="center",fill=NA),mm14_N=zoo::rollmean(Nuevos,14,align="center",fill=NA))




# head(fechaHiperten)
#serie de tiempo con dygraph
# FH<-xts(x=fechaHiperten$Total_hipertensos, order.by= fechaHiperten$FECHA_DEF)
# dygraph(FH)%>%
#   dyAxis("x", label="Fecha de defunción") %>%
#   dyAxis("y", label="Hipertensos")%>%
#   dyOptions(labelsKMB = TRUE)

#Correlación
confirmados<-covid19%>%
  select(FECHA_SINTOMAS,HIPERTENSION) %$%
  aggregate(HIPERTENSION~FECHA_SINTOMAS,FUN=sum) %>%
  mutate(Confirmados=cumsum(HIPERTENSION))%>%
  filter(FECHA_SINTOMAS>="2020-03-21")

#head(confirmados,20)
decesos<-fechaHiperten%>%
  select(FECHA_DEF,Hipertensos)%>%
  mutate(Decesos=cumsum(Hipertensos))

correlacion<-confirmados%>%
  bind_cols(decesos)

ggplot(correlacion, aes(x=Confirmados, y=Decesos)) +
  geom_point(shape=1) 


##### serie de tiempo con plotly

# Casos acumulados
plot_ly(data=fechaHiperten,x=~FECHA_DEF)%>%
  add_trace(y=~acumulados, name='Defunciones de hipertensos', mode='lines')%>%
  add_trace(y=~mm14,mode='lines',name='mm 7 días')%>%
  layout(xaxis = x)

# Casos nuevos de defunciones presentados por día
x <- list(
  title = "")
plot_ly(data=fechaHiperten,x=~FECHA_DEF)%>%
  add_trace(y=~Hipertensos, name='Defunciones de hipertensos', mode='lines')%>%
  add_trace(y=~mm7,mode='lines',name='mm 7 días')%>%
  layout(xaxis = x)


# Pie chart de la Republica

#para nuestro pie chart utilizaremos la  base del INEGI,en la cual se encuentran la  cantidad de personas con hipertensión y el porcentaje de ello. Para esto, combinaremos la base de Carrasco y la del Inegi filtrando por Estado.


base2<-base1%>%
  select(ENTIDAD_RES,HIPERTENSION,CLASIFICACION_FINAL)%>%
  filter(HIPERTENSION==1 & CLASIFICACION_FINAL==3)%>%
  group_by(ENTIDAD_RES)%>%
  summarise(Hipertensos=sum(HIPERTENSION))
#Con bind_cols combinaremos las bases
#Para ello ambas bases deben contener el mismo número de filas
covinegi<-base2%>%
  bind_cols(inegi)
# head(covinegi)

#### Pie chart

#Veremosla cantidad de personas con hipertensión por estado
vals1 <- paste(covinegi$Hipe, sep = "")
plot_ly(covinegi, labels = ~Estado, values = ~Hipe, type = 'pie', textinfo = "text", text = vals1) %>%
  layout(title = 'Personas con hipertensión en México',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#Personas con hipertensión que resultaron positivas para SARS-2 por estado
vals <- paste(covinegi$Hipertensos, sep = "")

plot_ly(covinegi, labels = ~Estado, values = ~Hipertensos, type = 'pie', textinfo = "text", text = vals) %>%
  layout(title = 'Hipertensos positivos a SARS-2 ',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


## Defunciones de hipertensos confirmados por zona

## Zona norte

# Comprende los siguientes Estados:
#   
#   
#   Baja California
# Baja California Sur
# Chihuahua
# Coahuila
# Durango
# Nuevo León
# Sinaloa
# Sonora
# Tamaulipas

Zonanorte<-covid19%>%
  filter(ENTIDAD_RES==02|ENTIDAD_RES==03|ENTIDAD_RES==05|ENTIDAD_RES==08|ENTIDAD_RES==10|ENTIDAD_RES==19|ENTIDAD_RES==25|ENTIDAD_RES==26|ENTIDAD_RES==28,FECHA_DEF!=is.na(FECHA_DEF))%>%
  group_by(FECHA_DEF)%>%
  summarise(Hipertensos=sum(HIPERTENSION))%>%
  mutate(mm7=zoo::rollmean(Hipertensos,7,align="center",fill=NA),mm14=zoo::rollmean(Hipertensos,14,align="center",fill=NA))

  
#### Serie de tiempo de la zona norte

# x <- list(
#   title = "")
plot_ly(data=Zonanorte,x=~FECHA_DEF)%>%
  add_trace(y=~Hipertensos, name='Defunciones Zona norte', mode='lines')%>%
  add_trace(y=~mm7,mode='lines',name='mm 7 días')%>%
  add_trace(y=~mm14,mode='lines',name='mm 14 días')%>%
  layout(xaxis = x)

#### Pie chart zona norte

Pienorte<-covinegi%>%
  filter(ENTIDAD_RES==02|ENTIDAD_RES==03|ENTIDAD_RES==05|ENTIDAD_RES==08|ENTIDAD_RES==10|ENTIDAD_RES==19|ENTIDAD_RES==25|ENTIDAD_RES==26|ENTIDAD_RES==28)

vals2 <- paste(Pienorte$Hipertensos, sep = "")
plot_ly(Pienorte, labels = ~Estado, values = ~Hipertensos, type = 'pie', textinfo = "text", text = vals2) %>%
  layout(title = 'Población hipertensa positiva a SARS-2 ',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#### Bar plot
#Población hipertensa en Zona norte
plot_ly(Pienorte, x = ~Estado, y = ~Hipe, type = 'bar', name = '', marker = list(color = 'rgb(49,130,189)')) %>% 
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')


## Zona centro
# Comprende los siguientes estados:
#   
#   Aguascalientes
# Ciudad de México
# Guanajuato
# Morelos
# Querétaro
# San Luis Potosí
# Zacatecas

Zonacentro<-covid19%>%
  filter(ENTIDAD_RES==01|ENTIDAD_RES==15|ENTIDAD_RES==11|ENTIDAD_RES==17|ENTIDAD_RES==22|ENTIDAD_RES==24|ENTIDAD_RES==32 & FECHA_DEF!=is.na(FECHA_DEF))%>%
  group_by(FECHA_DEF)%>%
  summarise(Hipertensos=sum(HIPERTENSION))%>%
  mutate(mm7=zoo::rollmean(Hipertensos,7,align="center",fill=NA),mm14=zoo::rollmean(Hipertensos,14,align="center",fill=NA))%>%
  filter(mm7<=80 & mm14<=80 )


#### Serie de tiempo zona centro 

x <- list(
  title = "")
plot_ly(data=Zonacentro,x=~FECHA_DEF)%>%
  add_trace(y=~Hipertensos, name='Defunciones Zona centro', mode='lines')%>%
  add_trace(y=~mm7,mode='lines',name='mm 7 días')%>%
  add_trace(y=~mm14,mode='lines',name='mm 14 días')%>%
  layout(xaxis = x)

#### Pie chart zona centro

Piecentro<-covinegi%>%
  filter(ENTIDAD_RES==01|ENTIDAD_RES==15|ENTIDAD_RES==11|ENTIDAD_RES==17|ENTIDAD_RES==22|ENTIDAD_RES==24|ENTIDAD_RES==3)

vals3 <- paste(Piecentro$Hipertensos, sep = "")
plot_ly(Piecentro, labels = ~Estado, values = ~Hipertensos, type = 'pie', textinfo = "text", text = vals3) %>%
  layout(title = 'Población hipertensa positiva a SARS-2 ',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
#### bar plot zona centro

#Población hipertensa en Zona norte
plot_ly(Piecentro, x = ~Estado, y = ~Hipe, type = 'bar', name = '', marker = list(color = 'rgb(49,130,189)')) %>% 
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')

### Zona sur

# Comprende los siguientes Estados:
#   
# Campeche
# Chiapas
# Guerrero
# Oaxaca
# Quintana Roo 
# Tabasco
# Yucatán

#Selección de la zona sur
Zonasur<-covid19%>%
  filter(ENTIDAD_RES==04|ENTIDAD_RES==07|ENTIDAD_RES==12|ENTIDAD_RES==20|ENTIDAD_RES==23|ENTIDAD_RES==27|ENTIDAD_RES==31 & FECHA_DEF!=is.na(FECHA_DEF))%>%
  group_by(FECHA_DEF)%>%
  summarise(Hipertensos=sum(HIPERTENSION))%>%
  mutate(mm7=zoo::rollmean(Hipertensos,7,align="center",fill=NA),mm14=zoo::rollmean(Hipertensos,14,align="center",fill=NA))%>%
  filter(mm7<=50 & mm14<=50)

#Evolución de decesos
plot_ly(data=Zonasur,x=~FECHA_DEF)%>%
  add_trace(y=~Hipertensos, name='Defunciones Zona centro', mode='lines')%>%
  add_trace(y=~mm7,mode='lines',name='mm 7 días')%>%
  add_trace(y=~mm14,mode='lines',name='mm 14 días')%>%
  layout(xaxis = x)

#creamos dataframe para nuestro piechart
Piesur<-covinegi%>%
  filter(ENTIDAD_RES==04|ENTIDAD_RES==07|ENTIDAD_RES==12|ENTIDAD_RES==20|ENTIDAD_RES==23|ENTIDAD_RES==27|ENTIDAD_RES==31)


vals4<- paste(Piesur$Hipertensos, sep = "")
plot_ly(Piesur, labels = ~Estado, values = ~Hipertensos, type = 'pie', textinfo = "text", text = vals4) %>%
  layout(title = 'Población hipertensa positiva a SARS-2 ',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

#### barplot zona sur

plot_ly(Piesur, x = ~Estado, y = ~Hipe, type = 'bar', name = '', marker = list(color = 'rgb(49,130,189)')) %>% 
  layout(xaxis = list(title = "", tickangle = -45),
         yaxis = list(title = ""),
         margin = list(b = 100),
         barmode = 'group')



