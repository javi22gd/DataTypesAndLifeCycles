## .page-header {

##     color: #ffffff;

##     background-image: linear-gradient(120deg, #ff5400, #ffaf8a);

## }

## .project-author {

##   color: #ffffff;

##   opacity: .9

## }

## .project-date {

##   color: #ffffff;

##   opacity: .8

## }

## .toc .toc-box {

##   background-image: linear-gradient(120deg, #ff5400, #ffaf8a);

##   border: solid 1px #ffff;

## }

## .toc .toc-box > ul {

##   color: #ffff;

## }

## .main-content h1 {

##   color: #ff5400;

##   opacity: .9

## }

## .main-content h2 {

##   color: #ff5400;

##   opacity: .8

## }

## .main-content h3 {

##   color: #ff5400;

##   opacity: .7

## }

## .main-content h4 {

##   color: #ff5400;

##   opacity: .6

## }

## .main-content h5 {

##   color: #ff5400;

##   opacity: .5

## }

## .main-content pre {

##   background-color: #faf5f2;

##   border: solid 1px #ffff;

## 
## }

## .main-content pre > code {

##   color: #716760;

## }

## .main-content table th {

##   background-color: #ff5400;

##   color: #ffffff;

##   opacity: .8

## }

## .pagedtable-header-type {

##     color: #ffffff;

##     opacity: .8

## }

## .main-content table td {

##   color: #716760;

## }

## .main-content table tr:nth-child(odd) {

##   background-color: #f7f6f5;

## }

## .main-content table tr:nth-child(even) {

##   background-color: #ff;

## }

## body {

##   color: #716760;

## }

## a {

##   color: #ffffff;

## }


## ----setup, include=FALSE----------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----------------------------------------------------------------------------------------------------------
library(readr)
d <- read.csv("activities2.csv", sep=",", encoding="UTF-8")
head(d)


## ----------------------------------------------------------------------------------------------------------
d <- subset(d, select = -c(ID.de.actividad, Nombre.de.la.actividad, Descripción.de.la.actividad, Desplazamiento, Nombre.del.archivo, Peso.del.deportista, Peso.de.la.bicicleta, Tiempo.transcurrido.1, Distancia.1, Pérdida.de.desnivel, Desnivel.bajo, Desnivel.alto, Pendiente.positiva.promedio, Pendiente.negativa.promedio, Ritmo.cardiaco.máximo.1, Vatios.máx., Vatios.promedio, Temperatura.máx., Temperatura.promedio, Esfuerzo.Relativo.1, Trabajo.total, Número.de.carreras, Tiempo.en.ascenso, Tiempo.en.descenso, Otro.tiempo, Esfuerzo.Percibido, Type, Start.Time, Potencia.promedio.ponderada, Recuento.de.potencia, Usar.Esfuerzo.Percibido, Esfuerzo.Relativo.percibido, Desplazamiento.1, Peso.total.levantado, Desde.la.carga, Distancia.ajustada.en.pendientes, Hora.de.observación.meteorológica, Condición.meteorológica, Temperatura.meteorológica, Temperatura.aparente, Punto.de.rocío, Humedad, Presión.meteorológica, Velocidad.del.viento, Ráfaga.de.viento, Dirección.del.viento, Intensidad.de.precipitación, Hora.del.amanecer, Hora.del.atardecer, Fase.lunar, Bicicleta, Equipamiento, Probabilidad.de.precipitación, Tipo.de.precipitación, Nubosidad, Visibilidad.meteorológica, Índice.UV, Ozono.meteorológico, Jump.Count, Total.Grit, Avg.Flow, Flagged, Avg.Elapsed.Speed, Dirt.Distance))
tail(d)


## ----------------------------------------------------------------------------------------------------------
summary(d$Fecha.de.la.actividad)


## ----------------------------------------------------------------------------------------------------------
sum(is.na(as.POSIXct(d$Fecha.de.la.actividad,format="%d %b %Y %H:%M:%S",tz='UTC')))


## ----------------------------------------------------------------------------------------------------------
head(d[is.na(as.POSIXct(d$Fecha.de.la.actividad,format="%d %b %Y %H:%M:%S",tz='UTC')),'Fecha.de.la.actividad'])
tail(d[is.na(as.POSIXct(d$Fecha.de.la.actividad,format="%d %b %Y %H:%M:%S",tz='UTC')),'Fecha.de.la.actividad'])


## ----------------------------------------------------------------------------------------------------------
d[is.na(as.POSIXct(d$Fecha.de.la.actividad,format="%d %b %Y %H:%M:%S",tz='UTC')),'Fecha.de.la.actividad'] <- sub("([A-Za-z]{3})[a-z]", "\\1", d[is.na(as.POSIXct(d$Fecha.de.la.actividad,format="%d %b %Y %H:%M:%S",tz='UTC')),'Fecha.de.la.actividad'])
d$Fecha.de.la.actividad <- as.POSIXct(d$Fecha.de.la.actividad,format="%d %b %Y %H:%M:%S",tz='UTC')
d$Fecha.de.la.actividad <- as.Date(d$Fecha.de.la.actividad)
summary(d$Fecha.de.la.actividad)


## ----------------------------------------------------------------------------------------------------------
summary(d$Tipo.de.actividad)


## ----------------------------------------------------------------------------------------------------------
d$Tipo.de.actividad <- as.factor(d$Tipo.de.actividad)
summary(d$Tipo.de.actividad)


## ----------------------------------------------------------------------------------------------------------
d[d$Tipo.de.actividad=='Carrera virtual',]
d[441,]


## ----------------------------------------------------------------------------------------------------------
d$Tipo.de.actividad[d$Tipo.de.actividad=='Carrera virtual'] <- 'Bicicleta virtual'
d$Tipo.de.actividad <- factor(d$Tipo.de.actividad)
summary(d$Tipo.de.actividad)


## ----------------------------------------------------------------------------------------------------------
summary(d$Tiempo.transcurrido)


## ----------------------------------------------------------------------------------------------------------
library(lubridate)
d$Tiempo.transcurrido <- seconds_to_period(d$Tiempo.transcurrido)


## ----------------------------------------------------------------------------------------------------------
summary(d$Tiempo.transcurrido)


## ----------------------------------------------------------------------------------------------------------
summary(d$Tiempo.en.movimiento)


## ----------------------------------------------------------------------------------------------------------
d$Tiempo.en.movimiento <- seconds_to_period(d$Tiempo.en.movimiento)
summary(d$Tiempo.en.movimiento)


## ----------------------------------------------------------------------------------------------------------
d$Tiempo.de.descanso <- period_to_seconds(d$Tiempo.transcurrido) - period_to_seconds(d$Tiempo.en.movimiento)
d$Tiempo.de.descanso <- seconds_to_period(d$Tiempo.de.descanso)
summary(d$Tiempo.de.descanso)


## ----------------------------------------------------------------------------------------------------------
summary(d$Distancia)


## ----------------------------------------------------------------------------------------------------------
d$Distancia <- as.numeric(gsub(",", ".", gsub("\\.", "", d$Distancia)))
summary(d$Distancia)


## ----------------------------------------------------------------------------------------------------------
d[d$Distancia==max(d$Distancia),]


## ----------------------------------------------------------------------------------------------------------
d[d$Tipo.de.actividad=='Natación',]$Distancia <- d[d$Tipo.de.actividad=='Natación',]$Distancia / 1000
summary(d$Distancia[d$Distancia!=0])


## ----------------------------------------------------------------------------------------------------------
summary(d$Tipo.de.actividad[d$Distancia==0])
d[(d$Distancia==0 & d$Tipo.de.actividad=='Entrenamiento'),]


## ----------------------------------------------------------------------------------------------------------
d$Tipo.de.actividad[d$Distancia==0 & d$Tipo.de.actividad=='Entrenamiento'] <- 'Entrenamiento con pesas'
summary(d$Tipo.de.actividad[d$Distancia==0])


## ----------------------------------------------------------------------------------------------------------
d[(d$Distancia==0 & d$Tipo.de.actividad=='Bicicleta'),]


## ----------------------------------------------------------------------------------------------------------
d$Tipo.de.actividad[(d$Distancia==0 & d$Tipo.de.actividad=='Bicicleta' & period_to_seconds(d$Tiempo.en.movimiento) < 7200)] <- 'Bicicleta virtual'
summary(d$Tipo.de.actividad[d$Distancia==0])


## ----------------------------------------------------------------------------------------------------------
for(i in c('Bicicleta', 'Bicicleta virtual')){
    index <- d$Distancia==0 & d$Tipo.de.actividad==i
    if(sum(index)>0){
      d$Distancia[index] <- ((mean(d$Velocidad.promedio[d$Tipo.de.actividad==i & d$Distancia>0 & d$Velocidad.promedio!=0], na.rm=TRUE)) * (period_to_seconds(d$Tiempo.en.movimiento[index]))) / 1000
    }
}
summary(d$Distancia)


## ----------------------------------------------------------------------------------------------------------
summary(d$Ritmo.cardiaco.máximo)


## ----------------------------------------------------------------------------------------------------------
imputar.media.por.tipo <- function(variable){
  for(i in levels(d$Tipo.de.actividad)){
    index <- is.na(variable) & d$Tipo.de.actividad==i
    if(sum(index)>0){
      variable[index] <- as.integer(mean(variable[d$Tipo.de.actividad==i], na.rm=TRUE))
    }
  }
  return(variable)
}

d$Ritmo.cardiaco.máximo <- imputar.media.por.tipo(d$Ritmo.cardiaco.máximo)
summary(d$Ritmo.cardiaco.máximo)


## ----------------------------------------------------------------------------------------------------------
d$Ritmo.cardiaco.promedio <- as.integer(d$Ritmo.cardiaco.promedio)
summary(d$Ritmo.cardiaco.promedio)


## ----------------------------------------------------------------------------------------------------------
d$Ritmo.cardiaco.promedio <- imputar.media.por.tipo(d$Ritmo.cardiaco.promedio)
summary(d$Ritmo.cardiaco.promedio)


## ----------------------------------------------------------------------------------------------------------
d$Esfuerzo.Relativo <- as.integer(d$Esfuerzo.Relativo)
summary(d$Esfuerzo.Relativo)


## ----------------------------------------------------------------------------------------------------------
summary(d$Equipamiento.de.la.actividad)


## ----------------------------------------------------------------------------------------------------------
d$Equipamiento.de.la.actividad <- as.factor(d$Equipamiento.de.la.actividad)
summary(d$Equipamiento.de.la.actividad)


## ----------------------------------------------------------------------------------------------------------
summary(d$Velocidad.máxima)


## ----------------------------------------------------------------------------------------------------------
d[which(is.na(d$Velocidad.máxima)),]


## ----------------------------------------------------------------------------------------------------------
d$Velocidad.máxima <- imputar.media.por.tipo(d$Velocidad.máxima)
summary(d$Velocidad.máxima)


## ----------------------------------------------------------------------------------------------------------
d$Velocidad.máxima <- d$Velocidad.máxima * 3.6
summary(d$Velocidad.máxima[d$Velocidad.máxima!=0])


## ----------------------------------------------------------------------------------------------------------
summary(d$Velocidad.promedio)


## ----------------------------------------------------------------------------------------------------------
summary(d$Tipo.de.actividad[is.na(d$Velocidad.promedio)])


## ----------------------------------------------------------------------------------------------------------
for(i in c('Bicicleta', 'Bicicleta virtual', 'Caminata', 'Entrenamiento', 'Natación')){
    index <- is.na(d$Velocidad.promedio) & d$Tipo.de.actividad==i
    if(sum(index)>0){
      d$Velocidad.promedio[index] <- (d$Distancia[index]*1000) / period_to_seconds(d$Tiempo.en.movimiento[index])
    }
}
d$Velocidad.promedio[is.na(d$Velocidad.promedio) & d$Tipo.de.actividad=='Entrenamiento con pesas'] <- 0
summary(d$Tipo.de.actividad[is.na(d$Velocidad.promedio)])


## ----------------------------------------------------------------------------------------------------------
d$Velocidad.promedio <- d$Velocidad.promedio * 3.6
summary(d$Velocidad.promedio[d$Velocidad.promedio!=0])


## ----------------------------------------------------------------------------------------------------------
summary(d$Tipo.de.actividad[d$Desnivel.positivo==0 | is.na(d$Desnivel.positivo)])


## ----------------------------------------------------------------------------------------------------------
d[(d$Desnivel.positivo==0 | is.na(d$Desnivel.positivo)) & d$Tipo.de.actividad=='Bicicleta',]


## ----------------------------------------------------------------------------------------------------------
d$Tipo.de.actividad[1028] <- 'Bicicleta virtual'


## ----------------------------------------------------------------------------------------------------------
d$Desnivel.positivo[(d$Desnivel.positivo==0 | is.na(d$Desnivel.positivo)) & d$Tipo.de.actividad=='Bicicleta'] <- mean(d$Desnivel.positivo[d$Tipo.de.actividad=='Bicicleta' & d$Desnivel.positivo>0 & d$Distancia>0] / d$Distancia[d$Tipo.de.actividad=='Bicicleta' & d$Distancia>0 & d$Desnivel.positivo>0], na.rm=TRUE) * d$Distancia[(d$Desnivel.positivo==0 | is.na(d$Desnivel.positivo)) & d$Tipo.de.actividad=='Bicicleta']
summary(d$Tipo.de.actividad[d$Desnivel.positivo==0 | is.na(d$Desnivel.positivo)])
summary(d$Desnivel.positivo)


## ----------------------------------------------------------------------------------------------------------
summary(d$Tipo.de.actividad[is.na(d$Pendiente.máxima)])


## ----------------------------------------------------------------------------------------------------------
d$Pendiente.máxima[is.na(d$Pendiente.máxima)] <- 0
summary(d$Pendiente.máxima)


## ----------------------------------------------------------------------------------------------------------
summary(d$Tipo.de.actividad[d$Pendiente.máxima>40])


## ----------------------------------------------------------------------------------------------------------
d$Pendiente.máxima[d$Pendiente.máxima>40 & d$Equipamiento.de.la.actividad!='Orbea Satellite'] <- 40


## ----------------------------------------------------------------------------------------------------------
summary(d$Pendiente.promedio)


## ----------------------------------------------------------------------------------------------------------
d[abs(d$Pendiente.promedio)>2,]


## ----------------------------------------------------------------------------------------------------------
# Esta actividad no crresponde a ninguna actividad deportiva, por lo que se elimina
d <- d[-c(70),]

index <- (d$Distancia[abs(d$Pendiente.promedio)>2] / d$Desnivel.positivo[abs(d$Pendiente.promedio)>2]) > 0.02 
d$Pendiente.promedio[abs(d$Pendiente.promedio)>2][index] <- 0
summary(d$Pendiente.promedio)


## ----------------------------------------------------------------------------------------------------------
summary(d$Cadencia.máx.)
summary(d$Tipo.de.actividad[is.na(d$Cadencia.máx.)])


## ----------------------------------------------------------------------------------------------------------
length(d$Cadencia.máx.[!is.na(d$Cadencia.máx.) & (d$Tipo.de.actividad=='Entrenamiento con pesas' | d$Tipo.de.actividad=='Natación')])


## ----------------------------------------------------------------------------------------------------------
summary(d$Cadencia.máx.[(d$Tipo.de.actividad=='Bicicleta' | d$Tipo.de.actividad=='Bicicleta virtual') & !is.na(d$Cadencia.máx.)])


## ----------------------------------------------------------------------------------------------------------
summary(d$Cadencia.promedio)
summary(d$Tipo.de.actividad[is.na(d$Cadencia.promedio)])


## ----------------------------------------------------------------------------------------------------------
d$Cadencia.promedio[is.na(d$Cadencia.máx.) & !is.na(d$Cadencia.promedio)]


## ----------------------------------------------------------------------------------------------------------
d$Cadencia.promedio[d$Cadencia.promedio==0 & !is.na(d$Cadencia.promedio)] <- NA


## ----------------------------------------------------------------------------------------------------------
length(d$Cadencia.promedio[!is.na(d$Cadencia.promedio) & (d$Tipo.de.actividad=='Entrenamiento con pesas' | d$Tipo.de.actividad=='Natación')])


## ----------------------------------------------------------------------------------------------------------
summary(d$Cadencia.promedio[(d$Tipo.de.actividad=='Bicicleta' | d$Tipo.de.actividad=='Bicicleta virtual') & !is.na(d$Cadencia.promedio)])


## ----------------------------------------------------------------------------------------------------------
d[(d$Tipo.de.actividad=='Bicicleta' | d$Tipo.de.actividad=='Bicicleta virtual') & !is.na(d$Cadencia.promedio) & d$Cadencia.promedio==min(d$Cadencia.promedio[(d$Tipo.de.actividad=='Bicicleta' | d$Tipo.de.actividad=='Bicicleta virtual') & !is.na(d$Cadencia.promedio)]),]


## ----------------------------------------------------------------------------------------------------------
d$Cadencia.promedio[d$Tipo.de.actividad=='Bicicleta' & !is.na(d$Cadencia.promedio) & d$Cadencia.promedio==min(d$Cadencia.promedio[d$Tipo.de.actividad=='Bicicleta' & !is.na(d$Cadencia.promedio)])] <- mean(d$Cadencia.promedio[d$Tipo.de.actividad=='Bicicleta' & !is.na(d$Cadencia.promedio)])
summary(d$Cadencia.promedio[(d$Tipo.de.actividad=='Bicicleta' | d$Tipo.de.actividad=='Bicicleta virtual') & !is.na(d$Cadencia.promedio)])


## ----------------------------------------------------------------------------------------------------------
d$Calorías <- as.integer(d$Calorías)
summary(d$Calorías)
summary(d$Tipo.de.actividad[is.na(d$Calorías)])


## ----------------------------------------------------------------------------------------------------------
cor(d$Calorías[!is.na(d$Calorías)],
    (d$Ritmo.cardiaco.promedio[!is.na(d$Calorías)] * period_to_seconds(d$Tiempo.en.movimiento[!is.na(d$Calorías)])),
    method='pearson')


## ----------------------------------------------------------------------------------------------------------
for(i in levels(d$Tipo.de.actividad)){
  index <- d$Tipo.de.actividad==i & !is.na(d$Calorías)
  ratio <- period_to_seconds(d$Tiempo.en.movimiento[index]) * d$Ritmo.cardiaco.promedio[index]
  if(sum(index)>1){
    rl <- lm(d$Calorías[index] ~ ratio)
    ypred <- rl$fitted.values
    plot(ratio,
         d$Calorías[index],
         main = paste('Regresión lineal: ', i),
         xlab = '(Ritmo cardíaco * Tiempo en movimiento)',
         ylab = 'Calorías',
         pch = 19,
         cex = .5,
         col = 'light blue',
         frame = FALSE)
    lines(ratio,
          ypred,
          lwd = 2.5,
          col = "#fc5200")
    }
}


## ----------------------------------------------------------------------------------------------------------
ratio <- period_to_seconds(d$Tiempo.en.movimiento[!is.na(d$Calorías)]) * d$Ritmo.cardiaco.promedio[!is.na(d$Calorías)]
rl <- lm(d$Calorías[!is.na(d$Calorías)] ~ ratio)
summary(rl)


## ----------------------------------------------------------------------------------------------------------
for(i in levels(d$Tipo.de.actividad)){
  index <- is.na(d$Calorías) & d$Tipo.de.actividad==i
  if(sum(index)>0){
    d$Calorías[index] <- as.integer(mean(d$Calorías[d$Tipo.de.actividad==i] / (period_to_seconds(d$Tiempo.en.movimiento[d$Tipo.de.actividad==i]) * d$Ritmo.cardiaco.promedio[d$Tipo.de.actividad==i]), na.rm=TRUE) * (period_to_seconds(d$Tiempo.en.movimiento[index]) * d$Ritmo.cardiaco.promedio[index]))
  }
}
summary(d$Calorías)
summary(d$Tipo.de.actividad[is.na(d$Calorías)])


## ----------------------------------------------------------------------------------------------------------
i <- 'Carrera'
index <- is.na(d$Calorías) & d$Tipo.de.actividad=='Natación'
d$Calorías[index] <- as.integer(mean(d$Calorías[d$Tipo.de.actividad==i] / (period_to_seconds(d$Tiempo.en.movimiento[d$Tipo.de.actividad==i]) * d$Ritmo.cardiaco.promedio[d$Tipo.de.actividad==i]), na.rm=TRUE) * (period_to_seconds(d$Tiempo.en.movimiento[index]) * d$Ritmo.cardiaco.promedio[index]))
summary(d$Calorías)
summary(d$Tipo.de.actividad[is.na(d$Calorías)])


## ----------------------------------------------------------------------------------------------------------
b <- d$Tipo.de.actividad == 'Bicicleta'
v <- d$Tipo.de.actividad == 'Bicicleta virtual'
p <- d$Tipo.de.actividad == 'Entrenamiento con pesas'
e <- d$Tipo.de.actividad == 'Entrenamiento'
c <- d$Tipo.de.actividad == 'Caminata'


## ----------------------------------------------------------------------------------------------------------
xmin = min(c(min(d$Fecha.de.la.actividad[b]),
         min(d$Fecha.de.la.actividad[v]),
         min(d$Fecha.de.la.actividad[p]),
         min(d$Fecha.de.la.actividad[e])))
xmax = max(c(max(d$Fecha.de.la.actividad[b]),
         max(d$Fecha.de.la.actividad[v]),
         max(d$Fecha.de.la.actividad[p]),
         max(d$Fecha.de.la.actividad[e])))
ymax = max(c(max(aggregate(d$Fecha.de.la.actividad[b], by=list(month=as.numeric(format(d$Fecha.de.la.actividad[b], "%Y%m"))), FUN=length)['x']),
             max(aggregate(d$Fecha.de.la.actividad[v], by=list(month=as.numeric(format(d$Fecha.de.la.actividad[v], "%Y%m"))), FUN=length)['x']),
             max(aggregate(d$Fecha.de.la.actividad[p], by=list(month=as.numeric(format(d$Fecha.de.la.actividad[p], "%Y%m"))), FUN=length)['x']),
             max(aggregate(d$Fecha.de.la.actividad[e], by=list(month=as.numeric(format(d$Fecha.de.la.actividad[e], "%Y%m"))), FUN=length)['x'])))

hist(d$Fecha.de.la.actividad[b],
     breaks = 'months',
     main='Actividades por tipo y mes',
     col=rgb(0,0,1,0.2),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[p],
     breaks = 'months',
     main='',
     col=rgb(0,1,0,0.3),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[v],
     breaks = 'months',
     main='',
     col=rgb(0,1,1,0.4),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[e],
     breaks = 'months',
     main='',
     col=rgb(1,0,0,0.5),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
legend("topleft", legend=c("Bicicleta", "Entrenamiento con pesas", "Bicicleta virtual", "Entrenamiento"), col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3), rgb(0,1,1,0.4), rgb(1,0,0,0.5)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[b],
     breaks = 'months',
     main='Actividades de Bicicleta por mes',
     col=rgb(0,0,1,0.2),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE)


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(as.numeric(d$Fecha.de.la.actividad[b])/24)


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[v],
     breaks = 'months',
     main='Actividades de Bicicleta virtual por mes',
     col=rgb(0,1,1,0.4),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE)


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(as.numeric(d$Fecha.de.la.actividad[v])/24)


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[p],
     breaks = 'months',
     main='Actividades de Entrenamiento con pesas por mes',
     col=rgb(0,1,0,0.3),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE)


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(as.numeric(d$Fecha.de.la.actividad[p])/24)


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[e],
     breaks = 'months',
     main='Actividades de Entrenamiento por mes',
     col=rgb(1,0,0,0.5),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE)


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(as.numeric(d$Fecha.de.la.actividad[e])/24)


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[b],
     breaks = 'months',
     main='Actividades por tipo y mes',
     col=rgb(0,0,1,0.2),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[v],
     breaks = 'months',
     main='',
     col=rgb(0,1,1,0.4),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
legend("topleft", legend=c("Bicicleta", "Bicicleta virtual"), col=c(rgb(0,0,1,0.2), rgb(0,1,1,0.4)), pt.cex=2, pch=15)


## ----------------------------------------------------------------------------------------------------------
var.test(d$Fecha.de.la.actividad[b], d$Fecha.de.la.actividad[v])


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[v],
     breaks = 'months',
     main='Actividades por tipo y mes',
     col=rgb(0,1,1,0.4),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[p],
     breaks = 'months',
     main='',
     col=rgb(0,1,0,0.3),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
legend("topleft", legend=c("Entrenamiento con pesas", "Bicicleta virtual"), col=c(rgb(0,1,0,0.3), rgb(0,1,1,0.4)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
var.test(d$Fecha.de.la.actividad[v], d$Fecha.de.la.actividad[p])


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[p],
     breaks = 'months',
     main='Actividades por tipo y mes',
     col=rgb(0,1,0,0.3),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[e],
     breaks = 'months',
     main='',
     col=rgb(1,0,0,0.5),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
legend("topleft", legend=c("Entrenamiento con pesas", "Entrenamiento"), col=c(rgb(0,1,0,0.3), rgb(1,0,0,0.5)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
var.test(d$Fecha.de.la.actividad[p], d$Fecha.de.la.actividad[e])


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[b]),
        period_to_seconds(d$Tiempo.en.movimiento[c]),
        period_to_seconds(d$Tiempo.en.movimiento[v]),
        period_to_seconds(d$Tiempo.en.movimiento[e]),
        period_to_seconds(d$Tiempo.en.movimiento[c]),
     main='Actividades por tipo y duración',
     col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3), rgb(0,1,1,0.4), rgb(1,0,0,0.5), rgb(1,0,1,0.6)),
     ylab='Segundos')
legend("topright", legend=c("Bicicleta", "Caminata", "Bicicleta virtual", "Entrenamiento", "Caminata"), col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3), rgb(0,1,1,0.4), rgb(1,0,0,0.5), rgb(1,0,1,0.6)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[b]),
     main='Actividades de Bicicleta',
     col=rgb(0,0,1,0.2),
     ylab='Segundos')


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(period_to_seconds(d$Tiempo.en.movimiento[b]))


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[v]),
     main='Actividades de Bicicleta virtual',
     col=rgb(0,1,1,0.4),
     ylab='Segundos')


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(period_to_seconds(d$Tiempo.en.movimiento[v]))


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[p]),
     main='Actividades de Entrenamiento con pesas',
     col=rgb(0,1,0,0.3),
     ylab='Segundos')


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(period_to_seconds(d$Tiempo.en.movimiento[p]))


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[e]),
     main='Actividades de Entrenamiento',
     col=rgb(1,0,0,0.5),
     ylab='Segundos')


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(period_to_seconds(d$Tiempo.en.movimiento[e]))


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[c]),
     main='Actividades de Caminata',
     col=rgb(1,0,1,0.6),
     ylab='Segundos')


## ----------------------------------------------------------------------------------------------------------
nortest::lillie.test(period_to_seconds(d$Tiempo.en.movimiento[c]))


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[b]), period_to_seconds(d$Tiempo.en.movimiento[v]),
     main='Actividades por tipo y duración',
     col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3)),
     ylab='Km')
legend("topright", legend=c("Bicicleta", "Bicicleta virtual"), col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
var.test(period_to_seconds(d$Tiempo.en.movimiento[b]), period_to_seconds(d$Tiempo.en.movimiento[v]))


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[v]), period_to_seconds(d$Tiempo.en.movimiento[c]),
     main='Actividades por tipo y duración',
     col=c(rgb(0,1,0,0.3), rgb(1,0,1,0.6)),
     ylab='Km')
legend("topleft", legend=c("Bicicleta virtual", "Caminata"), col=c(rgb(0,1,0,0.3), rgb(1,0,1,0.6)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
var.test(period_to_seconds(d$Tiempo.en.movimiento[v]), period_to_seconds(d$Tiempo.en.movimiento[c]))


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[p]), period_to_seconds(d$Tiempo.en.movimiento[c]),
     main='Actividades por tipo y duración',
     col=c(rgb(0,1,0,0.3), rgb(1,0,1,0.6)),
     ylab='Km')
legend("topleft", legend=c("Entrenamiento con pesas", "Caminata"), col=c(rgb(0,1,0,0.3), rgb(1,0,1,0.6)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
var.test(period_to_seconds(d$Tiempo.en.movimiento[p]), period_to_seconds(d$Tiempo.en.movimiento[c]))


## ----------------------------------------------------------------------------------------------------------
ratio <- period_to_seconds(d$Tiempo.en.movimiento[!is.na(d$Calorías)]) * d$Ritmo.cardiaco.promedio[!is.na(d$Calorías)]
rl <- lm(d$Calorías[!is.na(d$Calorías)] ~ ratio)
ypred <- rl$fitted.values
plot(ratio,
     d$Calorías[!is.na(d$Calorías)],
     main = paste('Regresión lineal: Todos los tipos de actividades'),
     xlab = '(Ritmo cardíaco * Tiempo en movimiento)',
     ylab = 'Calorías',
     pch = 19,
     cex = .5,
     col = 'light blue',
     frame = FALSE)
lines(ratio,
      ypred,
      lwd = 2,
      col = 'orange')


## ----------------------------------------------------------------------------------------------------------
hist(d$Fecha.de.la.actividad[b],
     breaks = 'months',
     main='Actividades por tipo y mes',
     col=rgb(0,0,1,0.2),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xlab='Fecha',
     ylab='Nº de actividades',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[p],
     breaks = 'months',
     main='',
     col=rgb(0,1,0,0.3),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[v],
     breaks = 'months',
     main='',
     col=rgb(0,1,1,0.4),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
hist(d$Fecha.de.la.actividad[e],
     breaks = 'months',
     main='',
     col=rgb(1,0,0,0.5),
     xlim=c(xmin, xmax), ylim=c(0,ymax),
     xaxt='n',
     xlab='',
     ylab='',
     freq=TRUE);par(new=TRUE)
legend("topleft", legend=c("Bicicleta", "Entrenamiento con pesas", "Bicicleta virtual", "Entrenamiento"), col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3), rgb(0,1,1,0.4), rgb(1,0,0,0.5)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
boxplot(period_to_seconds(d$Tiempo.en.movimiento[b]),
        period_to_seconds(d$Tiempo.en.movimiento[c]),
        period_to_seconds(d$Tiempo.en.movimiento[v]),
        period_to_seconds(d$Tiempo.en.movimiento[e]),
        period_to_seconds(d$Tiempo.en.movimiento[c]),
     main='Actividades por tipo y duración',
     col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3), rgb(0,1,1,0.4), rgb(1,0,0,0.5), rgb(1,0,1,0.6)),
     ylab='Segundos')
legend("topright", legend=c("Bicicleta", "Caminata", "Bicicleta virtual", "Entrenamiento", "Caminata"), col=c(rgb(0,0,1,0.2), rgb(0,1,0,0.3), rgb(0,1,1,0.4), rgb(1,0,0,0.5), rgb(1,0,1,0.6)), pt.cex=2, pch=15 )


## ----------------------------------------------------------------------------------------------------------
# F test entre las actividades de Entrenamiento con pesas y Entrenamiento por fecha
var.test(d$Fecha.de.la.actividad[p], d$Fecha.de.la.actividad[e])

# F test entre las actividades de Entrenamiento con pesas y Caminata por tiempo efectivo
var.test(period_to_seconds(d$Tiempo.en.movimiento[p]), period_to_seconds(d$Tiempo.en.movimiento[c]))

# F test entre las actividades de Bicicleta virtual y Caminata por tiempo efectivo
var.test(period_to_seconds(d$Tiempo.en.movimiento[v]), period_to_seconds(d$Tiempo.en.movimiento[c]))


## ----------------------------------------------------------------------------------------------------------
write.csv(d,"ActivitiesClean.csv", row.names = FALSE)

