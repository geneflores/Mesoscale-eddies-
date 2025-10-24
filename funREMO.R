###------------------------------------------------------------------------------
###   ELABORAR GRÁFICOS Y PERFILES MEDIANTE FUNCIONES
###------------------------------------------------------------------------------

#Llamar paqueterías
pacman::p_load(ncdf4, R.utils, maps, pracma, ggplot2, 
               RColorBrewer, paletteer,animation, tidyverse,
               plotly)
#Paqueterías
library(ncdf4)
library(tidyverse)
library(oce)
library(ggExtra)
library(corrplot)
library(npphen)
library(xts)
library(scales)
library(ggpubr)
library(akima)
library(geosphere)
library(gridExtra)
library(png)
library(patchwork)
library(grid)
library(interp)
library(sf)

###------------------------------------------------------------------------------
###   REMOLINOS <<<< GRAFICAR ya sea CICLÓNICOS o ANTICICLÓNICOS
###------------------------------------------------------------------------------

##CICLÓNICOS 185
setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic")
alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Cyclonic_long_19930101_20220209.nc"))
nc <- nc_open(alist) #print(nc) -- displays nc file info 
#conlat <- ncvar_get(nc,'effective_contour_latitude')
#conlon <- ncvar_get(nc,'effective_contour_longitude')
time <- ncvar_get(nc,'time') #days since 1950-01-01 00:00:00 UTC
t1 <- as.Date(time, origin = "1950-01-01 00:00:00", tz='UTC')

load("~/Remolinos/META3.2DTallsat/Cyclonic/rem_cy_boys") #Ciclónicos 
load("~/Remolinos/META3.2DTallsat/Cyclonic/cy_contornos_latitud")
load("~/Remolinos/META3.2DTallsat/Cyclonic/cy_contornos_longitud")


##ANTICICLÓNICOS 180
setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Anticyclonic")
alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Anticyclonic_long_19930101_20220209.nc"))
nc <- nc_open(alist) #print(nc) -- displays nc file info 
#conlat <- ncvar_get(nc,'effective_contour_latitude')
#conlon <- ncvar_get(nc,'effective_contour_longitude')

load("~/Remolinos/META3.2DTallsat/Anticyclonic/rem_anticy_boys") #Anticiclónicos
load("~/Remolinos/META3.2DTallsat/Anticyclonic/anticy_contornos_latitud")
load("~/Remolinos/META3.2DTallsat/Anticyclonic/anticy_contornos_longitud")

###------------------------------
#conlat[conlat == 0] <- NA
#conlon[conlon == 180] <- NA
#save(conlat, file = "cy_contornos_latitud")
###------------------------------

remolino <- function(irb){
  
  load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
  
  x11()
  Conf2mas1 = matrix(c(1,1,1,1,1,1,1,1,1,1,1,1,
                       2,2,3,3,2,2,3,3),
                     nrow=4, byrow=F) 
  Conf2mas1
  layout(Conf2mas1)
  
  rem <- which(track == remboy[irb])
  
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
  par(mar = c(4.1,4.1,1.1,1.1))
  plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
  draw.map(); grid(col=1)
  lines(conlon[,rem]-360,conlat[,rem], col = 'blue4') 
  points(lon[rem]-360, lat[rem], cex=1, col='black') #Centros de cada polígono
  
  text(x = -65, y = 20, #Coordenadas 
       label = paste0("Ciclónico\n",
                      "Remolino Caribe",  irb, "\n",
                      "Remolino mundial:",  (remboy[irb]), "\n",
                      "Duración:",  sum(diff(t1[rem])), "días" ,"\n",
                      "Inicio:",  t1[rem[1]], "\n",
                      "Fin:",  t1[rem[length(rem)]]),
       col = "blue4",
       cex = 1.7)
  
  
  iboy <- which(track[ibuenor]==remboy[irb])
  buoy <- unique(ibuenob[iboy])
  print(buoy)
  points(lon1[buoy],lat1[buoy],pch=16, cex = 2, col='gray')
  
  fechas <- c(datos$time[ini1[buoy]])
  fechas_posix <- as.POSIXct(fechas, tz = "UTC")
  fechas_ord <- fechas_posix[order(fechas_posix)]
  
  
  colores <- c("darkorchid", "sienna1", "blue4", "red2", "green4",
               "black", "aquamarine2", "deeppink3", "darkorange4",
               "darkolivegreen2", "bisque2", "khaki3", "lightcyan3", "pink2")
  
  # Ordenar las fechas y obtener los índices
  indices_orden <- order(fechas_posix)
  # Aplicar los índices a los colores
  colores_ordenados <- colores[indices_orden]
  
  for (i in 1:length(buoy)){
    ib=i #indice de la boya coincidente 5-11
    points(lon1[buoy[ib]],lat1[buoy[ib]],pch=16, cex = 2, col= colores[i])
  }
  
  
  #TEMPERATURA 
 
primera_linea <- TRUE

for (i in 1:length(buoy)) {
  ib <- i
  datos_x <- datos$CT[ini1[buoy[ib]]:(ini[which(ini == ini1[buoy[ib]]) + 1] - 1)]
  datos_y <- -datos$pres[ini1[buoy[ib]]:(ini[which(ini == ini1[buoy[ib]]) + 1] - 1)]
  
  # Verificar si datos_x o datos_y contienen NA
  if (!any(is.na(datos_x)) && !any(is.na(datos_y))) {
    if (primera_linea) {
      plot(x = datos_x,
           y = datos_y,
           type = 'l', 
           xlab = 'CT(deg C)',
           ylab = 'Press(db)', 
           col = colores[i])
      primera_linea <- FALSE
    } else {
      lines(x = datos_x,
            y = datos_y,
            col = colores[i])
    }
  }
}
  
  #Agregar los demás perfiles 
 # for (i in 2:length(buoy)) {
  #  ib= i #indice de la boya coincidente 5-11
    #lines(datos$CT[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],
          #-datos$pres[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],'l', 
          #xlab='CT(deg C)',ylab='Press(db)', col = colores [i])
  
  
  fechas_for <- format(fechas_ord, "%Y-%m-%d")
  legend("bottomright", legend = paste0(fechas_for), 
         col = colores_ordenados, lty = 1, lwd = 3)
  
  
  #SALINIDAD
  
  #ib = 1
  #plot(datos$SA[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],
   #    -datos$pres[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],'l', 
    #   xlab='SA(g/kg)',ylab='Press(db)', col = 'darkorchid')
  
  #Agregar los demás perfiles
 # for(i in 2:length(buoy)){
   # ib = i
   # lines(datos$SA[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],
      #    -datos$pres[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],'l', 
       #  xlab='SA(g/kg)',ylab='Press(db)', col = colores[i])
  

  primera_linea <- TRUE
  
  for (i in 1:length(buoy)) {
    ib <- i
    datos_x <- datos$SA[ini1[buoy[ib]]:(ini[which(ini == ini1[buoy[ib]]) + 1] - 1)]
    datos_y <- -datos$pres[ini1[buoy[ib]]:(ini[which(ini == ini1[buoy[ib]]) + 1] - 1)]
    
    # Verificar si datos_x o datos_y contienen NA
    if (!any(is.na(datos_x)) && !any(is.na(datos_y))) {
      if (primera_linea) {
        plot(x = datos_x,
             y = datos_y,
             type = 'l', 
             xlab = 'SA(g/kg)',
             ylab = 'Press(db)', 
             col = colores[i])
        primera_linea <- FALSE
      } else {
        lines(x = datos_x,
              y = datos_y,
              col = colores[i])
      }
    }
  }
  
  
  legend("bottomright", legend = paste0(fechas_for), 
         col = colores_ordenados, lty = 1, lwd = 3)
  
}

remolino(145)


###------------------------------------------------------------------------------
###   TRANSECTO 
###------------------------------------------------------------------------------

### VER REMOLINO Y POLÍGONO DE INTERÉS

##LONGITUDES  
polilon <- function(r, f ){
  
  load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
  par(mar = c(4.1,4.1,1.1,1.1))
  plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
  draw.map(); grid(col=1)
  
  rem <- which(track==remboy[r])
  for (i in rem){
    lines(conlon[,i]-360, conlat[,i], col = 'aquamarine3')
  }
  
  fechas <- t1[rem]
  fecha_buscada = f
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  lines(conlon[,dat]-360, conlat[,dat], col = "red3")
  
  plonmax <- max(conlon[,dat]-360)
  poslonmax <- which.max(conlon[,dat]);  
  platmax <- conlat[poslonmax,dat]
  print(paste("Longitud máxima", plonmax))
  print(paste("Latitud correspondiente:", platmax))
  
  plonmin <- min(conlon[,dat]-360)
  poslonmin <- which.min(conlon[,dat]);  
  platmin <- conlat[poslonmin,dat]
  print(paste("Longitud mínima", plonmin))
  print(paste("Latitud correspondiente:", platmin))
  
  m <- (platmin - platmax)/(plonmin - plonmax)
  b <-  platmax - m * plonmax
  
  xcoor <- seq(min(conlon[,dat]-360)-1,max(conlon[,dat]-360)+0.6 , by = 0.01)
  ycoor <- m * xcoor + b 
  lines(xcoor,ycoor, cex = 5)
  
  list(x_valores = xcoor, 
       y_valores = ycoor)
  
}

##LATIDUDES 
polilat <- function(r, f ){
  
  load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
  par(mar = c(4.1,4.1,1.1,1.1))
  plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
  draw.map(); grid(col=1)
  
  rem <- which(track==remboy[r])
  for (i in rem){
    lines(conlon[,i]-360, conlat[,i], col = 'aquamarine3')
  }
  
  fechas <- t1[rem]
  fecha_buscada = f
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  lines(conlon[,dat]-360, conlat[,dat], col = "red3")
  
  platmax <- max(conlat[,dat])
  poslatmax <- which.max(conlat[,dat]);  
  plonmax <- conlon[poslatmax,dat]-360
  print(paste("Latitud máxima", platmax))
  print(paste("Longitud correspondiente:", plonmax))
  
  platmin <- min(conlat[,dat])
  poslatmin <- which.min(conlat[,dat]);  
  plonmin <- conlon[poslatmin,dat]-360
  print(paste("Latitud mínima", platmin))
  print(paste("Longitud correspondiente:", plonmin))
  
  m <- (platmin - platmax)/(plonmin - plonmax)
  b <-  platmax - m * plonmax
  
  ycoor <- seq(min(conlat[,dat])-2,max(conlat[,dat])+2, by = 0.01)
  xcoor <- (ycoor - b)/m
  lines(xcoor,ycoor, cex = 5)
  
  list(x_valores = xcoor, 
       y_valores = ycoor)
  
}

##LATITUD MÁXIMA - LONGITUD MÍNIMA
polilatlon <- function(r, f ){
  
  load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
  par(mar = c(4.1,4.1,1.1,1.1))
  plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
  draw.map(); grid(col=1)
  
  rem <- which(track==remboy[r])
  for (i in rem){
    lines(conlon[,i]-360, conlat[,i], col = 'aquamarine3')
  }
  
  fechas <- t1[rem]
  fecha_buscada = f
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  lines(conlon[,dat]-360, conlat[,dat], col = "red3")
  
  platmax <- max(conlat[,dat])
  poslatmax <- which.max(conlat[,dat]);  
  plonmax <- conlon[poslatmax,dat]-360
  print(paste("Latitud máxima", platmax))
  print(paste("Longitud correspondiente:", plonmax))
  
  plonmin <- min(conlon[,dat]-360)
  poslonmin <- which.min(conlon[,dat]);  
  platmin <- conlat[poslonmin,dat]
  print(paste("Longitud mínima", plonmin))
  print(paste("Latitud correspondiente:", platmin))
  
  m <- (platmin - platmax)/(plonmin - plonmax)
  b <-  platmax - m * plonmax
  
  xcoor <- seq(min(conlon[,dat]-360)-2,max(conlon[,dat]-360)+2, by = 0.01)
  ycoor <- m * xcoor + b
  lines(xcoor,ycoor, cex = 5)
  
  list(x_valores = xcoor, 
       y_valores = ycoor)
  
}

##LATITUD MÁXIMA - LONGITUD MÁXIMA 
polilatlonmax <- function(r, f ){
  
  load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
  par(mar = c(4.1,4.1,1.1,1.1))
  plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
  draw.map(); grid(col=1)
  
  rem <- which(track==remboy[r])
  for (i in rem){
    lines(conlon[,i]-360, conlat[,i], col = 'aquamarine3')
  }
  
  fechas <- t1[rem]
  fecha_buscada = f
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  lines(conlon[,dat]-360, conlat[,dat], col = "red3")
  
  platmax <- max(conlat[,dat])
  poslatmax <- which.max(conlat[,dat]);  
  plonmax <- conlon[poslatmax,dat]-360
  print(paste("Latitud máxima", platmax))
  print(paste("Longitud correspondiente:", plonmax))
  
  plonmax2 <- max(conlon[,dat]-360)
  poslonmax <- which.max(conlon[,dat]);  
  platmax2 <- conlat[poslonmax,dat]
  print(paste("Longitud máxima", plonmax2))
  print(paste("Latitud correspondiente:", platmax2))
  
  m <- (platmax - platmax2)/(plonmax - plonmax2)
  b <-  platmax - m * plonmax
  
  xcoor <- seq(min(conlon[,dat]-360)-4.3,max(conlon[,dat]-360)+0.8 , by = 0.01)
  ycoor <- m * xcoor + b
  lines(xcoor,ycoor, cex = 5)
  
  list(x_valores = xcoor, 
       y_valores = ycoor)
  
}

valores <- polilatlonmax( r = 135, f = "2019-11-18")

x_valores <- valores$x_valores
y_valores <- valores$y_valores


###------------------------------------------------------------------------------
###   MODELOS 
###------------------------------------------------------------------------------


########### GLORYS - CAPA DE MEZCLA

setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS2")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_capamezcla.nc"))
nc <- nc_open(alist) #print(nc) 

mixed <- ncvar_get(nc,'mlotst')
lonmix <- ncvar_get(nc, 'longitude')
latmix <- ncvar_get(nc, 'latitude')

#fecha_buscada <- "2020-01-06"

transect_data_mixed <- c()

for (i in 1:dim(mixed)[2]){
  transect_data_mixed <- cbind(transect_data_mixed, 
                             interp(x = lonmix, 
                                     y = latmix, 
                                     xp = x_valores, 
                                     yp = y_valores, 
                                     method = 'linear'))
}

palS <- colorRampPalette((c("cyan","#841859","darkolivegreen2","#005600")))
prof<-seq(min(depth),max(depth),length=dim(salinity)[3])-max(depth)
filled.contour(dis-max(dis)/2,prof,rot90(transect_data_sal,2),
               xlab='distance [Km]', ylab='depth [m]', 
               color.palette = palS)
#title( paste0("Salinidad", fechas[day]))
#title( paste0(fechas[day]))

title( paste0 (fechas[day]), cex.main = 3)

#######################################################
########### GLORYS - SALINIDAD Y TEMPERATURA ##########
#######################################################

setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_junio20.nc"))
nc <- nc_open(alist) #print(nc) 

# A PARTIR de POLILON y POLILATLON
perfil_latitudinal  <- function(day) {
  
  lon <- ncvar_get(nc,'longitude')  #size 360 E: -60.08 O: -89.9
  lat <- ncvar_get(nc,'latitude')   #sixe 205 N: 24     S:  7
  depth <- ncvar_get(nc,'depth')    #size 31  Max: 453.93
  time <- ncvar_get(nc,'time')      #size 31 - Mes de Octube 
  salinity <- ncvar_get(nc,'so')
  temperature <- ncvar_get(nc, 'thetao')
  
  fechas <- as.POSIXct(time, origin="1970-01-01", tz="UTC")
  
  salinidad <- salinity[,,,day]       
  temperatura <- temperature[,,,day]
  
  # SALINIDAD
  
  transect_data_sal <- c()
  
  for (i in 1:dim(salinidad)[3]){
    transect_data_sal <- cbind(transect_data_sal, 
                              interp2(x = lon, 
                                      y = lat, 
                                      Z = t(as.matrix(salinidad[,,i])),
                                                         xp = x_valores, 
                                                         yp = y_valores, 
                                                         method = 'linear'))
  }
  
  
  transect_data_temp <-c()
  
  for (i in 1:dim(temperatura)[3]){
    transect_data_temp <-cbind(transect_data_temp, 
                               interp2(x = lon, 
                                       y = lat, 
                                       Z = t(as.matrix(temperatura[,,i])),
                                                           xp = x_valores, 
                                                           yp = y_valores, 
                                                           method = 'linear'))
  }
  
  
  dis<-0
  for (k in 1:(length(x_valores)-1)){
    aux<-distm(c(x_valores[k],x_valores[k]),
               c(x_valores[k+1],x_valores[k+1]),
               fun = distHaversine)
    dis[k+1]<-dis[k] + aux
  }
  
  dis <- as.numeric(format(0.8+dis/1e3,digits=2))
  
  
  #Salinidad
  palS <- colorRampPalette((c("cyan","cornflowerblue","darkolivegreen2","darkslategray")))
  palS <- colorRampPalette((c("cyan","#841859","darkolivegreen2","#005600")))
  prof<-seq(min(depth),max(depth),length=dim(salinity)[3])-max(depth)
  
  png(paste0("Sal_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  filled.contour(dis-max(dis)/2,prof,rot90(transect_data_sal,2),
                 xlab='distance [Km]', ylab='depth [m]', 
                 color.palette = palS)
  #title( paste0("Salinidad", fechas[day]))
  #title( paste0(fechas[day]))
  
  title( paste0 (fechas[day]), cex.main = 3)
  #text(x = -150, y = -100, label = paste0("Salinidad"), col = "black", cex = 2)
  
  dev.off()
  
  
  palT <- colorRampPalette(c("#3E0689","#AE1987","#EA8D2D","#E3E400")) #temp
  prof<-seq(min(depth),max(depth),length=dim(temperature)[3])-max(depth)
  
  png(paste0("Temp_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  filled.contour(dis-max(dis)/2,prof,rot90(transect_data_temp,2),
                 xlab='distance [Km]', ylab='depth[m]',
                 color.palette = palT)
  #title(paste0("Temperatura", fechas[day]))
  #title(paste0(fechas[day]))
  title( paste0 (fechas[day]), cex.main = 3)
  #text(x = -100, y = -100, label = paste0("Temperatura"), col = "black", cex = 2)
  
  dev.off()
  
}

#CAMBIAR CARPETA DE ACUERDO AL REMOLINO
setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS/anticy151")

perfil_latitudinal(day = 31)

for(i in 1:18){
  # Usar tryCatch para manejar errores
  tryCatch({
    perfil_latitudinal(day = i)  # Ejecuta la función
  }, error = function(e) {
    message(paste("Error en el día", i, ": ", e$message))
    # El ciclo continuará sin detenerse si hay un error
  })
}


# Para unir perfiles -- NO es un paso necesario de esta rutina
{
plot1 <- readPNG("Sal_2020-04-14.png") 
plot2 <- readPNG("Sal_2020-05-10.png") 
plot3 <- readPNG("Sal_2020-06-02.png") 

plot4 <- readPNG("Temp_2020-04-14.png")
plot5 <- readPNG("Temp_2020-05-10.png")
plot6 <- readPNG("Temp_2020-06-02.png")

png(filename = "164st.png", width = 14, height = 10,
    units = "in", res = 200)

grid.arrange(rasterGrob(plot1), rasterGrob(plot2),
             rasterGrob(plot3), rasterGrob(plot4),
             rasterGrob(plot5), rasterGrob(plot6),
             ncol = 3, nrow = 2)

dev.off ()
}

# A PARTIR DE POLILAT - POLILATLONMAX
perfil_lonlat <- function(day) {
  
  #setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS")
  #alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_abril19.nc"))
  #nc <- nc_open(alist) #print(nc) 
  
  lon <- ncvar_get(nc,'longitude')  #size 360 E: -60.08 O: -89.9
  lat <- ncvar_get(nc,'latitude')   #sixe 205 N: 24     S:  7
  depth <- ncvar_get(nc,'depth')    #size 31  Max: 453.93
  time <- ncvar_get(nc,'time')      #size 31 - Mes de Octube 
  salinity <- ncvar_get(nc,'so')
  temperature <- ncvar_get(nc, 'thetao')
  
  fechas <- as.POSIXct(time, origin="1970-01-01", tz="UTC")
  
  salinidad <- salinity[,,,day]       
  temperatura <- temperature[,,,day]
  
  # SALINIDAD
  
  transect_data_sal <- c()
  
  for (i in 1:dim(salinidad)[3]){
    transect_data_sal <- cbind(transect_data_sal, 
                               interp2(x = lon, 
                                       y = lat, 
                                       Z = t(as.matrix(salinidad[,,i])),
                                       xp = x_valores, 
                                       yp = y_valores, 
                                       method = 'linear'))
  }
  
  
  transect_data_temp <-c()
  
  for (i in 1:dim(temperatura)[3]){
    transect_data_temp <-cbind(transect_data_temp, 
                               interp2(x = lon, 
                                       y = lat, 
                                       Z = t(as.matrix(temperatura[,,i])),
                                       xp = x_valores, 
                                       yp = y_valores, 
                                       method = 'linear'))
  }
  
  
  dis<-0
  for (k in 1:(length(y_valores)-1)){
    aux<-distm(c(y_valores[k],y_valores[k]),
               c(y_valores[k+1],y_valores[k+1]),
               fun = distHaversine)
    dis[k+1]<-dis[k] + aux
  }
  
  dis <- as.numeric(format(0.8+dis/1e3,digits=2))
  
  
  #Salinidad
  #palS <- colorRampPalette((c("cyan","cornflowerblue","darkolivegreen2","darkslategray")))
  palS <- colorRampPalette((c("cyan","#841859","darkolivegreen2","#005600")))
  prof<-seq(min(depth),max(depth),length=dim(salinity)[3])-max(depth)
  
  png(paste0("Sal_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  filled.contour(dis-max(dis)/2,prof,rot90(transect_data_sal,2),
                 xlab='distance [Km]', ylab='depth [m]', 
                 color.palette = palS)
  #title( paste0("Salinidad", fechas[day]))
  
  title( paste0 (fechas[day]), cex.main = 3)
  #text(x = -350, y = -85, label = paste0("Salinidad"), col = "black", cex = 2)
  
  dev.off()
  
  
  palT <- colorRampPalette(c("#3E0689","#AE1987","#EA8D2D","#E3E400")) #temp
  prof<-seq(min(depth),max(depth),length=dim(temperature)[3])-max(depth)
  
  png(paste0("Temp_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  filled.contour(dis-max(dis)/2,prof,rot90(transect_data_temp,2),
                 xlab='distance [Km]', ylab='depth[m]',
                 color.palette = palT)
  #title(paste0("Temperatura", fechas[day]))
  #text(x = -300, y = -100, label = paste0("Temperatura"), col = "black", cex = 2)
  
  dev.off()
  
  
}

setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS/anticy146")
resultado_lon <- perfil_lonlat(day = 21)

# Para unir perfiles -- NO es un paso necesario de esta rutina
{
plot1 <- readPNG("Sal_2020-01-12.png") 
plot2 <- readPNG("Sal_2020-02-21.png") 
plot3 <- readPNG("Sal_2020-04-01.png") 

plot4 <- readPNG("Temp_2020-01-12.png")
plot5 <- readPNG("Temp_2020-02-21.png")
plot6 <- readPNG("Temp_2020-04-01.png")

png(filename = "146st.png", width = 14, height = 10,
    units = "in", res = 200)

grid.arrange(rasterGrob(plot1), rasterGrob(plot2),
             rasterGrob(plot3), rasterGrob(plot4),
             rasterGrob(plot5), rasterGrob(plot6),
             ncol = 3, nrow = 2)

dev.off ()
}

#rm(list = ls())

####################################################
###############    HINCAST   #######################
####################################################


setwd("C:/Users/genef/Documents/Remolinos/CMEMS/HINDCAST")
alist <- dir(pattern = glob2rx("cmems_mod_glo_bgc_my_0.25deg_P1D-m_junio20.nc"))
nc <- nc_open(alist) #print(nc) 

perfil_biogeoquimico  <- function(day) {
  
  lon <- ncvar_get(nc,'longitude')  #size 360 E: -60.08 O: -89.9
  lat <- ncvar_get(nc,'latitude')   #sixe 205 N: 24     S:  7
  depth <- ncvar_get(nc,'depth')    #size 31  Max: 453.93
  time <- ncvar_get(nc,'time')      #size 31 - Mes de Octube 
  
  print(names(nc$var))
  clorofila <- ncvar_get(nc,'chl')
  nitratos <- ncvar_get(nc, 'no3')
  fitoplancton <- ncvar_get(nc, 'nppv')
  oxigeno_disuelto <- ncvar_get(nc, 'o2')
  fosfato <- ncvar_get(nc, 'po4')
  silicato_disuelto <- ncvar_get(nc, 'si')
  
  fechas <- as.POSIXct(time, origin="1970-01-01", tz="UTC")
  
  chl <- clorofila[,,,day]       
  no3 <- nitratos[,,,day]
   nppv <- fitoplancton[,,,day]
  o2 <- oxigeno_disuelto[,,,day]
  po4 <- fosfato[,,,day]
  si <- silicato_disuelto[,,,day]
  
  # CLOROFILA
  transect_data_chl <- c()
  
  for (i in 1:dim(chl)[3]){
    transect_data_chl <- cbind(transect_data_chl, 
                               interp2(x = lon, 
                                       y = lat, 
                                       Z = t(as.matrix(chl[,,i])),
                                       xp = x_valores, 
                                       yp = y_valores, 
                                       method = 'linear'))
  }
  
  # NITRATOS
  transect_data_no3 <-c()
  
  for (i in 1:dim(no3)[3]){
    transect_data_no3 <-cbind(transect_data_no3, 
                               interp2(x = lon, 
                                       y = lat, 
                                       Z = t(as.matrix(no3[,,i])),
                                       xp = x_valores, 
                                       yp = y_valores, 
                                       method = 'linear'))
  }
  
  # # FITOPLANCTON
  transect_data_nppv <-c()

  for (i in 1:dim(nppv)[3]){
    transect_data_nppv <-cbind(transect_data_nppv,
                              interp2(x = lon,
                                      y = lat,
                                      Z = t(as.matrix(nppv[,,i])),
                                      xp = x_valores,
                                      yp = y_valores,
                                      method = 'linear'))
  }
  
  # OXIGENO DISUELTO 
  transect_data_o2 <-c()
  
  for (i in 1:dim(o2)[3]){
    transect_data_o2 <-cbind(transect_data_o2, 
                             interp2(x = lon, 
                                     y = lat, 
                                     Z = t(as.matrix(o2[,,i])),
                                     xp = x_valores, 
                                     yp = y_valores, 
                                     method = 'linear'))
  }
  
  # FOSFATOS
  transect_data_po4 <-c()
  
  for (i in 1:dim(po4)[3]){
    transect_data_po4 <-cbind(transect_data_po4, 
                              interp2(x = lon, 
                                      y = lat, 
                                      Z = t(as.matrix(po4[,,i])),
                                      xp = x_valores, 
                                      yp = y_valores, 
                                      method = 'linear'))
  }
  
  # SILICATO DISUELTO 
  transect_data_si <-c()
  
  for (i in 1:dim(o2)[3]){
    transect_data_si <-cbind(transect_data_si, 
                             interp2(x = lon, 
                                     y = lat, 
                                     Z = t(as.matrix(si[,,i])),
                                     xp = x_valores, 
                                     yp = y_valores, 
                                     method = 'linear'))
  }
  
  dis<-0
  for (k in 1:(length(x_valores)-1)){
    aux<-distm(c(x_valores[k],x_valores[k]),
               c(x_valores[k+1],x_valores[k+1]),
               fun = distHaversine)
    dis[k+1]<-dis[k] + aux
  }
  
  dis <- as.numeric(format(0.8+dis/1e3,digits=2))
  
  
  #######  Clorofila
  
  palS <- colorRampPalette((c("#4B1D91","#B0179C","#F28265","#EBCB83")))
  prof<-seq(min(depth),max(depth),length=dim(chl)[3])-max(depth)
  
  png(paste0("Chl_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot1 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_chl,2),
                 xlab='distance [Km]', ylab='depth [m]', 
                 cex.lab = 1.5, cex.axis = 3,
                 color.palette = palS)
  
  title( paste0 (fechas[day]), cex.main = 3)
  #text(x = -250, y = -100, label = paste0("Clorofila"), col = "black", cex = 2)
   
  dev.off()

  #######   Nitratos 
  
  palT <- colorRampPalette(c("#FCECA8","#F9C783","#F08370","#E24C80")) 
  prof<-seq(min(depth),max(depth),length=dim(no3)[3])-max(depth)
  
  png(paste0("NO3_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot2 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_no3,2),
                 xlab='distance [Km]', ylab='depth[m]',
                 cex.lab = 1.5, cex.axis = 3,
                 color.palette = palT)
  title(paste0(fechas[day]), cex.main = 3)
  #text(x = -250, y = -100, label = paste0("Nitratos"), col = "black", cex = 2)
  
  dev.off()
  
  #######  Fitoplancton 
  
  palT <- colorRampPalette(c("#006C76", "#21A97E", "#94D268","#CDE55C"))
  prof<-seq(min(depth),max(depth),length=dim(nppv)[3])-max(depth)

  png(paste0("NPPV_", fechas[day], ".png"), type = "cairo",
      width = 1200, height = 800,
      units = "px", res = 165, pointsize = 12 )

  plot3 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_nppv,2),
                 xlab='distance [Km]', ylab='depth[m]',
                 cex.lab = 1.5, cex.axis = 3,
                 color.palette = palT)
  title(paste0(fechas[day]),cex.main = 3)
  #text(x = -200, y = -100, label = paste0("Fitoplancton"), col = "black", cex = 2)

  dev.off()
  
  ####### Oxigeno disuelto 
  
  palT <- colorRampPalette(c("#D6F3CF", "#56C8B2", "#006DAA","#26185F"))
  prof<-seq(min(depth),max(depth),length=dim(o2)[3])-max(depth)
  
  png(paste0("O2_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot4 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_o2,2),
                 xlab='distance [Km]', ylab='depth[m]',
                 cex.lab = 1.5, cex.axis = 3,
                 color.palette = palT)
  title(paste0(fechas[day]), cex.main = 3)
  #text(x = -250, y = -150, label = paste0("Oxígeno", "\n", "disuelto"), col = "black", cex = 2)
  
  dev.off()
  
  ######   Fosfatos 
  
  palT <- colorRampPalette(c("#D1EEEC", "#69C2A7", "#008D50","#1C4E1C"))
  prof<-seq(min(depth),max(depth),length=dim(po4)[3])-max(depth)
  
  png(paste0("PO4_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot5 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_po4,2),
                 xlab='distance [Km]', ylab='depth[m]',
                 cex.lab = 1.5, cex.axis = 3,
                 color.palette = palT)
  title(paste0(fechas[day]), cex.main = 3)
  #text(x = -250, y = -100, label = paste0("Fosfatos"), col = "black", cex = 2)
  
  dev.off()
  
  ######  Silicato disuelto 
  
  palT <- colorRampPalette(c("#D8EAF1", "#97A8D1", "#7D3991","#540046"))
  prof<-seq(min(depth),max(depth),length=dim(si)[3])-max(depth)
  
  png(paste0("SI_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot6 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_si,2),
                 xlab='distance [Km]', ylab='depth[m]',
                 cex.lab = 1.5, cex.axis = 3,
                 color.palette = palT)
  title(paste0(fechas[day]), cex.main = 3)
  #text(x = -250, y = -150, label = paste0("Silicato", "\n", "disuelto"), col = "black", cex = 2)
  dev.off()
  

}

setwd("C:/Users/genef/Documents/Remolinos/CMEMS/HINDCAST/anticy151")
#perfil_biogeoquimico(day = 31 )

for(i in 1:18){
  # Usar tryCatch para manejar errores
  tryCatch({
    perfil_biogeoquimico(day = i)  # Ejecuta la función
  }, error = function(e) {
    message(paste("Error en el día", i, ": ", e$message))
    # El ciclo continuará sin detenerse si hay un error
  })
}

# Para unir perfiles -- NO es un paso necesario de esta rutina
{
plot1 <- readPNG("Chl_2020-01-12.png") 
plot2 <- readPNG("Chl_2020-02-21.png") 
plot3 <- readPNG("Chl_2020-04-01.png") 

plot4 <- readPNG("NO3_2020-01-12.png")
plot5 <- readPNG("NO3_2020-02-21.png")
plot6 <- readPNG("NO3_2020-04-01.png")

plot7 <- readPNG("NPPV_2020-01-12.png")
plot8 <- readPNG("NPPV_2020-02-21.png")
plot9 <- readPNG("NPPV_2020-04-01.png")

plot10 <- readPNG("O2_2020-01-12.png")
plot11 <- readPNG("O2_2020-02-21.png")
plot12 <- readPNG("O2_2020-04-01.png")

plot13 <- readPNG("PO4_2020-01-12.png")
plot14 <- readPNG("PO4_2020-02-21.png")
plot15 <- readPNG("PO4_2020-04-01.png")

plot16 <- readPNG("SI_2020-01-12.png")
plot17 <- readPNG("SI_2020-02-21.png")
plot18 <- readPNG("SI_2020-04-01.png")


png(filename = "146biogeo1.png", width = 14, height = 10,
    units = "in", res = 300)

grid.arrange(rasterGrob(plot1), rasterGrob(plot2),
             rasterGrob(plot3), rasterGrob(plot4),
             rasterGrob(plot5), rasterGrob(plot6),
             rasterGrob(plot7), rasterGrob(plot8),
             rasterGrob(plot9), 
             ncol = 3, nrow = 3)

dev.off ()


png(filename = "146biogeo2.png", width = 14, height = 10,
    units = "in", res = 300)

grid.arrange(rasterGrob(plot10),
             rasterGrob(plot11), rasterGrob(plot12), 
             rasterGrob(plot13), rasterGrob(plot14),
             rasterGrob(plot15), rasterGrob(plot16),
             rasterGrob(plot17), rasterGrob(plot18), 
             ncol = 3, nrow = 3)

dev.off ()

}
