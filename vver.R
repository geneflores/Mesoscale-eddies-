#################################################################
##   CALCULAR DIRECCIÓN VERTICAL DE LA VELOCIDAD               ##
##   Y SOBREPONER CAMPO DE DIRECCIONES HORIZONTALES            ##
#################################################################


###### ESTE CÓDIGO GENERA UN MAPA POR CADA FECHA/ETAPA

#Cargar paqueterías
{ pacman::p_load(ncdf4, R.utils, maps, pracma, ggplot2, 
                 RColorBrewer, paletteer,animation, tidyverse,
                 plotly)
  library(R.utils)
  library(maps)
  library(pracma)
  library(ggplot2)
  library(RColorBrewer)
  library(paletteer)
  library(animation)
  library(plotly)
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
  library(sf)}

# Elegir remolinos ciclónicos o anticiclónicos 
{
  ## CICLÓNICOS 185
  setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic")
  alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Cyclonic_long_19930101_20220209.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  time <- ncvar_get(nc,'time') #days since 1950-01-01 00:00:00 UTC
  t1 <- as.Date(time, origin = "1950-01-01 00:00:00", tz='UTC')
  
  load("~/Remolinos/META3.2DTallsat/Cyclonic/rem_cy_boys") #Ciclónicos 
  load("~/Remolinos/META3.2DTallsat/Cyclonic/cy_contornos_latitud")
  load("~/Remolinos/META3.2DTallsat/Cyclonic/cy_contornos_longitud")
  
  
  ##ANTICICLÓNICOS 180
  setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Anticyclonic")
  alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Anticyclonic_long_19930101_20220209.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  load("~/Remolinos/META3.2DTallsat/Anticyclonic/rem_anticy_boys") #Anticiclónicos
  load("~/Remolinos/META3.2DTallsat/Anticyclonic/anticy_contornos_latitud")
  load("~/Remolinos/META3.2DTallsat/Anticyclonic/anticy_contornos_longitud")
}  

# Llamar funciones personalizadas 
setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
source("filled.contour4.R")
source("filled.legend.R")
#Contorno de países
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}



#### GENERAR CICLÓNICO 145 

#Nombrar archivo 
  nombreArchivo <- paste0("velocidad/", "vv2_145.png")
  unidades <- c("m/s")
  
      png(file = nombreArchivo, width = 8, height = 10, units = "in", res = 500)
  
  # Ajuste general de márgenes y configuración de layout
  
  par(mar = c(4, 4, 2, 2), 
      oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos

#FECHA 1 -- 28-NOV-2019
{

  #Cargar datos de velocidad de fecha 1
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
  load("rem145_vv_f1.RData")
  
######### GRÁFICO 1 - Primer `filled.contour`
par(new = "TRUE", 
    plt = c(0.05, 0.80, 0.7, 0.9), 
    las = 1, 
    cex = 1)

###DATOS DE FECHA 1 
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_noviembre19.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  east <- ncvar_get(nc,'uo')
  north <- ncvar_get(nc, 'vo')
  
  ue <-  east[,,1,28]  # [Latitud, longitud, prof, tiempo]
  vn <- north[,,1,28]
  
  LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
  LA <- ncvar_get(nc,'latitude') 
  
  LOn <- head(LO, -1)
  LAn <- head(LA, -1)
  
  LO2 <- rbind(replicate(205,LO))
  LA2 <- t(rbind(replicate(384, LA)))
  
  
  r <- 145
  rem <- which(track==remboy[r])
  
  fechas <- t1[rem]
  fecha_buscada = "2019-11-28"
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]

p <- 25 #distacia de flechas
palV2 <- colorRampPalette((c("#9C0824","#F6796A", "white","#4F98C4","#26456E")))
#palV2 <- colorRampPalette((c("white","#76C1DF", "#4993C0","#1F5591","#26456E")))

#BARRA DE ESCALA 
lat_media <- mean(LAn)
escala_km <- 200
escala_grados <- escala_km / (111 * cos(pi * lat_media / 180))

filled.contour4(x = LOn, y = LAn, z = v_vertical,
               xlab='Longitud', ylab='Latitud', 
               color.palette = palV2,
               plot.title = title(main = "28-Nov-2019",
                                  cex.main = 1),
               plot.axes = {axis(1); axis(2)
                 #Líneas de latitud
                 abline (h = c(10,15,20),
                         col = "gray", lty = 2)
                 
                 #Líneas de longitud
                 abline(v = seq(min(LOn), max(LOn), by = 5), 
                        col = "gray", lty = 2)
                 
                 #Vectores de velocidad
                 quiver(x = LO2[seq(1, length(LO2), p)],
                        y = LA2[seq(1, length(LA2), p)],
                        u = ue [seq(1, length(ue), p)],
                        v = vn [seq(1, length(vn), p)], 
                        scale = 0.05, angle = 10)
                 
                 #Contornos de polígono del remolino
                 lines(conlon[,dat]-360, conlat[,dat], 
                       col = "#DE0093", lwd = 3.5)
                 
                 # Contorno de países
                 draw.map()
                 grid(col = "gray50")
                 
                 # --- Rosa de los vientos ---
                 xN <- max(LOn) - 5     # posición en X
                 yN <- min(LAn) + 0.5   # posición en Y
                 arrows(x0 = xN, y0 = yN, x1 = xN, y1 = yN + 0.4,
                        length = 0.08, lwd = 2, col = "black")
                 text(xN, yN + 0.8, "N", cex = 0.7, font = 2)
                 
                 # Dibuja una barra de escala
                 x0 <- max(LOn) - 9 
                 y0 <- min(LAn) + 1
                 segments(x0, y0 + 0.5, x0 + escala_grados, y0 + 0.5, lwd = 3)
                 text(x0, y0 - 0.3, "0", cex = 0.8)
                 text(x0 + escala_grados, y0 - 0.3, "200 km", cex = 0.65)
                 
                 text(x = -88.5, y = 20,  font = 2,paste("(A)"))
               },
               
)


################## LEYENDA para el gráfico 1

par(new = "TRUE", 
    plt = c(0.81, 0.83, 0.7, 0.9), 
    las = 1,
    cex = 1)

filled.legend(x = 1, y = 0.5,
              color = palV2,
              cex = 0.8)
mtext(unidades, side = 4, line = 2.5, cex = 1, las = 3)

rm(alist, nc, east, north, ue, vn, LO, LA, LOn, 
   LAn, LO2, LA2, lat_media, escala_grados, 
   r, rem, fechas, fecha_buscada, posicion, dat)

}

#FECHA 2 -- 10-ENE-2020
{
  
  #Cargar datos de velocidad de fecha 1
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
  load("rem145_vv_f2.RData")
  
  ######### GRÁFICO 2 - segundo `filled.contour`
  par(new = "TRUE", 
      plt = c(0.05, 0.80, 0.4, 0.6), 
      las = 1, 
      cex = 1)
  
  ###DATOS DE FECHA 2
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_enero20.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  east <- ncvar_get(nc,'uo')
  north <- ncvar_get(nc, 'vo')
  
  ue <-  east[,,1,10]  # [Latitud, longitud, prof, tiempo]
  vn <- north[,,1,10]
  
  LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
  LA <- ncvar_get(nc,'latitude') 
  
  LOn <- head(LO, -1)
  LAn <- head(LA, -1)
  
  LO2 <- rbind(replicate(205,LO))
  LA2 <- t(rbind(replicate(384, LA)))
  
  
  r <- 145
  rem <- which(track==remboy[r])
  
  fechas <- t1[rem]
  fecha_buscada = "2020-01-10"
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  
  p <- 25 #distacia de flechas
  palV2 <- colorRampPalette((c("#9C0824","#F6796A", "white","#4F98C4","#26456E")))
  #palV2 <- colorRampPalette((c("white","#76C1DF", "#4993C0","#1F5591","#26456E")))
  
  #BARRA DE ESCALA 
  lat_media <- mean(LAn)
  escala_km <- 200
  escala_grados <- escala_km / (111 * cos(pi * lat_media / 180))
  
  filled.contour4(x = LOn, y = LAn, z = v_vertical,
                  xlab='Longitud', ylab='Latitud', 
                  color.palette = palV2,
                  plot.title = title(main = "10-Ene-2020",
                                     cex.main = 1),
                  plot.axes = {axis(1); axis(2)
                    #Líneas de latitud
                    abline (h = c(10,15,20),
                            col = "gray", lty = 2)
                    
                    #Líneas de longitud
                    abline(v = seq(min(LOn), max(LOn), by = 5), 
                           col = "gray", lty = 2)
                    
                    #Vectores de velocidad
                    quiver(x = LO2[seq(1, length(LO2), p)],
                           y = LA2[seq(1, length(LA2), p)],
                           u = ue [seq(1, length(ue), p)],
                           v = vn [seq(1, length(vn), p)], 
                           scale = 0.05, angle = 10)
                    
                    #Contornos de polígono del remolino
                    lines(conlon[,dat]-360, conlat[,dat], 
                          col = "#DE0093", lwd = 3.5)
                    
                    # Contorno de países
                    draw.map()
                    grid(col = "gray50")
                    
                    # --- Rosa de los vientos ---
                    xN <- max(LOn) - 5     # posición en X
                    yN <- min(LAn) + 0.5   # posición en Y
                    arrows(x0 = xN, y0 = yN, x1 = xN, y1 = yN + 0.4,
                           length = 0.08, lwd = 2, col = "black")
                    text(xN, yN + 0.8, "N", cex = 0.7, font = 2)
                    
                    # Dibuja una barra de escala
                    x0 <- max(LOn) - 9 
                    y0 <- min(LAn) + 1
                    segments(x0, y0 + 0.5, x0 + escala_grados, y0 + 0.5, lwd = 3)
                    text(x0, y0 - 0.3, "0", cex = 0.8)
                    text(x0 + escala_grados, y0 - 0.3, "200 km", cex = 0.65)
                    
                    text(x = -88.5, y = 20,  font = 2,paste("(B)"))
                  },
                  
  )
  
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.81, 0.83, 0.4, 0.6), 
      las = 1,
      cex = 1)
  
  filled.legend(x = 1, y = 0.5,
                color = palV2,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 1, las = 3)
  
  rm(alist, nc, east, north, ue, vn, LO, LA, LOn, 
     LAn, LO2, LA2, lat_media, escala_grados, 
     r, rem, fechas, fecha_buscada, posicion, dat)
  
}

#FECHA 3 -- 08-FEB-2020
{
  
  #Cargar datos de velocidad de fecha 1
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
  load("rem145_vv_f3.RData")
  
  ######### GRÁFICO 2 - segundo `filled.contour`
  par(new = "TRUE", 
      plt = c(0.05, 0.80, 0.1, 0.3), 
      las = 1, 
      cex = 1)
  
  ###DATOS DE FECHA 2
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_febrero20.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  east <- ncvar_get(nc,'uo')
  north <- ncvar_get(nc, 'vo')
  
  ue <-  east[,,1,08]  # [Latitud, longitud, prof, tiempo]
  vn <- north[,,1,08]
  
  LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
  LA <- ncvar_get(nc,'latitude') 
  
  LOn <- head(LO, -1)
  LAn <- head(LA, -1)
  
  LO2 <- rbind(replicate(205,LO))
  LA2 <- t(rbind(replicate(384, LA)))
  
  
  r <- 145
  rem <- which(track==remboy[r])
  
  fechas <- t1[rem]
  fecha_buscada = "2020-02-08"
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  
  p <- 25 #distacia de flechas
  palV2 <- colorRampPalette((c("#9C0824","#F6796A", "white","#4F98C4","#26456E")))
  #palV2 <- colorRampPalette((c("white","#76C1DF", "#4993C0","#1F5591","#26456E")))
  
  #BARRA DE ESCALA 
  lat_media <- mean(LAn)
  escala_km <- 200
  escala_grados <- escala_km / (111 * cos(pi * lat_media / 180))
  
  filled.contour4(x = LOn, y = LAn, z = v_vertical,
                  xlab='Longitud', ylab='Latitud', 
                  color.palette = palV2,
                  plot.title = title(main = "08-Feb-2020",
                                     cex.main = 1),
                  plot.axes = {axis(1); axis(2)
                    #Líneas de latitud
                    abline (h = c(10,15,20),
                            col = "gray", lty = 2)
                    
                    #Líneas de longitud
                    abline(v = seq(min(LOn), max(LOn), by = 5), 
                           col = "gray", lty = 2)
                    
                    #Vectores de velocidad
                    quiver(x = LO2[seq(1, length(LO2), p)],
                           y = LA2[seq(1, length(LA2), p)],
                           u = ue [seq(1, length(ue), p)],
                           v = vn [seq(1, length(vn), p)], 
                           scale = 0.05, angle = 10)
                    
                    #Contornos de polígono del remolino
                    lines(conlon[,dat]-360, conlat[,dat], 
                          col = "#DE0093", lwd = 3.5)
                    
                    # Contorno de países
                    draw.map()
                    grid(col = "gray50")
                    
                    # --- Rosa de los vientos ---
                    xN <- max(LOn) - 5     # posición en X
                    yN <- min(LAn) + 0.5   # posición en Y
                    arrows(x0 = xN, y0 = yN, x1 = xN, y1 = yN + 0.4,
                           length = 0.08, lwd = 2, col = "black")
                    text(xN, yN + 0.8, "N", cex = 0.7, font = 2)
                    
                    # Dibuja una barra de escala
                    x0 <- max(LOn) - 9 
                    y0 <- min(LAn) + 1
                    segments(x0, y0 + 0.5, x0 + escala_grados, y0 + 0.5, lwd = 3)
                    text(x0, y0 - 0.3, "0", cex = 0.8)
                    text(x0 + escala_grados, y0 - 0.3, "200 km", cex = 0.65)
                    
                    text(x = -88.5, y = 20,  font = 2,paste("(C)"))
                  },
                  
  )
  
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.81, 0.83, 0.1, 0.3), 
      las = 1,
      cex = 1)
  
  filled.legend(x = 1, y = 0.5,
                color = palV2,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 1, las = 3)
  
  rm(alist, nc, east, north, ue, vn, LO, LA, LOn, 
     LAn, LO2, LA2, lat_media, escala_grados, 
     r, rem, fechas, fecha_buscada, posicion, dat)
  
}

dev.off()

########################################################################

#### GENERAR ANTICICLÓNICO 151


#Nombrar archivo 
nombreArchivo <- paste0("velocidad/", "vv2_151.png")
unidades <- c("m/s")

png(file = nombreArchivo, width = 8, height = 10, units = "in", res = 500)

# Ajuste general de márgenes y configuración de layout

par(mar = c(4, 4, 2, 2), 
    oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos

#FECHA 1 -- 04-ABR-2020
{
  
  #Cargar datos de velocidad de fecha 1
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
  load("rem151_vv_f1.RData")
  
  ######### GRÁFICO 1 - Primer `filled.contour`
  par(new = "TRUE", 
      plt = c(0.05, 0.80, 0.7, 0.9), 
      las = 1, 
      cex = 1)
  
  ###DATOS DE FECHA 1 
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_abril20.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  east <- ncvar_get(nc,'uo')
  north <- ncvar_get(nc, 'vo')
  
  ue <-  east[,,1,21]  # [Latitud, longitud, prof, tiempo]
  vn <- north[,,1,21]
  
  LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
  LA <- ncvar_get(nc,'latitude') 
  
  LOn <- head(LO, -1)
  LAn <- head(LA, -1)
  
  LO2 <- rbind(replicate(205,LO))
  LA2 <- t(rbind(replicate(384, LA)))
  
  
  r <- 151
  rem <- which(track==remboy[r])
  
  fechas <- t1[rem]
  fecha_buscada = "2020-04-21"
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  
  p <- 25 #distacia de flechas
  palV2 <- colorRampPalette((c("#9C0824","#F6796A", "white","#4F98C4","#26456E")))
  #palV2 <- colorRampPalette((c("white","#76C1DF", "#4993C0","#1F5591","#26456E")))
  
  #BARRA DE ESCALA 
  lat_media <- mean(LAn)
  escala_km <- 200
  escala_grados <- escala_km / (111 * cos(pi * lat_media / 180))
  
  filled.contour4(x = LOn, y = LAn, z = v_vertical,
                  xlab='Longitud', ylab='Latitud', 
                  color.palette = palV2,
                  plot.title = title(main = "21-Abr-2020",
                                     cex.main = 1),
                  plot.axes = {axis(1); axis(2)
                    #Líneas de latitud
                    abline (h = c(10,15,20),
                            col = "gray", lty = 2)
                    
                    #Líneas de longitud
                    abline(v = seq(min(LOn), max(LOn), by = 5), 
                           col = "gray", lty = 2)
                    
                    #Vectores de velocidad
                    quiver(x = LO2[seq(1, length(LO2), p)],
                           y = LA2[seq(1, length(LA2), p)],
                           u = ue [seq(1, length(ue), p)],
                           v = vn [seq(1, length(vn), p)], 
                           scale = 0.05, angle = 10)
                    
                    #Contornos de polígono del remolino
                    lines(conlon[,dat]-360, conlat[,dat], 
                          col = "#DE0093", lwd = 3.5)
                    
                    # Contorno de países
                    draw.map()
                    grid(col = "gray50")
                    
                    # --- Rosa de los vientos ---
                    xN <- max(LOn) - 5     # posición en X
                    yN <- min(LAn) + 0.5   # posición en Y
                    arrows(x0 = xN, y0 = yN, x1 = xN, y1 = yN + 0.4,
                           length = 0.08, lwd = 2, col = "black")
                    text(xN, yN + 0.8, "N", cex = 0.7, font = 2)
                    
                    # Dibuja una barra de escala
                    x0 <- max(LOn) - 9 
                    y0 <- min(LAn) + 1
                    segments(x0, y0 + 0.5, x0 + escala_grados, y0 + 0.5, lwd = 3)
                    text(x0, y0 - 0.3, "0", cex = 0.8)
                    text(x0 + escala_grados, y0 - 0.3, "200 km", cex = 0.65)
                    
                    text(x = -88.5, y = 20,  font = 2,paste("(A)"))
                  },
                  
  )
  
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.81, 0.83, 0.7, 0.9), 
      las = 1,
      cex = 1)
  
  filled.legend(x = 1, y = 0.5,
                color = palV2,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 1, las = 3)
  
  rm(alist, nc, east, north, ue, vn, LO, LA, LOn, 
     LAn, LO2, LA2, lat_media, escala_grados, 
     r, rem, fechas, fecha_buscada, posicion, dat)
  
}

#FECHA 2 -- 10-MAY-2020
{
  
  #Cargar datos de velocidad de fecha 1
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
  load("rem151_vv_f2.RData")
  
  ######### GRÁFICO 2 - segundo `filled.contour`
  par(new = "TRUE", 
      plt = c(0.05, 0.80, 0.4, 0.6), 
      las = 1, 
      cex = 1)
  
  ###DATOS DE FECHA 2
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_mayo20.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  east <- ncvar_get(nc,'uo')
  north <- ncvar_get(nc, 'vo')
  
  ue <-  east[,,1,10]  # [Latitud, longitud, prof, tiempo]
  vn <- north[,,1,10]
  
  LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
  LA <- ncvar_get(nc,'latitude') 
  
  LOn <- head(LO, -1)
  LAn <- head(LA, -1)
  
  LO2 <- rbind(replicate(205,LO))
  LA2 <- t(rbind(replicate(384, LA)))
  
  
  r <- 151
  rem <- which(track==remboy[r])
  
  fechas <- t1[rem]
  fecha_buscada = "2020-05-10"
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  
  p <- 25 #distacia de flechas
  palV2 <- colorRampPalette((c("#9C0824","#F6796A", "white","#4F98C4","#26456E")))
  #palV2 <- colorRampPalette((c("white","#76C1DF", "#4993C0","#1F5591","#26456E")))
  
  #BARRA DE ESCALA 
  lat_media <- mean(LAn)
  escala_km <- 200
  escala_grados <- escala_km / (111 * cos(pi * lat_media / 180))
  
  filled.contour4(x = LOn, y = LAn, z = v_vertical,
                  xlab='Longitud', ylab='Latitud', 
                  color.palette = palV2,
                  plot.title = title(main = "10-May-2020",
                                     cex.main = 1),
                  plot.axes = {axis(1); axis(2)
                    #Líneas de latitud
                    abline (h = c(10,15,20),
                            col = "gray", lty = 2)
                    
                    #Líneas de longitud
                    abline(v = seq(min(LOn), max(LOn), by = 5), 
                           col = "gray", lty = 2)
                    
                    #Vectores de velocidad
                    quiver(x = LO2[seq(1, length(LO2), p)],
                           y = LA2[seq(1, length(LA2), p)],
                           u = ue [seq(1, length(ue), p)],
                           v = vn [seq(1, length(vn), p)], 
                           scale = 0.05, angle = 10)
                    
                    #Contornos de polígono del remolino
                    lines(conlon[,dat]-360, conlat[,dat], 
                          col = "#DE0093", lwd = 3.5)
                    
                    # Contorno de países
                    draw.map()
                    grid(col = "gray50")
                    
                    # --- Rosa de los vientos ---
                    xN <- max(LOn) - 5     # posición en X
                    yN <- min(LAn) + 0.5   # posición en Y
                    arrows(x0 = xN, y0 = yN, x1 = xN, y1 = yN + 0.4,
                           length = 0.08, lwd = 2, col = "black")
                    text(xN, yN + 0.8, "N", cex = 0.7, font = 2)
                    
                    # Dibuja una barra de escala
                    x0 <- max(LOn) - 9 
                    y0 <- min(LAn) + 1
                    segments(x0, y0 + 0.5, x0 + escala_grados, y0 + 0.5, lwd = 3)
                    text(x0, y0 - 0.3, "0", cex = 0.8)
                    text(x0 + escala_grados, y0 - 0.3, "200 km", cex = 0.65)
                    
                    text(x = -88.5, y = 20,  font = 2,paste("(B)"))
                  },
                  
  )
  
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.81, 0.83, 0.4, 0.6), 
      las = 1,
      cex = 1)
  
  filled.legend(x = 1, y = 0.5,
                color = palV2,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 1, las = 3)
  
  rm(alist, nc, east, north, ue, vn, LO, LA, LOn, 
     LAn, LO2, LA2, lat_media, escala_grados, 
     r, rem, fechas, fecha_buscada, posicion, dat)
  
}

#FECHA 3 -- 07-JUN-2020
{
  
  #Cargar datos de velocidad de fecha 1
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
  load("rem151_vv_f3.RData")
  
  ######### GRÁFICO 2 - segundo `filled.contour`
  par(new = "TRUE", 
      plt = c(0.05, 0.80, 0.1, 0.3), 
      las = 1, 
      cex = 1)
  
  ###DATOS DE FECHA 2
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_junio20.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  east <- ncvar_get(nc,'uo')
  north <- ncvar_get(nc, 'vo')
  
  ue <-  east[,,1,07]  # [Latitud, longitud, prof, tiempo]
  vn <- north[,,1,07]
  
  LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
  LA <- ncvar_get(nc,'latitude') 
  
  LOn <- head(LO, -1)
  LAn <- head(LA, -1)
  
  LO2 <- rbind(replicate(205,LO))
  LA2 <- t(rbind(replicate(384, LA)))
  
  
  r <- 151
  rem <- which(track==remboy[r])
  
  fechas <- t1[rem]
  fecha_buscada = "2020-06-07"
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  
  p <- 25 #distacia de flechas
  palV2 <- colorRampPalette((c("#9C0824","#F6796A", "white","#4F98C4","#26456E")))
  #palV2 <- colorRampPalette((c("white","#76C1DF", "#4993C0","#1F5591","#26456E")))
  
  #BARRA DE ESCALA 
  lat_media <- mean(LAn)
  escala_km <- 200
  escala_grados <- escala_km / (111 * cos(pi * lat_media / 180))
  
  filled.contour4(x = LOn, y = LAn, z = v_vertical,
                  xlab='Longitud', ylab='Latitud', 
                  color.palette = palV2,
                  plot.title = title(main = "07-Jun-2020",
                                     cex.main = 1),
                  plot.axes = {axis(1); axis(2)
                    #Líneas de latitud
                    abline (h = c(10,15,20),
                            col = "gray", lty = 2)
                    
                    #Líneas de longitud
                    abline(v = seq(min(LOn), max(LOn), by = 5), 
                           col = "gray", lty = 2)
                    
                    #Vectores de velocidad
                    quiver(x = LO2[seq(1, length(LO2), p)],
                           y = LA2[seq(1, length(LA2), p)],
                           u = ue [seq(1, length(ue), p)],
                           v = vn [seq(1, length(vn), p)], 
                           scale = 0.05, angle = 10)
                    
                    #Contornos de polígono del remolino
                    lines(conlon[,dat]-360, conlat[,dat], 
                          col = "#DE0093", lwd = 3.5)
                    
                    # Contorno de países
                    draw.map()
                    grid(col = "gray50")
                    
                    # --- Rosa de los vientos ---
                    xN <- max(LOn) - 5     # posición en X
                    yN <- min(LAn) + 0.5   # posición en Y
                    arrows(x0 = xN, y0 = yN, x1 = xN, y1 = yN + 0.4,
                           length = 0.08, lwd = 2, col = "black")
                    text(xN, yN + 0.8, "N", cex = 0.7, font = 2)
                    
                    # Dibuja una barra de escala
                    x0 <- max(LOn) - 9 
                    y0 <- min(LAn) + 1
                    segments(x0, y0 + 0.5, x0 + escala_grados, y0 + 0.5, lwd = 3)
                    text(x0, y0 - 0.3, "0", cex = 0.8)
                    text(x0 + escala_grados, y0 - 0.3, "200 km", cex = 0.65)
                    
                    text(x = -88.5, y = 20,  font = 2,paste("(C)"))
                  },
                  
  )
  
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.81, 0.83, 0.1, 0.3), 
      las = 1,
      cex = 1)
  
  filled.legend(x = 1, y = 0.5,
                color = palV2,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 1, las = 3)
  
  rm(alist, nc, east, north, ue, vn, LO, LA, LOn, 
     LAn, LO2, LA2, lat_media, escala_grados, 
     r, rem, fechas, fecha_buscada, posicion, dat)
  
}

dev.off()
