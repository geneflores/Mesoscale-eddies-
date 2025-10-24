###############################################################
#### DENSIDAD POTENCIAL CON EL CAMPO DE LA DIRECCIÓN DE LA ####
####      COMPONENTE VERTICAL DE LA VELOCIDAD              ####
###############################################################

# A continuación, se describen los pasos a seguir con extractos  
# de las rutinas: funREMO, graf_rem_t, vel_remo y densidad_potencial

# PASO 1. LLamar paqueterías 
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

# PASO 2. Cargar remolinos
{# Elegir remolinos ciclónicos o anticiclónicos 
  
  ## CICLÓNICOS 185
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
}

# PASO 3. Extraer transectos del remolino y coordenadas del límite superficial del remolino 
{
  polilont <- function(r, fp,fi,fm,ff, a2, b2){
    
    #1. Extraer información de boyas ARGO
    {
      load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
      draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
      par(mar = c(4.1,4.1,1.1,1.1))
      plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
      draw.map(); grid(col=1)
    }
    
    #2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = 'skyblue')
      }
      
      fechas <- t1[rem]
      fecha_buscada = fp
      posicion <- which(fechas == fecha_buscada)
      dat <- rem[posicion]
      #lines(conlon[,dat]-360, conlat[,dat], col = "red3")
      
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
      
      xcoor <- seq(min(conlon[,dat]-360)-a2,max(conlon[,dat]-360)+b2 , by = 0.01)
      ycoor <- m * xcoor + b 
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 1.5)
      
    }
    
    #3. Buscar puntos de intersección entre la recta y polígonos de interés
    {
      r1 <- t(rbind(xcoor, ycoor))
      r2 <- st_linestring(r1)
      r3 <- st_sfc(r2)
      r_sf <- st_sf(geometry = r3)
      
      # GRAFICAR POLÍGONOS DE INTERÉS 
      fecha_buscada1 = fi
      posicion1 <- which(fechas == fecha_buscada1)
      dat1 <- rem[posicion1]
      lon1 <- conlon[,dat1]-360
      lat1 <- conlat[,dat1]
      pol1_1 <- rbind(lon1,lat1)
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 2)
      
      {
        pol1 <- t(pol1_1)
        p1 <- st_polygon(list(pol1))
        p1_1 <- st_sfc(p1)
        p1_sf <- st_sf(geometry = p1_1)
        
        p1 <- st_intersection(p1_sf,r_sf)
        line_geom1 <- p1$geometry
        line_coords1 <- st_coordinates(line_geom1)
        
        lomax1 <- max(line_coords1[,1]) 
        plomax1 <- which.max(line_coords1[,1])
        pala1 <-  line_coords1[plomax1,2]
        
        lomin1 <-  min(line_coords1[,1])
        plomin1 <-  which.min(line_coords1[,1])
        pila1 <- line_coords1[plomin1,2]
        
        coor1 <- data.frame(longitud = c(lomax1, lomin1),
                            latitud = c(pala1, pila1))
      }
      
      fecha_buscada2 = fm
      posicion2 <- which(fechas == fecha_buscada2)
      dat2 <- rem[posicion2]
      lon2 <- conlon[,dat2]-360
      lat2 <- conlat[,dat2]
      pol2_2 <- rbind(lon2,lat2)
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 2)
      
      {
        pol2 <- t(pol2_2)
        p2 <- st_polygon(list(pol2))
        p2_2 <- st_sfc(p2)
        p2_sf <- st_sf(geometry = p2_2)
        
        p2 <- st_intersection(p2_sf,r_sf)
        line_geom2 <- p2$geometry
        line_coords2 <- st_coordinates(line_geom2)
        
        lomax2 <- max(line_coords2[,1]) 
        plomax2 <- which.max(line_coords2[,1])
        pala2 <-  line_coords2[plomax2,2]
        
        lomin2 <-  min(line_coords2[,1])
        plomin2 <-  which.min(line_coords2[,1])
        pila2 <- line_coords2[plomin2,2]
        
        coor2 <- data.frame(longitud = c(lomax2, lomin2),
                            latitud = c(pala2, pila2))
      }
      
      fecha_buscada3 = ff
      posicion3 <- which(fechas == fecha_buscada3)
      dat3 <- rem[posicion3]
      lon3 <- conlon[,dat3]-360
      lat3 <- conlat[,dat3]
      pol3_3 <- rbind(lon3,lat3)
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =2)
      
      {
        pol3 <- t(pol3_3)
        p3 <- st_polygon(list(pol3))
        p3_3 <- st_sfc(p3)
        p3_sf <- st_sf(geometry = p3_3)
        
        p3 <- st_intersection(p3_sf,r_sf)
        line_geom3 <- p3$geometry
        line_coords3 <- st_coordinates(line_geom3)
        
        lomax3 <- max(line_coords3[,1]) 
        plomax3 <- which.max(line_coords3[,1])
        pala3 <-  line_coords3[plomax3,2]
        
        lomin3 <-  min(line_coords3[,1])
        plomin3 <-  which.min(line_coords3[,1])
        pila3 <- line_coords3[plomin3,2]
        
        coor3 <- data.frame(longitud = c(lomax3, lomin3),
                            latitud = c(pala3, pila3))
      }
      
      coor_total <- rbind(coor1, coor2, coor3)
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 0.7, pch = 16)
      
      fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      legend("topright", legend = c("Puntos de intersección","Transecto",paste0(fechas_bus)), 
             col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
             lty = c(NA, 1,1,1,1),
             lwd = c(NA,3,3,3,3))
      title(main = "Remolino ciclónico 145", cex.main = 1, line = 0.3)
      
    }
    
    #4. Guardar información necesaria para extraer los perfiles
    {
      list(x_valores = xcoor, 
           y_valores = ycoor,
           coordenadas = coor_total)
    }
    
  }
  
  polilatlonmax <- function(r, fp, fi, fm, ff, a2, b2 ){
    
    # 1. Extraer información de boyas ARGO 
    {
      load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
      draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
      par(mar = c(4.1,4.1,1.1,1.1))
      plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
      draw.map(); grid(col=1)
    }
    
    # 2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = 'skyblue')
      }
      
      fechas <- t1[rem]
      fecha_buscada = fp
      posicion <- which(fechas == fecha_buscada)
      dat <- rem[posicion]
      
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
      
      xcoor <- seq(min(conlon[,dat]-360)-a2,max(conlon[,dat]-360)+b2 , by = 0.01)
      ycoor <- m * xcoor + b
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 1.5)
    }
    
    #3. Buscar puntos de intersección entre la recta y polígonos de interés
    {
      r1 <- t(rbind(xcoor, ycoor))
      r2 <- st_linestring(r1)
      r3 <- st_sfc(r2)
      r_sf <- st_sf(geometry = r3)
      
      # GRAFICAR POLÍGONOS DE INTERÉS 
      fecha_buscada1 = fi
      posicion1 <- which(fechas == fecha_buscada1)
      dat1 <- rem[posicion1]
      lon1 <- conlon[,dat1]-360
      lat1 <- conlat[,dat1]
      pol1_1 <- rbind(lon1,lat1)
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 2)
      
      {
        pol1 <- t(pol1_1)
        p1 <- st_polygon(list(pol1))
        p1_1 <- st_sfc(p1)
        p1_sf <- st_sf(geometry = p1_1)
        
        p1 <- st_intersection(p1_sf,r_sf)
        line_geom1 <- p1$geometry
        line_coords1 <- st_coordinates(line_geom1)
        
        lomax1 <- max(line_coords1[,1]) 
        plomax1 <- which.max(line_coords1[,1])
        pala1 <-  line_coords1[plomax1,2]
        
        lomin1 <-  min(line_coords1[,1])
        plomin1 <-  which.min(line_coords1[,1])
        pila1 <- line_coords1[plomin1,2]
        
        coor1 <- data.frame(longitud = c(lomax1, lomin1),
                            latitud = c(pala1, pila1))
      }
      
      fecha_buscada2 = fm
      posicion2 <- which(fechas == fecha_buscada2)
      dat2 <- rem[posicion2]
      lon2 <- conlon[,dat2]-360
      lat2 <- conlat[,dat2]
      pol2_2 <- rbind(lon2,lat2)
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 2)
      
      {
        pol2 <- t(pol2_2)
        p2 <- st_polygon(list(pol2))
        p2_2 <- st_sfc(p2)
        p2_sf <- st_sf(geometry = p2_2)
        
        p2 <- st_intersection(p2_sf,r_sf)
        line_geom2 <- p2$geometry
        line_coords2 <- st_coordinates(line_geom2)
        
        lomax2 <- max(line_coords2[,1]) 
        plomax2 <- which.max(line_coords2[,1])
        pala2 <-  line_coords2[plomax2,2]
        
        lomin2 <-  min(line_coords2[,1])
        plomin2 <-  which.min(line_coords2[,1])
        pila2 <- line_coords2[plomin2,2]
        
        coor2 <- data.frame(longitud = c(lomax2, lomin2),
                            latitud = c(pala2, pila2))
      }
      
      fecha_buscada3 = ff
      posicion3 <- which(fechas == fecha_buscada3)
      dat3 <- rem[posicion3]
      lon3 <- conlon[,dat3]-360
      lat3 <- conlat[,dat3]
      pol3_3 <- rbind(lon3,lat3)
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =2)
      
      {
        pol3 <- t(pol3_3)
        p3 <- st_polygon(list(pol3))
        p3_3 <- st_sfc(p3)
        p3_sf <- st_sf(geometry = p3_3)
        
        p3 <- st_intersection(p3_sf,r_sf)
        line_geom3 <- p3$geometry
        line_coords3 <- st_coordinates(line_geom3)
        
        lomax3 <- max(line_coords3[,1]) 
        plomax3 <- which.max(line_coords3[,1])
        pala3 <-  line_coords3[plomax3,2]
        
        lomin3 <-  min(line_coords3[,1])
        plomin3 <-  which.min(line_coords3[,1])
        pila3 <- line_coords3[plomin3,2]
        
        coor3 <- data.frame(longitud = c(lomax3, lomin3),
                            latitud = c(pala3, pila3))
      }
      
      coor_total <- rbind(coor1, coor2, coor3)
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 0.7, pch = 16)
      
      fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      legend("topright", legend = c("Puntos de intersección","Transecto",paste0(fechas_bus)), 
             col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
             lty = c(NA, 1,1,1,1),
             lwd = c(NA,3,3,3,3))
      title(main = "Remolino ciclónico 135", cex.main = 1, line = 0.3)
      
    }
    
    list(x_valores = xcoor, 
         y_valores = ycoor,
         coordenadas = coor_total)
    
  }
  
  
  valores <- polilont( r = 151, fp = "2020-04-21",
                       fi = "2020-04-16", 
                       fm = "2020-05-12", 
                       ff = "2020-06-08",
                       a2 = 1, b2 = 0.6)
  
  
  
# 4. Extraemos datos de interés 
  x_valores <- valores$x_valores   #Coordenadas de la recta en el eje x 
  y_valores <- valores$y_valores   #Coordenadas de la recta en el eje y
  coor <- valores$coordenadas   #Coordenadas puntuales de las intersecciones
  
}

# PASO 4. Limpiar espacio de trabajo 
{
  #Limpiar espacio de trabajo 
  rm(conlat, conlon, nc, ibuenob, ibuenor, 
     ini, ini1, lat, lat1, lon, lon1, remboy, t1,
     time, track)
  rm(polilont)
  rm(polilatlonmax)
}

# PASO 5. Extraer perfil de salinidad y temperatura 
{
  #CABIAR BASE DE DATOS DE ACUERDO A LA FECHA DE INTERÉS 
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_junio20.nc"))
  nc <- nc_open(alist) #print(nc) 
  time <- ncvar_get(nc,'time')
  fechas <- as.POSIXct(time, origin="1970-01-01", tz="UTC")
  
  # A PARTIR DE POLILONT
  perfil_latitudinal2 <- function(day) {
    
    # 1. Extraer información de los archivos netCDF
    {
      lon <- ncvar_get(nc,'longitude')  #size 360 E: -60.08 O: -89.9
      lat <- ncvar_get(nc,'latitude')   #sixe 205 N: 24     S:  7
      depth <- ncvar_get(nc,'depth')    #size 31  Max: 453.93
      time <- ncvar_get(nc,'time')      #size 31 - Mes de Octube 
      salinity <- ncvar_get(nc,'so')
      temperature <- ncvar_get(nc, 'thetao')
      
      fechas <- as.POSIXct(time, origin="1970-01-01", tz="UTC")
      
      salinidad <- salinity[,,,day]       
      temperatura <- temperature[,,,day]
    }
    
    # 2. Calcular transectos 
    {
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
      
      # TEMPERATURA
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
      
    }
    
    list(sal  = transect_data_sal,
         temp = transect_data_temp)
  }
  
  # A PARTIR DE POLILAT - POLILATLONMAX
  perfil_lonlat <- function(day) {
    
    # 1. Extraer datos 
    {
    lon <- ncvar_get(nc,'longitude')  #size 360 E: -60.08 O: -89.9
    lat <- ncvar_get(nc,'latitude')   #sixe 205 N: 24     S:  7
    depth <- ncvar_get(nc,'depth')    #size 31  Max: 453.93
    time <- ncvar_get(nc,'time')      #size 31 - Mes de Octube 
    salinity <- ncvar_get(nc,'so')
    temperature <- ncvar_get(nc, 'thetao')
    
    fechas <- as.POSIXct(time, origin="1970-01-01", tz="UTC")
    
    salinidad <- salinity[,,,day]       
    temperatura <- temperature[,,,day]
    }
    
    # 2- Calcular transectos 
    {
    #SALINIDAD
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
    
    #TEMPERATURA 
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
    
    }
   
    # 3. Guardar variables 
    list(sal  = transect_data_sal,
         temp = transect_data_temp)
    
  }
  
  #Cambiar nombre de acuerdo al remolino y la fecha de interés
  transect <-  perfil_latitudinal2(8)  
  transect <- perfil_lonlat(20)
  
}

# PASO 6. Calcular "Anomalía de densidad potencial" 
{ #library(gsw)
  
  
  #### 1. Convertir la profundidad (metros) a presión (decibares)
  {
    Z <- ncvar_get(nc,'depth')  #34 niveles 
    lat <- ncvar_get(nc,'latitude')
    
    #1.1  CONVERTIR altura a profundidad y promediar latitudes
    z <- Z * -1
    la <- mean(lat)
    {
      #Haciendo pruebas la presión varió 0.0003 en la superfucie 
      #entre el valor mínimo y máximo de latitud y varió 0.72 decibares de 
      #en lo más profunddo de la base de datos entre el min y máx
      # así que no hay tanta diferencia en tomar el promedio de las latitudes 
    }
    
    #1.2 Convertir profundidad a presión 
    p <- gsw_p_from_z(z = z, latitude = la)
    
    rm(z, Z)
  }
  
  #### 2. Convertir la salinidad práctia (PSU) a Salinidad absoluta
  {
    # Variables: SP, p, lat, lon 
    
    lon <- x_valores
    lat <- y_valores
    
    SP <- transect$sal  # Cambiar a transect_data_sal
    
    
    SA <- matrix(NA, nrow = length(x_valores), ncol = 35)
    for(i in (1:length(lat))){
      for(j in (1:length(p))){
        
        SA[i,j] <- gsw_SA_from_SP(SP = SP[i, j], p = p,
                                  longitud = lon,
                                  latitude = lat)
      }
    }
  }
  
  #### 3. Calculamos la Temperatura conservativa a partir de la temperatura potencial 
  {
    # Variables que se requieren 
    # SA = Salinidad absoluta 
    # pt = potencial de temperatura 
    
    #NOTA: Modelo Glorys proporciona la temperatura potencial
    
    pt <-  transect$temp  #Cambiar a transect_data_temp
    
    CT <- gsw_CT_from_pt(SA,pt)
    
  }
  
  #### 4. Calculamos la Densidad potencial - Validar si será necesaria 
  {
    # Variables requeridas:
    # SA = Salinidad absoluta 
    # CT = Temperatura conservativa 
    # p  = presión (dbar)
    
    rho <- gsw_rho(SA, CT, p)
    
  }
  
  ### 5. Anomalía de densidad potencial - kg/m^3
  {
    # Variables requeridas: 
    # SA = Salinidad absoluta 
    # CT = Temperatura conservativa 
    
    sigma_theta <- gsw_sigma0(SA, CT) 
    
  }

}

# PASO 7. Convertir eje X a distancias en km 
{
  dis<-0
  for (k in 1:(length(x_valores)-1)){
    aux<-distm(c(x_valores[k],y_valores[k]),
               c(x_valores[k+1],y_valores[k+1]),
               fun = distHaversine)
    dis[k+1]<-dis[k] + aux
  }
  
  dis <- as.numeric(format(0.8+dis/1e3,digits=2))
  x_vals <- dis-max(dis)/2
  
}

# PASO 8. Calcular EL campo de la dirección de la componente vertical de la velocidad 
{
  #AJUSTAR MES DE ACUERDO A LA FECHA DE BÚSQUEDA 
  #CAMBIAN LAS FECHAS DE ACUERDO AL REMOLINO DE INTERÉS
  
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
  alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_junio20.nc"))
  nc <- nc_open(alist) #print(nc) -- displays nc file info 
  
  perfil_velocidades_verticales2 <- function(day){
    
    # 1. Extraer información del netCDF 
    {
      
      LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
      LA <- ncvar_get(nc,'latitude')   #size 205 N: 24     S:  7
      Z <- ncvar_get(nc,'depth')    #size 35  Max: 453.93
      time <- ncvar_get(nc,'time')      #size 31 - Mes de Octube 
      east <- ncvar_get(nc,'uo')
      north <- ncvar_get(nc, 'vo')
      
      fechas <- as.POSIXct(time, origin="1970-01-01", tz="UTC")
      
      # Número de datos que hay en LA, LO y tiempo 
      nLO <- length(LO)
      nLA <- length(LA)
      nZ <- length(Z)
      nT  <- length(time)
    }
    
    # 2. Convertimos distancia a metros para obtener 
    #  las diferenciales de las DISTANCIAS 
    {
      dy <- 0.0833 * 111.320                #latitud
      dx <- 0.0833 * 111.320 * cos(0.0833)  #longitud
      dz  <- diff(Z)  
    }
    
    # 3. Derivar por diferencias finitas hacia adelante 
    
    # 3.1 Calculamos componente u - ESTE-OESTE (Longitud)
    
    {
      du <- array(0, dim = c(nLO-1, nLA-1, nZ, nT))   # nLO-1 y nLA-1 por los límites en los bucles
      dudx <- array(0, dim = c(nLO-1, nLA-1, nZ, nT)) 
      
      for(j in 1:nLA-1){     #lat
        for(i in 1:nLO-1){   #lon
          for(k in 1:nZ){    #prof
            for(t in 1:nT){  #time
              
              du[i,j,k,t] <- east[i,j+1,k,t]-east[i,j,k,t]
              dudx[i,j,k,t] <- du[i,j,k,t] / dx
            }
          }
        }
      }
      
    }
    
    # 3.2 Calculamos componentes v - NORTE-SUR (Latitud)
    
    {
      dv <- array(0, dim = c(nLO-1, nLA-1, nZ, nT))   # nLO-1 y nLA-1 por los límites en los bucles
      dvdy <- array(0, dim = c(nLO-1, nLA-1, nZ, nT))
      
      for(i in 1:nLO-1){     #lat
        for(j in 1:nLA-1){   #lon
          for(k in 1:nZ){    #prof
            for(t in 1:nT){  #time
              
              dv[i,j,k,t] <- north[i+1,j,k,t]-north[i,j,k,t]
              dvdy[i,j,k,t] <- dv[i,j,k,t] / dy
            }
          }
        }
      }
      
    }
    
    # 4. Ajustar al signo negativo de la ecuación 
    #RECORDAR que La ecuación de CONTINUIDAD está multiplicada por -1
    
    {
      dudx2 <- -1*dudx
      dvdy2 <- -1*dvdy
      
    }
    
    # 5. SUMAR las derivadas parciales 
    dudv <- dudx2 + dvdy2
    
    # 6. INTEGRAR entre capas 
    
    {integral_dudv <- array(0, dim = c(nLO-1, nLA-1, nZ-1, nT))
      
      for(i in 1:nLO-1){      #Lon
        for(j in 1:nLA-1){    #Lat
          for(k in 1:nZ-1){   #Prof 
            for(t in 1:nT){   #Tiempo
              
              dudv1 <- dudv[i,j,k,t, drop = FALSE]
              dudv2 <- dudv[i,j,k+1,t, drop = FALSE]
              if (all(dim(dudv1) == dim(dudv2))){
                integral_dudv[i,j,k,t] <- dudv1 + dudv2
              } 
            }
          }
        }
      }
    }
    
    # 7. MULTIPLICAR el resultado de la integral por dz
    
    {integral_dudv_dz <- array(0, dim = c(nLO-1, nLA-1, nZ-1, nT))
      
      for(k in 1:nZ-1){
        integral_dudv_dz[,,k,] <- integral_dudv[,,k,] *  dz[k]
      }
    }
    
    # 8. Extraer el día de interés 
    
    v_vertical <- integral_dudv_dz[,,,day]
    
    # 9. AJUSTAR dimensiones de LO y LA para que queden del mismo tamaño 
    
    LO2 <- head(LO, -1)
    LA2 <- head(LA, -1)
    
    # 10. Extraer el perfil de velocidades del día de interés 
    {
      transect_data_vv <- c()
      for (i in 1:dim(v_vertical)[3]){
        transect_data_vv <- cbind(transect_data_vv, 
                                  interp2(x = LO2, 
                                          y = LA2, 
                                          Z = t(as.matrix(v_vertical[,,i])),
                                          xp = x_valores, 
                                          yp = y_valores, 
                                          method = 'linear'))
      }
    }
    
    # 11. Extraer variables de interés 
    
    list(int_dudv_dz = integral_dudv_dz,
         velocidad_vertical = transect_data_vv)
    
  }
  
  #AJUSTAR EL NOMBRE SEGÚN LA FECHA Y EL REMOLINO  
  v <- perfil_velocidades_verticales2(day = 8) 
  
  #Establecer parámetros del QUIVER
  {
    Z <- ncvar_get(nc,'depth') #Revisar netCDF
    integral_dudv_dz <- v$int_dudv_dz
    
    x_vals2 <- dis-max(dis)/2
    prof2 <-seq(min(Z),max(Z),length=dim(integral_dudv_dz)[3])-max(Z)
    
    u0 <- matrix(0, nrow = length(x_valores), ncol = 34)
    
    w1 <- t(rbind(replicate(length(x_valores), prof2))) #y
    w2 <- t(apply(w1, 1, rev))
    
    k1 <- rbind(replicate(34, x_vals2))    #x
    k2 <- k1[nrow(k1):1, ]
    }
}

# PASO 9. Generar líneas longitudinales con "coor"
{
  #NOTA: la posición de las líneas cambia con respecto al centro del 
  #       transecto, por lo tanto, AJUSTAR cada vez que se grafique 
  #       una FECHA DIFERENTE
  
  x1 <- 5    #1,3,5
  x2 <- 6    #2,4,6
  
  y1 <- 1
  y2 <- 1
  
  punto_central <- c(median(x_valores), median(y_valores))
  punto1 <- c(coor$longitud[x1], coor$latitud[x1])
  punto2 <- c(coor$longitud[x2], coor$latitud[x2])
  
  dis_centro_1 <- distHaversine(punto_central, punto1)
  dis_centro_2 <- distHaversine(punto_central, punto2)
  
  dis1 <- dis_centro_1/1000*y1  #1,2,3 multiplicar por *-1
  dis2 <- dis_centro_2/1000*y2    #4,5,6 NO multiplicar(depende el polígono)
}

#PASO 10. Generar gráficos con todos los detalles 
{
  
  transect_data_vv <- v$velocidad_vertical
  
  prof<-seq(min(Z),max(Z),length=length(Z))-max(Z)
  step <- 20
  palD <- colorRampPalette((c("#001889","#A40A8A","#E78140","#DAFF47")))
  
  filled.contour(x_vals,prof,rot90(sigma_theta,2),
                 xlab='distance [Km]', ylab='depth [m]', 
                 color.palette = palD,
                 #zlim = c(34.5, 37.5),
                 plot.axes = {axis(1); axis(2)
                   quiver(x = k2[seq(1, length(k2), step)], 
                          y = w2[seq(1, length(w2), step)],
                          u = u0[seq(1, length(u0), step)], 
                          v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 1000)
                          #scale = 0.5)
                   abline(v = dis1, col = 'black', lwd = 3, lty = 1)
                   abline(v = dis2, col = 'black', lwd = 3, lty = 1)}, 
                 key.title = mtext(expression("Anomalía de densidad potencial [kg/m"^3*"]"), 
                                   side = 4, line = 2.9, las = 3))
  

  
}

#PASO 11. Paso anterior, pero para guardar los gráficos 
{
  transect_data_vv <- v$velocidad_vertical
  
  day <- 8
  
  setwd("C:/Users/genef/Documents/Remolinos/remo_densidad_potencial/cy145")
  setwd("C:/Users/genef/Documents/Remolinos/remo_densidad_potencial/anticy151")
  
  png(paste0("ADP_", fechas[day], ".png"), type = "cairo", 
       width = 1200, height = 800, 
       units = "px", res = 165, pointsize = 12 )
  prof<-seq(min(Z),max(Z),length=length(Z))-max(Z)
  step <- 20
  palD <- colorRampPalette((c("#001889","#A40A8A","#E78140","#DAFF47")))
  
  filled.contour(x_vals,prof,rot90(sigma_theta,2),
                 xlab='Distancia [Km]', ylab='Profundidad [m]', 
                 color.palette = palD,
                 #zlim = c(34.5, 37.5),
                 plot.axes = {axis(1); axis(2)
                   quiver(x = k2[seq(1, length(k2), step)], 
                          y = w2[seq(1, length(w2), step)],
                          u = u0[seq(1, length(u0), step)], 
                          v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 1000)
                   abline(v = dis1, col = 'black', lwd = 3, lty = 1)
                   abline(v = dis2, col = 'black', lwd = 3, lty = 1)}, 
                 key.title = mtext(expression("Anomalía de densidad potencial [kg/m"^3*"]"), 
                                   side = 4, line = 2.9, las = 3))
  
  title( paste0 (fechas[day]), cex.main = 3)
  
  dev.off()
  
}


#  NOTA: REGRESAR AL paso 5 para usar otra fecha del mismo remolino
#        Si es otro remolino, volver al paso 2

rm(la, lat, dis, dis_centro_1, dis_centro_2, i, 
   j, k, aux, k1, p, y1, y2, x1, x2, punto_central,
   punto1, punto2, prof2, lon, x_vals2)

rm(sigma_theta, integral_dudv_dz,SP, SA, transect_data_vv, w1, w2, CT,
   pt, rho, v, transect, u0,day, time, prof, dis1, dis2, 
   x_vals, fechas, k2, perfil_latitudinal2,alist, Z, nc, 
   perfil_velocidades_verticales2, palD, step)
