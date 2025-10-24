
###################################################
#
#    GENERAR MAPA DE REMOLINOS CICLÓNICOS 
#              Y ANTICICLÓNICOS 
#
###################################################


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
  library(sf)
  pacman::p_load(maps, readxl, cmocean, geosphere, pracma)
  
  install.packages("prettymapr")
  library(prettymapr)
}

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



#### CICLÓNICOS 

# Configuración de tamaño y resolución


png(file = 'C:/Users/genef/Documents/Remolinos/perfiles/allciclonicos19_3.png', 
    width = 20, height = 12, units = "in", res = 400)
par(mar = c(5, 4, 8, 2))  # Ajusta márgenes

# Extraer información de boyas ARGO
{
  load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
  par(mar = c(4.1,4.1,1.1,1.1))
  plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F,
       cex.axis = 1.8 )
  draw.map(); grid(col=1)
}

#  REM 135
{
  
  polilatlonmax135 <- function(r, fp, fi, fm, ff, a2, b2 ){
    
    # 1. Extraer información de boyas ARGO 
    {
      # load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
      # draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
      # par(mar = c(4.1,4.1,1.1,1.1))
      # plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
      # draw.map(); grid(col=1)
    }
    
    # 2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = '#63B8FF')
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
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 2)
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
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 3.3)
      
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
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 3.3)
      
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
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =3.3)
      
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
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 2, pch = 16)
      
      # fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      # colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      # legend("topright", legend = c("Puntos de intersección","Transecto",paste0(fechas_bus)), 
      #        col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
      #        lty = c(NA, 1,1,1,1),
      #        lwd = c(NA,3,3,3,3))
      # title(main = "Remolino ciclónico 135", cex.main = 1, line = 0.3)
      
    }
    
    list(x_valores = xcoor, 
         y_valores = ycoor,
         coordenadas = coor_total)
    
  } #r135
  
  
  valores <- polilatlonmax135( r = 135, fp = "2019-11-18",
                            fi = "2019-06-20", 
                            fm = "2019-09-29", 
                            ff = "2019-11-01",
                            a2 = 0.2, b2 = 4.7)
  

  
}

#  REM 145
{
  polilont145 <- function(r, fp,fi,fm,ff, a2, b2){
    
    
    #2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = '#63B8FF')
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
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 2)
      
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
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 3.3)
      
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
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 3.3)
      
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
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =3.3)
      
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
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 2, pch = 16)
      
      # fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      # colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      # legend("topleft", legend = c("Puntos de intersección","Transecto",paste0(fechas_bus)), 
      #        col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
      #        lty = c(NA, 1,1,1,1),
      #        lwd = c(NA,3,3,3,3))
      # title(main = "Remolino ciclónico 145", cex.main = 1, line = 0.3)
      
    }
    
    #4. Guardar información necesaria para extraer los perfiles
    {
      list(x_valores = xcoor, 
           y_valores = ycoor,
           coordenadas = coor_total)
    }
    
  } 
  
  
  valores <- polilont145( r = 145, fp = "2020-01-06",
                            fi = "2019-11-28", 
                            fm = "2020-01-10", 
                            ff = "2020-02-08",
                            a2 = 4 , b2 = 3.8)
  
  
  
}

#  REM 164
{
  polilont164 <- function(r, fp,fi,fm,ff, a2, b2){
  
    
    #2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = '#63B8FF')
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
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 2)
      
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
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 3.3)
      
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
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 3.3)
      
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
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =3.3)
      
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
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 2, pch = 16)
      
      #fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      # colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      # legend("topright", legend = c("Puntos de intersección","Transecto", 
      #                              "Temprana", "Desarrollada", "Envejecida"), 
      #        col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
      #        lty = c(NA, 1,1,1,1),
      #        lwd = c(NA,3,3,3,3))
       #title(main = "Remolino ciclónicos ", cex.main = 1, line = 0.3)
      
      
      colores_ordenados <- c('tan1', 'red2','white' ,'violetred2', 'olivedrab3', 'purple3')
      
      legend("topright", legend = c("Puntos de intersección", "Transecto", 
                                    "Etapas", "Temprana", "Desarrollada", "Envejecida"), 
             col = colores_ordenados, pch = c(19, NA, NA, NA, NA, NA), 
             lty = c(NA, 1, NA, 1, 1, 1), 
             lwd = c(NA, 3, NA, 3, 3, 3), 
             cex = 2, # Aumenta el tamaño del texto
             bty = "n") # Opción para eliminar el cuadro de la leyenda
      
      #title(main = "Remolino ciclónicos de mesoescala", cex.main = 2, line = 1)
      
      
    }
    
    #4. Guardar información necesaria para extraer los perfiles
    {
      list(x_valores = xcoor, 
           y_valores = ycoor,
           coordenadas = coor_total)
    }
    
  }

  
  valores <- polilont164( r = 164, fp = "2020-10-04",
                            fi = "2020-10-17", 
                            fm = "2020-11-04", 
                            ff = "2020-12-01",
                            a2 = 2.5, b2 = 0.3)
  
  

  
}

map.scale(x = -68,    # centro en tu región
          y = 8.5,
          relwidth = 0.15,        # ancho relativo de la barra
          col = "black",          # color
          cex = 1.5,              # tamaño del texto
          ratio=FALSE)

addnortharrow(pos = "bottomright", scale=1.5)

text(x = -86, y = 19, label = "164", col = "black", cex = 3)
text(x = -81, y = 12, label = "135", col = "black", cex = 3)
text(x = -72.5, y = 16.5, label = "145", col = "black", cex = 3)
text(x = -87.5, y = 22.2, label = "a.", col = "black", cex = 3)

dev.off()

##### ANTICICLÓNICOS 


# Configuración de tamaño y resolución

png(file = 'C:/Users/genef/Documents/Remolinos/perfiles/allanticiclonicos_3.png', 
    width = 20, height = 12, units = "in", res = 400)
par(mar = c(5, 4, 8, 2))  # Ajusta márgenes

# Extraer información de boyas ARGO
{
  load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
  par(mar = c(4.1,4.1,1.1,1.1))
  plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F,
       cex.axis = 1.8 )
  draw.map(); grid(col=1)
}


#  REM 146
{
  polilatlonmax146 <- function(r, fp, fi, fm, ff, a2, b2 ){
    
  
    
    # 2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = '#33608C')
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
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 2)
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
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 3.3)
      
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
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 3.3)
      
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
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =3.3)
      
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
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 2, pch = 16)
      
      # fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      # colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      # legend("topright", legend = c("Puntos de intersección","Transecto",paste0(fechas_bus)), 
      #        col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
      #        lty = c(NA, 1,1,1,1),
      #        lwd = c(NA,3,3,3,3))
      # title(main = "Remolino ciclónico 135", cex.main = 1, line = 0.3)
      
    }
    
    list(x_valores = xcoor, 
         y_valores = ycoor,
         coordenadas = coor_total)
    
  } #r135
  
  
  valores <- polilatlonmax146( r = 146, fp = "2019-12-29",
                          fi = "2020-01-15", 
                          fm = "2020-02-08", 
                          ff = "2020-03-19",
                          a2 = 4.3, b2 = 0.8)
  
  
  
}

#  REM 151
{
  
  polilont151 <- function(r, fp,fi,fm,ff, a2, b2){
    
    
    #2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = '#63B8FF')
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
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 2)
      
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
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 3.3)
      
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
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 3.3)
      
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
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =3.3)
      
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
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 2, pch = 16)
      
      # fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      # colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      # legend("topleft", legend = c("Puntos de intersección","Transecto",paste0(fechas_bus)), 
      #        col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
      #        lty = c(NA, 1,1,1,1),
      #        lwd = c(NA,3,3,3,3))
      # title(main = "Remolino ciclónico 145", cex.main = 1, line = 0.3)
      
    }
    
    #4. Guardar información necesaria para extraer los perfiles
    {
      list(x_valores = xcoor, 
           y_valores = ycoor,
           coordenadas = coor_total)
    }
    
  } 
  
  
  valores <- polilont151( r = 151, fp = "2020-04-21",
                          fi = "2020-04-21", 
                          fm = "2020-05-10", 
                          ff = "2020-06-07",
                          a2 = 1, b2 = 0.6)
  
  
  
}


#  REM 131
{
    
  polilont131 <- function(r, fp,fi,fm,ff, a2, b2){
    
    
    #2. Buscar polígono para generar transecto
    {
      rem <- which(track==remboy[r])
      for (i in rem){
        lines(conlon[,i]-360, conlat[,i], col = '#63B8FF')
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
      lines(xcoor,ycoor, cex = 5, col = 'red2', lwd = 2)
      
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
      lines(conlon[,dat1]-360, conlat[,dat1], col = "violetred2", lwd = 3.3)
      
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
      lines(conlon[,dat2]-360, conlat[,dat2], col = "olivedrab3", lwd = 3.3)
      
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
      lines(conlon[,dat3]-360, conlat[,dat3], col = "purple3", lwd =3.3)
      
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
      points(coor_total$longitud,coor_total$latitud, col = 'tan1', cex = 2, pch = 16)
      
      #fechas_bus <- c(fecha_buscada1, fecha_buscada2, fecha_buscada3)
      # colores_ordenados <- c('tan1', 'red2', 'violetred2', 'olivedrab3','purple3')
      # legend("topright", legend = c("Puntos de intersección","Transecto", 
      #                              "Temprana", "Desarrollada", "Envejecida"), 
      #        col = colores_ordenados, pch = c(19, NA, NA, NA, NA), 
      #        lty = c(NA, 1,1,1,1),
      #        lwd = c(NA,3,3,3,3))
      #title(main = "Remolino ciclónicos ", cex.main = 1, line = 0.3)
      
      
      colores_ordenados <- c('tan1', 'red2','white', 'violetred2', 'olivedrab3', 'purple3')
      
      legend("topright", legend = c("Puntos de intersección", "Transecto", 
                                    "Etapas", "Temprana", "Desarrollada", "Envejecida"), 
             col = colores_ordenados, pch = c(19, NA, NA, NA, NA, NA), 
             lty = c(NA, 1, NA, 1, 1, 1), 
             lwd = c(NA, 3, NA, 3, 3, 3), 
             cex = 2, # Aumenta el tamaño del texto
             bty = "n") # Opción para eliminar el cuadro de la leyenda
      
      #title(main = "Remolino ciclónicos de mesoescala", cex.main = 2, line = 1)
      
      
    }
    
    #4. Guardar información necesaria para extraer los perfiles
    {
      list(x_valores = xcoor, 
           y_valores = ycoor,
           coordenadas = coor_total)
    }
    
  }
  
  
  valores <- polilont131( r = 131, fp = "2019-07-26",
                          fi = "2019-06-15", 
                          fm = "2019-08-15", 
                          ff = "2019-10-10",
                          a2 = 9.5, b2 = 3.5)
  
  
  
  
}

map.scale(x = -68,    # centro en tu región
          y = 8.5,
          relwidth = 0.15,        # ancho relativo de la barra
          col = "black",          # color
          cex = 1.5,              # tamaño del texto
          ratio=FALSE)

addnortharrow(pos = "bottomright", scale=1.5)

text(x = -86, y = 18, label = "151", col = "black", cex = 3)
text(x = -80, y = 20.1, label = "146", col = "black", cex = 3)
text(x = -73.5, y = 16.5, label = "131", col = "black", cex = 3)
text(x = -87.5, y = 22.2, label = "b.", col = "black", cex = 3)

dev.off()

