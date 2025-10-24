#################################################################
##   CALCULAR DIRECCIÓN VERTICAL DE LA VELOCIDAD               ##
##   Y SOBREPONER CAMPO DE DIRECCIONES HORIZONTALES            ##
#################################################################


# NOTAS 
{
  # Velocidades verticales - se debe escoger un día - ver rutina vvertical.R
  # velocidades horizontales - se debe escoger un nivel 
}

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

# Definir dirección de trabajo y fechas 
setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_junio20.nc"))
nc <- nc_open(alist) #print(nc) -- displays nc file info 



perfil_velocidades_verticales <- function(day){
  
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
  {
    # 3.1 Calculamos componente u - ESTE-OESTE (Longitud)
    {du <- array(0, dim = c(nLO-1, nLA-1, nZ, nT))   # nLO-1 y nLA-1 por los límites en los bucles
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
    
    # 3.2Calculamos componentes v - NORTE-SUR (Latitud)
    {dv <- array(0, dim = c(nLO-1, nLA-1, nZ, nT))   # nLO-1 y nLA-1 por los límites en los bucles
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
  }
  
  # 4. Multiplicar por -1  
  #RECORDAR que La ecuación de CONTINUIDAD está multiplicada por -1
  {dudx2 <- -1*dudx
    dvdy2 <- -1*dvdy
  }
  
  # 5. SUMA de derivadas parciales 
  dudv <- dudx2 + dvdy2
  
  # 6. INTEGRAR entre capas 
  {
    integral_dudv <- array(0, dim = c(nLO-1, nLA-1, nZ-1, nT))
    
    
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
  
  # 7. MULTIPLICAR resultado de la integral por dz
  {
    
    integral_dudv_dz <- array(0, dim = c(nLO-1, nLA-1, nZ-1, nT))
    
    for(k in 1:nZ-1){
      integral_dudv_dz[,,k,] <- integral_dudv[,,k,] *  dz[k]
    }
  }
  
  # 8. Extraer el día de interés de todo el cálculo 
  v_vertical <- integral_dudv_dz[,,,day]
  

  # 10. Extraer variable de interés 
  
  list(int_dudv_dz = integral_dudv_dz)
  
}

v <- perfil_velocidades_verticales(7)



##### CALCULAR DIRECCIÓN DE LAS CORRIENTES PARA UN NIVEL 
##### Puede ser superficial o de otra profundidad 

int_dudv_dz <- v$int_dudv_dz
v_vertical <- v$int_dudv_dz[,,1,07]

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

setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
save(v_vertical, file = "rem151_vv_f3.RData")

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
  
  r <- 145
  rem <- which(track==remboy[r])
  
  fechas <- t1[rem]
  fecha_buscada = "2020-01-10"
  posicion <- which(fechas == fecha_buscada)
  dat <- rem[posicion]
  
  
  
  #GRAFICAR CORRIENTES CON POLÍGONO DE REMOLINO DE INTERÉS
  p <- 20 #distacia de flechas
  palV2 <- colorRampPalette((c("#A90C38", "white","#2E5A87")))
  
  #BARRA DE ESCALA 
  lat_media <- mean(LAn)
  escala_km <- 200
  escala_grados <- escala_km / (111 * cos(pi * lat_media / 180))
  
  filled.contour(x = LOn, y = LAn, z = v_vertical,
                 xlab='Longitud', ylab='Latitud', 
                 color.palette = palV2,
                 plot.axes = {axis(1); axis(2)
                   #Líneas de latitud
                   abline (h = seq(min(LAn), max(LAn), by = 5),
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
                         col = "black", lwd = 3.5)
                 
                   # --- Rosa de los vientos ---
                   xN <- max(LOn) - 1     # posición en X
                   yN <- min(LAn) + 0.5   # posición en Y
                   arrows(x0 = xN, y0 = yN, x1 = xN, y1 = yN + 0.4,
                          length = 0.1, lwd = 2, col = "black")
                   text(xN, yN + 0.7, "N", cex = 1.2, font = 2)
                   
                   # Dibuja una barra de escala
                   x0 <- max(LOn) - 5 
                   y0 <- min(LAn) + 1
                   segments(x0, y0, x0 + escala_grados, y0, lwd = 3)
                   text(x0, y0 - 0.3, "0", cex = 0.8)
                   text(x0 + escala_grados, y0 - 0.3, "200 km", cex = 0.8)
                   
                   },
                 
                 )
  
  