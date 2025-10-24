##################################################################
## PERFILES CON CAMPOS DE LA DIRECCIÓN DE LA VELOCIDAD VERTICAL ##
##################################################################

##### CARGAR PAQUETERÍAS DE funREMO.R

# 1. Extraer la información del remolino de interés 
# OJO. Si se carga, se puede saltar el paso 2 
{
setwd("C:/Users/genef/Documents/Remolinos")
load("transectos_cy145.RData")               #Transectos de sal y temp de las fechas de interés
load("coor145.RData")                        # x-y valores y coordenadas de líneas longitudinales
}

# 2. EXTRAER LOS PERFILES DE SALINIDAD Y TEMPERATURA 
{
setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_noviembre19.nc"))
nc <- nc_open(alist) #print(nc) 

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
  
  # 2. Generar transectos 
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

transect25 <-  perfil_latitudinal2(25)
}

# 3. CALCULAR EL CAMPO DE LA DIRECCIÓN LAS DE VELOCIDADES VERTICLAES 
{
setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_noviembre19.nc"))
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

v25 <- perfil_velocidades_verticales2(day = 25) # Ajustar nombre según el remolino
}

###############################################
## PRUEBAS CON PERFILES Y CAMPO DE VELOCIDAD ##
###############################################

transect_data_vv <- v25$velocidad_vertical
integral_dudv_dz <- v25$int_dudv_dz


# 1. Generar líneas longitudinales #NOTA: cambiar de acuerdo a la fecha
{
  x1 <- 1
  x2 <- 2
  
  y1 <- -1
  y2 <- -1
  
  punto_central <- c(median(x_valores), median(y_valores))
  punto1 <- c(coor145$longitud[x1], coor145$latitud[x1])
  punto2 <- c(coor145$longitud[x2], coor145$latitud[x2])
  
  dis_centro_1 <- distHaversine(punto_central, punto1)
  dis_centro_2 <- distHaversine(punto_central, punto2)
  
  dis1 <- dis_centro_1/1000*y1  #1,2,3 multiplicar por *-1
  dis2 <- dis_centro_2/1000*y2    #4,5,6 NO multiplicar
  
  rm(x1,y1,x2,y2, punto_central, punto1, punto2, dis_centro_1, dis_centro_2)
}

# 2. Generar vector de la longitud X 
{dis<-0
for (k in 1:(length(x_valores)-1)){
  aux<-distm(c(x_valores[k],x_valores[k]),
             c(x_valores[k+1],x_valores[k+1]),
             fun = distHaversine)
  dis[k+1]<-dis[k] + aux
}

dis <- as.numeric(format(0.8+dis/1e3,digits=2))
}

# 3. Establecer parámetros del QUIVER
{Z <- ncvar_get(nc,'depth') #Revisar netCDF
  
  x_vals <- dis-max(dis)/2
  prof<-seq(min(Z),max(Z),length=dim(integral_dudv_dz)[3])-max(Z)

u0 <- matrix(0, nrow = 1027, ncol = 34)

w1 <- t(rbind(replicate(1027, prof))) #y
w2 <- t(apply(w1, 1, rev))

k1 <- rbind(replicate(34, x_vals))    #x
k2 <- k1[nrow(k1):1, ]
}

# Generar perfil de velocidades con campo de direcciones verticales de la velocidad  
step <- 23

{
palV <- colorRampPalette((c("#A90C38","#F59885", "white","#C3C8D2","#2E5A87")))

filled.contour(x_vals,prof,rot90(transect_data_vv,2),
               xlab='distance [Km]', ylab='depth [m]', 
               color.palette = palV, 
               plot.axes = {axis(1); axis(2)
                 quiver(x = k2[seq(1, length(k2), step)], 
                        y = w2[seq(1, length(w2), step)],
                        u = u0[seq(1, length(u0), step)], 
                        v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 100,
                        scale = 1.5) 
                 abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                 abline(v = dis2, col = 'black', lwd = 3, lty = 2)}
)

}

###########################################################################
## GRAFICAR CAMPO DE DIRECCIÓN DE LAS VELOCIDADES VERTICALES SOBRE LOS   ##
##               PERFILES DE TEMPERATURA Y SALINIDAD                     ##
###########################################################################

# Salinidad 
{transect_data_sal <- nov25$sal
 east <- ncvar_get(nc,'uo')

palS <- colorRampPalette((c("cyan","#841859","darkolivegreen2","#005600")))
prof2 <-seq(min(Z),max(Z),length=dim(east)[3])-max(Z)

filled.contour(x_vals,prof2,rot90(transect_data_sal,2),
               xlab='distance [Km]', ylab='depth [m]', 
               color.palette = palS,
               plot.axes = {axis(1); axis(2)
                 quiver(x = k2[seq(1, length(k2), step)], 
                        y = w2[seq(1, length(w2), step)],
                        u = u0[seq(1, length(u0), step)], 
                        v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 100,
                        scale = 1.5)
                 abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                 abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
               key.title = mtext("Salinidad [PSU]", 
                                 side = 4, line = 2.9, las = 3))

}

# TEMPERATURA 
{transect_data_temp <- nov25$temp
  east <- ncvar_get(nc,'uo')
  
  palT <- colorRampPalette(c("#3E0689","#AE1987","#EA8D2D","#E3E400"))
  prof2 <-seq(min(Z),max(Z),length=dim(east)[3])-max(Z)
  
  filled.contour(x_vals,prof2,rot90(transect_data_temp,2),
                 xlab='distance [Km]', ylab='depth [m]', 
                 color.palette = palT,
                 plot.axes = {axis(1); axis(2)
                   quiver(x = k2[seq(1, length(k2), step)], 
                          y = w2[seq(1, length(w2), step)],
                          u = u0[seq(1, length(u0), step)], 
                          v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 100,
                          scale = 1.5)
                   abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                   abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                 key.title = mtext("Temperatura potencial [°C]", 
                                   side = 4, line = 2.9, las = 3))
  
}
