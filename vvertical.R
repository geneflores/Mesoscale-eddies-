#################################################################
##   CALCULAR CAMPO DE VELOCIDADES VERTICALES Y HORIZONTALES   ##
#################################################################

# NOTAS 
{
  # Velocidades verticales - se debe escoger un día
  # velocidades horizontales - se debe escoger un nivel 
}

# NOTAS 2
{
#Limpiar espacio de trabajo 
rm(conlat,conlon,nc, valores, ibuenob, ibuenor, 
   ini, ini1, lat, lat1, lon, lon1,remboy, t1,
   time, track, polilont)

### CALCULAR VELOCIDADES VERTICALES 
### a partir de las velocidades horizontales 
### con los datos del modelo GLORYS

### Se usarán las velocidades hacia el norte y hacia el este 
  
### PARA calcular la función 'perfil_velocidades_verticales' 
  ## es necesario haber calculado previamente 
  ##      x_valores y y_valores 
  ##
  ## o en su defecto cargarla de archivos previamente guardados 
  setwd("C:/Users/genef/Documents/Remolinos")
  load("coor145.RData")
}

# Definir dirección de trabajo y fechas 
setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS_VEL")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_noviembre19.nc"))
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

  # 9. AJUSTAR dimensiones de LO y LA 
   {
   LO2 <- head(LO, -1)
   LA2 <- head(LA, -1)
   }
   
  # 10. Realizar interpolación con el transecto del remolino
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
  
  # 11. Generar vector de distancia con las longitudes 
   {
dis<-0
for (k in 1:(length(x_valores)-1)){
  aux<-distm(c(x_valores[k],x_valores[k]),
             c(x_valores[k+1],x_valores[k+1]),
             fun = distHaversine)
  dis[k+1]<-dis[k] + aux
}

dis <- as.numeric(format(0.8+dis/1e3,digits=2))

   }
   
  # 12. Ver perfil de velocidades 
   {
palV <- colorRampPalette((c("#A90C38","#F59885", "white","#C3C8D2","#2E5A87")))
prof <- seq(min(Z),max(Z),length=dim(integral_dudv_dz)[3])-max(Z)


filled.contour(dis-max(dis)/2, prof, rot90(transect_data_vv,2),
               xlab='distance [Km]', ylab='depth [m]', 
               color.palette = palV)

#title( paste0 (fechas[25]), cex.main = 3)

      }
  
   # 13. Extraer variables de interés 

      list(int_dudv_dz = integral_dudv_dz,
     velocidad_vertical = transect_data_vv)

}

v <- perfil_velocidades_verticales(29)


#Función anterior, solo para realizar ajustes 
{
### Obtener todas las variables del cálculo anterior 
### NO es necesario si se corrió la función anterior
day = 25
{
  
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
  
  # Derivar por diferencias finitas hacia adelante 
  {
  # Calculamos componente u - ESTE-OESTE (Longitud)
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
  
  # Calculamos componentes v - NORTE-SUR (Latitud)
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
  
  # RECORDAR que La ecuación de CONTINUIDAD está multiplicada por -1
  dudx2 <- -1*dudx
  dvdy2 <- -1*dvdy
  
  # SUMA de derivadas parciales 
  dudv <- dudx2 + dvdy2
  
  # INTEGRAR entre capas 
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
  
  # MULTIPLICAR el resultado de la integral por dz
  {
  integral_dudv_dz <- array(0, dim = c(nLO-1, nLA-1, nZ-1, nT))
  
  for(k in 1:nZ-1){
    integral_dudv_dz[,,k,] <- integral_dudv[,,k,] *  dz[k]
    
  }
  
  }
  
  v_vertical <- integral_dudv_dz[,,,day]
  
  # AJUSTAR dimensiones de LO y LA 
  LO2 <- head(LO, -1)
  LA2 <- head(LA, -1)
  
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
  
  
  #Calcular vector distancia a partir de las longitudes 
  {
  dis<-0
  for (k in 1:(length(x_valores)-1)){
    aux<-distm(c(x_valores[k],x_valores[k]),
               c(x_valores[k+1],x_valores[k+1]),
               fun = distHaversine)
    dis[k+1]<-dis[k] + aux
  }
  
  
  dis <- as.numeric(format(0.8+dis/1e3,digits=2))
  }
  
}
}

####################################################### 
# Si se cargan estos datos, 
# no es necesario calcular lo anterior

save(vv25_145, file = "vv25_145.RData")

setwd("C:/Users/genef/Documents/Remolinos")

load("vv25_145.RData")
transect_data_vv <- vv25_145$v_verticales

#######################################################

# Definir otras variables para graficar perfil con campo de direcciones
{
int_dudv_dz <- v$int_dudv_dz
Z <- ncvar_get(nc,'depth')
prof  <- seq(min(Z),max(Z),length=dim(v$int_dudv_dz)[3])-max(Z)
x_vals <- dis-max(dis)/2

u0 <- matrix(0, nrow = 1027, ncol = 34)

w1 <- t(rbind(replicate(1027, prof)))
w2 <- t(apply(w1, 1, rev))

k1 <- rbind(replicate(34, x_vals))
k2 <- k1[nrow(k1):1, ]
}

# Hacer gráfico del perfil vertical 
{
transect_data_vv <- v$velocidad_vertical

step <- 20
palV <- colorRampPalette((c("#A90C38","#F59885", "white","#C3C8D2","#2E5A87")))

filled.contour(x_vals, prof, rot90(transect_data_vv, 2),
               xlab = 'distance [Km]', ylab = 'depth [m]',
               color.palette = palV,
               plot.axes = {
                 axis(1);
                 axis(2)
                 quiver(x = k2[seq(1, length(k2), step)], 
                        y = w2[seq(1, length(w2), step)],
                        u = u0[seq(1, length(u0), step)], 
                        v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 100)
                 #abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                 #abline(v = dis2, col = 'black', lwd = 3, lty = 2)
               },
               key.title = mtext("Velocidad [m/s]",
                                 side = 4, line = 2.9, las = 3))
}


#########################################################################
#########################################################################
#########################################################################


#####  VER UNA REBANADA Y CAMPO DE VELOCIDADES VERTICALES y HORIZONTALES
{
  int_dudv_dz <- v$int_dudv_dz
  v_vertical <- v$int_dudv_dz[,,10,25]
  
  east <- ncvar_get(nc,'uo')
  north <- ncvar_get(nc, 'vo')
  
  ue <-  east[,,1,28]
  vn <- north[,,1,28]
  
  LO <- ncvar_get(nc,'longitude')  #size 384 E: -60.08 O: -89.9
  LA <- ncvar_get(nc,'latitude') 
  
  LOn <- head(LO, -1)
  LAn <- head(LA, -1)
  
  LO2 <- rbind(replicate(205,LO))
  
  LA2 <- t(rbind(replicate(384, LA)))

  p <- 20 #distacia de flechas
  palV <- colorRampPalette((c("#A90C38","#F59885", "white","#C3C8D2","#2E5A87")))
  palV2 <- colorRampPalette((c("#A90C38", "white","#2E5A87")))
  
  filled.contour(x = LOn, y = LAn, z = v_vertical,
                 xlab='Longitud', ylab='Latitud', 
                 color.palette = palV2,
                 plot.axes = {axis(1); axis(2)
                   quiver(x = LO2[seq(1, length(LO2), p)],
                          y = LA2[seq(1, length(LA2), p)],
                          u = ue [seq(1, length(ue), p)],
                          v = vn [seq(1, length(vn), p)], 
                          scale = 0.05, angle = 10)})
  
 
  # SIRVE, PERO SI QUEDA FUERA DEL FILLED.CONTOUR 
  # SE DESAJUSTAN LOS MÁRGENES 
   quiver(x = LO2[seq(1, length(LO2), p)],
         y = LA2[seq(1, length(LA2), p)],
         u = ue [seq(1, length(ue), p)],
         v = vn [seq(1, length(vn), p)], 
         scale = 0.05, angle = 10)
}

#####  VER UNA REBANADA Y CAMPO DE VELOCIDADES VERTICALES y HORIZONTALES
##### GRAFICAR LO ANTERIOR CON EL POLÍGONO DE INTERÉS 

{
  
#USAR rutina 'funREMO' para extraer los datos de 
# remolinos ciclónicos o anticiclónicos 

r <- 145
rem <- which(track==remboy[r])

fechas <- t1[rem]
fecha_buscada = "2019-11-25"
posicion <- which(fechas == fecha_buscada)
dat <- rem[posicion]
#lines(conlon[,dat]-360, conlat[,dat], col = "red3")

p <- 20
palV <- colorRampPalette((c("#A90C38","#F59885", "white","#C3C8D2","#2E5A87")))
palV2 <- colorRampPalette((c("#A90C38", "white","#2E5A87")))

filled.contour(x = LOn, y = LAn, z = v_vertical,
               xlab='Longitud', ylab='Latitud', 
               color.palette = palV2,
               plot.axes = {axis(1); axis(2)
                 quiver(x = LO2[seq(1, length(LO2), p)],
                        y = LA2[seq(1, length(LA2), p)],
                        u = ue [seq(1, length(ue), p)],
                        v = vn [seq(1, length(vn), p)], 
                        scale = 0.05, angle = 10)
                 lines(conlon[,dat]-360, conlat[,dat], 
                       col = "black", lwd = 3.5)})
}
