########################################################
############# DENSIDAD POTENCIAL - TEOS 10 #############
########################################################

library(gsw)

setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_noviembre19.nc"))
nc <- nc_open(alist) #print(nc) 


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
     }
   #1.2 Convertir profundidad a presión 
   p <- gsw_p_from_z(z = z, latitude = la)
}

#### 2. Convertir la salinidad práctia (PSU) a Salinidad absoluta
{
   # Variables: SP, p, lat, lon 

   setwd("C:/Users/genef/Documents/Remolinos")
   load("transectos_cy145.RData")               #Transectos de sal y temp de las fechas de interés
   load("coor145.RData")  

   lon <- x_valores
   lat <- y_valores
   
   SP <- nov25$sal
   
   SA <- matrix(NA, nrow = 1027, ncol = 35)
   for(i in (1:length(lat))){
     for(j in (1:length(p))){
   
     SA[i,j] <- gsw_SA_from_SP(SP = SP[i, j], p = p,
                                    longitud = lon,
                                    latitude = lat)
     }
   }
}

#### 3. Calculamos la Temperatura conservativa a partir de la temperatura potencia 
{
  # Variables que se requieren 
  # SA = Salinidad absoluta 
  # pt = potencial de temperatura 
  
  #NOTA: Modelo Glorys proporciona la temperatura potencial
  
  pt <-  nov25$temp  
  
  CT <- gsw_CT_from_pt(SA,pt)
  
}

#### 4. Calculamos la Densidad potencial 
{
  # Variables requeridas:
  # SA = Salinidad absoluta 
  # CT = Temperatura conservativa 
  # p  = presión (dbar)
  
  rho <- gsw_rho(SA, CT, p)
  
}
   
### 5. Anomalía de densidad potencial - kg/m^3
{
  # Variables requeridad: 
  # SA = Salinidad absoluta 
  # CT = Temperatura conservativa 
  
  sigma_theta <- gsw_sigma0(SA, CT) 
  
}


############# GRAFICAR DATOS #############

# 1. Crear eje x con distancias en Km
{
  dis<-0
  for (k in 1:(length(x_valores)-1)){
    aux<-distm(c(x_valores[k],x_valores[k]),
               c(x_valores[k+1],x_valores[k+1]),
               fun = distHaversine)
    dis[k+1]<-dis[k] + aux
  }
  
  dis <- as.numeric(format(0.8+dis/1e3,digits=2))
  x_vals <- dis-max(dis)/2
  
}

# 2. Generar líneas longitudinales #NOTA: x e y cambiar de acuerdo a la fecha
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
}

# 3. 
  
prof<-seq(min(Z),max(Z),length=length(Z))-max(Z)

# Densidad potencial 
palD <- colorRampPalette((c("#80146E","#7958AC","#2BACC2","#C4E8BB")))
palD <- colorRampPalette((c("#001889","#A40A8A","#E78140","#DAFF47")))
filled.contour(x_vals,prof,rot90(rho,2),
               xlab='distance [Km]', ylab='depth [m]', 
               color.palette = palD,
               #zlim = c(34.5, 37.5),
               plot.axes = {axis(1); axis(2)
                 abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                 abline(v = dis2, col = 'black', lwd = 3, lty = 2)}, 
               key.title = mtext("Densidad potencial [kg/m"^3*"]", 
                                side = 4, line = 2.9, las = 3))

# Anomalía de densidad potencial 
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
                        v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 100)
                 abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                 abline(v = dis2, col = 'black', lwd = 3, lty = 2)}, 
               key.title = mtext(expression("Anomalía de densidad potencial [kg/m"^3*"]"), 
                                 side = 4, line = 2.9, las = 3))
