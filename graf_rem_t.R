####################################################
##   INDICAR POSICIÓN DEL REMOLINO EN EL TIEMPO   ##
####################################################

# 1. EXTRAER DEL SCRIP funREMO.R los datos de 
#    remolinos ciclónicos o anticiclónicos 

# 2. EXTRAER los puntos de intersección entre
#   la recta y los polígonos 

# fp = fecha a partir de la que queremos generar un transecto
# fi = posición del remolino días (~15) después de su formación 
# fm = posición del remolino a mitad de su evolución 
# fm = posición del remolino días (~15) antes de su disipación 


# 3. Perfil a partir de LONGITUDES  
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

valores <- polilont( r = 145, fp = "2020-01-06",
                     fi = "2019-11-25", 
                     fm = "2020-01-10", 
                     ff = "2020-02-08",
                     a2 = 4, b2 = 3.8)

# 4. Extraemos datos de interés 
x_valores <- valores$x_valores   #Coordenadas de la recta en el eje x 
y_valores <- valores$y_valores   #Coordenadas de la recta en el eje y
coor <- valores$coordenadas   #Coordenadas puntuales de las intersecciones 

# 5. Extraemos perfiles de SALINIDAD y TEMPERATURA 
setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS")
alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_noviembre19.nc"))
nc <- nc_open(alist) #print(nc) 

perfil_latitudinal  <- function(day, x1, x2, y1, y2) {
  
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
  
  # 3. Crear eje x con distancias en Km
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
  
  # 4. Generar líneas longitudinales #NOTA: cambiar de acuerdo a la fecha
  {
    punto_central <- c(median(x_valores), median(y_valores))
    punto1 <- c(coor$longitud[x1], coor$latitud[x1])
    punto2 <- c(coor$longitud[x2], coor$latitud[x2])
    
    dis_centro_1 <- distHaversine(punto_central, punto1)
    dis_centro_2 <- distHaversine(punto_central, punto2)
    
    dis1 <- dis_centro_1/1000*y1  #1,2,3 multiplicar por *-1
    dis2 <- dis_centro_2/1000*y2    #4,5,6 NO multiplicar
  }
  
  # 5. Extraer perfiles y guardarlos en la carpeta indicada 
  #setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS/anticy151")
  
  {  
  #Salinidad
    {
   palS <- colorRampPalette((c("cyan","#841859","darkolivegreen2","#005600")))
   prof<-seq(min(depth),max(depth),length=dim(salinity)[3])-max(depth)
   x_vals <- dis-max(dis)/2
   
   # png(paste0("Sal_", fechas[day], ".png"), type = "cairo", 
   #     width = 1200, height = 800, 
   #     units = "px", res = 165, pointsize = 12 )
  
  filled.contour(x_vals,prof,rot90(transect_data_sal,2),
                  xlab='distance [Km]', ylab='depth [m]', 
                  color.palette = palS,
                 zlim = c(34.5, 37.5),
                 plot.axes = {axis(1); axis(2)
                   abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                   abline(v = dis2, col = 'black', lwd = 3, lty = 2)}, 
                 key.title = mtext("Salinidad [PSU]", 
                                   side = 4, line = 2.9, las = 3))
   
   title( paste0 (fechas[day]), cex.main = 3)
   
   #dev.off()
    }
    
  #Temperatura 
    {
  palT <- colorRampPalette(c("#3E0689","#AE1987","#EA8D2D","#E3E400")) #temp
  
  prof<-seq(min(depth),max(depth),length=dim(temperature)[3])-max(depth)
  x_vals <- dis-max(dis)/2
  
  # png(paste0("Temp_", fechas[day], ".png"), type = "cairo", 
  #     width = 1200, height = 800, 
  #     units = "px", res = 165, pointsize = 12 )
  
   filled.contour(x_vals,prof,rot90(transect_data_temp,2),
                  xlab='distance [Km]', ylab='depth [m]',
                  color.palette = palT,
                  zlim = c(5, 30),
                  plot.axes = {axis(1); axis(2); axis(3);
                    abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                    abline(v = dis2, col = 'black', lwd = 3, lty = 2)
                    (quiver(x = x2[seq(1, length(x2), step)], 
                          y = y2[seq(1, length(y2), step)],
                            u = u0[seq(1, length(u0), step)], 
                            v = transect_data_vv[seq(1, length(transect_data_vv), step)] * 100))},
                  key.title = mtext("Temperatura [°C]",
                                   side = 4, line = 2.9, las = 3))
   
  title( paste0 (fechas[day]), cex.main = 3)
  
  #dev.off()
    }
  }
  
  # 6. Guardar valores necesarios 
  
  # list(sal  = transect_data_sal,
  #      temp = transect_data_temp)
  
}

perfil_latitudinal(day = 25, 
                   x1 = 1, x2 = 2,
                   y1 = -1, y2 = -1)

jun07 <- perfil_latitudinal(day = 7,
                   x1 = 5, x2 = 6,
                   y1 = 1, y2 = 1)

# 6. Generar perfiles de ANOMALÍAS INSTANTÁNEAS de Sal y Temp

# 6.1 Realizar el cálculo
{
# Cargar las matrices de nuevo
#setwd("C:/Users/genef/Documents/Remolinos")
#load("transectos_cy145.RData")
#load("transectos_anticy151.RData")

f1 <- abr20
  f2 <- may10
    f3 <- jun07

mean_trans_sal  <- (f1$sal + f2$sal + f3$sal) / 3
mean_trans_temp <- (f1$temp + f2$temp + f3$temp) / 3

anomalia_sal1 <- f1$sal - mean_trans_sal
anomalia_sal2 <- f2$sal - mean_trans_sal
anomalia_sal3 <- f3$sal - mean_trans_sal

anomalia_tem1 <- f1$temp - mean_trans_temp
anomalia_tem2 <- f2$temp - mean_trans_temp
anomalia_tem3 <- f3$temp - mean_trans_temp
}

# 6.2 Generar el gráfico 

anomalias <- function(anomalia_s,anomalia_t, fecha, x1, x2, y1, y2) {
   # EXTRAER depth de archivo GLORYS correspondiente a la fecha
   depth <- ncvar_get(nc,'depth')    #size 31  Max: 453.93
  
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
    
  }
  
  # 2. Generar líneas longitudinales #NOTA: cambiar de acuerdo a la fecha
  {
    punto_central <- c(median(x_valores), median(y_valores))
    punto1 <- c(coor$longitud[x1], coor$latitud[x1])
    punto2 <- c(coor$longitud[x2], coor$latitud[x2])
    
    dis_centro_1 <- distHaversine(punto_central, punto1)
    dis_centro_2 <- distHaversine(punto_central, punto2)
    
    dis1 <- dis_centro_1/1000*y1  #1,2,3 multiplicar por *-1
    dis2 <- dis_centro_2/1000*y2  #4,5,6 NO multiplicar
  }
  
  # 3. Extraer perfiles y guardarlos en la carpeta indicada 
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/anomalias/anticy151")
  {  
    #Anomalía Salinidad
    {
      palS <- colorRampPalette((c("cyan","#841859","darkolivegreen2","#005600")))
      prof<-seq(min(depth),max(depth),length=dim(anomalia_s)[2])-max(depth)
      x_vals <- dis-max(dis)/2
      
      png(paste0("Sal_", fecha , ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
      
      filled.contour(x_vals,prof,rot90(anomalia_s,2),
                     xlab='distance [Km]', ylab='depth [m]', 
                     color.palette = palS,
                     zlim = c(-1.1, 1),
                     plot.axes = {axis(1); axis(2)
                       abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                       abline(v = dis2, col = 'black', lwd = 3, lty = 2)}, 
                     key.title = mtext("Anoamalía de salinidad [PSU]", 
                                       side = 4, line = 2.9, las = 3))
      
      title( paste0 (fecha), cex.main = 3)
      
      dev.off()
    }
    
    #Anomalía Temperatura 
    {
      palT <- colorRampPalette(c("#3E0689","#AE1987","#EA8D2D","#E3E400")) #temp
      prof<-seq(min(depth),max(depth),length=dim(anomalia_t)[2])-max(depth)
      x_vals <- dis-max(dis)/2
      
      png(paste0("Temp_", fecha, ".png"), type = "cairo", 
         width = 1200, height = 800, 
        units = "px", res = 165, pointsize = 12 )
      
      filled.contour(x_vals,prof,rot90(anomalia_t,2),
                     xlab='distance [Km]', ylab='depth [m]',
                     color.palette = palT,
                     zlim = c(-2.8, 2.8),
                     plot.axes = {axis(1); axis(2)
                       abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                       abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                     key.title = mtext("Anomalía de temperatura [°C]",
                                       side = 4, line = 2.9, las = 3))
      
      title( paste0 (fecha), cex.main = 3)
      
      dev.off()
    }
  }
  
}

anomalias(anomalia_s = anomalia_sal3, 
          anomalia_t = anomalia_tem3, 
          fecha = "2020-06-07",
          x1 = 5, x2 = 6,
          y1 = 1, y2 = 1)


# 7. Extraemos perfiles de parámetros BIOGEOQUÍMICOS 
setwd("C:/Users/genef/Documents/Remolinos/CMEMS/HINDCAST")
alist <- dir(pattern = glob2rx("cmems_mod_glo_bgc_my_0.25deg_P1D-m_abril20.nc"))
nc <- nc_open(alist) #print(nc) 

  
  #1. Extraer variables del netCDF 
  {
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
  }
  
  #2. Genear trasectos 
  {
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
  
  for (i in 1:dim(si)[3]){
    transect_data_si <-cbind(transect_data_si, 
                              interp2(x = lon, 
                                      y = lat, 
                                      Z = t(as.matrix(si[,,i])),
                                      xp = x_valores, 
                                      yp = y_valores, 
                                      method = 'linear'))
  }
  
  }
  
  #3. Crear coordenada X a partir de distancias en km 
  {
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
  }
  
  # Poner líneas longitudinales #NOTA: Cambiar de acuerdo a la fecha 
  {
    punto_central <- c(median(x_valores), median(y_valores))
    punto1 <- c(coor$longitud[x1], coor$latitud[x1])  #Cambiar de acuerdo 
    punto2 <- c(coor$longitud[x2], coor$latitud[x2])  # a la fecha 
    
    dis_centro_1 <- distHaversine(punto_central, punto1)
    dis_centro_2 <- distHaversine(punto_central, punto2)
    
    dis1 <- dis_centro_1/1000*y1    ##1,2,3 se multiplica por *-1
    dis2 <- dis_centro_2/1000*y2  ##4,5,6 no se multiplica 
  }
  
  #4. Extraer transecto de cada parámetro y gurdarlo en la carpeta indicada
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/HINDCAST/anticy151")
  
  {
  ######  Clorofila
  {
  palS <- colorRampPalette((c("#4B1D91","#B0179C","#F28265","#EBCB83")))
  prof<-seq(min(depth),max(depth),length=dim(chl)[3])-max(depth)
  
  png(paste0("Chl_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot1 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_chl,2),
                          xlab='distance [Km]', ylab='depth [m]', 
                          cex.lab = 1.5, cex.axis = 3,
                          color.palette = palS,
                          zlim = c(0, 0.6),
                          plot.axes = {axis(1); axis(2)
                            abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                            abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                          key.title = mtext(expression("Clorofila [mg/m"^3*"]"), 
                                            side = 4, line = 2.9, las = 3))
  
  title( paste0 (fechas[day]), cex.main = 3)
  
  dev.off()
  }

  ######   Nitratos 
  {
  palT <- colorRampPalette(c("#E24C80","#F08370","#F9C783","#FCECA8")) 
  prof<-seq(min(depth),max(depth),length=dim(no3)[3])-max(depth)
  
  png(paste0("NO3_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot2 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_no3,2),
                          xlab='distance [Km]', ylab='depth[m]',
                          cex.lab = 1.5, cex.axis = 3,
                          color.palette = palT,
                          zlim = c(0, 30),
                          plot.axes = {axis(1); axis(2)
                            abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                            abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                          key.title = mtext(expression("Nitratos [mmol/m"^3*"]"), 
                                            side = 4, line = 2.9, las = 3))
  title(paste0(fechas[day]), cex.main = 3)
  
  dev.off()
  }
  
  ######  Fitoplancton - Producción primaria neta  
  {
  palT <- colorRampPalette(c("#255668", "#21A97E", "#94D268","#CDE55C","#EDEF5C"))
  prof<-seq(min(depth),max(depth),length=dim(nppv)[3])-max(depth)
  
  png(paste0("NPPV_", fechas[day], ".png"), type = "cairo",
      width = 1200, height = 800,
      units = "px", res = 165, pointsize = 12 )
  
  plot3 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_nppv,2),
                          xlab='distance [Km]', ylab='depth[m]',
                          cex.lab = 1.5, cex.axis = 3,
                          color.palette = palT,
                          zlim = c(0, 27),
                          plot.axes = {axis(1); axis(2)
                            abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                            abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                          key.title = mtext(expression("NPPV [mg/m"^3*".día]"), 
                                            side = 4, line = 2.9, las = 3))
  title(paste0(fechas[day]),cex.main = 3)
  
  dev.off()
  }
  
  ###### Oxigeno disuelto 
  {
  palT <- colorRampPalette(c("#26185F","#006DAA", "#56C8B2","#D6F3CF"))
  prof<-seq(min(depth),max(depth),length=dim(o2)[3])-max(depth)
  
  png(paste0("O2_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot4 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_o2,2),
                          xlab='distance [Km]', ylab='depth[m]',
                          cex.lab = 1.5, cex.axis = 3,
                          color.palette = palT,
                          zlim = c(135, 230),
                          plot.axes = {axis(1); axis(2)
                            abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                            abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                          key.title = mtext(expression("Oxígeno disuelto [mmol/m"^3*"]"), 
                                            side = 4, line = 2.9, las = 3))
  title(paste0(fechas[day]), cex.main = 3)

  dev.off()
  }
  
  ######   Fosfatos 
  {
  palT <- colorRampPalette(c("#1C4E1C","#008D50","#69C2A7","#D1EEEC"))
  prof<-seq(min(depth),max(depth),length=dim(po4)[3])-max(depth)
  
  png(paste0("PO4_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot5 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_po4,2),
                          xlab='distance [Km]', ylab='depth[m]',
                          cex.lab = 1.5, cex.axis = 3,
                          color.palette = palT,
                          zlim = c(0,2),
                          plot.axes = {axis(1); axis(2)
                            abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                            abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                          key.title = mtext(expression("Fosfatos [mmol/m"^3*"]"), 
                                            side = 4, line = 2.9, las = 3))
  title(paste0(fechas[day]), cex.main = 3)

  dev.off()
  }
  
  ######  Silicato disuelto 
  {
  palT <- colorRampPalette(c("#540046","#7D3991","#97A8D1","#D8EAF1"))
  prof<-seq(min(depth),max(depth),length=dim(si)[3])-max(depth)
  
  png(paste0("SI_", fechas[day], ".png"), type = "cairo", 
      width = 1200, height = 800, 
      units = "px", res = 165, pointsize = 12 )
  
  plot6 <- filled.contour(dis-max(dis)/2,prof,rot90(transect_data_si,2),
                          xlab='distance [Km]', ylab='depth[m]',
                          cex.lab = 1.5, cex.axis = 3,
                          color.palette = palT,
                          zlim = c(1,29),
                          plot.axes = {axis(1); axis(2)
                            abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                            abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                          key.title = mtext(expression("Silicato disuelto [mmol/m"^3*"]"), 
                                            side = 4, line = 2.9, las = 3))
  title(paste0(fechas[day]), cex.main = 3)
    
  dev.off()
  }
  
  }
  

perfil_biogeoquimico(day = 20,
                     x1 = 1, x2 = 2,
                     y1 = -1, y2 = 1)

#Solo extraer variables 
perfil_biogeoquimico2  <- function(day) {
  
  #1. Extraer variables del netCDF 
  {
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
  }
  
  #2. Genear trasectos 
  {
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
    
    for (i in 1:dim(si)[3]){
      transect_data_si <-cbind(transect_data_si, 
                               interp2(x = lon, 
                                       y = lat, 
                                       Z = t(as.matrix(si[,,i])),
                                       xp = x_valores, 
                                       yp = y_valores, 
                                       method = 'linear'))
    }
    
    
  }
  
  #3. Guardar transectos de cada parámetro 
  list(chl  = transect_data_chl,
       no3 = transect_data_no3,
       nppv = transect_data_nppv,
       o2 = transect_data_o2,
       po4 = transect_data_po4,
       si = transect_data_si)
  
}
  
feb08 <- perfil_biogeoquimico2(day = 8)


# 8. Generar perfiles de ANOMALÍAS INSTANTÁNEAS de 
#    parámetros biogeoquímicos 

#save(nov25, ene10, feb08, file = "transectos_biogeo145.RData")

# 8.1 Realizar el cálculo
{
  # Cargar las matrices de nuevo
  #setwd("C:/Users/genef/Documents/Remolinos")
  #load("transectos_biogeo145.RData")
  #load("transectos_biogeo151.RData")
  
  f1 <- abr20
  f2 <- may10
  f3 <- jun07
  
  mean_trans_chl  <- (f1$chl + f2$chl + f3$chl) / 3
  mean_trans_o2 <- (f1$o2 + f2$o2 + f3$o2) / 3
  mean_trans_nppv <- (f1$nppv + f2$nppv + f3$nppv) / 3 
  
  anomalia_chl1 <- f1$chl - mean_trans_chl
  anomalia_chl2 <- f2$chl - mean_trans_chl
  anomalia_chl3 <- f3$chl - mean_trans_chl
  
  anomalia_o2_1 <- f1$o2 - mean_trans_o2
  anomalia_o2_2 <- f2$o2 - mean_trans_o2
  anomalia_o2_3 <- f3$o2 - mean_trans_o2
  
  anomalia_nppv1 <- f1$nppv - mean_trans_nppv
  anomalia_nppv2 <- f2$nppv - mean_trans_nppv
  anomalia_nppv3 <- f3$nppv - mean_trans_nppv
}

anomalias_biogeo  <- function(anomalia_chl, anomalia_o2, anomalia_nppv,
                                  fecha, x1, x2, y1, y2) {

  # EXTRAER depth de archivo HINDCAST correspondiente a la fecha
  depth <- ncvar_get(nc,'depth')    
  
  # 1. Crear coordenada X a partir de distancias en km 
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
  
  # 2. Poner líneas longitudinales #NOTA: Cambiar de acuerdo a la fecha 
  {
    punto_central <- c(median(x_valores), median(y_valores))
    punto1 <- c(coor$longitud[x1], coor$latitud[x1])  #Cambiar de acuerdo 
    punto2 <- c(coor$longitud[x2], coor$latitud[x2])  # a la fecha 
    
    dis_centro_1 <- distHaversine(punto_central, punto1)
    dis_centro_2 <- distHaversine(punto_central, punto2)
    
    dis1 <- dis_centro_1/1000*y1    ##1,2,3 se multiplica por *-1
    dis2 <- dis_centro_2/1000*y2  ##4,5,6 no se multiplica 
  }
  
  # 3. Extraer transecto de cada parámetro y gurdarlo en la carpeta indicada
  setwd("C:/Users/genef/Documents/Remolinos/CMEMS/anomalias/anticy151")
  
  {
    ######  Clorofila
    {
      palS <- colorRampPalette((c("#4B1D91","#B0179C","#F28265","#EBCB83")))
      prof<-seq(min(depth),max(depth),length=dim(anomalia_chl)[2])-max(depth)
      
      png(paste0("Anomalía_Chl_", fecha, ".png"), type = "cairo", 
          width = 1200, height = 800, 
          units = "px", res = 165, pointsize = 12 )
      
      plot1 <- filled.contour(dis-max(dis)/2,prof,rot90(anomalia_chl,2),
                              xlab='distance [Km]', ylab='depth [m]', 
                              cex.lab = 1.5, cex.axis = 3,
                              color.palette = palS,
                              zlim = c(-0.15, 0.25),
                              plot.axes = {axis(1); axis(2)
                                abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                                abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                              key.title = mtext(expression("Anomalía de clorofila [mg/m"^3*"]"), 
                                                side = 4, line = 2.9, las = 3))
      
      title( paste0 (fecha), cex.main = 3)
      
      dev.off()
    }

    ######  Fitoplancton - Producción primaria neta  
    {
      palT <- colorRampPalette(c("#255668", "#21A97E", "#94D268","#CDE55C","#EDEF5C"))
      prof<-seq(min(depth),max(depth),length=dim(anomalia_nppv)[2])-max(depth)
      
      png(paste0("Anomalía_NPPV_", fecha, ".png"), type = "cairo",
          width = 1200, height = 800,
          units = "px", res = 165, pointsize = 12 )
      
      plot3 <- filled.contour(dis-max(dis)/2,prof,rot90(anomalia_nppv,2),
                              xlab='distance [Km]', ylab='depth[m]',
                              cex.lab = 1.5, cex.axis = 3,
                              color.palette = palT,
                              zlim = c(-4.2, 7),
                              plot.axes = {axis(1); axis(2)
                                abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                                abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                              key.title = mtext(expression("Anomalía de NPPV [mg/m"^3*".día]"), 
                                                side = 4, line = 2.9, las = 3))
      title(paste0(fecha),cex.main = 3)
      
      dev.off()
    }
    
    ###### Oxigeno disuelto 
    {
      palT <- colorRampPalette(c("#26185F","#006DAA", "#56C8B2","#D6F3CF"))
      prof<-seq(min(depth),max(depth),length=dim(anomalia_o2)[2])-max(depth)
      
      png(paste0("Anomalía_O2_", fecha, ".png"), type = "cairo", 
          width = 1200, height = 800, 
          units = "px", res = 165, pointsize = 12 )
      
      plot4 <- filled.contour(dis-max(dis)/2,prof,rot90(anomalia_o2,2),
                              xlab='distance [Km]', ylab='depth[m]',
                              cex.lab = 1.5, cex.axis = 3,
                              color.palette = palT,
                              zlim = c(-15, 15),
                              plot.axes = {axis(1); axis(2)
                                abline(v = dis1, col = 'black', lwd = 3, lty = 2)
                                abline(v = dis2, col = 'black', lwd = 3, lty = 2)},
                              key.title = mtext(expression("Anomalía de oxígeno disuelto [mmol/m"^3*"]"), 
                                                side = 4, line = 2.9, las = 3))
      title(paste0(fecha), cex.main = 3)
      
      dev.off()
    }
    
  }
  
}

anomalias_biogeo(anomalia_chl = anomalia_chl3,
                 anomalia_o2 = anomalia_o2_3,
                 anomalia_nppv = anomalia_nppv3, 
                 fecha = "2020-06-07",
                 x1 = 5, x2 = 6,
                 y1 = 1, y2 = 1)
