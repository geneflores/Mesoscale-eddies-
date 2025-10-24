
#--------------------------------------------------------------------------------------
######  REMOLINOS DE PERIODO LARGO - ATLAS DE AVISO  #####
#--------------------------------------------------------------------------------------
## LEER NetCDF y EXTARER VARIABLES 

rm(list = ls()) # Limpia el espacio de memoria 
graphics.off()  # Cierra todas las gráficas 


#---------------------------------------------------------------------------------------------------
# aviso_netcdf: A script that reads netcdf files and
# subsets the data to the area of interest, for process and plotting.
# The matrix results is saved as structures.
#--------------------------------------------------------------------------------------------------

#Llamar paqueterías
pacman::p_load(ncdf4,R.utils,maps,pracma,RColorBrewer,paletteer,animation)

#Visualizar y leer el contenido de un archivo NetCDF
setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic")
alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Cyclonic_long_19930101_20220209.nc"))

nc <- nc_open(alist)
conlat <- ncvar_get(nc,'effective_contour_latitude')
conlon <- ncvar_get(nc,'effective_contour_longitude')
lat <- ncvar_get(nc,'latitude')
lon <- ncvar_get(nc,'longitude')

time <- ncvar_get(nc,'time') #days since 1950-01-01 00:00:00 UTC
t1 <- as.Date(time, origin = "1950-01-01 00:00:00", tz='UTC')


#--------------------------------------------------------------------------------------
#Convetir LONGITUDES y recortar al cuadrante (zona de estudio)
#--------------------------------------------------------------------------------------
#1. Transformar longitudes reales de 'conlon' y 'lon' (están de 0 a 360)
lon1 <- lon

lon1[lon > 180] <- lon1[lon > 180] - 360
conlon[conlon[,1] > 180,1] <- lon1[lon > 180] - 360

#CARIBE 
minlat <- 8; maxlat <- 22 
minlon <- -89; maxlon <- -75

LATLIMS <- c(minlat, maxlat) #Crea vectores secuenciales
LONLIMS <- c(minlon, maxlon) #con los máximos y mínimos establecidos 

#2. Recortar longitudes y latitudes al cuadrante 

ilon <- which(lon1 >= minlon & lon1 <= maxlon)
ilat <- which(lat >= minlat & lat <= maxlat)

ilat1 <- which(lat[ilon] >= minlat & lat[ilon] <= maxlat)
ilon1 <- which(lon1[ilat] >= minlon & lon1[ilat] <= maxlon)

#3. Establecer índices a cada remolino 

ilalo <- ilat[ilon1]  #Indica 123,795

#-----------------------------------------------------------------------------------------------
# Ver los polígonos de los remolinos en la zona de estudio 
#-----------------------------------------------------------------------------------------------

par(mar = c(4.1,4.1,1.1,1.1))
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
plot(c(minlon, maxlon), c(minlat, maxlat), "n", xlab = 'Lon', ylab = 'Lat')
for (i in 1:length(ilalo)){
  lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'cadetblue3')
}
draw.map()
grid()

#--------------------------------------------------------------------------------------
#Identificar los REMOLINOS y FECHAS que pueden estudiarse de manera MANUAL
#--------------------------------------------------------------------------------------

#1. Probar individualmente
graphics.off() #Cierra todas las gráficas 
par(mar = c(4.1,4.1,1.1,1.1))
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
plot(c(minlon, maxlon), c(minlat, maxlat), "n", xlab = 'Lon', ylab = 'Lat')

for (i in 123784:123795){
  lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'aquamarine4')
}

for (i in 123772:123783){
  lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'coral3')
}

for (i in 123755:123771){
  lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'lavenderblush4')
}

for (i in 123744:123754){
  lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'lightpink3')
}

for (i in 123718:123743){
  lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'olivedrab4')
}

for (i in 123713:123717){
  lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'red4')
}

draw.map()
grid()

#2. Identificar fechas

#tilalo1 <- t1[ilalo[123784:123795]]
#tilalo2 <- t1[ilalo[123772:123783]]
#tilalo3 <- t1[ilalo[123755:123771]]
#tilalo4 <- t1[ilalo[123744:123754]]
#tilalo5 <- t1[ilalo[123718:123743]]
#tilalo6 <- t1[ilalo[123713:123717]]

#3. Hacer otras pruebas con otras variables 

#amp <- ncvar_get(nc,'amplitude')
#area <- ncvar_get(nc,'effective_area')
#radio <- area <- ncvar_get(nc,'effective_radius')
ncon <- ncvar_get(nc,'num_contours')
obn <- ncvar_get(nc,'observation_number') 
track <- ncvar_get(nc,'track')
#uavg <- ncvar_get(nc,'uavg_profile') #tarda en abrir 

#-------------------------------------------------------------------------------------------------
#DETERMINAR CUÁNTOS REMOLINOS HAY EN NUESTRA ÁREA DE ESTUDIO 
#-------------------------------------------------------------------------------------------------
obnlong=obn[ilalo]

secuencia <- c(obnlong)
indices <- which(secuencia == 0)
limites <- c(indices, length(secuencia))

eddy <- list()

if (length(limites) > 1) {
  for (i in seq_along(limites)[-length(limites)]) {
    inicio <- limites[i]
    fin <- limites[i + 1] - 1   #Tomar en cuenta que al último eddy le faltará un valor 
    eddy[[i]] <- secuencia[inicio:fin]  
  }
}

## Hay 2866 EDDYS de 1993-2022
#---------------------------------------------------------------------------------------------------
#GRAFICAR POR NÚMERO DE EDDY 
#---------------------------------------------------------------------------------------------------

#graphics.off() #Cierra todas las gráficas 


par(mar = c(4.1,4.1,1.1,1.1))
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
plot(c(minlong, maxlong), c(minlat, maxlat), "n", xlab = 'Lon', ylab = 'Lat')
draw.map()
grid()

ind_eddy <- 1652
inicio <- limites[ind_eddy] 

fin <- limites[ind_eddy + 1] - 1

  for (i in inicio:fin){
    
    texto_x <- mean(conlon[ilalo-360])
    texto_y <- mean(conlat[ilalo])
    
    text(texto_x, texto_y, labels = i, col = 'gray25', cex = 0.6)
    
    lines(conlon[,ilalo[i]]-360, conlat[,ilalo[i]], col = 'palegreen4')
  }

#2355 - 'indianred4'
#1431 - 'palegreen4'
#1926 - 'lightgoldenrod4'
#2545 - 'lightpink3'
#1652 - 'lightskyblue'


#1322 - 'lighskyblue4'


#--------------------------------------------------------------------------
# GRAFICAR POR GRUPOS
#--------------------------------------------------------------------------

#graphics.off() #Cierra todas las gráficas 
par(mar = c(4.1,4.1,1.1,1.1))
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
plot(c(minlon, maxlon), c(minlat, maxlat), "n", xlab = 'Lon', ylab = 'Lat')
grid()
draw.map()

colores <- paletteer_d("colorBlindness::SteppedSequential5Steps")

for (i in 2770:2784){
  
  
  liminf <- limites[i]
  limsup <- limites[i + 1]
   
  color_index <- i - 2769
  
  texto_x <- mean(conlon[1, ilalo[liminf:limsup]]-360)
  texto_y <- mean(conlat[1, ilalo[liminf:limsup]])
  
  text(texto_x, texto_y, labels = i, col = 'gray25', cex = 0.6)
  
  for (j in liminf:limsup){
    lines(conlon[,ilalo[j]]-360, conlat[,ilalo[j]], col = colores [color_index])
    
  }
}



#----------------------------------------------------------------------------------------------------
#GRAFICAR REMOLINO POR REMOLINO 
#----------------------------------------------------------------------------------------------------

for (i in 1:(length(limites)-1)){
  
  graphics.off()
  par(mar = c(4.1,4.1,1.1,1.1))
  draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
  plot(c(minlon, maxlon), c(minlat, maxlat), "n", xlab = 'Lon', ylab = 'Lat')
  grid
  draw.map()
  
  if (i == 1){
    
    liminf <- 1
    limsup <- 17
    
  } else{
    
    liminf <- (limites[i]+1)
    limsup <- limites[i+1]
  
  }
  
  
  texto_x <- mean(conlon[1, ilalo[liminf:limsup]]-360)
  texto_y <- mean(conlat[1, ilalo[liminf:limsup]])
  
  text(texto_x, texto_y, labels = i, col = 'gray25', cex = 0.6)
  
  for (j in liminf:limsup){
    lines(conlon[,ilalo[j-1]]-360, conlat[,ilalo[j-1]], col = 'palegreen3')
    
  }
  
  print(paste0("Este es el ciclo ", i, " Límite inferior: ", 
               liminf, " Límite superior: ", limsup))
  cat("Presiona ENTER para continuar...")
  readline()
}





#--------------------------------------------------------------------------
# GRAFICAR UNA FIGURA ANIMADA - SIN TERMINAR 
#--------------------------------------------------------------------------

par(mar = c(4.1,4.1,1.1,1.1))
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
plot(c(minlon, maxlon), c(minlat, maxlat), "n", xlab = 'Lon', ylab = 'Lat')
draw.map()
grid()


colores <- paletteer_d("ggsci::default_igv")

ani.options(nmax = 2866)


for (i in 1:2866) {
  
  liminf <- limites[i]
  limsup <- limites[i + 1]
  
  labs(title = 'Remolino de 1 a 50')
  
  color_index <- i 
  
  texto_x <- mean(conlon[1, ilalo[liminf:limsup]] - 360)
  texto_y <- mean(conlat[1, ilalo[liminf:limsup]])
  
  text(texto_x, texto_y, labels = i, col = 'gray25', cex = 0.6)
  
  for (j in liminf:limsup) {
    lines(conlon[, ilalo[j]] - 360, conlat[, ilalo[j]], col = colores[color_index])
  }
  
}

## REVISAR FECHAS DE LOS REMOLINOS DE INTERÉS 

#1652 - 'lightskyblue'
#2355 - 'indianred4'
#1431 - 'palegreen4'
#1926 - 'lightgoldenrod4'
#2545 - 'lightpink3'


te1 <- t1[ilalo[71687:71784]]  #remolino 1652 > 24/04/2010 - 01/05/2010
te2 <- t1[ilalo[102213:102234]]#remolino 2355 > 15/01/2017 - 04/02/2017
te3 <- t1[ilalo[61548:61672]] #remolino 1431 > 21/07/2007 - 22/11/2007
te4 <- t1[ilalo[83391:83415]] #remolino 1926 > 24/09/2012 - 18/10/2012


