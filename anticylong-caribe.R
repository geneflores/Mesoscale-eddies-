
#--------------------------------------------------------------------------------------
######  REMOLINOS ANTICICLÓNICOS DE PERIODO LARGO - ATLAS DE AVISO  #####
#--------------------------------------------------------------------------------------
## LEER NetCDF y EXTARER VARIABLES 

rm(list = ls()) # Limpia el espacio de memoria 
#graphics.off()  # Cierra todas las gráficas 

#---------------------------------------------------------------------------------------------------
# aviso_netcdf: A script that reads netcdf files and
# subsets the data to the area of interest, for process and plotting.
# The matrix results is saved as structures.
#--------------------------------------------------------------------------------------------------

#Llamar paqueterías
pacman::p_load(ncdf4,R.utils,maps,pracma,RColorBrewer,paletteer,animation)

#Visualizar y leer el contenido de un archivo NetCDF
setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Anticyclonic")
alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Anticyclonic_long_19930101_20220209.nc"))

nc <- nc_open(alist) #print(nc) -- displays nc file info 

lon <- ncvar_get(nc,'longitude')
lat <- ncvar_get(nc,'latitude')
area <- ncvar_get(nc,'effective_area')     #m^2
altura <- ncvar_get(nc,'amplitude')        #m
radio<- ncvar_get(nc,'effective_radius')   #m
conlat <- ncvar_get(nc,'effective_contour_latitude')
conlon <- ncvar_get(nc,'effective_contour_longitude')
ncon <- ncvar_get(nc,'num_contours')       #number of contours selected eddy
dur <- ncvar_get(nc,'observation_number')  #Lo que perduró un remolino
vel <- ncvar_get(nc,'speed_average')       #m/s Average speed of the contour defining the radius scale “speed_radius”
track <- ncvar_get(nc,'track')             #Trajectory identification number

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
minlat <- 8     ;   maxlat <- 33
minlon <- -100  ;   maxlon <- -60

LATLIMS <- c(minlat, maxlat) #Crea vectores secuenciales con
LONLIMS <- c(minlon, maxlon) #los máximos y mínimos establecidos 

#2. Establecer índices de cada remolino
irem<-c(1,(which(diff(dur)<0)+1))  #Resultan 805,895 remoli
#3. Obtener el polígono del Mar Caribe 
lonp <- -1*c(87.8747, 89.5015, 86.2106, 82.6176, 80.7975, 79.3380, 78.6719, 77.2931,
             62.2013, 61.3251, 61.3654, 61.2409, 62.3344, 63.09, 65.6213, 67.0605,
             68.5166, 71.0929, 73.2380, 80.8204, 82.2571, 84.3120, 87.1101, 87.8747)
latp <- c( 21.2433, 15.9497, 13.8241, 8.8217, 8.4549, 9.3329, 9.2758, 
           7.4149, 9.0671, 10.3527, 12.5059, 15.2304, 17.2452, 18.0037, 18.2441,
           18.3233, 18.6439, 19.5587, 19.8845, 22.8951, 22.9208, 22.2705, 
           21.2580, 21.2433)

#4. Encontrar remolinos que están en el Caribe 

inp  <- inpolygon(lon[irem]-360, lat[irem], lonp, latp) #FALSE
icar <- irem[inp] #ARROJA 4190 Remolinos anticiclónicos solo en el CARIBE

#5. Ver puntos de remolinos en el área
par(mar = c(4.1,4.1,1.1,1.1))
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
plot(c(minlon,maxlon),c(minlat,maxlat),"n",xlab='Lon',ylab='Lat')#,ann=F,axes=F)
points(lon[icar]-360, lat[icar], pch=16, col='green')
draw.map()
grid(col=1)


#6. Extraer datos de los remolinos encontrados 

tcar<-t1[icar]     #Fechas remolinos caribeños
dcar<-dur[icar]    #Duración/Observación
rcar<-radio[icar]  #Radio
hcar<-altura[icar] #Altura
acar<-area[icar]   #Area
vcar<-vel[icar]    #Velocidad de giro
ncar<-ncon[icar]   #Número de contornos por remolino
clon<-conlon[,icar]
clat<-conlat[,icar]
idcar<-track[icar] #Identificador de remolino


#--------------------------------------------------------------------------------------
#ENCONTRAR BOYAS que coincidan en TIEMPO y ESPACIO
#--------------------------------------------------------------------------------------

#Cargar base de datos de las boyas 
load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")

#1. Número de boyas ARGO en zona de estudio 
ini  <- c(1,(which(diff(datos$pres)<0)+1)) #Número de boyas
lat1 <- datos$lat[ini]  #lat de cada boya  
lon1 <- datos$lon[ini]  #lon de cada boya 
ti   <- datos$time[ini] #Fecha de medición de cada boya


#2. Para graficar el perfil de profundidad por boya 
plot(datos$CT[ini[1]:(ini[2]-1)],-datos$pres[ini[1]:(ini[2]-1)],'l',xlab='CT(deg C)',ylab='Press(db)') # primer boya
plot(datos$CT[ini[length(ini)]:length(datos$CT)],-datos$pres[ini[length(ini)]:length(datos$CT)],'l',xlab='CT(deg C)',ylab='Press(db)') #ultima boya

#3. Ver distribuciónde boyas ARGO 
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
par(mar = c(4.1,4.1,1.1,1.1))
plot(c(min(datos$lon)+.5,max(datos$lon)-.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
points(datos$lon[ini],datos$lat[ini],pch=16,col="light gray"); 
draw.map(); grid(col=1)

ibuenor<-c()
ibuenob<-c()

for(i in 1:length(ti)){
  ife1 <- which(tcar==as.Date(ti[i]))
  
  if(length(ife1)!=0){
    
    for (k in 1:length(ife1)){
      
      inp1 <- inpolygon(lon1[i],lat1[i],clon[,ife1[k]]-360,clat[,ife1[k]])
      
      if(sum(inp1)!=0){
        
        lines(clon[,ife1[k]] - 360, clat[,ife1[k]], col = 'blue4') #Primer contorno
        
        #Para revisar cuantas veces caen boyas dentro de la familia del remolino
        lines(conlon[,which(track==idcar[ife1[k]])]-360,conlat[,which(track==idcar[ife1[k]])],col = 'blue4')
        ife   <- which(as.Date(ti)>=min(t1[which(track==idcar[ife1[k]])]) & as.Date(ti)<=max(t1[which(track==idcar[ife1[k]])]))
        inp2  <- inpolygon(lon1[ife],lat1[ife],clon[,ife1[k]]-360,clat[,ife1[k]])
        points(lon1[ife[inp2]],lat1[ife[inp2]],pch=16,col='red3') #boyas dentro
        ibuenob <- c(ibuenob,ife[inp2])
        #Si en el mismo remolino hay mas de una observacion de las boyas
        if (length(ife[inp2])==1){
          
          ibuenor <- c(ibuenor,ife1[k])
          
        }else {ibuenor<-c(ibuenor,rep(ife1[k],length(ife[inp2])))}       
      }
    }
  }
}


#Ver CARACTERÍSTICAS del remolino

ibu <- unique(ibuenor)  # Arroja 4 remolinos 
#OJO - ibuenor arroja remolinos repetidos, porque las repeticiones
#indica cuantas boyas obtuvieron información de ese remolino 

#SOLO se pude probar con 1,2,3,4 - 
#PORQUE en esta base de datos 4 remolinos 
#hacen match en tiempo y espacio con las boyas 

#Nos INTERESA el remolino 2 = 3866
which(ibuenor == unique(ibuenor)[2]) #Ver cuántas boyas detectaron el remolino

rem=ibu[2]
t1[which(track==idcar[rem])]     #Fechas
dur[which(track==idcar[rem])]    #Días
vel[which(track==idcar[rem])]    #m/s vel de giro
radio[which(track==idcar[rem])]  #m
altura[which(track==idcar[rem])] #cm
area[which(track==idcar[rem])]   #m2

###Obtener ESTADÍSTICOS 

#ALTURA
amax <- max(altura[which(track==idcar[rem])])
amin <- min(altura[which(track==idcar[rem])])
amean <- mean(altura[which(track==idcar[rem])])


##### 2...*3866* - irem = 226,100 - icar = 2,779,761 - dur = 34 días

for (i in icar[3866]:(icar[3866]+33)){
  
  lines(conlon[,i]-360, conlat[,i], col = 'purple')
}

#-----------------------------------------------------------------------------
#Datos HIDROGRÁFICOS de los remolinos
#-----------------------------------------------------------------------------


#OJO: Existen DIFERENTES boyas para cada REMOLINO 
#POR LO TANTO: verificar los DATOS de las 19 boyas encontradas 

#NOTA: de esta base datos, nos intera el remolino 3866
#Su ibuenob son: [3:15]
iboy<-which(ibuenor==unique(ibuenor)[2]) 

boy=ibuenob[3]

boy <- ibuenob[4]
datos$CT[ini[boy]:(ini[boy+1]-1)]
datos$SA[ini[boy]:(ini[boy+1]-1)]

#2.GRAFICAR el perfil de profundidad del remolino ANTICICLÓNICO de interés


#SALINIDAD
x11()
Conf2mas1 = matrix(c(1,1,2,2,1,1,2,2,
                     3,3,4,4,3,3,4,4,
                     5,5,6,6,5,5,6,6,
                     0,7,7,0,0,7,7,0),
                   nrow=4, byrow=F) # Creamos un matriz a partir de un vector con los valores c(1:3,3) que es igual que c(1,2,3,3)
Conf2mas1
layout(Conf2mas1)

for (i in 3:15){
  plot(datos$SA[ini[ibuenob[i]]:(ini[ibuenob[i]+1]-1)],
       -datos$pres[ini[ibuenob[i]]:(ini[ibuenob[i]+1]-1)],
       'l',xlab='SA(g/kg)',
       ylab='Press(db)',
       main = paste0(datos$time[ini[ibuenob[i]]])) 
}




#--------------------------------------------------------------------------------------
#VER REMOLINOS
#--------------------------------------------------------------------------------------

#HAY 4 REMOLINOS (ibuenor arroja 19)
#3929, 3866, 3309, 3342

draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
par(mar = c(4.1,4.1,1.1,1.1))
plot(c(min(datos$lon)+.5,max(datos$lon)-.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
draw.map()
grid(col=1)

##### 1...*3929* - irem = 228,580 - icar = 32,442,297 - dur = 14 días

for (i in icar[3929]:(icar[3929]+13)){
  
  lines(conlon[,i]-360, conlat[,i], col = 'orange3')
}

##### 2...*3866* - irem = 226,100 - icar = 2,779,761 - dur = 34 días

for (i in icar[3866]:(icar[3866]+33)){
  
  lines(conlon[,i]-360, conlat[,i], col = 'purple')
}

##### 3...*3309* - irem = 204,033 - icar = 27,286,172 - dur = 16 días

for (i in icar[3309]:(icar[3309]+15)){
  
  lines(conlon[,i]-360, conlat[,i], col = 'green4')
}

##### 4...*3342* - irem = 205,129 - icar = 27,641,482 - dur = 30 días

for (i in icar[3342]:(icar[3342]+29)){
  
  lines(conlon[,i]-360, conlat[,i], col = 'purple3')
}







