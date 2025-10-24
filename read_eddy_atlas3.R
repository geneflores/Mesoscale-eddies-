rm(list=ls()) #limpia el espacio de memoria 
graphics.off() # cierra todas las graficas

#---------------------------------------------------------------------------------------------------
# aviso_netcdf: A script that reads netcdf files and
#% subsets the data to the area of interest, for process and plotting.
#% The matrix results is saved as structures.
#%--------------------------------------------------------------------------------------------------

#ptm <- proc.time()
library("ncdf4")
library("R.utils")
library("maps")
library("pracma")

#Visualizar y leer el contenido de un archivo NetCDF
setwd("")
alist<-dir(pattern=glob2rx("*.nc"))

nc<- nc_open(alist)
#print(nc) # displays nc file info
lon <- ncvar_get(nc,'longitude')    
lat <- ncvar_get(nc,'latitude')
area <- ncvar_get(nc,'effective_area') #m^2
altura <- ncvar_get(nc,'amplitude') #m
radio<- ncvar_get(nc,'effective_radius') #m
conlat <- ncvar_get(nc,'effective_contour_latitude')
conlon <- ncvar_get(nc,'effective_contour_longitude')
ncon <- ncvar_get(nc,'num_contours') #number of contours selected eddy
dur <- ncvar_get(nc,'observation_number') #lo que perduro un remolino
vel <- ncvar_get(nc,'speed_average') # m/s Average speed of the contour defining the radius scale “speed_radius”
track <- ncvar_get(nc,'track') #Trajectory identification number
time <- ncvar_get(nc,'time') #days since 1950-01-01 00:00:00 UTC
t1<-as.Date(time, origin = "1950-01-01 00:00:00",tz='UTC')

#Convertir longitudes y recortar al cuadrante (zona de estudio)
minlat <-8;    maxlat <-33
minlong<- -100;    maxlong<- -60 #GOM
#minlong<- -130;    maxlong<- -75 #Eastern Pacific
LATLIMS=c(minlat, maxlat); LONLIMS<-c(minlong, maxlong)
lon1<-lon; lon1[lon>180]<-lon1[lon>180]-360

irem<-c(1,(which(diff(dur)<0)+1)) #indice de cada remolino
#polígono caribeño
lonp <- -1*c(87.8747, 89.5015, 86.2106, 82.6176, 80.7975, 79.3380, 78.6719, 77.2931,
             62.2013, 61.3251, 61.3654, 61.2409, 62.3344, 63.09, 65.6213, 67.0605,
             68.5166, 71.0929, 73.2380, 80.8204, 82.2571, 84.3120, 87.1101, 87.8747)
latp <- c( 21.2433, 15.9497, 13.8241, 8.8217, 8.4549, 9.3329, 9.2758, 
           7.4149, 9.0671, 10.3527, 12.5059, 15.2304, 17.2452, 18.0037, 18.2441,
           18.3233, 18.6439, 19.5587, 19.8845, 22.8951, 22.9208, 22.2705, 
           21.2580, 21.2433)
# remolinos que caen en el caribe
inp <- inpolygon(lon[irem]-360,lat[irem],lonp,latp)
icar<-irem[inp]

# ilon<-which(lon1>=minlong & lon1<=maxlong)
# ilat<-which(lat>=minlat & lat<=maxlat)
# ilat1<-which(lat[ilon] >= minlat & lat[ilon]<=maxlat)
# ilon1<-which(lon1[ilat] >= minlong & lon1[ilat]<=maxlong)
# ilalo<-ilat[ilon1] # región GoM, Caribe y Atlántico

par(mar = c(4.1,4.1,1.1,1.1))
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col=8)}
plot(c(minlong,maxlong),c(minlat,maxlat),"n",xlab='Lon',ylab='Lat')#,ann=F,axes=F)
points(lon[icar]-360, lat[icar], pch=16, col='green')
draw.map(); grid(col=1)
#lines(lonp,latp,col='green')

tcar<-t1[icar] # fechas remolinos caribeños
dcar<-dur[icar] #duración
rcar<-radio[icar] #radio
hcar<-altura[icar] # altura
acar<-area[icar] #area
vcar<-vel[icar] #velocidad de giro
ncar<-ncon[icar] #numero de contornos por remolino
clon<-conlon[,icar]
clat<-conlat[,icar]
idcar<-track[icar] #identificador de remolino
