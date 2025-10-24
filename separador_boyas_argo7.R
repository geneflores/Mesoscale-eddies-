# rm(list=ls()) #limpia el espacio de memoria 
# graphics.off() # cierra todas las graficas

# ocupa correr primero read_eddy_atlas.R
load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")

# num de boyas argo en zona de estudio
ini<-c(1,(which(diff(datos$pres)<0)+1)) #num de boyas
lat1<-datos$lat[ini] #lat de cada boya
lon1<-datos$lon[ini] #lon de cada boya
ti<-datos$time[ini] #fecha de medición de cada boya
# grafica de perfil a profundiad por boya
plot(datos$CT[ini[1]:(ini[2]-1)],-datos$pres[ini[1]:(ini[2]-1)],'l',xlab='CT(deg C)',ylab='Press(db)') # primer boya
plot(datos$CT[ini[length(ini)]:length(datos$CT)],-datos$pres[ini[length(ini)]:length(datos$CT)],'l',xlab='CT(deg C)',ylab='Press(db)') #ultima boya

# acomodo las boyas por fechas
aux<-sort(as.numeric(ti),index.return=T)
ti<-ti[aux$ix]; lat1<-lat1[aux$ix]; lon1<-lon1[aux$ix]
ini1<-ini[aux$ix]

#distribución de boyas argo
draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
par(mar = c(4.1,4.1,1.1,1.1))
plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
points(datos$lon[ini],datos$lat[ini],pch=16,col="light gray")
draw.map(); grid(col=1)

ibuenor<-c(); ibuenob<-c()
i1 <- which(tcar==as.Date(ti[1])) # comienza a partir de la primera fecha de boya (2001-11-08)
for(i in 1613:length(icar)){
  #print((i/length(icar))*100)
  imab<-match(t1[which(track==idcar[i])],as.Date(ti)); imab <- imab[!is.na(imab)] #boyas
  inp <- inpolygon(lon1[imab],lat1[imab],conlon[,which(track==idcar[i])]-360,conlat[,which(track==idcar[i])])
  if(sum(inp)!=0){
    # match en tiempo con remolinos
    ife<-match(as.Date(ti[imab[inp]]),t1[which(track==idcar[i])]); ife <- ife[!is.na(ife)]
    # match en tiempo con boyas
    ife2<-match(t1[which(track==idcar[i])[ife]],as.Date(ti[imab[inp]])); ife2 <- ife2[!is.na(ife2)]
    # match tiempo y espacio
    #inp2 <- inpolygon(lon1[imab[inp]][ife2],lat1[imab[inp]][ife2],conlon[,which(track==idcar[i])[ife]]-360,conlat[,which(track==idcar[i])[ife]])
    for (k in 1:length(which(track==idcar[i])[ife])){
      inp2 <- inpolygon(lon1[imab[inp]][ife2],lat1[imab[inp]][ife2],conlon[,which(track==idcar[i])[ife][k]]-360,conlat[,which(track==idcar[i])[ife][k]])
      if(sum(inp2)!=0){ 
        lines(conlon[,which(track==idcar[i])[ife][k]]-360,conlat[,which(track==idcar[i])[ife][k]], col = 'indianred') #primer contorno
        points(lon1[imab[inp]][ife2][inp2],lat1[imab[inp]][ife2][inp2],pch=16,col='green') #boyas dentro
        ibuenor<-c(ibuenor,which(track==idcar[i])[ife][k]) #id remolino
        ibuenob<-c(ibuenob,imab[inp][ife2][inp2]) #boya coincidente
        if (length(imab[inp][ife2][inp2])!=1){
          ibuenor<-c(ibuenor,rep(which(track==idcar[i])[ife][k],length(imab[inp][ife2][inp2])-1))
          }
         #readline(prompt="Press [enter] to continue")
      }
      #readline(prompt="Press [enter] to continue")
    }
  }
}

#remolinos con observaciones de boyas
remboy<-unique(track[ibuenor]) #num de rem con boyas
irb <- 143 #indice del remolino con boya <---- cambia este para que veas cada remolino
rem <- which(track==remboy[irb]) 

#183, 179, 174, 172, 164, 161, 159, 148 - 'pink4'

draw.map <- function(){maps::map("world", add=TRUE, fill=TRUE, col='gray')}
par(mar = c(4.1,4.1,1.1,1.1))
plot(c(min(datos$lon)+.5,max(datos$lon)+.5),c(min(datos$lat)-.5,max(datos$lat)+.5),"n",ann=F)
draw.map(); grid(col=1)
lines(conlon[,rem]-360,conlat[,rem], col = 'skyblue') #primer contorno

text(x = -82.5, y = 17, #Coordenadas 
     label = "Ciclónico
         irb: 172
        remboy: 830,537
        Duración 67 días
        Inicio: 25-Jun-2020
         Fin:   31-Ago-2020 ",
     col = "darkolivegreen",
     cex = 2)

##Centro de cada polígono
  points(lon[rem]-360, lat[rem], cex=1, col='black')



c(t1[rem[1]],t1[rem[length(rem)]]) #fechas
# sum(diff(t1[rem])) #duración en dias
# vel[rem] #m/s vel de giro
# radio[rem] #m
# altura[rem] #cm
# area[rem] #m2

#perfil hidrográfico de las boyas que caen dentro de los remolinos
iboy=which(track[ibuenor]==remboy[irb])
buoy<-unique(ibuenob[iboy])
nboy<-length(buoy) #número de boyas coincidentes durante la trayectoria del rem
c(min(ti[buoy]),max(ti[buoy])) #fechas boyas coincidentes
#ti[buoy] #fechas boyas coincidentes
points(lon1[buoy],lat1[buoy],pch=16, cex = 2,col='red3')

ib=1 #indice de la boya coincidente

datos$CT[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)]
datos$SA[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)]

# perfil a profundiad de la boya que cayó en un remolino
plot(datos$CT[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],
     -datos$pres[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],'l', 
     xlab='CT(deg C)',ylab='Press(db)')
title(main = paste0(datos$time[ini1[buoy[ib]]]), 
      line = 0.5)


plot(datos$SA[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],
     -datos$pres[ini1[buoy[ib]]:(ini[which(ini==ini1[buoy[ib]])+1]-1)],'l', 
     xlab='SA(g/kg)',ylab='Press(db)')



