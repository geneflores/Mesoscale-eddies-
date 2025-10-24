##########################################
# GRÁFICOS FILLED.CONTOUR4-personalizado #
##########################################



pacman::p_load(ncdf4, R.utils, maps, pracma, ggplot2, 
               RColorBrewer, paletteer, animation, tidyverse,
               plotly, oce, ggExtra, corrplot, npphen, xts,
               scales, ggpubr, akima, geosphere, gridExtra,
               png, patchwork, grid, interp, sf)


####### CON BOYAS #######
{
setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")

nombre <- c("alldatos146")


load(paste0(nombre, ".RData"))
load("prof.RData")
source("filled.contour4.R")
source("filled.legend.R")



fechasModelo <- c("Modelo 15-Ene-2020", 
                  "Modelo 08-Feb-2020")

fechasBoyas <- c("Boya 12-Ene-2020", 
                 "Boya 02-Feb-2020")


xpp <- 725


#  Convertir eje X a distancias en km - iniciando con 0 
{  x_valores <- valores$x_valores
  y_valores <- valores$y_valores
  
  
  dis<-0
  for (k in 1:(length(x_valores)-1)){
    aux<-distm(c(x_valores[k],y_valores[k]),
               c(x_valores[k+1],y_valores[k+1]),
               fun = distHaversine)
    dis[k+1]<-dis[k]+aux
  }
  
  
  x_vals <- dis/1e3
  
  x_median <-  median(x_vals) #Para posicionar fecha
  
}

# Generar líneas longitudinales con "coor"
{
  #NOTA: la posición de las líneas cambia con respecto al centro del 
  #       transecto, por lo tanto, AJUSTAR cada vez que se grafique 
  #       una FECHA DIFERENTE
  
  coor <-  valores$coordenadas
  punto_inicio <- c(x_valores[length(x_valores)], y_valores[length(y_valores)])
  
  x <- c(1:6)
  coordenadas <- c()
  
  
  for(i in 1:6){
    
    xi <- x[i]
    
    punto1 <- c(coor$longitud[xi], coor$latitud[xi])
    dis_puntos <- distHaversine(punto_inicio, punto1)
    
    dis <- dis_puntos/1000
    
    coordenadas <- c(coordenadas, dis)
  }
  
  #print(coordenadas)
  rm( punto1, dis_puntos, x, xi, i, dis)
}

# Convertir la profundidad (metros) a presión (decibares)
{
  
  # alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_febrero20.nc"))
  # 
  # nc <- nc_open(alist) #print(nc) 
  # 
  # Z <- ncvar_get(nc,'depth')  #34 niveles 
  # lat <- ncvar_get(nc,'latitude')
  # 
  # #1.1  CONVERTIR altura a profundidad y promediar latitudes
  # z <- Z * -1
  # la <- mean(lat)
  # {
  #   #Haciendo pruebas la presión varió 0.0003 en la superfucie 
  #   #entre el valor mínimo y máximo de latitud y varió 0.72 decibares de 
  #   #en lo más profunddo de la base de datos entre el min y máx
  #   # así que no hay tanta diferencia en tomar el promedio de las latitudes 
  # }
  # 
  # #1.2 Convertir profundidad a presión 
  # p <- gsw_p_from_z(z = z, latitude = la)
  # 
  # prof <- seq(min(p),max(p),length=length(p))-max(p)
  # #prof2 <- seq(min(p),max(p),length=length(p)) #revisar estas diferencias
  # 
  # rm(Z, lat, nc, alist, z, la)
  
}

r1_coord_a <- coordenadas[1]
r1_coord_b <- coordenadas[2]

r2_coord_a <- coordenadas[3]
r2_coord_b <- coordenadas[4]



limiteInferiorX <- ceiling(min(x_vals) / 100) * 100
limiteSuperiorX <- floor(max(x_vals) / 100) * 100
secuenciaX <- seq(limiteInferiorX, 
                  limiteSuperiorX, 
                  by = (limiteSuperiorX - limiteInferiorX) / 4)



Var <- ADP
x_vals <- x_vals
prof <- prof
titulo <- c("Diferencia de ADP")
unidades <- c("ADP [Kg/m³]")
nombreArchivo <- paste0("boyfinal2/", nombre, "_ADP.png")


palGENE <- colorRampPalette(
  c("#611300", "#C33A00", "#E49183", "#F7F5F4", "#A3C8DF", "#007FB6","#003560")
  )


palD <- colorRampPalette((c("#D33F6A","#F0B0BD", "white", "#904E9F", "#5B3794")))


diferenciasMinimas <- c(max(Var[[3]][[1]], na.rm = TRUE),
                        min(Var[[3]][[1]], na.rm = TRUE),
                        max(Var[[3]][[2]], na.rm = TRUE),
                        min(Var[[3]][[2]], na.rm = TRUE)
)


valoresMinimos <- c(max(Var[[2]][[1]], na.rm = TRUE),
                        min(Var[[2]][[1]], na.rm = TRUE),
                        max(Var[[2]][[2]], na.rm = TRUE),
                        min(Var[[2]][[2]], na.rm = TRUE)
)


boyasMinimos <- c(max(Var[[1]][[1]], na.rm = TRUE),
                    min(Var[[1]][[1]], na.rm = TRUE),
                    max(Var[[1]][[2]], na.rm = TRUE),
                    min(Var[[1]][[2]], na.rm = TRUE)
)


cminADP <- floor(min(diferenciasMinimas))  
cmaxADP <- ceiling(max(diferenciasMinimas))


if (cminADP > 0) {
  cminADP <- 0
}

if (cminADP < 0 && abs(cminADP) > cmaxADP) {
  cminADP <- -abs(cminADP)
  cmaxADP <- abs(cminADP)
} else {
  cminADP <- -abs(cmaxADP)
  cmaxADP <- abs(cmaxADP)
}



cminD <- floor(min(valoresMinimos)) 
cmaxD <- ceiling(max(valoresMinimos))


seqADP <- seq(cminADP, cmaxADP)
seqD <- seq(cminD, cmaxD)

rm(cminD, cmaxD, cminADP, cmaxADP)


{
# Configuración de tamaño y resolución
  png(file = nombreArchivo, 
      width = 35, height = 20, units = "in", res = 400)
  
  # Ajuste general de márgenes y configuración de layout
  # layout(matrix(1:5, nrow = 1, byrow = TRUE), widths = c(3, 0.4, 3, 0.4, 3))
  par(mar = c(4, 4, 2, 2), 
      oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos
  
  ######### GRÁFICO 1 - Primer `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.05, 0.32, 0.60, 0.9), 
      las = 1, 
      cex = 2.3)
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[2]][[1]], 2),
                  color.palette = palD,
                  plot.title = title(main = fechasModelo[1], 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX); 
                    axis(2)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r1_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r1_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(a1)"))
                  })
  


  mtext('Profundidad [dbar]',
        side = 2,
        cex = 2.3,
        line = 3,
        las = 3)

  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.33, 0.35, 0.60, 0.9), 
      las = 1,
      cex = 2.3)
  
  filled.legend(x = 1, y = seqD, z = matrix(seqD, 1, length(seqD)), 
                color = palD,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 2.3, las = 3)
  
  ######### GRÁFICO 2 - Segundo `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.42, 0.69, 0.60, 0.9), 
      las = 1)
  
  
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[3]][[1]],2),
                  color.palette = palGENE,
                  plot.title = title(main = titulo, 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2, labels = FALSE)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r1_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r1_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(b1)"))
                  })
  
  
  
  ################## LEYENDA para el gráfico 2
  
  par(new = "TRUE", 
      plt = c(0.70, 0.72, 0.60, 0.9), 
      las = 1, 
      cex = 2.3)
  
  
  filled.legend(x = 1, y = seqADP, z = matrix(seqADP, 1, length(seqADP)), 
                color = palGENE,
                cex = 0.8)
  
  mtext(unidades, side = 4, line = 2, cex = 2.3, las = 3)
  
  # Gráfico 3 - `plot` normal
  
  par(new = "TRUE", 
      plt = c(0.82, 0.96, 0.60, 0.9), 
      las = 1)
  
  plot(x = Var[[1]][[1]], y = pres_boyas[[1]],
       type = "l", main = fechasBoyas[1], 
       xlab = unidades,
       ylab = "",
       ylim = c(min(prof), 0), yaxs = "i", 
       xlim = c(floor(min(boyasMinimos)), 
                ceiling(max(boyasMinimos))),
       yaxt = "n")
  
  axis(2, labels = FALSE)
  
  text(x = 27, y = -100, 
       font = 2, 
       paste("(c1)"))
  
  # Agregar la cuadrícula
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  
  
  
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  
  
  
  par(new = "TRUE", 
      plt = c(0.05, 0.32, 0.1, 0.40), 
      las = 1, 
      cex = 2.3)
 
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[2]][[2]],2),
                  color.palette = palD,
                  plot.title = title(main = fechasModelo[2], 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r2_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r2_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(a2)"))
                  })
  
  
  
  mtext('Profundidad [dbar]',
        side = 2,
        cex = 2.3,
        line = 3,
        las = 3)
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.33, 0.35, 0.1, 0.40), 
      las = 1,
      cex = 2.3)
  
  filled.legend(x = 1, y = seqD, z = matrix(seqD, 1, length(seqD)), 
                color = palD,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 2.3, las = 3)
  
  ######### GRÁFICO 2 - Segundo `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.42, 0.69, 0.1, 0.40), 
      las = 1)
  
 
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[3]][[2]],2),
                  color.palette = palGENE,
                  plot.title = title(main = titulo, 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2, labels = FALSE)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r2_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r2_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(b2)"))
                  })
  
  
  
  ################## LEYENDA para el gráfico 2
  
  par(new = "TRUE", 
      plt = c(0.70, 0.72, 0.1, 0.40), 
      las = 1, 
      cex = 2.3)
  
  
  filled.legend(x = 1, y = seqADP, z = matrix(seqADP, 1, length(seqADP)), 
                color = palGENE,
                cex = 0.8)
  
  mtext(unidades, side = 4, line = 2, cex = 2.3, las = 3)
  
  # Gráfico 3 - `plot` normal
  
  par(new = "TRUE", 
      plt = c(0.82, 0.96, 0.1, 0.40), 
      las = 1)
  
  plot(x = Var[[1]][[2]], y = pres_boyas[[2]],
       type = "l", main = fechasBoyas[2], 
       xlab = unidades,
       ylab = "",
       ylim = c(min(prof), 0), yaxs = "i", 
       xlim = c(floor(min(boyasMinimos)), 
                ceiling(max(boyasMinimos))),
       yaxt = "n")
  
  axis(2, labels = FALSE)
  
  text(x = 27, y = -100, 
       font = 2, 
       paste("(c2)"))
  
  # Agregar la cuadrícula
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  
  
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  
  
  
  
  dev.off()
  
}

#########################################
#########################################
#########################################
#########################################







Var <- sal
x_vals <- x_vals
prof <- prof
titulo <- c("Diferencia de SA")
unidades <- c("SA [g/Kg]")
nombreArchivo <- paste0("boyfinal2/", nombre, "_SA.png")


palGENE <- colorRampPalette(
  c("#611300", "#C33A00", "#E49183", "#F7F5F4", "#A3C8DF", "#007FB6","#003560")
)

palD <- colorRampPalette(
  c("cyan", "#841859", "darkolivegreen2", "#005600")
)




diferenciasMinimas <- c(max(Var[[3]][[1]], na.rm = TRUE),
                        min(Var[[3]][[1]], na.rm = TRUE),
                        max(Var[[3]][[2]], na.rm = TRUE),
                        min(Var[[3]][[2]], na.rm = TRUE)
)


valoresMinimos <- c(max(Var[[2]][[1]], na.rm = TRUE),
                    min(Var[[2]][[1]], na.rm = TRUE),
                    max(Var[[2]][[2]], na.rm = TRUE),
                    min(Var[[2]][[2]], na.rm = TRUE)
)

boyasMinimos <- c(max(Var[[1]][[1]], na.rm = TRUE),
                  min(Var[[1]][[1]], na.rm = TRUE),
                  max(Var[[1]][[2]], na.rm = TRUE),
                  min(Var[[1]][[2]], na.rm = TRUE)
)

cminADP <- floor(min(diferenciasMinimas))  
cmaxADP <- ceiling(max(diferenciasMinimas))


if (cminADP > 0) {
  cminADP <- 0
}

if (cminADP < 0 && abs(cminADP) > cmaxADP) {
  cminADP <- -abs(cminADP)
  cmaxADP <- abs(cminADP)
} else {
  cminADP <- -abs(cmaxADP)
  cmaxADP <- abs(cmaxADP)
}



cminD <- floor(min(valoresMinimos)) 
cmaxD <- ceiling(max(valoresMinimos))


seqADP <- seq(cminADP, cmaxADP)
seqD <- seq(cminD, cmaxD)

rm(cminD, cmaxD, cminADP, cmaxADP)






########## REMOLINO ciclónico 135
{
  # Configuración de tamaño y resolución
  png(file = nombreArchivo, 
      width = 35, height = 20, units = "in", res = 400)
  
  # Ajuste general de márgenes y configuración de layout
  # layout(matrix(1:5, nrow = 1, byrow = TRUE), widths = c(3, 0.4, 3, 0.4, 3))
  par(mar = c(4, 4, 2, 2), 
      oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos
  
  ######### GRÁFICO 1 - Primer `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.05, 0.32, 0.60, 0.9), 
      las = 1, 
      cex = 2.3)
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[2]][[1]], 2),
                  color.palette = palD,
                  plot.title = title(main = fechasModelo[1], 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r1_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r1_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(a1)"))
                  })
  
  
  
  mtext('Profundidad [dbar]',
        side = 2,
        cex = 2.3,
        line = 3,
        las = 3)
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.33, 0.35, 0.60, 0.9), 
      las = 1,
      cex = 2.3)
  
  filled.legend(x = 1, y = seqD, z = matrix(seqD, 1, length(seqD)), 
                color = palD,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 2.3, las = 3)
  
  ######### GRÁFICO 2 - Segundo `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.42, 0.69, 0.60, 0.9), 
      las = 1)
  
  
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[3]][[1]],2),
                  color.palette = palGENE,
                  plot.title = title(main = titulo, 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX); 
                    axis(2, labels = FALSE)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r1_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r1_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(b1)"))
                  })
  
  
  
  ################## LEYENDA para el gráfico 2
  
  par(new = "TRUE", 
      plt = c(0.70, 0.72, 0.60, 0.9), 
      las = 1, 
      cex = 2.3)
  
  
  filled.legend(x = 1, y = seqADP, z = matrix(seqADP, 1, length(seqADP)), 
                color = palGENE,
                cex = 0.8)
  
  mtext(unidades, side = 4, line = 2, cex = 2.3, las = 3)
  
  # Gráfico 3 - `plot` normal
  
  par(new = "TRUE", 
      plt = c(0.82, 0.96, 0.60, 0.9), 
      las = 1)
  
  plot(x = Var[[1]][[1]], y = pres_boyas[[1]],
       type = "l", main = fechasBoyas[1], 
       xlab = unidades,
       ylab = "",
       ylim = c(min(prof), 0), yaxs = "i", 
       xlim = c(floor(min(boyasMinimos)), 
                ceiling(max(boyasMinimos))),
       yaxt = "n")
  
  axis(2, labels = FALSE)
  
  text(x = 37.5, y = -200, 
       font = 2, 
       paste("(c1)"))
  
  # Agregar la cuadrícula
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  
  
  
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  
  
  
  par(new = "TRUE", 
      plt = c(0.05, 0.32, 0.1, 0.40), 
      las = 1, 
      cex = 2.3)
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[2]][[2]],2),
                  color.palette = palD,
                  plot.title = title(main = fechasModelo[2], 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX); 
                    axis(2)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r2_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r2_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(a2)"))
                  })
  
  
  
  mtext('Profundidad [dbar]',
        side = 2,
        cex = 2.3,
        line = 3,
        las = 3)
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.33, 0.35, 0.1, 0.40), 
      las = 1,
      cex = 2.3)
  
  filled.legend(x = 1, y = seqD, z = matrix(seqD, 1, length(seqD)), 
                color = palD,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 2.3, las = 3)
  
  ######### GRÁFICO 2 - Segundo `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.42, 0.69, 0.1, 0.40), 
      las = 1)
  
  
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[3]][[2]],2),
                  color.palette = palGENE,
                  plot.title = title(main = titulo, 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2, labels = FALSE)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r2_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r2_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(b2)"))
                  })
  
  
  
  ################## LEYENDA para el gráfico 2
  
  par(new = "TRUE", 
      plt = c(0.70, 0.72, 0.1, 0.40), 
      las = 1, 
      cex = 2.3)
  
  
  filled.legend(x = 1, y = seqADP, z = matrix(seqADP, 1, length(seqADP)), 
                color = palGENE,
                cex = 0.8)
  
  mtext(unidades, side = 4, line = 2, cex = 2.3, las = 3)
  
  # Gráfico 3 - `plot` normal
  
  par(new = "TRUE", 
      plt = c(0.82, 0.96, 0.1, 0.40), 
      las = 1)
  
  plot(x = Var[[1]][[2]], y = pres_boyas[[2]],
       type = "l", main = fechasBoyas[2], 
       xlab = unidades,
       ylab = "",
       ylim = c(min(prof), 0), yaxs = "i", 
       xlim = c(floor(min(boyasMinimos)), 
                ceiling(max(boyasMinimos))),
       yaxt = "n")
  
  axis(2, labels = FALSE)
  
  text(x = 37.5, y = -200, 
       font = 2, 
       paste("(c2)"))
  
  # Agregar la cuadrícula
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  
  
  dev.off()
  
}




#########################################
#########################################
#########################################
#########################################







Var <- temp
x_vals <- x_vals
prof <- prof
titulo <- c("Diferencia de TP")
unidades <- c("TP [°C]")
nombreArchivo <- paste0("boyfinal2/", nombre, "_TP.png")


palGENE <- colorRampPalette(
  c("#611300", "#C33A00", "#E49183", "#F7F5F4", "#A3C8DF", "#007FB6","#003560")
)

palD <- colorRampPalette(
  c("#3E0689", "#AE1987", "#EA8D2D","#E3E400")
)




diferenciasMinimas <- c(max(Var[[3]][[1]], na.rm = TRUE),
                        min(Var[[3]][[1]], na.rm = TRUE),
                        max(Var[[3]][[2]], na.rm = TRUE),
                        min(Var[[3]][[2]], na.rm = TRUE)
)


valoresMinimos <- c(max(Var[[2]][[1]], na.rm = TRUE),
                    min(Var[[2]][[1]], na.rm = TRUE),
                    max(Var[[2]][[2]], na.rm = TRUE),
                    min(Var[[2]][[2]], na.rm = TRUE)
)

boyasMinimos <- c(max(Var[[1]][[1]], na.rm = TRUE),
                  min(Var[[1]][[1]], na.rm = TRUE),
                  max(Var[[1]][[2]], na.rm = TRUE),
                  min(Var[[1]][[2]], na.rm = TRUE)
)

cminADP <- floor(min(diferenciasMinimas))  
cmaxADP <- ceiling(max(diferenciasMinimas))


if (cminADP > 0) {
  cminADP <- 0
}

if (cminADP < 0 && abs(cminADP) > cmaxADP) {
  cminADP <- -abs(cminADP)
  cmaxADP <- abs(cminADP)
} else {
  cminADP <- -abs(cmaxADP)
  cmaxADP <- abs(cmaxADP)
}



cminD <- floor(min(valoresMinimos)) 
cmaxD <- ceiling(max(valoresMinimos))


seqADP <- seq(cminADP, cmaxADP)
seqD <- seq(cminD, cmaxD)

rm(cminD, cmaxD, cminADP, cmaxADP)






########## REMOLINO ciclónico 135
{
  # Configuración de tamaño y resolución
  png(file = nombreArchivo, 
      width = 35, height = 20, units = "in", res = 400)
  
  # Ajuste general de márgenes y configuración de layout
  # layout(matrix(1:5, nrow = 1, byrow = TRUE), widths = c(3, 0.4, 3, 0.4, 3))
  par(mar = c(4, 4, 2, 2), 
      oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos
  
  ######### GRÁFICO 1 - Primer `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.05, 0.32, 0.60, 0.9), 
      las = 1, 
      cex = 2.3)
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[2]][[1]], 2),
                  color.palette = palD,
                  plot.title = title(main = fechasModelo[1], 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2)
                    xaxs = "i"
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r1_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r1_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(a1)"))
                  })
  
  
  
  mtext('Profundidad [dbar]',
        side = 2,
        cex = 2.3,
        line = 3,
        las = 3)
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.33, 0.35, 0.60, 0.9), 
      las = 1,
      cex = 2.3)
  
  filled.legend(x = 1, y = seqD, z = matrix(seqD, 1, length(seqD)), 
                color = palD,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 2.3, las = 3)
  
  ######### GRÁFICO 2 - Segundo `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.42, 0.69, 0.60, 0.9), 
      las = 1)
  
  
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[3]][[1]],2),
                  color.palette = palGENE,
                  plot.title = title(main = titulo, 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2, labels = FALSE)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r1_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r1_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(b1)"))
                  })
  
  
  
  ################## LEYENDA para el gráfico 2
  
  par(new = "TRUE", 
      plt = c(0.70, 0.72, 0.60, 0.9), 
      las = 1, 
      cex = 2.3)
  
  
  filled.legend(x = 1, y = seqADP, z = matrix(seqADP, 1, length(seqADP)), 
                color = palGENE,
                cex = 0.8)
  
  mtext(unidades, side = 4, line = 2, cex = 2.3, las = 3)
  
  # Gráfico 3 - `plot` normal
  
  par(new = "TRUE", 
      plt = c(0.82, 0.96, 0.60, 0.9), 
      las = 1)
  
  plot(x = Var[[1]][[1]], y = pres_boyas[[1]],
       type = "l", main = fechasBoyas[1], 
       xlab = unidades,
       ylab = "",
       ylim = c(min(prof), 0), yaxs = "i", 
       xlim = c(floor(min(boyasMinimos)), 
                ceiling(max(boyasMinimos))),
       yaxt = "n")
  
  axis(2, labels = FALSE)
  
  text(x = 27, y = -200, 
       font = 2, 
       paste("(c1)"))
  
  # Agregar la cuadrícula
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  
  
  
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  ########################################################################
  
  
  
  par(new = "TRUE", 
      plt = c(0.05, 0.32, 0.1, 0.40), 
      las = 1, 
      cex = 2.3)
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[2]][[2]],2),
                  color.palette = palD,
                  plot.title = title(main = fechasModelo[2], 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r2_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r2_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(a2)"))
                  })
  
  
  
  mtext('Profundidad [dbar]',
        side = 2,
        cex = 2.3,
        line = 3,
        las = 3)
  
  ################## LEYENDA para el gráfico 1
  
  par(new = "TRUE", 
      plt = c(0.33, 0.35, 0.1, 0.40), 
      las = 1,
      cex = 2.3)
  
  filled.legend(x = 1, y = seqD, z = matrix(seqD, 1, length(seqD)), 
                color = palD,
                cex = 0.8)
  mtext(unidades, side = 4, line = 2.5, cex = 2.3, las = 3)
  
  ######### GRÁFICO 2 - Segundo `filled.contour`
  
  par(new = "TRUE", 
      plt = c(0.42, 0.69, 0.1, 0.40), 
      las = 1)
  
  
  
  
  filled.contour4(x = x_vals, y = prof, rot90(Var[[3]][[2]],2),
                  color.palette = palGENE,
                  plot.title = title(main = titulo, 
                                     xlab = "Distancia [Km]",
                                     cex.main = 1.2),
                  plot.axes = {
                    axis(1, at = secuenciaX) 
                    axis(2, labels = FALSE)
                    grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
                    abline(v = r2_coord_a, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    abline(v = r2_coord_b, 
                           col = 'black', 
                           lwd = 3, 
                           lty = 1)
                    text(x = xpp, y = -100, 
                         font = 2, 
                         paste("(b2)"))
                  })
  
  
  
  ################## LEYENDA para el gráfico 2
  
  par(new = "TRUE", 
      plt = c(0.70, 0.72, 0.1, 0.40), 
      las = 1, 
      cex = 2.3)
  
  
  filled.legend(x = 1, y = seqADP, z = matrix(seqADP, 1, length(seqADP)), 
                color = palGENE,
                cex = 0.8)
  
  mtext(unidades, side = 4, line = 2, cex = 2.3, las = 3)
  
  # Gráfico 3 - `plot` normal
  
  par(new = "TRUE", 
      plt = c(0.82, 0.96, 0.1, 0.40), 
      las = 1)
  
  plot(x = Var[[1]][[2]], y = pres_boyas[[2]],
       type = "l", main = fechasBoyas[2], 
       xlab = unidades,
       ylab = "",
       ylim = c(min(prof), 0), yaxs = "i", 
       xlim = c(floor(min(boyasMinimos)), 
                ceiling(max(boyasMinimos))),
       yaxt = "n")
  
  axis(2, labels = FALSE)
  
  text(x = 27, y = -200, 
       font = 2, 
       paste("(c2)"))
  
  # Agregar la cuadrícula
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  
  
  
  
  dev.off()
  
}


rm(list = ls())

}
