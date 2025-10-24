###################################################
###
###   PERFILES DE BOYAS CON GRADIENTE
###
##################################################


#DATOS AGRUPADOS -- MEJOR EXTRAER TODO 
{
  
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
  
  ### CICLÓNICOS 
  load("alldatos145.RData")  #remolino ciclónico 145
  
  load("alldatos135.RData")
  
  load("alldatos164.RData")
  
  ### ANTICICLÓNICOS
  load("alldatos151.RData")
  
  load("alldatos146.RData")
 
  load("alldatos131.RData")
  
}


###EXTRAER TODO 
{
  setwd("C:/Users/genef/Documents/Remolinos/rutinas/NUEVOS")
     #  145
 load("alldatos145.RData")
 tb_145    <- temp$temp_boyas
 sb_145    <- sal$sal_boyas
 adp_145   <- ADP$APD_boyas
 press_145 <- pres_boyas 
 rm(ADP, pres_boyas, sal, temp, valores)
 
 #  135
 load("alldatos135.RData")
 tb_135    <- temp$temp_boyas
 sb_135    <- sal$sal_boyas
 adp_135   <- ADP$APD_boyas
 press_135 <- pres_boyas 
 rm(ADP, pres_boyas, sal, temp, valores)
 
 #  164
 load("alldatos164.RData")
 tb_164    <- temp$temp_boyas
 sb_164    <- sal$sal_boyas
 adp_164   <- ADP$APD_boyas
 press_164 <- pres_boyas 
 rm(ADP, pres_boyas, sal, temp, valores)
 
 #  151
 load("alldatos151.RData")
 tb_151    <- temp$temp_boyas
 sb_151    <- sal$sal_boyas
 adp_151   <- ADP$APD_boyas
 press_151 <- pres_boyas 
 rm(ADP, pres_boyas, sal, temp, valores)
 
 #  146
 load("alldatos146.RData")
 tb_146    <- temp$temp_boyas
 sb_146    <- sal$sal_boyas
 adp_146   <- ADP$APD_boyas
 press_146 <- pres_boyas 
 rm(ADP, pres_boyas, sal, temp, valores)
 
 #  131
 load("alldatos131.RData")
 tb_131    <- temp$temp_boyas
 sb_131    <- sal$sal_boyas
 adp_131   <- ADP$APD_boyas
 press_131 <- pres_boyas 
 rm(ADP, pres_boyas, sal, temp, valores)
 
}
 
### SALINIDAD ABSOLUTA CY VS ANTICY
{
png(file = 'C:/Users/genef/Documents/Remolinos/perfiles/sal_boyas13.png', 
    width = 22, height = 18, units = "in", res = 400)


par(mar = c(4, 4, 2, 2), 
    oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos

#CICLÓNICOS
par(new = "TRUE", 
    plt = c(0.13, 0.5, 0.15, 0.95), 
    las = 1, 
    cex = 2.3)

 
  #145
  plot( x = sb_145$sal_2, y = press_145$p2, type = "l", 
        lty = 2, lwd = 3,
        yaxs = "i", 
        xlab = "SA [g/Kg]", ylab = "Profundidad [dbar]",
        xlim = c(34.75, 37.5))
  
  lines( x = sb_145$sal_1, y = press_145$p1, lty = 1,lwd = 3)
  lines( x = sb_145$sal_3, y = press_145$p3, lty = 3, lwd = 3) 
 
  #135
 lines( x = sb_135$sal_1, y = press_135$p1, lty = 1, col = "blue", lwd = 3)
  
  #164
 lines( x = sb_164$sal_2, y = press_164$p2, lty = 2,lwd = 3, col = "red3")
 lines( x = sb_164$sal_3, y = press_164$p3, lty = 3,lwd = 3 ,col = "red3")
 
 grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
 text(x = 34.9, y = -50, 
      font = 2, 
      paste("a."))
 
 colores_ordenados <- c('black', 'blue', 'red','white','black','black','black')
 legend("bottomright", legend = c("145","135","164", "Etapas",
                                  "Temprana", "Desarrollada", "Envejecida"), 
                                   col = colores_ordenados, 
                                   lty = c(1,1,1, NA, 1, 2, 3),
                                   lwd = c(3,3,3,NA,3,3,3))
 title(main = "Remolinos ciclónicos", cex.main = 1.3, line = 0.7)
 
 
######################### ANTICICLÓNICOS 
 
 par(new = "TRUE", 
     plt = c(0.58, 0.95, 0.15, 0.95), 
     las = 1)


 #151
 plot( x = sb_151$sal_1, y = press_151$p1, type = "l", lwd = 3,
       yaxs = "i", 
       xlab = "SA [g/Kg]", ylab = "", 
       xlim = c(34.75, 37.5),
       #xlim = c(floor(min(sb_131$sal_2)), max(sb_131$sal_2)),
       yaxt = "n")
 
 lines( x = sb_151$sal_2, y = press_151$p2, lty = 2, lwd = 3)
 lines( x = sb_151$sal_3, y = press_151$p3, lty = 3, lwd = 3) 
 
 #146
 lines( x = sb_146$sal_1, y = press_146$p1, lty = 1,lwd = 3, col = "blue")
 lines( x = sb_146$sal_2, y = press_146$p2, lty = 2,lwd = 3, col = "blue")
 
 #131
 lines( x = sb_131$sal_2, y = press_131$p2, lty = 2,lwd = 3, col = "red3")
 lines( x = sb_131$sal_3, y = press_131$p3, lty = 3, lwd = 3 ,col = "red3")
 grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
 
 text(x = 34.9, y = -50, 
      font = 2, 
      paste("b."))
 
 colores_ordenados <- c('black', 'blue', 'red','white','black','black','black')
 legend("bottomright", legend = c("151","146","131", "Etapas",
                                  "Temprana", "Desarrollada", "Envejecida"), 
                                   col = colores_ordenados,  
                                   lty = c(1,1,1, NA, 1, 2, 3),
                                    lwd = c(3,3,3,NA,3,3,3))
 
 title(main = "Remolinos anticiclónicos", cex.main = 1.3, line = 0.7)
 
 
 dev.off()
 
}



### TEMPERATURA POTENCIAL  CY VS ANTICY

{
  png(file = 'C:/Users/genef/Documents/Remolinos/perfiles/tem_boyas2.png', 
      width = 22, height = 18, units = "in", res = 400)
  
  
  par(mar = c(4, 4, 2, 2), 
      oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos
  
  #CICLÓNICOS
  par(new = "TRUE", 
      plt = c(0.13, 0.5, 0.15, 0.95), 
      las = 1, 
      cex = 2.3)
  
  
  #145
  plot( x = tb_145$pt_2, y = press_145$p2, type = "l", 
        lty = 2, lwd = 3,
        yaxs = "i", 
        xlab = "TP [°C]", ylab = "Profundidad [dbar]",
        xlim = c(5, 30))
  
  lines( x = tb_145$pt_1, y = press_145$p1, lty = 1,lwd = 3)
  lines( x = tb_145$pt_3, y = press_145$p3, lty = 3, lwd = 3) 
  
  #135
  lines( x = tb_135$pt_1, y = press_135$p1, lty = 1, col = "blue", lwd = 3)
  
  #164
  lines( x = tb_164$pt_2, y = press_164$p2, lty = 2,lwd = 3, col = "red3")
  lines( x = tb_164$pt_3, y = press_164$p3, lty = 3,lwd = 3 ,col = "red3")
  
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  text(x = 6, y = -50, 
       font = 2, 
       paste("a."))
  
  colores_ordenados <- c('black', 'blue', 'red','white','black','black','black')
  legend("bottomright", legend = c("145","135","164", "Etapas",
                                   "Temprana", "Desarrollada", "Envejecida"), 
         col = colores_ordenados, 
         lty = c(1,1,1, NA, 1, 2, 3),
         lwd = c(3,3,3,NA,3,3,3))
  title(main = "Remolinos ciclónicos", cex.main = 1.3, line = 0.7)
  
  
  ######################### ANTICICLÓNICOS 
  
  par(new = "TRUE", 
      plt = c(0.58, 0.95, 0.15, 0.95), 
      las = 1)
  
  
  #151
  plot( x = tb_151$pt_1, y = press_151$p1, type = "l", lwd = 3,
        yaxs = "i", 
        xlab = "TP [°C]", ylab = "", 
        xlim = c(5,30),
        #xlim = c(floor(min(sb_131$sal_2)), max(sb_131$sal_2)),
        yaxt = "n")
  
  lines( x = tb_151$pt_2, y = press_151$p2, lty = 2, lwd = 3)
  lines( x = tb_151$pt_3, y = press_151$p3, lty = 3, lwd = 3) 
  
  #146
  lines( x = tb_146$pt_1, y = press_146$p1, lty = 1,lwd = 3, col = "blue")
  lines( x = sb_146$sal_2, y = press_146$p2, lty = 2,lwd = 3, col = "blue")
  
  #131
  lines( x = tb_131$pt_2, y = press_131$p2, lty = 2,lwd = 3, col = "red3")
  lines( x = tb_131$pt_3, y = press_131$p3, lty = 3, lwd = 3 ,col = "red3")
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  text(x = 6, y = -50, 
       font = 2, 
       paste("b."))
  
  colores_ordenados <- c('black', 'blue', 'red','white','black','black','black')
  legend("bottomright", legend = c("151","146","131", "Etapas",
                                   "Temprana", "Desarrollada", "Envejecida"), 
         col = colores_ordenados,  
         lty = c(1,1,1, NA, 1, 2, 3),
         lwd = c(3,3,3,NA,3,3,3))
  
  title(main = "Remolinos anticiclónicos", cex.main = 1.3, line = 0.7)
  
  
  dev.off()
  
}


### ANOMALÍA DE DENSIDAD POTENCIAL CY VS ANTICY 

{
  png(file = 'C:/Users/genef/Documents/Remolinos/perfiles/adp_boyas.png', 
      width = 22, height = 18, units = "in", res = 400)
  
  
  par(mar = c(4, 4, 2, 2), 
      oma = c(1, 1, 1, 1)) # Márgenes ajustados y espacio entre gráficos
  
  #CICLÓNICOS
  par(new = "TRUE", 
      plt = c(0.13, 0.5, 0.15, 0.95), 
      las = 1, 
      cex = 2.3)
  
  
  #145
  plot( x = adp_145$ADP_2, y = press_145$p2, type = "l", 
        lty = 2, lwd = 3,
        yaxs = "i", 
        xlab = "ADP [Kg/m³]", ylab = "Profundidad [dbar]",
        xlim = c(22, 28))
  
  lines( x = adp_145$ADP_1, y = press_145$p1, lty = 1,lwd = 3)
  lines( x = adp_145$ADP_3, y = press_145$p3, lty = 3, lwd = 3) 
  
  #135
  lines( x = adp_135$ADP_1, y = press_135$p1, lty = 1, col = "blue", lwd = 3)
  
  #164
  lines( x = adp_164$ADP_2, y = press_164$p2, lty = 2,lwd = 3, col = "red3")
  lines( x = adp_164$ADP_3, y = press_164$p3, lty = 3,lwd = 3 ,col = "red3")
  
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  text(x = 27.5, y = -50, 
       font = 2, 
       paste("a."))
  
  colores_ordenados <- c('black', 'blue', 'red','white','black','black','black')
  legend("bottomleft", legend = c("145","135","164", "Etapas",
                                   "Temprana", "Desarrollada", "Envejecida"), 
         col = colores_ordenados, 
         lty = c(1,1,1, NA, 1, 2, 3),
         lwd = c(3,3,3,NA,3,3,3))
  title(main = "Remolinos ciclónicos", cex.main = 1.3, line = 0.7)
  
  
  ######################### ANTICICLÓNICOS 
  
  par(new = "TRUE", 
      plt = c(0.58, 0.95, 0.15, 0.95), 
      las = 1)
  
  
  #151
  plot( x = adp_151$ADP_1, y = press_151$p1, type = "l", lwd = 3,
        yaxs = "i", 
        xlab = "ADP [Kg/m³]", ylab = "", 
        xlim = c(22,28),
        #xlim = c(floor(min(sb_131$sal_2)), max(sb_131$sal_2)),
        yaxt = "n")
  
  lines( x = adp_151$ADP_2, y = press_151$p2, lty = 2, lwd = 3)
  lines( x = adp_151$ADP_3, y = press_151$p3, lty = 3, lwd = 3) 
  
  #146
  lines( x = adp_146$ADP_1, y = press_146$p1, lty = 1,lwd = 3, col = "blue")
  lines( x = adp_146$ADP_2, y = press_146$p2, lty = 2,lwd = 3, col = "blue")
  
  #131
  lines( x = adp_131$ADP_2, y = press_131$p2, lty = 2,lwd = 3, col = "red3")
  lines( x = adp_131$ADP_3, y = press_131$p3, lty = 3, lwd = 3 ,col = "red3")
  grid(nx = NULL, ny = NULL, col = "#2B2B2B", lty = "dotted")
  
  text(x = 27.5, y = -50, 
       font = 2, 
       paste("b."))
  
  colores_ordenados <- c('black', 'blue', 'red','white','black','black','black')
  legend("bottomleft", legend = c("151","146","131", "Etapas",
                                   "Temprana", "Desarrollada", "Envejecida"), 
         col = colores_ordenados,  
         lty = c(1,1,1, NA, 1, 2, 3),
         lwd = c(3,3,3,NA,3,3,3))
  
  title(main = "Remolinos anticiclónicos", cex.main = 1.3, line = 0.7)
  
  
  dev.off()
  
}


