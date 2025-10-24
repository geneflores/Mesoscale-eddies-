######
###  CALCULAR == DIFERENCIAS ENTRE ANOMALÍA DEL MODELO Y LAS BOYAS
###### 


###  PASOS  #### 

  ## 1. Cargar paqueterías
        { pacman::p_load(ncdf4, R.utils, maps, pracma, ggplot2, 
                   RColorBrewer, paletteer,animation, tidyverse,
                   plotly)
    library(R.utils)
    library(maps)
    library(pracma)
    library(ggplot2)
    library(RColorBrewer)
    library(paletteer)
    library(animation)
    library(plotly)
    library(ncdf4)
    library(tidyverse)
    library(oce)
    library(ggExtra)
    library(corrplot)
    library(npphen)
    library(xts)
    library(scales)
    library(ggpubr)
    library(akima)
    library(geosphere)
    library(gridExtra)
    library(png)
    library(patchwork)
    library(grid)
    library(interp)
    library(sf)}
  ## 2. Cargar remolinos (ciclónicos o anticiclónicos)
        {# Elegir remolinos ciclónicos o anticiclónicos 
    
    ## CICLÓNICOS 185
    setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic")
    alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Cyclonic_long_19930101_20220209.nc"))
    nc <- nc_open(alist) #print(nc) -- displays nc file info 
    time <- ncvar_get(nc,'time') #days since 1950-01-01 00:00:00 UTC
    t1 <- as.Date(time, origin = "1950-01-01 00:00:00", tz='UTC')
    
    load("~/Remolinos/META3.2DTallsat/Cyclonic/rem_cy_boys") #Ciclónicos 
    
    ##ANTICICLÓNICOS 180
    setwd("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Anticyclonic")
    alist <- dir(pattern = glob2rx("META3.2_DT_allsat_Anticyclonic_long_19930101_20220209.nc"))
    nc <- nc_open(alist) #print(nc) -- displays nc file info 
    
    load("~/Remolinos/META3.2DTallsat/Anticyclonic/rem_anticy_boys") #Anticiclónicos
  }
  ## 3. Cargar datos de boyas (por los track, remboy, etc.)
        #Archivo abajo
       ###JALAR DATOS DE LA BOYAS
       load("C:/Users/genef/Documents/Remolinos/META3.2DTallsat/Cyclonic/argos_CT_SA_tem_sal_caribe.RData")

  ## 4. Cargar variable "prof" 
        {
    
    setwd("C:/Users/genef/Documents/Remolinos/CMEMS/GLORYS")
    alist <- dir(pattern = glob2rx("cmems_mod_glo_phy_my_0.083deg_P1D-m_febrero20.nc"))
    
    #setwd("C:/Users/genef/Documents/Remolinos/CMEMS/HINDCAST")
    #alist <- dir(pattern = glob2rx("cmems_mod_glo_bgc_my_0.25deg_P1D-m_diciembre20.nc"))
    
    nc <- nc_open(alist) #print(nc) 
    
    Z <- ncvar_get(nc,'depth')  #34 niveles 
    lat <- ncvar_get(nc,'latitude')
    
    #1.1  CONVERTIR altura a profundidad y promediar latitudes
    z <- Z * -1
    la <- mean(lat)
    {
      #Haciendo pruebas la presión varió 0.0003 en la superfucie 
      #entre el valor mínimo y máximo de latitud y varió 0.72 decibares de 
      #en lo más profunddo de la base de datos entre el min y máx
      # así que no hay tanta diferencia en tomar el promedio de las latitudes 
    }
    
    #1.2 Convertir profundidad a presión 
    p <- gsw_p_from_z(z = z, latitude = la)
    
    prof <- seq(min(p),max(p),length=length(p))-max(p)
    #prof2 <- seq(min(p),max(p),length=length(p)) #revisar estas diferencias
    
    rm(Z, lat, nc, alist, z, la)
    
  }
  ## 5. Cargar datos del modelo del remolino de interés 
         # para revisar el tamaño de matriz
          #archivo dentro de cada remolino
  ## 6.  Convertir eje X a distancias en km 
  { x_valores <- valores$x_valores
    y_valores <- valores$y_valores
    
    dis<-0
    for (k in 1:(length(x_valores)-1)){
      aux<-distm(c(x_valores[k],y_valores[k]),
                 c(x_valores[k+1],y_valores[k+1]),
                 fun = distHaversine)
      dis[k+1]<-dis[k] + aux
    }
    
    
    dis<-0
    for (k in 1:(length(x_valores)-1)){
      aux<-distm(c(x_valores[k],y_valores[k]),
                 c(x_valores[k+1],y_valores[k+1]),
                 fun = distHaversine)
      dis[k+1]<-dis[k]+aux
    }
    
    #dis <- as.numeric(format(0.8+dis/1e3,digits=2))
    #dis <- as.numeric(format(dis/1e3,digits=2))
    #x_vals <- dis <-dis/1e3
    x_vals <- dis/1e3
}

##LINEAS LONGITUDINALES
{
  
coor <- valores$coordenadas
x1 <- 1    #1,3,5
x2 <- 2    #2,4,6

y1 <- -1
y2 <- -1

punto_central <- c(median(x_valores), median(y_valores))
punto1 <- c(coor$longitud[x1], coor$latitud[x1])
punto2 <- c(coor$longitud[x2], coor$latitud[x2])

dis_centro_1 <- distHaversine(punto_central, punto1)
dis_centro_2 <- distHaversine(punto_central, punto2)

dis1 <- dis_centro_1/1000*y1  #1,2,3 multiplicar por *-1
dis2 <- dis_centro_2/1000*y2    #4,5,6 NO multiplicar(depende el polígono)
}

       
       
       

####### CICLÓNICO 135
{
  #HACER ANÁLISIS DE BOYAS FECHA VS DATOS 
  {irb <- 135
  iboy <- which(track[ibuenor]==remboy[irb])
  buoy <- unique(ibuenob[iboy])
  
  #REVISAR FECHAS
  for(i in 1:length(buoy)){
    date <- datos$time[ini1[buoy[i]]]
    print(date)
  }
  
  # [1] "2019-09-22 12:50:30 UTC"  # BOYA INTERÉS > 29-SEP-2019 -NO 
  # [2] "2019-06-06 05:44:46 UTC"  
  # [3] "2019-06-16 04:29:55 UTC"  # BOYA INTERÉS > 20-JUN-2019 - SI   
  # [4] "2019-09-16 12:54:30 UTC"  NO
  # [5] "2019-08-11 12:54:30 UTC"  SI
  # [6] "2019-10-07 12:49:30 UTC"  NO
  # [7] "2019-10-16 12:50:30 UTC"  NO
  # [8] "2019-10-22 12:51:30 UTC"  NO
  # [9] "2019-10-25 12:51:30 UTC"  # BOYA INTERÉS > 01-NOV-2019 -NO 
  
  #VERIFICAR DATOS Y CALCULAR ADP
  }
  
  ################# 16-JUN-2019 - única fecha con datos 
  {
    ##CARGAR Variable "prof" previamente
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("prof.RData")
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 3
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_b <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_135_1_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      
      recorte_datos <- st_135_1_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 565,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 565,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 565,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r135.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_1 <- sigma_theta20 - ADP_b 
    
    dif_SA_1  <- transect20$sal - SA_b 
    
    dif_pt_1  <- transect20$temp - pt_b 
    
    #promedio en la dimension 2 en todo el transecto
    Tprom  <- apply(dif_pt_1,2,mean,na.rm=T) 
    
    #TP de las bpyas interpolada a la profundidad del modelo
    Ti <- approx(recorte_datos$presion,recorte_datos$pt,prof)
    
    #anomalia de tp
    aT<- Ti$y - Tprom
    
    
    #promedio en la dimension 1 en todo el transecto
    Tpromtransecto<-apply(dif_pt_1,1,mean,na.rm=T)
    
    
    ### PROMEDIO TEMP 
    Trem<-apply(transect20$temp[51:185,],2,mean,na.rm=T)
    
    aT<- transect20$temp - Trem
    
    
    
    resta <-rev(Trem) - Ti$y
  
    
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    palD <- colorRampPalette((c("#01357B", "#00AAAE","#FCF5F2","#FF6E5A","#C91837")))
    palGENE <- colorRampPalette(
      c("#611300", "#C33A00", "#E49183", "#F7F5F4", "#A3C8DF", "#007FB6","#003560")
    )
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD
    filled.contour(x_vals,prof,rot90(dif_SA_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_1,2),
                   #zlim = c(-20,20),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palGENE, 
                   plot.axes = {axis(1); axis(2)
                     abline(v = dis1, col = 'black', lwd = 3, lty = 1)
                     abline(v = dis2, col = 'black', lwd = 3, lty = 1)})
    
    
    
    
    palt <- colorRampPalette(c("#3E0689", "#AE1987", "#EA8D2D", "#E3E400"))
    
    filled.contour(x_vals,prof,rot90(transect20$temp,2),
                   xlab='distance [Km]', ylab='depth [m]', 
                   color.palette = palt,
                   #zlim = c(34.5, 37.5),
                   plot.axes = {axis(1); axis(2)
                     abline(v = dis1, col = 'black', lwd = 3, lty = 1)
                     abline(v = dis2, col = 'black', lwd = 3, lty = 1)})
    
    
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_135_1.RData", dif_ADP_1, dif_SA_1, dif_pt_1,
         st_135_1_recortado)
    
  }
  
  #JUNTAR TODAS LAS VARIABLES EN UN RData
  {
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("dif_135_1.RData")
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r135.RData")
    
    #Reajustar nombres
    
    #Anomalías modelo en una sola lista
    ADP_modelo <- list(ADP_1 = sigma_theta20)
    
    ADP_boyas <- list(ADP_1 = st_135_1_recortado$ADP)
    
    dif_ADP <- list(dif_1 = dif_ADP_1)
    
    temp_modelo <- list(pt_1 = transect20$temp)
    
    temp_boyas <- list(pt_1 = st_135_1_recortado$pt)
    
    dif_temp <- list(dif_1 = dif_pt_1)
    
    sal_modelo <-  list(sal_1 = transect20$sal)
    
    sal_boyas <- list(sal_1 = st_135_1_recortado$SA)
    
    dif_sal <- list(dif_1 = dif_SA_1)
    
    pres_boyas <- list(p1 = st_135_1_recortado$presion)
    
    rm(sigma_theta01, sigma_theta20, sigma_theta29, transect01, 
       transect20, transect29, dif_ADP_1, dif_pt_1, dif_SA_1, st_135_1_recortado)
    
    ADP <- list(APD_boyas = ADP_boyas, 
                ADP_modelo = ADP_modelo, 
                dif_ADP = dif_ADP)
    
    sal <- list(sal_boyas = sal_boyas, 
                sal_modelo = sal_modelo, 
                dif_sal = dif_sal)
    
    temp <- list(temp_boyas = temp_boyas, 
                 temp_modelo = temp_modelo, 
                 dif_temp = dif_temp)
    
    rm(ADP_boyas, ADP_modelo, dif_ADP, sal_boyas, sal_modelo, 
       temp_boyas, temp_modelo, dif_sal, dif_temp)
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "alldatos135.RData", ADP, sal, temp,
         pres_boyas, valores)
    
    #VERIFICAR QUE SE HAYA GUARDADO
    load("alldatos135.RData")
    
  }

  }

####### CICLÓNICO 145
{
  
  ######################  HACER ANÁLISIS DE BOYAS FECHA VS DATOS 
  { irb <- 145
  iboy <- which(track[ibuenor]==remboy[irb])
  buoy <- unique(ibuenob[iboy])
  
  #REVISAR FECHAS
  for(i in 1:length(buoy)){
    date <- datos$time[ini1[buoy[i]]]
    print(date)
  }
  
  # [1] "2020-02-09 03:49:40 UTC"  # INT > 08-FEB-2020   3
  # [2] "2019-11-29 09:10:43 UTC"  # INT > 28-NOV-2019   1
  # [3] "2019-12-09 02:23:29 UTC"
  # [4] "2020-01-07 11:26:00 UTC"  # INT > 10-ENE-2020   2
  # [5] "2020-03-09 05:59:53 UTC"
  # [6] "2019-11-20 07:13:00 UTC"
  # [7] "2019-12-10 07:19:00 UTC"
  # [8] "2019-12-30 07:14:00 UTC"
  # [9] "2019-11-18 12:50:30 UTC"
  
  #VERIFICAR DATOS Y CALCULAR ADP
  }
  
  ######################ORDENAR por fecha consecutiva y REALIZAR cálculos
  
  
  ################# 29-NOV-2019
  {ib <- 2
  
  min <- ini1[buoy[ib]]
  max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
  
  min; max
  max - min
  
  #Temperatura 
  CT_1 <- data.frame(temp = datos$CT[min:max],
                     presion = -datos$pres[min:max])
  
  #Salinidad 
  SA_1 <- data.frame(sal = datos$SA[min:max],
                     presion = -datos$pres[min:max])
  
  
  #####SOLO calcular ADP
  sigma_theta_145_1 <- gsw_sigma0(SA = SA_1$sal, CT = CT_1$temp) 
  

  ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
                                     #salinidad absoluta y la presion
                                     ####OJO > Convertir CT a pt
  
  sigma_t_145_1 <- data.frame(ADP = gsw_sigma0(SA = SA_1$sal, CT = CT_1$temp),
                              pt = gsw_pt_from_CT(SA = SA_1$sal, CT = CT_1$temp),
                              SA = SA_1$sal, 
                              presion = SA_1$presion
  )
  
  
  #RECORTAR DATA FRAME 
  st_145_1_recortado <- sigma_t_145_1[sigma_t_145_1$presion >= -912.04, ]
  
  #VER DATOS
  {
  plot(x = st_145_1_recortado$ADP, 
       y = st_145_1_recortado$presion, type = "l") }
  
  #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
  
  ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
  
  ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
  ##usando un ciclo flor
  
  ADP <- numeric(35)  
  SA  <- numeric(35) 
  pt  <- numeric(35)
  
  for(i in 1:35){
    
    x <- prof[i]
    
    if (i == 1){
      # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
      indices <- st_145_1_recortado$presion <= x
      cat("Intervalo 1: promedio de valores <= ", x, "\n")
    } else {
      # Para los demás índices, calcular promedio entre x y x2
      x2 <- prof[i - 1]
      x3 <- prof[i]
      indices <- st_145_1_recortado$presion >= x2 & st_145_1_recortado$presion <= x3
      cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
    }
     
    
    #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
    ADP[i] <- mean(st_145_1_recortado$ADP[indices], na.rm = TRUE)
    SA[i] <- mean(st_145_1_recortado$SA[indices], na.rm = TRUE)
    pt[i] <- mean(st_145_1_recortado$pt[indices], na.rm = TRUE)
    
    # Imprimir los resultados de cada paso para depuración
    cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
  }
  
  #### INVERTIR ORDEN DE LOS DATOS
  ADP_in <- rev(ADP)
  SA_in <- rev(SA)
  pt_in <- rev(pt)
  
  
  #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
  ADP_145_1 <- matrix(ADP_in, nrow = 1027,      
                      ncol = 35, byrow = TRUE)
  SA_145_1 <- matrix(SA_in, nrow = 1027,      
                      ncol = 35, byrow = TRUE)
  pt_145_1 <- matrix(pt_in, nrow = 1027,      
                      ncol = 35, byrow = TRUE)
  
  
  #JALAR DATOS DEL REMOLINO 
  setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
  load("datos_r145.RData")  ## 
  
  ########### CALCULAR DIFERENCIAS
  dif_ADP_1 <- sigma_theta28 - ADP_145_1 
  
  dif_SA_1  <- transect28$sal - SA_145_1 
  
  dif_pt_1  <- transect28$temp - pt_145_1 
  
  
  ########### PROBAR SI FUNCIONA EL GRÁFICO 
  palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
  
  palD <- colorRampPalette((c("#040404",
                              "#183456",
                              "#9BB1DA",
                              "#FCFCFC")))
  
  palD <- colorRampPalette((c("#01357B", "#00AAAE","#FCF5F2","#FF6E5A","#C91837")))
  palGENE <- colorRampPalette(
    c("#611300", "#C33A00", "#E49183", "#F7F5F4", "#A3C8DF", "#007FB6","#003560")
  )
  
  filled.contour(x_vals,prof,rot90(dif_ADP_1,2),
                 xlab='distance [Km]', ylab='presion [dbar]',
                 color.palette = palD)
  
  
  filled.contour(x_vals,prof,rot90(dif_SA_1,2),
                 xlab='distance [Km]', ylab='presion [dbar]',
                 color.palette = palD)
  
  filled.contour(x_vals,prof,rot90(dif_pt_1,2),
                 xlab='distance [Km]', ylab='presion [dbar]',
                 color.palette = palD)
  
  
  ########### GUARDAR DATOS
  
  setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
  
  save(file = "dif_145_1.RData", dif_ADP_1, dif_SA_1, dif_pt_1,
       st_145_1_recortado)
  
  }
  
  ################# 07-ENE-2020
  { 
    ## COREER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
    ib <- 4
    
    min <- ini1[buoy[ib]]
    max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
    
    min; max
    max - min
    
    #Temperatura 
    CT_b <- data.frame(temp = datos$CT[min:max],
                       presion = -datos$pres[min:max])
    
    #Salinidad 
    SA_b <- data.frame(sal = datos$SA[min:max],
                       presion = -datos$pres[min:max])
    
    
    #####SOLO calcular ADP
    sigma_theta_145 <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
    
    
    ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
    #salinidad absoluta y la presion
    ####OJO > Convertir CT a pt
    
    sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
    
    
    #RECORTAR DATA FRAME 
    st_145_2_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
    
    #VER DATOS de anomalía - también están los de sal y temp
    {
      plot(x = st_145_2_recortado$ADP, 
           y = st_145_2_recortado$presion, type = "l") }
    
    #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
    
    ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
    
    ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
    ##usando un ciclo flor
    
    recorte_datos <- st_145_2_recortado
    
    ADP <- numeric(35)  
    SA  <- numeric(35) 
    pt  <- numeric(35)
    
    for(i in 1:35){
      
      x <- prof[i]
      
      if (i == 1){
        # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
        indices <- recorte_datos$presion <= x
        cat("Intervalo 1: promedio de valores <= ", x, "\n")
      } else {
        # Para los demás índices, calcular promedio entre x y x2
        x2 <- prof[i - 1]
        x3 <- prof[i]
        indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
        cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
      }
      
      
      #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
      ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
      SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
      pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
      
      # Imprimir los resultados de cada paso para depuración
      cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
    }
    
    #### INVERTIR ORDEN DE LOS DATOS
    ADP_in <- rev(ADP)
    SA_in <- rev(SA)
    pt_in <- rev(pt)
    
    
    #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
    ADP_145 <- matrix(ADP_in, nrow = 1027,      
                        ncol = 35, byrow = TRUE)
    SA_145 <- matrix(SA_in, nrow = 1027,      
                       ncol = 35, byrow = TRUE)
    pt_145 <- matrix(pt_in, nrow = 1027,      
                       ncol = 35, byrow = TRUE)
    
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r145.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_2 <-  sigma_theta10 - ADP_145 
    
    dif_SA_2  <- transect10$sal - SA_145 
    
    dif_pt_2  <-  transect10$temp - pt_145 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    
    #palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    #palD <- colorRampPalette((c("#040404",
     #                           "#183456",
      #                          "#9BB1DA",
       #                         "#FCFCFC")))
    
    palD <- colorRampPalette((c("#01357B", "#00AAAE","#FCF5F2","#FF6E5A","#C91837")))
    palGENE <- colorRampPalette(
      c("#611300", "#C33A00", "#E49183", "#F7F5F4", "#A3C8DF", "#007FB6","#003560")
    )
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    filled.contour(x_vals,prof,rot90(dif_ADP_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    filled.contour(x_vals,prof,rot90(dif_SA_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    filled.contour(x_vals,prof,rot90(dif_pt_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_145_2.RData", dif_ADP_2, dif_SA_2, dif_pt_2,
         st_145_2_recortado)
  }
  
  ################# 09-FEB-2020
  {
    ##CARGAR Variable "prof" previamente
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 1
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_145 <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_145_3_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = st_145_3_recortado$ADP, 
             y = st_145_3_recortado$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      
      recorte_datos <- st_145_3_recortado
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_145 <- matrix(ADP_in, nrow = 1027,      
                        ncol = 35, byrow = TRUE)
      SA_145 <- matrix(SA_in, nrow = 1027,      
                       ncol = 35, byrow = TRUE)
      pt_145 <- matrix(pt_in, nrow = 1027,      
                       ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r145.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_3 <-  sigma_theta08 - ADP_145 
    
    dif_SA_3  <- transect08$sal - SA_145
    
    dif_pt_3  <-  transect08$temp - pt_145 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    palD <- colorRampPalette((c("#01357B", "#00AAAE","#FCF5F2","#FF6E5A","#C91837")))
    palGENE <- colorRampPalette(
      c("#611300", "#C33A00", "#E49183", "#F7F5F4", "#A3C8DF", "#007FB6","#003560")
    )
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    filled.contour(x_vals,prof,rot90(dif_ADP_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    filled.contour(x_vals,prof,rot90(dif_SA_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    filled.contour(x_vals,prof,rot90(dif_pt_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_145_3.RData", dif_ADP_3, dif_SA_3, dif_pt_3,
         st_145_3_recortado)
    
  }
  
  #JUNTAR TODAS LAS VARIABLES EN UN RData
  {
  setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
  load("dif_145_1.RData")
  load("dif_145_2.RData")
  load("dif_145_3.RData")
  setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
  load("datos_r145.RData")
  
  #Reajustar nombres
  
  #Anomalías modelo en una sola lista
  ADP_modelo <- list(ADP_1 = sigma_theta28, 
                     ADP_2 = sigma_theta10, 
                     ADP_3 = sigma_theta08)
  
  ADP_boyas <- list(ADP_1 = st_145_1_recortado$ADP, 
                    ADP_2 = st_145_2_recortado$ADP,
                    ADP_3 = st_145_3_recortado$ADP)
  
  dif_ADP <- list(dif_1 = dif_ADP_1, 
                  dif_2 = dif_ADP_2,
                  dif_3 = dif_ADP_3)
  
  
  temp_modelo <- list(pt_1 = transect28$temp, 
                      pt_2 = transect10$temp,
                      pt_3 = transect08$temp)
  
  temp_boyas <- list(pt_1 = st_145_1_recortado$pt, 
                     pt_2 = st_145_2_recortado$pt,
                     pt_3 = st_145_3_recortado$pt)
  
  dif_temp <- list(dif_1 = dif_pt_1, 
                   dif_2 = dif_pt_2, 
                   dif_3 = dif_pt_3)
  
  
  sal_modelo <-  list(sal_1 = transect28$sal, 
                      sal_2 = transect10$sal,
                      sal_3 = transect08$sal)
  
  sal_boyas <- list(sal_1 = st_145_1_recortado$SA, 
                    sal_2 = st_145_2_recortado$SA,
                    sal_3 = st_145_3_recortado$SA)
  
  dif_sal <- list(dif_1 = dif_SA_1, 
                  dif_2 = dif_SA_2, 
                  dif_3 = dif_SA_3)
  
  
  pres_boyas <- list(p1 = st_145_1_recortado$presion, 
                     p2 = st_145_2_recortado$presion, 
                     p3 = st_145_3_recortado$presion)
  
  ADP <- list(APD_boyas = ADP_boyas, 
              ADP_modelo = ADP_modelo, 
              dif_ADO = dif_ADP)
  
  sal <- list(sal_boyas = sal_boyas, 
              sal_modelo = sal_modelo, 
              dif_sal = dif_sal)
  
  temp <- list(temp_boyas = temp_boyas, 
               temp_modelo = temp_modelo, 
               dif_temp = dif_temp)
  
  setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
  
  save(file = "alldatos145.RData", ADP, sal, temp,
       pres_boyas, valores)
  
  load("alldatos145.RData")
  
  }
}

####### CICLÓNICO 164
{
  ######################  HACER ANÁLISIS DE BOYAS FECHA VS DATOS 
  { irb <- 164
  iboy <- which(track[ibuenor]==remboy[irb])
  buoy <- unique(ibuenob[iboy])
  
  #REVISAR FECHAS
  for(i in 1:length(buoy)){
    date <- datos$time[ini1[buoy[i]]]
    print(date)
  }
  
  # [1] "2020-11-03 15:17:40 UTC" # INT > 04-NOV-2020
  # [2] "2020-11-23 11:54:17 UTC"
  # [3] "2020-12-03 07:51:57 UTC" # INT > 01-DIC-2020
  
  #VERIFICAR DATOS Y CALCULAR ADP
  
  }
  
  ################# 03-NOV-2020
  {
    ##CARGAR Variable "prof" previamente
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("prof.RData")
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 1
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_164 <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_164_2_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = st_164_2_recortado$ADP, 
             y = st_164_2_recortado$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      
      recorte_datos <- st_164_2_recortado
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 446,      
                        ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 446,      
                       ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 446,      
                       ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r164.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_2 <- sigma_theta02 - ADP_b 
    
    dif_SA_2  <- transect2$sal - SA_b 
    
    dif_pt_2  <-  transect2$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    filled.contour(x_vals,prof,rot90(dif_ADP_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    filled.contour(x_vals,prof,rot90(dif_SA_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    filled.contour(x_vals,prof,rot90(dif_pt_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_164_2.RData", dif_ADP_2, dif_SA_2, dif_pt_2,
         st_164_2_recortado)
    
  }
  
  ################# 03-DIC-2020
  {
    ##CARGAR Variable "prof" previamente
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("prof.RData")
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 3
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_b <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_164_3_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      
      recorte_datos <- st_164_3_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 446,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 446,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 446,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r164.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_3 <- sigma_theta03 - ADP_b 
    
    dif_SA_3  <- transect3$sal - SA_b 
    
    dif_pt_3  <-  transect3$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD
    filled.contour(x_vals,prof,rot90(dif_SA_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_164_3.RData", dif_ADP_3, dif_SA_3, dif_pt_3,
         st_164_3_recortado)
    
  }
  
  #JUNTAR TODAS LAS VARIABLES EN UN RData
  {
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("dif_164_2.RData")
    load("dif_164_3.RData")
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r164.RData")
    
    #Reajustar nombres
    
    #Anomalías modelo en una sola lista
    ADP_modelo <- list(ADP_2 = sigma_theta02, 
                       ADP_3 = sigma_theta03)
    
    ADP_boyas <- list(ADP_2 = st_164_2_recortado$ADP, 
                      ADP_3 = st_164_3_recortado$ADP)
                      
    
    dif_ADP <- list(dif_2 = dif_ADP_2, 
                    dif_3 = dif_ADP_3)
                    
    
    temp_modelo <- list(pt_2 = transect2$temp, 
                        pt_3 = transect3$temp)
    
    temp_boyas <- list(pt_2 = st_164_2_recortado$pt, 
                       pt_3 = st_164_3_recortado$pt)
    
    dif_temp <- list(dif_2 = dif_pt_2, 
                     dif_3 = dif_pt_3)
    
    
    sal_modelo <-  list(sal_2 = transect2$sal, 
                        sal_3 = transect3$sal)
    
    sal_boyas <- list(sal_2 = st_164_2_recortado$SA, 
                      sal_3 = st_164_3_recortado$SA)
    
    dif_sal <- list(dif_2 = dif_SA_2, 
                    dif_3 = dif_SA_3)
    
    
    pres_boyas <- list(p2 = st_164_2_recortado$presion, 
                       p3 = st_164_3_recortado$presion)
    
    
    ADP <- list(APD_boyas = ADP_boyas, 
                ADP_modelo = ADP_modelo, 
                dif_ADP = dif_ADP)
    
    sal <- list(sal_boyas = sal_boyas, 
                sal_modelo = sal_modelo, 
                dif_sal = dif_sal)
    
    temp <- list(temp_boyas = temp_boyas, 
                 temp_modelo = temp_modelo, 
                 dif_temp = dif_temp)
    
    rm(ADP_boyas, ADP_modelo, dif_ADP, sal_boyas, sal_modelo, 
       temp_boyas, temp_modelo, dif_sal, dif_temp)
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "alldatos164.RData", ADP, sal, temp,
         pres_boyas, valores)
    
    #VERIFICAR QUE SE HAYA GUARDADO
    load("alldatos164.RData")
    
  }
  
}


###############################################

###### ANTICICLÓNICO 131
{
######################  HACER ANÁLISIS DE BOYAS FECHA VS DATOS 
{ 
  irb <- 131
  iboy <- which(track[ibuenor]==remboy[irb])
  buoy <- unique(ibuenob[iboy])

   #REVISAR FECHAS
   for(i in 1:length(buoy)){
      date <- datos$time[ini1[buoy[i]]]
       print(date)
     }

  # [1] "2019-09-13 11:47:00 UTC"  
  # [2] "2019-04-24 06:55:00 UTC"   
  # [3] "2019-05-04 07:09:00 UTC"
  # [4] "2019-06-03 07:06:00 UTC"
  # [5] "2019-08-07 21:08:00 UTC"
  # [6] "2019-06-08 20:48:00 UTC"   # INT > 15-JUN-2019   NO
  # [7] "2019-08-10 07:06:08 UTC"
  # [8] "2019-08-30 00:20:59 UTC"   
  # [9] "2019-10-09 21:20:52 UTC"   # INT > 10-OCT-2019  SÍ
  # [10] "2019-05-23 09:40:59 UTC"
  # [11] "2019-06-02 06:24:07 UTC"
  # [12] "2019-08-17 12:46:30 UTC"  # INT > 15-AGO-2019  SÍ
  # [13] "2019-08-11 12:54:30 UTC"
  # [14] "2019-09-04 12:50:30 UTC"
  # [15] "2019-09-16 12:54:30 UTC"
  # [16] "2019-09-25 12:50:30 UTC"
  # [17] "2019-10-10 12:49:30 UTC"  # INT > 10-OCT-2019 NO
  # [18] "2019-04-27 10:51:25 UTC"
  # [19] "2019-10-16 12:50:30 UTC"

#VERIFICAR DATOS Y CALCULAR ADP

}
  
  ################# 17-AGO-2019
  {
    ##CARGAR Variable "prof" previamente
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 12
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_131 <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_131_2_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      recorte_datos <- st_131_2_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
      load("prof.RData")
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 1694,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 1694,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 1694,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r131.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_2 <- sigma_theta02 - ADP_b 
    
    dif_SA_2  <- transect2$sal - SA_b 
    
    dif_pt_2  <- transect2$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD 
    filled.contour(x_vals,prof,rot90(dif_SA_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_131_2.RData", dif_ADP_2, dif_SA_2, dif_pt_2,
         st_131_2_recortado)
    
  }
  
  ################# 09-OCT-2019
  {
    ##CARGAR Variable "prof" previamente
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 9
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_131 <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_131_3_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      recorte_datos <- st_131_3_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
      load("prof.RData")
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 1694,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 1694,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 1694,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r131.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_3 <- sigma_theta03 - ADP_b 
    
    dif_SA_3  <- transect3$sal - SA_b 
    
    dif_pt_3  <-  transect3$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD 
    filled.contour(x_vals,prof,rot90(dif_SA_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_131_3.RData", dif_ADP_3, dif_SA_3, dif_pt_3,
         st_131_3_recortado)
    
  }
  
  #JUNTAR TODAS LAS VARIABLES EN UN RData
  {
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("dif_131_2.RData")
    load("dif_131_3.RData")
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r131.RData")
    
    #Reajustar nombres
    
    #Anomalías modelo en una sola lista
    ADP_modelo <- list(ADP_2 = sigma_theta02, 
                       ADP_3 = sigma_theta03)
    
    ADP_boyas <- list(ADP_2 = st_131_2_recortado$ADP, 
                      ADP_3 = st_131_3_recortado$ADP)
    
    
    dif_ADP <- list(dif_2 = dif_ADP_2, 
                    dif_3 = dif_ADP_3)
    
    
    temp_modelo <- list(pt_2 = transect2$temp, 
                        pt_3 = transect3$temp)
    
    temp_boyas <- list(pt_2 = st_131_2_recortado$pt, 
                       pt_3 = st_131_3_recortado$pt)
    
    dif_temp <- list(dif_2 = dif_pt_2, 
                     dif_3 = dif_pt_3)
    
    
    sal_modelo <-  list(sal_2 = transect2$sal, 
                        sal_3 = transect3$sal)
    
    sal_boyas <- list(sal_2 = st_131_2_recortado$SA, 
                      sal_3 = st_131_3_recortado$SA)
    
    dif_sal <- list(dif_2 = dif_SA_2, 
                    dif_3 = dif_SA_3)
    
    
    pres_boyas <- list(p2 = st_131_2_recortado$presion, 
                       p3 = st_131_3_recortado$presion)
    
    rm(sigma_theta01, sigma_theta02, sigma_theta03, transect01, 
       transect02, transect03, dif_ADP_2, dif_pt_2, dif_SA_2, st_131_2_recortado,
       dif_ADP_3, dif_pt_3, dif_SA_3, st_131_3_recortado)
    
    ADP <- list(APD_boyas = ADP_boyas, 
                ADP_modelo = ADP_modelo, 
                dif_ADP = dif_ADP)
    
    sal <- list(sal_boyas = sal_boyas, 
                sal_modelo = sal_modelo, 
                dif_sal = dif_sal)
    
    temp <- list(temp_boyas = temp_boyas, 
                 temp_modelo = temp_modelo, 
                 dif_temp = dif_temp)
    
    rm(ADP_boyas, ADP_modelo, dif_ADP, sal_boyas, sal_modelo, 
       temp_boyas, temp_modelo, dif_sal, dif_temp)
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "alldatos131.RData", ADP, sal, temp,
         pres_boyas, valores)
    
    #VERIFICAR QUE SE HAYA GUARDADO
    load("alldatos131.RData")
    
  }
  
}

###### ANTICICLÓNICO 146
{
  ######################  HACER ANÁLISIS DE BOYAS FECHA VS DATOS 
  { 
    irb <- 146
    iboy <- which(track[ibuenor]==remboy[irb])
    buoy <- unique(ibuenob[iboy])
    
    #REVISAR FECHAS
    for(i in 1:length(buoy)){
      date <- datos$time[ini1[buoy[i]]]
      print(date)
    }
    # 
    # [1] "2020-01-29 07:19:00 UTC"
    # [2] "2020-02-08 07:18:00 UTC" # INT > 08-FEB-2020 NO
    # [3] "2019-12-31 12:50:30 UTC"
    # [4] "2020-02-18 07:19:00 UTC"
    # [5] "2020-01-03 12:50:30 UTC"
    # [6] "2020-01-06 12:46:30 UTC"
    # [7] "2020-01-12 12:58:30 UTC"  # INT > 15-ENERO-2020 SÍ
    # [8] "2020-01-18 12:54:30 UTC"
    # [9] "2020-02-02 12:57:30 UTC"  # INT > 08-FEB-2020 SÍ
    
    #VERIFICAR DATOS Y CALCULAR ADP
    
  }
  
  ################# 12-ENERO-2020
  {
    ##CARGAR Variable "prof" previamente
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 7
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_b <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_146_1_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      recorte_datos <- st_146_1_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
      load("prof.RData")
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 577,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 577,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 577,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r146.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_1 <- sigma_theta01 - ADP_b 
    
    dif_SA_1  <- transect1$sal - SA_b 
    
    dif_pt_1  <-  transect1$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD 
    filled.contour(x_vals,prof,rot90(dif_SA_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_146_1.RData", dif_ADP_1, dif_SA_1, dif_pt_1,
         st_146_1_recortado)
    
  }
  
  ################# 02-FEBRERO-2020
  {
    ##CARGAR Variable "prof" previamente
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 9
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_b <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_146_2_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      recorte_datos <- st_146_2_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
      load("prof.RData")
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 577,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 577,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 577,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r146.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_2 <- sigma_theta02 - ADP_b 
    
    dif_SA_2  <- transect2$sal- SA_b 
    
    dif_pt_2  <-  transect2$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD 
    filled.contour(x_vals,prof,rot90(dif_SA_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_146_2.RData", dif_ADP_2, dif_SA_2, dif_pt_2,
         st_146_2_recortado)
    
  }
  
  #JUNTAR TODAS LAS VARIABLES EN UN RData
  {
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("dif_146_1.RData")
    load("dif_146_2.RData")
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r146.RData")
    
    #Reajustar nombres
    
    #Anomalías modelo en una sola lista
    ADP_modelo <- list(ADP_1 = sigma_theta01, 
                       ADP_2 = sigma_theta01)
    
    ADP_boyas <- list(ADP_1 = st_146_1_recortado$ADP, 
                      ADP_2 = st_146_2_recortado$ADP)
    
    
    dif_ADP <- list(dif_1 = dif_ADP_1, 
                    dif_2 = dif_ADP_2)
    
    
    temp_modelo <- list(pt_1 = transect1$temp, 
                        pt_2 = transect2$temp)
    
    temp_boyas <- list(pt_1 = st_146_1_recortado$pt, 
                       pt_2 = st_146_2_recortado$pt)
    
    dif_temp <- list(dif_1 = dif_pt_1, 
                     dif_2 = dif_pt_2)
    
    
    sal_modelo <-  list(sal_1 = transect1$sal, 
                        sal_2 = transect2$sal)
    
    sal_boyas <- list(sal_1 = st_146_1_recortado$SA, 
                      sal_2 = st_146_2_recortado$SA)
    
    dif_sal <- list(dif_1 = dif_SA_1, 
                    dif_2 = dif_SA_2)
    
    
    pres_boyas <- list(p1 = st_146_1_recortado$presion, 
                       p2 = st_146_2_recortado$presion)
    
    rm(sigma_theta01, sigma_theta02, sigma_theta03, transect01, 
       transect02, transect03, dif_ADP_2, dif_pt_2, dif_SA_2, st_146_2_recortado,
       dif_ADP_1, dif_pt_1, dif_SA_1, st_146_1_recortado,
       transect1, transect2, transect3)
    
    ADP <- list(APD_boyas = ADP_boyas, 
                ADP_modelo = ADP_modelo, 
                dif_ADP = dif_ADP)
    
    sal <- list(sal_boyas = sal_boyas, 
                sal_modelo = sal_modelo, 
                dif_sal = dif_sal)
    
    temp <- list(temp_boyas = temp_boyas, 
                 temp_modelo = temp_modelo, 
                 dif_temp = dif_temp)
    
    rm(ADP_boyas, ADP_modelo, dif_ADP, sal_boyas, sal_modelo, 
       temp_boyas, temp_modelo, dif_sal, dif_temp)
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "alldatos146.RData", ADP, sal, temp,
         pres_boyas, valores)
    
    #VERIFICAR QUE SE HAYA GUARDADO
    load("alldatos146.RData")
    
  }
  
}

###### ANTICICLÓNICO 151
{
  ######################  HACER ANÁLISIS DE BOYAS FECHA VS DATOS 
  { 
    irb <- 151
    iboy <- which(track[ibuenor]==remboy[irb])
    buoy <- unique(ibuenob[iboy])
    
    #REVISAR FECHAS
    for(i in 1:length(buoy)){
      date <- datos$time[ini1[buoy[i]]]
      print(date)
    }
    # 
    
    # [1] "2020-05-09 14:22:30 UTC"   # INT > 10- MAY-2020 SÍ
    # [2] "2020-05-11 12:52:30 UTC"
    # [3] "2020-05-23 12:50:30 UTC"
    # [4] "2020-05-26 12:55:30 UTC"
    # [5] "2020-06-01 12:52:30 UTC"   # INT > 07-JUN-2020 SÍ
    # [6] "2020-04-02 12:54:30 UTC"
    # [7] "2020-04-11 12:49:30 UTC"
    # [8] "2020-04-26 12:55:30 UTC"   # INT > 21-ABRIL-2020 SÍ
    # [9] "2020-04-29 12:55:30 UTC"
    # [10] "2020-06-16 12:51:30 UTC"
    
    
    
    #VERIFICAR DATOS Y CALCULAR ADP
    
  }
  
  ################# 26-ABRIL-2020
  {
    ##CARGAR Variable "prof" previamente
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 8
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_b <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_151_1_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      recorte_datos <- st_151_1_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
      load("prof.RData")
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 391,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 391,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 391,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r151.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_1 <- sigma_theta01 - ADP_b 
    
    dif_SA_1  <- transect1$sal - SA_b 
    
    dif_pt_1  <-  transect1$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD 
    filled.contour(x_vals,prof,rot90(dif_SA_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_1,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_151_1.RData", dif_ADP_1, dif_SA_1, dif_pt_1,
         st_151_1_recortado)
    
  }
  
  ################# 09-MAYO-2020
  {
    ##CARGAR Variable "prof" previamente
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 1
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_b <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_151_2_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      recorte_datos <- st_151_2_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
      load("prof.RData")
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 391,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 391,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 391,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r151.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_2 <- sigma_theta02 - ADP_b  
    
    dif_SA_2  <- transect2$sal - SA_b
    
    dif_pt_2  <-  transect2$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD 
    filled.contour(x_vals,prof,rot90(dif_SA_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_2,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_151_2.RData", dif_ADP_2, dif_SA_2, dif_pt_2,
         st_151_2_recortado)
    
  }
  
  ################# 01-JUNIO-2020
  {
    ##CARGAR Variable "prof" previamente
    
    ## CORRER SI SE HAN HECHO LOS CAMBIOS CORRESPONDIENTES
    {
      ib <- 5
      
      min <- ini1[buoy[ib]]
      max <- (ini[which(ini==ini1[buoy[ib]])+1]-1)
      
      min; max
      max - min
      
      #Temperatura 
      CT_b <- data.frame(temp = datos$CT[min:max],
                         presion = -datos$pres[min:max])
      
      #Salinidad 
      SA_b <- data.frame(sal = datos$SA[min:max],
                         presion = -datos$pres[min:max])
      
      
      #####SOLO calcular ADP
      sigma_theta_b <- gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp) 
      
      
      ### GENERAR DATA.FRAME con los datos de ADP, temperatura potencial, 
      #salinidad absoluta y la presion
      ####OJO > Convertir CT a pt
      
      sigma_theta <- data.frame(ADP = gsw_sigma0(SA = SA_b$sal, CT = CT_b$temp),
                                pt = gsw_pt_from_CT(SA = SA_b$sal, CT = CT_b$temp),
                                SA = SA_b$sal, 
                                presion = SA_b$presion)
      
      
      #RECORTAR DATA FRAME 
      st_151_3_recortado <- sigma_theta[sigma_theta$presion >= -912.04, ]
      recorte_datos <- st_151_3_recortado
      
      #VER DATOS de anomalía - también están los de sal y temp
      {
        plot(x = recorte_datos$ADP, 
             y = recorte_datos$presion, type = "l") }
      
      #AJUSTAR A 35 NIVELES CON RANGOS DE la variable "prof"
      
      ### OJOOOOOO > ¡¡¡¡ PRIMERO calcular prof con las rutinas plot.filled4 o plot_boy !!!!
      
      ##AJUSTAR LAS 3 VARIABLES A 35 DATOS EN LA PROFUNDIDAD
      ##usando un ciclo flor
      setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
      load("prof.RData")
      
      ADP <- numeric(35)  
      SA  <- numeric(35) 
      pt  <- numeric(35)
      
      for(i in 1:35){
        
        x <- prof[i]
        
        if (i == 1){
          # Para el primer índice, calcular el promedio menor a ese valor(profundidad)
          indices <- recorte_datos$presion <= x
          cat("Intervalo 1: promedio de valores <= ", x, "\n")
        } else {
          # Para los demás índices, calcular promedio entre x y x2
          x2 <- prof[i - 1]
          x3 <- prof[i]
          indices <- recorte_datos$presion >= x2 & recorte_datos$presion <= x3
          cat("Intervalo ", i, ": promedio entre ", x2, " y ", x3, "\n")
        }
        
        
        #CALCULAR PROMEDIOS PARA CADA VARIABLE REQUERIDA
        ADP[i] <- mean(recorte_datos$ADP[indices], na.rm = TRUE)
        SA[i] <- mean(recorte_datos$SA[indices], na.rm = TRUE)
        pt[i] <- mean(recorte_datos$pt[indices], na.rm = TRUE)
        
        # Imprimir los resultados de cada paso para depuración
        cat("ADP[", i, "] =", ADP[i], ", SA[", i, "] =", SA[i], ", pt[", i, "] =", pt[i], "\n\n")
      }
      
      #### INVERTIR ORDEN DE LOS DATOS
      ADP_in <- rev(ADP)
      SA_in <- rev(SA)
      pt_in <- rev(pt)
      
      
      #### HACER MATRICES DEL TAMAÑO DE LOS DATOS DEL MODELO 
      ADP_b <- matrix(ADP_in, nrow = 391,      
                      ncol = 35, byrow = TRUE)
      SA_b <- matrix(SA_in, nrow = 391,      
                     ncol = 35, byrow = TRUE)
      pt_b <- matrix(pt_in, nrow = 391,      
                     ncol = 35, byrow = TRUE)
      
    }
    
    #JALAR DATOS DEL REMOLINO 
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r151.RData")  
    
    ########### CALCULAR DIFERENCIAS
    dif_ADP_3 <- sigma_theta03 - ADP_b 
    
    dif_SA_3  <- transect3$sal - SA_b
    
    dif_pt_3  <-  transect3$temp - pt_b 
    
    
    ########### PROBAR SI FUNCIONA EL GRÁFICO 
    palGENE <- colorRampPalette(c("#006E37", "#86B07B", "#DDE7C1","#F9F7EA"))
    
    palD <- colorRampPalette((c("#040404",
                                "#183456",
                                "#9BB1DA",
                                "#FCFCFC")))
    
    #CALCULAR x_vals con plot_bou o plot_filled4
    
    #ADP
    filled.contour(x_vals,prof,rot90(dif_ADP_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    #SALINIDAD 
    filled.contour(x_vals,prof,rot90(dif_SA_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    #TEMPERATURA
    filled.contour(x_vals,prof,rot90(dif_pt_3,2),
                   xlab='distance [Km]', ylab='presion [dbar]',
                   color.palette = palD)
    
    
    ########### GUARDAR DATOS
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "dif_151_3.RData", dif_ADP_3, dif_SA_3, dif_pt_3,
         st_151_3_recortado)
    
  }
  
  
  #JUNTAR TODAS LAS VARIABLES EN UN RData
  {
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    load("dif_151_1.RData")
    load("dif_151_2.RData")
    load("dif_151_3.RData")
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datos")
    load("datos_r151.RData")
    
    #Reajustar nombres
    
    #Anomalías modelo en una sola lista
    ADP_modelo <- list(ADP_1 = sigma_theta01, 
                       ADP_2 = sigma_theta02, 
                       ADP_3 = sigma_theta03)
    
    ADP_boyas <- list(ADP_1 = st_151_1_recortado$ADP, 
                      ADP_2 = st_151_2_recortado$ADP,
                      ADP_3 = st_151_3_recortado$ADP)
    
    dif_ADP <- list(dif_1 = dif_ADP_1, 
                    dif_2 = dif_ADP_2,
                    dif_3 = dif_ADP_3)
    
    
    temp_modelo <- list(pt_1 = transect1$temp, 
                        pt_2 = transect2$temp,
                        pt_3 = transect3$temp)
    
    temp_boyas <- list(pt_1 = st_151_1_recortado$pt, 
                       pt_2 = st_151_2_recortado$pt,
                       pt_3 = st_151_3_recortado$pt)
    
    dif_temp <- list(dif_1 = dif_pt_1, 
                     dif_2 = dif_pt_2, 
                     dif_3 = dif_pt_3)
    
    
    sal_modelo <-  list(sal_1 = transect1$sal, 
                        sal_2 = transect2$sal,
                        sal_3 = transect3$sal)
    
    sal_boyas <- list(sal_1 = st_151_1_recortado$SA, 
                      sal_2 = st_151_2_recortado$SA,
                      sal_3 = st_151_3_recortado$SA)
    
    dif_sal <- list(dif_1 = dif_SA_1, 
                    dif_2 = dif_SA_2, 
                    dif_3 = dif_SA_3)
    
    
    pres_boyas <- list(p1 = st_151_1_recortado$presion, 
                       p2 = st_151_2_recortado$presion, 
                       p3 = st_151_3_recortado$presion)
    
    rm(sigma_theta01, sigma_theta02, sigma_theta03, dif_ADP_1, dif_ADP_2, dif_ADP_3,
       dif_pt_1, dif_pt_2, dif_pt_3, dif_SA_1, dif_SA_2, dif_SA_3, st_151_1_recortado, 
       st_151_2_recortado, st_151_3_recortado, transect1, transect2, transect3)
    
    ADP <- list(APD_boyas = ADP_boyas, 
                ADP_modelo = ADP_modelo, 
                dif_ADO = dif_ADP)
    
    sal <- list(sal_boyas = sal_boyas, 
                sal_modelo = sal_modelo, 
                dif_sal = dif_sal)
    
    temp <- list(temp_boyas = temp_boyas, 
                 temp_modelo = temp_modelo, 
                 dif_temp = dif_temp)
    
    rm(ADP_boyas, ADP_modelo, dif_ADP, sal_boyas, sal_modelo, dif_sal, 
       temp_boyas, temp_modelo, dif_temp)
    
    setwd("C:/Users/genef/Documents/Remolinos/perfiles/datosboy")
    
    save(file = "alldatos151.RData", ADP, sal, temp,
         pres_boyas, valores)
    
    load("alldatos151.RData")
    
  }
}


