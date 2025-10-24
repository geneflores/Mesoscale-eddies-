
filled.contour4 <- function (x = seq(0, 1, length.out = nrow(z)),
                             y = seq(0, 1, length.out = ncol(z)), 
                             z, 
                             xlim = range(x, finite = TRUE), 
                             ylim = range(y, finite = TRUE), 
                             zlim = range(z, finite = TRUE), 
                             levels = pretty(zlim, nlevels), 
                             nlevels = 20, 
                             color.palette = cm.colors, 
                             col = color.palette(length(levels) - 1), 
                             plot.title, 
                             plot.axes, 
                             key.title, 
                             key.axes, 
                             asp = NA, 
                             xaxs = "i", 
                             yaxs = "i", 
                             las = 1, 
                             axes = TRUE, 
                             frame.plot = axes,
                             mar = c(5, 4, 4, 5) + 0.1, 
                             ...) 
{
  # Si no se proporciona 'z', intentamos extraerlo de 'x' si es una lista
  if (missing(z)) {
    if (!missing(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      } else {
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
      }
    } else stop("no 'z' matrix specified")
  } else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  
  # Verificar si los valores de x e y están ordenados en forma ascendente
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  
  # Definir márgenes y parámetros gráficos
  #mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  #on.exit(par(par.orig))
  #par(las = las, mar = mar)
  
  # Crear una nueva ventana de gráficos
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  
  # Verificar la matriz 'z' (debe tener al menos dos filas y columnas)
  if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
    stop("no proper 'z' matrix specified")
  
  # Asegurarse de que z sea de tipo double
  if (!is.double(z)) 
    storage.mode(z) <- "double"
  
  # Dibujar el contorno relleno
  .filled.contour(as.double(x), as.double(y), z, as.double(levels), col = col)
  
  # Dibujar los ejes si no se pasan personalizados
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  } else {
    plot.axes
  }
  
  # Dibujar el marco si está habilitado
  if (frame.plot) 
    box()
  
  # Agregar el título del gráfico
  if (missing(plot.title)) 
    title(...)
  else plot.title
  
  invisible()
}
