library(sp)
library(sf)
library(splancs)


random.sample <- function(obj = NULL, poly = NULL, type, size, plotit = TRUE)

{
  if (is.null(type)){
    stop("\n 'type' must be provided")
  }
  if (type != "discrete" & type != "continuum")
    stop("'type' must be either 'discrete' or 'continuum'")

  if (type == "discrete"){
    obj.origin <- obj
  if (is.null(obj))
    stop("\n'obj' must be provided")
  if(!inherits(obj, 'SpatialPointsDataFrame')){
    if(!inherits(obj, 'SpatialPoints')){
      if(!inherits(obj,"sf") & !inherits(obj, "data.frame")){
        stop("\n 'obj' must be of class 'sp' or 'sf'")
        }
      }
    }
    if(inherits(obj, 'Spatial')){
      obj <- sf::st_as_sf(obj)
    }
    if (any(!is.numeric(sf::st_coordinates(obj))))
      stop("\n non-numerical values in 'obj' coordinates")
    if(any(is.na(sf::st_coordinates(obj)))){
      warning("\n NA's not allowed in 'obj' coordinates")
      obj <- obj[complete.cases(st_coordinates(obj)), , drop = FALSE]
      warning("\n eliminating rows with NA's")
    }
    if (is.null(poly)){
      poly <- sf::st_convex_hull(sf::st_union(obj))
    }
    if (length(size) > 0){
      if (!is.numeric(size) | size <= 0)
        stop("\n 'size' must be a positive integer")
    }
    if (size >= dim(obj)[1])
      stop("\n 'size' must be less than the total
           number of locations to sample from")
    if(size == 1){
      xy.sample <- obj[sample(1:dim(obj)[1], size, replace = FALSE), ]
      xy.sample <- xy.sample[which(!duplicated(xy.sample$geometry)), ]
    } else {
      ctr <- 1
      while(ctr < size){
        xy.sample <- obj[sample(1:dim(obj)[1], size, replace = FALSE), ]
        xy.sample <- xy.sample[which(!duplicated(xy.sample$geometry)), ]
        ctr <- dim(xy.sample)[1]
      }
    }
    res <- xy.sample
    if(class(xy.sample)[1] != class(obj.origin)[1]){
      res <- sf::as_Spatial(xy.sample, "Spatial")
    }
  }


  if (type == "continuum") {
    if (is.null(poly)){
      stop("\n Provide polygon in which to generate sample points")
    }
    if(!is.null(poly)){
      poly.origin <- poly
      if(!inherits(poly, 'SpatialPolygonsDataFrame'))
      if(!inherits(poly, 'SpatialPolygons'))
      if(!inherits(poly, 'Polygons'))
      if(!inherits(poly, 'Polygon'))
      if(!inherits(poly, 'sfc_POLYGON'))
      if(!inherits(poly, 'sfc'))
      if(!inherits(poly, 'sf'))
        stop("\n 'poly' must be of class 'sp' or 'sf'")
    }
    if(inherits(poly, 'Spatial')){
      plot.poly <- sf::st_as_sf(poly)
    } else {
      plot.poly <- poly
    }

    st.poly <- sf::st_coordinates(plot.poly)[,c(1:2)]
    xy.sample <- matrix(csr(st.poly,1),1,2)
    for (i in 2:size) {
      xy.try <- c(csr(st.poly,1))
      xy.sample <- rbind(xy.sample, xy.try)
    }
    xy.sample <- xy.sample %>%
      as.data.frame %>%
      sf::st_as_sf(coords = c(1,2))
    res <- xy.sample <- sf::st_as_sf(xy.sample)
    if(class(xy.sample)[1] != class(poly.origin)[1]){
      res <- sf::as_Spatial(xy.sample, "Spatial")
    }
  }

  if(plotit==TRUE){
    par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    if (type == "discrete"){
      plot(st_geometry(xy.sample), pch = 19, col = 1, axes = TRUE,
           xlab = "longitude", ylab = "lattitude", font.main = 3,
           cex.main = 1.2, col.main = "blue",
           main = paste("Random sampling design,", size, "points", sep = " "))
      if (class(obj.origin)[1] == "sf"){
        plot(st_geometry(obj.origin),pch=19, cex = 0.25, col="black", add = TRUE)
      }else{
        plot(obj.origin,pch=19, cex = 0.25, col="black", add = TRUE)
      }
    } else{
      plot(st_geometry(xy.sample),pch=19,col=1,axes = TRUE,
           xlab="longitude",ylab="lattitude", font.main = 3, cex.main = 1.2, col.main = "blue",
           main = paste("Random sampling design,", size, "points", sep = " "),
           xlim = c(range(st.poly[,1])),
           ylim = c(range(st.poly[,2])))
      plot(st_geometry(plot.poly), add= TRUE)
    }
  }

  return(res)
}




