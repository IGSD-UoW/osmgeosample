#######################
###############
<<<<<<< HEAD
library(sp)
library(sf)
library(splancs)
library(osmdata)
=======

>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6

# 1. Sampling from a discrete set of points.
library("dplyr")
x <- 0.015+0.03*(1:33)
xall <- rep(x,33)
yall <- c(t(matrix(xall,33,33)))
xy <- cbind(xall,yall)+matrix(-0.0075+0.015*runif(33*33*2),33*33,2)
colnames(xy) <- c('X','Y')

# Convert to SF
xy <- xy %>%
  as.data.frame %>%
  sf::st_as_sf(coords = c(1,2))
xy <- sf::st_as_sf(xy, coords = c('X', 'Y'))


# Sampling from a discrete set.
set.seed(15892)
xy.sample <- random.sample(obj = xy, size = 100, type = "discrete", plotit = TRUE)


# Sampling from a continuum.
library("geoR")
data("parana")
poly <- parana$borders
poly <- matrix(c(poly[,1],poly[,2]),dim(poly)[1],2,byrow=FALSE)
# Convert matrix to polygon
poly <- st_sf(st_sfc(st_polygon(list(as.matrix(poly)))))

set.seed(15892)
xy.sample <- random.sample(poly = poly,size = 100, type = "continuum", plotit = TRUE)


###########################################

library(sp)
library(sf)
library(splancs)
<<<<<<< HEAD
library(osmdata)
library(rgdal)

poly <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries" , layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile

poly<-"Failand"
boundary<- 0
jointype <- 1
type= "discrete"
size <- 20
plotit <- TRUE
key<- "building"
value = "yes"

if (boundary == 0) {
  dat <-  opq (poly@bbox) %>%
    add_osm_feature (key=key, value=value) %>%
=======


poly <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries" , layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile
poly <- st_sf(st_sfc(st_polygon(list(as.matrix(poly)))))

boundary<- 0
jointype <- 1


if (boundary == 0) {
  dat <-  opq (poly@bbox) %>%
    add_osm_feature (key="building", value="yes") %>%
>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6
    osmdata_sf () ## Returns all buildings within the bounding box

  dat_tr_ex <-trim_osmdata (dat, poly) # Returns all buildings that are fully within the specified area
  dat_tr <- trim_osmdata (dat, poly, exclude = FALSE) # Returns all buildings that intersect with the specified area
  bounding <- poly
} else if (boundary == 1) {

  dat <-  opq (poly@bbox) %>%
<<<<<<< HEAD
    add_osm_feature (key=key, value=value) %>%
=======
    add_osm_feature (key="building", value="yes") %>%
>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6
    osmdata_sf () ## Returns all buildings within the bounding box

  coords <- rbind(
    c(poly@bbox[1,1],poly@bbox[2,1]),
    c(poly@bbox[1,2],poly@bbox[2,1]),
    c(poly@bbox[1,2],poly@bbox[2,2]),
    c(poly@bbox[1,1],poly@bbox[2,2]),
    c(poly@bbox[1,1],poly@bbox[2,1])
  )

  dat_tr_ex <-trim_osmdata (dat, coords) # Returns all buildings that are fully within the specified area
  dat_tr <- trim_osmdata (dat, coords, exclude = FALSE) # Returns all buildings that intersect with the specified area
  bounding <- poly@bbox

} else if (boundary == 2) {

  proj4string(poly) <- CRS("+init=epsg:4326")
  countries_for_buff <- st_as_sf(poly)
  pc <- spTransform(poly, CRS( "+init=epsg:3347" ) )

  countries_buff_5km <- st_buffer(countries_for_buff, 0.05)

  dat_buf <-  opq (st_bbox(countries_buff_5km)) %>%
<<<<<<< HEAD
    add_osm_feature (key=key, value=value) %>%
=======
    add_osm_feature (key="building", value="yes") %>%
>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6
    osmdata_sf () ## Returns all buildings within the bounding box

  dat_tr_ex <-trim_osmdata (dat, countries_buff_5km) # Returns all buildings that are fully within the specified area
  dat_tr <- trim_osmdata (dat, countries_buff_5km, exclude = FALSE) # Returns all buildings that intersect with the specified area
  boundary <- countries_buff_5km
}

<<<<<<< HEAD
if (jointype == 0){obj <-dat_tr$osm_polygons } else {obj<-dat_tr_ex$osm_polygons}
=======
if (jointype == 0){buildings <-dat_tr$osm_polygons } else {buildings<-dat_tr_ex$osm_polygons}
>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6


random.sample <- function(obj = NULL, poly = NULL, type, size, plotit = TRUE)

{
  if (is.null(type)){
    stop("\n 'type' must be provided")
  }
<<<<<<< HEAD
  if (type != "discrete" & type != "continuum"){
    stop("'type' must be either 'discrete' or 'continuum'")
  }
=======
  if (type != "discrete" & type != "continuum")
    stop("'type' must be either 'discrete' or 'continuum'")

>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6
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
<<<<<<< HEAD
    if(class(poly.origin)[1] == "SpatialPolygonsDataFrame"){
      poly.origin<-st_as_sf(poly.origin)
    }
=======
>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6
    if(class(xy.sample)[1] != class(poly.origin)[1]){
      res <- sf::as_Spatial(xy.sample, "Spatial")
    }
  }

  if(plotit==TRUE){
    par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    if (type == "discrete"){
<<<<<<< HEAD
      if (class(obj.origin)[1] == "sf"){
        plot(st_geometry(obj.origin), pch = 19, col = "yellow", axes = TRUE,
           xlab = "longitude", ylab = "lattitude", font.main = 3,
           cex.main = 1.2, col.main = "blue",
           main = paste("Random sampling design,", size, "points", sep = " "))
      }else{
        plot(obj.origin, pch = 19, col = "yellow", axes = TRUE,
             xlab = "longitude", ylab = "lattitude", font.main = 3,
             cex.main = 1.2, col.main = "blue",
             main = paste("Random sampling design,", size, "points", sep = " "))
        }
      plot(st_geometry(xy.sample),pch=19, cex = 0.25, col=1, add = TRUE)
=======
      plot(st_geometry(xy.sample), pch = 19, col = 1, axes = TRUE,
           xlab = "longitude", ylab = "lattitude", font.main = 3,
           cex.main = 1.2, col.main = "blue",
           main = paste("Random sampling design,", size, "points", sep = " "))
      if (class(obj.origin)[1] == "sf"){
        plot(st_geometry(obj.origin),pch=19, cex = 0.25, col="black", add = TRUE)
      }else{
        plot(obj.origin,pch=19, cex = 0.25, col="black", add = TRUE)
      }
>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6
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




<<<<<<< HEAD
random.sample(obj = obj, poly = poly, type="discrete", size, plotit = TRUE)
=======
random.sample(obj = buildings, poly = buildings, type, size, plotit = TRUE)
>>>>>>> e957ef81415f9c25b8afdfe20aec6701e2f445b6


