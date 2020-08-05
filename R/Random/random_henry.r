#######################
###############
library(nngeo)
library("geoR")
library(sp)
library(sf)
library(splancs)
library(rgdal)
library(osmdata)
library(mapview)
library("dplyr")

# 1. Sampling from a discrete set of points.
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

poly <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries" , layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile

#poly<-"Failand"
boundary<- 2
buff_dist <- 0.05
join_type <- "within"
type= "discrete"
size <- 200
plotit <- TRUE
plotit_leaflet <- TRUE
key<- "building"
value = "yes"


random.sample <- function(poly = NULL, key= NULL, value = NULL, boundary = 0, buff_dist = 0, join_type = "within", type, size, plotit = TRUE, plotit_leaflet = TRUE){

  if (boundary < 2 && !is.null(buff_dist)) {
    print("warning: buff_dist is defined despite not requesting a buffered boundary ('boundary' = 2), this will be ignored")
  }
  if (boundary == 0) {
    dat <-  opq (poly@bbox) %>%
      add_osm_feature (key=key, value=value) %>%
      osmdata_sf () ## Returns all buildings within the bounding box

    dat_tr_ex <-trim_osmdata (dat, poly, exclude = TRUE) # Returns all buildings that are fully within the specified area
    dat_tr <- trim_osmdata (dat, poly, exclude = FALSE) # Returns all buildings that intersect with the specified area
    bounding <- poly
  } else if (boundary == 1) {

    dat <-  opq (poly@bbox) %>%
      add_osm_feature (key=key, value=value) %>%
      osmdata_sf () ## Returns all buildings within the bounding box

    coords <- rbind(
      c(poly@bbox[1,1],poly@bbox[2,1]),
      c(poly@bbox[1,2],poly@bbox[2,1]),
      c(poly@bbox[1,2],poly@bbox[2,2]),
      c(poly@bbox[1,1],poly@bbox[2,2]),
      c(poly@bbox[1,1],poly@bbox[2,1])
    )

    dat_tr_ex <-trim_osmdata (dat, coords, exclude = TRUE) # Returns all buildings that are fully within the specified area
    dat_tr <- trim_osmdata (dat, coords, exclude = FALSE) # Returns all buildings that intersect with the specified area
    bounding<-as.data.frame(coords)
    colnames(bounding)<-c("lat","lon")
    bounding <- bounding %>%
    st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")


  } else if (boundary == 2) {

    proj4string(poly) <- CRS("+init=epsg:4326")
    countries_for_buff <- st_as_sf(poly)
    pc <- spTransform(poly, CRS( "+init=epsg:3347" ) )

    countries_buff <- st_buffer(countries_for_buff, buff_dist)

    dat <-  opq (st_bbox(countries_buff)) %>%
      add_osm_feature (key=key, value=value) %>%
      osmdata_sf () ## Returns all buildings within the bounding box

    dat_tr_ex <-trim_osmdata (dat, countries_buff, exclude = TRUE) # Returns all buildings that are fully within the specified area
    dat_tr <- trim_osmdata (dat, countries_buff, exclude = FALSE) # Returns all buildings that intersect with the specified area
    bounding <- countries_buff
  } else {
    stop("boundary must be 0,1,2 which respectively refer to exact, bounding box and buffer.")
  }

  if (join_type == "within"){
    obj <-dat_tr$osm_polygons
    } else if (join_type == "intersect"){
      obj<-dat_tr_ex$osm_polygons
    } else {
        stop("join_type must be 'within' or 'intersect'")
      }

  if (is.null(type)){
    stop("\n 'type' must be provided")
  }
  if (type != "discrete" & type != "continuum"){
    stop("'type' must be either 'discrete' or 'continuum'")
  }
  if (type == "discrete"){
    obj.origin <- obj
    poly.origin <- poly
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
    if(class(poly.origin)[1] == "SpatialPolygonsDataFrame"){
      poly.origin<-st_as_sf(poly.origin)
    }
    if(class(xy.sample)[1] != class(poly.origin)[1]){
      res <- sf::as_Spatial(xy.sample, "Spatial")
    }
  }

  if(plotit==TRUE && plotit_leaflet == FALSE){
    par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    if (type == "discrete"){
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
    } else{
      plot(st_geometry(plot.poly),pch=19,col=1,axes = TRUE,
           xlab="longitude",ylab="lattitude", font.main = 3, cex.main = 1.2, col.main = "blue",
           main = paste("Random sampling design,", size, "points", sep = " "),
           xlim = c(range(st.poly[,1])),
           ylim = c(range(st.poly[,2])))
      plot(st_geometry(xy.sample), col="yellow", add= TRUE)
    }
  }

  if(plotit_leaflet == TRUE){
    par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
    if (type == "discrete"){
      par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
      if (class(obj.origin)[1] == "sf"){
        print(
        mapview(st_geometry(obj.origin),
              map.types = c("OpenStreetMap.DE"),
              layer.name = c("All Locations"),
              color = c("black"))+
        mapview((bounding), add= TRUE,
                layer.name = c("Boundary"),
                color = c("black"))+
        mapview(st_geometry(xy.sample), add= TRUE,
                layer.name = c("Sample Locations"),
                color=c("black")))
      } else {
        print(mapview(obj.origin,
                map.types = c("OpenStreetMap.DE"),
                layer.name = c("All Locations"),
                color = c("black"))+
          mapview((bounding), add= TRUE,
                  layer.name = c("Boundary"),
                  color = c("black"))+
          mapview(st_geometry(xy.sample), add= TRUE,
                  layer.name = c("Sample Locations"),
                  color=c("black")))
      }} else {
          print(mapview((bounding), add= TRUE,
                  layer.name = c("Boundary"),
                  color = c("black"))+
          mapview(st_geometry(xy.sample), add= TRUE,
                  layer.name = c("Sample Locations"),
                  color=c("black")))
    }
  }

  return(res)
}




random.sample(poly = poly, key= key, value = value, boundary = boundary, buff_dist = buff_dist, join_type = join_type, type = type, size = size, plotit = plotit, plotit_leaflet = plotit_leaflet)



