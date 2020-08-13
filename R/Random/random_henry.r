##########################################

library(nngeo)
library("geoR")
library(sp)
library(sf)
library(splancs)
library(rgdal)
library(osmdata)
library(mapview)
library("dplyr")

###########################################

random.sample <- function(poly = NULL, key= NULL, value = NULL, data_return = c("osm_polygons", "osm_points", "osm_multipolygons","multilines","lines"), boundary = 0, buff_dist = 0, buff_epsg = 4326, join_type = "within", type, size, plotit = TRUE, plotit_leaflet = TRUE){


  if (boundary < 2 && !is.null(buff_dist)) {
    warning("buff_dist is defined despite not requesting a buffered boundary ('boundary' = 2) - buff_dist ignored")
  }
  if (boundary == 0) {
    if (class(poly)=="character") {
      dat <-  opq (getbb(poly)) %>%
        add_osm_feature (key=key, value=value) %>%
        osmdata_sf () ## Returns all buildings within the bounding box

      poly <- rbind(
        c(getbb(poly)[1,1],getbb(poly)[2,1]),
        c(getbb(poly)[1,2],getbb(poly)[2,1]),
        c(getbb(poly)[1,2],getbb(poly)[2,2]),
        c(getbb(poly)[1,1],getbb(poly)[2,2]),
        c(getbb(poly)[1,1],getbb(poly)[2,1])
      )

      poly<-as.data.frame(poly)
      colnames(poly)<-c("lat","lon")
      bounding <- poly %>%
        st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      poly<-bounding
      dat_tr_ex <-trim_osmdata (dat, bounding, exclude = TRUE) # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata (dat, bounding, exclude = FALSE) # Returns all buildings that intersect with the specified area

      warning("the bounding box is used when poly is of type 'character'")

          } else if (class(poly) == "SpatialPolygonsDataFrame") {
      dat <-  opq (poly@bbox) %>%
        add_osm_feature (key=key, value=value) %>%
        osmdata_sf () ## Returns all buildings within the bounding box
        dat_tr_ex <-trim_osmdata (dat, poly, exclude = TRUE) # Returns all buildings that are fully within the specified area
        dat_tr <- trim_osmdata (dat, poly, exclude = FALSE) # Returns all buildings that intersect with the specified area
        bounding <- poly
    } else {warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")}

  } else if (boundary == 1) {

    if (class(poly)=="character") {
      dat <-  opq (getbb(poly)) %>%
        add_osm_feature (key=key, value=value) %>%
        osmdata_sf () ## Returns all buildings within the bounding box

      poly <- rbind(
        c(getbb(poly)[1,1],getbb(poly)[2,1]),
        c(getbb(poly)[1,2],getbb(poly)[2,1]),
        c(getbb(poly)[1,2],getbb(poly)[2,2]),
        c(getbb(poly)[1,1],getbb(poly)[2,2]),
        c(getbb(poly)[1,1],getbb(poly)[2,1])
      )

      poly<-as.data.frame(poly)
      colnames(poly)<-c("lat","lon")
      bounding <- poly %>%
        st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      poly<-bounding
      dat_tr_ex <-trim_osmdata (dat, bounding, exclude = TRUE) # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata (dat, bounding, exclude = FALSE) # Returns all buildings that intersect with the specified area

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame") {
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

      bounding<-as.data.frame(coords)
      colnames(bounding)<-c("lat","lon")
      bounding <- bounding %>%
        st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      dat_tr_ex <-trim_osmdata (dat, coords, exclude = TRUE) # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata (dat, coords, exclude = FALSE) # Returns all buildings that intersect with the specified area
      bounding<-as.data.frame(coords)
      colnames(bounding)<-c("lat","lon")
      bounding <- bounding %>%
        st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

    } else {warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")}

  } else if (boundary == 2) {


    if (class(poly)=="character") {

      if (buff_epsg == 4326) {
        poly <- rbind(
          c(getbb(poly)[1,1],getbb(poly)[2,1]),
          c(getbb(poly)[1,2],getbb(poly)[2,1]),
          c(getbb(poly)[1,2],getbb(poly)[2,2]),
          c(getbb(poly)[1,1],getbb(poly)[2,2]),
          c(getbb(poly)[1,1],getbb(poly)[2,1])
        )

        poly<-as.data.frame(poly)
        colnames(poly)<-c("lat","lon")
        bounding <- poly %>%
          st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        st_crs(bounding) = 4326
        poly<-bounding
        countries_for_buff <- st_as_sf(poly)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({CRS.new <- CRS("+init=epsg:4326")})
        countries_buff <- st_transform(countries_buff, CRS.new)
        bounding<-countries_buff
        dat <-  opq (st_bbox(countries_buff)) %>%
          add_osm_feature (key=key, value=value) %>%
          osmdata_sf () ## Returns all buildings within the buffer
      } else {
        poly <- rbind(
          c(getbb(poly)[1,1],getbb(poly)[2,1]),
          c(getbb(poly)[1,2],getbb(poly)[2,1]),
          c(getbb(poly)[1,2],getbb(poly)[2,2]),
          c(getbb(poly)[1,1],getbb(poly)[2,2]),
          c(getbb(poly)[1,1],getbb(poly)[2,1])
        )

        poly<-as.data.frame(poly)
        colnames(poly)<-c("lat","lon")
        bounding <- poly %>%
          st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        st_crs(bounding) = 4326
        poly<-bounding
        suppressWarnings({CRS.new <- CRS(paste0("+init=epsg:", buff_epsg))})
        poly <- st_transform(poly, CRS.new)
        countries_for_buff <- st_as_sf(poly)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({CRS.new <- CRS("+init=epsg:4326")})
        countries_buff <- st_transform(countries_buff, CRS.new)
        bounding<-countries_buff
        dat <-  opq (st_bbox(countries_buff)) %>%
          add_osm_feature (key=key, value=value) %>%
          osmdata_sf () ## Returns all buildings within the buffer
      }

      dat_tr_ex <-trim_osmdata (dat, bounding, exclude = TRUE) # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata (dat, bounding, exclude = FALSE) # Returns all buildings that intersect with the specified area

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame") {
      if (buff_epsg == 4326) {
        proj4string(poly) <- CRS("+init=epsg:4326")
        countries_for_buff <- st_as_sf(poly)
        pc <- spTransform(poly, CRS( "+init=epsg:3347" ) )
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        dat <-  opq (st_bbox(countries_buff)) %>%
          add_osm_feature (key=key, value=value) %>%
          osmdata_sf () ## Returns all buildings within the buffer
      } else {
        suppressWarnings({CRS.new <- CRS(paste0("+init=epsg:", buff_epsg))})
        poly <- spTransform(poly, CRS.new)
        countries_for_buff <- st_as_sf(poly)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({countries_buff<-as(countries_buff, 'Spatial')})
        suppressWarnings({proj4string(countries_buff) <- CRS(paste0("+init=epsg:",buff_epsg,""))})
        CRS.new <- CRS("+init=epsg:4326")
        countries_buff <- spTransform(countries_buff,CRS.new)
        suppressWarnings({dat <-  opq (st_bbox(countries_buff)) %>%
          add_osm_feature (key=key, value=value) %>%
          osmdata_sf ()}) ## Returns all buildings within the buffer
      }

      dat_tr_ex <-trim_osmdata (dat, countries_buff, exclude = TRUE) # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata (dat, countries_buff, exclude = FALSE) # Returns all buildings that intersect with the specified area
      bounding <- countries_buff
    } else {warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")}

  } else {
    stop("boundary must be 0,1,2 which respectively refer to exact, bounding box and buffer.")
  }

  if (join_type == "within"){
    obj <-dat_tr_ex[[data_return]]
    obj_for_sampling <- data.frame(NA, NA)
    names(obj_for_sampling)<-c("osm_id","geometry")

    for (i in 1:length(data_return)) {
      if (nrow(as.data.frame(obj[i]))==0){} else {osmid<-as.data.frame(obj[i])[(colnames(obj[i]) %in%  c("osm_id", "geometry"))]}
    obj_for_sampling<-rbind(obj_for_sampling, osmid)
    }
    obj<-as.data.frame(obj_for_sampling[!duplicated(obj_for_sampling$osm_id), ])
    obj<-obj[-1,]
    obj <- sf::st_as_sf(obj)

    } else if (join_type == "intersect"){
      obj<-dat_tr[data_return]
      obj_for_sampling <- data.frame(NA, ncol=2)
      names(obj_for_sampling)<-c("osm_id","geometry")

      for (i in 1:length(data_return)) {
        if (nrow(as.data.frame(obj[i]))==0){} else {osmid<-as.data.frame(obj[[i]])[(colnames(obj[[i]]) %in%  c("osm_id", "geometry"))]}
        obj_for_sampling<-rbind(obj_for_sampling, osmid)
      }
      obj<-as.data.frame(obj_for_sampling[!duplicated(obj_for_sampling), ])
      obj<-obj[-1,]
      obj <- sf::st_as_sf(obj)

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
    #if (any(!is.numeric(sf::st_coordinates(obj))))
    #  stop("\n non-numerical values in 'obj' coordinates")
    #if(any(is.na(sf::st_coordinates(obj)))){
    #  warning("\n NA's not allowed in 'obj' coordinates")
    #  obj <- obj[complete.cases(st_coordinates(obj)), , drop = FALSE]
    #  warning("\n eliminating rows with NA's")
    #}
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
      if (class(obj.origin)[1] == "sf"){
        st_crs(obj.origin) = 4326
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

  if (type=="discrete"){
    xy.sample_df<-as.data.frame(xy.sample)
    obj.origin_df<-as.data.frame(obj.origin)
    xy.sample_df <-xy.sample_df[ , !(names(xy.sample_df) %in%  c("geometry"))]
    xy.sample_df<-as.data.frame(xy.sample_df)
    obj.origin_df <-obj.origin_df[ , !(names(obj.origin_df) %in%  c("geometry"))]
    obj.origin_df<-as.data.frame(obj.origin_df)
    xy.sample_df$inSample <- 1
    names(xy.sample_df)<-c("osm_id","inSample")
    names(obj.origin_df)<-"osm_id"
    results<-merge(obj.origin_df,xy.sample_df, by="osm_id",all.x=TRUE)
    #results<-results[, -grep(".y", colnames(results))]
    results[is.na(results$inSample),"inSample"]<- 0

    assign ("results", results,  envir = .GlobalEnv)
  } else {
    xy.sample_coords <- xy.sample %>%
      st_cast("MULTIPOINT") %>%
      st_cast("POINT")
    xy.sample_coords<-st_coordinates(xy.sample_coords)
    xy.sample_coords<-(cbind(c(1:nrow(xy.sample_coords)), xy.sample_coords))
    colnames(xy.sample_coords)<-c("id","lat","long")
    assign ("results", xy.sample_coords,  envir = .GlobalEnv)
      }
  }


###################### testing the function #########################################

#poly <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries", layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile
poly<-"Coventry, UK"
boundary<- 2
buff_dist <- 1000
#buff_epsg <- 3857
buff_epsg <- 27700
join_type <- "intersect"
type <- "discrete"
size <- 500
plotit <- TRUE
plotit_leaflet <- TRUE
key<- "building"
value <- "yes"
#data_return <- c("osm_polygons", "osm_points", "osm_multipolygons","multilines","lines")
data_return <- c("osm_polygons")

random.sample(poly = poly, key= key, value = value, boundary = boundary, buff_dist = buff_dist, buff_epsg = buff_epsg, join_type = join_type, type = type, size = size, plotit = plotit, plotit_leaflet = plotit_leaflet, data_return=data_return)

### xy.sample is still not working
### I need to see the output, it is not bringing back the way point etc.
#### I need to add some warnings/errors that say that the data_returns that you are requesting are empty/null.
######### Note for users: find the epsg using the epsg database at http://epsg.io/map#srs=4326&x=-2.686930&y=51.441757&z=14&layer=streets

mapview(obj$geometry)

