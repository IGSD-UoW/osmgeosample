##' @title Continuous Inhibitory sample
##' @description This function draws a spatially random sample from a either (1) a discrete set of OSM features defined in the function parameters or (2) a continuous surface defined by a user definted geographical region.
##' @param bounding_geom a \code{sf} or \code{sp} object (with \eqn{N \geq \code{size}}) where each line corresponds to one spatial location. It should contain values of 2D coordinates, data and, optionally, covariate(s) value(s) at the locations. This argument must be provided when sampling from a \code{'discrete'} set of points, see \code{'type'} below for details.
##' @param sample_size a non-negative integer giving the total number of locations to be sampled.
##' @param plotit 'logical' specifying if graphical output is required. Default is \code{plotit = TRUE}.
##' @param plotit_leaflet 'logical' specifying if leaflet (html) graphical output is required. This is prioritised over plotit if both are selected. Default is \code{plotit_leaflet = TRUE}.
##' @param boundary aas
##' @param buff_dist aas
##' @param buff_epsg aas
##' @param delta aas
##' @param delta.fix aas
##' @param k aas
##' @param rho aas
##' @param ntries aas
##'
##'
##' @return a \code{df} object of dimension \eqn{n} by \code{4} containing the final sampled osm_ids, centroid locations and whether the instance is in the selected sample (0/1), if sampling from a \code{'discrete'} set of points. A \code{df} object of dimension \eqn{n} by \code{3} containing the serial id and centroid locations for all sample instances,if sampling from a \code{'continuum'}.
##'
##' @examples
##' set.seed(15892)
##' xy.sample <-
##'
##' set.seed(15892)
##' xy.sample <-
##'
##' set.seed(15892)
##' xy.sample
##'
##'
##' @author Henry J. Crosby \email{henry.crosby@warwick.ac.uk}
##' @author Godwin Yeboah \email{godwin.yeboah@warwick.ac.uk}
##' @author J. Porto De Albuquerque \email{J.Porto@warwick.ac.uk}
##'
##' @references Rowlingson, B. and Diggle, P. 1993 Splancs: spatial point pattern analysis code in S-Plus. Computers and Geosciences, 19, 627-655
##'
##' @import sp
##' @import sf
##' @importFrom splancs csr
##' @import nngeo
##' @import rgdal
##' @import osmdata
##' @import processx
##' @import mapview
##' @import dplyr
##' @export



# library(nngeo) library('geoR') library(sp) library(sf) library(splancs) library(rgdal) library(osmdata)
# library(processx) library(mapview) library('dplyr')

###########################################

osm.contin.inhibit <- function(bounding_geom = NULL,boundary = 0, buff_dist = 0, buff_epsg = 4326, sample_size,
                               plotit = TRUE, plotit_leaflet = TRUE, delta, delta.fix = FALSE,k=0,rho=NULL,
                               ntries = 10000)

  {

  poly <- bounding_geom
  size <- sample_size

  if (boundary < 2 && !is.null(buff_dist)) {
    warning("buff_dist is defined despite not requesting a buffered boundary ('boundary' = 2). buff_dist has been ignored")
  }
  if (boundary == 0) {
    if (class(poly) == "character") {

      poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 1]), c(getbb(poly)[1,2], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 1]))
      poly <- as.data.frame(poly)
      colnames(poly) <- c("lat", "lon")
      poly <- poly %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame")
      {} else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

  } else if (boundary == 1) {
    if (class(poly) == "character") {

      poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 1]), c(getbb(poly)[1,2], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 1]))
      poly <- as.data.frame(poly)
      colnames(poly) <- c("lat", "lon")
      poly <- poly %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame")
    {} else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

  } else if (boundary == 2) {

    if (class(poly) == "character") {

      if (buff_epsg == 4326) {
        poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                                                                      2], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 1]))
        poly <- as.data.frame(poly)
        colnames(poly) <- c("lat", "lon")
        bounding <- poly %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        st_crs(bounding) = 4326
        poly <- bounding
        countries_for_buff <- st_as_sf(poly)
        st_transform(countries_buff, 27700)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({
          CRS.new <- CRS("+init=epsg:4326")
        })
        countries_buff <- st_transform(countries_buff, CRS.new)
        bounding <- countries_buff

      } else {
        poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 1]), c(getbb(poly)[1,2], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 1]))
        poly <- as.data.frame(poly)
        colnames(poly) <- c("lat", "lon")
        bounding <- poly %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        st_crs(bounding) = 4326
        poly <- bounding
        suppressWarnings({CRS.new <- CRS(paste0("+init=epsg:", buff_epsg))})
        poly <- st_transform(poly, CRS.new)
        countries_for_buff <- st_as_sf(poly)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({CRS.new <- CRS("+init=epsg:4326")})
        countries_buff <- st_transform(countries_buff, CRS.new)
        bounding <- countries_buff
      }
    }

    else if (class(poly) == "SpatialPolygonsDataFrame") {
      if (buff_epsg == 4326) {
        proj4string(poly) <- CRS("+init=epsg:4326")
        countries_for_buff <- st_as_sf(poly)
        pc <- spTransform(poly, CRS("+init=epsg:3347"))
        countries_buff <- st_buffer(countries_for_buff, buff_dist)

      } else {
        suppressWarnings({CRS.new <- CRS(paste0("+init=epsg:", buff_epsg))})
        poly <- spTransform(poly, CRS.new)
        countries_for_buff <- st_as_sf(poly)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({countries_buff <- as(countries_buff, "Spatial")})
        suppressWarnings({proj4string(countries_buff) <- CRS(paste0("+init=epsg:", buff_epsg, ""))})
        CRS.new <- CRS("+init=epsg:4326")
        countries_buff <- spTransform(countries_buff, CRS.new)
        suppressWarnings({bounding <- countries_buff})
      }
        } else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

      } else {
    stop("boundary must be 0,1,2 which respectively refer to exact, bounding box and buffer.")
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
       if (inherits(poly, 'Spatial')){
         poly <- sf::st_as_sf(poly)
       } else {
         poly <- poly
       }
       if (length(size) > 0){
         if (!is.numeric(size) | size <= 0)
           stop("\n 'size' must be a positive integer")
         else
           orig.size <- size
       }
       if(length(delta) > 0){
         if(!is.numeric(delta) | delta < 0)
           stop("\n 'delta' must be a positive integer >= 0")
         if(delta == 0 && k > 0)
             stop("\n Close pairs not allowed for completely
                  random sample (i.e. when 'delta' = 0)")
         if(delta == 0 && k == 0)
           rho = NULL
       }
       if(length(k) > 0){
         if(!is.numeric(k) | k < 0)
           stop("\n 'k' must be a positive integer >= 0")
         if (k > size/2)
           stop("\n 'k' must be between 0 and 'size'/2")
         if(k>0 && is.null(rho)){
             stop("\n 'rho' must be provided if 'k' > 0")
         }
         if(k>0 && rho <= 0){
           stop("\n 'rho' must be positive,
                between > 0 and 'delta'/2")
         }
       }
       if(length(rho) > 0){
         if(!is.numeric(rho) | rho < 0)
           stop("\n 'rho' must be positive")
         if(rho > delta/2)
           stop("\n 'rho' must be between > 0
                and 'delta'/2")
       }

       st.poly <- sf::st_coordinates(poly)[,c(1:2)]
       xy.sample <- as.matrix(csr(st.poly,size))
       if(delta == 0){
         for (i in 2:size) {
             xy.try <- c(csr(st.poly,1))
             xy.sample <- rbind(xy.sample, xy.try)
           }
       } else {
           if (delta.fix == TRUE){
             delta = delta
           } else {
             delta <- delta * sqrt(size/(size - k))
           }
         dsq  <- delta*delta
         if (!is.infinite(size) && (size * pi * dsq/4 > as.numeric(st_area(poly))))
           stop("\n Polygon is too small to fit ", size,
                " points, with 'k' = ", k, " close pairs,",
                " at minimum separation ", round(delta, digits = 4))
         while (dim(xy.sample)[1] < size) {
           dmin<-0
           iter <- 1
           while (dmin<dsq) {
             xy.try<-c(csr(st.poly,1))
             dmin<-min((xy.sample[,1]-xy.try[1])^2+(xy.sample[,2]-xy.try[2])^2)
             iter <- iter + 1
             if(iter == ntries)
               break
           }
           xy.sample<-rbind(xy.sample,xy.try)
           if(iter == ntries && dim(xy.sample)[1] < size){
             warning("\n For the given 'delta' and 'size', only ", dim(xy.sample)[1],
                     " inhibitory sample locations placed out of ", size,
                     ". Consider revising 'delta' and/or 'size'")
             break
           }
         }
       }
       if (k>0) {
         k.origin <- k
         size <- dim(unique(xy.sample))[1]
         reduction <- ((orig.size - size)/orig.size)
         if (k > size/2){
           k <- floor(k*(1-reduction))
           warning("\n For the given parameters, only ", k,
                   " close pairs could be placed out of ", k.origin)
         }
         take<-matrix(sample(1:size,2*k,replace=FALSE),k,2)
         for (j in 1:k) {
           take1<-take[j,1]; take2<-take[j,2]
           xy1<-c(xy.sample[take1,])
           angle<-2*pi*runif(1)
           radius<-rho*sqrt(runif(1))
           xy.sample[take2,]<-xy1+radius*c(cos(angle),sin(angle))
         }
       }
       xy.sample <- xy.sample %>%
         as.data.frame %>%
         sf::st_as_sf(coords = c(1,2))
       sample.locs <- sf::st_as_sf(xy.sample)

       xy.sample <- sample.locs

       if(plotit==TRUE){
         par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
         plot(st_geometry(xy.sample),pch=19,col=1,axes = TRUE,
              xlab="longitude",ylab="lattitude", font.main = 3,
              cex.main = 1.2, col.main = "blue",
              main = paste("Continuous sampling design,", k,
                           "close pairs", sep = " "),
              xlim = c(range(st.poly[,1])),
              ylim = c(range(st.poly[,2])))
         plot(st_geometry(poly), add= TRUE)
       }
       res <- list()
       res$size <- dim(unique(xy.sample))[1]
       res$delta = delta
       res$k <- k
       res$sample.locs = sample.locs






  if (plotit_leaflet == TRUE) {
    par(oma = c(5, 5, 5, 5.5), mar = c(5.5, 5.1, 4.1, 2.1), mgp = c(3, 1, 0), las = 0)
    st_crs(xy.sample) = 4326
    print(mapview(st_geometry(poly), add = TRUE, layer.name = c("Boundary"), color = c("black"), alpha = 0.3, label = "Boundary") + mapview(st_geometry(xy.sample),
                                                                                                                                       add = TRUE, layer.name = c("Sample Locations"), color = c("yellow"), label = xy.sample$geometry, lwd = 2))
    }
       xy.sample_coords <- xy.sample %>% st_cast("MULTIPOINT") %>% st_cast("POINT")
       xy.sample_coords <- st_coordinates(xy.sample_coords)
       xy.sample_coords <- (cbind(c(1:nrow(xy.sample_coords)), xy.sample_coords))
       colnames(xy.sample_coords) <- c("id", "lat", "long")
       assign("results", xy.sample_coords, envir = .GlobalEnv)
}

