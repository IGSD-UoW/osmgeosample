##' @title Spatially discrete sampling
##' @description Draw a spatially discrete sample from a specified set of spatial locations within a polygonal sampling region according to an \bold{"inhibitory plus close pairs"} specification.
##' @param delta minimum permissible distance between any two locations in preliminary sample. This can be allowed to vary with number of \code{'close pairs'} if a \bold{simple inhibitory} design is compared to one of the \bold{inhibitory plus close pairs} design.
##' @param delta.fix 'logical' specifies whether \code{'delta'} is fixed or allowed to vary with number of close pairs \eqn{k}. Default is \code{delta.fix = FALSE}.
##' @param k number of close-pair locations in the sample. Must be an integer between 0 and \code{size}/2.
##' @param cp.criterion criterion for choosing close pairs \eqn{k}. The \code{"cp.zeta"} criterion chooses locations not included in the initial sample, from the uniform distribution of a disk with radius \code{'zeta'} (NB: \code{zeta} argument must be provided for this criterion). The \code{"cp.neighb"} criterion chooses nearest neighbours amongst locations not included in the initial sample (\code{'zeta'} becomes trivial for \code{'cp.neighb'} criterion).
##' @param zeta maximum permissible distance (radius of a disk with center \eqn{x^{*}_{j}, j = 1, \ldots, k}) within which a close-pair point is placed. See \bold{Details}.
##' @param ntries number of rejected proposals after which the algorithm terminates.
##' @param poly 'optional', a \code{sf} or \code{sp} polygon object in which the design sits. The default is the bounding box of points given by \code{obj}.
##' @param plotit 'logical' specifying if graphical output is required. Default is \code{plotit = TRUE}.
##' @param bounding_geom a \code{sf} or \code{sp} object (with \eqn{N \geq \code{size}}) where each line corresponds to one spatial location. It should contain values of 2D coordinates, data and, optionally, covariate(s) value(s) at the locations. This argument must be provided when sampling from a \code{'discrete'} set of points, see \code{'type'} below for details.
##' @param sample_size a non-negative integer giving the total number of locations to be sampled.
##' @param plotit 'logical' specifying if graphical output is required. Default is \code{plotit = TRUE}.
##' @param plotit_leaflet 'logical' specifying if leaflet (html) graphical output is required. This is prioritised over plotit if both are selected. Default is \code{plotit_leaflet = TRUE}.
##' @param boundary categorical variable to determine whether the exact boundary provided (\code{boundary = 0}), the bounding box \code{boundary = 1}) or a buffer around the boundary \code{boundary = 2}) is used for sampling. Default is \code{boundary = 0}.
##' @param buff_dist if \code{boundary = 2}) then this value determines the size of the buffer by distance. The default is \code{buff_dist is NULL}).
##' @param buff_epsg if \code{boundary = 2}) then this value determines the local geographic grid reference so that the buffer can be calculated in meters. The default is  \code{buff_epsg = 4326}) which will use decimal degrees instead of meters. As an example, 27700 relates to the British National Grid.
##' @param join_type aas
##' @param key aas
##' @param value aas
##' @param data_return aas
##'
##' @details To draw a sample of size \eqn{n} from a population of spatial locations \eqn{X_{i} : i  = 1,\ldots,N}, with the property that the distance between any two sampled locations is at least \eqn{\delta}, the function implements the following algorithm.
##' \itemize{
##' \item{Step 1.} Draw an initial sample of size \eqn{n}  completely at random and call this \eqn{x_{i}  : i  = 1,\dots, n}.
##' \item{Step 2.} Set \eqn{i  = 1}.
##' \item{Step 3.} Calculate the smallest distance, \eqn{d_{\min}}, from \eqn{x_{i}}  to all other \eqn{x_{j}}  in the initial sample.
##' \item{Step 4.} If \eqn{d_{\min} \ge \delta}, increase \eqn{i}  by 1 and return to step 2 if \eqn{i \le n}, otherwise stop.
##' \item{Step 5.} If \eqn{d_{\min} < \delta}, draw an integer \eqn{j}  at random from \eqn{1,  2,\ldots,N}, set \eqn{x_{i}  = X_{j}}  and return to step 3.}
##'
##' Samples generated in this way exhibit  more regular spatial arrangements than would random samples of the same size. The degree of regularity achievable will be influenced by the spatial arrangement of the population \eqn{X_{i}  : i  = 1,\ldots,N}, the specified value of \eqn{\delta}  and the sample size \eqn{n}. For any given population, if \eqn{n}  and/or \eqn{\delta} is too large, a sample of the required size with the distance between any two sampled locations at least \eqn{\delta} will not be achievable; the algorithm will then find \eqn{n_{s} < n} points that can be placed for the given parameters.
##'
##' \bold{Sampling close pairs of points.}
##'
##' For some purposes, typically when using the same sample for parameter estimation and spatial prediction, it is desirable that a spatial sampling scheme include pairs of closely spaced points \eqn{x}. The function offers two ways of specifying close pairs, either as the closest available unsampled point to an existing sampled point \code{(cp.critetrion = cp.neighb)}, or as a random choice from amongst all available unsampled points within distance \eqn{zeta} of an existing sampled point \code{(cp.criterion = cp.zeta)}.
##' The algorithm proceeds as follows.
##'
##' Let \eqn{k} be the required number of close pairs.
##' \itemize{
##' \item{Step 1.} Construct a simple inhibitory design \bold{SI}\eqn{(n - k, \delta)}.
##' \item{Step 2.} Sample \eqn{k} from \eqn{x_{1}, \ldots, x_{n - k}} without replacement and call this set \eqn{x_{j} : j = 1, \ldots, k}.
##' \item{Step 3.} For each \eqn{x_{j}: j = 1, \ldots, k}, select a close pair \eqn{x_{n-k+j}} according to the specified criterion.}
##'
##' \bold{Note:} Depending on the spatial configuration of potential sampling locations and, when the selection criterion \code{cp.criterion = cp.zeta}, the specified value of \eqn{zeta}, it is possible that one or more of the selected points  \eqn{x_{j}} in Step 2 will not have an eligible ``close pair''. In this case, the algorithm will try  find an alternative \eqn{x_{j}} and report a warning if it fails to do so.
##'
##' @return a list with the following four components:
##' @return \code{unique.locs:} the number of unique sampled locations.
##' @return \code{delta:} the value of \eqn{\delta} after taking into account the number of close pairs \eqn{k}. If \code{delta.fix = TRUE}, this will be \eqn{\delta} input by the user.
##' @return \eqn{k:} the number of close pairs included in the sample (for \bold{inhibitory plus close pairs} design).
##' @return \code{sample.locs:} a \code{sf} or \code{sp} object containing the final sampled locations and any associated values.
##'
##' @note If \code{'delta'} is set to 0, a completely random sample is generated. In this case, \emph{'close pairs'} are not permitted and \code{'zeta'} becomes trivial.
##'
##' @references Chipeta  M G, Terlouw D J, Phiri K S and Diggle P J. (2016). Inhibitory geostatistical designs for spatial prediction taking account of uncertain covariance structure, \emph{Enviromentrics}, pp. 1-11.
##' @references Diggle P J. (2014). \emph{Statistical Analysis of Spatial and Spatio-Temporal Point Patterns.} 3rd ed., Boca Raton: CRC Press
##' @references Diggle P J and Lophaven S. (2006). Bayesian geostatistical design, \emph{Scandinavian Journal of Statistics} \bold{33}(1) pp. 53 - 64.
##' @references Rowlingson, B. and Diggle, P. 1993 Splancs: spatial point pattern analysis code in S-Plus. Computers and Geosciences, 19, 627-655
##'
##' @author Henry J. Crosby \email{henry.crosby@warwick.ac.uk}
##' @author Godwin Yeboah \email{godwin.yeboah@warwick.ac.uk}
##' @author J. Porto De Albuquerque \email{J.Porto@warwick.ac.uk}
##'
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



###########################################


discrete.inhibit.sample  <- function(bounding_geom = NULL, key = NULL, value = NULL,
                                     data_return = c("osm_polygons", "osm_points", "osm_multipolygons","multilines", "lines"),
                                     boundary = 0, buff_dist = 0, buff_epsg = 4326, join_type = "within", sample_size,
                                     plotit = TRUE, plotit_leaflet = TRUE, delta, delta.fix = FALSE, k = 0, cp.criterion = NULL,
                                     zeta, ntries = 10000, poly = NULL) {

  poly <- bounding_geom
  size <- sample_size

  if (is.null(key)) {
    stop("A key must be specified")
  } else {
  }
  if (boundary < 2 && !is.null(buff_dist)) {
    warning("buff_dist is defined despite not requesting a buffered boundary ('boundary' = 2). buff_dist has been ignored")
  }
  if (boundary == 0) {
    if (class(poly) == "character") {

      if (is.null(value)) {
        dat <- opq(getbb(poly)) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
      } else {
        dat <- opq(getbb(poly)) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
      }

      poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                                                                    2], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 1]))

      poly <- as.data.frame(poly)
      colnames(poly) <- c("lat", "lon")
      bounding <- poly %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      poly <- bounding
      dat_tr_ex <- trim_osmdata(dat, bounding, exclude = TRUE)  # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata(dat, bounding, exclude = FALSE)  # Returns all buildings that intersect with the specified area

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame") {

      if (is.null(value)) {
        dat <- opq(poly@bbox) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
      } else {
        dat <- opq(poly@bbox) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
      }

      dat_tr_ex <- trim_osmdata(dat, poly, exclude = TRUE)  # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata(dat, poly, exclude = FALSE)  # Returns all buildings that intersect with the specified area
      bounding <- poly
    } else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

  } else if (boundary == 1) {

    if (class(poly) == "character") {

      if (is.null(value)) {
        dat <- opq(getbb(poly)) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
      } else {
        dat <- opq(getbb(poly)) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
      }

      poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                                                                    2], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 1]))

      poly <- as.data.frame(poly)
      colnames(poly) <- c("lat", "lon")
      bounding <- poly %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")
      poly <- bounding
      dat_tr_ex <- trim_osmdata(dat, bounding, exclude = TRUE)  # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata(dat, bounding, exclude = FALSE)  # Returns all buildings that intersect with the specified area

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame") {

      if (is.null(value)) {
        dat <- opq(poly@bbox) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
      } else {
        dat <- opq(poly@bbox) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
      }

      coords <- rbind(c(poly@bbox[1, 1], poly@bbox[2, 1]), c(poly@bbox[1, 2], poly@bbox[2, 1]), c(poly@bbox[1, 2], poly@bbox[2,
                                                                                                                             2]), c(poly@bbox[1, 1], poly@bbox[2, 2]), c(poly@bbox[1, 1], poly@bbox[2, 1]))

      bounding <- as.data.frame(coords)
      colnames(bounding) <- c("lat", "lon")
      bounding <- bounding %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      dat_tr_ex <- trim_osmdata(dat, coords, exclude = TRUE)  # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata(dat, coords, exclude = FALSE)  # Returns all buildings that intersect with the specified area
      bounding <- as.data.frame(coords)
      colnames(bounding) <- c("lat", "lon")
      bounding <- bounding %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

    } else {
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
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({
          CRS.new <- CRS("+init=epsg:4326")
        })
        countries_buff <- st_transform(countries_buff, CRS.new)
        bounding <- countries_buff

        if (is.null(value)) {
          dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
        } else {
          dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
        }

      } else {
        poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                                                                      2], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2, 1]))

        poly <- as.data.frame(poly)
        colnames(poly) <- c("lat", "lon")
        bounding <- poly %>% st_as_sf(coords = c("lat", "lon"), crs = 4326) %>% summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        st_crs(bounding) = 4326
        poly <- bounding
        suppressWarnings({
          CRS.new <- CRS(paste0("+init=epsg:", buff_epsg))
        })
        poly <- st_transform(poly, CRS.new)
        countries_for_buff <- st_as_sf(poly)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({
          CRS.new <- CRS("+init=epsg:4326")
        })
        countries_buff <- st_transform(countries_buff, CRS.new)
        bounding <- countries_buff

        if (is.null(value)) {
          dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
        } else {
          dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
        }

      }

      dat_tr_ex <- trim_osmdata(dat, bounding, exclude = TRUE)  # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata(dat, bounding, exclude = FALSE)  # Returns all buildings that intersect with the specified area

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame") {
      if (buff_epsg == 4326) {
        proj4string(poly) <- CRS("+init=epsg:4326")
        countries_for_buff <- st_as_sf(poly)
        pc <- spTransform(poly, CRS("+init=epsg:3347"))
        countries_buff <- st_buffer(countries_for_buff, buff_dist)

        if (is.null(value)) {
          dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
        } else {
          dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
        }

      } else {
        suppressWarnings({
          CRS.new <- CRS(paste0("+init=epsg:", buff_epsg))
        })
        poly <- spTransform(poly, CRS.new)
        countries_for_buff <- st_as_sf(poly)
        countries_buff <- st_buffer(countries_for_buff, buff_dist)
        suppressWarnings({
          countries_buff <- as(countries_buff, "Spatial")
        })
        suppressWarnings({
          proj4string(countries_buff) <- CRS(paste0("+init=epsg:", buff_epsg, ""))
        })
        CRS.new <- CRS("+init=epsg:4326")
        countries_buff <- spTransform(countries_buff, CRS.new)
        suppressWarnings({

          if (is.null(value)) {
            dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key) %>% osmdata_sf()  ## Returns all within the bounding box
          } else {
            dat <- opq(st_bbox(countries_buff)) %>% add_osm_feature(key = key, value = value) %>% osmdata_sf()  ## Returns all within the bounding box
          }

        })
      }

      dat_tr_ex <- trim_osmdata(dat, countries_buff, exclude = TRUE)  # Returns all buildings that are fully within the specified area
      dat_tr <- trim_osmdata(dat, countries_buff, exclude = FALSE)  # Returns all buildings that intersect with the specified area
      bounding <- countries_buff
    } else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

  } else {
    stop("boundary must be 0,1,2 which respectively refer to exact, bounding box and buffer.")
  }

  if (join_type == "within") {

    if (is.null(dat_tr_ex$osm_points) &&  c("osm_points") %in% data_return) {
      data_return<- data_return[!(data_return) %in% c("osm_points")]
      print("OSM have no osm_points within the specified area")
    }
    if (is.null(dat_tr_ex$osm_lines) &&  c("osm_lines") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_lines")]
      print("OSM have no osm_lines within the specified area")
    }
    if (is.null(dat_tr_ex$osm_polygons) &&  c("osm_polygons") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_polygons")]
      print("OSM have no osm_polygons within the specified area")
    }
    if (is.null(dat_tr_ex$osm_multilines) &&  c("osm_multilines") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_multilines")]
      print("OSM have no osm_multilines within the specified area")
    }
    if (is.null(dat_tr_ex$osm_multipolygons) &&  c("osm_multipolygons") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_multipolygons")]
      print("OSM have no osm_multipolygons within the specified area")
    }

    if (length(data_return) == 1) {
      obj <- dat_tr_ex[[data_return]]
      obj<-obj[c("osm_id","geometry")]
    } else {
      obj <- dat_tr_ex[data_return]
      obj3<-data.frame(NA, NA)
      names(obj3) <- c("osm_id", "geometry")
      for (i in 1:length(obj)){
        obj2<-obj[[i]]
        obj3<-rbind(obj3,obj2[c("osm_id","geometry")])
      }
      obj<-obj3[-1,]
    }

    obj_for_sampling <- obj
    obj <- as.data.frame(obj_for_sampling[!duplicated(obj_for_sampling$osm_id), ])
    obj <- obj[-1, ]
    obj <- sf::st_as_sf(obj)

  } else if (join_type == "intersect") {

    if (is.null(dat_tr$osm_points) &&  c("osm_points") %in% data_return) {
      data_return<- data_return[!(data_return) %in% c("osm_points")]
      print("OSM have no osm_points within the specified area")
    }
    if (is.null(dat_tr$osm_lines) &&  c("osm_lines") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_lines")]
      print("OSM have no osm_lines within the specified area")
    }
    if (is.null(dat_tr$osm_polygons) &&  c("osm_polygons") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_polygons")]
      print("OSM have no osm_polygons within the specified area")
    }
    if (is.null(dat_tr$osm_multilines) &&  c("osm_multilines") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_multilines")]
      print("OSM have no osm_multilines within the specified area")
    }
    if (is.null(dat_tr$osm_multipolygons) &&  c("osm_multipolygons") %in% data_return) {
      data_return<-data_return[!(data_return) %in% c("osm_multipolygons")]
      print("OSM have no osm_multipolygons within the specified area")
    }

    if (length(data_return) == 1) {
      obj <- dat_tr[[data_return]]
      obj<-obj[c("osm_id","geometry")]
    } else {
      obj <- dat_tr[data_return]
      obj3<-data.frame(NA, NA)
      names(obj3) <- c("osm_id", "geometry")
      for (i in 1:length(obj)){
        obj2<-obj[[i]]
        obj3<-rbind(obj3,obj2[c("osm_id","geometry")])
      }
      obj<-obj3[-1,]
    }

    obj_for_sampling <- obj
    obj <- as.data.frame(obj_for_sampling[!duplicated(obj_for_sampling$osm_id), ])
    obj <- obj[-1, ]
    obj <- sf::st_as_sf(obj)

  } else {
    stop("join_type must be 'within' or 'intersect'")
  }































  obj.origin <- obj
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
    stop("\n non-numerical values in the coordinates")
  if(any(is.na(sf::st_geometry(obj)))){
    warning("\n NA's not allowed in 'obj' coordinates")
    obj <- obj[complete.cases(obj), , drop = FALSE]
    warning("\n eliminating rows with NA's")
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
    poly <- st_as_sf(poly)
  } else{
    poly <- poly
    if (!identical(st_crs(obj), st_crs(poly)))
      stop("\n 'obj' and 'poly' are not in the same coordinate system")
  }
  if(length(size) > 0){
    if(!is.numeric(size) | size <= 0)
      stop("\n 'size' must be a positive integer")
    else
      orig.size <- size
  }
  if(length(k) > 0){
    if(k > 0){
      if(!is.numeric(k) | k < 0)
        stop("\n 'k' must be a positive integer >= 0")
      if (k > size/2)
        stop("\n 'k' must be between 0 and size/2")
      if(is.null(cp.criterion))
        stop("\n Close pairs selection criterion 'cp.criterion' must be provided")
      if (cp.criterion != "cp.zeta" & cp.criterion != "cp.neighb")
        stop("\n 'cp.criterion' must be either 'cp.neighb' or 'cp.zeta'")
    }
  }
  if(length(delta) > 0){
    if(!is.numeric(delta) | delta < 0)
      stop("\n 'delta' must be a positive integer >= 0")
  }

  ################################################################################################
  if(delta == 0){
    if(k>0){
      stop("\n close pairs not allowed for completely random sample")
    } else {
      res1 <- as.data.frame(unique(st_coordinates(obj)))
      N   <- dim(res1)[1]
      index  <- 1:N
      index.sample  <- sample(index, size, replace = FALSE)
      xy.sample  <- res1[index.sample,]; dim(xy.sample) #to remove this
    }
  } else {
    delta.orig <- delta
    if (delta.fix == TRUE){
      delta = delta
    } else {
      delta <- delta * sqrt(size/(size - k));delta
    }
    dsq  <- delta*delta; dsq
    if (is.null(poly)){
      poly.shape <- sf::st_convex_hull(st_union(obj))
    } else {
      poly.shape <- poly
    }
    if(!is.infinite(size) && (size * pi * dsq/4 > as.numeric(sf::st_area(poly.shape))))
      stop("\n Polygon is too small to fit ", size, " points, with 'k' = ", k, " close pairs,",
           " at minimum separation ", round(delta, digits = 4))

    # "XnotinF" Function
    xnotiny  <- function(a1,a2)
    {
      a1.vec <- apply(a1, 1, paste, collapse = "")
      a2.vec <- apply(a2, 1, paste, collapse = "")
      a1.without.a2.rows <- as.data.frame(a1[!a1.vec %in% a2.vec,])
      return(a1.without.a2.rows)
    }

    res1 <- as.data.frame(unique(sf::st_coordinates(obj)))
    N   <- dim(res1)[1]
    index  <- 1:N
    index.sample  <- sample(index, 1, replace = FALSE)
    xy.sample  <- res1[index.sample,]
    for (i in 2:size){
      dmin  <- 0
      iter <- 1
      while (as.numeric(dmin) < dsq){
        take <- sample(index, 1)
        iter <- iter+1


        xy.sample<-sf::st_as_sf(xy.sample, coords = c("X", "Y"))
        st_crs(xy.sample) = 4326

        res1take<- sf::st_as_sf(res1[take,], coords = c("X", "Y"))
        st_crs(res1take) = 4326
        dvec<-st_distance(res1take, xy.sample, by_element = TRUE)




        #dvec<-(res1[take,1]-xy.sample[,1])^2+(res1[take,2]-xy.sample[,2])^2
        dvec<-as.data.frame(as.numeric(dvec))
        names(dvec)<-"v1"
        dmin<-min(dvec[!is.na(dvec$v1),])
        if(iter == ntries)
          break
      }
      xy.sample[i,]  <- res1take
      num <- dim(xy.sample)[1]
      if(iter == ntries && dim(xy.sample)[1] < size){
        warning("\n For the given 'delta' and 'size', only ", num,
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
    xy.sample <- unique(xy.sample)
    if(cp.criterion == "cp.neighb"){
      for (j in 1:k) {
        take1<-take[j,1]; take2<-take[j,2]
        xy1<-as.numeric(c(xy.sample[take1,]))




        xy.sample<-sf::st_as_sf(xy.sample, coords = c("X", "Y"))
        st_crs(xy.sample) = 4326

        res1take<- sf::st_as_sf(res1[take,], coords = c("X", "Y"))
        st_crs(res1take) = 4326
        dvec<-st_distance(res1take, xy.sample, by_element = TRUE)




        #dvec<-(res1[,1]-xy1[1])^2+(res1[,2]-xy1[2])^2
        neighbour<-order(dvec)
        ##################################
        res1<- sf::st_as_sf(res1, coords = c("X", "Y"))
        xy.sample[take2,]<-res1[neighbour,]
      }
    }
    if(cp.criterion == "cp.zeta"){
      if(!is.numeric(zeta) | zeta < 0)
        stop("\n 'zeta' must be between > 0 and 'delta'/2")
      if(zeta < delta.orig*0.005)
        stop("\n 'zeta' too small.")
      if(zeta > delta.orig/2)
        stop("\n 'zeta' must be between > 0 and 'delta'/2")
      for (j in 1:k){
        take1<-take[j,1]; take2<-take[j,2]
        xy1<-as.numeric(c(xy.sample[take1,]))



        xy.sample<-sf::st_as_sf(xy.sample, coords = c("X", "Y"))
        st_crs(xy.sample) = 4326

        res1take<- sf::st_as_sf(res1[take,], coords = c("X", "Y"))
        st_crs(res1take) = 4326
        dvec<-st_distance(res1take, xy.sample, by_element = TRUE)





        #dvec<-(res1[,1]-xy1[1])^2+(res1[,2]-xy1[2])^2
        z.vec <- which(as.numeric(dvec) > 0 & as.numeric(dvec) <= zeta*0.25)
        z.vec.pts <- (1:dim(res1)[1])[z.vec]
        avail.locs <- xnotiny(res1[z.vec,], xy.sample)
        if (nrow(avail.locs) > 0) {
          rep.loc <- sample(1:dim(avail.locs)[1],1,replace = F)
          xy.sample[take2,]<-avail.locs[rep.loc,]
        } else {
          warning("\n One or more locations do not have
                      eligible 'close pairs'")
          break
        }
      }
    }
  }

  xy.sample <- sf::st_as_sf(xy.sample, coords = c("X", "Y"))
  st_crs(xy.sample) <- st_crs(obj)
  xy.sample <- obj[xy.sample, ]












  if (plotit == TRUE && plotit_leaflet == FALSE) {
    par(oma = c(5, 5, 5, 5.5), mar = c(5.5, 5.1, 4.1, 2.1), mgp = c(3, 1, 0), las = 0)

    if (class(obj.origin)[1] == "sf") {
      plot(st_geometry(obj.origin), pch = 19, col = "yellow", axes = TRUE, xlab = "longitude", ylab = "lattitude",
           font.main = 3, cex.main = 1.2, col.main = "blue", main = paste("Random sampling design,", size, "points",
                                                                          sep = " "))
    } else {
      plot(obj.origin, pch = 19, col = "yellow", axes = TRUE, xlab = "longitude", ylab = "lattitude", font.main = 3,
           cex.main = 1.2, col.main = "blue", main = paste("Random sampling design,", size, "points", sep = " "))
    }
    plot(st_geometry(xy.sample), pch = 19, cex = 0.25, col = 1, add = TRUE)

  }

  if (plotit_leaflet == TRUE) {
    par(oma = c(5, 5, 5, 5.5), mar = c(5.5, 5.1, 4.1, 2.1), mgp = c(3, 1, 0), las = 0)
    st_crs(xy.sample) = 4326
    st_crs(obj.origin) = 4326

    if (class(obj.origin)[1] == "sf") {
      print(mapview((bounding), map.types = c("OpenStreetMap.DE"), layer.name = c("Boundary"), color = c("black"),
                    alpha.regions = 0.3, label = "Boundary") + mapview(st_geometry(obj.origin), add = TRUE, layer.name = c("All Locations"), label = obj.origin$osm_id) +
              mapview(st_geometry(xy.sample), add = TRUE, layer.name = c("Sample Locations"), color = c("yellow"), label = xy.sample$osm_id, lwd = 2))
    } else {
      print(mapview((bounding), map.types = c("OpenStreetMap.DE"), layer.name = c("Boundary"), color = c("black"),
                    alpha.regions = 0.3, label = "Boundary") + mapview(obj.origin, add = TRUE, layer.name = c("All Locations"), label = obj.origin$osm_id) + mapview(st_geometry(xy.sample),
                                                                                                                                                                     add = TRUE, layer.name = c("Sample Locations"), color = c("yellow") , lwd = 2, label = xy.sample$osm_id))
    }

  }


  xy.sample_df <- as.data.frame(xy.sample)
  obj.origin_df <- as.data.frame(obj.origin)
  xy.sample_df <- xy.sample_df[, !(names(xy.sample_df) %in% c("geometry"))]
  xy.sample_df <- as.data.frame(xy.sample_df)
  obj.origin_df <- obj.origin_df[, !(names(obj.origin_df) %in% c("geometry"))]
  obj.origin_df <- as.data.frame(obj.origin_df)
  xy.sample_df$inSample <- 1
  names(xy.sample_df) <- c("osm_id", "inSample")
  names(obj.origin_df) <- "osm_id"
  results <- merge(obj.origin_df, xy.sample_df, by = "osm_id", all.x = TRUE)
  # results<-results[, -grep('.y', colnames(results))]
  results[is.na(results$inSample), "inSample"] <- 0
  suppressWarnings({
    results <- cbind(results, obj.origin %>% st_centroid() %>% st_geometry())
  })
  results <- cbind(results, unlist(st_geometry(st_as_sf(results))) %>% matrix(ncol = 2, byrow = TRUE) %>% as_tibble() %>%
                     setNames(c("centroid_lon", "centroid_lat")))
  results <- results[, !(names(results) %in% c("geometry"))]
  assign("results", results, envir = .GlobalEnv)

}
