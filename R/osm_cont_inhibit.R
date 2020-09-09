##'@title OSM Continuous Inhibitory sample
##'@description Draws a spatially continous sample of locations within a
##'  polygonal sampling region according to an \bold{'inhibitory plus close
##'  pairs'} specification. The region can be defined using OSM data or a user
##'  defined polygon.
##'@param bounding_geom a \code{sf} or \code{sp} object (with \eqn{N \geq
##'  \code{size}}) where each line corresponds to one spatial location. It
##'  should contain values of 2D coordinates, data and, optionally, covariate(s)
##'  value(s) at the locations. This argument must be provided when sampling
##'  from a \code{'discrete'} set of points, see \code{'type'} below for
##'  details.
##'@param sample_size a non-negative integer giving the total number of
##'  locations to be sampled.
##'@param plotit 'logical' specifying if graphical output is required. Default
##'  is \code{plotit = TRUE}.
##'@param plotit_leaflet 'logical' specifying if leaflet (html) graphical output
##'  is required. This is prioritised over plotit if both are selected. Default
##'  is \code{plotit_leaflet = TRUE}.
##'@param boundary categorical variable to determine whether the exact boundary
##'  provided (\code{boundary = 0}), the bounding box \code{boundary = 1}) or a
##'  buffer around the boundary \code{boundary = 2}) is used for sampling.
##'  Default is \code{boundary = 0}.
##'@param buff_dist if \code{boundary = 2}) then this value determines the size
##'  of the buffer by distance. The default is \code{buff_dist is NULL}).
##'@param buff_epsg if \code{boundary = 2}) then this value determines the local
##'  geographic grid reference so that the buffer can be calculated in meters.
##'  The default is  \code{buff_epsg = 4326}) which will use decimal degrees
##'  instead of meters. As an example, 27700 relates to the British National
##'  Grid.
##'@param delta minimum permissible distance between any two locations in
##'  preliminary sample. This can be allowed to vary with the number of
##'  \code{'close pairs'} if a \bold{simple inhibitory} design is compared to
##'  one of the \bold{inhibitory plus close pairs} design.
##'@param delta.fix 'logical' specifies whether \code{delta} is fixed or allowed
##'  to vary with number of close pairs \eqn{k}. Default is \code{delta.fix =
##'  FALSE}.
##'@param k number of locations in preliminary sample to be replaced by near
##'  neighbours of other preliminary sample locations to form \code{close pairs}
##'  (integer between 0 and \code{size/2}). A \bold{simple inhibitory} deisgn is
##'  generated when \eqn{k = 0}.
##'@param rho maximum distance between the two locations in a
##'  \code{'close-pair'}.
##'@param ntries number of rejected proposals after which the algorithm will
##'  terminate.
##'
##'@details  To draw a simple inhibitory (\bold{SI}) sample of size \code{n}
##'  from a spatially continuous region \eqn{A}, with the property that the
##'  distance between any two sampled locations is at least \code{delta}, the
##'  following algorithm is used. \itemize{ \item{Step 1.} Set \eqn{i  = 1} and
##'  generate a point \eqn{x_{1}}  uniformly distributed on \eqn{{\cal D}}.
##'  \item{Step 2.} Generate a point \eqn{x}  uniformly distributed on
##'  \eqn{{\cal D}} and calculate the minimum, \eqn{d_{\min}}, of the distances
##'  from \eqn{x_{i}} to all \eqn{x_{j}: j \leq i }. \item{Step 3.} If
##'  \eqn{d_{\min} \ge \delta}, increase \eqn{i}  by 1, set \eqn{x_{i} = x} and
##'  return to step 2 if \eqn{i \le n}, otherwise stop; \item{Step 4.} If
##'  \eqn{d_{\min} < \delta}, return to step 2 without increasing \eqn{i}. }
##'
##'  \bold{Sampling close pairs of points.}
##'
##'  For some purposes, it is desirable that a spatial sampling scheme include
##'  pairs of closely spaced points, resulting in an inhibitory plus close pairs
##'  (\bold{ICP}) design. In this case, the above algorithm requires the
##'  following additional steps to be taken. Let \eqn{k}  be the required number
##'  of close pairs. Choose a value \code{rho}  such that a close pair  of
##'  points will be a pair of points separated by a distance of at most
##'  \code{rho}. \itemize{ \item{Step 5.} Set \eqn{j  = 1} and draw a random
##'  sample of size 2 from integers \eqn{1, 2, \ldots, n}, say \eqn{(i_1, i_2)};
##'  \item{Step 6.} Replace \eqn{x_{i_{1}}} by \eqn{x_{i_{2}} + u} , where
##'  \eqn{u}  is uniformly distributed on the disc with centre \eqn{x_{i_{2}}}
##'  and radius \code{rho}, increase \eqn{i} by 1 and return to step 5 if \eqn{i
##'  \le k}, otherwise stop. }
##'
##'  When comparing a \bold{SI} design to one of the \bold{ICP} designs, the
##'  inhibitory components should have the same degree of spatial regularity.
##'  This requires \eqn{\delta} to become a function of \eqn{k} namely
##'  \deqn{\delta_{k} = \delta_{0}\sqrt{n/(n - k)}} with \eqn{\delta_{0}} held
##'  fixed.
##'
##'@return a list with the following four components:
##'@return \code{size:} the total number of sampled locations.
##'@return \code{delta:} the value of \eqn{\delta} after taking into account the
##'  number of close pairs \eqn{k}. If \code{delta.fix = TRUE}, this will be
##'  \eqn{\delta} input by the user.
##'@return \eqn{k:} the number of close pairs included in the sample (for
##'  \bold{inhibitory plus close pairs} design).
##'@return \code{sample.locs:} a \code{sf} or \code{sp} object containing
##'  coordinates of dimension \code{n} by 2 containing the sampled locations.
##'
##'@note If \code{'delta'} is set to 0, a completely random sample is generated.
##'  In this case, \code{'close pairs'} are not permitted and \code{rho} is
##'  irrelevant.
##'
##'@seealso \code{\link[osmgeosample:osm.random.sample]{osm.random.sample}} and
##'  osm.discrete.inhibit.sample
##'
##' @examples
##' library(sp)
##'bounding_geom<-
##'SpatialPolygonsDataFrame(
##'    SpatialPolygons(list(Polygons(list(Polygon(
##'        cbind(
##'            c(3.888959,3.888744,3.888585,3.888355,3.887893,3.887504,3.886955,3.886565,3.886303,3.886159,3.885650,3.885650,3.885595,3.885404,3.885444,3.885897,3.886692,3.887241,3.888068,3.888323,3.888697,3.889150,3.889548,3.889890,3.890184,3.890828,3.891258,3.891807,3.892061,3.892292,3.892689,3.893294,3.893008,3.893676,3.888959),
##'            c(7.379483,7.379785,7.380024,7.380294,7.380629,7.380986,7.381448,7.381861,7.382243,7.382474,7.383277,7.383468,7.383890,7.384263,7.384669,7.385258,7.385313,7.385194,7.384868,7.384900,7.385051,7.385067,7.384955,7.384749,7.384526,7.384120,7.384009,7.384080,7.384430,7.384478,7.384629,7.384772,7.383269,7.380963,7.379483)))), ID=1))),
##'    data.frame( ID=1))
##'proj4string(bounding_geom) <- CRS('+proj=longlat +datum=WGS84')
##'
##'set.seed(15892)
##'osm.contin.inhibit(bounding_geom = bounding_geom, boundary = 0, buff_dist=NULL,
##'buff_epsg = NULL, sample_size = 50, plotit = TRUE, plotit_leaflet = TRUE,
##'                   delta=50, delta.fix = FALSE,k=7,rho=1, ntries = 10)
##'
##'@author Henry J. Crosby \email{henry.crosby@warwick.ac.uk}
##'@author Godwin Yeboah \email{godwin.yeboah@warwick.ac.uk}
##'@author J. Porto De Albuquerque \email{J.Porto@warwick.ac.uk}
##'
##'@references Rowlingson, B. and Diggle, P. 1993 Splancs: spatial point pattern
##'  analysis code in S-Plus. Computers and Geosciences, 19, 627-655 Chipeta  M
##'  G, Terlouw D J, Phiri K S and Diggle P J. (2016b). Inhibitory
##'  geostatistical designs for spatial prediction taking account of uncertain
##'  covariance structure, \emph{Enviromentrics}, pp. 1-11.
##'  https://wiki.openstreetmap.org/wiki/Map_Features
##'@import sp
##'@import sf
##'@importFrom splancs csr
##'@import nngeo
##'@import rgdal
##'@import osmdata
##'@import processx
##'@import mapview
##'@import dplyr
##'@export




###########################################

osm.contin.inhibit <- function(bounding_geom = NULL, boundary = 0, buff_dist = 0,
                               buff_epsg = 4326, sample_size, plotit = TRUE, plotit_leaflet = TRUE, delta,
                               delta.fix = FALSE, k = 0, rho = NULL, ntries = 10000) {

  poly <- bounding_geom
  size <- sample_size + 1

  if (boundary < 2 && !is.null(buff_dist)) {
    warning("buff_dist is defined despite not requesting a buffered boundary ('boundary' = 2). buff_dist has been ignored")
  }
  if (boundary == 0) {
    if (class(poly) == "character") {

      poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                           2], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 2]),
                    c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2,
                                                                                              1]))
      poly <- as.data.frame(poly)
      colnames(poly) <- c("lat", "lon")
      poly <- poly %>%
        st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame") {
    } else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

  } else if (boundary == 1) {
    if (class(poly) == "character") {

      poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                           2], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 2]),
                    c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1], getbb(poly)[2,
                                                                                              1]))
      poly <- as.data.frame(poly)
      colnames(poly) <- c("lat", "lon")
      poly <- poly %>%
        st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON")

      warning("the bounding box is used when poly is of type 'character'")

    } else if (class(poly) == "SpatialPolygonsDataFrame") {
    } else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

  } else if (boundary == 2) {

    if (class(poly) == "character") {

      if (buff_epsg == 4326) {
        poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                             2], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 2]),
                      c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1],
                                                                 getbb(poly)[2, 1]))
        poly <- as.data.frame(poly)
        colnames(poly) <- c("lat", "lon")
        bounding <- poly %>%
          st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        st_crs(bounding) <- 4326
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
        poly <- rbind(c(getbb(poly)[1, 1], getbb(poly)[2, 1]), c(getbb(poly)[1,
                                                                             2], getbb(poly)[2, 1]), c(getbb(poly)[1, 2], getbb(poly)[2, 2]),
                      c(getbb(poly)[1, 1], getbb(poly)[2, 2]), c(getbb(poly)[1, 1],
                                                                 getbb(poly)[2, 1]))
        poly <- as.data.frame(poly)
        colnames(poly) <- c("lat", "lon")
        bounding <- poly %>%
          st_as_sf(coords = c("lat", "lon"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        st_crs(bounding) <- 4326
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
      }
    } else if (class(poly) == "SpatialPolygonsDataFrame") {
      if (buff_epsg == 4326) {
        proj4string(poly) <- CRS("+init=epsg:4326")
        countries_for_buff <- st_as_sf(poly)
        pc <- spTransform(poly, CRS("+init=epsg:3347"))
        countries_buff <- st_buffer(countries_for_buff, buff_dist)

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
          proj4string(countries_buff) <- CRS(paste0("+init=epsg:", buff_epsg,
                                                    ""))
        })
        CRS.new <- CRS("+init=epsg:4326")
        countries_buff <- spTransform(countries_buff, CRS.new)
        suppressWarnings({
          bounding <- countries_buff
        })
      }
    } else {
      warning("poly must be of type 'character' or 'SpatialPolygonsDataFrame'")
    }

  } else {
    stop("boundary must be 0,1,2 which respectively refer to exact, bounding box and buffer.")
  }


  if (!is.null(poly)) {
    poly.origin <- poly
    if (!inherits(poly, "SpatialPolygonsDataFrame"))
      if (!inherits(poly, "SpatialPolygons"))
        if (!inherits(poly, "Polygons"))
          if (!inherits(poly, "Polygon"))
            if (!inherits(poly, "sfc_POLYGON"))
              if (!inherits(poly, "sfc"))
                if (!inherits(poly, "sf"))
                  stop("\n 'poly' must be of class 'sp' or 'sf'")
  }
  if (inherits(poly, "Spatial")) {
    poly <- sf::st_as_sf(poly)
  } else {
    poly <- poly
  }
  if (length(size) > 0) {
    if (!is.numeric(size) | size <= 0)
      stop("\n 'size' must be a positive integer") else orig.size <- size
  }
  if (length(delta) > 0) {
    if (!is.numeric(delta) | delta < 0)
      stop("\n 'delta' must be a positive integer >= 0")
    if (delta == 0 && k > 0)
      stop("\n Close pairs not allowed for completely
                  random sample (i.e. when 'delta' = 0)")
    if (delta == 0 && k == 0)
      rho <- NULL
  }
  if (length(k) > 0) {
    if (!is.numeric(k) | k < 0)
      stop("\n 'k' must be a positive integer >= 0")
    if (k > size/2)
      stop("\n 'k' must be between 0 and 'size'/2")
    if (k > 0 && is.null(rho)) {
      stop("\n 'rho' must be provided if 'k' > 0")
    }
    if (k > 0 && rho <= 0) {
      stop("\n 'rho' must be positive,
                between > 0 and 'delta'/2")
    }
  }
  if (length(rho) > 0) {
    if (!is.numeric(rho) | rho < 0)
      stop("\n 'rho' must be positive")
    if (rho > delta/2)
      stop("\n 'rho' must be between > 0
                and 'delta'/2")
  }

  st.poly <- sf::st_coordinates(poly)[, c(1:2)]
  xy.sample <- as.matrix(csr(st.poly, size))
  if (delta == 0) {
    for (i in 2:size) {
      xy.try <- c(csr(st.poly, 1))
      xy.sample <- rbind(xy.sample, xy.try)
    }
  } else {
    if (delta.fix == TRUE) {
      delta = delta
    } else {
      delta <- delta * sqrt(size/(size - k))
    }
    dsq <- delta * delta
    if (!is.infinite(size) && (size * pi * dsq/4 > as.numeric(st_area(poly))))
      stop("\n Polygon is too small to fit ", size, " points, with 'k' = ",
           k, " close pairs,", " at minimum separation ", round(delta, digits = 4))
    while (dim(xy.sample)[1] < size) {
      dmin <- 0
      iter <- 1
      while (as.numeric(dmin) < dsq) {
        xy.try <- c(csr(st.poly, 1))


        # dmin<-min((xy.sample[,1]-xy.try[1])^2+(xy.sample[,2]-xy.try[2])^2)

        xy.sample <- sf::st_as_sf(as.data.frame(xy.sample), coords = c("xc",
                                                                       "yc"))
        st_crs(xy.sample) = 4326

        xy.try <- sf::st_as_sf(as.data.frame(t(as.data.frame(xy.try))),
                               coords = c("xc", "yc"))
        st_crs(xy.try) = 4326

        dmin <- st_distance(xy.sample, xy.try, by_element = TRUE)


        iter <- iter + 1
        if (iter == ntries)
          break
      }
      xy.sample <- rbind(xy.sample, xy.try)
      if (iter == ntries && dim(xy.sample)[1] < size) {
        warning("\n For the given 'delta' and 'size', only ", dim(xy.sample)[1],
                " inhibitory sample locations placed out of ", size, ". Consider revising 'delta' and/or 'size'")
        break
      }
    }
  }
  if (k > 0) {
    k.origin <- k
    size <- dim(unique(xy.sample))[1]
    reduction <- ((orig.size - size) / orig.size)
    if (k > size / 2) {
      k <- floor(k * (1 - reduction))
      warning("\n For the given parameters, only ", k, " close pairs could be placed out of ",
              k.origin)
    }
    take <- matrix(sample(1:size, 2 * k, replace = FALSE), k, 2)
    for (j in 1:k) {
      take1 <- take[j, 1]
      take2 <- take[j, 2]
      xy1 <- c(xy.sample[take1, ])
      angle <- 2 * pi * runif(1)
      radius <- rho * sqrt(runif(1))
      xy.sample[take2, ] <- xy1 + radius * c(cos(angle), sin(angle))
    }
  }
  xy.sample <- xy.sample %>% as.data.frame %>% sf::st_as_sf(coords = c(1, 2))
  sample.locs <- sf::st_as_sf(xy.sample)

  xy.sample <- sample.locs
  st_crs(xy.sample) <- 4326
  xy.sample <- st_intersection(st_geometry(poly), xy.sample$geometry)

  if (plotit == TRUE) {
    par(oma = c(5, 5, 5, 5.5), mar = c(5.5, 5.1, 4.1, 2.1), mgp = c(3, 1, 0),
        las = 0)
    plot(st_geometry(xy.sample), pch = 19, col = 1, axes = TRUE, xlab = "longitude",
         ylab = "lattitude", font.main = 3, cex.main = 1.2, col.main = "blue",
         main = paste("Continuous sampling design,", k, "close pairs", sep = " "),
         xlim = c(range(st.poly[, 1])), ylim = c(range(st.poly[, 2])))
    plot(st_geometry(poly), add = TRUE)
  }

  if (plotit_leaflet == TRUE) {
    par(oma = c(5, 5, 5, 5.5), mar = c(5.5, 5.1, 4.1, 2.1), mgp = c(3, 1, 0),
        las = 0)
    st_crs(xy.sample) <- 4326
    print(mapview(st_geometry(poly), add = TRUE, layer.name = c("Boundary"),
                  color = c("black"), alpha = 0.3, label = "Boundary") + mapview(st_geometry(xy.sample),
                                                                                 add = TRUE, layer.name = c("Sample Locations"), color = c("yellow"),
                                                                                 label = xy.sample$geometry, lwd = 2))
  }
  xy.sample_coords <- xy.sample %>% st_cast("MULTIPOINT") %>% st_cast("POINT")
  xy.sample_coords <- st_coordinates(xy.sample_coords)
  xy.sample_coords <- (cbind(c(seq_len(xy.sample_coords)), xy.sample_coords))
  colnames(xy.sample_coords) <- c("id", "lat", "long")
  assign("results", xy.sample_coords, envir = .GlobalEnv)
}

