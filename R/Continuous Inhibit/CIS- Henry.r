library("geoR")
library(sp)
library(sf)
library(splancs)
library(rgdal)
library(osmdata)
library(mapview)


# Boundary {exact: 0, bbox: 1, buffer: 2} Default: 0
# jointype {intersects: 0, within: 1} Default: 0


#poly <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries" , layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile
#boundary<- 0
#jointype <- 1
#size<-100
#delta<-100
#delta.fix <- FALSE
#k<-200
#rho<-NULL
#ntries <- 100
#plotit <- TRUE

contin.inhibit.simplified <-function(poly,size,delta, delta.fix = FALSE,
                                     k=0,rho=NULL, ntries = 10000, plotit = TRUE, boundary=0,jointype=0) {

 if (boundary == 0) {
  dat <-  opq (poly@bbox) %>%
    add_osm_feature (key="building", value="yes") %>%
    osmdata_sf () ## Returns all buildings within the bounding box

  dat_tr_ex <-trim_osmdata (dat, poly) # Returns all buildings that are fully within the specified area
  dat_tr <- trim_osmdata (dat, poly, exclude = FALSE) # Returns all buildings that intersect with the specified area
  bounding <- poly
  } else if (boundary == 1) {

      dat <-  opq (poly@bbox) %>%
     add_osm_feature (key="building", value="yes") %>%
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
     add_osm_feature (key="building", value="yes") %>%
     osmdata_sf () ## Returns all buildings within the bounding box

   dat_tr_ex <-trim_osmdata (dat, countries_buff_5km) # Returns all buildings that are fully within the specified area
   dat_tr <- trim_osmdata (dat, countries_buff_5km, exclude = FALSE) # Returns all buildings that intersect with the specified area
   boundary <- countries_buff_5km
      }

if (jointype == 0){buildings <-dat_tr$osm_polygons } else {buildings<-dat_tr_ex$osm_polygons}

  poly.origin <- boundary
  poly <- sf::st_as_sf(poly)
  orig.size <- size
  st.poly <- sf::st_coordinates(poly)[,c(1:2)]
  xy.sample <- matrix(csr(st.poly,1),1,2)
  delta <- delta * sqrt(size/(size - k))
  dsq  <- delta*delta
  for (i in 2:size) {
    xy.try <- c(csr(st.poly,1))
    xy.sample <- rbind(xy.sample, xy.try)
  }
  k.origin <- k
  reduction <- ((orig.size - size)/orig.size)
  take<-matrix(sample(1:size,2*k,replace=FALSE),k,2)
  xy.sample <- xy.sample %>%
    as.data.frame %>%
    sf::st_as_sf(coords = c(1,2))
  sample.locs <- sf::st_as_sf(xy.sample)
  xy.sample = xy.sample %>% st_set_crs(4326)

  res <- list()
  res$size <- dim(unique(xy.sample))[1]
  res$delta = delta
  res$k <- k
  res$sample.locs = sample.locs

  res$sample.locs = res$sample.locs %>% st_set_crs(4326)
  par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  mapview(st_geometry(res$sample.locs),
          map.types = c("OpenStreetMap.DE"),
          layer.name = c("Sample Locations"),
          color = c("black"))+
    mapview(st_geometry(poly), add= TRUE,
            layer.name = c("Boundary"),
            color = c("black"))+
    mapview(st_geometry(buildings), add= TRUE,
            layer.name = c("Building"),
            color=c("cornflowerblue"))


}

contin.inhibit.simplified(poly,size=1000,delta=100, delta.fix = FALSE, k=0,rho=NULL, ntries = 10000, plotit = TRUE,
                          boundary=1,jointype=0)

