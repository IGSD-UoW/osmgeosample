# ###########Preamble ###############################
# #install packages
# install.packages(c("geoR", "sp", "sf", "splancs", "rgdal","osmdata"))
#
# #read in the correct libraries and data
# library("geoR")
# library(sp)
# library(sf)
# library(splancs)
# library(rgdal)
# library(osmdata)
# #data("parana")
#
# ##### This is the border of one of the slums - we can use this directly in our continuous inhibit ######
# Idikan <- readOGR(dsn="C:/Users/Henry Crosby/Documents/University of Warwick/Boundaries" , layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile
#
# #### This is data that I extract from osm using the border above #####################
# dat <-  opq (bbox(Idikan)) %>%
#   add_osm_feature (key="building", value="yes") %>%
#   osmdata_sf ()
#
# plot(dat$osm_polygons$geometry)
# #dat <-  opq (bbox(Idikan)) %>%
# #  add_osm_feature (key="building", value="yes") %>%
# #  osmdata_sp ()
#
# #dat <- opq (bbox(Idikan)) %>%
# #  add_osm_feature (key="building", value="yes") %>%
# #  osmdata_xml ()
#
# id1 <- rownames (dat$osm_multipolygons)
# id2 <- rownames (dat$osm_polygons)
# id3 <- rownames (dat$osm_points)
# id4 <- rownames (dat$osm_multilines)
# id5 <- rownames (dat$osm_lines)
#
# if (!is.null(id1)){dat1<-osm_multipolygons (dat, id1)}
# if (!is.null(id2)){dat2<-osm_polygons (dat, id2)}
# if (!is.null(id3)){dat3<-osm_points (dat, as.character(id3))}  ################## For some reason this is not working?? Is there something wrong with the function?
# if (!is.null(id4)){dat4<-osm_multilines (dat, id4)}
# if (!is.null(id5)){dat5<-osm_lines (dat, id5)}
#
# poly<-dat3
# poly<-dat2
#
# ############## This is dummy data created by Chipeta using data("parana") ###################
# ##poly <- st_sf(st_sfc(st_polygon(list(as.matrix(matrix(c(parana$borders[,1],parana$borders[,2]),dim(parana$borders)[1],2,byrow=FALSE)))))) # create a polygon from the matrix
#
# ## This is the, now simplified function ##
# contin.inhibit.simplified <-function(poly,size,delta, delta.fix = FALSE,
#                                      k=0,rho=NULL, ntries = 10000, plotit = TRUE) {
#
#   poly.origin <- poly
#   poly <- sf::st_as_sf(poly)
#   orig.size <- size
#   st.poly <- sf::st_coordinates(poly)[,c(1:2)]
#   xy.sample <- matrix(csr(st.poly,1),1,2)
#   delta <- delta * sqrt(size/(size - k))
#   dsq  <- delta*delta
#   for (i in 2:size) {
#     xy.try <- c(csr(st.poly,1))
#     xy.sample <- rbind(xy.sample, xy.try)
#   }
#   k.origin <- k
#   reduction <- ((orig.size - size)/orig.size)
#   take<-matrix(sample(1:size,2*k,replace=FALSE),k,2)
#   xy.sample <- xy.sample %>%
#     as.data.frame %>%
#     sf::st_as_sf(coords = c(1,2))
#   sample.locs <- sf::st_as_sf(xy.sample)
#
#   par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
#   plot(st_geometry(xy.sample),pch=19,col=1,axes = TRUE,
#        xlab="longitude",ylab="lattitude", font.main = 3,
#        cex.main = 1.2, col.main = "blue",
#        main = paste("Continuous sampling design,", k,
#                     "close pairs", sep = " "),
#        xlim = c(range(st.poly[,1])),
#        ylim = c(range(st.poly[,2])))
#   plot(st_geometry(poly), add= TRUE)
#
#   res <- list()
#   res$size <- dim(unique(xy.sample))[1]
#   res$delta = delta
#   res$k <- k
#   res$sample.locs = sample.locs
#
# }
# library(OpenStreetMap)
# library(ggplot2)
# map <- openmap(xy.sample1, zoom = NULL,
#                type = c("osm", "stamen-toner", "stamen-terrain","stamen-watercolor", "esri","esri-topo")[6],
#                mergeTiles = TRUE)
# library(leaflet)
# r_birthplace_map <- leaflet() %>%
#   addTiles() %>%  # use the default base map which is OpenStreetMap tiles
#   addMarkers(lng=174.768, lat=-36.852,
#              popup="The birthplace of R")
# r_birthplace_map
#
# # Generate spatially regular sample
# set.seed(5871120)
# contin.inhibit.simplified(Idikan,size=100,delta=100000, delta.fix = FALSE, k=0,rho=NULL, ntries = 100000, plotit = TRUE)
# contin.inhibit.simplified(poly,size=100,delta=10, delta.fix = FALSE, k=0,rho=NULL, ntries = 1000, plotit = TRUE)
#
#
# # Generate spatially regular sample
# set.seed(5871121)
# xy.sample1 <- contin.inhibit.simplified(poly=Idikan,size = 100, delta = 30, plotit = TRUE)
# #xy.sample1 <- contin.inhibit.simplified(poly=poly,size = 100, delta = 30, plotit = TRUE)
#
#
# # Generate spatially regular sample with 10 close pairs
# set.seed(5871122)
# xy.sample2 <- contin.inhibit.simplified(Idikan,size = 100, delta = 30,k = 5, rho = 15, plotit = TRUE)
# #xy.sample2 <- contin.inhibit.simplified(poly,size = 100, delta = 30,k = 5, rho = 15, plotit = TRUE)
#
# # Generate spatially regular sample with 10 close pairs
# set.seed(5871123)
# xy.sample3 <- contin.inhibit.simplified(Idikan,size = 100, delta = 30, delta.fix = TRUE, k = 10, rho = 15, plotit = TRUE)
# #xy.sample3 <- contin.inhibit.simplified(poly,size = 100, delta = 30, delta.fix = TRUE, k = 10, rho = 15, plotit = TRUE)
