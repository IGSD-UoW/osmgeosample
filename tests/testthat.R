library(testthat)
library(osmgeosample)
library(nngeo)
library('geoR')
library(sp)
library(sf)
library(splancs)
library(rgdal)
library(osmdata)
library(processx)
library(mapview)
library('dplyr')
library(rgdal)


bounding_geom <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries", layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile
#bounding_geom<-"Hanwood, UK"
boundary<- 0
#buff_dist <- 1000
#buff_epsg <- 1168
buff_epsg <- 27700
join_type <- "intersect"
dis_or_cont <- "discrete"
sample_size <- 70
plotit <- TRUE
plotit_leaflet <- TRUE
key<- "building"
value <- NULL
#value <- "yes"
data_return <- c("osm_polygons", "osm_points", "osm_multipolygons","osm_multilines","osm_lines")


test_check("osmgeosample")

test_that("osm.random.sample working with shapefile", {
  model<-osm.random.sample(bounding_geom = bounding_geom, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
expect_output(str(results), 'data.frame')
})

test_that("osm.random.sample working with text", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
