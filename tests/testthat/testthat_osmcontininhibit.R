################################# osm.random.sample ####################################################
library(testthat)
library(osmgeosample)

bounding_geom <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries", layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile

test_that("osm.contin.inhibit working with shapefile", {
  model<-osm.contin.inhibit(bounding_geom = bounding_geom, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text", {
  model<-osm.contin.inhibit(bounding_geom = "Failand, UK", sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box", {
  model<-osm.contin.inhibit(bounding_geom = bounding_geom, boundary = 1, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box", {
  model<-osm.contin.inhibit(bounding_geom = "Failand, UK", boundary = 1, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer", {
  model<-osm.contin.inhibit(bounding_geom = bounding_geom,  buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer", {
  model<-osm.contin.inhibit(bounding_geom = "Failand, UK",  buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

