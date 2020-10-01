################################# osm.random.sample ####################################################
library(testthat)
library(osmgeosample)

bounding_geom <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries", layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile

test_that("osm.contin.inhibit working with shapefile within", {
 model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom, sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
 expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text within", {
  model<-osm.discrete.inhibit.sample(bounding_geom="Failand, UK", sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box within", {
  model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom,boundary=1,  sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box within", {
  model<-osm.discrete.inhibit.sample(bounding_geom="Failand, UK",boundary=1,  sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer within", {
  model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom,  buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer within", {
  model<-osm.discrete.inhibit.sample(bounding_geom="Failand, UK", buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀








test_that("osm.contin.inhibit working with shapefile instersects", {
  model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom, join_type = "intersect", sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text instersects", {
  model<-osm.discrete.inhibit.sample(bounding_geom="Failand, UK", join_type = "intersect", sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box instersects", {
  model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom,boundary=1, join_type = "intersect",  sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box instersects", {
  skip_on_cran('skip')
  model<-osm.discrete.inhibit.sample(bounding_geom="Failand, UK",boundary=1, join_type = "intersect",  sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer instersects", {
  skip_on_cran('skip')
  model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom, join_type = "intersect",  buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer instersects", {
  model<-osm.discrete.inhibit.sample(bounding_geom="Failand, UK", join_type = "intersect", buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀



model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom, join_type = "intersect", sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
model0<-nrow(results)
model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom,boundary=1, join_type = "intersect",  sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
model1<-nrow(results)
model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom, join_type = "intersect",  buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 3, data_return = "osm_polygons")
model2<-nrow(results)
expect_lt(model0, model1, label = NULL, expected.label = NULL)
expect_lt(model0, model2, label = NULL, expected.label = NULL)
expect_lt(model1, model2, label = NULL, expected.label = NULL)


