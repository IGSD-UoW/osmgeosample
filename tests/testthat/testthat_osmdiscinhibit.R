################################# osm.random.sample ####################################################

bounding_geom <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries", layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile


test_that("osm.contin.inhibit working with shapefile", {
 model<-osm.discrete.inhibit.sample(bounding_geom=bounding_geom, sample_size=70, delta = 5, key ='building', value=NULL, delta.fix = TRUE, k = 0, cp.criterion = 'cp.neighb', zeta = 0.025, ntries = 5)
 expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text", {
  model<-osm.discrete.inhibit.sample(bounding_geom = "Failand, UK", sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box", {
  model<-osm.discrete.inhibit.sample(bounding_geom = bounding_geom, boundary = 1, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box", {
  model<-osm.discrete.inhibit.sample(bounding_geom = "Failand, UK", boundary = 1, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer", {
  model<-osm.discrete.inhibit.sample(bounding_geom = bounding_geom,  buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer", {
  model<-osm.discrete.inhibit.sample(bounding_geom = "Failand, UK",  buff_dist = 1000, buff_epsg = 27700, boundary=2, sample_size = 70, delta = 50,delta.fix = FALSE, k = 7, rho = 1, ntries = 10)
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀





test_that("osm.random.sample working with shapefile and join_type is within", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within", dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and join_type is within", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", join_type = "within",dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and join_type is within", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within",boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and join_type is within", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "within", boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and join_type is within", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within",buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and join_type is within", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "within", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀





test_that("osm.random.sample working with shapefile and join_type is intersect", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect", dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and join_type is intersect", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", join_type = "intersect",dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and join_type is intersect", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and join_type is intersect", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "intersect", boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and join_type is intersect", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and join_type is intersect", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "intersect", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀



















test_that("osm.random.sample working with shapefile and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀





test_that("osm.random.sample working with shapefile and join_type is within and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within", dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and join_type is within and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", join_type = "within",dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and join_type is within and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within",boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and join_type is within and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "within", boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and join_type is within and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within",buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and join_type is within and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "within", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀





test_that("osm.random.sample working with shapefile and join_type is intersect and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect", dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and join_type is intersect and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK", join_type = "intersect",dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and join_type is intersect and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and join_type is intersect and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "intersect", boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and join_type is intersect and continuous", {
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and join_type is intersect and continuous", {
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "intersect", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀


model0<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect", dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
model0<-nrow(results)
model1<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
model1<-nrow(results)
model2<-osm.random.sample(bounding_geom = bounding_geom,join_type = "intersect", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
model2<-nrow(results)
expect_lt(model0, model1, label = NULL, expected.label = NULL)
expect_lt(model0, model2, label = NULL, expected.label = NULL)
expect_lt(model1, model2, label = NULL, expected.label = NULL)


