################################# osm.random.sample ####################################################
library(testthat)
library(osmgeosample)

bounding_geom <- readOGR(dsn="C:/Users/Henry/Documents/University of Warwick/Boundaries", layer="Boundary_Idikan",verbose=FALSE) ## here you can read in any shapefile


test_that("osm.random.sample working with shapefile within", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text within", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box within", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box within", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer within", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer within", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀



test_that("osm.random.sample working with shapefile and join_type is intersect", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect", dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and join_type is intersect", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", join_type = "intersect",dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and join_type is intersect", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and join_type is intersect", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "intersect", boundary=1, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and join_type is intersect", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and join_type is intersect", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "intersect", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "discrete", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀



















test_that("osm.random.sample working with shapefile and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀





test_that("osm.random.sample working with shapefile and join_type is within and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within", dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and join_type is within and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", join_type = "within",dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and join_type is within and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within",boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and join_type is within and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "within", boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and join_type is within and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "within",buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and join_type is within and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "within", buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀





test_that("osm.random.sample working with shapefile and join_type is intersect and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect", dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and join_type is intersect and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK", join_type = "intersect",dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and boundary box and join_type is intersect and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and boundary box and join_type is intersect and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = "Failand, UK",join_type = "intersect", boundary=1, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with shapefile and buffer and join_type is intersect and continuous", {
  skip_on_cran('skip')
  model<-osm.random.sample(bounding_geom = bounding_geom, join_type = "intersect",buff_dist = 1000, buff_epsg = 27700, boundary=2, dis_or_cont = "continuum", sample_size = 70, key = "building", data_return = c("osm_polygons"))
  expect_output(str(results), 'data.frame')
})
#> Test passed 😀

test_that("osm.random.sample working with text and buffer and join_type is intersect and continuous", {
  skip_on_cran('skip')
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


