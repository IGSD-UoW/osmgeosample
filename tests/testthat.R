library(testthat)
library(osmgeosample)


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
  expect_output(
    osm.random.sample(bounding_geom = bounding_geom, key= "building", value = "yes", boundary = 0, buff_epsg = 27700, join_type = "within", dis_or_cont = "discrete", sample_size = 70, plotit = TRUE, plotit_leaflet = TRUE, data_return=c("osm_polygons"), 'data.frame'))

  #> Test passed 😸

test_that("str_length of factor is length of level", {
  expect_equal(str_length(factor("a")), 1)
  expect_equal(str_length(factor("ab")), 2)
  expect_equal(str_length(factor("abc")), 3)
})
#> Test passed 🌈

test_that("str_length of missing is missing", {
  expect_equal(str_length(NA), NA_integer_)
  expect_equal(str_length(c(NA, 1)), c(NA, 1))
  expect_equal(str_length("NA"), 2)
})
