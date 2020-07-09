###########Preamble ###############################

#read in the correct libraries and data
library("geoR")
library(sp)
library(sf)
library(splancs)

data("parana")

# Define the parameters
poly <- st_sf(st_sfc(st_polygon(
  list(as.matrix(matrix(c(parana$borders[,1],parana$borders[,2]),dim(parana$borders)[1],2,byrow=FALSE)))))) # create a polygon from the matrix
size =100 ## to number of locatoins to be sampled
delta = 0.3 ## minimum permissable distance between any two locations in a sinly sample. 
delta.fix = FALSE ## is the value fixed or does it vary with the number of close pairs.
k=0 # number of locations in sample to be replaced by near neighbours of other sample locations to form close pair (max int size/2). k=0 is a simple inhibitory design.
rho = NULL # the mex distance etween any two locations in a 'close pair'
ntreis = 10000 # number of rejected proposals after which the algorithm will terminate.
plotit = TRUE # determines whether we should plot the sample.


## Function which draws a spatially continous sample of locations within a polygonal sampling region according to an "inhibitory plus close pairs" specification.
contin.inhibit.simplified <-function(poly,size,delta, delta.fix = FALSE,
                                    k=0,rho=NULL, ntries = 10000, plotit = TRUE) {
  
  poly.origin <- poly
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

  par(oma=c(5, 5, 5, 5.5), mar=c(5.5, 5.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
  plot(st_geometry(xy.sample),pch=19,col=1,axes = TRUE,
       xlab="longitude",ylab="lattitude", font.main = 3,
       cex.main = 1.2, col.main = "blue",
       main = paste("Continuous sampling design,", k,
                    "close pairs", sep = " "),
       xlim = c(range(st.poly[,1])),
       ylim = c(range(st.poly[,2])))
  plot(st_geometry(poly), add= TRUE)

res <- list()
res$size <- dim(unique(xy.sample))[1]
res$delta = delta
res$k <- k
res$sample.locs = sample.locs

}

# Generate spatially regular sample
set.seed(5871120)
contin.inhibit.simplified(poly,size=100,delta=3, delta.fix = FALSE, k=0,rho=NULL, ntries = 100000, plotit = TRUE)


# Generate spatially regular sample
set.seed(5871121)
xy.sample1 <- contin.inhibit.simplified(poly=poly,size = 100, delta = 30, plotit = TRUE)


# Generate spatially regular sample with 10 close pairs
set.seed(5871122)
xy.sample2 <- contin.inhibit.simplified(poly,size = 100, delta = 30,
                                    k = 5, rho = 15, plotit = TRUE)

# Generate spatially regular sample with 10 close pairs
set.seed(5871123)
xy.sample3 <- contin.inhibit.simplified(poly,size = 100, delta = 30, delta.fix = TRUE,
                                    k = 10, rho = 15, plotit = TRUE)



