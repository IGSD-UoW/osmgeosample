contin.inhibit.simplified <-function(poly,size,delta, delta.fix = FALSE,
                                    k=0,rho=NULL, ntries = 10000, plotit = TRUE) {
  
  poly.origin <- poly
  poly <- sf::st_as_sf(poly)
  orig.size <- size
  st.poly <- sf::st_coordinates(poly)[,c(1:2)]
  xy.sample <- matrix(csr(st.poly,1),1,2)
  delta <- delta * sqrt(size/(size - k))
  dsq  <- delta*delta
  xy.sample<-rbind(xy.sample,xy.try)
  k.origin <- k
  size <- dim(unique(xy.sample))[1]
  reduction <- ((orig.size - size)/orig.size)
  take<-matrix(sample(1:size,2*k,replace=FALSE),k,2)
  for (j in 1:k) {
    take1<-take[j,1]; take2<-take[j,2]
    xy1<-c(xy.sample[take1,])
    angle<-2*pi*runif(1)
    radius<-rho*sqrt(runif(1))
    xy.sample[take2,]<-xy1+radius*c(cos(angle),sin(angle))
  }
xy.sample <- xy.sample %>%
  as.data.frame %>%
  sf::st_as_sf(coords = c(1,2))
sample.locs <- sf::st_as_sf(xy.sample)
if(class(sample.locs)[1] != class(poly.origin)[1]){
  sample.locs <- sf::as_Spatial(xy.sample, "Spatial")
  
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

return(res)
}
