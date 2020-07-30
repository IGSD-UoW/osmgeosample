# # Descscription of the package:
#
# #Functions for constructing sampling designs, including spatially random, inhibitory (simple or with close pairs), both discrete and continuous, and
# #adaptive designs. For details on the methods, see the following references: Chipeta et al. (2016) <doi:10.1016/j.spasta.2015.12.004>
# #and Chipeta et al. (2016) <doi:10.1002/env.2425>.
#
# #Preamble
#
# install.packages("W:\\workspace\\henry_workspace\\Tasks\\Task 1 - Review the current work\\Exploratory R Scripts\\geosample_0.2.1 (3).tar.gz",
#                  repos = NULL, type = "source") # install geosample from https://cran.r-project.org/src/contrib/Archive/geosample/
#
# library(geosample) # access the geosample package in this session.
# library("dplyr")
# library("geoR")
# library("PrevMap")
# data("parana")
# data("border")
# data("majete")
# data("sim.data")
#
# ############# Getting documentation for the package
# ??geosample # provides documents DESCRIPTION, geosample-vignette, source and R code in the "Exploratory R Scripts" directory previously mentioned in this script
#
# # This is a useful page for more information - https://rdrr.io/cran/geosample/man/
#
# # This is where you can get Chipeta's first of two publications - http://www.research.lancs.ac.uk/portal/en/publications/adaptive-geostatistical-design-and-analysis-for-prevalence-surveys(29291e22-9b35-4a98-8453-76880ce61e5c).html
# # This is where you can get Chipeta's second of two publications - https://onlinelibrary.wiley.com/doi/abs/10.1002/env.2425
#
#
# ######################### Getting all data in the package #######################################
#
# data("border") # polygon given the spatial border in southern Malawi
# plot(border)
# data("majete") #malaria study in southern Malawi
# plot(majete$geometry)
#
# ####### Meta data for Majete
# #rdt: Rapid diagnostic test result; 0 = negative, 1 = positive.
# #age: Age of the individual in months.
# #quintile: Wealth quintile; ranging from 1 = poor to 5 = well to do.
# #itn: Insecticide treated bed-net usage; 0 = no, 1 = yes.
# #elev: Elevation; height above sea level in meters.
# #ndvi: Normalised difference vegetation index (greenness).
# #agecat: Age category; 1 = child, 2 = adult.
# #geometry: Point or household locations (UTM).
#
# data("sim.data")
# plot(sim.data)
# ############## Meta data for sim.data
# #This binomial data-set was simulated by generating a zero-mean stationary Gaussian process over a 35 by 35 grid covering the unit square with Matern correlation sturcture. The parameters used in the simulation are ??^2 = 0.7, ?? = 0.15, ?? = 1.5 and ??^2 = 0. The nugget effect was not included, hence tau2 = 0. The variables are as follows:
#
# #data - simulated values of the Gaussian process.
# #y - binomial observations.
# #units.m - binomial denominators.
# #geometry - X and Y coordinates.
#
#
#
# ############################# Getting the functions in the package #####################################
#
# lsf.str("package:geosample") # find all of the functions in geosample
#
# ########################### All functions (output of the request in line 8)##########################################
# #adaptive.sample : function (obj1, obj2, pred.var.col = NULL, excd.prob.col = NULL, batch.size = 1, delta, criterion, poly = NULL, plotit = TRUE)
# #contin.inhibit.sample : function (poly, size, delta, delta.fix = FALSE, k = 0, rho = NULL, ntries = 10000, plotit = TRUE)
# #discrete.inhibit.sample : function (obj, size, delta, delta.fix = FALSE, k = 0, cp.criterion = NULL, zeta, ntries = 10000, poly = NULL, plotit = TRUE)
# #random.sample : function (obj = NULL, poly = NULL, type, size, plotit = TRUE)
#
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ################################################## Adaptive - https://rdrr.io/cran/geosample/man/adaptive.sample.html###################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
#
# #example using toy datasets
# #1. sampling locations with associated prediction variance and exceedance probabilities
# set.seed(1234)
# xy.all <- expand.grid(x = seq(0,1, l = 10),y = seq(0,1, l = 10))
# xy.all$predvar <- runif(100, min=0, max = 2.5)
# xy.all$exceedprob <- runif(100, min = 0, max = 1)
# obj1 <- sf::st_as_sf(xy.all, coords = c('x', 'y'))
#
# #2. initial sample design
# set.seed(1234)
# xy.sample <- discrete.inhibit.sample(obj = obj1, size = 70,
#                                      delta = 0.075, k = 0,
#                                      plotit = TRUE)
# init.design <- xy.sample$sample.locs
#
# #3. adaptive sampling designs
# #a. using prediction variance criterion
# adapt.design.pv <- adaptive.sample(obj1 = obj1, obj2 = init.design,
#                                    pred.var.col = 1, criterion = "predvar",
#                                    delta = 0.1, batch.size = 10,
#                                    poly = NULL, plotit = TRUE)
#
#
# #b. using exceedance probability criterion
# adapt.design.ep <- adaptive.sample(obj1 = obj1, obj2 = init.design,
#                                    excd.prob.col = 2, criterion = "exceedprob",
#                                    delta = 0.1, batch.size = 10,
#                                    poly = NULL, plotit = TRUE)
#
#
#
# ## Not run:
# data("sim.data")
# library("PrevMap")
# library("sf")
#
# #1. Generate inhibitory design without close pairs using discrete.inhibit.sample().
# set.seed(1234)
# xy.sample <- discrete.inhibit.sample(obj = sim.data, size = 100, delta = 0.075,
#                                      k = 0, plotit = TRUE)
# names(xy.sample)
# init.design <- xy.sample$sample.locs
#
# #2. Data analysis
# knots <- as.matrix(expand.grid(seq(-0.2, 1.2, length = 15),
#                                seq(-0.2, 1.2, length = 15)))
# lr.mcmc <- control.mcmc.MCML(n.sim = 10000, burnin = 1000, thin = 6)
#
# par0.lr <- c(0.001, 1, 0.4)
# fit.MCML.lr <- binomial.logistic.MCML(y ~ 1,
#                                       units.m = ~units.m, coords = ~st_coordinates(init.design),
#                                       data = init.design, par0 = par0.lr, fixed.rel.nugget = 0,
#                                       start.cov.pars = par0.lr[3], control.mcmc = lr.mcmc,
#                                       low.rank = TRUE, knots = knots, kappa = 1.5,
#                                       method = "nlminb", messages = TRUE,
#                                       plot.correlogram = FALSE)
#
# summary(fit.MCML.lr, log.cov.pars = FALSE)
#
# # Note: parameter estimation above can and should be repeated several times with updated starting
# # values for the covariance function.
#
# #3. Plug-in prediction using estimated parameters
# pred.MCML.lr <- spatial.pred.binomial.MCML(object = fit.MCML.lr,
#                                            control.mcmc = lr.mcmc,
#                                            grid.pred = st_coordinates(sim.data),
#                                            type = "joint", messages = TRUE,
#                                            scale.predictions = "prevalence",
#                                            standard.errors = TRUE,  thresholds = 0.45,
#                                            scale.thresholds = "prevalence")
#
#
# #4. Visualisation of analysis from initial sample
# plot(pred.MCML.lr, type = "prevalence", summary = "predictions",
#      zlim = c(0, 1), main = "Prevalence - predictions")
# contour(pred.MCML.lr, "prevalence", "predictions",
#         zlim = c(0, 1), levels = seq(0.1,0.9, 0.1), add = TRUE)
#
# plot(pred.MCML.lr,  summary = "exceedance.prob",
#      zlim = c(0, 1), main = "Prevalence - exceedance probability")
# contour(pred.MCML.lr, summary = "exceedance.prob",
#         zlim = c(0, 1), levels = seq(0.1,0.3, 0.1), add = TRUE)
#
# plot(pred.MCML.lr, type = "prevalence",  summary = "standard.errors",
#      main = "Prevalence - standard errors")
#
# #5. Adaptive sampling
# #create data frame of ingredients to adaptive sampling from spatial predictions above
# obj1 <- as.data.frame(cbind(pred.MCML.lr$grid,
#                             c(pred.MCML.lr$prevalence$standard.errors)^2,
#                             pred.MCML.lr$exceedance.prob))
# colnames(obj1) <- c("x", "y", "pred.var", "exceed.prob")
# obj1 <- sf::st_as_sf(obj1, coords = c('x', 'y'))
#
#
# #adaptive sampling using prediction variance criterion.
# adapt.design.pv <- adaptive.sample(obj1 = obj1, obj2 = init.design,
#                                    pred.var.col = 1, excd.prob.col = 2,
#                                    criterion = "predvar", delta = 0.08,
#                                    batch.size = 10, poly = NULL, plotit = TRUE)
#
# #adaptive sampling using exceedance probability criterion.
# adapt.design.ep <- adaptive.sample(obj1 = obj1, obj2 = init.design,
#                                    pred.var.col = 1, excd.prob.col = 2,
#                                    criterion = "exceedprob", delta = 0.08,
#                                    batch.size = 10, poly = NULL, plotit = TRUE)
#
# ## End(Not run)
#
#
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ######################################## contin inhibit - https://rdrr.io/cran/geosample/man/contin.inhibit.sample.html
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
#
#
# poly <- parana$borders
# poly <- matrix(c(poly[,1],poly[,2]),dim(poly)[1],2,byrow=FALSE)
# #convert matrix to polygon
# poly <- st_sf(st_sfc(st_polygon(list(as.matrix(poly)))))
# #poly <- as(poly, "Spatial")
# poly
#
# # Generate spatially regular sample
# set.seed(5871121)
# xy.sample1 <- contin.inhibit.sample(poly=poly,size = 100, delta = 30, plotit = TRUE)
#
#
# # Generate spatially regular sample with 10 close pairs
# set.seed(5871122)
# xy.sample2 <- contin.inhibit.sample(poly,size = 100, delta = 30,
#                                     k = 5, rho = 15, plotit = TRUE)
#
# # Generate spatially regular sample with 10 close pairs
# set.seed(5871123)
# xy.sample3 <- contin.inhibit.sample(poly,size = 100, delta = 30, delta.fix = TRUE,
#                                     k = 10, rho = 15, plotit = TRUE)
#
#
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ####################################### discrete inhibit https://rdrr.io/cran/geosample/man/discrete.inhibit.sample.html
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
#
# set.seed(1234)
# x <- 0.015+0.03*(1:33)
# xall <- rep(x,33)
# yall <- c(t(matrix(xall,33,33)))
# xy <- cbind(xall,yall)+matrix(-0.0075+0.015*runif(33*33*2),33*33,2)
#
#
# # Convert to SF object
# xy <- xy %>%
#   as.data.frame %>%
#   sf::st_as_sf(coords = c(1,2))
#
#
# # Plot the points
# plot(st_geometry(xy),pch=19,cex=0.25,xlab="longitude",ylab="latitude",
#      cex.lab=1,cex.axis=1,cex.main=1, axes = TRUE)
#
#
# # Generate spatially random sample
# set.seed(15892)
# xy.sample1 <- xy[sample(1:dim(xy)[1],50,replace=FALSE),]
# plot(xy.sample1, pch = 19, col = 'black', add = TRUE)
#
#
# set.seed(15892)
# xy.sample2 <- discrete.inhibit.sample(obj=xy,size = 100,
#                                       delta = 0.08,plotit = TRUE)
# plot(st_geometry(xy),pch=19, cex = 0.25, col="black", add = TRUE)
#
#
# # Generate spatially inhibitory sample
# # with close pairs (cp.zeta criterion):
# set.seed(15892)
# xy.sample3 <- discrete.inhibit.sample(obj=xy, size = 100,delta = 0.065,
#                                       k = 25,cp.criterion = "cp.zeta",
#                                       zeta = 0.025, plotit = TRUE)
# plot(st_geometry(xy),pch=19, cex = 0.25, col="black", add = TRUE)
#
#
# # Generate spatially inhibitory sample
# # with close pairs (cp.neighb criterion):
# set.seed(15892)
# xy.sample4 <- discrete.inhibit.sample(obj=xy,size = 100,
#                                       delta = 0.065, k = 25,cp.criterion = "cp.neighb",
#                                       plotit = TRUE)
# plot(st_geometry(xy),pch=19, cex = 0.25, col="black", add = TRUE)
#
#
# # Generate spatially inhibitory sample
# # with close pairs (cp.zeta criterion):
# set.seed(15892)
# xy.sample5 <- discrete.inhibit.sample(obj=xy,size = 100,
#                                       delta = 0.065, cp.criterion = "cp.zeta",
#                                       zeta = 0.025, delta.fix = TRUE,
#                                       k = 25, plotit = TRUE)
# plot(st_geometry(xy),pch=19, cex = 0.25, col="black", add = TRUE)
#
#
# # Generate simple inhibitory sample from a regular grid
# set.seed(15892)
# xy.sample6 <- discrete.inhibit.sample(obj = sim.data,
#                                       size = 50, delta = 0.08,plotit = TRUE)
# plot(st_geometry(sim.data),pch=19,col="black", cex = 0.25, add = TRUE)
#
#
# # Generate inhibitory plus close pairs sample from a regular grid
# set.seed(15892)
# xy.sample7 <- discrete.inhibit.sample(obj = sim.data,
#                                       cp.criterion = "cp.neighb", size = 50,
#                                       delta = 0.1, k = 5, plotit =TRUE)
# plot(st_geometry(sim.data),pch=19,col="black", cex = 0.25, add = TRUE)
#
#
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ######################################### random https://rdrr.io/cran/geosample/man/random.sample.html
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
# ########################################################################################################################################################################################################
#
# #This function draws a spatially random sample from a discrete set of units located over some defined geographical region or generate completely spatially random points within a polygon.
#
# ############################ Example of how to use the data with random #####################################
#
# # 1. Sampling from a discrete set of points.
# x <- 0.015+0.03*(1:33) # create x-values
# xall <- rep(x,33) # create x-values
# yall <- c(t(matrix(xall,33,33))) # create y-values
# xy <- cbind(xall,yall)+matrix(-0.0075+0.015*runif(33*33*2),33*33,2) # combine the x- and y-values
# colnames(xy) <- c('X','Y') # name the columns
# plot(xy) # plot to see what it looks like
#
# # Convert to simple features (SF) - i.e., make them points on a map.
# xy <- xy %>%
#   as.data.frame %>%
#   sf::st_as_sf(coords = c(1,2)) #### making them points
# xy <- sf::st_as_sf(xy, coords = c('X', 'Y')) ##### converting them to sf
# plot(xy)  # plotting them as points.
#
# # Sampling from a discrete set.
# set.seed(15892)
# xy.sample <- random.sample(obj = xy, size = 100, type = "discrete", plotit = TRUE) ## running the random sample
# ##where obj = the dataframe that you are sampling. Size = the number of points in the sample, type = "discrete of continuous" and plotit = boolean dafault plot?
#
# # Sampling from a continuum.
# data("parana")
# plot(parana) # a continuous dataset
# poly <- parana$borders
# poly <- matrix(c(poly[,1],poly[,2]),dim(poly)[1],2,byrow=FALSE) # converting a list of border points into a set of 2-d points
# # Convert matrix to polygon
# poly <- st_sf(st_sfc(st_polygon(list(as.matrix(poly))))) # converting to a polygon
#
# set.seed(15892)
# xy.random.sample <- random.sample(poly = poly,size = 100, type = "continuum", plotit = TRUE) # samples all points within a polygon
#
