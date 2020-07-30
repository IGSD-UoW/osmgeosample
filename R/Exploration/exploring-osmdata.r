# library(osmdata)
# library(rosm)
# library(prettymapr)
#
#
# osmdata(
#   bbox = NULL, ## bounding box
#   overpass_call = NULL, ## overpass call
#   meta = NULL, ## timestamp and version number
#   osm_points = NULL, #
#   osm_lines = NULL,
#   osm_polygons = NULL,
#   osm_multilines = NULL,
#   osm_multipolygons = NULL
# )
#
# ############Functions to Prepare Queries
# getbb("lower failand") ## Get bounding box for a given place name
#
# bbox_to_string("bristol") ##Convert a named matrix or a named vector (or an unnamed vector) return a string
#
# opq(getbb("bristol")) ## Build an overpass query
#
# ##add_osm_feature: Add a feature to an overpass query
#
# ##opq_string: Convert an osmdata query to overpass API string
#
# #############Functions to Get Additional OSM Information
# available_features() #: List recognised features in OSM
# available_tags("substation") #: List tags associated with a feature
#
# ##Functions to Extract OSM Data
# sf<-osmdata_sf(opq(getbb("lower failand"))) #: Return OSM data in sf format
# sp<- osmdata_sp(opq(getbb("lower failand"))) #: Return OSM data in sp format
# xml<-osmdata_xml(opq(getbb("lower failand"))) #: Return OSM data in XML format
#
# dat <- opq(getbb("lower failand")) %>% osmdata_sf ()
# Wraxall_and_Failand <- which (dat$osm_multipolygons$name == "Wraxall and Failand")
# id <- rownames (dat$osm_multipolygons [Wraxall_and_Failand])
# AA<-osm_polygons (dat, id)
# poly<-AA[1,]
#
#
# ### Functions to plot osm data
#
# osm.plot(getbb("failand"))
# prettymap(osm.plot(getbb("failand")), scale.style="ticks", scale.tick.cex=0)
