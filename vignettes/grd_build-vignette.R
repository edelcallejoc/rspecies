## ---- echo=TRUE, results='hide', message=FALSE---------------------------

# loading libaries ------------------------------------------

library(rspecies)
library(sp)
library(rgdal)
library(raster)

## ---- echo=TRUE, results='asis'------------------------------------------
# loading data from rspecies package ------------------------
data(Mex0)
class(Mex0)
slotNames(Mex0)

# Obtaining data from GADM using getData() from raster package

Fra0<-raster::getData('GADM', country='FRA', level=0)
class(Fra0)
slotNames(Fra0)

## ---- echo=TRUE, fig.show='hold'-----------------------------------------
# ploting both polygons -------------------------------------
plot(Mex0)
plot(Fra0)

## ---- echo=TRUE, fig.show='asis', fig.align='center'---------------------

library(leaflet)

# Making the map --------------------------------------------

leaflet(width = "100%") %>%
addProviderTiles('OpenStreetMap.BlackAndWhite',
 options = providerTileOptions(noWrap = TRUE))%>%
addPolygons(data=Mex0, stroke=TRUE, color = '#2ca25f',
layerId = Mex0, weight = 1, opacity = 1,
fillColor = '#99d8c9', fillOpacity = 0.6)

## ---- echo = TRUE, results='hold'----------------------------------------

# Obtaining geographical projection system -----------------------

proj<-sp::proj4string(Mex0)
proj

# Obtainin EPSG code ---------------------------------------------

rgdal::showEPSG(proj)

## ---- echo = TRUE, results='hold'----------------------------------------

# Obtaining EPSG:3857 definition -----------------------

proj4trans <- sp::CRS("+init=epsg:3857")

# Transforming the polygon -----------------------------

Mex0_trans <- sp::spTransform(Mex0, proj4trans)
sp::proj4string(Mex0_trans)

## ---- echo = TRUE, results='asis'----------------------------------------

# Obtaining the bounding box -----------------------

bb <- bbox(Mex0_trans)

# Transforming bb into a expand matrix -------------

bb_points <- matrix(c(bb["x", "min"], bb["y", "max"], bb["x", "min"], bb["y", "min"], bb["x","max"], bb["y", "max"], bb["x", "max"], bb["y", "min"]), 4, 2, byrow = T, dimnames = list(c("NW","SW", "NE", "SE"), c("X", "Y")))

## ---- echo = FALSE, results='asis'---------------------------------------
knitr::kable(bb_points)

## ---- echo = TRUE, results='asis', fig.show='asis'-----------------------
# Transforming bb_points into a SpatialPoints object
# with the same projection system as Mex0_trans. ---

bb_sp <- sp::SpatialPoints(bb_points, proj4string = sp::CRS(sp::proj4string(Mex0_trans)))

plot(Mex0_trans)
plot(bb_sp, pch = 20, col ="red", add = T)
text(bb_points, labels = rownames(bb_points), pos = c(1,3,1,3), cex = 0.5)

## ---- echo = TRUE, results='asis'----------------------------------------
# Claculating distance between points --------------

bb_dist <- (sp::spDists(bb_sp, longlat = F)/1000) # Divided by a thousand to get 
                                                  # the unit in kilometers
dimnames(bb_dist) <- list(rownames(bb_points), rownames(bb_points))

## ---- echo = FALSE, results='asis'---------------------------------------
knitr::kable(bb_dist)

## ---- echo = TRUE, results='hold'----------------------------------------
resolution <- 20 # fix the resolution to 20

dist_longlat <- (bb_dist/resolution)[1, c(3, 2)] # calculation number of cel per
                                                 # dirección.
dist_longlat

## ---- echo = TRUE, results='hold'----------------------------------------
cs0 <- diff(t(bb))/dist_longlat  # cell size. without rounding
cs1 <- diff(t(bb))/floor(dist_longlat)  # cell size. with floor function

cs0
cs1

## ---- echo = TRUE, results='hold'----------------------------------------
cc0 <- bb[, 1] + (cs0/2)  # cell offset.
cc0

cc1 <- bb[, 1] + (cs1/2)  # cell offset.
cc1

## ---- echo = TRUE, fig.show='asis', fig.align='center', fig.width=6, fig.height=6----
# Creating the GRID Topology ------------------------
pol_grd <- sp::GridTopology(cellcentre.offset = as.vector(cc0), cellsize = as.vector(cs0), cells.dim = floor(dist_longlat)) # for exact resolution construction

# Transforming into SpatialPolygons -----------------
sp_pol <- sp::as.SpatialPolygons.GridTopology(pol_grd, proj4string = CRS(proj4string(Mex0_trans)))

# View grid with plot ----------------------------------------

plot(Mex0_trans, col = "red")
plot(sp_pol, add = TRUE)

## ---- echo = TRUE, fig.show='asis', fig.align='center'-------------------

# Cropping the GRID -------------------------------------
intg <- rgeos::gIntersects(Mex0_trans, sp_pol, byid = T)
sp_pol_int <- sp_pol[intg]


# Transforming to the original projection system --------

Mex0_grd <- sp::spTransform(sp_pol_int, proj4string(Mex0))

Mex0_df <- data.frame(ID = 1:length(Mex0_grd), row.names = sapply(Mex0_grd@polygons,
        methods::slot, name = "ID"))

Mex0_grd <- sp::SpatialPolygonsDataFrame(Mex0_grd, Mex0_df, match.ID = TRUE)

# Leaflet -----------------------------------------------
leaflet(width = "100%") %>%
addProviderTiles('OpenStreetMap.BlackAndWhite',
 options = providerTileOptions(noWrap = TRUE))%>%
addPolygons(data=Mex0_grd, stroke=TRUE, color = '#2ca25f',
layerId = Mex0_grd, weight = 1, opacity = 1,
fillColor = '#99d8c9', fillOpacity = 0.6)

## ---- echo = TRUE, results='asis', fig.show='asis', fig.align='center'----

# without pretty options --------------------------------------------
system.time(Mex0_grd1<-grd_build(Mex0)) # creating the GRID with 20Km resolution

# with pretty options -----------------------------------------------
system.time(Mex0_grd2<-grd_build(Mex0, pretty.int=T))

# leaflet interaction -----------------------------------------------

m1 <- leaflet(width = "100%") %>%
addProviderTiles('OpenStreetMap.BlackAndWhite',
 options = providerTileOptions(noWrap = TRUE))%>%
addPolygons(data=Mex0_grd1, stroke=TRUE, color = '#2ca25f',
layerId = Mex0_grd1, weight = 1, opacity = 1,
fillColor = '#99d8c9', fillOpacity = 0.6)
m1

m2 <- leaflet(width = "100%") %>%
addProviderTiles('OpenStreetMap.BlackAndWhite',
 options = providerTileOptions(noWrap = TRUE))%>%
addPolygons(data=Mex0_grd2, stroke=TRUE, color = '#2ca25f',
layerId = Mex0_grd2, weight = 1, opacity = 1,
fillColor = '#99d8c9', fillOpacity = 0.6)
m2

