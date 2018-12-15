
#' Generate a grid from SpatialPolygonsDataFrame.
#'
#' @description The function generates a grid from pseudo mercator or web Mercator
#'     (epsg:3857) projection system. It is recommended to use metrics projection
#'     to preserve areas. First, transform mappol into web Mercator projection.
#'     Then, the distances between the four coordinates given by the bounding box
#'     are calculated and divided by resolution argument to determine the number
#'     of cells per direction. Finally, \code{\link[sp]{GridTopology}} is applied
#'     to generate the coordinates that create the final object of class
#'     \code{SpatialPolygonsDataFrame}.
#'
#' @param mappol A \code{SpatialPolygonsDataFrame} object. \code{bbox(mappol)}
#'     should be different from NULL. See \code{help(bbox)} for more details.
#' @param keep.p4s Logical. If \code{TRUE} (by default) the final grid polygon keep
#'     proj4string of original polygon. If \code{FALSE} the final grid polygon
#'     modify the proj4string to \code{sp::CRS("+init=epsg:3857")}.
#' @param tol Argument pass to \code{gSimplify()}.  By default \code{Tol = 100}.
#'     See \code{help(gSimplify)} for more details.
#' @param TP Argument pass to \code{gSimplify()}. This directs to topologyPreserve
#'     argument in \code{gSimplify} function. See \code{help(gSimplify)} for
#'     more details.
#' @param resolution Double greater than 0. Units are in Km. Default \code{resolution = 20}.
#' @param r.ceiling Logical. If \code{TRUE} the distances are ceiling rounded.
#'     If \code{FALSE} the distances are floor rounded.
#' @param g.int Logical. If \code{TRUE} the grid's cells that intersects \code{mappol}
#'     are returned. If \code{FALSE} all grid's cells in the \code{mappol} bounding box
#'     are returned.
#' @param pretty.int Logical (\code{FALSE} by default). If \code{g.int=TRUE} and
#'     \code{pretty.int=TRUE} the grid's cells that intersects \code{mappol}'s
#'     boundaries are reshape for better visualization. Requires \code{g.int=TRUE}.
#'     This option expend more computing time.
#'
#' @return A \code{SpatialPolygonsDataFrame} object. It includes all the polygons
#'     that form the grid. Data slot contains the identifiers for each cells.
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' library(rgdal)
#' library(raster)
#' data(Mex0)
#'
#' # Whithout pretty intersection
#' # Default resolution is 20 km.
#'
#' system.time(Mex0.grd<-grd_build(Mex0))
#' plot(Mex0.grd)
#' plot(Mex0, add = TRUE)
#'
#' # ggplot2 interaction
#'
#' library(ggplot2)
#' library(mapproj)
#'
#' ggplot(data = fortify(Mex0.grd), aes(x = long, y = lat, group = group)) +
#'     geom_polygon(colour = "white") +
#'     coord_quickmap()
#'
#' # leaflet interaction
#'
#' library(leaflet)
#' leaflet() %>%
#'     addProviderTiles('OpenStreetMap.Mapnik',
#'     options = providerTileOptions(noWrap = TRUE))%>%
#'     addPolygons(data=Mex0.grd, stroke=TRUE, color = '#FFFFFF',
#'     layerId = Mex0.grd@data$ID, weight = 1, opacity = 0.3,
#'     fillColor = '#A9A9A9', fillOpacity = 0.6,
#'     popup = row.names(Mex0.grd@data))
#'
#' # With Pretty options
#'
#' system.time(Mex0.grd.p<-grd_build(Mex0, pretty.int=TRUE))
#' plot(Mex0.grd.p)
#'
#' # ggplot2 interaction
#'
#' ggplot(data = fortify(Mex0.grd.p), aes(x = long, y = lat, group = group)) +
#'     geom_polygon(colour = "white") +
#'     coord_map(projection = "mercator")
#'
#' @details This function generates a grid from \code{SpatialPolygonsDataFrame}
#'     object. The main steps are: projection system transformation, geometry
#'     simplification, bounding box extraction, distance calculation,
#'     calculation of cells per direction (Longitude, Latitude), grid
#'     topology creation, class reassignment of grid topology into
#'     \code{SpatialPolygons} object and compute the intersected polygons between
#'     grid and \code{mappol}. Finally, the returned object is a
#'     \code{SpatialPolygonsDataFrame} object.
#'
#'     \code{\link[sp]{spTransform}} (see \code{help(spTransform)} for more details)
#'     is applied for transformation of mappol into a metric projection
#'     (by default web mercator, epsg:3857).
#'
#'     \code{\link[rgeos]{gSimplify}} (see \code{help(gSimplify)} for more details)
#'     is applied for simplify geometry of transformed polygon in order
#'     to reduce computation time and memory size.
#'
#'     \code{\link[sp]{bbox}} (see \code{help(bbox)} for more details) is applied to
#'     extract the bounding box of the transformed and simplify \code{mappol}.
#'     \code{\link[sp]{spDists}} (see \code{help(spDists)} for more details) is applied
#'     for distances calculation. \code{\link[base]{ceiling}}  and
#'     \code{\link[base]{floor}} is applied for rounding options.
#'
#'     \code{\link[sp]{GridTopology}} (see \code{help(GridTopology)} for more details)
#'     is applied to generate the grid. Then, \code{\link[sp]{as.SpatialPolygons.GridTopology}}
#'     is applied to transform the \code{GridTopology} object into
#'     \code{SpatialPolygonsDataFrame}.
#'
#'     Finally, \code{\link[rgeos]{gIntersects}} (see \code{help(gIntersects)}
#'     for more details) is applied to extract the grid's cells that intersects
#'     \code{mappol}. \code{\link[rgeos]{gContains}} and \code{\link[rgeos]{gIntersection}}
#'     (see \code{help(gContains)} and \code{help(gIntersection)} for more details)
#'     is applied to extract the grid's cells that intersects \code{mappol} and generate
#'     a pretty visualization. The latter method may require considerable computing time
#'     depending on the number of coordinates contained in the \code{mappol} polygon.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @docType methods
#' @export
#'
#' @import sp rgdal rgeos maptools methods
#' @importFrom raster compareCRS


grd_build<-function(mappol, keep.p4s = TRUE, tol = 100, TP = TRUE, resolution = 20,
                    r.ceiling = TRUE, g.int = TRUE, pretty.int = FALSE){

    # Arguments' validation ---------------------------------------------

    if (class(mappol) != "SpatialPolygonsDataFrame") {
        stop("Argument mappol must be SpatialPolygonsDataFrame.")
    }
    logical_args <- list(keep.p4s = keep.p4s, TP = TP, r.ceiling = r.ceiling,
                         g.int = g.int, pretty.int = pretty.int)
    logical_val <- unlist(lapply(logical_args, is.logical))
    if (!all(logical_val)) {
        stop(paste("Argument ", names(logical_args)[!logical_val],
                  " should be logical type.\n ", sep =""))
    }
    numeric_args <- list(tol = tol, resolution = resolution)
    numeric_val <- unlist(lapply(numeric_args, function(x){(is.numeric(x) & x>0)}))
    if (!all(numeric_val)) {
      stop(paste("Argument ", names(numeric_args)[!numeric_val],
                 " should be numeric type and greater than 0.\n ", sep =""))
    }
    # Transform to a metric projection system ---------------------------
    # and simplying spatial geometry ------------------------------------

    proj4trans <- sp::CRS("+init=epsg:3857")
    projmappol <- sp::proj4string(mappol)
    projcompare <- raster::compareCRS(projmappol, proj4trans)

    if (projcompare) {
      mappol.trsim <- rgeos::gSimplify(mappol, tol = tol, topologyPreserve = TRUE)
    } else {
        mappol.tr <- sp::spTransform(mappol, proj4trans)
        mappol.trsim <- rgeos::gSimplify(mappol.tr, tol = tol, topologyPreserve = TRUE)
    }

    # bounding box and distance calculation -----------------------------

    bb <- sp::bbox(mappol.trsim)

    bb.dist <- diff(t(bb))

    dist.longlat <- (bb.dist/1000)/resolution # Number of blocks per side
                    # Divided by 1000 in order to convert the units in Km

    # Creating a GridTopoly object --------------------------------------

    cs <- as.vector(bb.dist/dist.longlat)  # cell size.
    cc <- bb[, 1] + (cs/2)  # cell offset.
    if (r.ceiling) {
        cd <- ceiling(dist.longlat)  # number of cells per direction
    } else {
        cd <- floor(dist.longlat)
    }
    mappol.grd <- sp::GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)

    sp_pol <- sp::as.SpatialPolygons.GridTopology(mappol.grd, proj4string = CRS(proj4string(mappol.trsim)))

   # Extracting intersections ------------------------------------------

    if (g.int) {
        intg <- rgeos::gIntersects(mappol.trsim, sp_pol, byid = TRUE)
        sp_pol_int <- sp_pol[apply(intg,1,any)]

        if (pretty.int) {
            contg <- rgeos::gContains(mappol.trsim, sp_pol_int, byid = TRUE)
            sp_pol_aux <- sp_pol_int[!contg]
            int.pol <- rgeos::gIntersection(mappol.trsim, sp_pol_aux, byid = TRUE)
            mappol_grd <- maptools::spRbind(sp_pol_int[contg], int.pol)
        } else {
            mappol_grd <- sp_pol_int
        }
    } else {
        mappol_grd <- sp_pol
    }

    # Transforming to the original projection system --------------------

    if(keep.p4s){
    mappol_grd <- sp::spTransform(mappol_grd, sp::proj4string(mappol))
    }

    # Converting into SpatialPolygonsDataFrame --------------------------
    id_names <- sapply(mappol_grd@polygons, methods::slot, name = "ID")

    mappol.df <- data.frame(id = id_names, row.names = id_names, stringsAsFactors = FALSE)

    mappol_grd <- sp::SpatialPolygonsDataFrame(mappol_grd, mappol.df, match.ID = TRUE)

    # Return SpatialPolygonsDataFrame object ----------------------------

    return(mappol_grd)

    # -------------------------------------------------------------------
}
