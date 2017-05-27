
#' Generate a grid from SpatialPolygonsDataFrame.
#'
#' @description The grid is generated from pseudo mercator
#'     or web mercator (epsg:3857) projection system. It is recomended
#'     to use metrics projection in order to preserve areas. First,
#'     transform mappol into web mercator projection. Then, the distances
#'     between the four coordinates given by the bounding box are calculated
#'     and divided by resolution argument in order to determine the number of
#'     cells per direction. Finally, \code{\link[sp]{GridTopology}} is applied to
#'     generate the coordinates that create the final objecto of class
#'     \code{SpatialPolygonsDataFrame}.
#'
#' @param mappol A \code{SpatialPolygonsDataFrame} object. \code{bbox(mappol)}
#'     should be different from NULL. See \code{help(bbox)} for more details.
#' @param proj4trans A \code{CRS} class object. By default \code{CRS('+init=3857')}.
#'     se \code{help(CRS)} for more details. See \url{http://spatialreference.org/}
#'     for more projection system. It is recomended to use a metric
#'     projection.
#' @param tol argument pass to \code{gSimplify()}. See \code{help(gSimplify)}
#'     for more details.
#' @param TP argument pass to \code{gSimplify()}. See \code{help(gSimplify)}
#'     for more details.
#' @param resolution integer. Greater than 0. Units are in Km. Default 20.
#' @param r.ceiling logical. If \code{TRUE} the distances are ceiling rounded.
#'     If \code{FALSE} the distances are floor rounded.
#' @param g.int logical. If \code{TRUE} the grid's cells that intersects \code{mappol}
#'     are returned. If \code{FALSE} all grid's cells in the \code{mappol} bounding box
#'     are returned.
#' @param pretty.int logical. If \code{g.int=TRUE} and \code{pretty.int=TRUE} the
#'     grid's cells that intersects \code{mappol}'s boundaries are reshape for
#'     better visualization. Requires \code{g.int=TRUE}. This option expend more
#'     computing time.
#' @param info logical. If \code{TRUE} cell's info is printed. By default is \code{FALSE}.
#'
#' @return A \code{SpatialPolygonsDataFrame} object. It includes all the polygons
#'     that form the grid. Data slot contains the identifiers for each cells.
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' data(Mex0)
#'
#' # Whithout pretty intersection
#' # Default resolution is 20 km.
#'
#' system.time(Mex0.grd<-grd_build(Mex0))
#' plot(Mex0.grd)
#' plot(Mex0, add=T)
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
#' system.time(Mex0.grd.p<-grd_build(Mex0, pretty.int=T))
#' plot(Mex0.grd.p)
#'
#' @details This function generates a grid from \code{SpatialPolygonsDataFrame}.
#'     The main steps are: projection system transformation, geometry
#'     simplification, bounding box extraction, distance calculation,
#'     calculation of cells per direction (Longitude, Latitude), grid
#'     topology creation, class reassigment of grid topology into
#'     \code{SpatialPolygons} object and compute the intersected polygons between
#'     grid and \code{mappol}. Finally, the returned object is a
#'     \code{SpatialPolygonsDataFrame}.
#'
#'     \code{\link[sp]{spTransform}} (see \code{help(spTransform)} for more details)
#'     is applied for transformation of mappol into a metric projection
#'     (by default web mercator, epsg:3857). The returned object in this
#'     step is a \code{SpatialPolygonsDataFrame} called \code{mappol.tr}
#'
#'     \code{\link[rgeos]{gSimplify}} (see \code{help(gSimplify)} for more details)
#'     is applied for simplify geometry of the transformed mappol in order
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
#' @export
#'
#' @import sp rgdal rgeos maptools methods


grd_build<-function(mappol, proj4trans = sp::CRS("+init=epsg:3857"),
                                tol = 100, TP = TRUE, resolution = 20,
                                r.ceiling = TRUE, g.int = TRUE, pretty.int = FALSE,
                                info = FALSE){

    # Arguments' validation ---------------------------------------------

    if (class(mappol) != "SpatialPolygonsDataFrame") {
        stop("Argument mappol must be SpatialPolygonsDataFrame.")
    }
    logical_args <- list(TP = TP, r.ceiling = r.ceiling,
                         g.int = g.int, pretty.int = pretty.int,
                         info = info)
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

    projmappol <- rgdal::showEPSG(sp::proj4string(mappol))
    projcompare <- (projmappol == rgdal::showEPSG(rgdal::CRSargs(proj4trans)))

    if (!projcompare) {
        mappol.tr <- sp::spTransform(mappol, proj4trans)
        mappol.trsim <- rgeos::gSimplify(mappol.tr, tol = tol, topologyPreserve = TRUE)
    } else {
        mappol.trsim <- rgeos::gSimplify(mappol, tol = tol, topologyPreserve = TRUE)
    }

    # bounding box and distance calculation -----------------------------

    bb <- sp::bbox(mappol.trsim)

    bb.sp <- matrix(c(bb["x", "min"], bb["y", "max"], bb["x", "min"], bb["y", "min"], bb["x",
        "max"], bb["y", "max"], bb["x", "max"], bb["y", "min"]), 4, 2, byrow = T, dimnames = list(c("NO",
        "SO", "NE", "SE"), c("X", "Y")))

    bb.points <- sp::SpatialPoints(bb.sp, proj4string = CRS(proj4string(mappol.trsim)))

    bb.dist <- (sp::spDists(bb.points, longlat = F)/1000)  # se divide entre mil para pasarlo a kilometros
    dimnames(bb.dist) <- list(c("NO", "SO", "NE", "SE"), c("NO", "SO", "NE", "SE"))


    dist.longlat <- (bb.dist/resolution)[1, c(3, 2)]

    # Creating a GridTopoly object --------------------------------------

    cs <- as.vector(diff(t(bb))/dist.longlat)  # cell size.
    cc <- bb[, 1] + (cs/2)  # cell offset.
    if (r.ceiling) {
        cd <- ceiling(dist.longlat)  # number of cells per direction
    } else {
        cd <- floor(dist.longlat)
    }
    mappol.grd <- sp::GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)

    sp_pol <- sp::as.SpatialPolygons.GridTopology(mappol.grd, proj4string = CRS(proj4string(mappol.trsim)))

    # Cells info -------------------------------------------------------

    cs.km <- cs/1000

    cel.inf <- round(c(cs.km, prod(cs.km)), digits = 2)

    # Extracting intersections ------------------------------------------

    if (g.int == T) {
        intg <- rgeos::gIntersects(mappol.trsim, sp_pol, byid = T)
        sp_pol_int <- sp_pol[intg]

        if (pretty.int) {
            contg <- rgeos::gContains(mappol.trsim, sp_pol_int, byid = T)
            sp_pol_aux <- sp_pol_int[!contg]
            int.pol <- rgeos::gIntersection(mappol.trsim, sp_pol_aux, byid = T)
            mappol_grd <- maptools::spRbind(sp_pol_int[contg], int.pol)
        } else {
            mappol_grd <- sp_pol_int
        }
    } else {
        mappol_grd <- sp_pol
    }

    # Transforming to the original projection system --------------------

    mappol_grd <- sp::spTransform(mappol_grd, sp::proj4string(mappol))

    mappol.df <- data.frame(ID = 1:length(mappol_grd), row.names = sapply(mappol_grd@polygons,
        methods::slot, name = "ID"))

    mappol_grd <- sp::SpatialPolygonsDataFrame(mappol_grd, mappol.df, match.ID = TRUE)

    # Print information message -----------------------------------------

    if (info) {
        cat(paste("Cells info: longitud distance=", cel.inf[1], ", latitud distance=",
            cel.inf[2], ", Area=", cel.inf[3], ". Units=KM.", sep = ""))
        cat(paste("All metrics were compute under EPSG:", showEPSG(CRSargs(proj4trans)),
            " projection system.", sep = ""))
    }

    # Return SpatialPolygonsDataFrame object ----------------------------

    return(mappol_grd)

    # -------------------------------------------------------------------
}
