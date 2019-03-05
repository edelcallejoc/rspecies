
#' Polygon of Mexico whithout politic's division.
#'
#' This polygon was extracted form \url{http://www.gadm.org/} using
#' the function \code{\link[raster]{getData}} from \code{raster} package.
#'
#' @source \code{getData('GADM', country='MEX', level=0)}
#' @format A SpatialPolygonsDataFrame object whit 5 slots:
#'     \describe{
#'         \item{data}{A data.frame object with 1 row and 68 variables}
#'         \item{polygons}{A list object with 5 slots: Polygons, plotOrder,
#'             labpt, ID and area}
#'         \item{plotOrder}{An integer object with 1 element}
#'         \item{bbox}{A matrix object with 2 rows (Longitude (\code{x}), Latitude (\code{y}))
#'             and 2 columns (\code{min}, \code{max}). See \code{\link[sp]{bbox}}}
#'         \item{proj4string}{A CRS object. See \code{\link[sp]{CRS-class}}}
#'     }
#'
#' @seealso \code{\link[raster]{getData}} for more details.
"Mex0" 
