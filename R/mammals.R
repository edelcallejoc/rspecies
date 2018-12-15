
#' Coordinates of 443 mammals' species from Mexico.
#'
#' A \code{SpatialPointsDataFrame} object. It contains 58439 coordinates
#' from 443 species of mammals in Mexico. See \code{mammals@data$nameID}
#' for exact species' names.
#'
#' @source This data is a sample data from CONABIO species' ocurrence database.
#'     Dr. Constantino Gonzalez provided the data with a proper preparation and
#'     clean.
#' @format A SpatialPointsDataFrame object whit 5 slots:
#'     \describe{
#'         \item{data}{A data.frame object with 58439 row and 1 column (\code{nameID}).
#'             \code{nameID} contains the scientific name for each species and
#'             proper association whit it's coordinate.}
#'         \item{coords.nrs}{\code{numeric}. See \code{SpatialPointsDataFrame} class
#'             documentation.}
#'         \item{coords}{A \code{matrix} object. 58439 rows and 2 columns (\code{LONGITUD},
#'             \code{LATITUD}).}
#'         \item{bbox}{A matrix object with 2 rows (Longitude (\code{x}), Latitude (\code{y}))
#'             and 2 columns (\code{min}, \code{max}). See \code{\link[sp]{bbox}}}
#'         \item{proj4string}{A CRS object. See \code{\link[sp]{CRS}}}
#'     }
#'
"mammals"
