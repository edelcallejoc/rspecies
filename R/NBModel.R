

#' Function to fit Spatial Naive Bayes Model
#'
#' @description \code{NBModel} is a function to fit Spatial Naive Bayes Model.
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @name NBModel
#' @rdname NBModel
#' @exportMethod NBModel
#'
#' @include grd_build.R id_pts.R SpNaBaMatrix.R SpNaBaCounts.R SpNaBaProbs.R SpNaBaEps.R SpNaBaScore.R
#'

setGeneric("NBModel", function(points, map, target = NULL,
                               fac.lap = NULL, isgrid = FALSE,
                               contain, ...) standardGeneric("NBModel"))


#' @rdname NBModel
#' @aliases NBModel,SpNaBa-methods,NBModel-methods
#' @usage NULL
#'
#' @param points A \code{\link[sp]{SpatialPointsDataFrame-class}} object or
#'     a \code{\link[rspecies]{SpNaBaMatrix-class}} object.
#'
#' @param map A \code{\link[sp]{SpatialPointsDataFrame-class}} object or NULL. If NULL, \code{points}
#'     argument must be a \code{\link[rspecies]{SpNaBaMatrix-class}} object.
#'
#' @param target \code{character} or \code{numeric} or \code{logical} or NULL. Indicates the columns
#'     of binmatrix slot to be consired as target factors.
#'
#' @param fac.lap \code{numeric} or \code{NULL}. it indicates the laplace factor to be applied.
#'     If \code{NULL} or \code{missing} the laplace factor applied is 0.01.
#'
#' @param isgrid logical (\code{FALSE} by default). If is TRUE idicates that the map is gridded,
#'     if is FALSE the grid is calculated based on the bounding box of the map.
#'
#' @param contain Character (\code{simple} or \code{properly}).
#'     By default \code{simple}, it implies a simple contention
#'     evaluation (see \code{\link[rgeos]{gContains}})).
#'     \code{properly} suggests a properly contention evaluation
#'     (see \code{\link[rgeos]{gContainsProperly}})). It requieres that
#'     points argument should be a \code{\link[sp]{SpatialPointsDataFrame}} object
#'     and map argument should be declared as a \code{\link[sp]{SpatialPointsDataFrame-class}} object.
#'
#' @param ... Arguments pass to \code{grd_build} function. See \code{\link[rspecies]{grd_build}}.
#'
#' @return \code{NBModel}: An object of class \code{\link[rspecies]{SpNaBaModel}}.
#'
#' @seealso \code{\link[rspecies]{SpNaBaMatrix-class}}, \code{\link[rspecies]{SpNaBaCounts}},
#'     \code{\link[rspecies]{SpNaBaProbs}}, \code{\link[rspecies]{SpNaBa-methods}},
#'     \code{\link[rspecies]{SpNaBaEps}}, \code{\link[rspecies]{epsilon}},
#'     \code{\link[rspecies]{SpNaBaScore}}, \code{\link[rspecies]{score}}
#'
#'
#' @examples
#' # Using the function id_pts() for spatial data --------
#'
#' library(sp)
#' library(rgeos)
#' library(rgdal)
#' library(raster)
#'
#' # Loading data
#' data(Mex0)
#' data(mammals)
#'
#' # Using an SpatialPointsDataFrame and
#' # SpatialPolygonsDataFrame object
#'
#' x.model <- NBModel(mammals, Mex0, target = 1:10, fac.lap = 0.01)
#'


setMethod("NBModel", c("SpatialPointsDataFrame", "SpatialPolygonsDataFrame",
                       "ANY", "ANY", "ANY", "ANY"),
          function(points, map, target = NULL, fac.lap = NULL,
                   isgrid = FALSE, contain = "simple", ...){

          # if map is not gridded

            if(missing(isgrid)){
              isgrid <- FALSE
            }

            if(!isgrid){
               map <- grd_build(map, ...)
            }

          # Calculating intersection

            if(missing(contain)){
              contain <- "simple"
            }

            x.mat<-id_pts(grd = map, pts = points, contain = contain)

          # Calculating counts

            if(missing(target) | is.null(target)){
              target <- NULL
            }

            x.counts <- counts(x.mat, target = target)

          # Calculating Probabilities

            if(missing(fac.lap) | is.null(fac.lap)){
              fac.lap <- 0.01
            }

            x.probs <- probs(x.counts, fac.lap = fac.lap)

          # Calculating Epsilon

            x.eps <- epsilon(x.counts, x.probs)

          # Calculating Score

            x.score <- score(x.probs)

          # Saving the output

            output <- SpNaBaModel(x.mat, x.counts, x.probs, x.eps, x.score)

            return(output)
          })




#' @rdname NBModel
#' @aliases NBModel,SpNaBa-methods,NBModel-methods
#' @usage NULL
#'
#' @examples
#'
#' # Using an SpNaBaMAtrix object
#'
#' # Grid
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals)
#'
#' x.model <- NBModel(x.mat, target = 1:10, fac.lap = 0.01)
#'

setMethod("NBModel", c("SpNaBaMatrix", "ANY",
                       "ANY", "ANY", "ANY", "ANY"),
          function(points, map, target = NULL, fac.lap = NULL,
                   isgrid = FALSE, contain = "simple", ...){

            # Calculating counts

            if(missing(target) | is.null(target)){
              target <- NULL
            }

            x.counts <- counts(points, target = target)

            # Calculating Probabilities

            if(missing(fac.lap) | is.null(fac.lap)){
              fac.lap <- 0.01
            }

            x.probs <- probs(x.counts, fac.lap = fac.lap)

            # Calculating Epsilon

            x.eps <- epsilon(x.counts, x.probs)

            # Calculating Score

            x.score <- score(x.probs)

            # Saving the output

            output <- SpNaBaModel(points, x.counts, x.probs, x.eps, x.score)

            return(output)
          })
