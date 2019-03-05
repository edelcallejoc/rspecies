

#' Score function for the Spatial Naive Bayes Model
#'
#' @description \code{score} is a function to estimates score parameter for
#'     the Spatial Naive Bayes Model.
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @name score
#' @rdname score
#' @usage NULL
#' @exportMethod score
#'

setGeneric("score", function(x.probs) standardGeneric("score"))


#' @rdname score
#' @aliases score,SpNaba-methods,score-methods
#'
#' @param x.probs A \code{\link[rspecies]{SpNaBaProbs}} object.
#'
#' @return \code{score}: An object of class \code{\link[rspecies]{SpNaBaScore}}.
#'
#' @seealso \code{\link[rspecies]{SpNaBaProbs}}, \code{\link[rspecies]{SpNaBa-methods}}
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
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals)
#'
#' # Without target declaration --------------------------
#'
#' system.time(x.counts <- counts(x.mat))
#'
#' # With target declaration -----------------------------
#'
#' x.counts <- counts(x.mat, target = 1:10)
#'
#' # Probability matrices -----------------------------------
#' x.probs <- probs(x.counts, fac.lap = 0.1)
#'
#' # Score parameter -----------------------------------
#'
#' x.score <- score(x.probs)
#'


setMethod("score", c("SpNaBaProbs"),
          function(x.probs){

            # Extracting Ps ----------------------------------

            Pc <- get_Pc(x.probs)
            Pcdim <- length(Pc)

            Pcx <- get_Pcx(x.probs)

            Pxc <- get_Pxc(x.probs)

            Pxnc <- get_Pxnc(x.probs)

            # Calculating the epsilon parameters -------------

            Apriori <- log(Pc/(1-Pc))

            Scx <- log(Pxc/Pxnc)

            output <- SpNaBaScore(Apriori = Apriori, Scx = Scx)

            return(output)
          })



