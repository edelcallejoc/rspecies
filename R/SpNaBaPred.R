

#' Class SpNaBaPred
#'
#' @include SpNaBaMatrix.R SpNaBaCounts.R SpNaBaProbs.R SpNaBaEps.R SpNaBaScore.R
#' @include SpNaBaModel.R
#'
#' @name SpNaBaPred-class
#'
#' @description This is a class associated to prediction methods applied to
#'     SpNaBaModel objects (See \code{\link[rspeciesdev]{predict}}).
#'
#' @slot Prediction a matrix object.
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}).
#'
#' @seealso \code{\link[rspeciesdev]{grd_build}}, \code{\link[rspeciesdev]{id_pts}}
#'     \code{\link[rspeciesdev]{NBModel}}, \code{\link[rspeciesdev]{predict}}
#'
#'
#' @include SpNaBaMatrix.R SpNaBaCounts.R SpNaBaProbs.R
#' @include SpNaBaEps.R SpNaBaScore.R SpNaBaModel.R NBModel.R
#'
#' @exportClass SpNaBaPred


setClass(Class = "SpNaBaPred",
         slots = c(Prediction = "matrix"))


#' @rdname SpNaBaPred-class
#' @usage NULL
#'
#'
#'

setMethod("initialize", "SpNaBaPred",
          function(.Object, Prediction = "matrix", ...){

            .Object <- callNextMethod()

            # Checking if Prediction is missing -----------------------

            if(missing(Prediction)){
              stop("Prediction slot is required.")
            }

            # Assign slots to the object -----------------------

            .Object@Prediction <- Prediction

            # Validate the object ------------------------------

            validObject(.Object)

            # Return object ------------------------------------

            return(.Object)
          })


#' @rdname SpNaBaPred-class
#' @export
#'
#' @param Prediction a matrix. See \code{predict}.
#'
#' @param ... pass to other methods.
#'
#' @return An object of class \code{SpNaBaPred}. See slot section for more details.
#'


SpNaBaPred <- function(Prediction, ...){
  new("SpNaBaPred", Prediction, ...)
}


#' @rdname SpNaBaPred-class
#' @usage NULL
#'
#' @importFrom utils head

setMethod(f = "show",
          signature = "SpNaBaPred",
          definition = function(object){
            cat("An object of class 'SpNaBaPred'.\n")
            cat("Slot: Prediction\n", "Type:",  class(object@Prediction), "\n",
                "target: ", colnames(object@Prediction),"\n",
                "dimension:", dim(object@Prediction), "\n")}
)

