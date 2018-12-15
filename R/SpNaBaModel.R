

#'
#' Class SpNaBaModel
#'
#' SpNaBaModel is used to store information about the naive Bayes model.
#'
#' @slot ModMatrix a SpNaBaMatrix. See \code{SpNaBaMatrix}.
#'
#' @slot Counts a SpNaBaCounts. See \code{SpNaBaCounts}.
#'
#' @slot Probs a SpNaBaProbs. See \code{SpNaBaProbs}.
#'
#' @slot Epsilon a SpNaBaEps. See \code{SpNaBaEps}.
#'
#' @slot Score a SpNaBaScore. See \code{SpNaBaScore}.
#'
#'
#' @name SpNaBaModel-class
#' @rdname SpNaBaModel-class
#' @exportClass SpNaBaModel
#' @include SpNaBaMatrix.R SpNaBaCounts.R SpNaBaProbs.R SpNaBaEps.R SpNaBaScore.R
#'

setClass(Class = "SpNaBaModel", slots = c(ModMatrix = "SpNaBaMatrix", Counts = "SpNaBaCounts",
                                          Probs = "SpNaBaProbs", Epsilon = "SpNaBaEps",
                                          Score = "SpNaBaScore"))

#' @rdname SpNaBaModel-class
#' @usage NULL
#'

setMethod("initialize", "SpNaBaModel",
          function(.Object, ModMatrix = "SpNaBaMatrix", Counts = "SpNaBaCounts",
                   Probs = "SpNaBaProbs", Epsilon = "SpNaBaEps", Score = "SpNaBaScore",
                   ...){

            .Object <- callNextMethod()

            # Checking if ModMatrix is missing -----------------------

            if(missing(ModMatrix)){
              stop("ModMatrix slot is required.")
            }else{
              if(!is(ModMatrix, "SpNaBaMatrix")){
                stop("ModMatrix must be SpNaBaMatrix object")
              }
            }

            # Checking if Counts is missing -----------------------

            if(missing(Counts)){
              stop("Counts slot is required.")
            }else{
              if(!is(Counts, "SpNaBaCounts")){
                stop("Counts must be SpNaBaCounts object")
              }
            }

            # Checking if Probs is missing -----------------------

            if(missing(Probs)){
              stop("Probs slot is required.")
            }else{
              if(!is(Probs, "SpNaBaProbs")){
                stop("Probs must be SpNaBaProbs object")
              }
            }

            # Checking if Epsilon is missing -----------------------

            if(missing(Epsilon)){
              stop("Epsilon slot is required.")
            }else{
              if(!is(Epsilon, "SpNaBaEps")){
                stop("Epsilon must be SpNaBaEps object")
              }
            }

            # Checking if ModMatrix is missing -----------------------

            if(missing(Score)){
              stop("Score slot is required.")
            }else{
              if(!is(Score, "SpNaBaScore")){
                stop("Score must be SpNaBaScore object")
              }
            }


            # Assign slots to the object -----------------------

            .Object@ModMatrix <- ModMatrix
            .Object@Counts <- Counts
            .Object@Probs <- Probs
            .Object@Epsilon <- Epsilon
            .Object@Score <- Score

            # Validate the object ------------------------------

            validObject(.Object)

            # Return object ------------------------------------

            return(.Object)
          })


#' @rdname SpNaBaModel-class
#' @export
#'
#' @param ModMatrix a SpNaBaMatrix. See \code{SpNaBaMatrix}.
#'
#' @param Counts a SpNaBaCounts. See \code{SpNaBaCounts}.
#'
#' @param Probs a SpNaBaProbs. See \code{SpNaBaProbs}.
#'
#' @param Epsilon a SpNaBaEps. See \code{SpNaBaEps}.
#'
#' @param Score a SpNaBaScore. See \code{SpNaBaScore}.
#'
#' @param ... argument pass to other methods
#'
#' @return An object of class \code{SpNaBaModel}. See slot section for more details.
#'
#' @examples
#'
#' # Using SpNaBaMatrix constructor ----------------------
#'
#' a <- matrix(rbinom(100, 100, runif(100, 0, 0.1)), nrow = 10, ncol = 10,
#'   dimnames = list(paste0("id:", 1:10), letters[1:10]))
#'
#' x.mat <- SpNaBaMatrix(freqmatrix = a)
#'
#' # Counts calculation ----------------------------------
#'
#' x.counts <- counts(x.mat)
#'
#' # Probability calculation -----------------------------
#'
#' x.probs <- probs(x.counts)
#'
#' # Estimating epsilon ----------------------------------
#'
#' x.eps <- epsilon(x.counts, x.probs)
#'
#' # Estimating Score
#'
#' x.score <- score(x.probs)
#'
#' x.model <- SpNaBaModel(x.mat, x.counts, x.probs,
#'                        x.eps, x.score)
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'

SpNaBaModel <- function(ModMatrix, Counts, Probs, Epsilon,
                        Score, ...){

  new("SpNaBaModel", ModMatrix, Counts, Probs, Epsilon,
      Score, ...)
}


#' @rdname SpNaBaModel-class
#' @usage NULL
#'
#' @importFrom utils head

setMethod("show", "SpNaBaModel",
          function(object){
            cat("An object of class 'SpNaBaModel'.\n")
            cat("slots:", slotNames(object), "\n",
                "names-target:", head(rownames(get_Ecx(object@Epsilon))),"...\n",
                "names-predictors:",  head(colnames(get_Ecx(object@Epsilon))),"...\n"
            )
          }
)



