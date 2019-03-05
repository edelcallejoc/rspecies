

#'
#' Class SpNaBaScore
#'
#' SpNaBaScore is used to store Score's parameters of the Naive Bayes model.
#'
#' @slot Apriori \code{vector} with \eqn{S} rows. Each element represents
#'    the apriori probability \eqn{log(P(C)/P(C')}. See
#'    \code{\link[rspecies]{score}}.
#'
#' @slot Scx \code{matrix} with \eqn{M} rows and \eqn{S} columns. Each element
#'    represents the Score function \eqn{S(X)}. See
#'    \code{\link[rspecies]{score}}.
#'
#' @name SpNaBaScore-class
#' @rdname SpNaBaScore-class
#' @exportClass SpNaBaScore
#'

setClass(Class = "SpNaBaScore", slots = c(Apriori = "numeric", Scx = "matrix"))

#' @rdname SpNaBaScore-class
#' @usage NULL
#'

setMethod("initialize", "SpNaBaScore",
          function(.Object, Apriori = "numeric", Scx = "matrix", ...){

            .Object <- callNextMethod()

            # Checking if Apriori is missing -----------------------

            if(missing(Apriori)){
              stop("Apriori slots is required.")
            }else{
              if(!is.numeric(Apriori)){
                stop("Apriori must be numeric.")
              }
            }

            # Checking if Scx is missing -----------------------

            if(missing(Scx)){
              stop("Scx slots is required.")
            }else{
              if(!is.numeric(Apriori)){
                stop("Scx must be numeric.")
              }
            }

            # Checking dimension--------------------------------

            if(ncol(Scx) != length(Apriori)){
              stop("Apriori slot must have the same number of columns as Scx slot")
            }

            # Assign slots to the object -----------------------

            .Object@Apriori <- Apriori
            .Object@Scx <- Scx

            # Validate the object ------------------------------

            validObject(.Object)

            # Return object ------------------------------------

            return(.Object)
          })


#' @rdname SpNaBaScore-class
#' @export
#'
#' @param Apriori numeric \code{vector}. See slot section for more details.
#'
#' @param Scx numeric \code{matrix}. See slot section for more details.
#'
#' @param ... argument pass to other methods.
#'
#' @return An object of class \code{SpNaBaScore}. See slot section for more details.
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
#' x.counts <- counts(x.mat, target = 1:3)
#'
#' # Probability calculation -----------------------------
#'
#' x.probs <- probs(x.counts)
#'
#' # Estimating epsilon ----------------------------------
#'
#' x.score <- score(x.probs)
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'

SpNaBaScore <- function(Apriori, Scx, ...){
  args <- c(Apriori = missing(Apriori), Scx = missing(Scx))

  if(any(args)){
    stop(paste(c("Argument(s)", paste(names(args)[args], collapse = ", "), "must be declared."), collapse = " "))
  }

  new("SpNaBaScore", Apriori = Apriori, Scx = Scx, ...)

}



#' @rdname SpNaBaScore-class
#' @usage NULL
#'
#' @importFrom utils head

setMethod("show", "SpNaBaScore",
          function(object){
            cat("An object of class 'SpNaBaScore'.\n")
            cat("slots: Apriori, Scx",
                "slots type:", " vector", " matrix", "\n",
                "names-target:", head(colnames(object@Scx)),"...\n",
                "names-predictors:",  head(rownames(object@Scx)),"...\n"
            )
          }
)

