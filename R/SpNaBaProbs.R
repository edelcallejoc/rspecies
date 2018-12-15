
#' Class SpNaBaProbs
#'
#' SpNaBaProbs is used to store probability information of the Naive Bayes model.
#'
#' @slot Pc numeric \code{vector}. It contains the marginal probabilities of the
#'     target variables declared in the NaBa model.
#'
#' @slot Px numeric \code{vector}. It contains the marginal probabilities of the
#'     predictor variables declared in the NaBa model.
#'
#' @slot Pc.x numeric \code{matrix}. Each element represents the condicional
#'     probability \eqn{P(c|x)}. The sufix \code{c.x} implies that the rows of this
#'     matrix represents the target variables (c) and the columns represents the
#'     predictors variables (x) declared in the Naive Bayes model.
#'
#' @slot Px.c numeric \code{matrix}. Each element represents the condicional
#'     probability \eqn{P(x|c)}. The sufix \code{x.c} implies that the rows of this
#'     matrix represents the predictors variables (x) and the columns represents
#'     the target variables (c) declared in the Naive Bayes model.
#'
#' @slot Px.nc numeric \code{matrix}. Each element represents the condicional
#'     probability \eqn{P(x|c')}. The sufix \code{x.nc} implies that the rows of this
#'     matrix represents the predictors variables (x) and the columns represents
#'     the complement of the target variables (\eqn{c'}) declared in the Naive Bayes
#'      model.
#'
#' @name SpNaBaProbs-class
#' @rdname SpNaBaProbs-class
#' @exportClass SpNaBaProbs
#'
#'

setClass(Class = "SpNaBaProbs",
         slots = c(Pc = "numeric", Px = "numeric", Pc.x = "matrix", Px.c = "matrix", Px.nc = "matrix")
)



#' @rdname SpNaBaProbs-class
#' @usage NULL
#'
setMethod("initialize", "SpNaBaProbs", function(.Object,
                                                Pc = "numeric",
                                                Px = "numeric",
                                                Pc.x = "matrix",
                                                Px.c = "matrix",
                                                Px.nc = "matrix", ...)
{

  .Object <- callNextMethod()

  # Checking if Pc is missing -----------------

  if(missing(Pc)){
    stop("Pc slots is required.")
  }

  # Checking if Px is missing -----------------

  if(missing(Px)){
    stop("Px slots is required.")
  }

  # Checking if Pc.x is missing -----------------

  if(missing(Pc.x)){
    stop("Pc.x slots is required.")
  }else{
    if(!is.numeric(Pc.x)){
      stop("Pc.x must be numeric.")
    }
  }

  # Checking if Px.c is missing -----------------

  if(missing(Px.c)){
    stop("Px.c slots is required.")
  }else{
    if(!is.numeric(Px.c)){
      stop("Px.c must be numeric.")
    }
  }

  # Checking if Px.nc is missing -----------------

  if(missing(Px.nc)){
    stop("Px.nc slots is required.")
  }else{
    if(!is.numeric(Px.nc)){
      stop("Px.nc must be numeric.")
    }
  }
  # Checking dimnames --------------------------------

  if(!all(colnames(Pc.x) == names(Px))){
    stop("Column names of Pc.x must be identical to the names in vector Px.")
  }
  if(!all(rownames(Pc.x) == names(Pc))){
    stop("Row names of Pc.x must  be identical to the names in vector Pc.")
  }

  if(!all(rownames(Px.c) == names(Px))){
    stop("Row names of Px.c must be identical to the names in vector Px.")
  }
  if(!all(colnames(Px.c) == names(Pc))){
    stop("Columns names of Px.c must  be identical to the names in vector Pc.")
  }

  if(!all(rownames(Px.nc) == names(Px))){
    stop("Row names of Px.c must be identical to the names in vector Px.")
  }
  if(!all(colnames(Px.nc) == names(Pc))){
    stop("Columns names of Px.nc must  be identical to the names in vector Pc.")
  }

  # Assign slots to the object -----------------------

  .Object@Pc <- Pc
  .Object@Px <- Px
  .Object@Pc.x <- Pc.x
  .Object@Px.c <- Px.c
  .Object@Px.nc <- Px.nc

  # Validate the object ------------------------------

  validObject(.Object)

  # Return object ------------------------------------

  return(.Object)
})



#' @rdname SpNaBaProbs-class
#' @export
#'
#' @param Pc numeric \code{vector}. See slot section for more details.
#'
#' @param Px numeric \code{vector}. See slot section for more details.
#'
#' @param Pc.x numeric \code{matrix}. See slot section for more details.
#'
#' @param Px.c numeric \code{matrix}. See slot section for more details.
#'
#' @param Px.nc numeric \code{matrix}. See slot section for more details.
#'
#' @param ... argument pass to other methods.
#'
#' @return An object of class \code{SpNaBaProbs}. See slot section for more details.
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
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'

SpNaBaProbs <- function(Pc, Px, Pc.x, Px.c, Px.nc, ...){
  args <- c(Pc = missing(Pc), Px = missing(Px), Pc.x = missing(Pc.x),
            Px.c = missing(Px.c), Px.nc = missing(Px.nc))

  if(any(args)){
    stop(paste(c("Argument(s)", paste(names(args)[args], collapse = ", "), "must be declared."), collapse = " "))
  }

  new("SpNaBaProbs", Pc = Pc, Px = Px, Pc.x = Pc.x, Px.c = Px.c, Px.nc = Px.nc, ...)

}


#' @rdname SpNaBaProbs-class
#' @usage NULL
#'
#' @importFrom utils head

setMethod("show", "SpNaBaProbs",
          function(object){
            cat("An object of class 'SpNaBaProbs'.\n")
            cat("slots: Pc, Px, Pc.x, Px.c, Px.nc\n",
                "slots type:", "numeric,", " numeric,", " matrix", " matrix"," matrix","\n",
                "names-target:", head(rownames(object@Pc)),"...\n",
                "names-predictors:",  head(colnames(object@Px)),"...\n"
            )
          }
)


