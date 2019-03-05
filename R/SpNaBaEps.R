
#'
#' Class SpNaBaEps
#'
#' SpNaBaEps is used to store Epsilon's parameters of the Naive Bayes model.
#'
#' @slot Ecx \code{matrix} with \eqn{S} rows and \eqn{M} columns. Each element
#'    represents the epsilon parameter \eqn{\epsilon(c|x)}. See
#'    \code{\link[rspecies]{SpNaBaMatrix}}.
#'
#' @name SpNaBaEps-class
#' @rdname SpNaBaEps-class
#' @exportClass SpNaBaEps
#'

setClass(Class = "SpNaBaEps", slots = c(Ecx = "matrix"))

#' @rdname SpNaBaEps-class
#' @usage NULL
#'

setMethod("initialize", "SpNaBaEps",
          function(.Object, Ecx = "matrix", ...){

  .Object <- callNextMethod()

  # Checking if Ecx is missing -----------------------

  if(missing(Ecx)){
    stop("Ecx slots is required.")
  }else{
    if(!is.numeric(Ecx)){
      stop("Ecx must be numeric.")
    }
  }

  # Assign slots to the object -----------------------

  .Object@Ecx <- Ecx

  # Validate the object ------------------------------

  validObject(.Object)

  # Return object ------------------------------------

  return(.Object)
})


#' @rdname SpNaBaEps-class
#' @export
#'
#' @param Ecx numeric \code{matrix}. See slot section for more details.
#'
#' @param ... argument pass to other methods.
#'
#' @return An object of class \code{SpNaBaEps}. See slot section for more details.
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
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'

SpNaBaEps <- function(Ecx, ...){
  args <- missing(Ecx)

  if(any(args)){
    stop(paste(c("Argument(s)", paste(names(args)[args], collapse = ", "), "must be declared."), collapse = " "))
  }

  new("SpNaBaEps", Ecx = Ecx, ...)

}



#' @rdname SpNaBaEps-class
#' @usage NULL
#'
#' @importFrom utils head

setMethod("show", "SpNaBaEps",
          function(object){
            cat("An object of class 'SpNaBaEps'.\n")
            cat("slots: Ecx",
                "slots type:", " matrix", "\n",
                "names-target:", head(rownames(object@Ecx)),"...\n",
                "names-predictors:",  head(colnames(object@Ecx)),"...\n"
            )
          }
)

