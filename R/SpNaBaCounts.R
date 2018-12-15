
#' Class SpNaBaCounts
#'
#' SpNaBaCounts is used to store frequency information of the Naive Bayes model.
#'
#' @slot N \code{numeric} of length 1. It contains the number of blocks of the partition
#'     of the sample space. If the model is not for spatial data, N is the number of cases in the
#'     sample. If the model is for spatial data, N is the number of cells in the grid
#'     (see \code{gr_build}).
#'
#' @slot Nc \code{numeric}. Each element represents the number of blocks with at least one
#'           observation of target factor $c$. If the model is not for spatial data Nc
#'           represents the number of cases with presences of target factor $c$. If the
#'           model is for spatial data Nc represents the number of cell in the GRID
#'           with at least one obervations of the target factor $c$.
#'
#' @slot Nx \code{numeric}. Each element represents the number of blocks with at least one
#'           observation of predictor factor $x$. If the model is not for spatial data Nx
#'           represents the number of cases with presences of predictor factor $x$. If the
#'           model is for spatial data Nx represents the number of cell in the GRID
#'           with at least one obervations of the predictor factor $x$.
#'
#' @slot Ncx \code{matrix}. Each element represents the number of blocks with at least one
#'           observation of target factor $c$ and predictor factor $x$. If the model is not
#'           for spatial data Ncx represents the joint frequency of target factor $c$ and
#'           the predictor factor $x$. If the model is for spatial data Ncx represents the
#'           number of cell in the GRID with at least one obervations of the target factor
#'           $c$ and the predictor factor $x$.
#'
#' @name SpNaBaCounts-class
#' @rdname SpNaBaCounts-class
#' @exportClass SpNaBaCounts
#'
#'

setClass(Class = "SpNaBaCounts",
         slots = c(N = "numeric", Nc = "numeric", Nx = "numeric", Ncx = "matrix")
)



#' @rdname SpNaBaCounts-class
#' @usage NULL
#'
setMethod("initialize", "SpNaBaCounts", function(.Object,
                                                 N = "numeric",
                                                 Nc = "numeric",
                                                 Nx = "numeric",
                                                 Ncx = "matrix", ...)
{

  .Object <- callNextMethod()

  # Checking if N is missing -----------------

  if(missing(N)){
    stop("N slots is required.")
  }else{
    if(!is.numeric(N)){
      stop("N must be numeric.")
    }
    if(length(N) != 1){
      stop("N must be a vector of length 1.")
    }
    if(N <= 0){
      stop("N must be greater than 0.")
    }
  }

  # Checking if Nc is missing -----------------

  if(missing(Nc)){
    stop("Nc slots is required.")
  }else{
    if(any((N-Nc) < 0)){
      stop("N must be greater or equal to any value in Nc.")
    }
  }

  # Checking if Nx is missing -----------------

  if(missing(Nx)){
    stop("Nx slots is required.")
  }else{
    if(any((N-Nx) < 0)){
      stop("N must be greater or equal to any value in Nx.")
    }
  }

  # Checking if Ncx is missing -----------------

  if(missing(Ncx)){
    stop("Ncx slots is required.")
  }else{
    if(any((N-Ncx) < 0)){
      stop("N must be greater or equal to any value in Ncx.")
    }
    if(any((Nc-Ncx) < 0)){
      stop("Element i of Nc vector must be greater or equal to any value in the row i of Ncx matrix.")
    }
    if(any((Nx-t(Ncx)) < 0)){
      stop("Element j of Nx vector must be greater or equal to any value in the column j of Ncx matrix.")
    }
    if(length(Nc) != nrow(Ncx)){
      stop("length(Nc) must be identical to nrow(Ncx).")
    }
    if(length(Nx) != ncol(Ncx)){
      stop("length(Nx) must be identical to ncol(Ncx).")
    }
  }

  # Checking dimnames --------------------------------

  if(!all(colnames(Ncx) == names(Nx))){
    stop("Column names of Ncx must be identical to the names in vector Nx.")
  }
  if(!all(rownames(Ncx) == names(Nc))){
    stop("Row names must of Ncx be identical to the names in vector Nc.")
  }

  # Assign slots to the object -----------------------

  .Object@N <- N
  .Object@Nc <- Nc
  .Object@Nx <- Nx
  .Object@Ncx <- Ncx

  # Validate the object ------------------------------

  validObject(.Object)

  # Return object ------------------------------------

  return(.Object)
})



#' @rdname SpNaBaCounts-class
#' @export
#'
#' @param N \code{vector} of length 1. See slot section for more details.
#'
#' @param Nc \code{vector}. See slot section for more details.
#'
#' @param Nx \code{vector}. See slot section for more details.
#'
#' @param Ncx \code{matrix}. See slot section for more details.
#'
#' @param ... argument pass to other methods.
#'
#' @return An object of class \code{SpNaBaCounts}. See slot section for more details.
#'
#' @examples
#'
#' # Using SpNaBaMatrix constructor ----------------------
#'
#' a <- matrix(rbinom(100, 100, runif(100)), nrow = 10, ncol = 10,
#'   dimnames = list(paste0("id:", 1:10), letters[1:10]))
#'
#' x.mat <- SpNaBaMatrix(freqmatrix = a)
#'
#' SpNaBaCounts(N = nrow(x.mat@binmatrix),
#'              Nc = colSums(x.mat@binmatrix),
#'              Nx = colSums(x.mat@binmatrix),
#'              Ncx = t(x.mat@binmatrix) %*% x.mat@binmatrix)
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'

SpNaBaCounts <- function(N, Nc, Nx, Ncx, ...){
    args <- c(N = missing(N), Nc = missing(Nc), Nx = missing(Nx), Ncx = missing(Ncx))

    if(any(args)){
      stop(paste(c("Argument(s)", paste(names(args)[args], collapse = ", "), "must be declared."), collapse = " "))
    }

    new("SpNaBaCounts", N = N, Nc = Nc, Nx = Nx, Ncx = Ncx, ...)

}


#' @rdname SpNaBaCounts-class
#' @usage NULL
#'
#' @importFrom utils head

setMethod("show", "SpNaBaCounts",
          function(object){
            cat("An object of class 'SpNaBaCounts'.\n")
            cat("slots: N, Nc, Nx, Ncx\n",
                "slots type:", "numeric(length = 1),", " numeric,", " numeric,", " matrix", "\n",
                "names-target:", head(rownames(object@Ncx)),"...\n",
                "names-predictors:",  head(colnames(object@Ncx)),"...\n"
            )
          }
)





