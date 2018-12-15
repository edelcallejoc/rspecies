
#' Class SpNaBaMatrix
#'
#' SpNaBaMatrix is used to fit Spatial Naive Bayes models. It can be used to extract information
#'     of the sample and to adjust Spatial Naive Bayes models.
#'
#' @slot freqmatrix \code{matrix}. A matrix with $N$ rows (rows correspond to the number of blocks
#'     in the GRID) and $M$ columns (columns represent the number of factors included in the model),
#'     where each element $i,j$ shows the number of observations of the factor $j$ in the cell $i$.
#'
#' @slot binmatrix \code{matrix}. A matrix with $N$ rows (rows correspond to the number of blocks
#'     in the GRID) and $M$ columns (columns represent the number of factors included in the model),
#'     where each element $i,j$ shows \code{TRUE} if the cell $i$ contains at least one observation
#'     of the factor $j$. \code{FALSE} otherwise.
#'
#' @name SpNaBaMatrix-class
#' @rdname SpNaBaMatrix-class
#' @exportClass SpNaBaMatrix
#'
#'

setClass(Class = "SpNaBaMatrix",
         slots = c(freqmatrix = "matrix", binmatrix = "matrix")
)



#' @rdname SpNaBaMatrix-class
#' @usage NULL
#'
setMethod("initialize", "SpNaBaMatrix", function(.Object,
                                                 freqmatrix = "matrix",
                                                 binmatrix = "matrix", ...)
{

  .Object <- callNextMethod()

  # Checking if freqmatrix is missing -----------------

  if(missing(freqmatrix)){
    freqmatrix <- matrix()
  }else{
    if(!is.numeric(freqmatrix)){
      stop("freqmatrix must be numeric.")
    }
    if(any(freqmatrix < 0)){
      stop("freqmatrix must contains values greater or equal to 0.")
    }
  }

  # Checking if binmatrix is missing -----------------

  if(missing(binmatrix)){
    if(all(is.na(freqmatrix))){
      binmatrix <- freqmatrix
    }else{
      binmatrix <- (freqmatrix > 0)
    }
  }else{
    if(any((freqmatrix > 0) != binmatrix)){
      stop("The information in the slot binmatrix is not compatible to the slot freqmatrix")
    }
  }

  # Checking dimnames --------------------------------

  if(!all(colnames(freqmatrix) == colnames(binmatrix))){
    stop("Column names must be identical for the slots freqmatrix and binmatrix.")
  }
  if(!all(rownames(freqmatrix) == rownames(binmatrix))){
    stop("Row names must be identical for the slots freqmatrix and binmatrix.")
  }

  # Assign slots to the object -----------------------

  .Object@freqmatrix <- freqmatrix
  .Object@binmatrix <- binmatrix

  # Validate the object ------------------------------

  validObject(.Object)

  # Return object ------------------------------------

  return(.Object)
})



#' @rdname SpNaBaMatrix-class
#' @export
#'
#' @param freqmatrix \code{matrix}. typically, the output of \code{id_pts()} function.
#'     See slots section for more details.
#' @param ... argument pass to other methods.
#'
#' @return An object of class \code{SpNaBaMatrix}. See slot section for more details.
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
#' x.mat
#'
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
#' x.mat
#'
#' @seealso \code{\link[rspeciesdev]{grd_build}} and \code{\link[rspeciesdev]{id_pts}}.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'

SpNaBaMatrix <- function(freqmatrix, ...){
  if(missing(freqmatrix)){
    new("SpNaBaMatrix", ...)
  }else{
    new("SpNaBaMatrix", freqmatrix = freqmatrix, ...)
  }
}


#' @rdname SpNaBaMatrix-class
#' @usage NULL
#'
#' @importFrom utils head

setMethod("show", "SpNaBaMatrix",
          function(object){
            cat("An object of class 'SpNaBaMatrix'.\n")
            cat("slots: freqmatrix - binmatrix\n",
                "slots type:",  class(object@freqmatrix), "integer", "logical", "\n",
                "dimension: ", dim(object@freqmatrix),"\n",
                "colnames:", head(colnames(object@freqmatrix)),"...\n",
                "rownames:", head(rownames(object@freqmatrix)),"...\n")
          }
)


