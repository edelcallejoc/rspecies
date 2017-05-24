#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatEps-class
#'
#' @slot Epsilon a list object whit 2 elements, Ecx and Encx.
#'
#' @details The elements of the list are defined as follow.
#'
#'     \strong{Ecx}: numeric matrix. With s rows and m columns
#'
#'     \strong{Pcnx}a numeric matrix. With s rows and m columns.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' data(Mex0)
#' data(mammals)
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals with colnames = NULL.
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals, colnames = NULL)
#'
#' # Counting matrices
#' count.mat<-counts(x.mat)
#'
#' # Probability matrices
#' prob.mat<-probs(count.mat, laplace = 0.1)
#'
#' # Epsilon function
#' epsilon.mat<-epsilon(prob.mat, count.mat)
#'

NULL

# Class definition ---------------------------------------------------

#' @rdname BinMatEps-class
#' @name new-BinMatEps
#' @docType methods
#' @export

.BinMatEps <- setClass(Class = "BinMatEps", contains = "BinMat",
         slots = list(Epsilon = "list"))


# Create a validity function -------------------------------------------
# Set the validity function as default inspector -------------------------

setValidity("BinMatEps", function(object){
  obj_lis <- is.list(object@Epsilon)
  obj_len <- (length(object@Epsilon) == 2)
  obj_nam <- all(names(object@Epsilon) == c("Ecx", "Encx"))
  obj_eps <- all(dim(object@Epsilon$Ecx) == dim(object@Epsilon$Encx))

  obj_val <- c(obj_lis, obj_len, obj_nam, obj_eps)

  if(!all(obj_val)){
    stop("Slot Epsilon is not of the correct type. See documentation of BinMatEps class.")
  }else{
    return(TRUE)
  }
})

# Create an constructor method for class BinMatCount --------------------------

#' @rdname BinMatEps-class
#' @name BinMatEps-Contstructor
#' @docType methods
#' @export


BinMatEps <- function(name_ID, DMNB, BMNB, Epsilon, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatEps(obj, Epsilon = Epsilon, ...)
}

# Create a show method for class BinMat ----------------------------------

#' @rdname BinMatEps-class
#' @name show-BinMatEps
#' @docType methods
#' @export

setMethod(f = "show",
          signature = "BinMatEps",
          definition = function(object){
            cat("An object of class 'BinMatEps'.\n")
            cat("Slot: Epsilon\n", "Type:",  class(object@Epsilon), "\n",
                "names: ", names(object@Epsilon),"\n",
                "types:", class(object@Epsilon$Ecx), class(object@Epsilon$Encx),"\n")}
)


# Create a print method for class BinMat ------------------------------------

#' @rdname BinMatEps-class
#' @name print-BinMatEps
#' @docType methods
#' @export

setMethod (f = "print",
           signature = "BinMatEps",
           definition = function(x,...){
             cat("An object of class 'BinMatEps'.\n")
             cat("Ecx:", class(x@Epsilon$Ecx),"\n"); print(head(x@Epsilon$Ecx[,1:6]))
             cat("Encx:", class(x@Epsilon$Encx),"\n"); print(head(x@Epsilon$Encx[,1:6]))
           }
)
