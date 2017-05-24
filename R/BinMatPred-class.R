#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatPred-class
#'
#' @description This is a class associated to prediction methods applied to
#'     BinMatScore objects (See \code{\link[rspecies]{predict}}).
#'
#' @slot Prediction a matrix object.
#'
#' @details The elements of the list are defined as follow.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}).
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
#' count.mat<-counts(x.mat,  target = c("F.169", "F.272"))
#'
#' # Probability matrices
#' prob.mat<-probs(count.mat, lap_fac = 0.1)
#'
#' #score function
#' score.mat<-score(prob.mat, count.mat)
#'
#' # Prediction
#' pred.mat<-predict(score.mat, apr_inc = FALSE, comp_inc = FALSE)
#'
#'
#'
NULL

# Class definition ---------------------------------------------------

#' @rdname BinMatPred-class
#' @name new-BinMatPred
#' @docType methods
#' @export

.BinMatPred <- setClass(Class = "BinMatPred", contains = "BinMat",
         slots = list(Prediction = "matrix"))


# Create a validity function -------------------------------------------
# Set the validity function as default inspector -------------------------

setValidity("BinMatPred", function(object){
  obj_lis <- is.matrix(object@Prediction)
  obj_dim <- (dim(object@Prediction)[1] == dim(object@DMNB)[1])

  obj_val <- c(obj_lis, obj_dim)

  if(!all(obj_val)){
    stop("Slot Prediction is not of the correct type. See documentation of BinMatPred class.")
  }else{
    return(TRUE)
  }
})

# Create an constructor method for class BinMatCount --------------------------

#' @rdname BinMatPred-class
#' @name BinMatPred-Constructor
#' @docType methods
#' @export

BinMatPred <- function(name_ID, DMNB, BMNB, Prediction, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatPred(obj, Prediction = Prediction, ...)
}

# Create a show method for class BinMatPred ---------------------------------


#' @rdname BinMatPred-class
#' @name show-BinMatPred
#' @docType methods
#' @export
#'
setMethod(f = "show",
          signature = "BinMatPred",
          definition = function(object){
            cat("An object of class 'BinMatPred'.\n")
            cat("Slot: Prediction\n", "Type:",  class(object@Prediction), "\n",
                "target: ", colnames(object@Prediction),"\n",
                "dimension:", dim(object@Score$Prediction), "\n")}
)


# Create a print method for class BinMat ------------------------------------

#' @rdname BinMatPred-class
#' @name print-BinMatPred
#' @docType methods
#' @export

setMethod (f = "print",
           signature = "BinMatPred",
           definition = function(x,...){
             cat("An object of class 'BinMatPred'.\n")
             cat("Prediction:", class(x@Score$Apriori),"\n"); print(head(x@Prediction[,head(1:ncol(x@Prediction))]))
           }
)
