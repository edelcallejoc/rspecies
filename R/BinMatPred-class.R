#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatPred-class
#'
#' @slot Prediction a matrix object.
#'
#' @details The elements of the list are defined as follow.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}).
#'
#' @examples
#'
NULL

# Class definition ---------------------------------------------------

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
#' @name Contstructor
#' @docType methods
#' @export

BinMatPred <- function(name_ID, DMNB, BMNB, Prediction, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatPred(obj, Prediction = Prediction, ...)
}

#
# setMethod(f = "initialize",
#           signature = "BinMatCount",
#           definition = function(.Object, ..., Count){
#             .Object@Count <- Count
#             callNextMethod(.Object, ...)
#             validObject(.Object)
#             return(.Object)
#           }
# )



# Create a show method for class BinMat ----------------------------------

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
#' @name print
#' @docType methods
#' @export

setMethod (f = "print",
           signature = "BinMatPred",
           definition = function(x,...){
             cat("An object of class 'BinMatPred'.\n")
             cat("Prediction:", class(x@Score$Apriori),"\n"); print(head(x@Prediction[,head(1:ncol(x@Prediction))]))
           }
)
