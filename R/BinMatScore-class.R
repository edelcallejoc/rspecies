#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatScore-class
#'
#' @slot Score a list object whit 3 elements, Apriori, Scx and Scnx.
#'
#' @details The elements of the list are defined as follow.
#'
#'     \strong{Apriori} a numeric vector. \strong{Scx} a numeric matrix.
#'     \strong{Scnx} a numeric matrix.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}).
#'
#' @examples
#'
NULL

# Class definition ---------------------------------------------------

.BinMatScore <- setClass(Class = "BinMatScore", contains = "BinMat",
         slots = list(Score = "list"))


# Create a validity function -------------------------------------------
# Set the validity function as default inspector -------------------------

setValidity("BinMatScore", function(object){
  obj_lis <- is.list(object@Score)
  obj_len <- (length(object@Score) == 3)
  obj_nam <- all(names(object@Score) == c("Apriori", "Scx", "Scnx"))
  obj_sco <- all(dim(object@Score$Scx) == dim(object@Score$Scnx))
  obj_apr <- all(all(dim(object@Score$Scx)[2] == nrow(object@Score$Apriori)),
                 all(dim(object@Score$Scnx)[2] == nrow(object@Score$Apriori)))

  obj_val <- c(obj_lis, obj_len, obj_nam, obj_sco, obj_apr)

  if(!all(obj_val)){
    stop("Slot Score is not of the correct type. See documentation of BinMatScore class.")
  }else{
    return(TRUE)
  }
})

# Create an constructor method for class BinMatCount --------------------------

#' @rdname BinMatScore-class
#' @name Contstructor
#' @docType methods
#' @export

BinMatScore <- function(name_ID, DMNB, BMNB, Score, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatScore(obj, Score = Score, ...)
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
          signature = "BinMatScore",
          definition = function(object){
            cat("An object of class 'BinMatScore'.\n")
            cat("Slot: Score\n", "Type:",  class(object@Score), "\n",
                "names: ", names(object@Score),"\n",
                "types:", class(object@Score$Apriori), class(object@Score$Scx),
                class(object@Score$Scnx), "\n")}
)


# Create a print method for class BinMat ------------------------------------

#' @rdname BinMatScore-class
#' @name print
#' @docType methods
#' @export

setMethod (f = "print",
           signature = "BinMatScore",
           definition = function(x,...){
             cat("An object of class 'BinMatScore'.\n")
             cat("Apriori:", class(x@Score$Apriori),"\n"); print(head(x@Score$Apriori))
             cat("Scx:", class(x@Score$Scx),"\n"); print(head(x@Score$Scx[,head(1:ncol(x@Score$Scx))]))
             cat("Scnx:", class(x@Score$Scnx),"\n"); print(head(x@Score$Scnx[,head(1:ncol(x@Score$Scnx))]))
           }
)
