#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatCount-class
#'
#' @slot Count a list object whit 4 elements, N, Nc, Nx and Ncx. See details
#'
#' @details The elements of the list are defined as follow.
#'
#'     \strong{N} an integer. It corresponds to the number of rows in the slot
#'     name_ID of BinMat object.
#'
#'     \strong{Nc} a numeric vector.
#'
#'     \strong{Nx} a numeric vector.
#'
#'     \strong{Ncx} a numeric matrix.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @examples
#'
#' name_ID<-data.frame(name = c("carlos", "pepe"), row.names = c("X1","X2"), stringsAsFactors = F)
#' DMNB<-matrix(rbinom(10,10, 0.5), ncol = nrow(name_ID), dimnames = list(1:5,rownames(name_ID)))
#' BMNB<-matrix(rbinom(10,1, 0.5), ncol = nrow(name_ID), dimnames = list(1:5,rownames(name_ID)))
#' x.mat<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
#'
#' x.mat
#'
#' Count.mat<-list(N=10L, Nc = t(t(c(1L, 2L))), Nx = t(t(c(1L, 2L, 3L))), Ncx = matrix(1L:6L,2,3))
#'
#' c.mat<-BinMatCount(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB, Count = Count.mat)

NULL

# Class definition -------------------------------------

.BinMatCount <- setClass(Class = "BinMatCount", contains = "BinMat",
         slots = list(Count = "list"))


# Create a validity function -------------------------------------------
# Set the validity function as default inspector -------------------------

setValidity("BinMatCount", function(object){
  obj_lis <- is.list(object@Count)
  obj_len <- (length(object@Count) == 4)
  obj_nam <- all(names(object@Count) == c("N", "Nc", "Nx", "Ncx"))
  obj_N <- (length(object@Count$N) == 1)
  obj_Nc <- all(!is.null(dim(object@Count$Nc)), (dim(object@Count$Nc)[2] == 1))
  obj_Nx <- all(!is.null(dim(object@Count$Nx)), (dim(object@Count$Nx)[2] == 1))
  obj_Ncx <- all(all(dim(object@Count$Ncx) == c(nrow(object@Count$Nc),
                                                nrow(object@Count$Nx))))
  obj_val <- c(obj_lis, obj_len, obj_nam, obj_N, obj_Nc, obj_Nx, obj_Ncx)
  if(!all(obj_val)){
    stop("Slot Count is not of the correct type. See documentation of BinMatCount class.")
  }else{
    return(TRUE)
  }
})

# Create an constructor method for class BinMatCount --------------------------

#' @rdname BinMatCount-class
#' @name Contstructor
#' @docType methods
#' @export

BinMatCount <- function(name_ID, DMNB, BMNB, Count, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatCount(obj, Count = Count, ...)
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
          signature = "BinMatCount",
          definition = function(object){
            cat("An object of class 'BinMatCount'.\n")
            cat("Slot: Count\n", "Type:",  class(object@Count), "\n",
                "names: ", names(object@Count),"\n",
                "types:", class(object@Count$N), class(object@Count$Nc),
                class(object@Count$Nx), class(object@Count$Ncx),"\n")}
)


# Create a print method for class BinMat ------------------------------------

#' @rdname BinMatCount-class
#' @name print
#' @docType methods
#' @export

setMethod (f = "print",
           signature = "BinMatCount",
           definition = function(x,...){
             cat("An object of class 'BinMatCount'.\n")
             cat("N: integer\n"); print(x@Count$N)
             cat("Nc: matrix\n"); print(head(x@Count$Nc))
             cat("Nx: matrix\n"); print(head(x@Count$Nx))
             cat("Ncx: matrix\n"); print(head(x@Count$Ncx[,head(1:nrow(x@Count$Nx))]))
           }
)
