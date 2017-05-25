#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatCount-class
#'
#' @description This is a class associated to count methods applied to
#'     BinMat objects (See \code{\link[rspecies]{counts}}).
#'
#' @slot name_ID a data.frame. rownames(name_ID) = colnames(DMNB) = colnames(BMNB).
#'     A data containing the ID's for column names in slots DMNB and BMNB.
#'
#' @slot DMNB a matrix of integers. Contains the number of observations per cells
#'     per column. The number of columns must be identical to the number of rows
#'     in name_ID.
#'
#' @slot BMNB a logical matrix. Contains TRUE for all DMNB > 0, FALSE otherwise.
#'     The number of columns must be identical to the number of rows in name_ID.
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
#' library(sp)
#' library(rgeos)
#' data(Mex0)
#' data(mammals)
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd <- grd_build(Mex0)
#'
#' # Identification points of mammals with colnames especified.
#' names <- paste("X", 1:nlevels(as.factor(mammals@data$nameID)), sep = "")
#'
#' x.mat <- id_pts(grd = Mex0.grd, pts = mammals, colnames = names)
#'
#' # Counting matrices
#' system.time(count.mat <- counts(x.mat))
#' getN(count.mat)
#' head(getNc(count.mat))
#' head(getNx(count.mat))
#' head(getNcx(count.mat))
NULL

# Class definition -------------------------------------

#' @rdname BinMatCount-class
#' @name new-BinMatCount
#' @docType methods
#' @export


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
#' @name BinMatCount-Contstructor
#'
#' @param ... pass to another methods.
#'
#' @docType methods
#' @export

BinMatCount <- function(name_ID, DMNB, BMNB, Count, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatCount(obj, Count = Count, ...)
}

# Create a show method for class BinMat ----------------------------------

#' @rdname BinMatCount-class
#' @name show-BinMatCount
#' @aliases show-BinMatCount
#'
#' @param object an object of class BinMatCount.
#'
#' @docType methods
#' @export


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
#' @name print-BinMatCount
#' @aliases print-BinMatCount
#'
#' @param x an object of class BinMatCount.
#'
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
