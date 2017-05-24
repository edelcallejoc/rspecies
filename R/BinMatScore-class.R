#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatScore-class
#'
#' @description This is a class associated to score method applied to
#'     BinMatProb objects (See \code{\link[rspecies]{score}}).
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
#' #' library(sp)
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
NULL

# Class definition ---------------------------------------------------

#' @rdname BinMatScore-class
#' @name new-BinMatScore
#' @docType methods
#' @export

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
#' @name BinMatScore-Contstructor
#'
#' @param ... pass to another methods.
#'
#' @docType methods
#' @export

BinMatScore <- function(name_ID, DMNB, BMNB, Score, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatScore(obj, Score = Score, ...)
}

# Create a show method for class BinMat ----------------------------------

#' @rdname BinMatScore-class
#' @name show-BinMatScore
#' @aliases show-BinMatScore
#'
#' @param object an object of class BinMatScore.
#'
#' @docType methods
#' @export

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
#' @name print-BinMatScore
#' @aliases print-BinMatScore
#'
#' @param object an object of class BinMatScore.
#'
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
