#'
#' An S4 subclass from BinMat class.
#'
#' @include BinMat-class.R
#'
#' @name BinMatProb-class
#'
#' @description This is a class associated to probability methods applied to
#'     BinMatCount objects (See \code{\link[rspecies]{probs}}).
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
#' @slot Prob a list object whit 8 elements, Pc, Px, Pcx, Pcnx, Pxc, Pxnc,
#'     Pnxc and Pnxnc. See details.
#'
#' @details The elements of the list are defined as follow.
#'
#'     \strong{Pc} a numeric matrix. With s rows and 1 column.
#'
#'     \strong{Px} a numeric matrix. With m rows and 1 column.
#'
#'     \strong{Pcx} a numeric matrix. With s rows and m columns.
#'
#'     \strong{Pcnx}a numeric matrix. With s rows and m columns.
#'
#'     \strong{Pxc} a numeric matrix. With m rows and s columns.
#'
#'     \strong{Pxnc} a numeric matrix. With m rows and s columns.
#'
#'     \strong{Pnxc} a numeric matrix. With m rows and s columns.
#'
#'     \strong{Pnxnc} a numeric matrix. With m rows and s columns.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @examples
#'#' #' library(sp)
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
#' print(prob.mat)

NULL

# Class definition ---------------------------------------------------

#' @rdname BinMatProb-class
#' @name new-BinMatProb
#' @docType methods
#' @export

.BinMatProb <- setClass(Class = "BinMatProb", contains = "BinMat",
         slots = list(Prob = "list"))


# Create a validity function -------------------------------------------
# Set the validity function as default inspector -------------------------

setValidity("BinMatProb", function(object){
  obj_lis <- is.list(object@Prob)
  obj_len <- (length(object@Prob) == 8)
  obj_nam <- all(names(object@Prob) == c("Pc", "Px", "Pcx", "Pcnx", "Pxc", "Pxnc",
                                         "Pnxc", "Pnxnc"))
  obj_Pc <- all(!is.null(dim(object@Prob$Pc)), (dim(object@Prob$Pc)[2] == 1),
                is.numeric(object@Prob$Pc))
  obj_Px <- all(!is.null(dim(object@Prob$Px)), (dim(object@Prob$Px)[2] == 1),
                is.numeric(object@Prob$Px))
  obj_Pcx <- all(all(dim(object@Prob$Pcx) == c(nrow(object@Prob$Pc),
                                                nrow(object@Prob$Px))),
                 is.numeric(object@Prob$Pcx))
  obj_Pcnx <- all(all(dim(object@Prob$Pcnx) == dim(object@Prob$Pcx)),
                  is.numeric(object@Prob$Pcnx))
  obj_Pxc <- all(all(dim(object@Prob$Pxc) == c(nrow(object@Prob$Px),
                                               nrow(object@Prob$Pc))),
                 is.numeric(object@Prob$Pxc))
  obj_Pxnc <- all(all(dim(object@Prob$Pxnc) == dim(object@Prob$Pxc)),
                 is.numeric(object@Prob$Pxnc))
  obj_Pnxc <- all(all(dim(object@Prob$Pnxc) == dim(object@Prob$Pxc)),
                  is.numeric(object@Prob$Pnxc))
  obj_Pnxnc <- all(all(dim(object@Prob$Pnxnc) == dim(object@Prob$Pxc)),
                  is.numeric(object@Prob$Pnxnc))
  obj_val <- c(obj_lis, obj_len, obj_nam, obj_Pc, obj_Px, obj_Pcx, obj_Pcnx,
               obj_Pxc, obj_Pxnc, obj_Pnxc, obj_Pnxnc)
  if(!all(obj_val)){
    stop("Slot Prob is not of the correct type. See documentation of BinMatProb class.")
  }else{
    return(TRUE)
  }
})

# Create an constructor method for class BinMatCount --------------------------

#' @rdname BinMatProb-class
#' @name BinMatProb-Contstructor
#'
#' @param ... pass to another methods.
#'
#' @docType methods
#' @export

BinMatProb <- function(name_ID, DMNB, BMNB, Prob, ...){
  obj<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
  .BinMatProb(obj, Prob = Prob, ...)
}

# Create a show method for class BinMat ----------------------------------


#' @rdname BinMatProb-class
#' @name show-BinMatProb
#' @aliases show-BinMatProb
#'
#' @param object an object of class BinMatProb.
#'
#' @docType methods
#' @export

setMethod(f = "show",
          signature = "BinMatProb",
          definition = function(object){
            cat("An object of class 'BinMatProb'.\n")
            cat("Slot: Prob\n", "Type:",  class(object@Prob), "\n",
                "names: ", names(object@Prob),"\n",
                "types:", class(object@Prob$Pc), class(object@Prob$Px),
                class(object@Prob$Pcx), class(object@Prob$Pcnx),
                class(object@Prob$Pxc),class(object@Prob$Pxnc),
                class(object@Prob$Pnxc),class(object@Prob$Pnxnc),"\n")}
)


# Create a print method for class BinMat ------------------------------------

#' @rdname BinMatProb-class
#' @name print-BinMatProb
#'
#' @param x an object of class BinMatProb.
#'
#' @aliases print-BinMatProb
#' @docType methods
#' @export

setMethod (f = "print",
           signature = "BinMatProb",
           definition = function(x,...){
             cat("An object of class 'BinMatProb'.\n")
             cat("Pc:", class(x@Prob$Pc),"\n"); print(head(x@Prob$Pc))
             cat("Px:", class(x@Prob$Px),"\n"); print(head(x@Prob$Px))
             cat("Pcx:", class(x@Prob$Pcx),"\n"); print(head(x@Prob$Pcx[,1:6]))
             cat("Pcnx:", class(x@Prob$Pcnx),"\n"); print(head(x@Prob$Pcnx[,1:6]))
             cat("Pxc:", class(x@Prob$Pxc),"\n"); print(head(x@Prob$Pxc[,head(1:length(x@Prob$Pc))]))
             cat("Pxnc:", class(x@Prob$Pxnc),"\n"); print(head(x@Prob$Pxnc[,head(1:length(x@Prob$Pc))]))
             cat("Pnxc:", class(x@Prob$Pnxc),"\n"); print(head(x@Prob$Pnxc[,head(1:length(x@Prob$Pc))]))
             cat("Pnxnc:", class(x@Prob$Pnxnc),"\n"); print(head(x@Prob$Pnxnc[,head(1:length(x@Prob$Pc))]))
           }
)
