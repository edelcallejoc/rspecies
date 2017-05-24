
#' An S4 class to represent a matrix model for naive bayes spatial
#' data mining model.
#'
#' @name BinMat-class
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
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @examples
#'
#' # Good example
#' name_ID<-data.frame(name = c("carlos", "pepe"), row.names = c("X1","X2"), stringsAsFactors = FALSE)
#' DMNB<-matrix(rbinom(10,10, 0.5), ncol = nrow(name_ID), dimnames = list(1:5,rownames(name_ID)))
#' BMNB<-matrix(rbinom(10,1, 0.5), ncol = nrow(name_ID), dimnames = list(1:5,rownames(name_ID)))
#' x.mat<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)
#'
#' x.mat
#' print(x.mat)
#'
#' # Bad example
#' \dontrun{
#' name_ID<-data.frame(name = c("carlos", "pepe"), row.names = c("X1","X2"),
#'                     stringsAsFactors = F)
#' DMNB<-matrix(rep(1,10), ncol = nrow(name_ID), dimnames = list(1:5,rownames(name_ID)))
#' BMNB<-matrix(rep(1,10), ncol = 5, dimnames = list(1:2,c(LETTERS[1:5])))
#' x.mat<-BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB)}
#'


NULL

# Class definition ----------------------------------------------------

.BinMat<-setClass(Class = "BinMat",
         slots = list(name_ID = "data.frame", DMNB = "matrix", BMNB = "matrix")
         )

# Create a validity function -------------------------------------------
# Set the validity function as default inspector -------------------------

setValidity("BinMat", function(object){
  DMNB_val <- all(apply(object@DMNB,2, function(x){all(x >= 0)}))
  BMNB_val <- all(apply(object@BMNB,2, function(x){all(x %in% 0:1)}))
  nameId_val <- is.data.frame(object@name_ID)
  args_val <- c(DMNB_val, BMNB_val, nameId_val)
  if(!all(args_val)){
    stop(paste("Slot ", c("DMNB", "BMNB", "name_ID")[!args_val],
               " is not of the correct type.\n",
               "See BinMat-Class documentation.\n",
               sep =""))
  }else{
    col_val<-c(identical(dim(object@name_ID)[1], dim(object@DMNB)[2]),
               identical(dim(object@name_ID)[1], dim(object@BMNB)[2]))
    if(!all(col_val)){
      stop(paste("Slot ", c("DMNB", "BMNB")[!col_val], ":",
                 "The number of columns must be identical to the number of rows in name_ID.\n",
                 "See BinMat-Class documentation.\n",
                 sep =""))
    }else{
    return(TRUE)
    }
  }
})


# Create a constructor method for class BinMat --------------------------

#' @rdname BinMat-class
#' @name BinMat
#' @docType methods
#' @export

BinMat <- function(name_ID, DMNB, BMNB, ...){
  .BinMat(name_ID = name_ID, DMNB = DMNB, BMNB = BMNB, ...)
}



# setMethod(f = "initialize",
#           signature = "BinMat",
#           definition = function(.Object, DMNB, BMNB, name_ID){
#               .Object@DMNB <- DMNB
#               .Object@BMNB <- BMNB
#               .Object@name_ID <- name_ID
#               callNextMethod(.Object)
#               validObject(.Object)
#               return(.Object)
#           }
# )


# Create a show method for class BinMat ----------------------------------

setMethod(f = "show",
          signature = "BinMat",
          definition = function(object){
              cat("An object of class 'BinMat'.\n")
              cat("Slot: name_ID\n", "Type:",  class(object@name_ID), "\n",
                  "dimension: ", dim(object@name_ID),"\n",
                  "colnames:", head(colnames(object@name_ID)),"...\n",
                  "rownames:", head(rownames(object@name_ID)),"...\n")
              cat("Slot: DMNB\n", "Type:",  class(object@DMNB), "\n",
                  "dimension: ", dim(object@DMNB),"\n",
                  "colnames:", head(colnames(object@DMNB)),"...\n",
                  "rownames:", head(rownames(object@DMNB)),"...\n")
                cat("Slot: BMNB\n", "Type:",  class(object@BMNB), "\n",
                  "dimension: ", dim(object@BMNB),"\n",
                  "colnames:", head(colnames(object@BMNB)),"...\n",
                  "rownames:", head(rownames(object@BMNB)),"...\n")}
)



# Create a print method for class BinMat ------------------------------------

#' @rdname BinMat-class
#' @name print
#' @docType methods
#' @export


setMethod (f = "print",
           signature = "BinMat",
           definition = function(x,...){
             cat("An object of class 'BinMat'.\n")
             cat("Slot name_ID: data.frame\n"); print (x@name_ID)
             cat("Slot DMNB: matrix\n"); print (x@DMNB)
             cat("Slot BMNB: matrix\n"); print (x@BMNB)
             }
           )

