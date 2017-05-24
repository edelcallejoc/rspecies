
#' Replace methods for BinMat class and subclass
#' @include BinMatCount-class.R
#'
#' @description The setters methods for BinMatCount-class, each function is a
#'     method to acces to each slot in BinMat-class
#'
#' @name set-BinMatCount
#'
#' @param object A \code{\link[rspecies]{BinMatCount}} object.
#'
#' @param value  An object for replacement the slots of BinMatCount.
#'     'setN' an integer vector of length 1. 'setNc' an integer vector.
#'     'setNx' an integer vector. 'setNcx' an integer matrix. (See
#'     \code{\link[rspecies]{BinMatCount}} for more details.)
#'
#' @return return a slot of BinMat object.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}).

NULL

# Setters ----------------------------------------------------

#' @name set-BinMatCount
#' @rdname set-BinMatCount
#' @exportMethod setN<-

setGeneric("setN<-",function(object, value){standardGeneric ("setN<-")})

setReplaceMethod (f = "setN",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$N <- value
                    validObject(object)
                    return(object)
                  }
)

#' @name set-BinMatCount
#' @rdname set-BinMatCount
#' @exportMethod setNc<-

setGeneric("setNc<-",function(object, value){standardGeneric ("setNc<-")})

setReplaceMethod (f = "setNc",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Nc <- value
                    validObject(object)
                    return(object)
                  }
)


#' @name set-BinMatCount
#' @rdname set-BinMatCount
#' @aliases setNx,BinMatCount
#' @exportMethod setNx<-

setGeneric("setNx<-",function(object, value){standardGeneric ("setNx<-")})

setReplaceMethod (f = "setNx",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Nx <- value
                    validObject(object)
                    return(object)
                  }
)

#' @name set-BinMatCount
#' @rdname set-BinMatCount
#' @aliases setNcx,BinMatCount
#' @exportMethod setNcx<-

setGeneric("setNcx<-",function(object, value){standardGeneric ("setNcx<-")})

setReplaceMethod (f = "setNcx",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Ncx <- value
                    validObject(object)
                    return(object)
                  }
)
