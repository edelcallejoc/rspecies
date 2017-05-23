
#' Replace methods for BinMat class and subclass
#' @include BinMatCount-class.R
#'
#' @description The setters methods for BinMatCount-class, each function is a
#'     method to acces to each slot in BinMat-class
#'
#' @name set
#'
#' @param object A \code{\link[rspecies]{BinMatCount}} object.
#'
#' @return return a slot of BinMat object.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}).

NULL

# Setters ----------------------------------------------------

#' @rdname set
#' @name setN
#' @aliases setN
#' @docType methods
#' @export


setGeneric("setN<-",function(object, value){standardGeneric ("setN<-")})

setReplaceMethod (f = "setN",
           signature = "BinMatCount",
           definition = function(object, value){
             object@Count$N <- value
             validObject(object)
             return(object)
           }
)


#' @rdname set
#' @name setNc
#' @aliases setNc
#' @docType methods
#' @export


setGeneric("setNc<-",function(object, value){standardGeneric ("setNc<-")})

setReplaceMethod (f = "setNc",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Nc <- value
                    validObject(object)
                    return(object)
                  }
)


#' @rdname set
#' @name setNx
#' @docType methods
#' @export


setGeneric("setNx<-",function(object, value){standardGeneric ("setNx<-")})

setReplaceMethod (f = "setNx",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Nx <- value
                    validObject(object)
                    return(object)
                  }
)


#' @rdname set
#' @name setNcx
#' @docType methods
#' @export


setGeneric("setNcx<-",function(object, value){standardGeneric ("setNcx<-")})

setReplaceMethod (f = "setNcx",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Ncx <- value
                    validObject(object)
                    return(object)
                  }
)
