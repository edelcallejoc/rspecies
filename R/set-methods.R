
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

#' @rdname set
#' @name setN-gen
#' @aliases setN-gen
#' @docType methods
#' @export
#'

setGeneric("setN<-",function(object, value){standardGeneric ("setN<-")})


#' @rdname set
#' @name setN
#' @aliases setN
#' @docType methods
#' @export

setReplaceMethod (f = "setN",
           signature = "BinMatCount",
           definition = function(object, value){
             object@Count$N <- value
             validObject(object)
             return(object)
           }
)


#' @rdname set
#' @name setNc-gen
#' @aliases setNc-gen
#' @docType methods
#' @export
#'

setGeneric("setNc<-",function(object, value){standardGeneric ("setNc<-")})


#' @rdname set
#' @name setNc
#' @aliases setNc
#' @docType methods
#' @export

setReplaceMethod (f = "setNc",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Nc <- value
                    validObject(object)
                    return(object)
                  }
)


#' @rdname set
#' @name setNx-gen
#' @aliases setNx-gen
#' @docType methods
#' @export

setGeneric("setNx<-",function(object, value){standardGeneric ("setNx<-")})

#' @rdname set
#' @name setNx
#' @aliases setNx
#' @docType methods
#' @export

setReplaceMethod (f = "setNx",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Nx <- value
                    validObject(object)
                    return(object)
                  }
)


#' @rdname set
#' @name setNcx-gen
#' @aliases setNcx-gen
#' @docType methods
#' @export


setGeneric("setNcx<-",function(object, value){standardGeneric ("setNcx<-")})


#' @rdname set
#' @name setNcx
#' @aliases setNcx
#' @docType methods
#' @export

setReplaceMethod (f = "setNcx",
                  signature = "BinMatCount",
                  definition = function(object, value){
                    object@Count$Ncx <- value
                    validObject(object)
                    return(object)
                  }
)
