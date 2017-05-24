
#' Accesor methods for BinMat class and subclass
#' @include BinMatCount-class.R
#'
#' @description The accesor methods for BinMat-class, each function is a
#'     method to acces to each slot in BinMat-class
#'
#' @name get
#'
#' @param object A \code{\link[rspecies]{BinMat}} object.
#'
#' @return return a slot of BinMat object.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).

NULL

# Accessors ----------------------------------------------------

#' @rdname get
#' @name getName_ID
#' @aliases getName_ID
#' @docType methods
#' @export


setGeneric("getName_ID",function(object){standardGeneric ("getName_ID")})

setMethod (f = "getName_ID",
           signature = "BinMat",
           definition = function(object){
             return(object@name_ID)
           }
)

#' @rdname get
#' @name getDMNB
#' @aliases getDMNB
#' @docType methods
#' @export

setGeneric("getDMNB",function(object){standardGeneric ("getDMNB")})

setMethod (f = "getDMNB",
           signature = "BinMat",
           definition = function(object){
             return(object@DMNB)
           }
)


#' @rdname get
#' @name getBMNB
#' @aliases getBMNB
#' @docType methods
#' @export

setGeneric("getBMNB",function(object){standardGeneric ("getBMNB")})

setMethod (f = "getBMNB",
           signature = "BinMat",
           definition = function(object){
             return(object@BMNB)
           }
)


#' @rdname get
#' @name getN
#' @aliases getN
#' @docType methods
#' @export

setGeneric("getN",function(object){standardGeneric ("getN")})

setMethod (f = "getN",
           signature = "BinMatCount",
           definition = function(object){
             return(object@Count$N)
           }
)

#' @rdname get
#' @name getNc
#' @aliases getNc
#' @docType methods
#' @export

setGeneric("getNc",function(object){standardGeneric ("getNc")})

setMethod (f = "getNc",
           signature = "BinMatCount",
           definition = function(object){
             return(object@Count$Nc)
           }
)

#' @rdname get
#' @name getNx
#' @aliases getNx
#' @docType methods
#' @export

setGeneric("getNx",function(object){standardGeneric ("getNx")})

setMethod (f = "getNx",
           signature = "BinMatCount",
           definition = function(object){
             return(object@Count$Nx)
           }
)

#' @rdname get
#' @name getNcx
#' @aliases getNcx
#' @docType methods
#' @export

setGeneric("getNcx",function(object){standardGeneric ("getNcx")})

setMethod (f = "getNcx",
           signature = "BinMatCount",
           definition = function(object){
             return(object@Count$Ncx)
           }
)


#' @rdname get
#' @name getPc
#' @aliases getPc
#' @docType methods
#' @export

setGeneric("getPc",function(object){standardGeneric ("getPc")})

setMethod (f = "getPc",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Pc)
           }
)


#' @rdname get
#' @name getPx
#' @aliases getPx
#' @docType methods
#' @export

setGeneric("getPx",function(object){standardGeneric ("getPx")})

setMethod (f = "getPx",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Px)
           }
)

#' @rdname get
#' @name getPcx
#' @aliases getPcx
#' @docType methods
#' @export

setGeneric("getPcx",function(object){standardGeneric ("getPcx")})

setMethod (f = "getPcx",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Pcx)
           }
)


#' @rdname get
#' @name getPcnx
#' @aliases getPcnx
#' @docType methods
#' @export

setGeneric("getPcnx",function(object){standardGeneric ("getPcnx")})

setMethod (f = "getPcnx",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Pcnx)
           }
)

#' @rdname get
#' @name getPxc
#' @aliases getPxc
#' @docType methods
#' @export

setGeneric("getPxc",function(object){standardGeneric ("getPxc")})

setMethod (f = "getPxc",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Pxc)
           }
)


#' @rdname get
#' @name getPxnc
#' @aliases getPxnc
#' @docType methods
#' @export

setGeneric("getPxnc",function(object){standardGeneric ("getPxnc")})

setMethod (f = "getPxnc",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Pxnc)
           }
)

#' @rdname get
#' @name getPnxc
#' @aliases getPnxc
#' @docType methods
#' @export

setGeneric("getPnxc",function(object){standardGeneric ("getPnxc")})

setMethod (f = "getPnxc",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Pnxc)
           }
)

#' @rdname get
#' @name getPnxnc
#' @aliases getPnxnc
#' @docType methods
#' @export

setGeneric("getPnxnc",function(object){standardGeneric ("getPnxnc")})

setMethod (f = "getPnxnc",
           signature = "BinMatProb",
           definition = function(object){
             return(object@Prob$Pnxnc)
           }
)


#' @rdname get
#' @name getEcx
#' @aliases getEcx
#' @docType methods
#' @export

setGeneric("getEcx",function(object){standardGeneric ("getEcx")})

setMethod (f = "getEcx",
           signature = "BinMatEps",
           definition = function(object){
             return(object@Epsilon$Ecx)
           }
)


#' @rdname get
#' @name getEncx
#' @aliases getEncx
#' @docType methods
#' @export

setGeneric("getEncx",function(object){standardGeneric ("getEncx")})

setMethod (f = "getEncx",
           signature = "BinMatEps",
           definition = function(object){
             return(object@Epsilon$Encx)
           }
)


#' @rdname get
#' @name getApr
#' @docType methods
#' @export

setGeneric("getApr",function(object){standardGeneric ("getApr")})

setMethod (f = "getApr",
           signature = "BinMatScore",
           definition = function(object){
             return(object@Score$Apriori)
           }
)

#' @rdname get
#' @name getScx
#' @aliases getScx
#' @docType methods
#' @export

setGeneric("getScx",function(object){standardGeneric ("getScx")})

setMethod (f = "getScx",
           signature = "BinMatScore",
           definition = function(object){
             return(object@Score$Scx)
           }
)


#' @rdname get
#' @name getScnx
#' @aliases getScnx
#' @docType methods
#' @export

setGeneric("getScnx",function(object){standardGeneric ("getScnx")})

setMethod (f = "getScnx",
           signature = "BinMatScore",
           definition = function(object){
             return(object@Score$Scnx)
           }
)
