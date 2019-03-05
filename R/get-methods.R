
#' Accesor methods for SpNaBa-objects
#'
#' @description \code{get_} functions are methods to extract slot from \code{SpNaBaMatrix},
#'     \code{SpNaBaCounts}, \code{SpNaBaProbs} objects. The accesor functions are build in
#'     with a snake style; i.e. get_suffix, where the suffix are de abbreciation of the slot
#'     to extract. For example, the suffix \code{FM} (\code{get_FM()}) stands to refer the
#'     \code{freqmatrix} slot of the \code{SpNaBaMatrix} object. In the same way, the suffix
#'     \code{BM} (\code{get_BM()}) stands to refer the \code{binmatrix} of the
#'     \code{SpNaBaMatrix} object.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @name get_FM
#' @rdname get-methods
#' @exportMethod get_FM
#'

setGeneric("get_FM", function(x.mat){standardGeneric ("get_FM")})

#' @rdname get-methods
#' @aliases get_FM,get-methods
#' @usage NULL
#'
#' @param x.mat A \code{\link[rspecies]{SpNaBaMatrix}} or
#'     \code{\link[rspecies]{SpNaBaModel}} object.
#'
#' @return \code{get_FM} return the slot \code{freqmatrix} a \code{SpNaBaMatrix} object.
#'
#' @seealso \code{\link[rspecies]{SpNaBaMatrix}}

#' @examples
#' # Using the function id_pts() for spatial data --------
#'
#' library(sp)
#' library(rgeos)
#' library(rgdal)
#' library(raster)
#'
#' # Loading data
#' data(Mex0)
#' data(mammals)
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals)
#'
#' # Extracting Frequqency matrix
#'
#' FM <- get_FM(x.mat)
#'
#' FM[1:5,1:5]


setMethod (f = "get_FM",
           signature = "SpNaBaMatrix",
           definition = function(x.mat){
             return(x.mat@freqmatrix)
           }
)

#' @rdname get-methods
#' @aliases get_FM,get-methods
#' @usage NULL
#'

setMethod (f = "get_FM",
           signature = "SpNaBaModel",
           definition = function(x.mat){
             return(get_FM(x.mat@ModMatrix))
           }
)


#' @name get_BM
#' @rdname get-methods
#' @exportMethod get_BM
#'

setGeneric("get_BM", function(x.mat){standardGeneric ("get_BM")})

#' @rdname get-methods
#' @aliases get_BM,get-methods
#' @usage NULL
#'
#' @return \code{get_BM} return the slot \code{binmatrix} a \code{SpNaBaMatrix} object.
#'
#' @examples
#'
#' # Extracting Binary matrix
#'
#' BM <- get_BM(x.mat)
#'
#' BM[1:5,1:5]


setMethod (f = "get_BM",
           signature = "SpNaBaMatrix",
           definition = function(x.mat){
             return(x.mat@binmatrix)
           }
)

#' @rdname get-methods
#' @aliases get_BM,get-methods
#' @usage NULL
#'

setMethod (f = "get_BM",
           signature = "SpNaBaModel",
           definition = function(x.mat){
             return(get_BM(x.mat@ModMatrix))
           }
)



#' @name get_N
#' @rdname get-methods
#' @exportMethod get_N
#'

setGeneric("get_N", function(x.counts){standardGeneric ("get_N")})

#' @rdname get-methods
#' @aliases get_N,get-methods
#' @usage NULL
#'
#' @param x.counts A \code{\link[rspecies]{SpNaBaCounts}} object  or
#'     \code{\link[rspecies]{SpNaBaModel}} object.
#'
#' @return \code{get_N} return the slot \code{N} a \code{SpNaBaCounts} object.
#'
#' @examples
#'
#' # Calculating counts
#'
#' x.counts <- counts(x.mat)
#'
#' # Extracting N
#'
#' N <- get_N(x.counts)
#' N


setMethod (f = "get_N",
           signature = "SpNaBaCounts",
           definition = function(x.counts){
             return(x.counts@N)
           }
)

#' @rdname get-methods
#' @aliases get_N,get-methods
#' @usage NULL
#'

setMethod (f = "get_N",
           signature = "SpNaBaModel",
           definition = function(x.counts){
             return(get_N(x.counts@Counts))
           }
)



#' @name get_Nc
#' @rdname get-methods
#' @exportMethod get_Nc
#'

setGeneric("get_Nc", function(x.counts){standardGeneric ("get_Nc")})

#' @rdname get-methods
#' @aliases get_Nc,get-methods
#' @usage NULL
#'
#' @return \code{get_Nc} return the slot \code{Nc} a \code{SpNaBaCounts} object.
#'
#' @examples
#'
#' # Extracting Nc
#'
#' Nc <- get_Nc(x.counts)
#' Nc


setMethod (f = "get_Nc",
           signature = "SpNaBaCounts",
           definition = function(x.counts){
             return(x.counts@Nc)
           }
)

#' @rdname get-methods
#' @aliases get_Nc,get-methods
#' @usage NULL
#'

setMethod (f = "get_Nc",
           signature = "SpNaBaModel",
           definition = function(x.counts){
             return(get_Nc(x.counts@Counts))
           }
)




#' @name get_Nx
#' @rdname get-methods
#' @exportMethod get_Nx
#'

setGeneric("get_Nx", function(x.counts){standardGeneric ("get_Nx")})

#' @rdname get-methods
#' @aliases get_Nx,get-methods
#' @usage NULL
#'
#' @return \code{get_Nx} return the slot \code{Nx} a \code{SpNaBaCounts} object.
#'
#' @examples
#'
#' # Extracting Nx
#'
#' Nx <- get_Nx(x.counts)
#' Nx


setMethod (f = "get_Nx",
           signature = "SpNaBaCounts",
           definition = function(x.counts){
             return(x.counts@Nx)
           }
)


#' @rdname get-methods
#' @aliases get_Nx,get-methods
#' @usage NULL
#'

setMethod (f = "get_Nx",
           signature = "SpNaBaModel",
           definition = function(x.counts){
             return(get_Nx(x.counts@Counts))
           }
)



#' @name get_Ncx
#' @rdname get-methods
#' @exportMethod get_Ncx
#'

setGeneric("get_Ncx", function(x.counts){standardGeneric ("get_Ncx")})

#' @rdname get-methods
#' @aliases get_Ncx,get-methods
#' @usage NULL
#'
#' @return \code{get_Ncx} return the slot \code{Ncx} a \code{SpNaBaCounts} object.
#'
#' @examples
#'
#' # Extracting Ncx
#'
#' Ncx <- get_Ncx(x.counts)
#' Ncx


setMethod (f = "get_Ncx",
           signature = "SpNaBaCounts",
           definition = function(x.counts){
             return(x.counts@Ncx)
           }
)


#' @rdname get-methods
#' @aliases get_Ncx,get-methods
#' @usage NULL
#'

setMethod (f = "get_Ncx",
           signature = "SpNaBaModel",
           definition = function(x.counts){
             return(get_Ncx(x.counts@Counts))
           }
)





#' @name get_Pc
#' @rdname get-methods
#' @exportMethod get_Pc
#'

setGeneric("get_Pc", function(x.probs){standardGeneric ("get_Pc")})

#' @rdname get-methods
#' @aliases get_Pc,get-methods
#' @usage NULL
#'
#' @param x.probs A \code{\link[rspecies]{SpNaBaProbs}} object  or
#'     \code{\link[rspecies]{SpNaBaModel}} object.
#'
#' @return \code{get_Pc} return the slot \code{Pc} a \code{SpNaBaProbs} object.
#'
#' @examples
#'
#' # Calculating probabilities
#'
#' x.probs <- probs(x.counts, fac.lap = 0.1)
#'
#' # Extracting Pc
#'
#' Pc <- get_Pc(x.probs)
#' Pc


setMethod (f = "get_Pc",
           signature = "SpNaBaProbs",
           definition = function(x.probs){
             return(x.probs@Pc)
           }
)

#' @rdname get-methods
#' @aliases get_Pc,get-methods
#' @usage NULL
#'

setMethod (f = "get_Pc",
           signature = "SpNaBaModel",
           definition = function(x.probs){
             return(get_Pc(x.probs@Probs))
           }
)


#' @name get_Px
#' @rdname get-methods
#' @exportMethod get_Px
#'

setGeneric("get_Px", function(x.probs){standardGeneric ("get_Px")})

#' @rdname get-methods
#' @aliases get_Px,get-methods
#' @usage NULL
#'
#' @return \code{get_Px} return the slot \code{Px} a \code{SpNaBaProbs} object.
#'
#' @examples
#'
#' # Extracting Px
#'
#' Px <- get_Px(x.probs)
#' Px


setMethod (f = "get_Px",
           signature = "SpNaBaProbs",
           definition = function(x.probs){
             return(x.probs@Px)
           }
)


#' @rdname get-methods
#' @aliases get_Px,get-methods
#' @usage NULL
#'

setMethod (f = "get_Px",
           signature = "SpNaBaModel",
           definition = function(x.probs){
             return(get_Px(x.probs@Probs))
           }
)


#' @name get_Pcx
#' @rdname get-methods
#' @exportMethod get_Pcx
#'

setGeneric("get_Pcx", function(x.probs){standardGeneric ("get_Pcx")})

#' @rdname get-methods
#' @aliases get_Pcx,get-methods
#' @usage NULL
#'
#' @return \code{get_Pcx} return the slot \code{Pc.x} a \code{SpNaBaProbs} object.
#'
#' @examples
#'
#' # Extracting Pc.x
#'
#' Pc.x <- get_Pcx(x.probs)
#' Pc.x


setMethod (f = "get_Pcx",
           signature = "SpNaBaProbs",
           definition = function(x.probs){
             return(x.probs@Pc.x)
           }
)

#' @rdname get-methods
#' @aliases get_Pcx,get-methods
#' @usage NULL
#'

setMethod (f = "get_Pcx",
           signature = "SpNaBaModel",
           definition = function(x.probs){
             return(get_Pcx(x.probs@Probs))
           }
)


#' @name get_Pxc
#' @rdname get-methods
#' @exportMethod get_Pxc
#'

setGeneric("get_Pxc", function(x.probs){standardGeneric ("get_Pxc")})

#' @rdname get-methods
#' @aliases get_Pxc,get-methods
#' @usage NULL
#'
#' @return \code{get_Pxc} return the slot \code{Px.c} a \code{SpNaBaProbs} object.
#'
#' @examples
#'
#' # Extracting Px.c
#'
#' Px.c <- get_Pxc(x.probs)
#' Px.c


setMethod (f = "get_Pxc",
           signature = "SpNaBaProbs",
           definition = function(x.probs){
             return(x.probs@Px.c)
           }
)

#' @rdname get-methods
#' @aliases get_Pxc,get-methods
#' @usage NULL
#'

setMethod (f = "get_Pxc",
           signature = "SpNaBaModel",
           definition = function(x.probs){
             return(get_Pxc(x.probs@Probs))
           }
)



#' @name get_Pxnc
#' @rdname get-methods
#' @exportMethod get_Pxnc
#'

setGeneric("get_Pxnc", function(x.probs){standardGeneric ("get_Pxnc")})


#' @rdname get-methods
#' @aliases get_Pxnc,get-methods
#' @usage NULL
#'
#' @return \code{get_Pxnc} return the slot \code{Px.nc} a \code{SpNaBaProbs} object.
#'
#' @examples
#'
#' # Extracting Px.nc
#'
#' Px.nc <- get_Pxnc(x.probs)
#' Px.nc


setMethod (f = "get_Pxnc",
           signature = "SpNaBaProbs",
           definition = function(x.probs){
             return(x.probs@Px.nc)
           }
)


#' @rdname get-methods
#' @aliases get_Pxnc,get-methods
#' @usage NULL
#'

setMethod (f = "get_Pxnc",
           signature = "SpNaBaModel",
           definition = function(x.probs){
             return(get_Pxnc(x.probs@Probs))
           }
)




#' @name get_Ecx
#' @rdname get-methods
#' @exportMethod get_Ecx
#'

setGeneric("get_Ecx", function(x.eps){standardGeneric ("get_Ecx")})

#' @rdname get-methods
#' @aliases get_Ecx,get-methods
#' @usage NULL
#'
#' @param x.eps A \code{SpNaBaEps} object, the resulting object of epsilon function
#'     or A \code{SpNaBaModel} object. The resulting object of NBModel function.
#'
#' @return \code{get_Ecx} return the slot \code{Ecx} of a \code{SpNaBaEps} object.
#'
#' @examples
#'
#' # Calculating epsilon
#'
#' x.eps <- epsilon(x.counts, x.probs)
#'
#'
#' # Extracting Epsilon matrix
#'
#' Ecx <- get_Ecx(x.eps)
#'
#' Ecx[1:5,1:5]


setMethod (f = "get_Ecx",
           signature = "SpNaBaEps",
           definition = function(x.eps){
             return(x.eps@Ecx)
           }
)




#' @rdname get-methods
#' @aliases get_Ecx,get-methods
#' @usage NULL

setMethod (f = "get_Ecx",
           signature = "SpNaBaModel",
           definition = function(x.eps){
             return(get_Ecx(x.eps@Epsilon))
           }
)



#' @name get_Apriori
#' @rdname get-methods
#' @exportMethod get_Apriori
#'

setGeneric("get_Apriori", function(x.score){standardGeneric ("get_Apriori")})

#' @rdname get-methods
#' @aliases get_Apriori,score-methods
#' @usage NULL
#'
#' @param x.score A \code{SpNaBaScore} object. The resulting object of score function
#'     or A \code{SpNaBaModel} object. The resulting object of NBModel function.
#'
#' @return \code{get_Apriori} return the slot \code{Apriori} a \code{SpNaBaScore} object.
#'
#' @examples
#'
#' # Calculating score
#'
#' x.score <- score(x.probs)
#'
#'
#' # Extracting Apriori slot
#'
#' S.Apriori <- get_Apriori(x.score)
#'
#' S.Apriori[1:5]


setMethod (f = "get_Apriori",
           signature = "SpNaBaScore",
           definition = function(x.score){
             return(x.score@Apriori)
           }
)


#' @rdname get-methods
#' @aliases get_Apriori,get-methods
#' @usage NULL

setMethod (f = "get_Apriori",
           signature = "SpNaBaModel",
           definition = function(x.score){
             return(get_Apriori(x.score@Score))
           }
)


#' @name get_Scx
#' @rdname get-methods
#' @exportMethod get_Scx
#'

setGeneric("get_Scx", function(x.score){standardGeneric ("get_Scx")})

#' @rdname get-methods
#' @aliases get_Scx,get-methods
#' @usage NULL
#' @return \code{get_Scx} return the slot \code{Scx} of a \code{SpNaBaScore} object.
#'
#' @examples
#'
#' # Extracting Score matrix
#'
#' Scx <- get_Scx(x.score)
#'
#' Scx[1:5, 1:5]


setMethod (f = "get_Scx",
           signature = "SpNaBaScore",
           definition = function(x.score){
             return(x.score@Scx)
           }
)



#' @rdname get-methods
#' @aliases get_Scx,get-methods
#' @usage NULL

setMethod (f = "get_Scx",
           signature = "SpNaBaModel",
           definition = function(x.score){
             return(get_Scx(x.score@Score))
           }
)

