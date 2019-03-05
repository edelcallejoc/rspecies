
#' Correlation method for the Spatial Naive Bayes Model
#'
#' @description \code{epsilon} is a function to estimates epsilon parameter for
#'     the Spatial Naive Bayes Model.
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @name epsilon
#' @rdname epsilon
#' @usage NULL
#' @exportMethod epsilon
#'

setGeneric("epsilon", function(x.counts, x.probs) standardGeneric("epsilon"))


#' @rdname epsilon
#' @aliases epsilon,SpNaba-methods,epsilon-methods
#'
#' @param x.counts A \code{\link[rspecies]{SpNaBaCounts}} object.
#'
#' @param x.probs A \code{\link[rspecies]{SpNaBaProbs}} object.
#'
#' @return \code{epsilon}: An object of class \code{\link[rspecies]{SpNaBaEps}}.
#'
#' @seealso \code{\link[rspecies]{SpNaBaCounts}}, \code{\link[rspecies]{SpNaBaProbs}},
#'      \code{\link[rspecies]{SpNaBa-methods}}
#'
#'
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
#' # Without target declaration --------------------------
#'
#' system.time(x.counts <- counts(x.mat))
#'
#' # With target declaration -----------------------------
#'
#' x.counts <- counts(x.mat, target = 1:10)
#'
#' # Probability matrices -----------------------------------
#' x.probs <- probs(x.counts, fac.lap = 0.1)
#'
#' # Epsilon parameter -----------------------------------
#'
#' x.eps <- epsilon(x.counts, x.probs)
#'


setMethod("epsilon", c("SpNaBaCounts", "SpNaBaProbs"),
          function(x.counts, x.probs){

            # Extracting Nx ----------------------------------

            Nx <- get_Nx(x.counts)

            # Extracting Ps ----------------------------------

            Pc <- get_Pc(x.probs)
            Pcdim <- length(Pc)

            Pcx <- get_Pcx(x.probs)
            Pcxdim<- dim(Pcx)[2]

            # Creating auxiliar matrices ---------------------
            MPc <- matrix(rep(Pc, Pcxdim), ncol = Pcxdim,
                          dimnames = list(names(Pc), colnames(Pcx)))

            MNx <- matrix(rep(Nx, Pcdim), nrow = Pcdim, byrow = TRUE,
                          dimnames = list(names(Pc), names(Nx)))

            # Calculating the epsilon parameters -------------

            NumEcx <- MNx * (Pcx - MPc) # Numerator

            DenE <- sqrt(MNx * MPc * (1-MPc)) # denominator

            Ecx <- NumEcx/DenE # Epsilon matrix


            output <- SpNaBaEps(Ecx = Ecx)

            return(output)
          })

