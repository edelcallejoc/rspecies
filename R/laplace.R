
#' Laplace correction for spatial data mining.
#'
#' @name laplace
#'
#' @description Take a BinMatCount object from
#'     \code{\link[rspecies]{counts}} and adding a factor
#'     between 0 and 1 to avoid entries with 0.
#'
#' @param count.mat A BinMatCount object. See documentation
#'     \code{\link[rspecies]{BinMatCount}}
#'
#' @param lap_fac numeric. by default 0.1 (see Details for further explanation).
#'
#' @return A list object with the same structure as \code{\link[rspecies]{BinMatCount}}.
#'     The correspond addings are: N  adds 2 * laplace, Nc and Nx add
#'     laplace, and Ncx adds laplace/2.
#'
#' @details This function is motivated by the follow formulation:
#'     \deqn{P(C|X)P(X) = P(X|C)P(C)}
#'     \deqn{\frac{N_{cx}}{N_{x}}*\frac{N_{x}}{N} = \frac{N_{cx}}{N_{c}}*\frac{N_{c}}{N}}
#'     \deqn{\frac{N_{cx}+\frac{\alpha}{|C|}}{N_{x}+\alpha}*\frac{N_{x}+\alpha}{N+ 2*\alpha}
#'      = \frac{N_{cx}+\frac{\alpha}{|X|}}{N_{c}+\alpha}*\frac{N_{c}+\alpha}{N+2*\alpha}}
#'      Where, \eqn{\alpha} is a number between 0 and 1 and \eqn{|C|, |X|} are the cardinalities of
#'      the sets C and X respectively.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @examples
#' #' library(sp)
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
#' count.mat <- counts(x.mat)
#'
#' # laplace correction with laplace factor of 0.1
#' lap.mat <- laplace(count.mat)
#'
#'

NULL

# Generic definition ------------------------------------------------------

setGeneric("laplace",function(count.mat, lap_fac = 0.1){standardGeneric ("laplace")})

#' @rdname laplace
#' @name laplace
#' @aliases laplace
#' @docType methods
#' @export


setMethod("laplace", c("BinMat", "ANY"),
          function(count.mat, lap_fac = 0.1){

    if(lap_fac <= 0 | lap_fac > 1){
    stop("lap.fac must be a number between 0 and 1.")
  }

  setN(count.mat)<-getN(count.mat)+ (2*lap_fac)
  setNc(count.mat)<-getNc(count.mat)+ lap_fac
  setNx(count.mat)<-getNx(count.mat)+ lap_fac
  setNcx(count.mat)<-getNcx(count.mat)+ (lap_fac/2)
  return(count.mat)

})


