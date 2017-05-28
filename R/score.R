
#' Score function for NB classifier for Spatial Data Mining
#'
#' @param prob.mat A \code{BinMatProb} object from \code{\link[rspecies]{probs}}.
#' @param count.mat A \code{BinMatCount} object from \code{\link[rspecies]{counts}}.
#' @param lap_fac numeric. See \code{\link[rspecies]{laplace}}.
#' @param ... pass to another methods.
#'
#' @return a \code{\link[rspecies:BinMatScore-class]{BinMatScore}} object.
#'
#' @details The elements of the list are defined as follow.
#'
#'     \strong{Apriori} a numeric vector. The apriori factor
#'     \eqn{\ln{\frac{P(C)}{P(C')}}}, where C' is the complement of C.
#'
#'     \strong{Scx} a numeric matrix. The Score factor of each explanatory variables,
#'     \eqn{\ln{\frac{P(X|C)}{P(X|C')}}}. Rows corresponds to X and columns corresponds
#'     to C.
#'
#'     \strong{Scnx} a numeric matrix. The Score factor of X's complement,
#'     \eqn{\ln{\frac{P(X'|C)}{P(X'|C')}}}. Rows corresponds to X' and columns corresponds
#'     to C.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' data(Mex0)
#' data(mammals)
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals with colnames especified.
#' names <- paste("X", 1:nlevels(as.factor(mammals@data$nameID)), sep = "")
#'
#' x.mat <- id_pts(grd = Mex0.grd, pts = mammals, colnames = names)
#' # Counting matrices
#' count.mat<-counts(x.mat)
#'
#' # Including bioclim
#' # Extracting values from 19 bioclim variables
#' library(raster)
#' bioclim <- getData('worldclim', var='bio', res=2.5)
#' bio.sp <- raster_breaks(bioclim, Mex0.grd)
#' bnames <- as.character(levels(as.factor(bio.sp@data$nameID)))
#' bio.mat <- id_pts(grd = Mex0.grd, pts = bio.sp, colnames = bnames)
#' count.bmat <- counts(x.mat, target = c("X1","X2"), bioclim = bio.mat)
#'
#' # Probability matrices
#' prob.mat<-probs(count.mat, lap_fac = 0.1)
#' prob.bmat<-probs(count.bmat, lap_fac = 0.1)
#'
#' #score function
#' score.mat<-score(prob.mat, count.mat)
#' score.bmat<-score(prob.bmat, count.bmat)
#'
#' # See data with DT package
#' library(DT)
#'
#' datatable(data.frame(Scx = getScx(score.mat)[,c(1,2)]),
#'     rownames = getName_ID(score.mat)[,1],
#'     colnames = getName_ID(score.mat)[c(1,2),1],
#'     caption = "Score - laplace factor: 0.1")%>%
#'     formatRound(1:2,digits = 3)
#'
#' datatable(data.frame(Scx = getScx(score.bmat)[,c(1,2)]),
#'     rownames = getName_ID(score.bmat)[-c(1,2),1],
#'     colnames = getName_ID(score.bmat)[c(1,2),1],
#'     caption = "Score - laplace factor: 0.1")%>%
#'     formatRound(1:2,digits = 3)
#'

# Generic definition ------------------------------------------------------

#' @export
#' @docType methods
#' @rdname score

setGeneric("score",function(prob.mat, count.mat, lap_fac = 0.1,...){
  standardGeneric ("score")})


#' @rdname score
#' @aliases score,BinMatProb,BinMatCount,ANY-method

setMethod("score", c("BinMatProb", "BinMatCount", "ANY"),
           function(prob.mat, count.mat, lap_fac = 0.1, ...){

  # Arguments verification -------------------------------------------

  if(lap_fac != 0){
    count.mat <- laplace(count.mat = count.mat, lap_fac = lap_fac)
  }
  # -------------------------------------------------------------------

  apri <- log(getPc(prob.mat)/(1-getPc(prob.mat)))

  Scx <- log(getPxc(prob.mat)/getPxnc(prob.mat))

  Scnx <- log(getPnxc(prob.mat)/getPnxnc(prob.mat))

  Sco_ls <- list(Apriori = apri, Scx = Scx, Scnx = Scnx)

  output <- BinMatScore(name_ID = prob.mat@name_ID, DMNB = prob.mat@DMNB,
                        BMNB = prob.mat@BMNB, Score = Sco_ls)

  return(output)

})
