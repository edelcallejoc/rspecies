
#' Score function for NB classifier for Spatial Data Mining
#'
#' @name score
#'
#' @param prob.mat A \code{list} object from \code{\link[rspecies]{probs}}.
#' @param count.mat A \code{list} object from \code{\link[rspecies]{counts}}.
#' @param laplace numeric. See \code{\link[rspecies]{laplace}}.
#'
#' @return This function returns a \code{list} object with 11 elements.
#'     The elements are: Apriori, Scx, Scnx, SEcx, SEcnx, ZScx, ZScnx,
#'     LLScx, ULScx, LLScx and ULScx, (see Details for further explanation).
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
#' load('./data/Mex0.rda')
#' load('./data/mammals.RData')
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals with colnames = NULL.
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals, colnames = NULL)
#'
#' # Counting matrices
#' count.mat<-counts(x.mat)
#'
#' # Including bioclim
#' # Extracting values from 19 bioclim variables
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
#' datatable(data.frame(Scx = score.mat@Score$Scx[169,]),
#'     rownames = score.mat@name_ID$Name,
#'     colnames = score.mat@name_ID$Name[169],
#'     caption = "laplace factor: 0.1")%>%
#'     formatRound(1,digits = 3)
#'
#' datatable(data.frame(Scx = score.bmat$Scx[,1],
#'     SEcx = score.bmat$SEcx[,1],
#'     ZScx = score.bmat$ZScx[,1],
#'     LLScx = score.bmat$LLScx[,1],
#'     ULScx = score.bmat$ULScx[,1]),
#'     caption = "Target: X1 - Laplace's factor: 0.1")%>%
#'     formatRound(1:5,digits = 3)



NULL


# Generic definition ------------------------------------------------------

setGeneric("score",function(prob.mat, count.mat, lap_fac = 0.1,...){
  standardGeneric ("score")})


#' @rdname epsilon
#' @name epsilon
#' @docType methods
#' @export
#'

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
