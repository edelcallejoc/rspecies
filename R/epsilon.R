
#' Epsilon function for NB Hypothesis Testing for Spatial Data Mining
#'
#' @description Take a \code{list} object from \code{\link[rspecies]{probs}}
#'     and a \code{list} object from \code{\link[rspecies]{counts}} and
#'     calculates the Epsilon values for Naive Bayes Hypothesis Testing
#'     for Spatial Data Mining.
#'
#' @name epsilon
#'
#' @param prob.mat a \code{list} object from \code{\link[rspecies]{probs}}.
#' @param count.mat a \code{list} object from \code{\link[rspecies]{counts}}.
#' @param lap_fac numeric. See \code{\link[rspecies]{laplace}}.
#'
#' @return This function returns a \code{list} object with 2 elements.
#'     The elements are: Ecx and Ecnx (see Details for further explanation).
#'
#' @details The elements of the list are defined as follow.
#'
#'     \strong{Ecx}: numeric matrix. The epsilon value for C given X is
#'     calculated as \eqn{\frac{Nx(P(C|X)-P(C))}{\sqrt{Nx*P(C)*(1-P(C))}}}.
#'     Where \eqn{Nx} is the number of X's successes, \eqn{P(C|X)} is the
#'     conditional probability of C given X and \eqn{P(C)} is the probability
#'     of succes of C.
#'
#'     \strong{Ecnx}: numeric matrix. The epsilon value for C given X's
#'     complement is calculated as \eqn{\frac{Nx'(P(C|X')-P(C))}{\sqrt{Nx'*P(C)*(1-P(C))}}}.
#'     Where \eqn{Nx'} is the number of X's failures, \eqn{P(C|X')} is the
#'     conditional probability of C given X's complement and \eqn{P(C)}
#'     is the probability of succes of C.
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
#' prob.mat<-probs(count.mat, laplace = 0.1)
#' prob.bmat<-probs(count.bmat, laplace = 0.1)
#'
#' # Epsilon function
#' system.time(epsilon.mat<-epsilon(prob.mat, count.mat))
#' system.time(epsilon.bmat<-epsilon(prob.bmat, count.bmat))
#'
#' #' # See data with DT package
#' library(DT)
#' datatable(data.frame(Ecx = epsilon.mat$Ecx[,1]),
#'     caption = "Target: X1 - Laplace's factor: 0.1")%>%
#'     formatRound(1:5,digits = 3)

NULL

# Generic definition ------------------------------------------------------

#' @rdname epsilon
#' @name epsilon-gen
#' @aliases epsilon-generic
#' @docType methods
#' @export


setGeneric("epsilon",function(prob.mat, count.mat, lap_fac = 0.1){
           standardGeneric ("epsilon")})


#' @rdname epsilon
#' @name epsilon
#' @aliases epsilon
#' @docType methods
#' @export
#'

setMethod("epsilon", c("BinMatProb", "BinMatCount", "ANY"),
           function(prob.mat, count.mat, lap_fac = 0.1){

    if(lap_fac != 0){
    count.mat <- laplace(count.mat = count.mat, lap_fac = lap_fac)
  }
  # -------------------------------------------------------------------

  Nx <- getNx(count.mat)

  Pc <- getPc(prob.mat)
  Pcdim<- dim(Pc)[1]
  Pnc <- 1-Pc
  Pncdim<- dim(Pnc)[1]

  Pcx <- getPcx(prob.mat)
  Pcxdim<- dim(Pcx)[2]
  Pcnx <- getPcnx(prob.mat)

  MPc <- matrix(rep(Pc, Pcxdim), ncol = Pcxdim)
  rownames(MPc) <- rownames(Pc)
  colnames(MPc) <- colnames(Pcx)

  MPnc <- matrix(rep(Pnc, Pcxdim), ncol = Pcxdim)
  rownames(MPnc) <- rownames(Pnc)
  colnames(MPnc) <- colnames(Pcnx)

  MNx <- matrix(rep(Nx, Pcdim), nrow = Pcdim)
  rownames(MNx) <- rownames(Pc)
  colnames(MNx) <- rownames(Nx)

  MNx <- matrix(rep(Nx, Pcdim), nrow = Pcdim)
  rownames(MNx) <- rownames(Pc)
  colnames(MNx) <- rownames(Nx)

  NumEcx <- MNx * (Pcx - MPc)
  NumEncx <- MNx * (Pcx - MPnc)

  DenE <- sqrt(MNx * MPc * MPnc)

  Ecx <- NumEcx/DenE
  Encx <- NumEncx/DenE

  output <- BinMatEps(name_ID = prob.mat@name_ID, DMNB = prob.mat@DMNB, BMNB = prob.mat@BMNB,
                      Epsilon = list(Ecx = Ecx, Encx = Encx))

  return(output)
})
