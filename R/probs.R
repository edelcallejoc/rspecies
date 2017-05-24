
#' Probability matrices for spatial data mining.
#'
#'
#' @description Take a \code{list} object from \code{\link[rspecies]{counts}}
#'     and calculates the probability matrices for spatial data mining.
#'     The probability matrices are for joint and conditional probabilities.
#'
#' @name probs
#'
#' @param count.mat A BinMatCount object. See documentation
#'     \code{\link[rspecies]{BinMatCount}}.
#' @param lap_fac numeric. See \code{\link[rspecies]{laplace}}.
#'
#' @return This function returns a \code{BinMatEps} object.
#'
#' @details The elements of the list are defined as follow.
#'
#'     \strong{Pc} a numeric vector. The probability of success of the target.
#'     \strong{Px} a numeric vector. The probability of success of the explanatory
#'     variables. \strong{Pcx} a numeric matrix. The conditional
#'     probabilies \eqn{P(C|X)}. Rows correspond target C and columns correspond
#'     the explanatory variables X. \strong{Pcnx}a numeric matrix. The conditional
#'     probabilies \eqn{P(C|X')}, where \eqn{X'} is the complement of X.
#'     Rows correspond target C and columns correspond  the explanatory variables X'.
#'     \strong{Pxc} a numeric matrix. The conditional probabilies \eqn{P(X|C)}. Rows
#'     correspond  the explanatory variables X and columns correspond target C.
#'     \strong{Pxnc} a numeric matrix. The conditional probabilies \eqn{P(X|C')}, where
#'     C' is the complement of C. Rows correspond  the explanatory variables X and
#'     columns correspond C'. \strong{Pnxc} a numeric matrix. The conditional probabilies
#'     \eqn{P(X'|C)}. Rows correspond  the explanatory variables X' and columns correspond
#'     C'. \strong{Pnxnc} a numeric matrix. The conditional probabilies \eqn{P(X'|C')}.
#'     Rows correspond  the explanatory variables X' and columns correspond C'.
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
#' # Probability matrices
#' system.time(prob.mat<-probs(count.mat, lap_fac = 0.1))

NULL

# Generic definition ------------------------------------------------------

setGeneric("probs",function(count.mat, lap_fac = 0.1){standardGeneric ("probs")})

#' @rdname probs
#' @name probs
#' @aliases probs-BinMatCount
#'
#' @docType methods
#' @export

setMethod("probs", c("BinMatCount", "ANY"),
           function(count.mat, lap_fac = 0.1){
  if(lap_fac != 0){
    count.mat <- laplace(count.mat = count.mat, lap_fac = lap_fac)
  }

  N <- getN(count.mat)
  Nc <- getNc(count.mat)
  Nnc <- N-Nc
  Nx <- getNx(count.mat)
  Nnx <- N-Nx
  Ncx <- getNcx(count.mat)
  Ncnx <- matrix(Nc,nrow(Nc),nrow(Nx))-Ncx
  Nncx <- matrix(Nx,nrow(Nnc),nrow(Nx), byrow = T)-Ncx
  Nncnx <- matrix(N,nrow(Nnc),nrow(Nnx))-(matrix(Nc,nrow(Nc),nrow(Nx))+matrix(Nx,nrow(Nc),nrow(Nx), byrow = T)-Ncx)

  ifelse(dim(Nx)[1]>1, diagNx <-solve(diag(as.vector(Nx))), diagNx <- 1/Nx)
  ifelse(dim(Nnx)[1]>1, diagNnx <- solve(diag(as.vector(Nnx))), diagNnx <- 1/Nnx)
  ifelse(dim(Nc)[1]>1, diagNc <- solve(diag(as.vector(Nc))), diagNc <- 1/Nc)
  ifelse(dim(Nnc)[1]>1, diagNnc <- solve(diag(as.vector(Nnc))), diagNnc <- 1/Nnc)


  Pc <- (1/N) * Nc
  Px <- (1/N) * Nx
  Pcx <- Ncx %*% diagNx
  rownames(Pcx) <- rownames(Nc)
  colnames(Pcx) <- rownames(Nx)
  Pcnx <- Ncnx %*% diagNnx
  colnames(Pcnx) <- rownames(Nnx)
  Pxc <- t(Ncx) %*% diagNc
  colnames(Pxc) <- rownames(Nc)
  Pxnc <- t(Nncx) %*% diagNnc
  colnames(Pxnc) <- rownames(Nc)
  Pnxc <- t(Ncnx) %*% diagNc
  colnames(Pnxc) <- rownames(Nc)
  Pnxnc <- t(Nncnx) %*% diagNnc
  colnames(Pnxnc) <- rownames(Nc)

  Prob <- list(Pc = Pc, Px = Px,
               Pcx = Pcx, Pcnx = Pcnx,
               Pxc = Pxc, Pxnc = Pxnc,
               Pnxc = Pnxc, Pnxnc = Pnxnc)

  output <- BinMatProb(name_ID = getName_ID(count.mat), DMNB = getDMNB(count.mat),
                       BMNB = getBMNB(count.mat), Prob = Prob)

  return(output)
})


