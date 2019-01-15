
#' Spatial Naive Bayes Model methods
#'
#' @description \code{counts} is a function to extract frequency information
#'     for the Spatial Naive Bayes Model.
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @name counts
#' @rdname SpNaBa-methods
#' @usage NULL
#' @exportMethod counts
#'

setGeneric("counts", function(x.mat, target = NULL) standardGeneric("counts"))


#' @rdname SpNaBa-methods
#' @aliases counts,SpNaBa-methods
#'
#' @param x.mat A \code{\link[rspeciesdev]{SpNaBaMatrix}} object.
#'
#' @param target \code{character} or \code{numeric} or \code{logical} or NULL. Indicates the columns
#'     of binmatrix slot to be consired as target factors.
#'
#' @return \code{counts}: An object of class \code{\link[rspeciesdev]{SpNaBaCounts}}.
#'
#' @seealso \code{\link[rspeciesdev]{SpNaBaCounts}}, \code{SpNaBaMatrix}, \code{grd_build}, \code{id_pts}
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

setMethod("counts", c("SpNaBaMatrix","ANY"), function(x.mat, target = NULL){

  # Extract binmatrix

  BM <- get_BM(x.mat)

  # args verification ----------------------------------------

   if(missing(x.mat)){
     stop("Argument x.mat is required.")
   }

   if(missing(target) | is.null(target)){
     target <- NULL
   }else{
     names.mat <- colnames(BM)
     if(!is.vector(target)){
       stop("Argument target must be vector type.")
     }else{
       if(is.character(target)){
         cnames <- (target %in% names.mat)
         if(!all(cnames)){
           stop(paste(c(target[!cnames], "not found"), collapse = ", "))
         }
         target <- which(names.mat %in% unique(target))
       }
       if(is.numeric(target)){
         if(max(target) > ncol(BM)){
           stop("target contains values to subscript out of bounds")
         }
         target <- unique(target)
       }
       if(length(target) > ncol(BM)){
           stop("target: logical subscript too long.")
       }
     }
   }

  # Calculations --------------------------------------------

  if(is.null(target) | length(target) == ncol(BM)){
    output <- SpNaBaCounts(N = nrow(BM),
                           Nc = colSums(BM),
                           Nx = colSums(BM),
                           Ncx = t(BM) %*% BM)
  }else{
    output <- SpNaBaCounts(N = nrow(BM),
                           Nc = colSums(BM[,target, drop = FALSE]),
                           Nx = colSums(BM[,-target, drop = FALSE]),
                           Ncx = t(BM[,target, drop = FALSE]) %*% BM[,-target, drop = FALSE])
  }

  return(output)

}
)


#' @description \code{laplace} is a function to applied the Laplace smothing method
#'     to an object of class \code{SpNaBaCounts}.
#'
#' @name laplace
#' @rdname SpNaBa-methods
#' @usage NULL
#' @exportMethod laplace
#'

setGeneric("laplace", function(x.counts, fac.lap = NULL){
  standardGeneric("laplace")
})


#' @rdname SpNaBa-methods
#' @aliases laplace,SpNaBa-methods
#'
#' @param x.counts A \code{\link[rspeciesdev]{SpNaBaCounts}} object.
#'
#' @param fac.lap \code{numeric} or \code{NULL}. it indicates the laplace factor to be applied.
#'     If \code{NULL} or \code{missing} the laplace factor applied is 0.01.
#'
#' @return \code{laplace}: An object of class \code{\link[rspeciesdev]{SpNaBaCounts}}.
#'
#' @examples
#'
#' # Appliying the Laplace smothing ----------------------
#'
#' x.counts.ls <- laplace(x.counts, fac.lap = 0.1)
#'

setMethod("laplace", c("SpNaBaCounts","ANY"), function(x.counts, fac.lap = NULL){

  # args verification ----------------------------------------

  if(missing(x.counts)){
    stop("Argument x.counts is required.")
  }

  if(missing(fac.lap) | is.null(fac.lap)){
    fac.lap <- 0.01
  }else{
    if(!is.numeric(fac.lap)){
      stop("Argument fac.lap must be numeric type.")
    }
    if(fac.lap<0){
      stop("Argument fac.lap must be greater than 0.")
    }
  }

  # Calculations --------------------------------------------

  output <- SpNaBaCounts(N = get_N(x.counts) + 2*fac.lap,
                         Nc = get_Nc(x.counts) + fac.lap,
                         Nx = get_Nx(x.counts) + fac.lap,
                         Ncx = get_Ncx(x.counts) + fac.lap/2)

  return(output)

}
)





#' @description Take a \code{SpNaBaCounts} object from \code{\link[rspecies]{counts}}
#'     and calculates the probability matrices for spatial data mining.
#'     The probability matrices are for joint and conditional probabilities.
#'
#' @name probs
#' @rdname SpNaBa-methods
#' @usage NULL
#' @exportMethod probs
#'

setGeneric("probs", function(x.counts, fac.lap = NULL){
  standardGeneric("probs")
})

#' @rdname SpNaBa-methods
#' @aliases probs,SpNaBa-methods
#'
#' @return \code{probs}: An object of class \code{\link[rspeciesdev]{SpNaBaProbs}}.
#'
#' @examples
#'
#' # Probability matrices -----------------------------------
#' x.probs <- probs(x.counts, fac.lap = 0.1)
#'


setMethod("probs", c("SpNaBaCounts", "ANY"),
          function(x.counts, fac.lap = NULL){

            if(missing(fac.lap)){fac.lap <- 0.1}

            if(fac.lap > 0){
              x.counts <- laplace(x.counts = x.counts, fac.lap = fac.lap)
            }

            # Extracting Ns ----------------------------------

            N <- get_N(x.counts)
            Nc <- get_Nc(x.counts)
            Nx <- get_Nx(x.counts)
            Ncx <- get_Ncx(x.counts)
            Nncx <- (Nx - t(Ncx))

            # Creating a digonal matrix from Nx --------------

            ifelse(length(Nx)>1, diagNx <-solve(diag(as.vector(Nx))), diagNx <- 1/Nx)

            # Creating a digonal matrix from Nc and Nnc --------------

            if(length(Nc)>1){
              diagNc <-solve(diag(as.vector(Nc)))
              diagNnc <-solve(diag(as.vector(N-Nc)))
            }else{
              diagNc <- 1/Nc
              diagNnc <- 1/(N-Nc)
            }

            # Calculating probabilities ----------------------

            Pc <- (1/N) * Nc
            Px <- (1/N) * Nx

            Pc.x <- Ncx %*% diagNx
            rownames(Pc.x) <- names(Nc)
            colnames(Pc.x) <- names(Nx)

            Px.c <- t(Ncx) %*% diagNc
            rownames(Px.c) <- names(Nx)
            colnames(Px.c) <- names(Nc)

            Px.nc <- Nncx %*% diagNnc
            rownames(Px.nc) <- names(Nx)
            colnames(Px.nc) <- names(Nc)

            # Creating the object SpNaBaProbs ----------------

            output <- SpNaBaProbs(Pc = Pc, Px = Px, Pc.x = Pc.x, Px.c = Px.c, Px.nc = Px.nc)

            return(output)
          })

