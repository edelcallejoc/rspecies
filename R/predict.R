

#' Predict function for Naive Bayes Spatial Model
#'
#' @description This function allows to calculate the classification of each cell in the
#'     GRID, based on the score function of the naive Bayes model.
#'     See \code{\link[rspeciesdev]{score}} function.
#'
#' @param object A \code{SpNaBaModel} object from
#'     \code{\link[rspeciesdev]{NBModel}} function.
#'
#' @param apr_inc logical. If TRUE the apriori score is included for
#'     prediction calculation.
#'
#' @param ... pass to other methods.
#'
#' @author Enrique Del Callejo-Canal (\email{edelcallejoc@@gmail.com}).
#'
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
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals)
#'
#' x.model <- NBModel(x.mat, target = 1:10, fac.lap = 0.01)
#'
#' x.prediction <- predict(x.model, apr_inc = FALSE)
#'
#' @name predict
#' @rdname predict
#' @exportMethod predict
#'
#' @include SpNaBaMatrix.R SpNaBaCounts.R SpNaBaProbs.R
#' @include SpNaBaEps.R SpNaBaScore.R SpNaBaModel.R NBModel.R
#'
#'

setGeneric("predict", function(object, apr_inc, ...) standardGeneric("predict"))

#' @rdname predict
#' @aliases predict,SpNaBa-methods,NBModel-methods
#' @usage NULL

setMethod("predict", c("SpNaBaModel", "logical"),
          function(object, apr_inc = FALSE, ...){

            # Argument validation -------------------------------------
            arg_val <- all(all(is.logical(apr_inc), length(apr_inc) == 1))
            if(!arg_val){
              stop("Arguments: apr_inc is not of the correct type.\n
                   see documentation of rspecies predict function.")
            }

            # Calculate prediccion ------------------------------------

            Scx <- get_Scx(object) # Score matrix
            BM <- get_BM(object)[, rownames(Scx)] # Binary matrix

            Scx_pred <- crossprod(t(BM), replace(Scx, is.na(Scx), 0))

            # Checking if apriori is required ------------------------

            if(apr_inc){
              apr <- get_Apriori(object)
              apr_mat <- matrix(apr, nrow(BM), length(apr), byrow = T)
              Scx_pred <- apr_mat + Scx_pred
            }

            # Return the object ---------------------------------------

            output <- SpNaBaPred(Prediction = Scx_pred)

            return(output)

            })




