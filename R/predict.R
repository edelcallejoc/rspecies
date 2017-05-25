
#' Function to calculate predictions form BinMatScore Class
#'
#' @description This function allows to calculate the classifier of each cell in the
#'     grid of the polygon defined for spatial data mining.
#'
#' @param object A \code{\link[rspecies]{BinMatScore}} object from
#'     \code{\link[rspecies]{score}} function.
#'
#' @param apr_inc logical. If TRUE the apriori score is included for
#'     prediction calculation.
#'
#' @param comp_inc logical. If TRUE the complement of explanatory variables
#'     included for prediction claculation
#'
#'
#'
#' @author Enrique Del Callejo-Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @import leaflet RColorBrewer
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
#' x.mat<-id_pts(grd = Mex0.grd, pts = mammals, colnames = NULL)
#'
#' # Counting matrices
#' count.mat<-counts(x.mat,  target = c("F.169", "F.272"))
#'
#' # Probability matrices
#' prob.mat<-probs(count.mat, lap_fac = 0.1)
#'
#' #score function
#' score.mat<-score(prob.mat, count.mat)
#'
#' # Prediction
#' pred.mat<-predict(score.mat, apr_inc = FALSE, comp_inc = FALSE)
#'
#' # See map with leaflet - "Lynx rufus"
#'
#' plot(Mex0.grd, pred.mat, target = "Lynx rufus", leaflet = T)
#'
#' # See data with DT package
#' library(DT)
#' datatable(data.frame(pred.mat@Prediction[,head(1:ncol(pred.mat@Prediction))]),
#'     colnames = pred.mat@name_ID[colnames(pred.mat@Prediction),],
#'     caption = "laplace factor: 0.1")%>%
#'     formatRound(1:6,digits = 3)
#'
#' @name predict-methods
#' @rdname predict
#' @aliases predict,BinMatScore
#' @exportMethod predict
#'
# Method definition  --------------------------------------------------

setMethod("predict", c("BinMatScore"),
                    function(object, apr_inc = TRUE, comp_inc = TRUE, ...){

         # Argument validation ---------------------------------------------
         arg_val <- all(all(is.logical(apr_inc), length(apr_inc) == 1),
                         all(is.logical(comp_inc), length(comp_inc) == 1))
         if(!arg_val){
           stop("Arguments: apr_inc and/or comp_inc, are/is not of the correct type.\n
                see documentation of rspecies predict function.")
         }

         # -----------------------------------------------------------------

         Scx <- getScx(object)
         BMNB <- getBMNB(object)[,rownames(Scx)]
         s1 <- crossprod(t(BMNB), replace(Scx, is.na(Scx), 0))

         if(apr_inc){
           apr <- getApr(object)
           apr_mat <- matrix(apr, nrow(BMNB), nrow(apr), byrow = T)
         } else{
           apr_mat <- 0
         }
         if(comp_inc){
           Scnx <- getScnx(object)
           s2 <- crossprod(t(1-BMNB), replace(Scnx, is.na(Scnx), 0))
         } else{
           s2 <- 0
         }


         Scell <- apr_mat+s1+s2
         colnames(Scell) <- colnames(Scx)
         output<-BinMatPred(name_ID = object@name_ID, DMNB = object@DMNB,
                     BMNB = object@BMNB, Prediction = Scell)

         return(output)
})

