

#' An S4 method for SpatialPolygonsDataFrame and SpNaBaPred classes objects.
#'
#' @name plot_predict
#' @rdname plot-predict
#'
#' @description A plot methods for classes "SpatialPolygonsDataFrame" and "SpNaBaPred".
#'
#' @param map A \code{SpatialPolygonsDataFrame} object from
#'     \code{\link[rspeciesdev]{grd_build}} function.
#'
#' @param prediction A \code{\linkS4class{SpNaBaPred}} object from
#'     \code{\link[rspeciesdev]{predict}} function.
#'
#' @param target \code{logical}, \code{character}, \code{numeric} or \code{NULL}.
#'     A vector with names, position (integer or boolean) that especifies the
#'     columns of the target variables. If length = 1, the prediction map for the
#'     target is displayed. If length > 1, the prediction richness map of the targets
#'     is displayed.
#'
#' @param ... Parameters passed to other functions (See
#'     \code{\link[graphics]{par}}).
#'
#' @return a ggplot object
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @exportMethod plot_predict
#' @import ggplot2
#'
#' @include SpNaBaMatrix.R SpNaBaCounts.R SpNaBaProbs.R SpNaBaEps.R
#' @include SpNaBaScore.R SpNaBaModel.R NBModel.R SpNaBaPred.R predict.R
#'
#'
#'

setGeneric("plot_predict", function(map, prediction, target = NULL, ...) standardGeneric("plot_predict"))

#' @rdname plot-predict
#' @aliases plot_predict,SpNaBa-methods,NBModel-methods
#' @usage NULL
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
#' # Creando el modelo
#' x.model <- NBModel(x.mat, target = 1:10, fac.lap = 0.01)
#'
#' # Calculando la predicción
#' x.prediction <- predict(x.model, apr_inc = FALSE)
#'
#' # Plot of one target
#' plot_predict(Mex0.grd, x.prediction, target = 1)
#'
#' # plot prediction of richness
#' plot_predict(Mex0.grd, x.prediction, target = 1:10)
#'
#'
#' @importFrom RColorBrewer brewer.pal

setMethod("plot_predict", c("SpatialPolygonsDataFrame", "SpNaBaPred", "ANY"),
          function(map, prediction, target = NULL, ...) {

            # Argument validation -----------------------------------
            if(is.null(target)){
              taux <- 1
            }else{
              if(!is.numeric(target) & !is.character(target)){
                stop("target must be numeric or character type")
              }else{
                name_ID <- colnames(prediction@Prediction)
                if(any(name_ID %in% target)){
                  taux <- target
                }else{
                  taux <- name_ID[target]
                 }
              }
            }

            # Checking cell id match ------------------------------

            if(!all(rownames(map@data) %in% rownames(prediction@Prediction))){
              stop("map and prediction have no cells id matches.")
            }

            # Merge target and grid -------------------------------

            if(length(target) > 1){
              message(paste("'target' have length greater than 1.",
                             "The displayed map shows the total number",
                              "of positive prediction per row.", sep =" "))

              y_prediction <- ifelse(prediction@Prediction[,target] > 0, 1, 0)
              y_prediction <- data.frame(y = apply(y_prediction,1,sum))
              legend_title <- "Richnes"
              colours_fill <- brewer.pal(9,"Reds")
            }else{
              y_prediction <- prediction@Prediction[,target]
              legend_title <-  name_ID[target]
              colours_fill <- rev(brewer.pal(11,"RdYlBu"))
            }


            x.for <- merge(ggplot2::fortify(map), y_prediction,
                           by.x = "id", by.y = "row.names")

           # Creating plot ----------------------------------------

            pmap <- ggplot(x.for, aes_string(x = "long", y = "lat", group = "group")) +
              geom_polygon(aes_string(fill = "y"), color = "white", size = 0.05) +
              labs(fill = legend_title) +
              scale_fill_gradientn(colours = colours_fill)

            # Print graph -----------------------------------------

            print(pmap)

            # Return pmap as invisible object ---------------------

            return(invisible(pmap))


    })
