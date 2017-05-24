

#' An S4 method for SpatialPolygonsDataFrame and BinMat classes objects.
#'
#' @name plot-methods
#'
#' @description A plot methods for classes "SpatialPolygonsDataFrame", "BinMat".
#'
#' @param x A \code{SpatialPolygonsDataFrame} object from
#'     \code{\link[rspecies]{grd_build}} function.
#'
#' @param y A \code{\linkS4class{BinMat}} object from
#'     \code{\link[rspecies]{id_pts}} function or \code{\linkS4class{BinMatPred}}
#'     object from \code{\link[rspecies]{predict}} function .
#'
#' @param target \code{logical}, \code{character}, \code{numeric} or \code{NULL}.
#'     A vector with names, position (integer or boolean) that especifies the
#'     columns of slots DMNB or BMNB from \code{\linkS4class{BinMat}}
#'     that should be considered as target.
#'
#' @param sou.den logical. If \code{TRUE} the color palette is based on DMNB
#'     slot from \code{\linkS4class{BinMat}} object. If \code{FALSE} the color
#'     palette is based in BMNB slot from \code{\linkS4class{BinMat}} object.
#'
#' @param leaflet logical. If \code{TRUE} the function uses the leaflet package
#'     and methods. If \code{FALSE} the plotting method from sp package are used.
#'
#' @param ... Parameters passed to other functions (See
#'     \code{\link[graphics]{par}}).
#'
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
#' # plot - BinMat Class
#' plot(Mex0.grd, x.mat, leaflet = TRUE)
#'
#' @import leaflet RColorBrewer
#'
#'

NULL

# Plot method for "SpatialPolygonsDataFrame" and "BinMat" objects ------------------

#' @rdname plot-methods
#' @name plot-BinMat
#' @aliases plot-BinMat
#' @docType methods
#' @export


setMethod("plot", c("SpatialPolygonsDataFrame", "BinMat"),
          function(x, y, target = NULL, sou_den = TRUE, leaflet = FALSE, ...) {

            # Argument validation -----------------------------------
            if(is.null(target)){
              target <- 1
            }else{
              if(length(target)!= 1){
                stop("target must have length equal to one")
              }else{
                if(!is.numeric(target) & !is.character(target)){
                  stop("target must be numeric or character type")
                }else{
                  name_ID <- getName_ID(y)
                  if(any(rownames(name_ID) %in% target)){
                    target <- target
                  }else{
                    target <- rownames(name_ID)[(name_ID$Name %in% target)]
                  }
                }
              }
            }

            if(!is.logical(sou_den)){
              stop("sou.den must be logical type")
            }

            #---------------------------------------------------------
            if(leaflet){

              if(sou_den){
                pal <- colorNumeric("Reds", domain = range(y@DMNB[,target]))
                col_souden <- ~pal(y@DMNB[,target])
                col_fac <- rev(levels(as.factor(pal(y@DMNB[,target]))))

                leaflet() %>%
                  addProviderTiles('OpenStreetMap.Mapnik',
                                   options = providerTileOptions(noWrap = TRUE)) %>%
                  addPolygons(data=x, stroke=TRUE, color = col_souden,
                              layerId = x@data$ID, weight = 1, opacity = 0.6,
                              fillColor = col_souden, fillOpacity = 0.6,
                              popup = row.names(x@data)) %>%
                  addLegend("bottomleft", colors = col_fac,
                            labels = levels(cut(range(y@DMNB[,target]),
                                                length(col_fac),
                                                dig.lab = 1, right = FALSE)),
                            opacity = 1, title = y@name_ID[target,])
              }else{
                pal <- colorBin(c("white","red"), domain = range(y@BMNB[,target]))
                col_souden <- ~pal(as.numeric(y@BMNB[,target]))
                col_fac <- rev(levels(as.factor(pal(as.numeric(y@BMNB[,target])))))

                leaflet() %>%
                  addProviderTiles('OpenStreetMap.Mapnik',
                                   options = providerTileOptions(noWrap = TRUE)) %>%
                  addPolygons(data=x, stroke=TRUE, color = col_souden,
                              layerId = x@data$ID, weight = 1, opacity = 0.6,
                              fillColor = col_souden, fillOpacity = 0.6,
                              popup = row.names(x@data)) %>%
                  addLegend("bottomleft", colors = col_fac,
                            labels = c("Abscence", "Prescence"),
                            opacity = 1, title = y@name_ID[target,])
              }


            }else{
              if(sou_den){
                range_souden <- range(y@DMNB[,target])
                diff_range_souden <- diff(range_souden)
                if(diff_range_souden<10){
                  col_souden <- rev(heat.colors(diff_range_souden+1,
                                                alpha = 0.8))[cut(y@DMNB[,target], diff_range_souden+1)]
                }else{
                  col_souden <- rev(heat.colors(10,
                                                alpha = 0.8))[cut(y@DMNB[,target], 10)]
                }
              }else{
                col_souden <- rev(heat.colors(2,
                                              alpha = 0.8))[cut(as.numeric(y@BMNB[,target]), 2)]
              }
              plot(x, col = col_souden, ...)
            }
          }
)


#' @rdname plot-methods
#' @name plot-BinMatPred
#' @aliases plot-BinMatPred
#'
#' @description A plot methods for classes "SpatialPolygonsDataFrame", "BinMatPred".
#'
#'
#' @examples
#' # Counting matrices
#' count.mat<-counts(x.mat,  target = c("F.169", "F.272"))
#'
#' # Probability matrices
#' prob.mat<-probs(count.mat, lap_fac = 0.1)
#'
#' # score function
#' score.mat<-score(prob.mat, count.mat)
#'
#' # Prediction
#' pred.mat<-predict(score.mat, apr_inc = FALSE, comp_inc = FALSE)
#'
#' # plot - BinMatPred class
#' plot(Mex0.grd, pred.mat, target = "Lynx rufus", leaflet = T)
#'
#' @docType methods
#' @export

setMethod("plot", c("SpatialPolygonsDataFrame", "BinMatPred"),
          function(x, y, target = NULL, leaflet = FALSE, ...) {

            # Argument validation -----------------------------------
            if(is.null(target)){
              if(dim(y@Prediction)[1] == 1){
                target <- colnames(y@Prediction)
              }else{
                target <- colnames(y@Prediction)[1]
              }
            }else{
              if(length(target)!= 1){
                stop("target must have length equal to one")
              }else{
                if(!is.numeric(target) & !is.character(target)){
                  stop("target must be numeric or character type")
                }else{
                  name_ID <- getName_ID(y)
                  if(any(rownames(name_ID) %in% target)){
                    target <- target
                  }else{
                    target <- rownames(name_ID)[(name_ID$Name %in% target)]
                  }
                }
              }
            }

            if(!is.logical(leaflet)){
              stop("leaflet must be logical type")
            }

            #---------------------------------------------------------

            var_tar <- y@Prediction[,target]
            ran_vt <- range(var_tar)

            if(leaflet){

                dom <- c(-max(abs(ran_vt)), max(abs(ran_vt)))

                pal <- colorNumeric(rev(colorRampPalette(brewer.pal(9,"RdBu"))(100)),
                                    domain = dom, na.color = "#FFFFFF" )
                col_tar <- ~pal(var_tar)
                col_fac <- levels(as.factor(pal(var_tar)))

                leaflet() %>%
                  addProviderTiles('OpenStreetMap.Mapnik',
                                   options = providerTileOptions(noWrap = TRUE)) %>%
                  addPolygons(data=x, stroke=TRUE, color = col_tar,
                              layerId = x@data$ID, weight = 1, opacity = 0.8,
                              fillColor = col_tar, fillOpacity = 0.9,
                              popup = paste("Score: ", var_tar)) %>%
                  addLegend("bottomleft", pal = pal, values = var_tar,
                            labFormat = labelFormat(prefix = '', suffix = '',
                            between = ', ', digits = 1), bins = 7, opacity = 1,
                            title = paste("Score:",y@name_ID[target,], sep=" "))

            }else{
                  col_souden <- rev(heat.colors(20,
                                                alpha = 0.8))[cut(var_tar, 20)]
              plot(x, col = col_souden, ...)
            }
          }
)

