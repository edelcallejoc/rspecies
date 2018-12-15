

#' Identify points coordinates inside each cell of a grid.

#' @description This function takes a \code{SpatialPointsDataFrame}
#'     object and compares each point with the boundaries of each
#'     cell in a grid (\code{SpatialPolygonsDataFrame} from the
#'     \code{grd_build} function.
#'
#' @param grd A \code{SpatialPolygonsDataFrame} object from the
#'     \code{grd_build} function.
#' @param pts A \code{SpatialPointsDataFrame} object.
#' @param contain Character (\code{simple} or \code{properly}).
#'     By default \code{simple}, it implies a simple contention
#'     evaluation (see \code{\link[rgeos]{gContains}})).
#'     \code{properly} suggests a properly contention evaluation
#'     (see \code{\link[rgeos]{gContainsProperly}})).
#'
#' @return An object of class \code{SpatialPolygonsDataFrame}. The data slot
#'     contains a data.frame with a column "id" and m columns. The \eqn{m}
#'     columns represent the sample intensity in each cell of the grid for
#'     each factor.
#'
#' @details \code{id_pts()} extract the number of observations per cell
#'     for each factor. It uses parallel programing to reduce the computation
#'     time. This function depends on \code{parallel}, \code{doParallel} and
#'     \code{foreach} packages. By defaul the number of cores for parallel
#'     computing is two, but it is posible to extend the number of cores
#'     by modify the body of the funtion at line \code{cores <- 2}
#'
#' @examples
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
#' system.time(x.mat<-id_pts(grd = Mex0.grd, pts = mammals))
#'
#' # Loading extra libraries for ggplot2
#' library(plyr)
#' library(ggplot2)
#' library(mapproj)
#'
#' Mex0.grd_for <- fortify(Mex0.grd)
#' Mex0.grd_df <- plyr::join(Mex0.grd_for,
#'  data.frame(id = rownames(x.mat@freqmatrix ), x.mat@freqmatrix, stringsAsFactors = FALSE), by="id")
#'
#' ggplot(data = Mex0.grd_df, aes(x = long, y = lat, group = group, fill = Alouatta_palliata)) +
#'     geom_polygon(colour = "gray") +
#'     scale_fill_gradient(low = "white", high = "red") +
#'     coord_quickmap()
#'
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @docType methods
#' @export
#'
#' @import sp rgdal rgeos maptools parallel doParallel foreach

id_pts<-function(grd, pts, contain = c("simple", "properly")){

  #### Verificación de argumentos ####

  if(class(grd) != "SpatialPolygonsDataFrame"){
    stop("Argument 'grd' must be SpatialPolygonsDataFrame.")
  }
  if(class(pts) != "SpatialPointsDataFrame"){
    stop("Argument 'pts' must be SpatialPointsDataFrame.")
  }
  if(!is.character(contain)){
    stop("Argument 'contain' must be character type.")
  }
  if(length(contain)==2){contain<-"simple"}

  ####################################

  pts.level<-levels(as.factor(pts@data$nameID))
  i<-NULL
  m.c<-length(pts.level)

  cores <- 2 # parallel::detectCores() - 1
  cl <- parallel::makeCluster(cores)
  parallel::clusterExport(cl = cl, varlist = c("grd", "pts",
      "pts.level"), envir = environment())
  doParallel::registerDoParallel(cl)
  if(contain=="simple"){
      ide.mat<-foreach::`%dopar%`(foreach::foreach(i = 1:m.c, .combine = cbind,
          .multicombine = FALSE, .packages = c("rgeos", "sp")),{
              as.numeric(sapply(rgeos::gContains(spgeom1 = grd,
                  spgeom2 = pts[pts@data$nameID ==pts.level[i], ],
                  byid = TRUE, returnDense=FALSE), length))})
    }else{
      ide.mat<-foreach::`%dopar%`(foreach::foreach(i = 1:m.c, .combine = cbind,
          .multicombine = FALSE, .packages = c("rgeos", "sp")),{
          as.numeric(sapply(rgeos::gContainsProperly(spgeom1 = grd,
          spgeom2 = pts[pts@data$nameID ==pts.level[i], ],
          byid = TRUE, returnDense=FALSE), length))})
    }

  parallel::stopCluster(cl)

  ide.mat <- as.matrix(ide.mat)
  colnames(ide.mat) <- sapply(strsplit(pts.level, " "), paste, collapse = "_")
  rownames(ide.mat) <- rownames(grd@data)


  output <- SpNaBaMatrix(freqmatrix = ide.mat)

  return(output)
}



