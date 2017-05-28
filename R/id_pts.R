

#' Identify points coordinates inside each cell of the grid.

#' @description Take a \code{SpatialPointsDataFrame} object and
#'     compare each points with the boundaries of each cell in a
#'     grid (\code{SpatialPolygonsDataFrame} from \code{grd_build}
#'     function.
#'
#' @param grd A \code{SpatialPolygonsDataFrame} object from
#'     \code{grd_build} function.
#' @param pts A \code{SpatialPointsDataFrame} object.
#' @param contain A \code{character} object \code{simple} or \code{properly}.
#'     By default \code{simple}, it implies a simple contain evaluation (see
#'     \code{\link[rgeos]{gContains}}). \code{properly} implies a properly
#'     contain evaluation (see \code{\link[rgeos]{gContainsProperly}}).
#'
#' @param colnames \code{NULL} or \code{character} vector. If \code{NULL}
#'     colnames for the resulting matrix are created as a sequence of the form
#'     F1, F2, \ldots. If \code{character} vector, colnames are
#'     used to name the columns of the resulting matrix.
#'
#' @return An object of class BinMat. See \code{\link[rspecies]{BinMat-class}}
#'     documentation.
#'
#' @details This function creates a binary matrix with N rows associated to the
#'     cells ID of \code{grd} object and M columns associated to the factors in
#'     the \code{pts} object. The element of the row i and column j is 1 if the
#'     cell i of the \code{grd} object contains at least one point of the factor
#'     j of the \code{pts} object and 0 otherwise.
#'
#'     The matrix is created using the functions \code{\link[rgeos]{gContains}}()
#'     or \code{\link[rgeos]{gContainsProperly}}() from \code{rgeos} package.
#'     \code{\link[base]{sapply}}() function is used to extract the cell's ids from
#'     the returned object of \code{\link[rgeos]{gContains}} or \code{gContainsProperly}.
#'
#' @seealso \code{\link[rspecies:plot-methods]{plot}}
#'
#' @examples
#'
#' library(sp)
#' library(rgeos)
#'
#' data(Mex0)
#' data(mammals)
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Identification points of mammals
#' system.time(x.mat<-id_pts(grd = Mex0.grd, pts = mammals))
#'
#' # Plot sample density - target default
#' plot(Mex0.grd, x.mat,  sou_den = TRUE, leaflet = TRUE)
#'
#' # Plot sample density - target "Linx rufus"
#' plot(Mex0.grd, x.mat, target="Lynx rufus", sou_den = TRUE, leaflet = TRUE)
#'
#' # Plot sample - target default
#' plot(Mex0.grd, x.mat,  sou_den = FALSE, leaflet = TRUE)
#'
#' # Plot sample - target "Linx rufus"
#' plot(Mex0.grd, x.mat, target="Lynx rufus", sou_den = FALSE, leaflet = TRUE)
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

id_pts<-function(grd, pts, contain = c("simple", "properly"), colnames = NULL){

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
  auxcolnames<-colnames
  if(is.null(colnames)){
      colnames <- paste("F",1:m.c,sep=".")
  }else{
    if(!is.character(colnames)){
      stop("Argument 'colnames' must be character type.")
    }else{
        colnames <- colnames
    }
  }

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
                  byid = T, returnDense=F), length))})
    }else{
      ide.mat<-foreach::`%dopar%`(foreach::foreach(i = 1:m.c, .combine = cbind,
          .multicombine = FALSE, .packages = c("rgeos", "sp")),{
          as.numeric(sapply(rgeos::gContainsProperly(spgeom1 = grd,
          spgeom2 = pts[pts@data$nameID ==pts.level[i], ],
          byid = T, returnDense=F), length))})
    }

  parallel::stopCluster(cl)

  ide.mat <- as.matrix(ide.mat)
  colnames(ide.mat) <- colnames
  rownames(ide.mat) <- rownames(grd@data)


      DataID <- data.frame(Name=pts.level, row.names=colnames,
                           stringsAsFactors = FALSE)

      output <- BinMat(name_ID = DataID, DMNB = ide.mat, BMNB = (ide.mat>0))


  return(output)
}



