
#' Extract pixels from raster object and breaks it into n bins.

#' @description Take a \code{raster} object (\code{\link[raster]{RasterLayer}},
#'     \code{\link[raster]{RasterStack}}, \code{\link[raster]{RasterBrick}}),
#'     and extract pixels values contain by \code{\link[sp]{SpatialPolygonsDataFrame}}
#'     object. Then, \code{raster_break()} generates bins, given by \code{breaks}
#'     argument, from pixels values. Finally, it converts all bins into a
#'     \code{\link[sp]{SpatialPointsDataFrame}} object. See details for further
#'     explanation
#'
#' @param x.ras A \code{raster} object (\code{\link[raster]{RasterLayer}},
#'     \code{\link[raster]{RasterStack}}, \code{\link[raster]{RasterBrick}}).
#' @param pol.ext A \code{SpatialPolygonsDataFrame} object. An object from
#'     \code{\link[rspecies]{grd_build}()} is recommended.
#' @param bb A integer vector of the form \code{c(min['long'], max['long'],
#'     min['lat'], max['lat'])}. By default \code{bb = c(t(sp::bbox(po.ext)))}.
#' @param breaks \code{NULL}, integer or numeric vector. A vector of
#'     length >= 3, indicating the lower limit and upper limit of each bin.
#'     If breaks has length = 3, 2 bins are created. By default \code{breaks = NULL},
#'     if NULL or integer bins are calculated with \code{\link{quantile}()}.
#'     Se details for further explanation.
#' @param breaks.by double. A value between 0 and 1. Argument pass to quantile function, it requires
#'     \code{breaks = NULL}.
#'
#' @return A \code{SpatialPointsDataFrame} object. It includes all raster values and
#'     its coordinates, and the bin associated.
#'
#' @examples
#' library(sp)
#' library(rgeos)
#' library(raster)
#'
#' load('./data/Mex0.rda')
#'
#' # Generating de grid from Mex0 data
#' Mex0.grd<-grd_build(Mex0)
#'
#' # Extracting bioclim variables from worldclim
#' bioclim<-getData('worldclim', var='bio', res=2.5)
#'
#' # Extracting values from 19 bioclim variables
#' system.time(bio.sp<-raster_breaks(bioclim, Mex0.grd))
#' x11()
#' plot(Mex0.grd)
#' plot(bio.sp[which(bio.sp$nameID == 'bio1.p01'),], pch = 20,
#'     col = 'blue', add = TRUE)
#'
#' # Leaflet interaction
#'
#' library(leaflet)
#' leaflet() %>%
#'     addProviderTiles('OpenStreetMap.Mapnik',
#'         options = providerTileOptions(noWrap = TRUE))%>%
#'     addPolygons(data = Mex0.grd, stroke = TRUE, color = '#FFFFFF',
#'         layerId = Mex0.grd@data$ID, weight = 1, opacity = 0.3,
#'         fillColor = '#A9A9A9', fillOpacity = 0.6,
#'         popup = row.names(Mex0.grd@data)) %>%
#'     addCircleMarkers(data = bio.sp[which(bio.sp$nameID == 'bio1.p01'),],
#'         radius = 3,
#'         popup = paste(bio.sp[which(bio.sp$nameID == 'bio1.p01'),]$nameID,
#'                   bio.sp[which(bio.sp$nameID == 'bio1.p01'),]$val.p, sep = ' '))
#'
#'
#' # Extracting values from first bioclim variable
#' system.time(bio1.sp<-raster_breaks(bioclim$bio1, Mex0.grd))
#'
#' #' # Leaflet interaction
#'
#' library(leaflet)
#' library(RColorBrewer)
#'
#' # Not run, it can take time
#' pal <- colorFactor('RdYlBu', levels = rev(levels(as.factor(bio1.sp$nameID))))
#'
#' leaflet() %>%
#'     addProviderTiles('OpenStreetMap.Mapnik',
#'         options = providerTileOptions(noWrap = TRUE))%>%
#'     addPolygons(data = Mex0.grd, stroke = TRUE, color = '#FFFFFF',
#'         layerId = Mex0.grd@data$ID, weight = 1, opacity = 0.3,
#'         fillColor = '#A9A9A9', fillOpacity = 0.6,
#'         popup = row.names(Mex0.grd@data)) %>%
#'     addCircleMarkers(data = bio1.sp, radius = 1, color = ~pal(bio1.sp$nameID),
#'         popup = paste(bio1.sp$nameID, bio1.sp$val.p, sep = ' '))
#'
#' @details This function extract all pixels values and its coordinates from
#'     raster object. Valid raster objects are (\code{\link[raster]{RasterLayer}},
#'     \code{\link[raster]{RasterStack}}, \code{\link[raster]{RasterBrick}}).
#'     Then, divides values into bins given by \code{breaks} argument. Finally, it
#'     converts this data into a \code{\link[sp]{SpatialPointsDataFrame}}.
#'
#'     \code{bb} argument is used for corpping raster object from \code{pol.ext}'
#'     bounding box. After cropping, cropped raster object is masked into
#'     \code{pol.ext}' boundaries. Both step are important in order to reduce
#'     processing time.
#'
#'     Extraction of values and coordinates are done by \code{\link[raster]{values}} and
#'     \code{\link[sp]{coordinates}}. Both function are applied to the raster object.
#'     output is concatenated into \code{data.frame} object.
#'
#'     \code{breaks} argument can be pass as \code{NULL}, integer or double vector.
#'     If \code{breaks = NULL}, the bins are calculated trough
#'     \code{\link[base]{quantile}(x, probs, na.rm = TRUE)} function,
#'     where \code{x} argument is \code{values(rasterobj)},
#'     \code{probs = seq(0, 1, by = breaks.by)}. If \code{breaks = integer} >= 1, the
#'     bins are calculated in similar way as \code{breaks = NULL}, but
#'     \code{probs = seq(0, 1, length = (breaks +1))}. If \code{breaks = c()}, where
#'     a vector of the form \code{c(2.1, 2.5, 2.9)} produce two bins as \code{[2.1, 2.5)}
#'     and \code{[2.5, 2.9)}. Then the raster values are compared with the range of each bins.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}),
#'     based on implemented algortihms in web platform SPECIES (see References).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @export
#'
#' @import parallel doParallel foreach raster
#'
#' @importFrom stats quantile
#'
#'
#'
#'

########## Biobreaks modificado ########


raster_breaks <- function(x.ras, pol.ext, bb = c(t(sp::bbox(pol.ext))), breaks = NULL,
    breaks.by = 0.1) {
    clsarg1 <- class(x.ras)
    if (!any(clsarg1 %in% c("RasterLayer", "RasterStack", "RasterBrick"))) {
        stop("x.ras must be a raster class object. See raster package documentation.")
    }
    if (!any(class(pol.ext) %in% c("SpatialPolygonsDataFrame", "SpatialPointsDataFrame"))) {
        stop("pol.ext must be a SpatialPolygonsDataFrame or SpatialPointsDataFrame object.")
    }
    if (!is.numeric(bb) | length(bb) != 4) {
      stop("bb must be a numeric vector of length 4.")
    }
    if(!is.null(breaks)){
    if(length(breaks) == 1){
      if(!is.integer(breaks)){
        stop("breaks must be NULL or integer of length 1 or numeric vector of length greater than 3.")
      }
    }else{
      if(!is.numeric(breaks) | length(breaks)<3){
        stop("breaks must be NULL or integer of length 1 or numeric vector of length greater than 3.")
      }
    }}
    if(!is.double(breaks.by) | breaks.by <= 0 | breaks.by >= 1){
      stop("breaks.by must be a double type between 0 and 1.")
    }

    #### bounding box ######

    xras.bb <- raster::crop(x.ras, bb)  # extrayendo el raster del bbox
    xras.msk <- raster::mask(xras.bb, pol.ext)

    #### Extrayendo los valores de temperatura para el objeto espacial ####


    if (clsarg1 == "RasterLayer") {
        xras.ext <- aux_foo(xras.msk, breaks = breaks, breaks.by = breaks.by,
                            proj4string = sp::proj4string(pol.ext))
    } else {
        i<-NULL
        nlay <- raster::nlayers(xras.msk)
        cores <- parallel::detectCores() - 1
        cl <- parallel::makeCluster(cores)
        parallel::clusterExport(cl = cl, varlist = c("aux_foo"), envir = globalenv())
        doParallel::registerDoParallel(cl)
        xras.ext <- foreach::`%dopar%`(foreach::foreach(i = 1:nlay, .combine = list, .multicombine = TRUE,
            .packages = c("raster", "sp")), {
            do.call("aux_foo", list(xras.msk[[i]], breaks = breaks, breaks.by = breaks.by,
                proj4string = sp::proj4string(pol.ext)), envir = globalenv())
        })
        parallel::stopCluster(cl)

        xras.ext <- do.call("rbind", xras.ext)
    }



    return(xras.ext)
}

########## Función auxiliar #############

aux_foo <- function(x, breaks, breaks.by, proj4string = sp::proj4string(x)) {
    nms <- names(x)
    foo_data <- data.frame(sp::coordinates(x), raster::values(x),
                           stringsAsFactors = FALSE)
    names(foo_data) <- c("long", "lat", "val.bio")
    foo_data <- foo_data[!is.na(foo_data$val.bio), ]
    nr <- dim(foo_data)[1]
    foo_data$bio.var <- rep(nms, length = nr)
    foo_data$nameID <- character(nr)
    foo_data$val.p <- character(nr)

    ######## Calculando los cuantiles ###############

    if (is.null(breaks)) {
        breakst <- stats::quantile(foo_data$val.bio, probs = seq(0, 1, by = breaks.by), na.rm = TRUE)
    } else {
        if (length(breaks) == 1) {
            breakst <- stats::quantile(foo_data$val.bio, probs = seq(0, 1, length = (breaks +
                1)), na.rm = TRUE)
        } else {
            breakst <- breaks
        }
    }

    n_breaks <- length(breakst)
    i <- 2
    while (i <= n_breaks) {
        if (i <= 10) {
            nm_aux <- paste(nms, ".p0", (i - 1), sep = "")
        } else {
            nm_aux <- paste(nms, ".p", (i - 1), sep = "")
        }
        if (i == n_breaks) {
            aux0 <- which(foo_data$val.bio >= breakst[i - 1] & foo_data$val.bio <= breakst[i])
            aux1 <- paste("[", breakst[i - 1], ",", breakst[i], "]", sep = "")
        } else {
            aux0 <- which(foo_data$val.bio >= breakst[i - 1] & foo_data$val.bio < breakst[i])
            aux1 <- paste("[", breakst[i - 1], ",", breakst[i], ")", sep = "")
        }
        foo_data$nameID[aux0] <- nm_aux
        foo_data$val.p[aux0] <- aux1
        i <- i + 1
    }

    pts <- foo_data[, c("long", "lat")]
    dimnames(pts)[[1]] <- seq(1, nrow(pts))
    df <- foo_data[, c("val.bio", "bio.var", "nameID", "val.p")]
    rownames(df) <- seq(1, nrow(pts))
    foo_data <- sp::SpatialPointsDataFrame(pts, df, match.ID = TRUE, proj4string = asp::CRS(proj4string))


    return(foo_data)
}
