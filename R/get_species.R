
#' Get data from SPECIE framework
#'
#' @description a family functions to extract data from SPECIES WEB framework.
#'
#'
#' @param species a character vector of length 1. The name of the specie desired as
#'     web SPECIES platform shows.
#'
#' @param from The name of web platform to consult data. At this point
#'     it only works for \url{http://species.conabio.gob.mx}.
#'
#' @param date logical. If TRUE the collection date is added.
#'
#' @return \code{get_species_coords()} returns a \code{data.frame} object.
#'
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @references \url{http://species.conabio.gob.mx/}
#'
#' @note \code{get_species_coords}. Only one species per query is allowed. This restriction
#'     is for memory care purposes. You can use the 'apply' function family to extend
#'     your search, but it is recommended to do the search one by one to not exceed
#'     the memory limits.
#'
#' @examples
#'
#' library(httr)
#' library(jsonlite)
#'
#' # get_species_coords() examples
#'
#' # Lynx rufus
#' LR<-get_species_coords(species = "Lynx rufus")
#' LR
#'
#' # Aedes aegypti
#' AA<-get_species_coords(species = "Aedes aegypti")
#' AA
#'
#' @name get_species_
#'
#' @export
#'
#' @import httr jsonlite




get_species_coords<-function(species = NULL, from = "SPECIES", date = TRUE){
  
  if(is.null(species)){stop("species must be especified.")}
  if(!is.character(species)){stop("species must be character type.")}
  if(length(species)!=1){stop("Only one species per query is allowed.")}
  if(length(strsplit(species,' ')[[1]])!=2){
    stop("species must be 2 word character string to match properly species' names.")
  }
  
  if(from == "SPECIES"){
    id_list<-get_species_names(level = "specie", name = species)
    
    id_coords <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                                "getSpecies", sep = "/"),
                                          body = list(id = as.character(id_list$id),
                                                      sfecha = tolower(date)),
                                          encode = "json"))
    
    coords <- as.data.frame(t(sapply(id_coords$data, function(x){jsonlite::fromJSON(x$json_geom)$coordinates})))
    colnames(coords) <- c("Long", "Lat")
    rec_date <- sapply(id_coords$data, function(x){x$fechacolecta})
    coords$date <- rec_date
    coords$name <- id_list$Specie
  }
  
  return(coords)
}


#' @rdname get_species_
#'
#' @param level a character vector of length 1. The name of the taxonomic rank.
#'     The allowed values are: "kingdom", "phylum", "class", "order", "family",
#'     "genus" and "specie". By default "genus".
#'
#' @param name a character vector of length 1. The name of the specie desired as
#'     web SPECIES platform shows.
#'
#' @param id Logical. If \code{TRUE}, id metada is included in the output. If
#'     \code{FALSE} only the names of species are display.
#'
#' @return \code{get_species_names()} returns a \code{data.frame} object.
#'
#'
#' @note \code{get_species_names}. The data can be very large so proceed with carefull.
#'
#' @examples
#'
#' # get_species_names() examples
#'
#' # Initial capital letter
#' get_species_names(name = "Aedes")
#'
#' # lowercase is allowed
#' get_species_names(name = "aedes")
#'
#'
#' @export


get_species_names<-function(level = "genus", name = NULL, id = TRUE){
  
  # args validation -----------------------------------
  
  if(is.null(level)){stop("level must be especified.")}
  if(!is.character(level)){stop("level must be character type.")}
  if(length(level)!=1){stop("level must be of length 1")}
  
  if(is.null(name)){stop("name must be especified.")}
  if(!is.character(name)){stop("name must be character type.")}
  if(length(name)!=1){stop("name must be of length 1")}
  
  # ---------------------------------------------------
  level_aux<-data.frame(level = c("kingdom", "phylum", "class", "order", "family",
                                  "genus", "specie"),
                        qlevel = c("reinovalido", "phylumdivisionvalido", "clasevalida",
                                   "ordenvalido", "familiavalida", "generovalido",
                                   "especievalidabusqueda"),
                        stringsAsFactors = FALSE)
  
  level_name <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                               "getEntList", sep = "/"),
                                         body = list(searchStr = name,
                                                     nivel = level_aux$qlevel[level_aux$level == level],
                                                     source = "0",
                                                     limit = "true"),
                                         encode = "json"))$data
  
  if(length(level_name) == 0){
    stop(paste("Could not find the name ", name, ".", sep = ""))
  }else{
    level_name <- level_name[[1]][[level_aux$qlevel[level_aux$level == level]]]
  }
  
  
  id_list <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                            "getVariables", sep = "/"),
                                      body = list(parentitem = level_name,
                                                  field = "especievalidabusqueda",
                                                  parentfield = level_aux$qlevel[level_aux$level == level]),
                                      encode = "json"))
  
  if(length(id_list$data)>0){
    output <- data.frame(name = do.call(rbind,sapply(id_list$data, as.data.frame,
                                                     stringsAsFactors = FALSE, simplify = FALSE))$name,
                         stringsAsFactors = FALSE)
    
    if(id){
      output <- do.call(rbind, sapply(output$name, function(x){
        aux_out <- as.data.frame(httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                                                "getEntList", sep = "/"),
                                                          body = list(searchStr = x,
                                                                      nivel = "especievalidabusqueda",
                                                                      source = "1"),
                                                          encode = "json"))$data, stringsAsFactors = FALSE)
      }, simplify = FALSE))
      
      colnames(output) <-c("id", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
                           "Specie", "occ")
      output <- output[!output[,"Specie"] == "",]}
    
  }else{
    stop(paste("Could not find the name ", name, ".", sep = ""))
  }
  
  
  return(as.data.frame(output))
  
}




#' @rdname get_species_
#'
#' @param resolution The resolution of the grid in km. The resolution at this momment are 8, 16, 32, and 64 km.
#'
#' @return \code{get_species_grid()} returns a \code{SpatialPolygonsDataFrame} object. It includes all the polygons
#'     that form the grid. Data slot contains the identifiers for each cells.
#'
#' @examples
#'
#' # get_species_grid() examples
#'
#' # By default resolution is 16 km.
#' system.time(a16 <- get_species_grid())
#'
#' system.time(a32 <- get_species_grid(resolution = 32))
#'
#'
#' @export


get_species_grid<-function(resolution = 16){
  
  # args validation -----------------------------------
  
  if(sum(c(8,16,32,64) %in% resolution) == 0){
    stop("Allow resolution is 8, 16, 32 and 64 km.")
  }
  
  
  map_data <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                             "getGridGeoJson", sep = "/"),
                                       body = list(grid_res = as.character(resolution)),
                                       encode = "json"))
  
  map_grd <- do.call(rbind,lapply(map_data$features,function(x){
    
    aux0 <- matrix(unlist(x$geometry$coordinates), ncol =2, byrow = TRUE,
                   dimnames = list(c(), c("long", "lat")))
    aux1 <- rep(x$properties$gridid, each = 5)
    
    output <- data.frame(aux0, gridid = aux1, stringsAsFactors = FALSE)
    return(output)
  }))
  
  grd_SPDF <- split(map_grd, map_grd$gridid)
  grd_SPDF <- lapply(grd_SPDF, function(x) { x["gridid"] <- NULL; x })
  
  
  grd_SPDF <- lapply(grd_SPDF, sp::Polygon)
  
  grd_SPDF <- lapply(seq_along(grd_SPDF), function(i) sp::Polygons(list(grd_SPDF[[i]]),
                                                                   ID = paste("g",unique(map_grd$gridid)[i], sep ="")))
  
  grd_SPDF <- sp::SpatialPolygons(grd_SPDF)
  
  grd_SPDF <- sp::SpatialPolygonsDataFrame(grd_SPDF, data =  data.frame(ID = paste("g",unique(map_grd$gridid), sep =""),
                                                                        row.names = paste("g",unique(map_grd$gridid), sep =""),
                                                                        stringsAsFactors = FALSE), match.ID = TRUE)
  
  return(grd_SPDF)
  
}


