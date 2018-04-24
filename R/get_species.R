
#' Get data from SPECIES framework
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
    if(date){
    rec_date <- sapply(id_coords$data, function(x){x$fechacolecta})
    coords$date <- rec_date
    }
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
#' @param sublevel character or NULL. A vector of length 1. The name of taxonomic
#'     rank sublevel to search names. See examples.
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
#' # Cheking names of a level
#' # Cheking class' names of Craniata phylum
#' get_species_names(level = "phylum", name = "Craniata",
#'  sublevel = "class", id = FALSE)
#'
#' # Cheking class' names of Animalia kingdom
#' get_species_names(level = "kingdom", name = "Animalia",
#'  sublevel = "class", id = FALSE)
#'
#'
#' @export


get_species_names<-function(level = "genus", name = NULL, sublevel, id = TRUE){
  
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
  
  level_which <- (level_aux$level %in% level)
  
  if(all(!level_which)){stop(paste0("level not found. Posible values are: ",
                                    paste0(level_aux$level, collapse = ", ")))}
  
  level_order <- which(level_which)
  
  # check sublevel arg ---------------------------------
  if(level_order == nrow(level_aux)){
    sublevel <- level_aux$qlevel[level_order]
  }else{
    if(missing(sublevel)){
      sublevel <- level_aux$qlevel[level_order+1]
    }else{
      if(!is.character(sublevel)){stop("sublevel must be character type.")}
      
      sublevel_which <- (level_aux$level %in% sublevel)
      
      if(all(!sublevel_which)){stop(paste0("sublevel not found. Posible values are: ",
                                           paste0(level_aux$level[level_order:7], collapse = ", ")))}
      
      sublevel <- level_aux$qlevel[which(sublevel_which)]
    }
  }
  
  # ---------------------------------------------------
  
  level_name <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                               "getEntList", sep = "/"),
                                         body = list(searchStr = name,
                                                     nivel = level_aux$qlevel[level_order],
                                                     source = "0",
                                                     limit = "true"),
                                         encode = "json"))$data
  
  if(length(level_name) == 0){
    stop(paste("Could not find the name ", name, ".", sep = ""))
  }else{
    level_name <- level_name[[1]][[level_aux$qlevel[level_order]]]
  }
  
  
  id_list <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                            "getVariables", sep = "/"),
                                      body = list(parentitem = level_name,
                                                  field = sublevel,
                                                  parentfield = level_aux$qlevel[level_order]),
                                      encode = "json"))
  
  if(length(id_list$data)>0){
    output <- data.frame(name = do.call(rbind,sapply(id_list$data, as.data.frame,
                                                     stringsAsFactors = FALSE, simplify = FALSE))$name,
                         stringsAsFactors = FALSE)
    
    if(id){
      
      aux_list <- lapply(output$name, function(x){
        aux_out <- httr::content(
          httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                           "getEntList", sep = "/"),
                     body = list(searchStr = x,
                                 nivel = sublevel,
                                 source = "1"),
                     encode = "json"))$data
      })
      
      aux_list <- lapply(aux_list, function(x){
        aux_f <- lapply(x, function(y){
          output <- as.data.frame(y, stringsAsFactors = FALSE)})
        out_dt <- do.call(rbind, aux_f)
        return(out_dt)})
      
      output <- do.call(rbind, aux_list)
      
      colnames(output) <-c("id", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
                           "Specie", "occ")
      #output <- output[!output[,"Specie"] == "",]
    }
    
  }else{
    stop(paste("Could not find the name ", name, ".", sep = ""))
  }
  
  
  return(as.data.frame(output))
  
}


#' @rdname get_species_
#'
#' @param grid_res The resolution of the grid in km. The resolution at this momment are 8, 16, 32, and 64 km.
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
#' system.time(a32 <- get_species_grid(grid_res = 32))
#'
#'
#' @export


get_species_grid<-function(grid_res = 16){
  
  # args validation -----------------------------------
  
  if(sum(c(8,16,32,64) %in% grid_res) == 0){
    stop("Allow resolution is 8, 16, 32 and 64 km.")
  }
  
  
  map_data <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                             "getGridGeoJson", sep = "/"),
                                       body = list(grid_res = as.character(grid_res)),
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


#' @rdname get_species_
#'
#' @param target a character vector of length 1. The name of the specie desired as
#'     web SPECIES platform shows.
#'
#' @param start_level a character vector of length 1. The name of the taxonomic rank.
#'     The allowed values are: "kingdom", "phylum", "class", "order", "family",
#'     "genus" and "specie".
#'
#' @param value a character vector of length 1. The taxonomic name associated to
#'     the taxonomic rank (level argument) as web SPECIES platform shows.
#'
#' @param validation logical. If TRUE validation process is applied to the model.
#'
#' @param fossil logical. If TRUE fossil data is included in the model.
#'
#' @param no_date logical. If TRUE data without recolection date is included.
#'
#' @param min_occ numeric. The minimum number of cells occurrences (nj).
#'
#' @param bioclim logical. If TRUE bioclim data is included in the model.
#'
#' @return a data.frame object.
#'
#' @examples
#'
#' Model1 <- get_species_georel(target = "Aedes aegypti",
#'     start_level = "class", value = "aves", min_occ = 5)
#'
#' head(Model1)
#'
#' @export


get_species_georel<-function(target, start_level, value, grid_res = 16,
                             validation = FALSE, fossil = TRUE, no_date = TRUE,
                             min_occ = 1, bioclim = FALSE){
  
  ## Args validation
  
  if(missing(target)){
    stop("target must be declared.")
  }else{
    if(!is.character(target)){stop("name must be character type.")}
    if(length(target)!=1){stop("name must be of length 1")}
  }
  if(missing(start_level)){
    stop("start_level must be declared. Possible values are: class, order, family and genus")
  }else{
    if(!is.character(start_level)){stop("start_level must be character type.")}
    if(length(start_level)!=1){stop("start_level must be of length 1")}
    if(all(!c("class", "order", "family", "genus") %in% start_level)){
      stop("Possible values for start_level argument are: class, order, family and genus")}
    if(missing(value)){
      stop("value argument must be declared.")
    }else{
      if(!is.character(value)){stop("value must be character type.")}
      if(length(value)!=1){stop("level must be of length 1")}
    }
  }
  
  log_args <- c(validation = validation, fossil = fossil, no_date = no_date, bioclim = bioclim)
  if(any(!is.logical(log_args))){stop(paste("Arguments:", paste(names(log_args), collapse = " "), "should be logical type"))}
  
  num_args <- c(grid_res = grid_res, min_occ = min_occ)
  if(any(!is.numeric(num_args))){stop(paste("Arguments:", paste(names(num_args), collapse = " "), "should be numeric type"))}
  if(all(!num_args[1] %in% c(8,16,32,64))){stop(paste("Argument grid_res: Allow resolution is 8, 16, 32 and 64 km."))}
  if(num_args[2] <= 0){stop(paste("Argument min_occ should be a value greater than 0"))}
  
  ## --------------------------------------
  
  # ---------------------------------------------------
  level_aux<-data.frame(level = c("kingdom", "phylum", "class", "order", "family",
                                  "genus", "specie"),
                        qlevel = c("reinovalido", "phylumdivisionvalido", "clasevalida",
                                   "ordenvalido", "familiavalida", "generovalido",
                                   "especievalidabusqueda"),
                        stringsAsFactors = FALSE)
  
  api <- "http://species.conabio.gob.mx/api/niche"
  
  id_target<-get_species_names(level = "specie", name = target)$id
  
  q_list <- list(id = as.character(id_target),
                 min_occ = as.character(min_occ),
                 fossil = tolower(fossil),
                 sfecha = tolower(no_date),
                 val_process = tolower(validation),
                 idtabla = "no_table",
                 grid_res = as.character(grid_res),
                 hasBios = "true",
                 hasRaster = tolower(bioclim))
  
  level_order <- which(level_aux$level %in% start_level)
  
  
  value_name <- httr::content(httr::POST(paste("http://species.conabio.gob.mx/api/niche/especie",
                                               "getEntList", sep = "/"),
                                         body = list(searchStr = value,
                                                     nivel = level_aux$qlevel[level_order],
                                                     source = "0",
                                                     limit = "true"),
                                         encode = "json"))$data
  
  if(length(value_name) == 0){
    stop(paste("Could not find the value ", value, ".", sep = ""))
  }else{
    value_name <- value_name[[1]][[level_aux$qlevel[level_order]]]
  }
  
  q_list <- c(q_list, list(
    "tfilters[0][field]" = level_aux$qlevel[level_order],
    "tfilters[0][value]" = value_name,
    "tfilters[0][type]" = "4")
  )
  
  georel_post <- httr::POST(url = paste(api, "/getGeoRel",sep = ""),
                            query = q_list,
                            encode = "json")
  
  georel_content <- httr::content(georel_post)$data
  
  georel_dt <- as.data.frame(do.call(rbind, georel_content), stringsAsFactors = FALSE)
  
  georel_dt <- data.frame(lapply(georel_dt, unlist), stringsAsFactors = FALSE)
  
  georel_dt <- as.data.frame(lapply(georel_dt, function(x){
    aux <- suppressWarnings(as.numeric(x))
    
    if(all(is.na(aux))){
      output <- x
    }else{
      output <- aux
    }
    
    return(output)
  }), stringsAsFactors = FALSE)
  
  colnames(georel_dt) <- c("spid", "Specie", "nij", "nj", "ni", "n", level_aux$level[1:5],
                           "Epsilon", "Score")
  
  georel_dt <- georel_dt[, c("spid", "Specie", "nij", "nj", "ni", "n", "Epsilon", "Score",
                             level_aux$level[1:5])]
  
  return(georel_dt)
}
