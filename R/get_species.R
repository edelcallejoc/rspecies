
#' Get data from SPECIE framework
#'
#' @description a function to extract data from SPECIES WEB framework.
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
#' @author Enrique Del Callejo Canal (\email{edelcallejoc@@gmail.com}).
#'
#' @note \code{get_species}. Only one species per query is allowed. This restriction
#'     is for memory care purposes. You can use the 'apply' function family to extend
#'     your search, but it is recommended to do the search one by one to not exceed
#'     the memory limits.
#'
#' @examples
#'
#' library(httr)
#' library(jsonlite)
#'
#' # get_species() examples
#'
#' # Lynx rufus
#' LR<-get_species(species = "Lynx rufus")
#' LR
#'
#' # Aedes aegypti
#' AA<-get_species(species = "Aedes aegypti")
#' AA
#'
#' @name get_species
#'
#' @export
#'
#' @import httr jsonlite
#'


get_species<-function(species = NULL, from = "SPECIES", date = TRUE){

  if(is.null(species)){stop("species must be especified.")}
  if(!is.character(species)){stop("species must be character type.")}
  if(length(species)!=1){stop("Only one species per query is allowed.")}
  if(length(strsplit(species,' ')[[1]])!=2){
    stop("species must be 2 word character string to match properly species' names.")
  }

  if(from == "SPECIES"){
      id_list<-get_species_names(level = "specie", name = species)

      id_coords <- httr::content(httr::POST("http://species.conabio.gob.mx/api/niche/especie",
                     body = list(qtype = "getSpecies", id = as.character(id_list$id),
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


#' @rdname get_species
#'
#' @param level a character vector of length 1. The name of the taxonomic rank.
#'     The allowed values are: "kingdom", "phylum", "class", "order", "family",
#'     "genus" and "specie". By default "genus".
#'
#' @param name a character vector of length 1. The name of the specie desired as
#'     web SPECIES platform shows.
#'
#' @param limit a integer vector of length 1. The first "limit" elements to get from
#'     web SPECIES data base. By default is \code{NULL} and extract all data that
#'     corresponds to the name of the taxonomic rank.
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

get_species_names<-function(level = "genus", name = NULL, limit = NULL){

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
                                   "especievalidabusqueda"))


  if(!is.null(limit)){
    if(!is.numeric(limit)){stop("limit must be numeric")}

    id_list <- httr::content(httr::POST("http://species.conabio.gob.mx/api/niche/especie",
                                        body = list(qtype = "getEntList", searchStr = name,
                                                    nivel = level_aux$qlevel[level_aux$level == level],
                                                    source = "1", limit = limit),
                                        encode = "json"))
  }else{
  id_list <- httr::content(httr::POST("http://species.conabio.gob.mx/api/niche/especie",
                                   body = list(qtype = "getEntList", searchStr = name,
                                               nivel = level_aux$qlevel[level_aux$level == level],
                                               source = "1"),
                                   encode = "json"))}
  if(length(id_list$data)>0){
    output <- t(sapply(id_list$data, as.data.frame, stringsAsFactors = FALSE))
    colnames(output) <-c("id", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
                         "Specie")
    output <- output[!output[,"Specie"] == "",]
  }else{
      stop(paste("Could not find the name ", name, ".", sep = ""))
    }


  return(as.data.frame(output))

}


