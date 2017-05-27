
#' Get data from SPECIE framework
#'
#' @description a function to extract data from SPECIES WEB framework.
#'
#'
#' @param species a character vector of length 1. The name of the specie desired as
#'     web SPECIES platform shows.
#'
#' @param from The name of web platform to consult data. At this point
#'     it only works for \url{http://species.conabio.gob.mx/geoportal_v0.1.html}.
#'
#' @param date logical. If TRUE the recolection date is added.
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
      id_list<-get_species_names(species)

      id_coords <- httr::content(httr::POST("http://species.conabio.gob.mx/niche3/niche/especie",
                     body = list(qtype = "getSpecies", id = as.character(id_list[,"id"]),
                                 sfecha = tolower(date)),
                     encode = "json"))

      coords <- as.data.frame(t(sapply(id_coords$data, function(x){jsonlite::fromJSON(x$json_geom)$coordinates})))
      colnames(coords) <- c("Long", "Lat")
      rec_date <- sapply(id_coords$data, function(x){x$fechacolecta})
      coords$date <- rec_date
      coords$name <- id_list$data.especievalidabusqueda[1]
  }

  return(coords)
 }


#' @rdname get_species
#'
#' @param genus a character vector of length 1. The name of the specie desired as
#'     web SPECIES platform shows.
#'
#' @note \code{get_species_names}. Only one genus per query is allowed. This restriction
#'     is for memory care purposes. You can use the 'apply' function family to extend
#'     your search, but it is recommended to do the search one by one to not exceed
#'     the memory limits.
#'
#' @examples
#'
#' # get_species_names() examples
#'
#' # Initial capital letter
#' get_species_names(genus = "Aedes")
#'
#' # lowercase is allowed
#' get_species_names(genus = "aedes")
#'
#'
#' @export

get_species_names<-function(genus = NULL){

  # args validation -----------------------------------

  if(is.null(genus)){stop("gender must be especified.")}
  if(!is.character(genus)){stop("gender must be character type.")}
  if(length(genus)!=1){stop("Only one genus per query is allowed.")}

  # ---------------------------------------------------


  id_list <- httr::content(httr::POST("http://species.conabio.gob.mx/niche3/niche/especie",
                                   body = list(qtype = "getEntList", searchStr = genus,
                                               nivel = "especievalidabusqueda", source = "1"),
                                   encode = "json"))
  if(length(id_list$data)>0){
    output <- t(sapply(id_list$data, as.data.frame, stringsAsFactors = FALSE))
    colnames(output) <-c("id", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
                         "Specie")
  }else{
      stop(paste("Could not find the name ", genus, ".", sep = ""))
    }


  return(output)

}


