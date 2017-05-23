
#' Get data from SPECIE framework
#'
#' @description a function to extract data from SPECIES WEB framework.
#'
#'
#' @param species a character vector. The name of the specie desired as
#'     web SPECIES platform shows.
#'
#' @param from The name of web platform to consult data.
#'
#' @param date logical. If TRUE the recolection date is added.
#'
#'
#' @export
#'
#' @import httr jsonlite
#'
get_species<-function(species = NULL, from = "SPECIES", date = TRUE){

  if(is.null(species)){stop("Species must be especified.")}
  if(!is.character(species)){stop("Species must be character type.")}

  if(from == "SPECIES"){
      id_list<-as.data.frame(httr::content(httr::POST("http://species.conabio.gob.mx/niche3/niche/especie",
                    body = list(qtype = "getEntList", searchStr = species,
                                nivel = "especievalidabusqueda", source = "1"),
                    encode = "json")))

      id_coords <- httr::content(httr::POST("http://species.conabio.gob.mx/niche3/niche/especie",
                     body = list(qtype = "getSpecies", id = as.character(id_list$data.spid[1]),
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

# debug(get_species)

# a <- get_species("Lynx rufus")
