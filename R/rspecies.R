
#' rspecies: A Spatial Data Mining Framework for Species Distribution
#'     Modelling
#'
#' An implementation of Spatial Data Mining for Species Distribution
#' Modelling. It combines Spatial Data Mining Philosophy and Bayesian Statistic
#' Theory for handling large volumes of spatial data. It also implements
#' Generalized Naive Bayes algorithms in order to extract useful knowledge from
#' occurrence spatial information. The main goal is to use rspecies with presence/
#' absence and environmental data for species distribution modelling. However, It
#' can use for other modelling purposes.
#'
#' @section rspecies functions:
#'     \code{\link[rspecies]{grd_build}()}
#'     \code{\link[rspecies]{raster_breaks}()}
#'     \code{\link[rspecies]{get_species}()}
#'
#' @section rspecies S4 classes:
#'     \code{\link[rspecies]{BinMat}}
#'     \code{\link[rspecies]{BinMatCount}}
#'     \code{\link[rspecies]{BinMatProb}}
#'     \code{\link[rspecies]{BinMatEps}}
#'     \code{\link[rspecies]{BinMatScore}}
#'     \code{\link[rspecies]{BinMatPred}}
#'
#' @section rspecies S4 mehtods:
#'     \code{\link[rspecies]{id_pts}}
#'     \code{\link[rspecies]{counts}}
#'     \code{\link[rspecies]{probs}}
#'     \code{\link[rspecies]{epsilon}}
#'     \code{\link[rspecies]{score}}
#'     \code{\link[rspecies]{predict}}
#'     \code{\link[rspecies]{plot}}
#'     \code{\link[rspecies]{print}}
#'
#' @docType package
#' @name rspecies
#'
#' @import methods grDevices
#' @importFrom utils head
#'
#'
NULL
