## ----setup, echo = FALSE, include = FALSE--------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#"
)

## ----getnames1, message=FALSE, results = 'markup'------------------------
library(rspecies)

aedes <- get_species_names(level = "genus", name = "Aedes", id = FALSE)

head(aedes, n=5)


## ----getnames2, message=FALSE, results = 'markup'------------------------
mammal <- get_species_names(level = "class", name = "Mammalia", sublevel = "specie", id = FALSE)

head(mammal, n=5)


## ----getcoords, message=FALSE, results = 'markup'------------------------
LR_coords <- get_species_coords(species = "Lynx rufus")

head(LR_coords, n=5)


## ----getcoords2, message=FALSE, results = 'markup'-----------------------
LR_coords_ndate <- get_species_coords(species = "Lynx rufus", date = FALSE)

head(LR_coords_ndate, n=5)


## ----getgrid, message=FALSE, results = 'markup', fig.align='left', fig.width=5, fig.show='asis'----
Mex_grd32 <- get_species_grid(grid_res = 32)

# using ggplot
library(ggplot2)

ggplot(fortify(Mex_grd32), aes(x = long, y = lat, group = id)) +
  geom_polygon() +
  geom_path(color="white") +
  coord_map()


## ----getgeorel, message=FALSE, results = 'markup'------------------------

# Lynx rufus model with all Mammals
LR_Model <- get_species_georel(target = "Lynx rufus",
                               start_level = "class",
                               value = "Mammalia",
                               validation = FALSE,
                               fossil = FALSE,
                               no_date = TRUE,
                               min_occ = 5,
                               bioclim = FALSE)

head(LR_Model, n = 5)
  

