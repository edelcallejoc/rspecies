[SPECIES web platform](http://species.conabio.gob.mx) is an interactive tool for the analysis of ecological niches and forecast species potential distribution, as well as to build Complex Inference Networks to identify potential species interactions. SPECIES development is supported by [C3-Centro de Ciencias de la Complejidad](http://www.c3.unam.mx/), Universidad Nacional Autónoma de México and the Comisión Nacional para el Conocimiento y Uso de la Biodiversidad ([CONABIO](https://www.gob.mx/conabio)).

This tutorial is a basic intriduction to use the r functions that connects to SPECIES api. This functions are contained in the developing package `rspecies`. `rspecies` is allocated in github public repositorie <https://github.com/edelcallejoc/rspecies>.

Vignette Info
-------------

The information contained in this vignette is corresponds to the usages of r functions to interact with SPECIES api. There are four functions that allows to extract information about SPECIES database's names. Each function can be used for many porpuses from consulting species names to adjust models in several ways.

Consulting taxon names from SPECIES framework (get\_species\_names)
-------------------------------------------------------------------

The simplest way to use **get\_species\_names** is for cheking species names available for a specific taxon rank. For example, the names of the species of `Aedes` genus.

``` r
library(rspecies)

aedes <- get_species_names(level = "genus", name = "Aedes", id = FALSE)

head(aedes, n=5)
#                    name
# 1         Aedes aegypti
# 2      Aedes albopictus
# 3      Aedes allotecnon
# 4 Aedes angustivittatus
# 5  Aedes atactavittatus
```

A more complex example is to extract species' name from a higher taxon level. For example, The names of the species of `Mammalia` class.

``` r
mammal <- get_species_names(level = "class", name = "Mammalia", sublevel = "specie", id = FALSE)

head(mammal, n=5)
#                         name
# 1          Alouatta palliata
# 2           Alouatta villosa
# 3  Ammospermophilus harrisii
# 4 Ammospermophilus interpres
# 5  Ammospermophilus leucurus
```

It is important to declared `id = FALSE` in order to avoid extensive computation. The `id` argument is intended to be used for individual queries by species.

Extracting species' coordinates from SPECIES framework (get\_species\_coords)
-----------------------------------------------------------------------------

The function **get\_species\_coords** is made to extract the geographical coordinates of observations by species from the API of the SPECIES framework. The simplest way to use this function is to indicate the `species` argument with the name of a particular species, eg *Lynx rufus*.

``` r
LR_coords <- get_species_coords(species = "Lynx rufus")

head(LR_coords, n=5)
#        Long      Lat    date       name
# 1 -100.0494 25.78639 1967-01 Lynx rufus
# 2 -100.0669 21.50289 2008-10 Lynx rufus
# 3 -100.0669 21.50289 2008-12 Lynx rufus
# 4 -100.0669 21.50289 2008-12 Lynx rufus
# 5 -100.0669 21.50289 2008-12 Lynx rufus
```

The `date` argument is used to add collection date information, by default this argument is defined as TRUE. While the argument `from` is still in development and at this moment it has no use.

``` r
LR_coords_ndate <- get_species_coords(species = "Lynx rufus", date = FALSE)

head(LR_coords_ndate, n=5)
#        Long      Lat    date       name
# 1 -100.0494 25.78639 1967-01 Lynx rufus
# 2 -100.0669 21.50289 2008-10 Lynx rufus
# 3 -100.0669 21.50289 2008-12 Lynx rufus
# 4 -100.0669 21.50289 2008-12 Lynx rufus
# 5 -100.0669 21.50289 2008-12 Lynx rufus
```

Downloading the map grid from the SPECIES framework (get\_species\_grid)
------------------------------------------------------------------------

The download of the grid is done with the function **get\_species\_grid**, which takes as argument the resolution of the grid, (`grid_res`). There are 4 possible values for the argument, 8,16,32 and 64.

``` r
Mex_grd32 <- get_species_grid(grid_res = 32)

# using ggplot
library(ggplot2)

ggplot(fortify(Mex_grd32), aes(x = long, y = lat, group = id)) +
  geom_polygon() +
  geom_path(color="white") +
  coord_map()
```

<img src="C:\Users\enriq\AppData\Local\Temp\RtmpeKisiY\preview-98c31d0505f.dir\get_species-vignette_files/figure-markdown_github/getgrid-1.png" style="display: block; margin: auto auto auto 0;" />

Download model results from the SPECIES platform. (get\_species\_georel)
------------------------------------------------------------------------

The **get\_species\_georel** function is used to download the results of the model set in the SPECIES platform. For the moment the function is useful to extract the results of the model for a single species. However, it is possible to use the `apply` family of functions to extract results from different models (do this operation with care since it can take a lot of time).

``` r

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
#    spid                   Specie nij  nj  ni     n Epsilon Score  kingdom
# 1 27317            Canis latrans 106 400 238 26944   54.75  3.70 Animalia
# 2 27321 Urocyon cinereoargenteus  85 535 238 26944   37.09  3.05 Animalia
# 3 27359            Taxidea taxus  32  87 238 26944   35.79  4.18 Animalia
# 4 27634       Lepus californicus  64 383 238 26944   33.10  3.11 Animalia
# 5 27769   Peromyscus maniculatus  99 871 238 26944   33.06  2.67 Animalia
#     phylum    class      order     family
# 1 Craniata Mammalia  Carnivora    Canidae
# 2 Craniata Mammalia  Carnivora    Canidae
# 3 Craniata Mammalia  Carnivora Mustelidae
# 4 Craniata Mammalia Lagomorpha  Leporidae
# 5 Craniata Mammalia   Rodentia Cricetidae
```
