---
title: "README"
author: "Enrique Del Callejo Canal"
date: "23 de mayo de 2017"
output: html_document
---



## rspecies package --------------------------------------------------
A Spatial Data Mining Framework for Species Distribution
Modelling

## Before starting with rspecies -------------------------------------

*rspecies* is a developing package oriented to the modeling of species distribution. This is the first version that includes some of the classes and methods that have been developed up to this moment. Therefore, this package is not in CRAN yet.

You can install the content of this package through the following code.
At this time, the information in this file is informative.


```r
## Not run -----------------------------------------------------------------

install.packages("devtools") # Installin devtools package from CRAN
devtools::install_github("hadley/devtools") # Installing development version

# Intalling rspecies from github
devtools::install_github("edelcallejoc/rspecies")

## End not run -------------------------------------------------------------
```

The package is structured in three function without S4 generic and methods declaration: *grd_build()*, *raster_breaks()* and *get_species()*. There are 6 S4 classes: *BinMat* (SuperClass), *BinMatCount*, *BinMatProb*, *BinMatEps*, *BinMatScore* and *BinMatPred*. Finally, 32 methods associated to this classes, you can see help files for more details.

## Vignettes

The vignettes that you can explore are:

* [grd_build](https://github.com/edelcallejoc/rspecies/blob/master/vignettes/grd_build-vignette.md)



