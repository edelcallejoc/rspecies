rspecies package --------------------------------------------------
-------------------------------------------------------------------

A Spatial Data Mining Framework for Species Distribution Modelling

Before starting with rspecies -------------------------------------
-------------------------------------------------------------------

*rspecies* is a developing package oriented to the modeling of species
distribution. This is the first version that includes some of the
classes and methods that have been developed up to this moment.
Therefore, this package is not in CRAN yet.

This package was build in R version 3.5.1 (2018-07-02).

     ## Session info --------------------------------------------------------------------
     
    ## setting  value                       
    ## version  R version 3.5.1 (2018-07-02)
    ## system   x86_64-w64-mingw32/x64 (64-bit)             
    ## ui       RStudio (1.1.442)           
    ## language (EN)                        
    ## collate  Spanish_Mexico.1252         
    ## tz       America/Mexico_City         
    ## date     2018-12-14                  

Intruction for updating R can be found in
[here](https://www.r-statistics.com/2013/03/updating-r-from-r-on-windows-using-the-installr-package/)

You can install the content of this package through the following code.
At this time, the information in this file is informative.

    ## Not run -----------------------------------------------------------------

    install.packages("devtools") # Installin devtools package from CRAN
    devtools::install_github("hadley/devtools") # Installing development version

    # Intalling rspecies from github
    devtools::install_github("edelcallejoc/rspecies")

    ## End not run -------------------------------------------------------------

Vignettes
---------

The vignettes that you can explore are:

-   [grd\_build](https://github.com/edelcallejoc/rspecies/blob/master/vignettes/grd_build-vignette.md)
-   [get\_species\_](https://github.com/edelcallejoc/rspecies/blob/master/vignettes/get_species-vignette.md)
