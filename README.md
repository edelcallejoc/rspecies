rspecies package --------------------------------------------------
-------------------------------------------------------------------

A Spatial Data Mining Framework for Species Distribution Modelling

Before starting with rspecies -------------------------------------
-------------------------------------------------------------------

*rspecies* is a developing package oriented to the modeling of species
distribution. This is the first version that includes some of the
classes and methods that have been developed up to this moment.
Therefore, this package is not in CRAN yet.

This package was build in R version 3.4.3, (2017-11-30).

     ## Session info --------------------------------------------------------------------
     
    ## setting  value                       
    ## version  R version 3.4.3 (2017-11-30)
    ## system   x86_64, mingw32             
    ## ui       RStudio (1.1.383)           
    ## language (EN)                        
    ## collate  Spanish_Mexico.1252         
    ## tz       America/Mexico_City         
    ## date     2018-03-15                  

    ## Packages -------------------------------------------------------------------------
    
    ## package   * version date       source        
    ## base      * 3.4.3   2017-12-06 local         
    ## compiler    3.4.3   2017-12-06 local         
    ## datasets  * 3.4.3   2017-12-06 local         
    ## devtools    1.13.4  2017-11-09 CRAN (R 3.4.3)
    ## digest      0.6.13  2017-12-14 CRAN (R 3.4.3)
    ## graphics  * 3.4.3   2017-12-06 local         
    ## grDevices * 3.4.3   2017-12-06 local         
    ## memoise     1.1.0   2017-04-21 CRAN (R 3.4.3)
    ## methods   * 3.4.3   2017-12-06 local         
    ## stats     * 3.4.3   2017-12-06 local         
    ## tools       3.4.3   2017-12-06 local         
    ## utils     * 3.4.3   2017-12-06 local         
    ## withr       2.1.1   2017-12-19 CRAN (R 3.4.3)
    ## yaml        2.1.16  2017-12-12 CRAN (R 3.4.3)

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

The package is structured in three function without S4 generic and
methods declaration: *grd\_build()*, *raster\_breaks()* and
*get\_species()*. There are 6 S4 classes: *BinMat* (SuperClass),
*BinMatCount*, *BinMatProb*, *BinMatEps*, *BinMatScore* and
*BinMatPred*. Finally, 32 methods associated to this classes, you can
see help files for more details.

Vignettes
---------

The vignettes that you can explore are:

-   [grd\_build](https://github.com/edelcallejoc/rspecies/blob/master/vignettes/grd_build-vignette.md)
-   [get\_species\_](https://github.com/edelcallejoc/rspecies/blob/master/vignettes/get_species-vignette.Rmd)
