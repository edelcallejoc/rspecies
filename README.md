rspecies package --------------------------------------------------
-------------------------------------------------------------------

A Spatial Data Mining Framework for Species Distribution Modelling

Before starting with rspecies -------------------------------------
-------------------------------------------------------------------

*rspecies* is a developing package oriented to the modeling of species
distribution. This is the first version that includes some of the
classes and methods that have been developed up to this moment.
Therefore, this package is not in CRAN yet.

This package was build in R version 3.4.0, (2017-04-21).

    ## R version 3.4.0 (2017-04-21)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 15063)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Spanish_Mexico.1252  LC_CTYPE=Spanish_Mexico.1252   
    ## [3] LC_MONETARY=Spanish_Mexico.1252 LC_NUMERIC=C                   
    ## [5] LC_TIME=Spanish_Mexico.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] compiler_3.4.0  backports_1.0.5 magrittr_1.5    rprojroot_1.2  
    ##  [5] tools_3.4.0     htmltools_0.3.6 yaml_2.1.14     Rcpp_0.12.10   
    ##  [9] stringi_1.1.5   rmarkdown_1.5   knitr_1.15.1    stringr_1.2.0  
    ## [13] digest_0.6.12   evaluate_0.10

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
