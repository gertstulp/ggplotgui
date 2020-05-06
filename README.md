
<!-- README.md is generated from README.Rmd. Please edit that file -->
ggplotgui
=========

[![Travis-CI Build Status](https://travis-ci.org/gertstulp/ggplotgui.svg?branch=master)](https://travis-ci.org/gertstulp/ggplotgui) [![CRAN\_Release\_Badge](http://www.r-pkg.org/badges/version-ago/ggplotgui)](https://CRAN.R-project.org/package=ggplotgui) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/ggplotgui)](https://CRAN.R-project.org/package=ggplotgui)

Quick overview
==============

This package allows users to visualize their data using an online graphical user interface (GUI) that makes use of [R](https://www.r-project.org/)'s visualization package [ggplot](http://ggplot2.org/). There are two ways of using this functionality: 1) online, where users can upload their data and visualize it without needing R, by visiting this link: <https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/>; 2) from within the R-environment (by using the `ggplot_shiny()` function). In either case, R-code will be provided such that the user can recreate the graphs within the R-environment.

Background
==========

[R](https://www.r-project.org/) is amazing, but daunting for many. The programming style of R, compared to the point-and-click style of typical software, is a hurdle for many. Perhaps particularly so for those in the social sciences, whose statistical needs are often met by other software packages. Yet such packages are often very limited in terms of their options to visualize the data at hand. I believe that the amazing visualization-capabilities of R might be one way to get more people to use it. To lower the barrier to start using R, this package allows users to visualize their data using an online graphical user interface (GUI) that makes use of R's visualization package [ggplot](http://ggplot2.org/). There are two ways of using this functionality: 1) online, where users can upload their data and visualize it without needing R, by visiting this link: <https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/>; 2) from within the R-environment (by using the ggplot\_shiny() function). Importantly, the R-code will also be provided such that the user can recreate the graphs within the R-environment. The main aim (or hope) is to get more people using R and its wonderful (graphing) capabilities.

Installation
============

``` r
install.packages("ggplotgui")

# In order to install the most recent version of this package, you'll need to use the "devtools"-package
install.packages("devtools")
devtools::install_github("gertstulp/ggplotgui")

library("ggplotgui")
```

Usage
=====

There are two ways to use this functionality:

-   By calling it from within R

``` r
# You can call the function with and without passing a dataset
ggplot_shiny()
ggplot_shiny(mpg) # Passing ggplot's mpg dataset
```

-   By using the following link: <https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/>

This will open up the following screen: ![](man/figures/tab_data_upload.png) There are four ways in which you can use data within this GUI:
1. You can pass your dataframe through R, by using ggplot\_gui(my\_df)
2. You can examine sample data (data 'mpg' from the ggplot2 package)
3. You can upload your datafile in many different formats (e.g., csv, SPSS, Excel)
4. You can paste some data into a window (the data need to be "[tidy](http://tidyr.tidyverse.org/)")

A ggplot-graph can be made through point and click (after data is loaded and through the ggplot-tab): ![](man/figures/tab_ggplot.png) Aesthetics of the graph can be changed with the tabs on the right hand side.

An interactive version (through plotly) of the graph is displayed (the Plotly-tab) to allow for further exploration of the data: ![](man/figures/tab_plotly.png)

Importantly, the R-code to recreate the graphs will be provided (in the R-code tab): ![](man/figures/tab_R-code.png)

Current functionality
=====================

Currently, only widely used graphs are implemented: boxplots, density plots, dot + error plots, dotplots, histograms, scatter plots, and violin plots. Only a select number of aesthetic features can be changed. Please do report bugs and send feature requests.

Acknowledgements
================

I am grateful to the people who made [R](https://www.r-project.org/), and to [Hadley Wickham](http://hadley.nz/) for making such good packages (and open access books describing them), that allow even low-skilled and low-talented programmers like myself to be able to contribute to R. This package makes use of: [ggplot2](http://ggplot2.tidyverse.org/), [Shiny](http://shiny.rstudio.com/), [stringr](http://stringr.tidyverse.org/), [plotly](https://plot.ly/r/), [readr](http://readr.tidyverse.org/), [readxl](http://readxl.tidyverse.org/), [haven](http://haven.tidyverse.org/), and [RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf). Package development through [RStudio](https://www.rstudio.com/) and [Github](https://github.com/), and with the help of [R Markdown](http://rmarkdown.rstudio.com) and [devtools](https://www.rstudio.com/products/rpackages/devtools/). The code that allows for online data input was based somewhat on the [BoxPlotR Shiny app](https://github.com/VizWizard/BoxPlotR.shiny). Many thanks to Wilmer Joling who set up the magical "[docker](https://www.docker.com/)" and [website](https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/) where the online version of this packages runs.
