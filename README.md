# **mobr_app** -- Measurement of Biodiversity across scales using the `mobr` R-package 

## Authors

*Caroline Oliver* - <olivercs@g.cofc.edu>
*Daniel McGlinn* - <mcglinndj@cofc.edu>


## Description

The mobr application, mobr_app, is an R Shiny development to help users access the functionality of the mobr R-package without having to directly employ the functions within an R console. This graphical user interface, or GUI, will allow its users to upload a community matrix and plot attribute data in the data tab. They will then be presented with information on:

* Rarefaction (Exploratory Analysis tab)

* Measurement of Biodiversity Metrics (MoB Metrics tab)

* Delta Statistics (Delta Stats tab)

The application also provides link tabs that connect the user to both the mobr Github homepage, as well as, the issues sections to report any issues or to ask any questions that may come up during a users time in the application.

## Run the app locally
The app can be run locally. Several R packages are required for locally hosting the app, these can be installed
from the R terminal:

```r
install.packages(c('shiny', 'shinydashboard', 'shinyjqui',
                   'shinycssloaders', 'devtools'))
devtools::install_github('mobiodiv/mobr')
```

Once those dependencies are installed you can run the app from the R terminal using:

```r
shiny::runGitHub("mobiodiv/mobr_app", ref="shiny")
```

## License

GNU GPL
