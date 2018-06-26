# **mobr_app** -- Measurement of Biodiversity across scales using the `mobr` R-package 

## Authors

*Caroline Oliver* - <olivercs@g.cofc.edu>
*Daniel McGlinn* - <mcglinndj@cofc.edu>


## Description

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
shiny::runGitHub("mobiodiv/mobsim_app", ref="shiny")
```

## License

GNU GPL
