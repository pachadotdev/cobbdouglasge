
# cobbdouglasge

<!-- badges: start -->
<!-- badges: end -->

The goal of cobbdouglasge is to show how to plot the solutions of a 2x2 Cobb-Douglas general equilibrium model in R using Shiny.

## Prerequisites

Before running the app locally, make sure you have:

- R installed
- An editor such as RStudio or VS Code
- The `devtools` package installed in R

You can install `devtools` with:

```r
install.packages("devtools")
```

## Run locally

Clone the repository https://github.com/pachadotdev/cobbdouglasge and then run the following in the R console:

```r
devtools::install_deps()
```

Then load the package and start the app:

```r
devtools::load_all()
run_app()
```

The base for this demonstration was exercise 15.B.2 from MWG.

I hope this is useful!
