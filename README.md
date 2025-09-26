
# lab4linreg

<!-- badges: start -->
[![R-CMD-check](https://github.com/njmurov-ux/lab3algorithms/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shahin1290/lab4linreg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of lab4linreg is to performs linear regression using QR decomposition.

## Installation

You can install the development version of lab4linreg from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("shahin1290/lab4linreg")
```

## Example

This is a basic example which shows you how to use the package based on iris 
data and all available functions:

``` r
library(lab4linreg)
linreg_mod <- linreg$new(formula = Petal.Length ~ Species, data=iris)
linreg_mod$plot()
linreg_mod$print()
linreg_mod$resid()
linreg_mod$pred()
linreg_mod$coef()
linreg_mod$summary()
```

