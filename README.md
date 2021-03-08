
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r6methods

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/jakubsob/r6methods/workflows/R-CMD-check/badge.svg)](https://github.com/jakubsob/r6methods/actions)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

Generate boilerplate code for R6 classes. Given R6 class create getters
and/or setters for selected class fields or use RStudio addins to insert
methods straight into class definition.

## Installation

You can install development version from Github using:

``` r
remotes::install_github("jakubsob/r6methods")
```

## Basic usage

Core functionality comes with `make_methods` function.

``` r
library(r6methods)
library(R6)

Person <- R6Class(
  "Person", 
  public = list(
    name = NULL,
    age = NA,
    initialize = function(name, age = NA) {
      self$name <- name
      self$age <- age
    },
    print = function(...) {
      cat("Person: \n")
      cat("  Name: ", self$name, "\n", sep = "")
      cat("  Age:  ", self$age, "\n", sep = "")
      invisible(self)
    }
  ),
  private = list(
    secret1 = NULL,
    secret2 = NULL
  )
)
```

Create getters and setters for private fields:

``` r
make_methods(Person, "private", "both")
#> #' @description Setter for secret1
#> set_secret1 = function(secret1) {
#>   private$secret1 <- secret1
#> },
#> #' @description Getter for secret1
#> get_secret1 = function() {
#>   private$secret1
#> },
#> #' @description Setter for secret2
#> set_secret2 = function(secret2) {
#>   private$secret2 <- secret2
#> },
#> #' @description Getter for secret2
#> get_secret2 = function() {
#>   private$secret2
#> }
```

Or only getters:

``` r
make_methods(Person, "private", "get", add_roxygen = FALSE)
#> get_secret1 = function() {
#>   private$secret1
#> },
#> get_secret2 = function() {
#>   private$secret2
#> }
```

You can also create methods for fields of your liking, not only all of
private/public:

``` r
make_methods(Person, c("age", "secret1"), "get", add_roxygen = FALSE)
#> get_age = function() {
#>   self$age
#> },
#> get_secret1 = function() {
#>   private$secret1
#> }
```

## RStudio addins

Four addins are supplied with the package. They are grouped into 2
families:

  - Generate: makes method strings and prints them to console to be
    copied to class definition.
  - Insert: makes method strings and inserts them into class definition.

Addins with `gadget` suffix open gadget in RStudio which allows user to
have more control over the generated methods.
