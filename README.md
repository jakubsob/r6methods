
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
and/or setters for selected class fields.

## Installation

``` r
remotes::install_github("jakubsob/r6methods")
```

## Examples

Given an example R6 class we can easily create methods to put into class
definition.

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
#>   self$secret1 <- secret1
#> },
#> #' @description Getter for secret1 
#> get_secret1 = function() {
#>   self$secret1
#> },
#> #' @description Setter for secret2 
#> set_secret2 = function(secret2) {
#>   self$secret2 <- secret2
#> },
#> #' @description Getter for secret2 
#> get_secret2 = function() {
#>   self$secret2
#> }
```

Or we make only getters:

``` r
make_methods(Person, "private", "get", add_roxygen = FALSE)
#> get_secret1 = function() {
#>   self$secret1
#> },
#> get_secret2 = function() {
#>   self$secret2
#> }
```

You can also create methods for fields of your liking, not only all of
private/public:

``` r
make_methods(Person, "age", "both")
#> #' @description Setter for age 
#> set_age = function(age) {
#>   self$age <- age
#> },
#> #' @description Getter for age 
#> get_age = function() {
#>   self$age
#> }
```

Two addins are also supplied:

-   Generate R6 methods: creates both getters and setters for all fields
-   Generate R6 methods gadget: gives you control over which methods are
    generated

To use the addin you have to select whole class definition.
