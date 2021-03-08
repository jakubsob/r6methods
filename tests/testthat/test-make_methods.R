test_that("no fields", {
  Test <- R6::R6Class("Test")
  expect_error(make_methods(Test, "all", "both", add_roxygen = FALSE))
})

Test <- R6::R6Class(
  "Test",
  public = list(
    x = NULL,
    y = NULL
  ),
  private = list(
    z = NULL
  ),
  lock_class = FALSE
)

test_that("setters for public fields", {
  expect_equal(
    as.character(make_methods(Test, "public", "set")),
    "#' @description Setter for x
set_x = function(x) {
  self$x <- x
},
#' @description Setter for y
set_y = function(y) {
  self$y <- y
}"
  )
})

test_that("setters for private fields", {
  expect_equal(
    as.character(make_methods(Test, "private", "set", add_roxygen = FALSE)),
    "set_z = function(z) {
  private$z <- z
}"
  )
})

test_that("getters for public fields", {
  expect_equal(
    as.character(make_methods(Test, "public", "get", add_roxygen = FALSE)),
    "get_x = function() {
  self$x
},
get_y = function() {
  self$y
}"
  )
})

test_that("getters for private fields", {
  expect_equal(
    as.character(make_methods(Test, "private", "get")),
    "#' @description Getter for z
get_z = function() {
  private$z
}"
  )
})

test_that("select explicit fields", {
  expect_equal(
    as.character(make_methods(Test, c("x", "z"), "both", FALSE)),
    "set_x = function(x) {
  self$x <- x
},
get_x = function() {
  self$x
},
set_z = function(z) {
  private$z <- z
},
get_z = function() {
  private$z
}"
  )
})

test_that("all methods", {
  expect_equal(
    as.character(make_methods(Test, "all", "both", add_roxygen = FALSE)),
    "set_x = function(x) {
  self$x <- x
},
get_x = function() {
  self$x
},
set_y = function(y) {
  self$y <- y
},
get_y = function() {
  self$y
},
set_z = function(z) {
  private$z <- z
},
get_z = function() {
  private$z
}"
  )
})

test_that("no duplicaton of methods", {
  Test <- R6::R6Class(
    "Test",
    public = list(
      x = NULL,
      y = NULL,
      get_z = function() {
        private$z
      },
      set_x = function(x) {
        self$x <- x
      }
    ),
    private = list(
      z = NULL
    )
  )

  expect_equal(
    as.character(make_methods(Test, "all", "both", FALSE)),
    "get_x = function() {
  self$x
},
set_y = function(y) {
  self$y <- y
},
get_y = function() {
  self$y
},
set_z = function(z) {
  private$z <- z
}"
  )
})
