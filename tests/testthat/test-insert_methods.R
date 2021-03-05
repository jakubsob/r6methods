test_that("no class", {
  content <- "

  "
  expect_error(insert_methods(content, 2))
})

test_that("basic class, standard indentation", {

  content <- "
code

Class <- R6::R6Class(
  \"Class\",
  list(
    age = NULL,
    name = NULL
  ),
  list(
    secret1 = NULL
  )
)

code"

  expect_equal(
    insert_methods(content, 4),
    "Class <- R6::R6Class(
  \"Class\",
  list(
    age = NULL,
    name = NULL,
    #' @description Setter for age
    set_age = function(age) {
      self$age <- age
    },
    #' @description Getter for age
    get_age = function() {
      self$age
    },
    #' @description Setter for name
    set_name = function(name) {
      self$name <- name
    },
    #' @description Getter for name
    get_name = function() {
      self$name
    },
    #' @description Setter for secret1
    set_secret1 = function(secret1) {
      self$secret1 <- secret1
    },
    #' @description Getter for secret1
    get_secret1 = function() {
      self$secret1
    }
  ),
  list(
    secret1 = NULL
  )
)"
  )
})

test_that("basic class, standard indentation, private list first", {

  content <- "Class <- R6::R6Class(
  \"Class\",
  private = list(
    secret1 = NULL
  ),
  list(
    age = NULL,
    name = NULL
  )
)"

  expect_equal(
    insert_methods(content, 2),
    "Class <- R6::R6Class(
  \"Class\",
  private = list(
    secret1 = NULL
  ),
  list(
    age = NULL,
    name = NULL,
    #' @description Setter for age
    set_age = function(age) {
      self$age <- age
    },
    #' @description Getter for age
    get_age = function() {
      self$age
    },
    #' @description Setter for name
    set_name = function(name) {
      self$name <- name
    },
    #' @description Getter for name
    get_name = function() {
      self$name
    },
    #' @description Setter for secret1
    set_secret1 = function(secret1) {
      self$secret1 <- secret1
    },
    #' @description Getter for secret1
    get_secret1 = function() {
      self$secret1
    }
  )
)"
  )
})

test_that("class with more arguments, extra newline indentation, private list first", {

  content <- "Class <- R6::R6Class(
  \"Class\",

  private = list(
    secret1 = NULL
  ),

  lock_objects = FALSE,

  list(
    age = NULL,
    name = NULL
  )
)"

  expect_equal(
    insert_methods(content, 2),
    "Class <- R6::R6Class(
  \"Class\",

  private = list(
    secret1 = NULL
  ),

  lock_objects = FALSE,

  list(
    age = NULL,
    name = NULL,
    #' @description Setter for age
    set_age = function(age) {
      self$age <- age
    },
    #' @description Getter for age
    get_age = function() {
      self$age
    },
    #' @description Setter for name
    set_name = function(name) {
      self$name <- name
    },
    #' @description Getter for name
    get_name = function() {
      self$name
    },
    #' @description Setter for secret1
    set_secret1 = function(secret1) {
      self$secret1 <- secret1
    },
    #' @description Getter for secret1
    get_secret1 = function() {
      self$secret1
    }
  )
)"
  )
})

test_that("class with more arguments, strange indentation, private list first", {

  content <- "Class <- R6::R6Class(\"Class\",
                                private = list(
                                  secret1 = NULL
                                ),

                                lock_objects = FALSE,

                                  list(
                                    age = NULL,
                                    name = NULL
                                  )
                              )"

  expect_equal(
    insert_methods(content, 2),
    "Class <- R6::R6Class(\"Class\",
                                private = list(
                                  secret1 = NULL
                                ),

                                lock_objects = FALSE,

                                  list(
                                    age = NULL,
                                    name = NULL,
                                    #' @description Setter for age
                                    set_age = function(age) {
                                      self$age <- age
                                    },
                                    #' @description Getter for age
                                    get_age = function() {
                                      self$age
                                    },
                                    #' @description Setter for name
                                    set_name = function(name) {
                                      self$name <- name
                                    },
                                    #' @description Getter for name
                                    get_name = function() {
                                      self$name
                                    },
                                    #' @description Setter for secret1
                                    set_secret1 = function(secret1) {
                                      self$secret1 <- secret1
                                    },
                                    #' @description Getter for secret1
                                    get_secret1 = function() {
                                      self$secret1
                                    }
                                  )
                              )"
  )
})

test_that("cursor above", {

  content <- "

  Class <- R6::R6Class(
  \"Class\",
  list(
    age = NULL,
    name = NULL
  ),
  list(
    secret1 = NULL
  )
)"

  expect_error(insert_methods(content, 1))
})

test_that("cursor below", {

  content <- "

  Class <- R6::R6Class(
  \"Class\",
  list(
    age = NULL,
    name = NULL
  ),
  list(
    secret1 = NULL
  )
)


"
  expect_error(insert_methods(content, 15))
})
