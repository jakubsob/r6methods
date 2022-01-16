#' ReactiveR6
#'
#' @description This class allows you to make your R6 class reactive by inheriting from \code{ReactiveR6}
#' By calling \code{private$invalidate()} in a method you can invalidate the class in a controlled way, i.e.
#' only when specific methods are called. See example how to use it.
#'
#' Inspired by \url{https://community.rstudio.com/t/good-way-to-create-a-reactive-aware-r6-class/84890}
#'
#' @export
#' @examples
#' \dontrun{
#' if (interactive()) {
#' library(shiny)
#' library(r6methods)
#'
#' Counter <-  R6::R6Class(
#'   "Counter",
#'   inherit = ReactiveR6,
#'   public = list(
#'     increment = function() {
#'       private$counter <- private$counter + 1
#'       private$invalidate()
#'     },
#'     decrement = function() {
#'       private$counter <- private$counter - 1
#'       private$invalidate()
#'     },
#'     silent_increment = function() {
#'       private$counter <- private$counter + 1
#'     },
#'     get_counter = function() {
#'       private$counter
#'     }
#'   ),
#'   private = list(
#'     counter = 0
#'   )
#' )
#'
#' counter <- Counter$new()$reactive()
#'
#' shinyApp(
#'   fluidPage(
#'     actionButton("increment", "Increment"),
#'     actionButton("decrement", "Decrement"),
#'     actionButton("silent_increment", "Silent increment"),
#'     textOutput("value")
#'   ),
#'   function(input, output, session) {
#'     observeEvent(input$increment, {
#'       counter()$increment()
#'     })
#'
#'     observeEvent(input$decrement, {
#'       counter()$decrement()
#'     })
#'
#'     observeEvent(input$silent_increment, {
#'       counter()$silent_increment()
#'     })
#'
#'     output$value <- renderText({
#'       counter()$get_counter()
#'     })
#'   }
#' )
#' }
#' }
ReactiveR6 <- R6::R6Class(
  "ReactiveR6",
  public = list(
    #' @description Call this method to make object instance reactive
    reactive = function() {
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- shiny::reactiveVal(0)
        private$reactiveExpr <- shiny::reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    }
  ),
  private = list(
    reactiveDep = NULL,
    reactiveExpr = NULL,
    count = 0,
    invalidate = function() {
      if (is.null(private$reactiveDep)) return()
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    }
  )
)
