#' Make Methods Addin
#'
#' @export
#' @importFrom rstudioapi getActiveDocumentContext
make_methods_addin <- function() {

  x <- getActiveDocumentContext()
  cursor_text <- x$selection[[1]]$text

  r6 <- source_class(cursor_text)
  cat(make_methods(r6))
}

#' Make Methods Addin
#'
#' @export
#'
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @import shiny
make_methods_addin_gadget <- function() {

  x <- getActiveDocumentContext()
  cursor_text <- x$selection[[1]]$text
  r6 <- source_class(cursor_text)

  ui <- miniPage(

    gadgetTitleBar("Add R6 Methods"),
    miniContentPanel(
      "Select fields to make methods for",
      hr(),
      checkboxGroupInput(
        "fields",
        label = NULL,
        choices = c("both", "private", "public"),
        selected = "both"
      ),
      checkboxInput("roxygen", "Add roxygen docs?")
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      cat(make_methods(r6, field = input$fields[1], add_roxygen = input$roxygen))
      invisible(stopApp())
    })

  }

  viewer <- dialogViewer("Make R6 Methods", width = 100, height = 200)
  runGadget(ui, server, viewer = viewer)
}
