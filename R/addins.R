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

    observeEvent(input$cancel, {
      invisible(stopApp())
    })
  }

  viewer <- dialogViewer("Make R6 Methods", width = 100, height = 200)
  runGadget(ui, server, viewer = viewer)
}


#' An addin for inserting methods straigth into the source file
#'
#' @export
#'
insert_methods_addin <- function() {

  context <- rstudioapi::getActiveDocumentContext()

  # Obtain content of the file
  content <- context$content
  # Obtain row at which the cursor is positioned
  cursor_row <- context$selection[[1]]$range$start[1]

  new_content <- insert_methods(content, cursor_row)

  rstudioapi::setDocumentContents(new_content)
}

#' Insert Methods Addin
#'
#' @export
#'
#' @import miniUI
#' @import shiny
insert_methods_addin_gadget <- function() {

  context <- getActiveDocumentContext()
  content <- context$content
  cursor_row <- context$selection[[1]]$range$start[1]

  ui <- miniPage(

    gadgetTitleBar("Add R6 Methods"),
    miniContentPanel(
      "Select fields to make methods for",
      hr(),
      fillPage(
        fillRow(
          fillCol(
            checkboxGroupInput(
              "fields",
              label = NULL,
              choices = c("both", "private", "public"),
              selected = "both"
            )
          ),
          fillRow(
            checkboxGroupInput(
              "methods",
              label = NULL,
              choices = c("both", "get", "set"),
              selected = "both"
            )
          )
        ),
        fillRow(
          checkboxInput("roxygen", "Add roxygen docs?")
        )
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      new_content <- insert_methods(
        content,
        cursor_row,
        input$fields,
        input$methods,
        input$roxygen
      )
      rstudioapi::setDocumentContents(new_content)
      invisible(stopApp())
    })

    observeEvent(input$cancel, {
      invisible(stopApp())
    })
  }

  viewer <- dialogViewer("Make R6 Methods", width = 100, height = 200)
  runGadget(ui, server, viewer = viewer)
}
