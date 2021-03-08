#' Get Cursor Position From Active Document
#'
#' @param x Document Context
#'
#' @return Integer, position of cursor in text
get_cursor_pos <- function(x) {
  content <- x$contents
  range <- x$selection[[1]]$range

  start <- range$start
  start_row <- range$start[1]
  start_col <- range$start[2]

  cont <- content
  cont[cont == ""] <- " "
  start_pos <- sum(nchar(cont[1:(start_row - 1)]))
  start_pos <- start_pos + sum(nchar(substr(cont[start_row], 1 ,start_col)))
}

#' Make Methods Addin
#'
#' @export
#' @importFrom magrittr %>%
#' @importFrom rstudioapi getActiveDocumentContext
make_methods_addin <- function() {

  x <- getActiveDocumentContext()
  pos <- get_cursor_pos(x)

  x$contents %>%
    paste0(collapse = "\n") %>%
    extract_class(pos) %>%
    source_class() %>%
    make_methods() %>%
    cat()
}

#' Make Methods Addin
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @import shiny
make_methods_addin_gadget <- function() {

  x <- getActiveDocumentContext()
  pos <- get_cursor_pos(x)
  r6 <- x$contents %>%
    paste0(collapse = "\n") %>%
    extract_class(pos) %>%
    source_class()

  fields_choices <- c("both", "private", "public")
  methods_choices <- c("both", "get", "set")

  ui <- miniPage(
    gadgetTitleBar("Insert R6 Methods"),
    miniContentPanel(
      fillPage(
        fillRow(
          height = "50px",
          checkboxGroupInput(
            "fields",
            inline = TRUE,
            label = "Fields",
            choices = fields_choices,
            selected = "both"
          )
        ),
        fillRow(
          height = "50px",
          checkboxGroupInput(
            "methods",
            inline = TRUE,
            label = "Methods",
            choices = methods_choices,
            selected = "both"
          )
        ),
        fillRow(
          height = "50px",
          checkboxInput("roxygen", "Add roxygen docs?")
        )
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {
      methods <- make_methods(
        r6,
        input$fields,
        input$methods,
        input$roxygen
      )
      cat(methods)
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
#' @importFrom magrittr %>%
#' @importFrom rstudioapi getActiveDocumentContext setDocumentContents
insert_methods_addin <- function() {

  x <- getActiveDocumentContext()
  pos <- get_cursor_pos(x)

  x$contents %>%
    paste0(collapse = "\n") %>%
    insert_methods(pos) %>%
    setDocumentContents()
}

#' Insert Methods Addin
#'
#' @export
#'
#' @import miniUI
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom rstudioapi getActiveDocumentContext setDocumentContents
insert_methods_addin_gadget <- function() {

  x <- getActiveDocumentContext()
  pos <- get_cursor_pos(x)
  r6 <- x$contents %>%
    paste0(collapse = "\n") %>%
    extract_class(pos) %>%
    source_class()

  fields <- c(
    "all", "private", "public",
    names(r6$private_fields),
    names(r6$public_fields)
  )
  methods <- c("both", "set", "get")

  ui <- miniPage(
    gadgetTitleBar("Insert R6 Methods"),
    miniContentPanel(
      fillPage(
        fillRow(
          height = "50px",
          checkboxInput(
            "add_roxygen",
            "Add roxygen docs",
            TRUE
          )
        ),
        lapply(fields, function(f) {
          fillRow(
            height = "50px",
            checkboxGroupInput(
              f,
              inline = TRUE,
              label = f,
              choices = methods,
              selected = if (f == "all") "both" else NULL
            )
          )
        })
      )
    )
  )

  server <- function(input, output, session) {

    observeEvent(input$done, {

      content <- paste0(x$contents, collapse = "\n")
      print(input[[fields[2]]])

      purrr::walk(fields, function(f) {
        method <- input[[f]]
        if (is.null(method)) return()

        content <<- insert_methods(
          content,
          pos,
          field = f,
          method = method,
          add_roxygen = input$add_roxygen
        )
      })

      setDocumentContents(content)
      invisible(stopApp())
    })

    observeEvent(input$cancel, {
      invisible(stopApp())
    })
  }

  viewer <- dialogViewer("Insert R6 Methods", width = 300, height = 200)
  runGadget(ui, server, viewer = viewer)
}
