#' Get Cursor Position From Active Document
#'
#' @param context Active document context
#'
#' @return Integer, position of cursor in text
get_cursor_pos <- function(context) {
  content <- context$contents
  start_row <- context$selection[[1]]$range$start[1]
  nchar(paste0(content[1:start_row], collapse = "\n"))
}

#' Make Methods Addin
#'
#' @return No return value, called for side effects
#'
#' @export
#' @importFrom magrittr %>%
make_methods_addin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  context$contents %>%
    paste0(collapse = "\n") %>%
    extract_class(get_cursor_pos(context)) %>%
    source_class() %>%
    make_methods() %>%
    cat()
}

#' An addin for inserting methods straigth into the source file
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom rstudioapi getActiveDocumentContext setDocumentContents
insert_methods_addin <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  context$contents %>%
    paste0(collapse = "\n") %>%
    insert_methods(get_cursor_pos(context)) %>%
    rstudioapi::setDocumentContents()
}

#' Make Gadget
#'
#' Create gadget for generating R6 methods. Action after clicking `Done` button is defined
#' by `done_fun`.
#'
#' @param title Character, title of gadget window
#' @param title_bar Character, gadget title bar
#' @param done_fun Function to be used after clicking `Done` button
#'
#' @return Function creating and running a Shiny gadget
#'
#' @import shiny
#' @importFrom magrittr %>%
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom purrr walk
#' @importFrom rstudioapi getActiveDocumentContext
make_gadget <- function(title, title_bar, done_fun) {
  function() {

    context <- rstudioapi::getActiveDocumentContext()
    content <- paste0(context$contents, collapse = "\n")
    pos <- get_cursor_pos(context)
    r6 <- content %>%
      extract_class(pos) %>%
      source_class()

    fields <- c("all", "private", "public")
    methods <- c("both", "set", "get")

    checkboxes <- lapply(fields, function(f) {
      fillRow(
        height = "40px",
        checkboxGroupInput(
          f,
          inline = TRUE,
          label = f,
          choices = methods,
          selected = if (f == "all") "both" else NULL
        )
      )
    })

    ui <- miniPage(
      gadgetTitleBar(title_bar),
      miniContentPanel(
        fillPage(
          fillRow(
            height = "20px",
            checkboxInput("add_roxygen", "Add roxygen docs", TRUE)
          ),
          hr(),
          checkboxes
        )
      )
    )

    server <- function(input, output, session) {

      observeEvent(input$done, {
        done_fun(
          input = input,
          context = context,
          pos = pos,
          fields = fields,
          r6 = r6
        )
        invisible(stopApp())
      })

      observeEvent(input$cancel, {
        invisible(stopApp())
      })
    }

    viewer <- dialogViewer(title, width = 300, height = 200)
    runGadget(ui, server, viewer = viewer)
  }
}

#' Make methods addin gadget
#'
#' @return No return value, called for side effects
#'
#' @export
#' @importFrom purrr walk
#' @importFrom glue glue_collapse
make_methods_addin_gadget <- make_gadget(
  "R6 Methods",
  "Make R6 Methods",
  function(...) {
    args <- list(...)
    context <- args$context
    r6 <- args$r6
    fields <- args$fields
    input <- args$input

    content <- paste0(context$contents, collapse = "\n")
    map(fields, function(f) {
      if (is.null(input[[f]])) return()
      make_methods(r6, field = f, method = input[[f]], input$add_roxygen)
    }) %>%
      unlist() %>%
      glue_collapse(sep = ",\n") %>%
      cat()
  }
)

#' Insert methods addin gadget
#'
#' @return No return value, called for side effects
#'
#' @export
#' @importFrom purrr walk
#' @importFrom rstudioapi setDocumentContents
insert_methods_addin_gadget <- make_gadget(
  "R6 Methods",
  "Insert R6 Methods",
  function(...) {
    args <- list(...)
    context <- args$context
    pos <- args$pos
    fields <- args$fields
    input <- args$input

    content <- paste0(context$contents, collapse = "\n")
    walk(fields, function(f) {
      if (is.null(input[[f]])) return()
      content <<- insert_methods(
        content,
        pos,
        field = f,
        method = input[[f]],
        add_roxygen = input$add_roxygen
      )
    })
    rstudioapi::setDocumentContents(content)
  }
)

