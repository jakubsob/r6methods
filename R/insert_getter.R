#' Source class
#'
#' Sources R6 class from text, prepends namespace to `R6Class` in order to not
#' require `R6` to be loaded.
#'
#' @param txt Character, text containing class definition
#'
#' @return R6 class
source_class <- function(txt) {
  if (!any(grepl("R6Class", txt))) stop("Text doesn't contain R6 class")
  txt <- gsub("(?<!R6::)R6Class", "R6::R6Class", txt, perl = TRUE)
  envir <- new.env()
  eval(parse(text = txt), envir = envir)
  get(ls(envir)[1], envir = envir)
}

#' Make method string
#'
#' @param field Character name of class field
#' @param add_roxygen Logical, whether to add roxygen description of method
#'
#' @return Character
#'
#' @rdname make_method_str
#'
#' @importFrom glue glue
make_getter_method_str <- function(field, add_roxygen = TRUE) {
  if (is.null(field)) return(NULL)

  method_str <- glue("get_{field} = function() {{
    self${field}
  }}")

  if (add_roxygen) {
    return(glue("#' @description Getter for {field} \n {method_str}"))
  }

  method_str
}

#' @rdname make_method_str
make_setter_method_str <- function(field, add_roxygen = TRUE) {
  if (is.null(field)) return(NULL)

  method_str <- glue("set_{field} = function({field}) {{
    self${field} <- {field}
  }}")

  if (add_roxygen) {
    return(glue("#' @description Setter for {field} \n {method_str}"))
  }

  method_str
}

#' Make methods
#'
#' @param r6 R6 class for which to create methods
#' @param field Character, fields for which to create method. May be "both",
#'   "public", "private" or name of class field. Multiple values allowed.
#' @param method Character, methods to create. One of "both", "get", "set"
#' @param add_roxygen Logical, whether to add roxygen description of method
#'
#' @return Character containing generated methods to put into class definition
#' @export
#'
#' @importFrom glue glue_collapse
make_methods <- function(
  r6,
  field = c("both", "public", "private", names(r6$public_fields), names(r6$private_fields)),
  method = c("both", "get", "set"),
  add_roxygen = TRUE
) {

  match.arg(field, several.ok = TRUE)
  match.arg(method)

  method <- method[1]

  public_fields <- names(r6$public_fields)
  private_fields <- names(r6$private_fields)

  fields <- list(
    "both" = c(public_fields, private_fields),
    "public" = public_fields,
    "private" = private_fields
  )

  methods <- list(
    "both" = list(make_setter_method_str, make_getter_method_str),
    "get" = list(make_getter_method_str),
    "set" = list(make_setter_method_str)
  )

  # Get names of fields and make sure they aren't repeated
  fields <- fields[field]
  fields <- fields[!is.null(fields)]
  fields <- unique(unlist(fields, use.names = FALSE))
  fields <- c(fields, field[!field %in% fields & !field %in% c("both", "public", "private")])

  methods_str <- lapply(fields, function(f) {
    lapply(methods[[method]], function(m) {
      m(f, add_roxygen)
    })
  })

  glue_collapse(unlist(methods_str), sep = ",\n")
}

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
