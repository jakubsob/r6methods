#' Make method string
#'
#' @param field Character name of class field
#' @param is_public Logical, whether the field is in public list
#' @param add_roxygen Logical, whether to add roxygen description of method
#'
#' @return Character
#'
#' @rdname make_method_str
#'
#' @importFrom glue glue
make_getter_method_str <- function(field, is_public = TRUE, add_roxygen = TRUE) {
  if (is.null(field)) return(NULL)

  if (is_public) {
    method_str <- glue("get_{field} = function() {{
      self${field}
    }}")
  } else {
    method_str <- glue("get_{field} = function() {{
      private${field}
    }}")
  }

  if (add_roxygen) {
    return(glue("#' @description Getter for {field}\n{method_str}"))
  }

  method_str
}

#' @rdname make_method_str
make_setter_method_str <- function(field, is_public = TRUE, add_roxygen = TRUE) {
  if (is.null(field)) return(NULL)

  if (is_public) {
    method_str <- glue("set_{field} = function({field}) {{
      self${field} <- {field}
    }}")
  } else {
    method_str <- glue("set_{field} = function({field}) {{
      private${field} <- {field}
    }}")
  }

  if (add_roxygen) {
    return(glue("#' @description Setter for {field}\n{method_str}"))
  }

  method_str
}

#' Make methods
#'
#' @param r6 R6 class for which to create methods
#' @param field Character, fields for which to create method. May be "all",
#'   "public", "private" or name of class field. Multiple values allowed.
#' @param method Character, methods to create. One of "both", "get", "set"
#' @param add_roxygen Logical, whether to add roxygen description of method
#'
#' @return Character containing generated methods to put into class definition
#' @export
#'
#' @importFrom glue glue_collapse
#' @importFrom purrr map imap
make_methods <- function(
  r6,
  field = c("all", "public", "private", names(r6$public_fields), names(r6$private_fields)),
  method = c("both", "get", "set"),
  add_roxygen = TRUE
) {

  match.arg(field, several.ok = TRUE)
  match.arg(method, several.ok = TRUE)

  # method <- method[1]

  public_methods <- names(r6$public_methods)
  public_fields <- names(r6$public_fields)
  private_fields <- names(r6$private_fields)

  if (length(c(public_fields, private_fields)) == 0) {
    stop("Supplied R6 class has no fields")
  }

  fields <- list(
    "all" = c(public_fields, private_fields),
    "public" = public_fields,
    "private" = private_fields
  )

  methods <- list(
    "set" = make_setter_method_str,
    "get" = make_getter_method_str
  )

  if ("both" %in% method) method <- c("set", "get")

  # Get names of fields and make sure they aren't repeated
  fields <- fields[field]
  fields <- fields[!is.null(fields)]
  fields <- unique(unlist(fields, use.names = FALSE))
  fields <- c(fields, field[!field %in% fields & !field %in% c("all", "public", "private")])

  methods_str <- map(fields, function(f) {
    imap(methods[method], function(m, n) {
      if (glue("{n}_{f}") %in% public_methods) return(NULL)
      m(f, f %in% public_fields, add_roxygen)
    })
  })

  glue_collapse(unlist(methods_str), sep = ",\n")
}
