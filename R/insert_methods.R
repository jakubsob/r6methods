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

#' Extract R6 Class
#'
#' @param content Character, content of a file or a string
#' @param start_pos Integer, row position of cursor. Serves as starting point to search
#'   for class definition
#'
#' @return A list with fields
#' \itemize{
#'   \item{class_content}{Character, extracted class definition}
#'   \item{start}{Integer, start position of class definition within `content`}
#'   \item{end}{Integer, end position of class definition within `content`}
#' }
#'
#' @importFrom stringr str_locate_all
#' @importFrom dplyr filter
#' @importFrom purrr map_int
extract_class <- function(content, start_pos = 1) {
  # Find R6 class calls
  calls <- str_locate_all(
    content,
    "\\w+\\s+(<-|=)\\s+R6::R6Class\\(|\\w+\\s+(<-|=)\\s+R6Class\\("
  )[[1]]

  selected_call <- calls %>%
    as.data.frame() %>%
    mutate(end = map_int(start, ~ start + find_closing(substr(content, .x, nchar(content))))) %>%
    filter(start_pos >= start & start_pos <= end)

  if (nrow(selected_call) != 1) {
    stop("Cursor is not inside R6 class definition")
  }

  # Get substring up to closing parathensis of class call
  start <- selected_call$start
  end <- selected_call$end
  class_content <- substr(content, start, end)

  list(
    class_content = class_content,
    start = start,
    end = end
  )
}

#' Insert Methods
#'
#' @param content Character, content of the file or a string
#' @param start_pos Integer, position of cursor within `content`. Number of characters
#'   before the cursor.
#' @inheritParams make_methods
#'
#' @return Character, modified \code{content} with injected methods
#'
#' @importFrom stringr str_locate str_locate_all str_match
insert_methods <- function(
  content,
  start_pos = 1,
  field = c("all", "public", "private"),
  method = c("both", "get", "set"),
  add_roxygen = TRUE
) {

  extracted_class <- extract_class(content, start_pos)
  class_content <- extracted_class$class_content

  # Get start position of public list within class definition
  public_list_start_pos <- str_locate(class_content, "public[ \t]+=[\t ]+list\\(")
  if (all(is.na(public_list_start_pos))) {
    # Find appearences of `list(` in class call not starting with `private`
    # Get the first one as `public` argument comes before `private`
    public_list_start_pos <- str_locate_all(
      class_content,
      "(?<!private[ \t\n]{0,200}=[ \t\n]{0,200})list\\("
    )[[1]][1, 1]
  }

  if (!is.numeric(public_list_start_pos)) {
    stop("No public list found in R6 class call")
  }

  # Extract public list from class definition and get content before and after the list
  after_public_list <- substr(class_content, public_list_start_pos, nchar(class_content))
  public_list_end_pos <- find_closing(after_public_list) + public_list_start_pos - 1
  public_list <- substr(class_content, public_list_start_pos, public_list_end_pos)
  list_content_sep <- str_match(public_list, "list\\(([ \n\t]+)")[1, 2]
  public_list_closing <- str_locate(public_list, "[ \n\t]+\\)$")[1, 1]

  # Make methods strings
  methods <- class_content %>%
    source_class() %>%
    make_methods(field, method, add_roxygen)

  new_public_list <- paste0(
    substr(public_list, 1, public_list_closing - 1),
    paste0(",", list_content_sep),
    paste0(strsplit(methods, "\n")[[1]], collapse = list_content_sep),
    substr(public_list, public_list_closing, nchar(public_list))
  )

  new_class <- paste0(
    substr(class_content, 1, public_list_start_pos - 1),
    new_public_list,
    substr(class_content, public_list_end_pos + 1, nchar(class_content))
  )

  paste0(
    substr(content, 0, max(extracted_class$start - 1, 0)),
    new_class,
    substr(content, extracted_class$end + 1, nchar(content))
  )
}
