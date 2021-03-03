#' Insert Methods
#'
#' @param content Character, content of the file or a string
#' @param cursor_row Integer, row position of cursor
#'
#' @return Character, modified \code{content} with injected methods
#'
insert_methods <- function(content, cursor_row) {

  if (length(content) == 1) content <- strsplit(content, "\n")[[1]]

  # Find R6 class calls
  start_row <- grep("R6Class\\(", content)
  # Check whether cursor is placed after `R6Class` call by getting all
  # calls that take place before the cursor
  start_row <- start_row[start_row <= cursor_row]
  if (length(start_row) == 0) {
    stop("Place cursor inside R6 class definition")
  }
  # Get the nearest one
  start_row <- start_row[length(start_row)]

  # Get text that is after selected class definition
  class_content <- content[start_row:length(content)]
  class_content <- paste(class_content, collapse = "\n")
  # Get substring up to closing parathensis of class call
  class_content <- substr(class_content, 1, find_closing(class_content))

  # Get end row of class definition
  end_row <- start_row + length(strsplit(class_content, "\n")[[1]]) - 1

  if (start_row == end_row) {
    stop("One-line class definitions are not supported")
  }

  # Get position of public list
  public_list_start_pos <- stringr::str_locate(class_content, "public[ \t]+=[\t ]+list\\(")
  if (all(is.na(public_list_start_pos))) {
    # Find appearences of `list(` in class call not starting with `private`
    # Get the first one as `public` arguments comes before `private`
    public_list_start_pos <- stringr::str_locate_all(
      class_content,
      "(?<!private[ \t\n]{0,200}=[ \t\n]{0,200})list\\("
    )[[1]][1, 1]
  }

  if (!is.numeric(public_list_start_pos)) {
    stop("No public list found in R6 class call")
  }

  after_public_list <- substr(class_content, public_list_start_pos, nchar(class_content))
  public_list_end_pos <- find_closing(after_public_list) + public_list_start_pos - 1

  public_list <- substr(class_content, public_list_start_pos, public_list_end_pos)
  list_content_sep <- stringr::str_match(public_list, "list\\(([ \n\t]+)")[1, 2]
  public_list_closing <- stringr::str_locate(public_list, "[ \n\t]+\\)$")[1, 1]

  # Make methods strings
  methods <- class_content %>%
    source_class() %>%
    make_methods()

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

  # Recreate file content containing new class
  content_before <- content[1:max(1, start_row - 1)]
  content_after <- content[min(length(content), end_row + 1):length(content)]

  new_class_split <- strsplit(new_class, "\n")[[1]]
  if (content_before[length(content_before)] == new_class_split[1]) content_before <- ""
  if (content_after[1] == new_class_split[length(new_class_split)]) content_after <- ""

  paste0(
    paste(content_before, collapse = "\n"),
    new_class,
    paste(content_after, collapse = "\n"),
    collapse = "\n"
  )
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

