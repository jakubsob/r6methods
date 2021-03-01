insert_methods <- function() {

  context <- rstudioapi::getActiveDocumentContext()

  # Obtain row at which the cursor is positioned
  cursor_row <- context$selection[[1]]$range$start[1]
  # cursor_row <- 26
  # Obtain content of the file
  content <- context$contents

  # Find R6 class calls
  start_row <- grep("R6Class\\(", content)
  # Check whether cursor is placed after `R6Class` call by getting all
  # calls that take place before the cursor
  start_row <- start_row[start_row < cursor_row]
  # Get the nearest one
  start_row <- start_row[length(start_row)]

  # Get text that is after selected class definition
  class_content <- content[start_row:length(content)]
  class_content <- paste(class_content, collapse = "\n")

  # Get class definition only
  class_closing <- find_closing(class_content)
  class_content <- substr(class_content, 1, class_closing)

  # Get end row of class definition
  end_row <- start_row + length(strsplit(class_content, "\n")[[1]])

  public_list_pos <- stringr::str_locate(class_content, "public[ \t]+=[\t ]+list\\(")
  if (all(is.na(public_list_pos))) {
    # Find appearences of `list(` in class call not starting with `private`
    # Get the first one as `public` arguments comes before `private`
    public_list_pos <- stringr::str_locate_all(
      class_content,
      "(?<!private[ \t\n]{0,10}=[ \t\n]{0,10})list\\("
    )[[1]][1, 2]
  } else {
    stop("No public list in R6 class call")
  }

  before_public_list <- substr(class_content, 1, public_list_pos - 1)
  after_public_list <- substr(class_content, public_list_pos, nchar(class_content))

  closing_ind <- find_closing(after_public_list)
  public_list <- substr(after_public_list, 1, closing_ind)

  after_public_list <- substr(after_public_list, nchar(public_list) + 1, nchar(after_public_list))

  public_list_opened <- trimws(substr(public_list, 1, nchar(public_list) - 1), "right")

  # Get indent in public list
  indent <- stringr::str_extract(public_list, "^\\([ \n]+")[[1]]
  indent <- stringr::str_extract(indent, "[ ]+")

  # Make methods strings
  methods <- class_content %>%
    source_class() %>%
    make_methods()

  # Create public list string
  new_public_list <- paste0(
    public_list_opened,
    ",\n",
    paste(
      indent,
      strsplit(methods, "\n")[[1]],
      collapse = "\n"
    ),
    "\n",
    indent,
    ")"
  )

  # Compose the whole class
  new_class <- paste0(
    before_public_list,
    new_public_list,
    after_public_list
  )
  new_class <- strsplit(new_class, "\n")[[1]]

  # Recreate file content containing new class
  new_content <- c(
    content[1:(start_row - 1)],
    new_class,
    content[end_row:length(content)]
  )

  rstudioapi::documentNew(new_content, type = "r")
}

