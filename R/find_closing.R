#' Find Closing
#'
#' Find position of closing character to first encountered opening character
#'
#' @param text Character, text to search
#' @param opening Opening character
#' @param closing Closing character
#'
#' @return Integer
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_locate_all
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select arrange filter pull bind_rows
find_closing <- function(text, opening = "\\(", closing = "\\)") {
  # Get all parathenses starting from public list
  parathenses <- dplyr::bind_rows(
    stringr::str_locate_all(text, "\\(")[[1]] %>%
      as_tibble() %>%
      dplyr::mutate(type = "1"),
    stringr::str_locate_all(text, "\\)")[[1]] %>%
      as_tibble() %>%
      dplyr::mutate(type = "-1")
  ) %>%
    dplyr::select(-end) %>%
    dplyr::arrange(start) %>%
    dplyr::mutate(type = cumsum(type))

  # Get first matching closing bracket for public list opening
  parathenses %>%
    dplyr::filter(type == 0) %>%
    dplyr::pull(start) %>%
    .[1]
}
