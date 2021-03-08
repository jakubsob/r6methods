#' Find Closing
#'
#' Find position of closing character to first encountered opening character
#'
#' @param text Character, text to search
#' @param opening Opening character
#' @param closing Closing character
#'
#' @return Integer, position of closing character
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_locate_all
#' @importFrom dplyr mutate select arrange filter pull bind_rows
find_closing <- function(text, opening = "\\(", closing = "\\)") {
  type <- end <- start <- csum <- NULL

  # Get all matches of opening and closing character in the string
  matches <- bind_rows(
    str_locate_all(text, opening)[[1]] %>%
      as.data.frame() %>%
      mutate(type = 1),
    str_locate_all(text, closing)[[1]] %>%
      as.data.frame() %>%
      mutate(type = -1)
  ) %>%
    select(-end) %>%
    arrange(start) %>%
    mutate(csum = cumsum(type))

  # Get first matching closing bracket for public list opening
  filter(matches, csum == 0)$start[1]
}
