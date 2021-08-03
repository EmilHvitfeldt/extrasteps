#' @importFrom recipes sel2char is_trained format_ch_vec format_selectors
#' @importFrom recipes has_type
#' @importFrom rlang na_chr
#' @importFrom tibble tibble
simple_terms <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res
}


globalVariables(".")
