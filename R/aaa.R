#' @importFrom recipes sel2char is_trained
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
