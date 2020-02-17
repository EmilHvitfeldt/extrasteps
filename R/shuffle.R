#' Shuffle Transformation
#'
#' `step_shuffle` creates a *specification* of a recipe
#'  step that will return a random permutation of the values in selected
#'  columns.

#'
#' @inheritParams recipes::step_center
#' @inherit recipes::step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [selections()] for
#'  more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be affected.
#' @export
#' @examples
#' library(recipes)
#' set.seed(313)
#' examples <- data.frame(letters = letters,
#'                        numbers = seq_along(letters))
#'
#' rec <- recipe(~ ., data = examples) %>%
#'   step_shuffle(letters)
#'
#' prep(rec) %>% juice()
#' @importFrom recipes rand_id add_step ellipse_check
step_shuffle <- function(recipe, ..., role = NA, trained = FALSE,
                         columns = NULL, skip = FALSE,
                         id = rand_id("shuffle")) {
  add_step(
    recipe,
    step_shuffle_new(
      terms = ellipse_check(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_shuffle_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "shuffle",
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }


#' @importFrom recipes bake prep terms_select
#' @export
prep.step_shuffle <- function(x, training, info = NULL, ...) {

  step_shuffle_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = NULL,
    skip = x$skip,
    id = x$id
  )
}

#' @export
#' @importFrom tibble as_tibble
bake.step_shuffle <- function(object, new_data, ...) {
  col_names <- object$columns
  for (i in seq_along(col_names))
    new_data[, col_names[i]] <-
      sample(getElement(new_data, col_names[i]))
  as_tibble(new_data)
}

#' @importFrom recipes printer terms_select
#' @export
print.step_shuffle <- function(x, width = max(20, options()$width - 29), ...) {
  cat("Shuffle on ", sep = "")
  printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_shuffle
#' @param x A `step_shuffle` object.
#' @export
tidy.step_shuffle <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}
