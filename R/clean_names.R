#' Clean all column names
#'
#' `step_clean_names` creates a *specification* of a recipe
#'  step that will clean all column names so they are fit for future functions.

#'
#' @inheritParams recipes::step_center
#' @inherit recipes::step_center return
#' @param ... One or more selector functions to choose which
#'  variables will be transformed. See [selections()] for
#'  more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param columns Not used in this step as all columns are used.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` which
#'  is the columns that will be affected.
#' @export
#' @examples
#' library(recipes)
#' examples <- data.frame(`5letters` = letters,
#'                        `   "numbers"` = seq_along(letters))
#'
#' rec <- recipe(~ ., data = examples) %>%
#'   step_clean_names()
#'
#' prep(rec) %>% juice()
#' @importFrom recipes rand_id add_step ellipse_check
step_clean_names <- function(recipe, ..., role = NA, trained = FALSE,
                         columns = NULL, skip = FALSE,
                         id = rand_id("clean_names")) {
  add_step(
    recipe,
    step_clean_names_new(
      terms = NULL,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

#' @importFrom recipes step
step_clean_names_new <-
  function(terms, role, trained, columns, skip, id) {
    step(
      subclass = "clean_names",
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
prep.step_clean_names <- function(x, training, info = NULL, ...) {

  step_clean_names_new(
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
bake.step_clean_names <- function(object, new_data, ...) {
  colnames(new_data) <- make_clean_names(colnames(new_data))
  as_tibble(new_data)
}

#' @importFrom recipes printer terms_select
#' @export
print.step_clean_names <- function(x, width = max(20, options()$width - 29), ...) {
  cat("clean_names on ", sep = "")
  printer(x$columns, x$terms, x$trained, width = width)
  invisible(x)
}

#' @rdname step_clean_names
#' @param x A `step_clean_names` object.
#' @export
tidy.step_clean_names <- function(x, ...) {
  res <- simple_terms(x, ...)
  res$id <- x$id
  res
}

make_clean_names <- function(string) {
  new_names <- string %>%
    gsub("'", "", .) %>%
    gsub("\"", "", .) %>%
    gsub("%", ".percent_", .) %>%
    gsub("#", ".number_", .) %>%
    gsub("^[[:space:][:punct:]]+", "", .) %>%
    make.names() %>%
    tolower()

  dupe_count <- vapply(seq_along(new_names), function(i) {
    sum(new_names[i] == new_names[1:i])
  }, integer(1))

  new_names[dupe_count > 1] <- paste(
    new_names[dupe_count > 1],
    dupe_count[dupe_count > 1],
    sep = "_"
  )
  new_names
}
