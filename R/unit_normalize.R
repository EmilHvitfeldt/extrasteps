#' Perform Unit Normalization
#'
#' `step_unit_normalize` creates a *specification* of a recipe step that will
#' perform unit normalization by scaling individual samples to have unit norm.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.  For the `tidy`
#'   method, these are not currently used.
#' @param norm Character denoting which type of normalization to perform. Must
#'   be one of `"l1"`, `"l2"`, or "`"max"`.
#' @param columns A character string of variable names that will be populated
#'   (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns `terms` (the columns that will be affected) and `base`.
#' @export
#' @examples
#' library(recipes)
#'
#' rec <- recipe(~., data = mtcars) %>%
#'   step_unit_normalize(all_predictors()) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
step_unit_normalize <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           norm = c("l2", "l1", "max"),
           columns = NULL,
           skip = FALSE,
           id = rand_id("unit_normalize")
  ) {

    add_step(
      recipe,
      step_unit_normalize_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        norm = norm,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_unit_normalize_new <-
  function(terms, role, trained, norm, columns, skip, id) {
    step(
      subclass = "unit_normalize",
      terms = terms,
      role = role,
      trained = trained,
      norm = norm,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_unit_normalize <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(training[, col_names])

  step_unit_normalize_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    norm = x$norm,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_unit_normalize <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  if (length(col_names) == 0) {
    return(new_data)
  }

  new_data[, col_names] <- unit_normalize_apply(
    new_data[, col_names, drop = FALSE],
    norm = object$norm
  )

  new_data
}

unit_normalize_apply <- function(x, norm = c("l2", "l1", "max")) {
  norm <- match.arg(norm)

  if (norm == "l2") {
    res <- x / sqrt(rowSums(x ^ 2))
  }
  if (norm == "l1") {
    res <- x / rowSums(abs(x))
  }
  if (norm == "max") {
    res <- x / apply(abs(x), 1, max)
  }

  res
}

#' @export
print.step_unit_normalize <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Unit Normalization on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_unit_normalize` object.
#' @export
tidy.step_unit_normalize <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$columns)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names
    )
  }
  res$id <- x$id
  res
}
