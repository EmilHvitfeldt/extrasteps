#' Perform Max Abs Scaling
#'
#' `step_maxabs` creates a *specification* of a recipe step that will perform
#' Max Abs scaling.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.  For the `tidy`
#'   method, these are not currently used.
#' @param res A list containing absolute max of training variables is stored
#'   here once this preprocessing step has be trained by [prep()].
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
#'   step_maxabs(all_predictors()) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
step_maxabs <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           res = NULL,
           columns = NULL,
           skip = FALSE,
           id = rand_id("maxabs")
  ) {

    add_step(
      recipe,
      step_maxabs_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        res = res,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_maxabs_new <-
  function(terms, role, trained, res, columns, skip, id) {
    step(
      subclass = "maxabs",
      terms = terms,
      role = role,
      trained = trained,
      res = res,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_maxabs <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  values <- lapply(training[, col_names], maxabs_impl)

  step_maxabs_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    res = values,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

maxabs_impl <- function(x) {
  list(max = abs(max(x, na.rm = TRUE)))
}

#' @export
bake.step_maxabs <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (col_name in col_names) {
    new_data[, col_name] <- maxabs_apply(
      new_data[[col_name]],
      object$res[[col_name]]
    )
  }
  new_data
}

maxabs_apply <- function(x, res) {
  x / res$max
}

#' @export
print.step_maxabs <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Max Abs scaling on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_maxabs` object.
#' @export
tidy.step_maxabs <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$res),
      statistic = rep("max", each = length(x$res)),
      value = unname(map_dbl(x$res, "max"))
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      statistic = NA_character_,
      value = NA_real_
    )
  }
  res$id <- x$id
  res
}
