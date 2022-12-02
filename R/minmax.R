#' Perform Min Max Scaling
#'
#' `step_minmax` creates a *specification* of a recipe step that will perform
#' Min Max scaling.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.  For the `tidy`
#'   method, these are not currently used.
#' @param res A list containing min and max of training variables is stored here
#'   once this preprocessing step has be trained by [prep()].
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
#'   step_minmax(all_predictors()) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
step_minmax <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           res = NULL,
           columns = NULL,
           skip = FALSE,
           id = rand_id("minmax")
  ) {

    add_step(
      recipe,
      step_minmax_new(
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

step_minmax_new <-
  function(terms, role, trained, res, columns, skip, id) {
    step(
      subclass = "minmax",
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
prep.step_minmax <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  values <- lapply(training[, col_names], minmax_impl)

  step_minmax_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    res = values,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

minmax_impl <- function(x) {
  list(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
}

#' @export
bake.step_minmax <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (col_name in col_names) {
    new_data[, col_name] <- minmax_apply(
      new_data[[col_name]],
      object$res[[col_name]]
    )
  }
  new_data
}

minmax_apply <- function(x, res) {
  (x - res$min) / (res$max - res$min)
}

#' @export
print.step_minmax <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Min Max scaling on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_minmax` object.
#' @export
tidy.step_minmax <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = c(names(x$res), names(x$res)),
      statistic = rep(c("min", "max"), each = length(x$res)),
      value = unname(c(map_dbl(x$res, "min"), map_dbl(x$res, "max")))
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

#' @rdname required_pkgs.extrasteps
#' @export
required_pkgs.step_minmax <- function(x, ...) {
  c("extrasteps")
}
