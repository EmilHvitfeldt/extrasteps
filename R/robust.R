#' Perform Robust Scaling
#'
#' `step_robust` creates a *specification* of a recipe step that will perform
#' Robust scaling.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [selections()] for more details.  For the `tidy`
#'   method, these are not currently used.
#' @param res A list containing the 3 quantiles of training variables is stored
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
#'   step_robust(all_predictors()) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
step_robust <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           res = NULL,
           columns = NULL,
           skip = FALSE,
           id = rand_id("robust")
  ) {

    add_step(
      recipe,
      step_robust_new(
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

step_robust_new <-
  function(terms, role, trained, res, columns, skip, id) {
    step(
      subclass = "robust",
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
prep.step_robust <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  values <- lapply(training[, col_names], robust_impl)

  step_robust_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    res = values,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

robust_impl <- function(x) {
  quantiles <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  list(lower = quantiles[1], median = quantiles[2], upper = quantiles[3])
}

#' @export
bake.step_robust <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (col_name in col_names) {
    new_data[, col_name] <- robust_apply(
      new_data[[col_name]],
      object$res[[col_name]]
    )
  }
  as_tibble(new_data)
}

robust_apply <- function(x, res) {
  (x - res$median) / (res$upper - res$lower)
}

#' @export
print.step_robust <-
  function(x, width = max(20, options()$width - 31), ...) {
    cat("Robust scaling on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_robust` object.
#' @export
tidy.step_robust <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = rep(names(x$res), each = 3),
      statistic = rep(c("lower", "median", "higher"), length(x$res)),
      value = unname(unlist(x$res)) %||% numeric()
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
