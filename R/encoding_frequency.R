#' Perform frequency encoding
#'
#' `step_encoding_frequency()` creates a *specification* of a recipe step that
#' will perform frequency encoding.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [recipes::selections()] for more details.  For the `tidy`
#'   method, these are not currently used.
#' @param res A list frequencies of the levels of the training variables is
#'   stored here once this preprocessing step has be trained by [recipes::prep()].
#' @param columns A character string of variable names that will be populated
#'   (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step added to the
#'   sequence of existing steps (if any). For the `tidy` method, a tibble with
#'   columns `terms` (the columns that will be affected) and `base`.
#' @export
#' @examples
#' library(recipes)
#' library(modeldata)
#'
#' data(ames)
#'
#' rec <- recipe(~ Land_Contour + Neighborhood, data = ames) %>%
#'   step_encoding_frequency(all_nominal_predictors()) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
step_encoding_frequency <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    res = NULL,
    columns = NULL,
    skip = FALSE,
    id = rand_id("encoding_frequency")
  ) {
    add_step(
      recipe,
      step_encoding_frequency_new(
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

step_encoding_frequency_new <-
  function(terms, role, trained, res, columns, skip, id) {
    step(
      subclass = "encoding_frequency",
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
prep.step_encoding_frequency <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(
    training[, col_names],
    types = c("factor", "string"),
  )

  values <- lapply(training[, col_names], encoding_frequency_impl)

  step_encoding_frequency_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    res = values,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

encoding_frequency_impl <- function(x) {
  table(x) / length(x)
}

#' @export
bake.step_encoding_frequency <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (col_name in col_names) {
    new_data[[col_name]] <- encoding_frequency_apply(
      new_data[[col_name]],
      object$res[[col_name]]
    )
  }
  new_data
}

encoding_frequency_apply <- function(x, freqs) {
  res <- unname(freqs[x])
  res <- as.numeric(res)
  res[is.na(res)] <- 0
  res
}

#' @export
print.step_encoding_frequency <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Frequency encoding on "
    print_step(x$columns, x$terms, x$trained, width = width, title = title)
    invisible(x)
  }

#' @rdname step_encoding_frequency
#' @usage NULL
#' @export
tidy.step_encoding_frequency <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = rep(names(x$res), lengths(x$res)),
      level = names(unlist(unname(x$res))),
      frequency = unname(unlist(unname(x$res)))
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      level = NA_character_,
      frequency = NA_real_
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.extrasteps
#' @export
required_pkgs.step_encoding_frequency <- function(x, ...) {
  c("extrasteps")
}
