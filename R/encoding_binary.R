#' Perform binary encoding of factor variables
#'
#' `step_encoding_binary()` creates a *specification* of a recipe step that will
#' perform binary encoding of factor variables.
#'
#' @inheritParams recipes::step_center
#' @inheritParams recipes::step_dummy
#' @param ... One or more selector functions to choose which variables are
#'   affected by the step. See [recipes::selections()] for more details.  For the `tidy`
#'   method, these are not currently used.
#' @param res A list containing levels of training variables is stored
#'   here once this preprocessing step has be trained by [recipes::prep()].
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
#'   step_encoding_binary(all_nominal_predictors()) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
step_encoding_binary <-
  function(
    recipe,
    ...,
    role = NA,
    trained = FALSE,
    res = NULL,
    columns = NULL,
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("encoding_binary")
  ) {
    add_step(
      recipe,
      step_encoding_binary_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        res = res,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_encoding_binary_new <-
  function(terms, role, trained, res, columns, keep_original_cols, skip, id) {
    step(
      subclass = "encoding_binary",
      terms = terms,
      role = role,
      trained = trained,
      res = res,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_encoding_binary <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  check_type(
    training[, col_names],
    types = c("factor", "ordered", "unordered"),
  )

  values <- lapply(training[, col_names], encoding_binary_impl)

  step_encoding_binary_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    res = values,
    columns = col_names,
    keep_original_cols = recipes::get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

encoding_binary_impl <- function(x) {
  levels(x)
}

#' @export
bake.step_encoding_binary <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (col_name in col_names) {
    new_cols <- encoding_binary_apply(
      new_data[[col_name]],
      object$res[[col_name]]
    )

    colnames(new_cols) <- paste(col_name, colnames(new_cols), sep = "_")

    new_cols <- check_name(new_cols, new_data, object, names(new_cols))

    new_data <- vctrs::vec_cbind(new_data, new_cols)
  }

  new_data <- remove_original_cols(new_data, object, col_names)

  new_data
}

encoding_binary_apply <- function(x, lvls) {
  n_cols <- ceiling(log2(length(lvls))) + 1

  if (!identical(levels(x), lvls)) {
    rlang::abort("levels doesn't match")
  }

  res <- t(sapply(as.integer(x), function(x) {
    as.integer(intToBits(x))
  }))
  res <- res[, seq_len(n_cols)]

  colnames(res) <- 2^(seq_len(n_cols) - 1)

  dplyr::as_tibble(res)
}

#' @export
print.step_encoding_binary <-
  function(x, width = max(20, options()$width - 31), ...) {
    title <- "Binary Encoding on "
    print_step(x$columns, x$terms, x$trained, width = width, title = title)
    invisible(x)
  }

#' @rdname step_encoding_binary
#' @usage NULL
#' @export
tidy.step_encoding_binary <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(
      terms = names(x$res),
      value = lengths(x$res)
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(
      terms = term_names,
      value = NA_real_
    )
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.extrasteps
#' @export
required_pkgs.step_encoding_binary <- function(x, ...) {
  c("extrasteps")
}
