#' Combine factor levels using stringdist
#'
#' `step_combine_stringdist()` creates a *specification* of a recipe step that
#' will combine factor levels that have a low stringdist between them.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details.  For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param distance Integer, value to determine which strings should be combined
#'  with which. The value is being used inclusive, so `2` will combine levels
#'  that have a string distance between them of 2 or lower.
#' @param res A list denoting the way the labels should be collapses is stored
#'  here once this preprocessing step has be trained by [prep()].
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected) and `base`.
#' @export
#' @examples
#' library(recipes)
#' library(tibble)
#' data0 <- tibble(
#'   x1 = c("a", "b", "d", "e", "sfgsfgsd", "hjhgfgjgr"),
#'   x2 = c("ak", "b", "djj", "e", "hjhgfgjgr", "hjhgfgjgr")
#'  )
#'
#' rec <- recipe(~., data = data0) %>%
#'   step_combine_stringdist(all_predictors(), distance = 1) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
#'
#' rec <- recipe(~., data = data0) %>%
#'   step_combine_stringdist(all_predictors(), distance = 2) %>%
#'   prep()
#'
#' rec %>%
#'   bake(new_data = NULL)
#'
#' tidy(rec, 1)
#'
step_combine_stringdist <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           distance = NULL,
           res = NULL,
           columns = NULL,
           skip = FALSE,
           id = rand_id("combine_stringdist")
  ) {

    if (is.null(distance)) {
      rlang::abort("`distance` argument must be set.")
    }

    add_step(
      recipe,
      step_combine_stringdist_new(
        terms = enquos(...),
        role = role,
        trained = trained,
        distance = distance,
        res = res,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_combine_stringdist_new <-
  function(terms, role, trained, distance, res, columns, skip, id) {
    step(
      subclass = "combine_stringdist",
      terms = terms,
      role = role,
      trained = trained,
      distance = distance,
      res = res,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_combine_stringdist <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  values <- lapply(training[ ,col_names], combine_stringdist_impl, x$distance)

  step_combine_stringdist_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    distance = x$distance,
    res = values,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

combine_stringdist_impl <- function(x, dist) {
  x <- as.character(x)
  dists <- stringdist::stringdistmatrix(x, x)

  pairs <- which(dists <= dist, arr.ind = TRUE)

  empty_logical <- logical(length(x))

  groups <- list()

  while (nrow(pairs) > 0) {
    group <- empty_logical
    selected <- pairs[1, 2]

    repeat {
      group[selected] <- TRUE
      new_selected <- pairs[pairs[, 2] %in% selected, 1]
      if (length(new_selected) == 0) break
      pairs <- pairs[!pairs[, 2] %in% selected, , drop = FALSE]
      selected <- new_selected
    }

    groups <- c(groups, list(which(group)))
  }

  lapply(groups, function(.x) x[.x])
}

#' @export
bake.step_combine_stringdist <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
    new_data[[col_names[i]]] <- combine_apply(
        new_data[[col_names[i]]],
        object$res[[i]]
      )
  }
  new_data
}

combine_apply <- function(x, dict) {
  dict <- purrr::map_dfr(dict, ~list(from = .x, to = .x[1]))

  dict$to[match(x, dict$from)]
}

#' @export
print.step_combine_stringdist <-
  function(x, width = max(20, options()$width - 31), ...) {
    msg <- ifelse(x$signed, "Signed combine_stringdist ", "combine_stringdist ")
    cat(msg, "transformation on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_combine_stringdist
#' @usage NULL
#' @export
tidy.step_combine_stringdist <- function(x, ...) {
  if (is_trained(x)) {
    res <- purrr::map_dfr(
      x$res,
      ~purrr::map_dfr(.x, ~list(from = .x, to = .x[1])),
      .id = "terms"
    )
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.extrasteps
#' @export
required_pkgs.step_combine_stringdist <- function(x, ...) {
  c("extrasteps", "stringdist")
}
