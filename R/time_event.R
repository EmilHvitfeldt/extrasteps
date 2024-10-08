#' Indicate Recurrent Date Time Event
#'
#' `step_time_event()` creates a *specification* of a recipe step that will
#' create new columns indicating if the date fall on recurrent event.
#'
#' @inheritParams recipes::step_center
#' @inheritParams recipes::step_date
#' @param rules Named list of `almanac` rules.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [recipes::prep.recipe()] is used.
#' @return An updated version of `recipe` with the new check added to the
#'  sequence of any existing operations.
#' @export
#' @details Unlike some other steps `step_time_event` does *not* remove the
#' original date variables by default. Set `keep_original_cols` to `FALSE` to
#' remove them.
#'
#' @examples
#' library(recipes)
#' library(extrasteps)
#' library(almanac)
#' library(modeldata)
#'
#' data(Chicago)
#'
#' on_easter <- yearly() %>% recur_on_easter()
#' on_weekend <- weekly() %>% recur_on_weekends()
#'
#' rules <- list(easter = on_easter, weekend = on_weekend)
#'
#' rec_spec <- recipe(ridership ~ date, data = Chicago) %>%
#'   step_time_event(date, rules = rules)
#'
#' rec_spec_preped <- prep(rec_spec)
#'
#' bake(rec_spec_preped, new_data = NULL)
step_time_event <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           rules = list(),
           columns = NULL,
           keep_original_cols = FALSE,
           skip = FALSE,
           id = rand_id("time_event")) {

    add_step(
      recipe,
      step_time_event_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        rules = rules,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_time_event_new <-
  function(terms, role, trained, rules, columns, keep_original_cols, skip, id) {
    step(
      subclass = "time_event",
      terms = terms,
      role = role,
      trained = trained,
      rules = rules,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
}

#' @export
prep.step_time_event <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  date_data <- info[info$variable %in% col_names, ]
  if (any(date_data$type != "date"))
    rlang::abort(
      paste0("All variables for `step_date` should be either `Date` or",
             "`POSIXct` classes."
      )
    )

  if (is.null(names(x$rules)) || !is.list(x$rules)) {
    rlang::abort(
      "`rules` must be a named list."
    )
  }

  if (!all(purrr::map_lgl(x$rules, inherits, "almanac_rschedule"))) {
    rlang::abort(
      "All `rules` must be `rschedule`s from {almanac}"
    )
  }

  step_time_event_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    rules = x$rules,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_time_event <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  for (col_name in col_names) {
    tmp <- get_time_events(
      rules = object$rules,
      column = object$columns[col_name],
      name = col_name,
      new_data = new_data
    )

    names(tmp) <- paste(col_name, names(tmp), sep = "_")
    tmp[] <- lapply(X = tmp, FUN = vctrs::vec_cast, integer())

    tmp <- check_name(tmp, new_data, object, names(tmp))
    new_data <- vctrs::vec_cbind(new_data, tmp)
  }

  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

get_time_events <- function(rules, column, name, new_data) {
  res <- lapply(X = rules, FUN = function(x) almanac::alma_in(new_data[[column]],x))
  res <- as_tibble(res)
  res
}

#' @export
print.step_time_event <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Time events from ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_time_event
#' @usage NULL
#' @export
tidy.step_time_event <- function(x, ...) {
  if (is_trained(x)) {
    if (length(x$columns) == 0) {
      res <- tibble(terms = character(), rules = list())
    } else {
      res <- tibble(terms = unname(x$columns), rules = unname(x$rules))
    }
  } else {
    term_names <- sel2char(x$terms)
    if (length(x$columns) == 0) {
      res <- tibble(terms = character(), rules = list())
    } else {
      res <- tibble(terms = term_names, rules = unname(x$rules))
    }
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.extrasteps
#' @export
required_pkgs.step_time_event <- function(x, ...) {
  c("extrasteps", "almanac")
}
