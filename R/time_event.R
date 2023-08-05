#' Indicate Recurrent Date Time Event
#'
#' `step_time_event()` creates a *specification* of a recipe step that will
#' create new columns indicating if the date fall on recurrent event.
#'
#' @inheritParams recipes::step_center
#' @param rules Named list of `almanac` rules.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [prep.recipe()] is used.
#' @return An updated version of `recipe` with the new check added to the
#'  sequence of any existing operations.
#' @export
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
        skip = skip,
        id = id
      )
    )
  }

step_time_event_new <-
  function(terms, role, trained, rules, columns, skip, id) {
    step(
      subclass = "time_event",
      terms = terms,
      role = role,
      trained = trained,
      rules = rules,
      columns = columns,
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
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_time_event <- function(object, new_data, ...) {
  if (length(object$column) == 0L) {
    # Empty selection
    return(new_data)
  }

  new_cols <- purrr::imap_dfc(object$columns, time_event_helper,
                              new_data, object$rules)

  new_cols <- check_name(new_cols, new_data, object, names(new_cols))

  new_data <- dplyr::bind_cols(new_data, new_cols)
  new_data <- dplyr::select(new_data, -names(object$columns))
  new_data
}

time_event_helper <- function(columnn, name, new_data, rule) {
  res <- purrr::map_dfc(rule, ~ alma_in(new_data[[columnn]], .x))

  names(res) <- paste(name, names(res), sep = "_")
  res
}

#' @export
print.step_time_event <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Time events from ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_time_event` object.
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
