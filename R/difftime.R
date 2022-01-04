#' difftimearithmic Transformation
#'
#' `step_difftime` creates a *specification* of a recipe step
#'  that will calculate difftimes of the data.
#'
#' @inheritParams recipes::step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details.  For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param time date-time or date objects. Used for reference. Must match the
#' type of variable.
#' @param tz an optional time zone specification to be used for the conversion,
#' mainly for "POSIXlt" objects.
#' @param unit character string. Units in which the results are desired. Must be
#'  one of "auto", "secs", "mins", "hours","days",  and "weeks" Defaults to
#'  "auto".
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  columns that will be affected) and `base`.
#' @export
#' @examples
#' library(recipes)
#' example_date <- data.frame(
#'   dates = seq(as.Date("2010/1/1"), as.Date("2016/1/1"), by = "quarter")
#'  )
#'
#'  example_datetime <- data.frame(
#'   datetimes = seq(ISOdate(1993,1,1), ISOdate(1993,1,2), by = "hour")
#'  )
#'
#'
#' rec <- recipe(~ dates, data = example_date) %>%
#'   step_difftime(dates, time = as.Date("2010/1/1"))
#'
#' difftime_obj <- prep(rec)

#' juice(difftime_obj)
#'
#' recipe(~ dates, data = example_date) %>%
#'   step_difftime(dates, time = as.Date("2010/1/1"), unit = "weeks") %>%
#'   prep() %>%
#'   juice()
#'
#' recipe(~ datetimes, data = example_datetime) %>%
#'   step_difftime(datetimes, time = ISOdate(1993,1,1), unit = "secs") %>%
#'   prep() %>%
#'   juice()
#'
step_difftime <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           time = NULL,
           tz = NULL,
           unit = "auto",
           columns = NULL,
           skip = FALSE,
           id = rand_id("difftime")
  ) {
    add_step(
      recipe,
      step_difftime_new(
        terms = ellipse_check(...),
        role = role,
        trained = trained,
        time = time,
        tz = tz,
        unit = unit,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_difftime_new <-
  function(terms, role, trained, time, tz, unit, columns, skip, id) {
    step(
      subclass = "difftime",
      terms = terms,
      role = role,
      trained = trained,
      time = time,
      tz = tz,
      unit = unit,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_difftime <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(x$terms, info = info)

  if (is.null(x$time)) {
    rlang::abort("`time` argument must be set.")
  }

  date_data <- info[info$variable %in% col_names, ]
  if (any(date_data$type != "date"))
    rlang::abort(
      paste0("All variables for `step_date` should be either `Date` or",
             "`POSIXct` classes."
      )
    )
  step_difftime_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    time = x$time,
    tz = x$tz,
    unit = x$unit,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_difftime <- function(object, new_data, ...) {
  col_names <- object$columns
  # for backward compat

  for (i in seq_along(col_names)) {
      new_data[, col_names[i]] <-
        as.numeric(
          difftime(new_data[[ col_names[i] ]],
                   object$time,
                   object$tz,
                   object$unit)
        )
  }
  as_tibble(new_data)
}

#' @export
print.step_difftime <-
  function(x, width = max(20, options()$width - 31), ...) {
    msg <- ifelse(x$signed, "Signed difftime ", "difftime ")
    cat(msg, "transformation on ", sep = "")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @param x A `step_difftime` object.
#' @export
tidy.step_difftime <- function(x, ...) {
  out <- data.frame(time = x$time,
                    unit = x$unit,
                    id = x$id)
  out
}
