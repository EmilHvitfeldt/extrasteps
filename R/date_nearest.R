#' Time to Nearest Recurrent Date Time Event
#'
#' `step_date_nearest()` creates a *specification* of a recipe step that will
#' create new columns indicating the time to nearest recurrent event.
#'
#' @inheritParams recipes::step_center
#' @inheritParams step_time_event
#' @param rules Named list of `almanac` rules.
#' @param transform A function or character indication a function used oon the
#'  resulting variables. See details for allowed names and their functions.
#' @param columns A character string of variables that will be
#'  used as inputs. This field is a placeholder and will be
#'  populated once [recipes::prep.recipe()] is used.
#' @return An updated version of `recipe` with the new check added to the
#'  sequence of any existing operations.
#' @export
#'
#' @details
#'
#' The `transform` argument can be function that takes a numeric vector and
#' returns a numeric vector of the same length. It can also be a character
#' vector, below is the supported vector names. Some functions come with offset
#' to avoid `Inf`.
#'
#' ```r
#' "identity"
#' function(x) x
#'
#' "inverse"
#' function(x) 1 / (x + 0.5)
#'
#' "exp"
#' function(x) exp(x)
#'
#' "log"
#' function(x) log(x + 0.5)
#' ```
#'
#' The effect of `transform` is illustrated below.
#'
#'
#' ```{r date_nearest, echo=FALSE, message=FALSE}
#' library(almanac)
#' library(ggplot2)
#' library(dplyr)
#' library(recipes)
#'
#' examples <- tibble(
#' date1 = as.Date("2021-01-01") + 0:13
#' )
#'
#' mondays <- weekly() %>% recur_on_day_of_week("Monday")
#'
#' recipe(~., data = examples) %>%
#'   step_date_nearest(date1,
#'                    rules = list(monday = mondays),
#'                    transform = "identity") %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   bind_cols(examples) %>%
#'   ggplot(aes(date1, date1_nearest_monday)) +
#'   geom_col() +
#'   theme_minimal() +
#'   labs(title = "Days nearest Mondays in January 2021",
#'        subtitle = "Without transformation",
#'        y = NULL, x = NULL) +
#'   scale_x_date(date_breaks = "1 day", date_labels = "%d")
#'
#' recipe(~., data = examples) %>%
#'   step_date_nearest(date1,
#'                    rules = list(monday = mondays),
#'                    transform = "inverse") %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   bind_cols(examples) %>%
#'   ggplot(aes(date1, date1_nearest_monday)) +
#'   geom_col() +
#'   theme_minimal() +
#'   labs(title = "Days nearest Mondays in January 2021",
#'        subtitle = "With \"inverse\" transformation",
#'        y = NULL, x = NULL) +
#'   scale_x_date(date_breaks = "1 day", date_labels = "%d")
#'
#' recipe(~., data = examples) %>%
#'   step_date_nearest(date1,
#'                    rules = list(monday = mondays),
#'                    transform = "exp") %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   bind_cols(examples) %>%
#'   ggplot(aes(date1, date1_nearest_monday)) +
#'   geom_col() +
#'   theme_minimal() +
#'   labs(title = "Days nearest Mondays in January 2021",
#'        subtitle = "With \"exp\" transformation",
#'        y = NULL, x = NULL) +
#'   scale_x_date(date_breaks = "1 day", date_labels = "%d")
#'
#' recipe(~., data = examples) %>%
#'   step_date_nearest(date1,
#'                    rules = list(monday = mondays),
#'                    transform = "log") %>%
#'   prep() %>%
#'   bake(new_data = NULL) %>%
#'   bind_cols(examples) %>%
#'   ggplot(aes(date1, date1_nearest_monday)) +
#'   geom_col() +
#'   theme_minimal() +
#'   labs(title = "Days nearest Mondays in January 2021",
#'        subtitle = "With \"log\" transformation",
#'        y = NULL, x = NULL) +
#'   scale_x_date(date_breaks = "1 day", date_labels = "%d")
#' ```
#'
#' The naming of the resulting variables will be on the form
#'
#' ```r
#' {variable name}_nearest_{name of rule}
#' ```
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
#'   step_date_nearest(date, rules = rules)
#'
#' rec_spec_preped <- prep(rec_spec)
#'
#' bake(rec_spec_preped, new_data = NULL)
step_date_nearest <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    rules = list(),
    transform = "identity",
    columns = NULL,
    keep_original_cols = FALSE,
    skip = FALSE,
    id = rand_id("date_nearest")
  ) {
    add_step(
      recipe,
      step_date_nearest_new(
        terms = enquos(...),
        trained = trained,
        role = role,
        rules = rules,
        transform = transform,
        columns = columns,
        keep_original_cols = keep_original_cols,
        skip = skip,
        id = id
      )
    )
  }

step_date_nearest_new <-
  function(
    terms,
    role,
    trained,
    rules,
    transform,
    columns,
    keep_original_cols,
    skip,
    id
  ) {
    step(
      subclass = "date_nearest",
      terms = terms,
      role = role,
      trained = trained,
      rules = rules,
      transform = transform,
      columns = columns,
      keep_original_cols = keep_original_cols,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_date_nearest <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  date_data <- info[info$variable %in% col_names, ]
  if (any(date_data$type != "date")) {
    rlang::abort(
      paste0(
        "All variables for `step_date` should be either `Date` or",
        "`POSIXct` classes."
      )
    )
  }

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

  step_date_nearest_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    rules = x$rules,
    transform = x$transform,
    columns = col_names,
    keep_original_cols = get_keep_original_cols(x),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_date_nearest <- function(object, new_data, ...) {
  col_names <- names(object$columns)
  check_new_data(col_names, object, new_data)

  if (length(col_names) == 0L) {
    # Empty selection
    return(new_data)
  }

  transform <- fetch_date_transforms(object$transform)

  new_cols <- purrr::imap_dfc(
    object$columns,
    date_nearest_helper,
    new_data,
    object$rules,
    transform
  )

  new_cols <- check_name(new_cols, new_data, object, names(new_cols))

  new_data <- dplyr::bind_cols(new_data, new_cols)
  new_data <- remove_original_cols(new_data, object, col_names)
  new_data
}

date_nearest_helper <- function(columnn, name, new_data, rule, transform) {
  res <- purrr::map_dfc(
    rule,
    ~ {
      values <- new_data[[columnn]]

      before <- alma_next(values, .x, inclusive = TRUE) - values
      after <- values - alma_previous(values, .x, inclusive = TRUE)
      res <- pmin(before, after)
      res <- as.integer(res)
      res <- transform(res)
      res
    }
  )

  names(res) <- paste(name, "nearest", names(res), sep = "_")
  res
}

#' @export
print.step_date_nearest <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Time events from "
    print_step(x$columns, x$terms, x$trained, width = width, title = title)
    invisible(x)
  }

#' @rdname step_date_nearest
#' @usage NULL
#' @export
tidy.step_date_nearest <- function(x, ...) {
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
required_pkgs.step_date_nearest <- function(x, ...) {
  c("extrasteps", "almanac")
}
