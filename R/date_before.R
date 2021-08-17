#' Time before Recurrent Date Time Event
#'
#' `step_date_before` creates a *specification* of a recipe
#'  step that will create new columns indicating the time before an
#'  recurrent event.
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
#'   step_date_before(date, rules = rules)
#'
#' rec_spec_preped <- prep(rec_spec)
#'
#' bake(rec_spec_preped, new_data = NULL)
step_date_before <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           rules = list(),
           columns = NULL,
           skip = FALSE,
           id = rand_id("date_before")) {

    add_step(
      recipe,
      step_date_before_new(
        terms = ellipse_check(...),
        trained = trained,
        role = role,
        rules = rules,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_date_before_new <-
  function(terms, role, trained, rules, columns, skip, id) {
    step(
      subclass = "date_before",
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
prep.step_date_before <- function(x, training, info = NULL, ...) {
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

  if (!all(purrr::map_lgl(x$rules, inherits, "rschedule"))) {
    rlang::abort(
      "All `rules` must be `rschedule`s from {almanac}"
    )
  }

  step_date_before_new(
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
bake.step_date_before <- function(object, new_data, ...) {

  new_columns <- purrr::imap(object$columns, date_before_helper,
                             new_data, object$rules)

  new_data <- dplyr::bind_cols(new_data, new_columns)
  new_data <- dplyr::select(new_data, -names(object$columns))
  new_data
}

date_before_helper <- function(columnn, name, new_data, rule) {
  res <- purrr::map_dfc(rule, ~ {
    as.numeric(alma_next(new_data[[columnn]], .x, inclusive = TRUE) - new_data[[columnn]])
    }
  )

  names(res) <- paste(name, names(res), sep = "_")
  res
}

print.step_date_before <-
  function(x, width = max(20, options()$width - 35), ...) {
    cat("Time events from ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_date_before
#' @param x A `step_date_before` object.
#' @export
tidy.step_date_before <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = names(x$objects), rules = x$rules)
  } else {
    term_names <- sel2char(x$terms)
    res <- tibble(terms = term_names, rules = x$rules)
  }
  res$id <- x$id
  res
}
