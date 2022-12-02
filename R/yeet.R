#' Removes date and datetimes variables
#'
#' `step_yeet` creates a *specification* of a recipe step
#'  that will remove date and datetime variables. (This is a meme)
#'
#' @inheritParams recipes::step_center
#' @param removals A character string that contains the names of
#'  columns that should be removed. These values are not determined
#'  until [prep.recipe()] is called.
#' @return An updated version of `recipe` with the new step added to the
#'  sequence of any existing operations.
#'
#' @export
#' @examples
#' library(recipes)
#' dat <- data.frame(
#'   dt = as.POSIXct("2012-01-15"),
#'   d = as.Date("2020-01-01"),
#'   n = 4,
#'   c = "h"
#' )
#'
#' recipe(~., data = dat) %>%
#'   step_yeet() %>%
#'   prep() %>%
#'   bake(new_data = NULL)
step_yeet <- function(recipe,
                    ...,
                    role = NA,
                    trained = FALSE,
                    removals = NULL,
                    skip = FALSE,
                    id = rand_id("yeet")) {
  add_step(
    recipe,
    step_yeet_new(
      terms = ellipse_check(has_type("date")),
      role = role,
      trained = trained,
      removals = removals,
      skip = skip,
      id = id
    )
  )
}

step_yeet_new <- function(terms, role, trained, removals, skip, id) {
  step(
    subclass = "yeet",
    terms = terms,
    role = role,
    trained = trained,
    removals = removals,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_yeet <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)

  step_yeet_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    removals = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_yeet <- function(object, new_data, ...) {
  if (length(object$removals) > 0) {
    new_data <- new_data[, !(colnames(new_data) %in% object$removals)]
  }
  new_data
}

#' @export
print.step_yeet <-
  function(x, width = max(20, options()$width - 22), ...) {
    if (x$trained) {
      if (length(x$removals) > 0) {
        cat("Variables removed ")
        cat(format_ch_vec(x$removals, width = width))
      } else {
        cat("No variables were removed")
      }
    } else {
      cat("Delete terms ", sep = "")
      cat(format_selectors(x$terms, width = width))
    }
    if (x$trained) {
      cat(" [trained]\n")
    } else {
      cat("\n")
    }
    invisible(x)
  }


#' @rdname tidy.recipe
#' @param x A `step_yeet` object.
#' @export
tidy.step_yeet <- function (x, ...) {
  if (is_trained(x)) {
    res <- tibble::tibble(terms = x$removals)
  }
  else {
    term_names <- recipes::sel2char(x$terms)
    res <- tibble::tibble(terms = na_chr)
  }
  res$id <- x$id
  res
}

#' @rdname required_pkgs.extrasteps
#' @export
required_pkgs.step_yeet <- function(x, ...) {
  c("extrasteps")
}
