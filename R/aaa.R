# Used in
# - step_date_after
# - step_date_before
# - step_date_nearest
date_transforms <- list(
  identity = identity,
  inverse = function(x) 1 / (x + 0.5),
  exp = exp,
  log = function(x) log(x + 0.5)
)

fetch_date_transforms <- function(x) {
  if (is.function(x)) {
    return(x)
  }
  if (!x %in% names(date_transforms)) {
    rlang::abort(
      paste("`transform` must be a function or one of built-in names.",
            "See `?step_date_nearest` for valid input.")
    )
  }
  date_transforms[[x]]
}
