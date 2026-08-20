# difftimearithmic Transformation

`step_difftime()` creates a *specification* of a recipe step that will
calculate difftimes of the data.

## Usage

``` r
step_difftime(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  time = NULL,
  tz = NULL,
  unit = "auto",
  columns = NULL,
  skip = FALSE,
  id = rand_id("difftime")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose which variables are affected
  by the step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details. For the `tidy` method, these are not currently used.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- time:

  date-time or date objects. Used for reference. Must match the type of
  variable.

- tz:

  an optional time zone specification to be used for the conversion,
  mainly for "POSIXlt" objects.

- unit:

  character string. Units in which the results are desired. Must be one
  of "auto", "secs", "mins", "hours","days", and "weeks" Defaults to
  "auto".

- columns:

  A character string of variable names that will be populated
  (eventually) by the `terms` argument.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any). For the `tidy` method, a tibble with columns
`terms` (the columns that will be affected) and `base`.

## Examples

``` r
library(recipes)
example_date <- data.frame(
  dates = seq(as.Date("2010/1/1"), as.Date("2016/1/1"), by = "quarter")
 )

 example_datetime <- data.frame(
  datetimes = seq(ISOdate(1993,1,1), ISOdate(1993,1,2), by = "hour")
 )

rec <- recipe(~ dates, data = example_date) %>%
  step_difftime(dates, time = as.Date("2010/1/1"))

difftime_obj <- prep(rec)

bake(difftime_obj, new_data = NULL)
#> # A tibble: 25 × 1
#>       dates
#>       <dbl>
#>  1        0
#>  2  7776000
#>  3 15638400
#>  4 23587200
#>  5 31536000
#>  6 39312000
#>  7 47174400
#>  8 55123200
#>  9 63072000
#> 10 70934400
#> # ℹ 15 more rows

recipe(~ dates, data = example_date) %>%
  step_difftime(dates, time = as.Date("2010/1/1"), unit = "weeks") %>%
  prep() %>%
  bake(new_data = NULL)
#> # A tibble: 25 × 1
#>    dates
#>    <dbl>
#>  1   0  
#>  2  12.9
#>  3  25.9
#>  4  39  
#>  5  52.1
#>  6  65  
#>  7  78  
#>  8  91.1
#>  9 104. 
#> 10 117. 
#> # ℹ 15 more rows

recipe(~ datetimes, data = example_datetime) %>%
  step_difftime(datetimes, time = ISOdate(1993,1,1), unit = "secs") %>%
  prep() %>%
  bake(new_data = NULL)
#> # A tibble: 25 × 1
#>    datetimes
#>        <dbl>
#>  1         0
#>  2      3600
#>  3      7200
#>  4     10800
#>  5     14400
#>  6     18000
#>  7     21600
#>  8     25200
#>  9     28800
#> 10     32400
#> # ℹ 15 more rows
```
