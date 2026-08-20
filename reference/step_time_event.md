# Indicate Recurrent Date Time Event

`step_time_event()` creates a *specification* of a recipe step that will
create new columns indicating if the date fall on recurrent event.

## Usage

``` r
step_time_event(
  recipe,
  ...,
  role = "predictor",
  trained = FALSE,
  rules = list(),
  columns = NULL,
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("time_event")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  Not used by this step since no new variables are created.

- trained:

  A logical to indicate if the quantities for preprocessing have been
  estimated.

- rules:

  Named list of `almanac` rules.

- columns:

  A character string of variables that will be used as inputs. This
  field is a placeholder and will be populated once
  [`recipes::prep.recipe()`](https://recipes.tidymodels.org/reference/prep.html)
  is used.

- keep_original_cols:

  A logical to keep the original variables in the output. Defaults to
  `FALSE`.

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

An updated version of `recipe` with the new check added to the sequence
of any existing operations.

## Details

Unlike some other steps `step_time_event` does *not* remove the original
date variables by default. Set `keep_original_cols` to `FALSE` to remove
them.

## Examples

``` r
library(recipes)
library(extrasteps)
library(almanac)
library(modeldata)

data(Chicago)

on_easter <- yearly() %>% recur_on_easter()
on_weekend <- weekly() %>% recur_on_weekends()

rules <- list(easter = on_easter, weekend = on_weekend)

rec_spec <- recipe(ridership ~ date, data = Chicago) %>%
  step_time_event(date, rules = rules)

rec_spec_preped <- prep(rec_spec)

bake(rec_spec_preped, new_data = NULL)
#> # A tibble: 5,698 × 3
#>    ridership date_easter date_weekend
#>        <dbl>       <int>        <int>
#>  1     15.7            0            0
#>  2     15.8            0            0
#>  3     15.9            0            0
#>  4     15.9            0            0
#>  5     15.4            0            0
#>  6      2.42           0            1
#>  7      1.47           0            1
#>  8     15.5            0            0
#>  9     15.9            0            0
#> 10     15.9            0            0
#> # ℹ 5,688 more rows
```
