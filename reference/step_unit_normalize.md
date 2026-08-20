# Perform Unit Normalization

`step_unit_normalize()` creates a *specification* of a recipe step that
will perform unit normalization by scaling individual samples to have
unit norm.

## Usage

``` r
step_unit_normalize(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  norm = c("l2", "l1", "max"),
  columns = NULL,
  skip = FALSE,
  id = rand_id("unit_normalize")
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

- norm:

  Character denoting which type of normalization to perform. Must be one
  of `"l1"`, `"l2"`, or "`"max"`.

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

rec <- recipe(~., data = mtcars) %>%
  step_unit_normalize(all_predictors()) %>%
  prep()

rec %>%
  bake(new_data = NULL)
#> # A tibble: 32 × 11
#>       mpg    cyl  disp    hp    drat      wt   qsec      vs      am    gear
#>     <dbl>  <dbl> <dbl> <dbl>   <dbl>   <dbl>  <dbl>   <dbl>   <dbl>   <dbl>
#>  1 0.107  0.0306 0.815 0.561 0.0199  0.0134  0.0839 0       0.00510 0.0204 
#>  2 0.107  0.0306 0.815 0.560 0.0199  0.0146  0.0867 0       0.00509 0.0204 
#>  3 0.156  0.0274 0.741 0.638 0.0264  0.0159  0.128  0.00686 0.00686 0.0274 
#>  4 0.0759 0.0213 0.915 0.390 0.0109  0.0114  0.0689 0.00355 0       0.0106 
#>  5 0.0466 0.0199 0.897 0.436 0.00785 0.00857 0.0424 0       0       0.00748
#>  6 0.0724 0.0240 0.900 0.420 0.0110  0.0138  0.0809 0.00400 0       0.0120 
#>  7 0.0328 0.0183 0.825 0.562 0.00736 0.00819 0.0363 0       0       0.00688
#>  8 0.150  0.0246 0.903 0.381 0.0227  0.0196  0.123  0.00615 0       0.0246 
#>  9 0.132  0.0231 0.814 0.549 0.0226  0.0182  0.132  0.00578 0       0.0231 
#> 10 0.0915 0.0286 0.799 0.586 0.0187  0.0164  0.0872 0.00477 0       0.0191 
#> # ℹ 22 more rows
#> # ℹ 1 more variable: carb <dbl>

tidy(rec, 1)
#> # A tibble: 11 × 2
#>    terms id                  
#>    <chr> <chr>               
#>  1 mpg   unit_normalize_BBVkz
#>  2 cyl   unit_normalize_BBVkz
#>  3 disp  unit_normalize_BBVkz
#>  4 hp    unit_normalize_BBVkz
#>  5 drat  unit_normalize_BBVkz
#>  6 wt    unit_normalize_BBVkz
#>  7 qsec  unit_normalize_BBVkz
#>  8 vs    unit_normalize_BBVkz
#>  9 am    unit_normalize_BBVkz
#> 10 gear  unit_normalize_BBVkz
#> 11 carb  unit_normalize_BBVkz
```
