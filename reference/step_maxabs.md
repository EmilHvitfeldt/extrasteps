# Perform Max Abs Scaling

`step_maxabs()` creates a *specification* of a recipe step that will
perform Max Abs scaling.

## Usage

``` r
step_maxabs(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("maxabs")
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

- res:

  A list containing absolute max of training variables is stored here
  once this preprocessing step has be trained by
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html).

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
  step_maxabs(all_predictors()) %>%
  prep()

rec %>%
  bake(new_data = NULL)
#> # A tibble: 32 × 11
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 0.619  0.75 0.339 0.328 0.791 0.483 0.719     0     1   0.8 0.5  
#>  2 0.619  0.75 0.339 0.328 0.791 0.530 0.743     0     1   0.8 0.5  
#>  3 0.673  0.5  0.229 0.278 0.781 0.428 0.813     1     1   0.8 0.125
#>  4 0.631  0.75 0.547 0.328 0.625 0.593 0.849     1     0   0.6 0.125
#>  5 0.552  1    0.763 0.522 0.639 0.634 0.743     0     0   0.6 0.25 
#>  6 0.534  0.75 0.477 0.313 0.560 0.638 0.883     1     0   0.6 0.125
#>  7 0.422  1    0.763 0.731 0.651 0.658 0.692     0     0   0.6 0.5  
#>  8 0.720  0.5  0.311 0.185 0.748 0.588 0.873     1     0   0.8 0.25 
#>  9 0.673  0.5  0.298 0.284 0.795 0.581 1         1     0   0.8 0.25 
#> 10 0.566  0.75 0.355 0.367 0.795 0.634 0.799     1     0   0.8 0.5  
#> # ℹ 22 more rows

tidy(rec, 1)
#> # A tibble: 11 × 4
#>    terms statistic  value id          
#>    <chr> <chr>      <dbl> <chr>       
#>  1 mpg   max        33.9  maxabs_vjF0H
#>  2 cyl   max         8    maxabs_vjF0H
#>  3 disp  max       472    maxabs_vjF0H
#>  4 hp    max       335    maxabs_vjF0H
#>  5 drat  max         4.93 maxabs_vjF0H
#>  6 wt    max         5.42 maxabs_vjF0H
#>  7 qsec  max        22.9  maxabs_vjF0H
#>  8 vs    max         1    maxabs_vjF0H
#>  9 am    max         1    maxabs_vjF0H
#> 10 gear  max         5    maxabs_vjF0H
#> 11 carb  max         8    maxabs_vjF0H
```
