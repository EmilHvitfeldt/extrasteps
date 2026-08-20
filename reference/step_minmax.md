# Perform Min Max Scaling

`step_minmax()` creates a *specification* of a recipe step that will
perform Min Max scaling.

## Usage

``` r
step_minmax(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("minmax")
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

  A list containing min and max of training variables is stored here
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
  step_minmax(all_predictors()) %>%
  prep()

rec %>%
  bake(new_data = NULL)
#> # A tibble: 32 × 11
#>      mpg   cyl   disp     hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 0.451   0.5 0.222  0.205  0.525 0.283 0.233     0     1   0.5 0.429
#>  2 0.451   0.5 0.222  0.205  0.525 0.348 0.3       0     1   0.5 0.429
#>  3 0.528   0   0.0920 0.145  0.502 0.206 0.489     1     1   0.5 0    
#>  4 0.468   0.5 0.466  0.205  0.147 0.435 0.588     1     0   0   0    
#>  5 0.353   1   0.721  0.435  0.180 0.493 0.3       0     0   0   0.143
#>  6 0.328   0.5 0.384  0.187  0     0.498 0.681     1     0   0   0    
#>  7 0.166   1   0.721  0.682  0.207 0.526 0.160     0     0   0   0.429
#>  8 0.596   0   0.189  0.0353 0.429 0.429 0.655     1     0   0.5 0.143
#>  9 0.528   0   0.174  0.152  0.535 0.419 1         1     0   0.5 0.143
#> 10 0.374   0.5 0.241  0.251  0.535 0.493 0.452     1     0   0.5 0.429
#> # ℹ 22 more rows

tidy(rec, 1)
#> # A tibble: 22 × 4
#>    terms statistic value id          
#>    <chr> <chr>     <dbl> <chr>       
#>  1 mpg   min       10.4  minmax_qZ3vE
#>  2 cyl   min        4    minmax_qZ3vE
#>  3 disp  min       71.1  minmax_qZ3vE
#>  4 hp    min       52    minmax_qZ3vE
#>  5 drat  min        2.76 minmax_qZ3vE
#>  6 wt    min        1.51 minmax_qZ3vE
#>  7 qsec  min       14.5  minmax_qZ3vE
#>  8 vs    min        0    minmax_qZ3vE
#>  9 am    min        0    minmax_qZ3vE
#> 10 gear  min        3    minmax_qZ3vE
#> # ℹ 12 more rows
```
