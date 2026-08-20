# Perform binary encoding of factor variables

`step_encoding_binary()` creates a *specification* of a recipe step that
will perform binary encoding of factor variables.

## Usage

``` r
step_encoding_binary(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  res = NULL,
  columns = NULL,
  keep_original_cols = FALSE,
  skip = FALSE,
  id = rand_id("encoding_binary")
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

  A list containing levels of training variables is stored here once
  this preprocessing step has be trained by
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html).

- columns:

  A character string of variable names that will be populated
  (eventually) by the `terms` argument.

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

An updated version of `recipe` with the new step added to the sequence
of existing steps (if any). For the `tidy` method, a tibble with columns
`terms` (the columns that will be affected) and `base`.

## Examples

``` r
library(recipes)
library(modeldata)

data(ames)

rec <- recipe(~ Land_Contour + Neighborhood, data = ames) %>%
  step_encoding_binary(all_nominal_predictors()) %>%
  prep()

rec %>%
  bake(new_data = NULL)
#> # A tibble: 2,930 × 9
#>    Land_Contour_1 Land_Contour_2 Land_Contour_4 Neighborhood_1 Neighborhood_2
#>             <int>          <int>          <int>          <int>          <int>
#>  1              0              0              1              1              0
#>  2              0              0              1              1              0
#>  3              0              0              1              1              0
#>  4              0              0              1              1              0
#>  5              0              0              1              1              1
#>  6              0              0              1              1              1
#>  7              0              0              1              1              0
#>  8              0              1              0              1              0
#>  9              0              0              1              1              0
#> 10              0              0              1              1              1
#> # ℹ 2,920 more rows
#> # ℹ 4 more variables: Neighborhood_4 <int>, Neighborhood_8 <int>,
#> #   Neighborhood_16 <int>, Neighborhood_32 <int>

tidy(rec, 1)
#> # A tibble: 2 × 3
#>   terms        value id                   
#>   <chr>        <int> <chr>                
#> 1 Land_Contour     4 encoding_binary_HIyvQ
#> 2 Neighborhood    29 encoding_binary_HIyvQ
```
