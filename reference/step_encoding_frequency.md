# Perform frequency encoding

`step_encoding_frequency()` creates a *specification* of a recipe step
that will perform frequency encoding.

## Usage

``` r
step_encoding_frequency(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("encoding_frequency")
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

  A list frequencies of the levels of the training variables is stored
  here once this preprocessing step has be trained by
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
library(modeldata)

data(ames)

rec <- recipe(~ Land_Contour + Neighborhood, data = ames) %>%
  step_encoding_frequency(all_nominal_predictors()) %>%
  prep()

rec %>%
  bake(new_data = NULL)
#> # A tibble: 2,930 × 2
#>    Land_Contour Neighborhood
#>           <dbl>        <dbl>
#>  1       0.899        0.151 
#>  2       0.899        0.151 
#>  3       0.899        0.151 
#>  4       0.899        0.151 
#>  5       0.899        0.0563
#>  6       0.899        0.0563
#>  7       0.899        0.0174
#>  8       0.0410       0.0174
#>  9       0.899        0.0174
#> 10       0.899        0.0563
#> # ℹ 2,920 more rows

tidy(rec, 1)
#> # A tibble: 33 × 4
#>    terms        level              frequency id                      
#>    <chr>        <chr>                  <dbl> <chr>                   
#>  1 Land_Contour Bnk                   0.0399 encoding_frequency_gF0fi
#>  2 Land_Contour HLS                   0.0410 encoding_frequency_gF0fi
#>  3 Land_Contour Low                   0.0205 encoding_frequency_gF0fi
#>  4 Land_Contour Lvl                   0.899  encoding_frequency_gF0fi
#>  5 Neighborhood North_Ames            0.151  encoding_frequency_gF0fi
#>  6 Neighborhood College_Creek         0.0911 encoding_frequency_gF0fi
#>  7 Neighborhood Old_Town              0.0816 encoding_frequency_gF0fi
#>  8 Neighborhood Edwards               0.0662 encoding_frequency_gF0fi
#>  9 Neighborhood Somerset              0.0621 encoding_frequency_gF0fi
#> 10 Neighborhood Northridge_Heights    0.0567 encoding_frequency_gF0fi
#> # ℹ 23 more rows
```
