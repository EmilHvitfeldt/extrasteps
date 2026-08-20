# Perform Robust Scaling

`step_robust()` creates a *specification* of a recipe step that will
perform Robust scaling.

## Usage

``` r
step_robust(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  range = c(0.25, 0.75),
  res = NULL,
  columns = NULL,
  skip = FALSE,
  id = rand_id("robust")
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

- range:

  A numeric vector with 2 values denoting the lower and upper quantile
  that is used for scaling. Defaults to `c(0.25, 0.75)`.

- res:

  A list containing the 3 quantiles of training variables is stored here
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

## Details

The scaling performed by this step is done using the following
transformation

\$\$x_new = (x - Q2(x)) / (Q3(x) - Q1(x))\$\$

where `Q2(x)` is the median, `Q3(x)` is the upper quantile (defaults to
0.75) and `Q1(x)` is the lower quantile (defaults to 0.25). The upper
and lower quantiles can be changed with the `range` argument.

## Examples

``` r
library(recipes)

rec <- recipe(~., data = mtcars) %>%
  step_robust(all_predictors()) %>%
  prep()

rec %>%
  bake(new_data = NULL)
#> # A tibble: 32 × 11
#>        mpg   cyl   disp     hp     drat     wt   qsec    vs    am  gear  carb
#>      <dbl> <dbl>  <dbl>  <dbl>    <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  0.244    0   -0.177 -0.156  0.244   -0.685 -0.623     0     1     0   1  
#>  2  0.244    0   -0.177 -0.156  0.244   -0.437 -0.344     0     1     0   1  
#>  3  0.488   -0.5 -0.430 -0.359  0.185   -0.977  0.448     1     1     0  -0.5
#>  4  0.298    0    0.301 -0.156 -0.732   -0.107  0.862     1     0    -1  -0.5
#>  5 -0.0678   0.5  0.798  0.623 -0.649    0.112 -0.344     0     0    -1   0  
#>  6 -0.149    0    0.140 -0.216 -1.11     0.131  1.25      1     0    -1  -0.5
#>  7 -0.664    0.5  0.798  1.46  -0.577    0.238 -0.932     0     0    -1   1  
#>  8  0.705   -0.5 -0.242 -0.731 -0.00595 -0.131  1.14      1     0     0   0  
#>  9  0.488   -0.5 -0.271 -0.335  0.268   -0.170  2.59      1     0     0   0  
#> 10  0        0   -0.140  0      0.268    0.112  0.294     1     0     0   1  
#> # ℹ 22 more rows

tidy(rec, 1)
#> # A tibble: 33 × 4
#>    terms statistic value id          
#>    <chr> <chr>     <dbl> <chr>       
#>  1 mpg   lower      15.4 robust_afHYT
#>  2 mpg   median     19.2 robust_afHYT
#>  3 mpg   higher     22.8 robust_afHYT
#>  4 cyl   lower       4   robust_afHYT
#>  5 cyl   median      6   robust_afHYT
#>  6 cyl   higher      8   robust_afHYT
#>  7 disp  lower     121.  robust_afHYT
#>  8 disp  median    196.  robust_afHYT
#>  9 disp  higher    326   robust_afHYT
#> 10 hp    lower      96.5 robust_afHYT
#> # ℹ 23 more rows

rec <- recipe(~., data = mtcars) %>%
  step_robust(all_predictors(), range = c(0.1, 0.9)) %>%
  prep()

rec %>%
  bake(new_data = NULL)
#> # A tibble: 32 × 11
#>        mpg   cyl    disp      hp     drat      wt   qsec    vs    am  gear
#>      <dbl> <dbl>   <dbl>   <dbl>    <dbl>   <dbl>  <dbl> <dbl> <dbl> <dbl>
#>  1  0.114    0   -0.115  -0.0732  0.171   -0.337  -0.281     0     1   0  
#>  2  0.114    0   -0.115  -0.0732  0.171   -0.215  -0.155     0     1   0  
#>  3  0.229   -0.5 -0.280  -0.169   0.129   -0.480   0.202     1     1   0  
#>  4  0.140    0    0.196  -0.0732 -0.512   -0.0526  0.388     1     0  -0.5
#>  5 -0.0317   0.5  0.519   0.293  -0.453    0.0550 -0.155     0     0  -0.5
#>  6 -0.0698   0    0.0910 -0.101  -0.778    0.0645  0.563     1     0  -0.5
#>  7 -0.311    0.5  0.519   0.687  -0.403    0.117  -0.420     0     0  -0.5
#>  8  0.330   -0.5 -0.157  -0.344  -0.00416 -0.0645  0.514     1     0   0  
#>  9  0.229   -0.5 -0.176  -0.158   0.187   -0.0837  1.16      1     0   0  
#> 10  0        0   -0.0910  0       0.187    0.0550  0.132     1     0   0  
#> # ℹ 22 more rows
#> # ℹ 1 more variable: carb <dbl>

tidy(rec, 1)
#> # A tibble: 33 × 4
#>    terms statistic value id          
#>    <chr> <chr>     <dbl> <chr>       
#>  1 mpg   lower      14.3 robust_ayUXK
#>  2 mpg   median     19.2 robust_ayUXK
#>  3 mpg   higher     30.1 robust_ayUXK
#>  4 cyl   lower       4   robust_ayUXK
#>  5 cyl   median      6   robust_ayUXK
#>  6 cyl   higher      8   robust_ayUXK
#>  7 disp  lower      80.6 robust_ayUXK
#>  8 disp  median    196.  robust_ayUXK
#>  9 disp  higher    396   robust_ayUXK
#> 10 hp    lower      66   robust_ayUXK
#> # ℹ 23 more rows
```
