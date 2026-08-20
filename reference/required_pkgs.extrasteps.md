# S3 methods for tracking which additional packages are needed for steps.

Recipe-adjacent packages always list themselves as a required package so
that the steps can function properly within parallel processing schemes.

## Usage

``` r
# S3 method for class 'step_date_after'
required_pkgs(x, ...)

# S3 method for class 'step_date_before'
required_pkgs(x, ...)

# S3 method for class 'step_date_nearest'
required_pkgs(x, ...)

# S3 method for class 'step_difftime'
required_pkgs(x, ...)

# S3 method for class 'step_encoding_binary'
required_pkgs(x, ...)

# S3 method for class 'step_encoding_frequency'
required_pkgs(x, ...)

# S3 method for class 'step_maxabs'
required_pkgs(x, ...)

# S3 method for class 'step_minmax'
required_pkgs(x, ...)

# S3 method for class 'step_robust'
required_pkgs(x, ...)

# S3 method for class 'step_time_event'
required_pkgs(x, ...)

# S3 method for class 'step_unit_normalize'
required_pkgs(x, ...)
```

## Arguments

- x:

  A recipe step

## Value

A character vector
