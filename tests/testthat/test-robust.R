library(testthat)
library(rlang)
library(recipes)

library(modeldata)
data(biomass)

lower <- vapply(biomass[, 3:7], quantile, c(min = 0), prob = 0.25, na.rm = TRUE)
medians <- vapply(biomass[, 3:7], quantile, c(max = 0), prob = 0.5, na.rm = TRUE)
higher <- vapply(biomass[, 3:7], quantile, c(min = 0), prob = 0.75, na.rm = TRUE)

rec <- recipe(~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass
)

test_that("robust works", {
  standardized <- rec %>%
    step_robust(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "") %>%
    prep() %>%
    bake(new_data = NULL)

  exp_ref <- (t(biomass[, 3:7]) - medians) / (higher - lower)

  expect_equal(as.matrix(standardized), t(exp_ref))
})

test_that("range argument works", {
  lower01 <- vapply(biomass[, 3:7], quantile, c(min = 0), prob = 0.1, na.rm = TRUE)
  higher09 <- vapply(biomass[, 3:7], quantile, c(min = 0), prob = 0.9, na.rm = TRUE)


  standardized <- rec %>%
    step_robust(carbon, hydrogen, oxygen, nitrogen, sulfur,
                range = c(0.1, 0.9)) %>%
    prep() %>%
    bake(new_data = NULL)

  exp_ref <- (t(biomass[, 3:7]) - medians) / (higher09 - lower01)

  expect_equal(as.matrix(standardized), t(exp_ref))
})

test_that("correct min and max", {
  standardized <- rec %>%
    step_robust(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "")

  vrs <- c("carbon", "hydrogen", "oxygen", "nitrogen", "sulfur")
  norm_tibble_un <-
    tibble(
      terms = vrs,
      statistic = rep(na_chr, 5),
      value = rep(na_dbl, 5),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized, 1), norm_tibble_un)

  standardized_trained <- prep(standardized, training = biomass)

  norm_tibble_tr <-
    tibble(
      terms = rep(vrs, each = 3),
      statistic = rep(c("lower", "median", "higher"), times = 5),
      value = as.numeric(matrix(c(lower, medians, higher), nrow = 3, byrow = TRUE)),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized_trained, 1), norm_tibble_tr)
})

# Infrastructure ---------------------------------------------------------------

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_robust(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_robust(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_robust(rec)

  expect <- tibble(
    terms = character(),
    statistic = character(),
    value = double(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  rec <- recipe(~., data = mtcars) %>%
    step_robust(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
