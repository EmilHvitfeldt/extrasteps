library(testthat)
library(rlang)
library(recipes)

library(modeldata)
data(biomass)

maxs <- abs(vapply(biomass[, 3:7], max, c(max = 0), na.rm = TRUE))

rec <- recipe(~ carbon + hydrogen + oxygen + nitrogen + sulfur, data = biomass)

test_that("maxabs works", {
  standardized <- rec %>%
    step_maxabs(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "") %>%
    prep() %>%
    bake(new_data = NULL)

  exp_ref <- (t(biomass[, 3:7])) / (maxs)

  expect_equal(as.matrix(standardized), t(exp_ref))
})

test_that("correct min and max", {
  standardized <- rec %>%
    step_maxabs(carbon, hydrogen, oxygen, nitrogen, sulfur, id = "")

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
      terms = vrs,
      statistic = rep("max", each = 5),
      value = unname(maxs),
      id = standardized$steps[[1]]$id
    )

  expect_equal(tidy(standardized_trained, 1), norm_tibble_tr)
})

# Infrastructure ---------------------------------------------------------------

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_maxabs(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_maxabs(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_maxabs(rec)

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
    step_maxabs(all_predictors())

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
