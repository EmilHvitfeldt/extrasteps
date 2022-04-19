library(testthat)
library(rlang)
library(recipes)

library(modeldata)
data(biomass)

biomass_l1 <- biomass[, 3:7] / rowSums(abs(biomass[, 3:7]))
biomass_l2 <- biomass[, 3:7] / sqrt(rowSums(biomass[, 3:7] ^ 2))
biomass_max <- biomass[, 3:7] / apply(abs(biomass[, 3:7]), 1, max)

rec <- recipe(~ carbon + hydrogen + oxygen + nitrogen + sulfur,
              data = biomass
)

test_that("robust works", {
  standardized_l1 <- rec %>%
    step_unit_normalize(carbon, hydrogen, oxygen, nitrogen, sulfur,
                        norm = "l1") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(standardized_l1, as_tibble(biomass_l1))

  standardized_l2 <- rec %>%
    step_unit_normalize(carbon, hydrogen, oxygen, nitrogen, sulfur,
                        norm = "l2") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(standardized_l2, as_tibble(biomass_l2))

  standardized_max <- rec %>%
    step_unit_normalize(carbon, hydrogen, oxygen, nitrogen, sulfur,
                        norm = "max") %>%
    prep() %>%
    bake(new_data = NULL)

  expect_equal(standardized_max, as_tibble(biomass_max))
})

test_that("normalize - empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_unit_normalize(rec1)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked2)
})

test_that("normalize - empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_unit_normalize(rec)

  expect <- tibble(
    terms = character(),
    id = character()
  )

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("normalize - empty printing", {
  skip_if(packageVersion("rlang") < "1.0.0")
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_unit_normalize(rec)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
