library(testthat)
library(recipes)

test_that("printing", {
  example_date <- data.frame(
    dates = seq(as.Date("2010/1/1"), as.Date("2016/1/1"), by = "quarter")
  )

  rec <- recipe(~., data = example_date) %>%
    step_difftime(dates, time = as.Date("2010/1/1"))
  expect_snapshot(rec)
  expect_snapshot(prep(rec))
})


test_that("empty selection prep/bake is a no-op", {
  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_difftime(rec1, time = as.Date("2010/1/1"))

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_difftime(rec, time = as.Date("2010/1/1"))

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), id = character())
  )
})

test_that("empty printing", {
  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_difftime(rec, time = as.Date("2010/1/1"))

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
