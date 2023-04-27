library(recipes)

# Infrastructure ---------------------------------------------------------------

test_that("printing", {
  rec <- recipe(~., data = mtcars) %>%
    step_yeet()

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
