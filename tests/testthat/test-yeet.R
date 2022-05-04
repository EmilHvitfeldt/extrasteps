library(recipes)

test_that("printing", {
  rec <- recipe(~., data = mtcars) %>%
    step_yeet()
  expect_snapshot(rec)
  expect_snapshot(prep(rec))
})
