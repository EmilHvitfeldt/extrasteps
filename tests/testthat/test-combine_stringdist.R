library(recipes)

data0 <- tibble(
 x1 = c("a", "b", "d", "e", "sfgsfgsd", "hjhgfgjgr"),
 x2 = c("ak", "b", "djj", "e", "hjhgfgjgr", "hjhgfgjgr")
)

rec <- recipe(~., data = data0) %>%
  step_combine_stringdist(all_predictors(), distance = 1) %>%
  prep()

test_that("printing", {
  rec <- recipe(~., data = data0) %>%
    step_combine_stringdist(all_predictors(), distance = 1)

  expect_snapshot(rec)
  expect_snapshot(prep(rec))
})

# Infrastructure ---------------------------------------------------------------
