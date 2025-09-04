library(testthat)
library(recipes)
library(almanac)

examples <- tibble(
  date1 = as.Date("2021-08-16") + 1:20,
  date2 = as.Date("2020-08-16") + 1:20,
  numeric = 1:20
)

test_that("time_event works", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)
  rec_spec <- recipe(~date1, data = examples) %>%
    step_time_event(date1, rules = rules) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(names(res), c("date1_weekend", "date1_weekday"))

  expect_equal(
    vctrs::vec_cast(alma_in(examples$date1, on_weekdays), integer()),
    res$date1_weekday
  )
  expect_equal(
    vctrs::vec_cast(alma_in(examples$date1, on_weekends), integer()),
    res$date1_weekend
  )
})

test_that("time_event works with multiple columns", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)
  rec_spec <- recipe(~ date1 + date2, data = examples) %>%
    step_time_event(date1, date2, rules = rules) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    names(res),
    c("date1_weekend", "date1_weekday", "date2_weekend", "date2_weekday")
  )

  expect_equal(
    vctrs::vec_cast(alma_in(examples$date1, on_weekdays), integer()),
    res$date1_weekday
  )
  expect_equal(
    vctrs::vec_cast(alma_in(examples$date1, on_weekends), integer()),
    res$date1_weekend
  )
  expect_equal(
    vctrs::vec_cast(alma_in(examples$date2, on_weekdays), integer()),
    res$date2_weekday
  )
  expect_equal(
    vctrs::vec_cast(alma_in(examples$date2, on_weekends), integer()),
    res$date2_weekend
  )
})

test_that("time_event errors", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  expect_snapshot(
    error = TRUE,
    recipe(~., data = examples) %>%
      step_time_event(numeric, rules = rules) %>%
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = examples) %>%
      step_time_event(date1, rules = "wrong") %>%
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = examples) %>%
      step_time_event(date1, rules = list(weekend = on_weekends, "Hello")) %>%
      prep()
  )

  expect_snapshot(
    error = TRUE,
    recipe(~., data = examples) %>%
      step_time_event(
        date1,
        rules = list(weekend = on_weekends, christmas = "2020-12-25")
      ) %>%
      prep()
  )
})

test_that("check_name() is used", {
  dat <- examples
  dat$date1_weekend <- dat$date1

  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  rec <- recipe(~., data = dat) |>
    step_time_event(date1, rules = rules)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------

test_that("empty printing", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_time_event(rec, rules = rules)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})

test_that("empty selection prep/bake is a no-op", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_time_event(rec1, rules = rules)

  rec1 <- prep(rec1, mtcars)
  rec2 <- prep(rec2, mtcars)

  baked1 <- bake(rec1, mtcars)
  baked2 <- bake(rec2, mtcars)

  expect_identical(baked1, baked1)
})

test_that("empty selection tidy method works", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_time_event(rec, rules = rules)

  expect <- tibble(terms = character(), rules = list(), id = character())

  expect_identical(tidy(rec, number = 1), expect)

  rec <- prep(rec, mtcars)

  expect_identical(tidy(rec, number = 1), expect)
})

test_that("printing", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)
  rec <- recipe(~date1, data = examples) %>%
    step_time_event(date1, rules = rules)

  expect_snapshot(print(rec))
  expect_snapshot(prep(rec))
})
