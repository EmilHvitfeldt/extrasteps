library(testthat)
library(recipes)
library(almanac)

examples <- tibble(
  date1 = as.Date("2021-08-16") + 1:20,
  date2 = as.Date("2020-08-16") + 1:20,
  numeric = 1:20
)

before_fun <- function(date, rule) {
  as.numeric(alma_next(date, rule, inclusive = TRUE) - date)
}

test_that("time_event works", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)
  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(names(res), c("date1_before_weekend", "date1_before_weekday"))

  expect_equal(
    before_fun(examples$date1, on_weekdays),
    res$date1_before_weekday
  )
  expect_equal(
    before_fun(examples$date1, on_weekends),
    res$date1_before_weekend
  )
})

test_that("time_event works with multiple columns", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)
  rec_spec <- recipe(~ date1 + date2, data = examples) %>%
    step_date_before(date1, date2, rules = rules) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(names(res), c("date1_before_weekend", "date1_before_weekday",
                             "date2_before_weekend", "date2_before_weekday"))

  expect_equal(
    before_fun(examples$date1, on_weekdays),
    res$date1_before_weekday
  )
  expect_equal(
    before_fun(examples$date1, on_weekends),
    res$date1_before_weekend
  )
  expect_equal(
    before_fun(examples$date2, on_weekdays),
    res$date2_before_weekday
  )
  expect_equal(
    before_fun(examples$date2, on_weekends),
    res$date2_before_weekend
  )
})


test_that("time_event works with transform", {
  on_weekends <- weekly() %>% recur_on_weekends()

  rules <- list(weekend = on_weekends)

  # Inverse
  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules, transform = "inverse") %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    1 / (before_fun(examples$date1, on_weekends) + 0.5),
    res$date1_before_weekend
  )

  # exp
  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules, transform = "exp") %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    exp(before_fun(examples$date1, on_weekends)),
    res$date1_before_weekend
  )

  # log
  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules, transform = "log") %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    log(before_fun(examples$date1, on_weekends) + 0.5),
    res$date1_before_weekend
  )

  # custom
  custom_fun <- function(x) x / 2

  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules, transform = custom_fun) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    custom_fun(before_fun(examples$date1, on_weekends)),
    res$date1_before_weekend
  )
})

test_that("time_event errors", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  expect_error(
    recipe(~ ., data = examples) %>%
      step_date_before(numeric, rules = rules) %>%
      prep()
  )

  expect_error(
    recipe(~ ., data = examples) %>%
      step_date_before(date1, rules = "wrong") %>%
      prep()
  )

  expect_error(
    recipe(~ ., data = examples) %>%
      step_date_before(date1, rules = list(weekend = on_weekends, "Hello")) %>%
      prep()
  )

  expect_error(
    recipe(~ ., data = examples) %>%
      step_date_before(date1, rules = list(weekend = on_weekends,
                                            christmas = "2020-12-25")) %>%
      prep()
  )

})

test_that("check_name() is used", {
  dat <- examples
  dat$date1_before_weekend <- dat$date1

  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  rec <- recipe(~., data = dat) |>
    step_date_before(date1, rules = rules)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

test_that("printing", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)
  rec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules)

  expect_snapshot(rec)
  expect_snapshot(prep(rec))
})

test_that("empty selection prep/bake is a no-op", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  rec1 <- recipe(mpg ~ ., mtcars)
  rec2 <- step_date_before(rec1, rules = rules)

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
  rec <- step_date_before(rec, rules = rules)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), rules = list(), id = character())
  )

  rec <- prep(rec, mtcars)

  expect_identical(
    tidy(rec, number = 1),
    tibble(terms = character(), rules = list(), id = character())
  )
})

test_that("empty printing", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  rec <- recipe(mpg ~ ., mtcars)
  rec <- step_date_before(rec, rules = rules)

  expect_snapshot(rec)

  rec <- prep(rec, mtcars)

  expect_snapshot(rec)
})
