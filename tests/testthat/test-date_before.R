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
  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(names(res), c("date1_before_weekend", "date1_before_weekday"))

  expect_equal(
    as.numeric(alma_next(examples$date1, on_weekdays, inclusive = TRUE) - examples$date1),
    res$date1_before_weekday
  )
  expect_equal(
    as.numeric(alma_next(examples$date1, on_weekends, inclusive = TRUE) - examples$date1),
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
    as.numeric(alma_next(examples$date1, on_weekdays, inclusive = TRUE) - examples$date1),
    res$date1_before_weekday
  )
  expect_equal(
    as.numeric(alma_next(examples$date1, on_weekends, inclusive = TRUE) - examples$date1),
    res$date1_before_weekend
  )
  expect_equal(
    as.numeric(alma_next(examples$date2, on_weekdays, inclusive = TRUE) - examples$date2),
    res$date2_before_weekday
  )
  expect_equal(
    as.numeric(alma_next(examples$date2, on_weekends, inclusive = TRUE) - examples$date2),
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
    1 / (as.numeric(alma_next(examples$date1, on_weekends, inclusive = TRUE) - examples$date1) + 0.5),
    res$date1_before_weekend
  )

  # exp
  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules, transform = "exp") %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    exp(as.numeric(alma_next(examples$date1, on_weekends, inclusive = TRUE) - examples$date1)),
    res$date1_before_weekend
  )

  # log
  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules, transform = "log") %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    log(as.numeric(alma_next(examples$date1, on_weekends, inclusive = TRUE) - examples$date1) + 0.5),
    res$date1_before_weekend
  )

  # custom
  custom_fun <- function(x) x / 2

  rec_spec <- recipe(~ date1, data = examples) %>%
    step_date_before(date1, rules = rules, transform = custom_fun) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(
    custom_fun(
      as.numeric(alma_next(examples$date1, on_weekends, inclusive = TRUE) - examples$date1)
    ),
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