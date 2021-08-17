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
    step_time_event(date1, rules = rules) %>%
    prep()

  res <- bake(rec_spec, new_data = NULL)

  expect_equal(names(res), c("date1_weekend", "date1_weekday"))

  expect_equal(
    alma_in(examples$date1, on_weekdays),
    res$date1_weekday
  )
  expect_equal(
    alma_in(examples$date1, on_weekends),
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

  expect_equal(names(res), c("date1_weekend", "date1_weekday",
                             "date2_weekend", "date2_weekday"))

  expect_equal(
    alma_in(examples$date1, on_weekdays),
    res$date1_weekday
  )
  expect_equal(
    alma_in(examples$date1, on_weekends),
    res$date1_weekend
  )
  expect_equal(
    alma_in(examples$date2, on_weekdays),
    res$date2_weekday
  )
  expect_equal(
    alma_in(examples$date2, on_weekends),
    res$date2_weekend
  )
})

test_that("time_event errors", {
  on_weekends <- weekly() %>% recur_on_weekends()
  on_weekdays <- weekly() %>% recur_on_weekdays()

  rules <- list(weekend = on_weekends, weekday = on_weekdays)

  expect_error(
    recipe(~ ., data = examples) %>%
      step_time_event(numeric, rules = rules) %>%
      prep()
  )

  expect_error(
    recipe(~ ., data = examples) %>%
      step_time_event(date1, rules = "wrong") %>%
      prep()
  )

  expect_error(
    recipe(~ ., data = examples) %>%
      step_time_event(date1, rules = list(weekend = on_weekends, "Hello")) %>%
      prep()
  )

  expect_error(
    recipe(~ ., data = examples) %>%
      step_time_event(date1, rules = list(weekend = on_weekends,
                                          christmas = "2020-12-25")) %>%
      prep()
  )

})
