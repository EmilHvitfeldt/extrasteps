test_that("check_name() is used", {
  dat <- iris
  dat$Species_1 <- dat$Species

  rec <- recipe(~., data = dat) |>
    step_encoding_binary(Species)

  expect_snapshot(
    error = TRUE,
    prep(rec, training = dat)
  )
})

# Infrastructure ---------------------------------------------------------------
