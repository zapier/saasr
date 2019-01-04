context("R/subscriptions.R")

test_that("cohort_to_calendar returns a tibble", {
  expect_is(cohort_to_calendar(rweibull(1, 1, 1)), c("tbl_df", "tbl", "data.frame"))
})

test_that("subscribers_by_day returns a tibble", {
  expect_is(subscribers_by_day(dist = "weibull", shape = 0.8, scale = 300), c("tbl_df", "tbl", "data.frame"))
})
