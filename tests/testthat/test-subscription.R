context("R/subscriptions.R")

test_that("cohort_to_calendar returns a tibble", {
  expect_is(cohort_to_calendar(rweibull(1, 1, 1)), c("tbl_df", "tbl", "data.frame"))
})
