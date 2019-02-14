context("Test Utility functions")


# datemunge ---------------------------------------------------------------


test_that("datemunge formats dates", {
  dates     <- c("1/1/2006",   "10/5/2009",  "12/31/2012")
  dates_exp <- c("2006-01-01", "2009-10-05", "2012-12-31")
  dates_obs <- datemunge(dates)
  expect_equal(dates_obs, dates_exp)
})

test_that("datemunge accepts arbitrary input field ordering", {
  # As above, but, D/M/Y
  dates     <- c("1/1/2006",   "5/10/2009",  "31/12/2012")
  dates_exp <- c("2006-01-01", "2009-10-05", "2012-12-31")
  dates_obs <- datemunge(dates, ord_input = c("D", "M", "Y"))
  expect_equal(dates_obs, dates_exp)
})

test_that("datemunge rejects invalid input", {
  dates     <- c("1/1/2006",   "10/5/2009", "not a date")
  expect_error(datemunge(dates))
  dates[3] <- "also/not/date"
  expect_error(datemunge(dates))
  # Oops, we forgot to specify the field order
  dates     <- c("1/1/2006",   "5/10/2009",  "31/12/2012")
  expect_error(datemunge(dates))
})
