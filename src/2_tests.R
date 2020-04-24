library(testthat)

df <- readRDS(here::here('data', 'CCG_GP.rds'))

test_that("All variables present",{
  expected <- c("file", "ccg_code", "ccg_ons_code", "ccg_name", "stp_code", "regional_local_office_ons_code", "region_ons_code", "appointment_date", "appt_status", "hcp_type", "appt_mode", "time_between_book_and_appt", "count_of_appointments", "appt_week", "appt_month", "appt_year", "appt_dow", "appt_year_month")
  actual <- names(df)
  expect_equal(expected, actual)
})


