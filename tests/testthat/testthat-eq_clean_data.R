library(rcap)
context("Testing that the clean_data function runs correctly")

test_that("Data gets cleaned correctly", {
  earthquake_data <- load_data() %>% eq_clean_data()
  expect_is(earthquake_data, "data.frame")        # the result will be a dataframe
  expect_is(earthquake_data$date, "Date")         # the date column will be of the class date
  expect_is(earthquake_data$LATITUDE, "numeric")  # that each of LATITUDE and LONGITUDE are numeric
  expect_is(earthquake_data$LONGITUDE, "numeric")
})
