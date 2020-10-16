library(rcap)
context("Testing that the raw data loads correctly")

test_that("Data can be collected from the data_raw folder and loaded into memory", {
  earthquake_data <- load_data()
  expect_equal(dim(earthquake_data)[1], 5979)
  expect_true("data.frame" %in% class(df))
})
