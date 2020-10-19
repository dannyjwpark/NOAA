library(Rcapture)
library(testthat)
context("Testing that the eq_location_clean function runs correctly")

test_that("Location gets cleaned correctly", {
  earthquake_data <- load_data() %>% eq_location_clean
  expect_true(all(earthquake_data$LOCATION_NAME[1:2] == c("JORDAN:  BAB-A-DARAA,AL-KARAK", "SYRIA:  UGARIT")))
  expect_true(all(earthquake_data$CLEAN_LOCATION_NAME[1:2] == c("Bab-A-Daraa,Al-Karak", "Ugarit")))
})
