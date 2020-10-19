#devtools::use_testthat()
Sys.setenv("R_TESTS" = "")
library(testthat)
library(NOAA)

test_check("NOAA")
