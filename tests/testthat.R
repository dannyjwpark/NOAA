#devtools::use_testthat()
Sys.setenv("R_TESTS" = "")
library(testthat)
library(rcap)

test_check("rcap")
