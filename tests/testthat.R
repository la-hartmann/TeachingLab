Sys.setenv("R_TESTS" = "")
library(testthat)
library(TeachingLab)

test_check("TeachingLab")
