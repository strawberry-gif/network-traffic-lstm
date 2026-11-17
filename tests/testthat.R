library(testthat)
library(here)

test_dir(here("tests", "testthat"), reporter = "summary", stop_on_failure = TRUE)
