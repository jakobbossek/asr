library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(asr)
}

test_dir("tests/testthat")
