library(testthat)
library(battery)

test_dir <- function(dir) {
  path <- file.path(getSrcDirectory(function(x) x), dir)
  if (length(path) == 0) {
    path <- dir
  }
  testthat::test_dir(path)
}

test_dir("mocks")
## TODO
## test_dir("components")
