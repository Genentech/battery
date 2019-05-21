library(testthat)
library(battery)

observeEvent <- battery::observeEventMock
assignInNamespace("observeEvent", observeEvent, "battery")

test_dir <- function(dir) {
  path <- file.path(getSrcDirectory(function(x) x), dir)
  if (length(path) == 0) {
    path <- dir
  }
  testthat::test_dir(path)
}

test_dir("mocks")
test_dir("components")
