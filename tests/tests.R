library(testthat)
library(battery)
library(shiny)

## setup mocks
observeEvent <- battery::observeEventMock
assignInNamespace("observeEvent", observeEvent, "battery")
assignInNamespace("observeEvent", observeEvent, "shiny")
isolate <- battery::isolate
assignInNamespace("isolate", isolate, "shiny")
makeReactiveBinding <- battery::makeReactiveBinding
assignInNamespace("makeReactiveBinding", makeReactiveBinding, "shiny")
renderUI <- battery::renderUI
assignInNamespace("renderUI", renderUI, "shiny")
uiOutput <- battery::uiOutput
assignInNamespace("uiOutput", uiOutput, "shiny")

test_dir <- function(dir) {
  path <- file.path(getSrcDirectory(function(x) x), dir)
  if (length(path) == 0) {
    path <- dir
  }
  testthat::test_dir(path)
}

test_dir("mocks")
test_dir("components")
