library(testthat)
library(shiny)

context("test_input")

battery::useMocks()

test_that('it should create init binding', {
  input <- activeInput(foo = NULL, bar = NULL)
  input$foo <- 'foo'
  input$bar <- 'bar'
  specs <- list(
    list(input$foo, 'foo'),
    list(input$`__foo`, 'foo'),
    list(input$bar, 'bar'),
    list(input$`__bar`, 'bar')
  )
  for (spec in specs) {
    do.call(expect_equal, spec)
  }
})

## ----------------------------------------------------------------------------
test_that('it create event before binding', {
  input <- activeInput()
  args <- NULL
  input$on('foo', function(old, new) {
    args <<- list(old, new)
  })
  input$new('foo')
  input$foo <- 10
  expect_equal(args, list(NULL, 10))
})

## ----------------------------------------------------------------------------
test_that('it should invoke listener', {
  input <- activeInput(foo = NULL)
  called <- FALSE
  input$on('foo', function(old, new) {
    called <<- TRUE
  })
  input$foo <- 10
  expect_equal(input$foo, 10)
  expect_equal(called, TRUE)
})

## ----------------------------------------------------------------------------
test_that('event have proper args', {
  input <- activeInput(foo = NULL)
  args <- list()
  input$on('foo', function(old, new) {
    args <<- append(args, list(list(old, new)))
  })
  input$foo <- 10
  input$foo <- 20
  expect_equal(args, list(list(NULL, 10), list(10, 20)))
})

## ----------------------------------------------------------------------------
test_that('create proxy value', {
  input <- activeInput(foo = function(value) {
    if (missing(value)) {
      self$`__x`
    } else {
      self$`__x` <- value + 10
    }
  })
  input$foo <- 10
  expect_equal(input$foo, 20)
  expect_equal(input$`__x`, 20)
})
