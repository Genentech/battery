library(testthat)
library(shiny)

context('test_observeEvent')

battery::useMocks()

test_that('it should invoke observeEvent', {
  input <- activeInput(foo = NULL)
  env <- new.env()
  env$calls <- list()
  observeEvent(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  input$foo <- 10
  expect_equal(env$calls, list(10))
})

## ----------------------------------------------------------------------------
test_that('it should invoke observeEvent when called before creating active prop', {
  input <- activeInput()
  env <- new.env()
  env$calls <- list()
  observeEvent(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  input$new('foo')
  input$foo <- 10
  expect_equal(env$calls, list(10))
})

## ----------------------------------------------------------------------------
test_that('it should clear observe event', {
  input <- activeInput(foo = NULL)
  env <- new.env()
  env$calls <- list()
  observeEvent(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  observeEvent(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  input$foo <- 10
  expect_equal(env$calls, list(10))
})

## ----------------------------------------------------------------------------
test_that('it should invoke observer multiple times', {
  input <- activeInput(foo = NULL)
  env <- new.env()
  env$calls <- list()
  observeEvent(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })

  input$foo <- 10
  input$foo <- 20
  input$foo <- 30
  expect_equal(env$calls, list(10, 20, 30))
})

## ----------------------------------------------------------------------------
test_that('it should invoke observer only once', {
  input <- activeInput(foo = NULL)
  env <- new.env()
  env$calls <- list()
  observeEvent(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  }, once = TRUE)

  input$foo <- 10
  input$foo <- 20
  input$foo <- 30
  expect_equal(env$calls, list(10))
})

## ----------------------------------------------------------------------------
test_that('it should call on NULL value', {
  input <- activeInput(foo = NULL)
  env <- new.env()
  env$calls <- list()
  observeEvent(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  }, ignoreNULL = FALSE)

  input$foo <- 10
  input$foo <- NULL
  input$foo <- 30
  expect_equal(env$calls, list(NULL, 10, NULL, 30))
})

battery::clearMocks()
