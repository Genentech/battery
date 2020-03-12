library(testthat)
library(shiny)

context('test_EventEmitter')

battery::useMocks()

## ----------------------------------------------------------------------------
createEmitter <- function() {
  e <- battery::EventEmitter$new(spy = TRUE, shiny = TRUE)

  ## mock reactiveBinding
  e$events <- battery::activeInput()
  e$events$new("test")
  e
}

## ----------------------------------------------------------------------------
test_that('it should send event without arguments', {
  e <- createEmitter()

  called <- FALSE
  e$on("test", function() {
    called <<- TRUE
  })
  e$emit("test")
  expect_equal(called, TRUE)
  e$emit("test", 100)
  expect_equal(e$.calls[["invoke"]], list(list("test", TRUE), list("test", 100)))
})

## ----------------------------------------------------------------------------
test_that('it should receive data from event', {
  e <- createEmitter()
  output <- NULL
  e$on("test", function(data) {
    output <<- data
  })
  e$emit("test", 10)
  expect_equal(output, 10)
  input <- list(foo = 10)
  e$emit("test", input)
  expect_true(identical(output, input))
})

## ----------------------------------------------------------------------------
test_that('it should send nitification event multiple times', {
  e <- createEmitter()

  count <- 0
  e$on("test", function() {
    count <<- count + 1
  })

  e$emit("test")
  e$emit("test")
  e$emit("test")
  e$emit("test")

  expect_equal(count, 4)
})

## ----------------------------------------------------------------------------
test_that('it should attach multiple handlers', {
  e <- createEmitter()

  foo <- FALSE
  bar <- FALSE
  baz <- FALSE

  e$on("test", function() {
    foo <<- TRUE
  })
  e$on("test", function() {
    bar <<- TRUE
  })
  e$on("test", function() {
    baz <<- TRUE
  })

  e$emit("test")

  expect_true(foo)
  expect_true(bar)
  expect_true(baz)
})

## ----------------------------------------------------------------------------
test_that('it should remove single handler', {
  e <- createEmitter()

  foo <- 0
  bar <- 0
  baz <- 0

  fooHandler <- function() {
    foo <<- foo + 1
  }
  barHandler <- function() {
    bar <<- bar + 1
  }
  bazHandler <- function() {
    baz <<- baz + 1
  }

  e$on("test", fooHandler)
  e$on("test", barHandler)
  e$on("test", bazHandler)

  e$emit("test")
  e$off("test", fooHandler)
  e$emit("test")
  e$emit("test")

  expect_equal(foo, 1)
  expect_equal(bar, 3)
  expect_equal(baz, 3)
})

## ----------------------------------------------------------------------------
test_that('it should remove handlers all', {
  e <- createEmitter()

  foo <- 0
  bar <- 0
  baz <- 0

  e$on("test", function() {
    foo <<- foo + 1
  })
  e$on("test", function() {
    bar <<- bar + 1
  })
  e$on("test", function() {
    baz <<- baz + 1
  })

  e$emit("test")
  e$off("test")
  ## this will show warnings in console (that emitter have no listeners)
  e$emit("test")
  e$emit("test")
  e$emit("test")
  e$emit("test")

  expect_equal(foo, 1)
  expect_equal(bar, 1)
  expect_equal(baz, 1)
})

## ----------------------------------------------------------------------------
battery::clearMocks()
