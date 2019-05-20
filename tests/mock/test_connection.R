source('../../R/mock.R')

test_that('it should create input/output binding', {
  input <- activeInput(foo = NULL)
  output <- activeOutput(bar = NULL)
  output$bar <- renderUI({ input$foo + 10 })

  input$foo <- 10
  expect_equal(output$bar, 20)
  expect_equal(input$foo, 10)
})

test_that('it should create single listener', {
  input <- activeInput(foo = NULL)
  output <- activeOutput(bar = NULL)
  env <- new.env()
  env$calls <- list()
  input$foo <- 5
  output$bar <- renderUI({
    ret <- input$foo + 10
    env$calls <- append(env$calls, list(ret))
    ret
  })
  output$bar <- renderUI({
    ret <- input$foo + 10
    env$calls <- append(env$calls, list(ret))
    ret
  })
  output$bar <- renderUI({
    ret <- input$foo + 10
    env$calls <- append(env$calls, list(ret))
    ret
  })
  input$foo <- 20
  expect_equal(length(input$listeners[['foo']]), 1)
  ## every renderUI will get called once - inital value - and once for reactive update
  expect_equal(env$calls, list(15, 15, 15, 30))
})

test_that('it should bind in nested function', {
  foo <- function() {
    input$foo + 10
  }
  input <- activeInput(foo = NULL)
  output <- activeOutput(bar = NULL)
  output$bar <- renderUI({
    foo() + 20
  })
  input$foo <- 10
  expect_equal(output$bar, 40)
})

test_that('it should bind in deep nested function', {
  foo <- function() {
    bar() + 1
  }
  bar <- function() {
    baz() + 1
  }
  baz <- function() {
    input$foo + 1
  }
  input <- activeInput(foo = NULL)
  output <- activeOutput(bar = NULL)
  output$bar <- renderUI({
    foo() + 1
  })
  input$foo <- 1
  expect_equal(output$bar, 5)
})

test_that('it should bind in nested object method', {
  x <- list(
    foo = function() {
      bar() + 1
    }
  )
  bar <- function() {
    input$foo + 1
  }
  input <- activeInput(foo = NULL)
  output <- activeOutput(bar = NULL)
  output$bar <- renderUI({
    x$foo() + 1
  })
  input$foo <- 1
  expect_equal(output$bar, 4)
  foo <- function() {
    x$foo() + 1
  }
  output$bar <- renderUI({
    foo() + 1
  })
  input$foo <- 2
  expect_equal(output$bar, 6)
  ## test string prop
  foo <- function() {
    x[['foo']]() + 1
  }
  input$foo <- 3
  expect_equal(output$bar, 7)
  ## test variable prop
  name <- 'foo'
  foo <- function() {
    x[[name]]() + 1
  }
  input$foo <- 4
  expect_equal(output$bar, 8)
})
test_that('it should find active variable inside R constructs', {
  x <- TRUE
  specs <- list(
    renderUI({
      if (x && !is.null(input$foo)) {
        20
      }
    }),
    renderUI({
      if (TRUE && TRUE) {
        input$foo + 10
      }
    }),
    renderUI({
      l <- list(1, 2, 3)
      r <- NULL
      for (x in l) {
        r <- x + input$foo
      }
      20
    }),
    renderUI({
      x <- list(
        foo = input$foo
      )
      x$foo + 10
    }),
    renderUI({
      foo(bar(input$foo)) + 10
    }),
    renderUI({
      x <- c(input$foo)
      x[1] + 10
    })
  )
  foo <- function(x) x
  bar <- foo
  for (spec in specs) {
    input <- activeInput(foo = NULL)
    output <- activeOutput(bar = NULL)
    output$bar <- spec
    input$foo <- 10
    expect_equal(output$bar, 20)
  }
})

test_that('it should not find active name when using isolate', {
   input <- activeInput(foo = NULL)
   output <- activeOutput(bar = NULL)
   input$foo <- 10
   output$bar <- renderUI({
     isolate({ input$foo })
   })
   input$foo <- 20
   expect_equal(output$bar, 10)
   output$bar <- renderUI({
     isolate({ input$foo })
   })
   expect_equal(output$bar, 20)
})
