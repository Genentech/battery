test_that('it should invoke observeEvent', {
  input <- activeInput(foo = NULL)
  env <- new.env()
  env$calls <- list()
  observeEventMock(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  input$foo <- 10
  expect_equal(env$calls, list(10))
})

test_that('it should invoke observeEvent when called before creating active prop', {
  input <- activeInput()
  env <- new.env()
  env$calls <- list()
  observeEventMock(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  input$new("foo")
  input$foo <- 10
  expect_equal(env$calls, list(10))
})

test_that('it should clear observe event', {
  input <- activeInput(foo = NULL)
  env <- new.env()
  env$calls <- list()
  observeEventMock(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  observeEventMock(input$foo, {
    env$calls <- c(env$calls, list(input$foo))
  })
  input$new("foo")
  input$foo <- 10
  # two init calls + active update
  expect_equal(env$calls, list(10, 10, 10))
})

test_that('it should invoke observer only once', {
  
})
