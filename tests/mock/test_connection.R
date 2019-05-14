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
  expect_equal(length(input$listeners[["foo"]]), 1)
  ## every renderUI will get called once - inital value - and once for reactive update
  expect_equal(env$calls, list(15, 15, 15, 30))
})
