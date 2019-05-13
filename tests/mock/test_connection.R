source('../../R/mock.R')

test_that('it should create input/output binding', {
  input <- activeInput(foo = NULL)
  output <- activeOutput(bar = NULL)
  output$bar <- renderUI({ input$foo + 10 })

  input$foo <- 10
  expect_equal(output$bar, 20)
  expect_equal(input$foo, 10)
})
