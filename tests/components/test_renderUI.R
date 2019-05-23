context('test_renderUI')

test_that('it renders the output using uiOutput', {
  session <- list()
  input <- activeInput()
  output <- activeOutput()

  C <- battery::component(
    classname = "C",
    public = list(
      constructor = function() {
        self$output[[self$ns("xx")]] <- renderUI({
          tags$p("hello")
        })
      },
      render = function() {
        tags$div(
          tags$p("foo bar"),
          uiOutput(self$ns("xx"))
        )
      }
    )
  )
  ## component.id is a way to force id when creating two instances 
  c <- C$new(input = input, output = output, session = session)
  output$new(c$ns("xx"))
  output$new("foo")
  output$foo <- renderUI({ c$render() })
  expect_equal(
    output$foo,
    tags$div(
      tags$p("foo bar"),
      tags$div(
        class = "shiny-html-output shiny-bound-output",
        tags$p("hello")
      )
    )
  )
})

