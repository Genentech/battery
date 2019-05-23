context('test_renderUI')

test_that('it renders the output', {
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
  output$new("foo_xx")
  output$new("foo")
  c <- C$new(input = input, output = output, session = session, component.id = "foo")
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

