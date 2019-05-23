context('test_renderUI')

source("../../R/mocks.R")
source("../../R/components.R")

## overwrite shiny function so we know that we have right inject function istead of shiny one
uiOutput <- function(x) x

x_test_that <- function(x,y) NULL

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
  expect_equal(output[[c$ns("xx")]], tags$p("hello"))
  expect_equal(
    output$foo,
    tags$div(
      tags$p("foo bar"),
      tags$div(
        id = c$ns("xx"),
        class = "shiny-html-output",
        tags$p("hello")
      )
    )
  )
})

x_test_that('is should render output when uiOutput is in method', {
  
})

test_that('is should render output when uiOutput is in function', {
  ## TODO: fix when function have args 
  args <- list()
  fn <- uiOutput
  foo <- function(name) {
    args <<- list(name)
    print(uiOutput)
    expect_false(identical(uiOutput, fn))
    uiOutput(name)
  }
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
          foo(self$ns("xx"))
        )
      }
    )
  )
  ## component.id is a way to force id when creating two instances 
  c <- C$new(input = input, output = output, session = session)
  output$new(c$ns("xx"))
  output$new("foo")
  output$foo <- renderUI({ c$render() })
  expect_equal(args, list(c$ns("xx")))
  expect_equal(
    output$foo,
    tags$div(
      tags$p("foo bar"),
      tags$div(
        class = "shiny-html-output",
        tags$p("hello")
      )
    )
  )
})


