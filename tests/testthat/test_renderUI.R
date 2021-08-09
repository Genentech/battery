library(testthat)
library(shiny)

context('test_renderUI')


battery::useMocks()

## overwrite shiny function so we know that we have right injected function istead of shiny one
uiOutput <- function(x) stop('function not overwritten by mocks')

x_test_that <- function(x,y) NULL

## --------------------------------------------------------------------------------------
test_that('it renders the output using uiOutput', {
  input <- battery::activeInput(foo = NULL)
  output <- battery::activeOutput()
  session <- battery::Session$new(input = input, output = output)

  Comp <- battery::component(
    classname = 'C',
    public = list(
      constructor = function() {
        self$output[[self$ns('xx')]] <- renderUI({
          tags$p('hello')
        })
      },
      render = function() {
        tags$div(
          tags$p('foo bar'),
          uiOutput(self$ns('xx'))
        )
      }
    )
  )
  ## component.id is a way to force id when creating two instances
  x <- Comp$new(session = session)
  ## it also work when you create active output after adding first value
  ## so you can know what is the id when creating multiple instances
  output$new(x$ns('xx'))
  output$new('foo')
  output$foo <- renderUI({ x$render() })
  expect_equal(output[[x$ns('xx')]], tags$p('hello'))
  expect_equal(
    output$foo,
    tags$div(
      tags$p('foo bar'),
      tags$div(
        id = x$ns('xx'),
        class = 'shiny-html-output',
        tags$p('hello')
      )
    )
  )
})


## --------------------------------------------------------------------------------------
test_that('is should render output when uiOutput is in function', {
  ## TODO: fix when function have args
  args <- list()
  fn <- uiOutput
  foo <- function(name) {
    args <<- list(name)
    ## foo environment is patched in battery::activeOutput
    ## so it have different uiOutput function with active value
    ## same shiny::tags as with shiny and with output[[name]] as
    ## content inside shiny::tags
    expect_false(identical(uiOutput, fn))
    uiOutput(name)
  }
  session <- list()
  input <- battery::activeInput()
  output <- battery::activeOutput()

  C <- battery::component(
    classname = 'C',
    public = list(
      constructor = function() {
        self$output[[self$ns('xx')]] <- renderUI({
          tags$p('hello')
        })
      },
      render = function() {
        tags$div(
          tags$p('foo bar'),
          foo(self$ns('xx'))
        )
      }
    )
  )
  ## component.id is a way to force id when creating two instances
  c <- C$new(input = input, output = output, session = session)
  output$new(c$ns('xx'))
  output$new('foo')
  output$foo <- renderUI({ c$render() })
  expect_equal(args, list(c$ns('xx')))
  expect_equal(
    output$foo,
    tags$div(
      tags$p('foo bar'),
      tags$div(
        id = 'C1_xx',
        class = 'shiny-html-output',
        tags$p('hello')
      )
    )
  )
})

## --------------------------------------------------------------------------------------
test_that('it should render output when uiOutput is in other method', {
  session <- list()
  input <- battery::activeInput()
  output <- battery::activeOutput()

  Comp <- battery::component(
    classname = 'C',
    public = list(
      constructor = function() {
        self$output[[self$ns('xxx')]] <- renderUI({
          tags$p('hello')
        })
      },
      render = function() {
        tags$div(
          tags$p('foo bar'),
          self$foo('xxx')
        )
      },
      foo = function(name) {
        tags$div(class = 'foo', uiOutput(self$ns(name)))
      }
    )
  )
  x <- Comp$new(input = input, output = output, session = session)
  output$new('foo')
  output$foo <- renderUI({
    x$render()
  })

  expect_equal(
    output$foo,
    tags$div(
      tags$p('foo bar'),
      tags$div(
        class = 'foo',
        tags$div(
          id = 'C1_xxx',
          class = 'shiny-html-output',
          tags$p('hello')
        )
      )
    )
  )
})

## --------------------------------------------------------------------------------------
test_that('is should render output when uiOutput is in other method that have if', {
  session <- list()
  input <- battery::activeInput()
  output <- battery::activeOutput()

  Comp <- battery::component(
    classname = 'C',
    public = list(
      constructor = function() {
        self$output[[self$ns('xxx')]] <- renderUI({
          tags$p('hello')
        })
      },
      render = function() {
        tags$div(
          tags$p('foo bar'),
          self$foo('hello'),
          self$foo('xxx')
        )
      },
      foo = function(name) {
        if (name == 'xxx') {
          uiOutput(self$ns(name))
        } else {
          name
        }
      }
    )
  )
  x <- Comp$new(input = input, output = output, session = session)
  output$new('foo')
  output$foo <- renderUI({
    x$render()
  })

  expect_equal(
    output$foo,
    tags$div(
      tags$p('foo bar'),
      'hello',
      tags$div(
        id = 'C1_xxx',
        class = 'shiny-html-output',
        tags$p('hello')
      )
    )
  )

})
## --------------------------------------------------------------------------------------

test_that('it should render output with active input', {
  session <- list()
  input <- battery::activeInput()
  output <- battery::activeOutput()

  Comp <- battery::component(
    classname = 'C',
    public = list(
      constructor = function() {
        count <- 0
        self$output[[self$ns('xxx')]] <- renderUI({
          count <- count + 1
          tags$p(paste0('hello ', self$input$foo + count))
        })
      },
      render = function() {
        tags$div(
          tags$p('foo bar'),
          uiOutput(self$ns('xxx'))
        )
      },

      foo = function(value) {
        self$input$foo <- value
      }
    )
  )
  input$new('foo')
  x <- Comp$new(input = input, output = output, session = session)
  output$new(x$ns('xxx'))
  output$new('foo')
  x$foo(100)
  x$foo(50)
  output$foo <- renderUI({
    x$render()
  })

  expect_equal(
    output$foo,
    tags$div(
      tags$p('foo bar'),
      tags$div(
        id = 'C1_xxx',
        class = 'shiny-html-output',
        tags$p(paste0('hello ', 53)) # count is 1 init value + 2 changes (+ 50 from input)
      )
    )
  )
})

## --------------------------------------------------------------------------------------
test_that('it should render output with active input in function', {
  session <- list()
  input <- battery::activeInput()
  output <- battery::activeOutput()

  foo <- function(self) {
    self$input$foo
  }

  Comp <- battery::component(
    classname = 'C',
    public = list(
      constructor = function() {
        count <- 0
        self$output[[self$ns('xxx')]] <- renderUI({
          count <- count + 1
          tags$p(paste0('hello ', foo(self) + count))
        })
      },
      render = function() {
        tags$div(
          tags$p('foo bar'),
          uiOutput(self$ns('xxx'))
        )
      },

      foo = function(value) {
        self$input$foo <- value
      }
    )
  )
  input$new('foo')
  x <- Comp$new(input = input, output = output, session = session)
  output$new(x$ns('xxx'))
  output$new('foo')
  x$foo(100)
  x$foo(50)
  output$foo <- renderUI({
    x$render()
  })

  expect_equal(
    output$foo,
    tags$div(
      tags$p('foo bar'),
      tags$div(
        id = 'C1_xxx',
        class = 'shiny-html-output',
        tags$p(paste0('hello ', 53)) # count is 1 init value + 2 changes (+ 50 from input)
      )
    )
  )
})

## --------------------------------------------------------------------------------------
test_that('it should render output with active input in method', {
  session <- list()
  input <- battery::activeInput()
  output <- battery::activeOutput()

  Comp <- battery::component(
    classname = 'C',
    public = list(
      constructor = function() {
        count <- 0
        self$output[[self$ns('xxx')]] <- renderUI({
          count <- count + 1
          tags$p(paste0('hello ', self$bar() + count))
        })
      },
      render = function() {
        tags$div(
          tags$p('foo bar'),
          uiOutput(self$ns('xxx'))
        )
      },

      bar = function() {
        self$input$foo
      },

      foo = function(value) {
        self$input$foo <- value
      }
    )
  )
  input$new('foo')
  x <- Comp$new(input = input, output = output, session = session)
  output$new(x$ns('xxx'))
  output$new('foo')
  x$foo(100)
  x$foo(50)
  output$foo <- renderUI({
    x$render()
  })

  expect_equal(
    output$foo,
    tags$div(
      tags$p('foo bar'),
      tags$div(
        id = 'C1_xxx',
        class = 'shiny-html-output',
        tags$p(paste0('hello ', 53)) # count is 1 init value + 2 changes (+ 50 from input)
      )
    )
  )
})

battery::clearMocks()
