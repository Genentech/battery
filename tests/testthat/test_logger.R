library(testthat)
library(shiny)

context('test_logger')

battery::useMocks()

test_it <- function(message, expr) {
  test_that(message, {
    expr
    after_each()
  })
}

after_each <- function() {
  battery::exceptions(reset = TRUE)
}

test_it('should restore log level after method call', {
  session <- battery::Session$new()
  A <- battery::component(
    classname = "A",
    public = list(
      foo = function() {
        invisible(NULL)
      }
    )
  )
  a <- A$new(session = session)
  level <- a$static$.global$.level

  a$foo()
  a$foo()
  a$foo()

  expect_equal(a$static$.global$.level, level)
})

test_it('should restore log level when method throws an error', {
  session <- battery::Session$new()
  A <- battery::component(
    classname = "A",
    public = list(
      foo = function() {
        stop("BATTERY ERROR")
      }
    )
  )
  battery::exceptions(list(
    error = function() {
      return(FALSE)
    }
  ), session = session)

  a <- A$new(session = session)
  level <- a$static$.global$.level

  a$foo()

  expect_equal(a$static$.global$.level, level)
})

test_it('should restore log level when method signals an exception', {
  session <- battery::Session$new()
  A <- battery::component(
    classname = "A",
    public = list(
      foo = function() {
        battery::signal('log', 'HELLO')
      }
    )
  )
  battery::exceptions(list(
    log = function(cond) {
      invisible(NULL)
    }
  ), session = session)

  a <- A$new(session = session)
  level <- a$static$.global$.level

  a$foo()

  expect_equal(a$static$.global$.level, level)
})

test_it('should indent nested method calls in log messages', {
  session <- battery::Session$new()
  messages <- c()

  A <- battery::component(
    classname = "A",
    public = list(
      outer = function() {
        self$inner()
      },
      inner = function() {
        invisible(NULL)
      }
    )
  )

  a <- A$new(session = session)
  a$logger('info', function(data) {
    messages <<- c(messages, data$message)
  })

  a$outer()

  expect_equal(messages, c(
    "outer::before",
    "  inner::before",
    "  inner::after",
    "outer::after"
  ))
})

battery::clearMocks()
