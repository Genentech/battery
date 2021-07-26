library(testthat)
library(shiny)

context('test_create')

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

test_it('should add exception handler', {
  fn <- function() {
    print("HELLO")
  }

  battery::exceptions(list(
    foo = fn
  ))

  expect_equal(battery:::global$exceptions$foo, fn)
})

test_it('should clear exception handlers', {
  fn <- function() {
    print("HELLO")
  }

  battery::exceptions(list(
    foo = fn,
    bar = fn,
    baz = fn
  ))
  battery::exceptions(reset = TRUE)
  expect_equal(battery:::global$exceptions, list())
})


test_it('should catch exception', {
  data <- NULL
  msg <- "BATTERY EXCEPTION"
  battery::exceptions(list(
    foo = function(cond) {
      data <<- cond$message
    }
  ))
  battery::withExceptions({
    battery::signal('foo', msg)
  })
  expect_equal(data, msg)
})

test_it('should handle exception from handler', {
  data <- NULL
  msg <- "BATTERY EXCEPTION"
  battery::exceptions(list(
    bar = function(cond) {
      data <<- cond$message
    },
    foo = function(cond) {
      battery::signal('bar', msg)
    }
  ))
  battery::withExceptions({
    battery::signal('foo', msg)
  })
  expect_equal(data, msg)
})

test_it('should catch error with exception', {
  data <- NULL
  msg <- "BATTERY ERROR"
  battery::exceptions(list(
    error = function() {
      data <<- msg
    }
  ))
  battery::withExceptions({
    foo()
  })

  expect_equal(data, msg)
})

test_it('should catch error in exception handler', {
  data <- NULL
  msg <- "BATTERY ERROR"
  battery::exceptions(list(
    error = function() {
      data <<- msg
    },
    foo = function() {
      foo()
    }
  ))
  battery::withExceptions({
    battery::signal('foo')
  })

  expect_equal(data, msg)
})

test_it('should catch error in error handler', {
  data <- NULL
  msg <- "BATTERY ERROR"
  count <- 0
  battery::exceptions(list(
    error = function() {
      count <<- count + 1
      foo()
    },
    foo = function() {
      foo()
    }
  ))
  tryCatch({
    battery::withExceptions({
      battery::signal('foo')
    })
  }, error = function(cond) {
    data <<- msg
  })

  expect_equal(count, 1)
  expect_equal(data, msg)
})

test_it('should catch error in handler', {
  data <- NULL
  msg <- "BATTERY ERROR"
  battery::exceptions(list(
    foo = function() {
      foo()
    },
    error = function() {
      data <<- msg
    }
  ))
  battery::withExceptions({
    battery::signal('foo')
  })

  expect_equal(data, msg)
})



test_it('should not remove handler', {
  data <- NULL
  msg <- "BATTERY EXCEPTION"
  battery::exceptions(list(
    foo = function(cond) {
      data <<- cond$message
    }
  ))
  battery::exceptions(list(
    bar = function(cond) {
      data <<- cond$message
    }
  ))
  battery::withExceptions({
    battery::signal('foo', msg)
  })
  expect_equal(data, msg)
})

test_it('should stop execution after exception', {
  data <- NULL
  msg <- "BATTERY EXCEPTION"
  battery::exceptions(list(
    foo = function(cond) {
      data <<- cond$message
    }
  ))
  battery::withExceptions({
    battery::signal('foo', msg)
    data <<- NULL
  })
  expect_equal(data, msg)
})

test_it('should handle exception in component method', {
  input <- battery::activeInput()
  output <- battery::activeOutput()
  session <- list()
  data <- NULL
  msg <- "BATTERY EXCEPTION"
  A <- battery::component(
    classname = "A",
    public = list(
      foo = function() {
        battery::signal('foo', msg)
      }
    )
  )
  battery::exceptions(list(
    foo = function(cond) {
      data <<- cond$message
    }
  ))

  a <- A$new(input = input, output = output, session = session)

  a$foo()

  expect_equal(data, msg)
})

test_it('should handle exception in internal event handler', {
  input <- battery::activeInput()
  output <- battery::activeOutput()
  session <- list()
  data <- NULL
  msg <- "BATTERY EXCEPTION"
  A <- battery::component(
    classname = "A",
    public = list(
      constructor = function() {
        self$on('foo', function(msg) {
          battery::signal('foo', msg)
        })
      }
    )
  )
  battery::exceptions(list(
    foo = function(cond) {
      data <<- cond$message
    }
  ))

  a <- A$new(input = input, output = output, session = session)

  a$trigger('foo', list(value = msg))

  expect_equal(data, msg)
})

test_it('should catch exception in input', {
  input <- battery::activeInput()
  session <- list()
  data <- NULL
  msg <- "BATTERY EXCEPTION"
  A <- battery::component(
    classname = "A",
    public = list(
      constructor = function() {
        self$on(self$ns('foo'), function() {
          battery::signal('foo', msg)
        }, input = TRUE)
      }
    )
  )
  battery::exceptions(list(
    foo = function(cond) {
      data <<- cond$message
    }
  ))

  a <- A$new(input = input, output = output, session = session)
  input$new(a$ns('foo'))
  input[[ a$ns('foo') ]] <- FALSE

  expect_equal(data, msg)
})

battery::clearMocks()
