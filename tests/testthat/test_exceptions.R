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

  expect_equal(battery:::global$exceptions$global$foo, fn)
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
  expect_equal(battery:::global$exceptions$global, list())
})

test_it('should clear exception handlers per session', {
  token_A <- "AAAA"
  token_B <- "BBBB"
  session_A <- list(token = token_A)
  session_B <- list(token = token_B)
  fn <- function() {
    print("HELLO")
  }

  battery::exceptions(list(
    foo = fn,
    bar = fn,
    baz = fn
  ), session = session_A)

  battery::exceptions(list(
    foo = fn,
    bar = fn,
    baz = fn
  ), session = session_B)

  battery::exceptions(reset = TRUE, session = session_B)
  expect_equal(battery:::global$exceptions$sessions[[ token_B ]], list())
  expect_equal(battery:::global$exceptions$sessions[[ token_A ]], list(
    foo = fn,
    bar = fn,
    baz = fn
  ))
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

test_it('should execute exception per session', {
  token_A <- "AAAA"
  token_B <- "BBBB"
  session_A <- battery::Session$new(token_A)
  session_B <- battery::Session$new(token_B)
  data <- NULL
  fn <- function() {
    count <<- count + 1
  }

  battery::exceptions(list(
    foo = function() {
      data <<- "SESSION_A"
    }
  ), session = session_A)

  battery::exceptions(list(
    foo = function() {
      data <<- "SESSION_B"
    }
  ), session = session_B)

  battery::withExceptions({
    battery::signal('foo', msg)
  }, session = session_A )

  expect_equal(data, "SESSION_A")

  battery::withExceptions({
    battery::signal('foo', msg)
  }, session = session_B)

  expect_equal(data, "SESSION_B")
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
  session <- battery::Session$new()
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
  ), session = session)

  a <- A$new(session = session)

  a$foo()

  expect_equal(data, msg)
})

test_it('should handle exception in internal event handler', {
  session <- battery::Session$new()
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
  ), session = session)

  a <- A$new(session = session)

  a$trigger('foo', list(value = msg))

  expect_equal(data, msg)
})

test_it('should catch exception in input', {
  session <- battery::Session$new()
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
  ), session = session)

  a <- A$new(session = session)
  input <- session$input
  input$new(a$ns('foo'))
  input[[ a$ns('foo') ]] <- FALSE

  expect_equal(data, msg)
})

battery::clearMocks()
