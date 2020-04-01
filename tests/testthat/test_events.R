library(testthat)
library(shiny)

context('test_events')

battery::useMocks()

test_that('it should emit event', {
  InputComponent <- battery::component(
    classname = 'InputComponent',
    public = list(
      constructor = function() {
        self$logger(c('info', 'battery'), function(data) {
          print(paste(data$message, data$id, `if`(length(data$args) > 0, data$args[[1]])))
        })
      },
      run = function(arg) {
        self$emit('foo', arg)
      },
      foo = function(x) {

      }
    )
  )
  args <- list()
  TestingComponent <- battery::component(
    classname = 'TestingComponent',
    public = list(
      constructor = function() {
        InputComponent$new(component.name = 'c', parent = self, spy = TRUE)
        self$on('foo', function(value) {
          print("TRIGGER")
          self$foo(value)
          args <<- list(value)
        })
      },
      foo = function(x) { }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  t <- TestingComponent$new(input = input, output = output, session = session,  spy = TRUE)
  t$children$c$run(100)

  expect_equal(args, list(100))
  expect_equal(t$.calls$foo, list(list(100)))
  t$children$c$run(200)
  expect_equal(args, list(200))
  expect_equal(t$.calls$foo, list(list(100), list(200)))
})

test_that('should emit events to parent of parent', {
  InputComponent <- battery::component(
    classname = 'InputComponent',
    public = list(
      constructor = function() {},
      run = function(arg) {
        self$emit('foo', arg)
      }
    )
  )
  TestingComponent <- battery::component(
    classname = 'TestingComponent',
    public = list(
      constructor = function() {
        InputComponent$new(component.name = 'c', parent = self)
      }
    )
  )
  args <- list()
  ParentTestingComponent <- battery::component(
    classname = 'TestingComponent',
    public = list(
      constructor = function() {
        TestingComponent$new(component.name = 'tc', parent = self)
        self$on('foo', function(value) {
          self$foo(value)
          args <<- list(value)
        })
      },
      foo = function(x) { }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()

  t <- ParentTestingComponent$new(input = input, output = output, session = session, spy = TRUE)

  t$children$tc$children$c$run(100)
  expect_equal(args, list(100))
  expect_equal(t$.calls$foo, list(list(100)))
  t$children$tc$children$c$run(200)
  expect_equal(args, list(200))
  expect_equal(t$.calls$foo, list(list(100), list(200)))
})


test_that('it should broadcast event', {

  NestedComponent <- battery::component(
    classname = 'NestedComponent',
    private = list(
      x = NULL,
      foo = function(x) {
        private$x <- x
      }
    ),
    public = list(
      constructor = function() {
        self$on('foo', function(value) {
          private$foo(value)
        })
      }
    )
  )

  InputComponent <- battery::component(
    classname = 'InputComponent',
    public = list(
      run = function(arg) {
        self$broadcast('foo', arg)
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  ic <- InputComponent$new(input = input, output = output, session = session)
  ## we create child component outside of InputComponent so it recieve broadcast event
  ## it's outside so it emulate component that is outside of tests (main testing component)
  NestedComponent$new(component.name = 'nc', parent = ic)
  ic$run(100)
  ic$run('foo')
  args <- lapply(ic$children$nc$events$.calls$foo, function(x) { x[['new']][['value']] })
  expect_equal(args, list(100, 'foo'))
})

battery::clearMocks()
