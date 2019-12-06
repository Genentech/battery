library(testthat)
library(shiny)
library(battery)

context('test_serivices')

battery::useMocks()
## ----------------------------------------------------------------------------
test_that('should call init services from constructor', {
  # test service
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )
  service <- Service$new()

  # basic component
  A <- component(
    class = 'A'
  )

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  # instance
  a <- A$new(
    input = input,
    output = output,
    session = session,
    services = list(
      foo = service
    )
  )

  expect_true(identical(a$services$foo, service))

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('should create service using method', {
  # test service
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )
  service <- Service$new()

  # basic component
  A <- component(
    class = 'A'
  )

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  # instance
  a <- A$new(
    input = input,
    output = output,
    session = session
  )

  name <- "foo"
  a$addService(name, service)

  expect_true(identical(a$services[[name]], service))

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('should call method on service', {
  # test service
  called <- FALSE

  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )
  service <- Service$new()

  # basic component
  A <- component(
    class = 'A'
  )

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  # instance
  a <- A$new(
    input = input,
    output = output,
    session = session
  )

  name <- "foo"
  a$addService(name, service)

  a$services[[name]]$test()
  expect_true(called)

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('should share service between components', {
  # test service
  called <- FALSE
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )
  service <- Service$new()

  # basic component
  A <- component(
    class = 'A',
    public = list(
      constructor = function(name) {
        self$addService(name, service)
      }
    )
  )
  B <- component(
    class = 'B',
    public = list(
      constructor = function(name) {
        A$new(parent = self, name = name, component.name = "c_a")
      }
    )
  )
  C <- component(
    class = 'C',
    public = list(
      constructor = function(name) {
        B$new(parent = self, name = name, component.name = "c_a")
      }
    )
  )

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  # name of the service
  name <- "foo"
  # instance
  c <- C$new(
    input = input,
    output = output,
    session = session,
    name = name
  )
  c$services[[name]]$test()
  expect_true(called)

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('should allow to create multiple services', {
  # test service
  called <- FALSE
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      name = NULL,
      initialize = function(name) {
        self$name <- name
      },
      test = function() {
        self$name
      }
    )
  )

  # basic component
  A <- component(
    class = 'A',
    public = list(
      constructor = function() {
        self$addService("foo", Service$new("FOO"))
      }
    )
  )
  B <- component(
    class = 'B',
    public = list(
      constructor = function() {
        A$new(parent = self, component.name = "c_a")
        self$addService("bar", Service$new("BAR"))
      }
    )
  )
  C <- component(
    class = 'C',
    public = list(
      constructor = function() {
        B$new(parent = self, component.name = "c_a")
        self$addService("baz", Service$new("BAZ"))
      }
    )
  )

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  # name of the service
  name <- "foo"
  # instance
  c <- C$new(
    input = input,
    output = output,
    session = session
  )
  ## each constructor create it's own service
  ## and we can access them all in one component
  expect_equal(c$services$foo$test(), "FOO")
  expect_equal(c$services$bar$test(), "BAR")
  expect_equal(c$services$baz$test(), "BAZ")

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('it should throw when duplicated service is added [constructor]', {
  # test service
  called <- FALSE
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )
  service <- Service$new()

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  A <- component(
    class = 'A'
  )
  B <- component(
    class = 'B'
  )
  a <- A$new(
    input = input,
    output = output,
    session = session,
    services = list(
      foo = service
    )
  )
  throw <- FALSE
  tryCatch({
    b <- B$new(
      input = input,
      output = output,
      session = session,
      services = list(
        foo = service
      )
    )
  }, error = function(cond) {
    ## cond will be "[B1] Service 'foo' already exists"
    ## we don't test this, since it may change randomly
    throw <<- TRUE
  })

  expect_true(throw)

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('it should throw when duplicated service is added [method]', {
  # test service
  called <- FALSE
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )
  service <- Service$new()

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  A <- component(
    class = 'A'
  )
  B <- component(
    class = 'B'
  )
  a <- A$new(
    input = input,
    output = output,
    session = session
  )
  name <- "foo"
  a$addService(name, service)
  throw <- FALSE
  tryCatch({
    b <- B$new(
      input = input,
      output = output,
      session = session
    )
    b$addService(name, service)
  }, error = function(cond) {
    ## cond will be "[B1] Service 'foo' already exists"
    ## we don't test this, since it may change randomly
    throw <<- TRUE
  })

  expect_true(throw)

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('it should throw when duplicated service is added [method + new instance]', {
  # test service
  called <- FALSE
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )

  # mocks
  session <- battery::Session$new()
  input <- activeInput()
  output <- activeOutput()

  A <- component(
    class = 'A'
  )
  B <- component(
    class = 'B'
  )
  a <- A$new(
    input = input,
    output = output,
    session = session
  )
  name <- "foo"
  ## difference between prev test is that we create two instances
  ## directly inside addService calls
  a$addService(name, Service$new())
  throw <- FALSE
  tryCatch({
    b <- B$new(
      input = input,
      output = output,
      session = session
    )
    b$addService(name, Service$new())
  }, error = function(cond) {
    ## cond will be "[B1] Service 'foo' already exists"
    ## we don't test this, since it may change randomly
    throw <<- TRUE
  })

  expect_true(throw)

  session$destroy()
})

## ----------------------------------------------------------------------------
battery::clearMocks()
