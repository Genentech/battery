library(testthat)
library(shiny)

context("test_create")

battery::useMocks()

test_that('it should create component using R6Class', {
  A <- R6::R6Class(
    classname = "A",
    inherit = Component,
    public = list(
      static = list2env(list(count = 0)),
      value = NULL,
      name = NULL,
      constructor = function(value, name) {
        self$value <- value
        self$name <- name
      },
      render = function() {
        shiny::tags$p(paste("component", self$name))
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  a <- A$new(value = 10, name = "hello", input = input, output = output, session = session)
  expect_equal(a$value, 10)
  expect_equal(a$render(), shiny::tags$p("component hello"))
})

## ----------------------------------------------------------------------------
test_that('it should create component using component function', {
  A <- component(
    classname = "A",
    public = list(
      value = NULL,
      name = NULL,
      constructor = function(value, name) {
        self$value <- value
        self$name <- name
      },
      render = function() {
        shiny::tags$p(paste("component", self$name))
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  a <- A$new(value = 10, name = "hello", input = input, output = output, session = session)
  expect_equal(a$value, 10)
  expect_equal(a$render(), shiny::tags$p("component hello"))
})

## ----------------------------------------------------------------------------
test_that('it should extend base component using extend static method', {
  A <- Component$extend(
    classname = "A",
    public = list(
      value = NULL,
      name = NULL,
      constructor = function(value, name) {
        self$value <- value
        self$name <- name
      },
      render = function() {
        shiny::tags$p(paste("component", self$name))
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  a <- A$new(value = 10, name = "hello", input = input, output = output, session = session)
  expect_equal(a$value, 10)
  expect_equal(a$render(), shiny::tags$p("component hello"))
})

## ----------------------------------------------------------------------------
test_that('it should create child component', {
  A <- Component$extend(
    classname = "A",
    public = list(
      value = NULL,
      name = NULL,
      constructor = function(value, name) {
        self$value <- value
        self$name <- name
      },
      render = function() {
        shiny::tags$p(paste("component", self$name))
      }
    )
  )
  B <- A$extend(
    classname = "B",
    public = list(
      constructor = function(value) {
        super$constructor(value = value, name = "hi")
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  b <- B$new(value = 10, input = input, output = output, session = session)
  expect_equal(b$value, 10)
  expect_equal(b$render(), shiny::tags$p("component hi"))
})

## ----------------------------------------------------------------------------
test_that('it should create static fields', {
  A <- Component$extend(
    classname = "A",
    static = list(
      number = 10
    ),
    public = list(
      value = NULL,
      name = NULL,
      constructor = function() {
        static$number = static$number + 1
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  a <- A$new(input = input, output = output, session = session)
  expect_equal(a$static$number, 11)
  b <- A$new(input = input, output = output, session = session)
  c <- A$new(input = input, output = output, session = session)
  expect_equal(a$static$number, 13)
  expect_equal(b$static$number, 13)
  expect_equal(c$static$number, 13)
})

## ----------------------------------------------------------------------------
test_that('it should call parent constructor if no constuctor in child component', {
  A <- Component$extend(
    classname = "A",
    public = list(
      name = NULL,
      constructor = function() {
        self$name = "foo"
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  B <- A$extend(
    classname = "B",
    public = list(
      x = 10
    )
  )
  b <- B$new(input = input, output = output, session = session)
  expect_equal(b$name, "foo")
  expect_equal(b$x, 10)
})

## ----------------------------------------------------------------------------
test_that('it should create new static field in child component', {
  A <- Component$extend(
    classname = "A",
    static = list(
      number = 10
    ),
    public = list(
      constructor = function() {
        static$number = static$number + 1
      }
    )
  )
  B <- A$extend(
    classname = "B",
    static = list(
      number = 20
    ),
    public = list(
      constructor = function() {
        super$constructor()
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  a_1 <- A$new(input = input, output = output, session = session)
  expect_equal(a_1$static$number, 11)
  a_2 <- A$new(input = input, output = output, session = session)
  expect_equal(a_1$static$number, 12)
  b_1 <- B$new(input = input, output = output, session = session)
  expect_equal(a_1$static$number, 12)
  expect_equal(b_1$static$number, 21)
  b_2 <- B$new(input = input, output = output, session = session)
  expect_equal(a_1$static$number, 12)
  expect_equal(b_1$static$number, 22)
})


## ----------------------------------------------------------------------------
test_that('it should update static field', {
  A <- Component$extend(
    classname = "A",
    static = list(
      number = 10,
      value = NULL
    ),
    public = list(
      inc = function() {
        static$number = static$number + 1
      },
      set = function(value) {
        static$value = value
      }
    )
  )
  B <- A$extend(
    classname = "B",
    static = list(
      number = 20,
      value = NULL
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  a_1 <- A$new(input = input, output = output, session = session)
  a_1$inc()
  a_2 <- A$new(input = input, output = output, session = session)
  expect_equal(a_1$static$number, 11)
  a_2$inc()
  expect_equal(a_1$static$number, 12)
  expect_equal(a_2$static$number, 12)
  env <- new.env()
  env$x <- 10
  a_1$set(env)
  expect_true(identical(a_2$static$value, a_1$static$value))
  expect_true(identical(a_2$static$value$x, a_1$static$value$x))
  b <- B$new(input = input, output = output, session = session)
  expect_equal(b$static$number, 20)
  b$inc()
  expect_equal(b$static$number, 21)
  ## same value as previous tests
  expect_equal(a_1$static$number, 12)
  expect_equal(a_2$static$number, 12)
})

## ----------------------------------------------------------------------------
test_that('it should compose objects', {
  A <- Component$extend(
    classname = "A",
    public = list(
      name = NULL,
      constructor = function(name) {
        self$name <- name
      }
    )
  )
  B <- Component$extend(
    classname = "B",
    public = list(
      constructor = function() {
        A$new(component.name = "a_1", name = "foo", parent = self)
        A$new(component.name = "a_2", name = "bar", parent = self)
        A$new(component.name = "a_3", name = "baz", parent = self)
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  b <- B$new(input = input, output = output, session = session)
  expect_equal(b$children$a_1$id, "A1")
  expect_equal(b$children$a_2$id, "A2")
  expect_equal(b$children$a_3$id, "A3")
  expect_equal(b$children$a_1$ns("foo"), "A1_foo")
  expect_equal(b$children$a_2$ns("foo"), "A2_foo")
  expect_equal(b$children$a_3$ns("foo"), "A3_foo")
  expect_equal(b$children$a_1$name, "foo")
  expect_equal(b$children$a_2$name, "bar")
  expect_equal(b$children$a_3$name, "baz")
})
