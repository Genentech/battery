source("../../R/components.R")
source("../../R/mock.R")

test_that('it should create component using R6Class', {
  A <- R6::R6Class(
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
test_that('it should create component using component function', {
  A <- component(
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



test_that('it should extend base component using extend static method', {
  A <- Component$extend(
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

test_that('it should create child component', {
  A <- Component$extend(
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

test_that('it should create static fields', {
  A <- Component$extend(
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

test_that('it should call parent constructor if no constuctor in child component', {
  A <- Component$extend(
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
  B <- A$extend(public = list(
    x = 10
  ))
  b <- B$new(input = input, output = output, session = session)
  expect_equal(b$name, "foo")
  expect_equal(b$x, 10)
})

test_that('it should create new static field in child component', {
  A <- Component$extend(
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
