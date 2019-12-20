library(testthat)
library(shiny)
library(battery)

context('test_sessions')

battery::useMocks()


## ref: https://stackoverflow.com/a/42734863/387194
rand.string <- function(n = 5000) {
  a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
}

rand.token <- function(n = 5000) {
  rand.string(n)[1]
}

## ----------------------------------------------------------------------------
test_that('it should reset counter for different users', {

  # basic components
  A <- component(
    classname = 'A'
  )
  B <- component(
    classname = 'B'
  )
  C <- B$extend(classname = "C")
  # mocks
  session <- battery::Session$new(token = rand.token())
  print(session$token)
  input <- activeInput()
  output <- activeOutput()

  # instances
  a_1 <- A$new(
    input = input,
    output = output,
    session = session
  )

  b_1 <- B$new(parent = a_1)
  c_1 <- C$new(parent = b_1)

  a_2 <- A$new(
    input = input,
    output = output,
    session = session
  )

  b_2 <- B$new(parent = a_2)
  c_2 <- C$new(parent = b_2)

  session <- battery::Session$new(token = rand.token())

  a_3 <- A$new(
    input = input,
    output = output,
    session = session
  )

  b_3 <- B$new(parent = a_3)
  c_3 <- C$new(parent = b_3)

  ## 3rd classes have counter starts from 1 because they have different token

  expect_equal(a_1$id, "A1")
  expect_equal(a_2$id, "A2")
  expect_equal(a_3$id, "A1")

  expect_equal(b_1$id, "B1")
  expect_equal(b_2$id, "B2")
  expect_equal(b_3$id, "B1")

  expect_equal(c_1$id, "C1")
  expect_equal(c_2$id, "C2")
  expect_equal(c_3$id, "C1")

  session$destroy()
})
## ----------------------------------------------------------------------------
test_that('should create same counter for same session', {

  # basic components
  A <- component(
    classname = 'A'
  )
  B <- component(
    classname = 'B'
  )
  C <- B$extend(classname = "C")

  # mocks
  session <- battery::Session$new(token = rand.token())
  input <- activeInput()
  output <- activeOutput()

  # instances
  a_1 <- A$new(
    input = input,
    output = output,
    session = session
  )

  b_1 <- B$new(parent = a_1)
  c_1 <- C$new(parent = b_1)

  a_2 <- A$new(
    input = input,
    output = output,
    session = session
  )

  b_2 <- B$new(parent = a_2)
  c_2 <- C$new(parent = b_2)

  expect_equal(a_1$id, "A1")
  expect_equal(a_2$id, "A2")

  expect_equal(b_1$id, "B1")
  expect_equal(b_2$id, "B2")

  expect_equal(c_1$id, "C1")
  expect_equal(c_2$id, "C2")

  session$destroy()
})

## ----------------------------------------------------------------------------
test_that('it should share static env', {
  A <- component(
    classname = 'A'
  )
  B <- component(
    classname = 'B'
  )
  C <- B$extend(classname = "C")

  # mocks
  session <- battery::Session$new(token = rand.token())
  input <- activeInput()
  output <- activeOutput()

  # instances
  a_1_1 <- A$new(
    input = input,
    output = output,
    session = session
  )
  a_1_2 <- A$new(
    input = input,
    output = output,
    session = session
  )


  b_1_1 <- B$new(parent = a_1_1)
  b_1_2 <- B$new(parent = a_1_1)
  c_1_1 <- C$new(parent = b_1_1)
  c_1_2 <- C$new(parent = b_1_1)

  ## same satic env for single token
  expect_true(identical(a_1_1$static, a_1_2$static))
  expect_true(identical(b_1_1$static, b_1_2$static))
  expect_true(identical(c_1_1$static, c_1_2$static))

  session <- battery::Session$new(token = rand.token())

  a_2_1 <- A$new(
    input = input,
    output = output,
    session = session
  )
  a_2_2 <- A$new(
    input = input,
    output = output,
    session = session
  )

  b_2_1 <- B$new(parent = a_2_1)
  b_2_2 <- B$new(parent = a_2_1)
  c_2_1 <- C$new(parent = b_2_1)
  c_2_2 <- C$new(parent = b_2_1)

  ## same satic env for single token
  expect_true(identical(a_2_1$static, a_2_2$static))
  expect_true(identical(b_2_1$static, b_2_2$static))
  expect_true(identical(c_2_1$static, c_2_2$static))

  ## different satic env for across the tokens
  expect_false(identical(a_1_1$static, a_2_2$static))
  expect_false(identical(b_1_1$static, b_2_2$static))
  expect_false(identical(c_1_1$static, c_2_2$static))
})

## ----------------------------------------------------------------------------
test_that('it should not throw on same service for different roots', {
    # basic components
  A <- component(
    classname = 'A'
  )
  # service
  Service <- R6::R6Class(
    classname = 'Service',
    public = list(
      test = function() {
        called <<- TRUE
      }
    )
  )
  service <- Service$new()

  session <- battery::Session$new(token = rand.token())
  input <- activeInput()
  output <- activeOutput()

  a_1 <- A$new(
    input = input,
    output = output,
    session = session
  )
  a_1$addService("foo", service)

  a_2 <- A$new(
    input = input,
    output = output,
    session = session
  )
  a_2$addService("foo", service)

  expect_true(identical(a_1$services$foo, service))
  expect_true(identical(a_2$services$foo, service))

})

## ----------------------------------------------------------------------------
battery::clearMocks()
