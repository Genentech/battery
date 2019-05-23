context("test_events")

test_that('it should emit event', {
  InputComponent <- battery::component(
    classname = "InputComponent",
    public = list(
      constructor = function() {},
      run = function(arg) {
        self$emit("foo", arg)
      },
      foo = function(x) {

      }
    ),
    spy = TRUE
  )
  args <- list()
  TestingComponent <- battery::component(
    classname = "TestingComponent",
    public = list(
      constructor = function() {
        InputComponent$new(component.name = "c", parent = self)
        self$on("foo", function(value, target) {
          self$foo(value)
          args <<- list(value)
        })
      },
      foo = function(x) { }
    ),
    spy = TRUE
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  t <- TestingComponent$new(input = input, output = output, session = session)
  t$children$c$run(100)
  expect_equal(args, list(100))
  expect_equal(t$.calls$foo, list(list(100)))
  t$children$c$run(200)
  expect_equal(args, list(200))
  expect_equal(t$.calls$foo, list(list(100), list(200)))

})

test_that('should emit events to parent of parent', {
  InputComponent <- battery::component(
    classname = "InputComponent",
    public = list(
      constructor = function() {},
      run = function(arg) {
        self$emit("foo", arg)
      }
    )
  )
  TestingComponent <- battery::component(
    classname = "TestingComponent",
    public = list(
      constructor = function() {
        InputComponent$new(component.name = "c", parent = self)
      }
    )
  )
  args <- list()
  ParentTestingComponent <- battery::component(
    classname = "TestingComponent",
    public = list(
      constructor = function() {
        TestingComponent$new(component.name = "tc", parent = self)
        self$on("foo", function(value, target) {
          self$foo(value)
          args <<- list(value)
        })
      },
      foo = function(x) { }
    ),
    spy = TRUE
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()

  t <- ParentTestingComponent$new(input = input, output = output, session = session)

  t$children$tc$children$c$run(100)
  expect_equal(args, list(100))
  expect_equal(t$.calls$foo, list(list(100)))
  t$children$tc$children$c$run(200)
  expect_equal(args, list(200))
  expect_equal(t$.calls$foo, list(list(100), list(200)))
})


test_that('it should broadcast event', {

  NestedComponent <- battery::component(
    classname = "NestedComponent",
    private = list(
      x = NULL,
      foo = function(x) {
        private$x <- x
      }
    ),
    public = list(
      constructor = function() {
        self$on("foo", function(value, target) {
          private$foo(value)
        })
      }
    )
  )

  InputComponent <- battery::component(
    classname = "InputComponent",
    public = list(
      constructor = function() {
        NestedComponent$new(component.name = "nc", parent = self)
      },
      run = function(arg) {
        self$broadcast("foo", arg)
      }
    )
  )
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  ic <- InputComponent$new(input = input, output = output, session = session)
  ic$run(100)
  ic$run("foo")
  args <- lapply(ic$children$nc$events$.calls$foo, function(x) { x[["new"]][["value"]] })
  expect_equal(args, list(100, "foo"))
})
