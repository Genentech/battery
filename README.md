```
 _____     _   _
| __  |___| |_| |_ ___ ___ _ _
| __ -| .'|  _|  _| -_|  _| | |
|_____|__,|_| |_| |___|_| |_  |
                          |___| v 0.1.1
```

Battery - R6Class based component architecture for Shiny apps with testing framework.
The components design is based on AngularJS that can emit event from root it it's children
and broadcast events from child to parents. It give better structure of non trivial shiny apps,
that need to have lots of differnt parts.

# Components

Components are based on R6Class. To create new component you should call `battery::component` function or use
$extend method on any component. You can also create new R6 class that inherit from `battery::Component`
but this should not be used because you will loose access to static variables inside R6Class methods
(you will need to access them using `self$static`) and will not be able to use spy parameter to check
method calls while testing your components.


```R
server <- function(input, output, session) {
  Button <- battery::component(
    classname = "Button",
    public = list(
      count = NULL,
      label = NULL,
      ## constructor is artifical method so you don't need to call super
      ## which you may forget to add
      constructor = function(label, canEdit = TRUE) {
        self$label <- label
        self$connect("click", self$ns("button"))
        self$count <- 0
        self$on("click", function(e = NULL, target = NULL) {
          self$count <- self$count + 1
        }, enabled = canEdit)
        self$output[[self$ns("buttonOutput")]] <- shiny::renderUI({
          self$events$click
          tags$div(
            tags$span(self$count),
            actionButton(self$ns("button"), "click")
          )
        })
      },
      render = function() {
        tags$div(
          class = "button-component",
          tags$p(class = "buton-label", self$label),
          shiny::uiOutput(self$ns("buttonOutput"))
        )
      }
    )
  )

  HelloButton <- Button$extend(
    classname = "HelloButton",
    public = list(
      constructor = function() {
        super$constructor("hello")
      }
    )
  )

  Panel <- battery::Component$extend(
    classname = "Panel",
    public = list(
      title = NULL,
      constructor = function(title) {
        self$title <- title
        Button$new(label = "click Me", component.name = "btn1", parent = self)
        HelloButton$new(component.name = "btn2", parent = self)
        self$output[[self$ns("button")]] <- shiny::renderUI({
          tags$div(
            self$children$btn1$render(),
            self$children$btn2$render()
          )
        })
      },
      render = function() {
        tags$div(
          tags$h2(self$title),
          tags$div(shiny::uiOutput(self$ns("button")))
        )
      }
    )
  )

  

  
  App <- battery::component(
    classname = "App",
    public = list(
      constructor = function() {
        ## for root node you don't need to use ns to create namespace but you can
          a <- Panel$new(title = "A", component.name = "panelA", parent = self)
          b <- Panel$new(title = "B", component.name = "panelB", parent = self)
        self$output[[ self$ns("root") ]] <- shiny::renderUI({
          tags$div(
            a$render(),
            b$render()
          )
        })
      },
      render = function() {
        tags$div(
          titlePanel('Shiny App using Battery R package'),
          mainPanel(shiny::uiOutput(self$ns("root")))
        )
      }
    )
  )
}
```

this is how you connect components to normal code, you can create one component
App that will be added to single output using shiny::renderUI and the reset of the application
can use components.
  
```R
## Root component that don't have parent need to be called with input output and session.
app <- App$new(input = input, output = output, session = session)
output$root <- renderUI({
  app$render()
})
```


Every other component only need parent named argument and you don't need to specify those parameters
in `constructor` because R6Class use `initialize` function as constructor and battery use handy
`constructor` function so you don't need to remember to call super, and only use extra parameters
you need when you create components.

If you're creating child component you have two options:

```R
btn <- Button$new(parent = self)
self$appendChild("button", btn)
```

or

```R
Button$new(parent = self, component.name = "button")
```

Second is shortcut. You need use either so you have proper tree of components so event propagaion
will work properly.


See example/shiny-app.R for simple shiny app that you can run to test battery R pacakge.


## Events

Nice feature of battery is that create structure for events, just like in AngularJS (which battery is inspired by) you
can call `$emit` and `$broadcast` methods to send events to parets and to all children. Future versions may have notion of
services but right now to send to siblings you need to emit the event to parent and in parent broadcast the event it all children.

# Testing Components

when testing Components you can use this mocks instead of running whole shiny app. So you can
test single component in isolation.

Here is quick summary for how to use testing framework. See `./tests/` directory to see how
to tests you own components.

## Mocks

You should never need to use mocks directly but here is examples how to use it, for testing components see next section

creating empty input

```R
input <- activeInput()
```

input with single ative binding

```R
input <- activeInput(foo = function(value) {
  if (missing(value)) {
    self[["__foo"]]
  } else {
    self[["__foo"]] <- value
  }
})
```

NULL will create default setter/getter and inital active binding

```R
input <- activeInput(foo = NULL)
```

### ObserveEvent mock

```R
observeEvent(input$foo, {
  print(paste0("value set to ", input$foo))
})
```

observeEvent just call input$on and it will create listener for reactive value
it will not create active binding so you need to call new first just in case you
don't know what the value is before observeEvent is called (can be called in component
constructor with `self$ns` as name the you can ten get call self$ns on component to create
actual biding after component is created)


this will create active binding with default setter/getter

```R
input$new("foo")
```

```R
input$new(component$ns("save"))
```

reactive value with that may have different logic here same code as default


```R
input$new("foo", function(value) {
   if (missing(value)) {
    self[["__foo"]]
  } else {
    self[["__foo"]] <- value
  }
})
```

if you set the value

```R
input$foo <- 10
```

observer expression will be avaluated also input$listeners will have data for each listener
that will have data about each call to each listener with old and new values

usually there will be single listener for single active value

### Output

Output and connection between input and output can be explained using this example code

```R
input <- activeInput(foo = NULL)


input$foo <- 100

output <- activeOutput(bar = NULL)

output$bar <- renderUI({ input$foo + 10 })

print(output$bar) ## 110

input$foo <- 200

print(input$foo) ## 200
print(output$bar) ## 210
```

## Testing components

## Mock reactive shiny data

when you're writing tests first you need to call


```
library(testthat)
library(shiny)

battery::useMocks()
```

At the root of the test. This will import testthat and shiny and `useMocks()` will patch the functions
that came from shiny with proper mocks created by battery, Mocks give better way to test the active variables.
You can inspect `output` and `input` without an any issue usually related to shiny apps (e.g. require of `isolate`).

Also Battery have moks for input and output that you can use to test your components. Just create session
(base battery component don't use it) `activeInput` and `activeOutput` and create instance of component
using `battery::component` or `battery::Component$extend`.

```
test_that('it should work', {
  session <- list()
  input <- activeInput()
  output <- activeOutput()
  x <- Comp$new(input = input, output = output, session = session)
})
```

if your component have more arguments pass them to the constructor.

now if component call something like this:

```R
Comp <- battery::component(
  public = list(
    constructor = function() {
      self$output[[self$ns("xxx")]] <- renderUI({
        paste0("you typed: ", self$input$foo)
      })
    }
  )
)
```

you can call:

```R
output$new(x$ns("xxx"))
```

after constructor is called, so this renderUI can be in constructor (where they usually be created) same is with active input
active property with `$new` can be created after component is created so you can get namespace id from component.

```
input$new("foo")
```

the order doesn't matter you can create input before output and vice versa.

After this if you call:

```R
input$foo <- "hello"
```

the output will be updated and `output[[self$ns("xxx")]]` will have string `"you typed: hello"`.

## Spies

If you create your component with spy option set to `TRUE` it will spy on all the methods. Each time a method
is called it will be in component$.calls named list, were each function will have list of argument lists

e.g.

```R
t <- TestingComponent$new(input = input, output = output, session = session, spy = TRUE)

t$foo(10)
t$foo(x = 20)
expect_that(t$.calls$foo, list(list(10), list(x = 20)))
```

constructor is also on the list of `.calls`, everything except of functions that are in base component R6 class (this may change in
the future if will be needed).
