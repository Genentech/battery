```
 _____     _   _
| __  |___| |_| |_ ___ ___ _ _
| __ -| .'|  _|  _| -_|  _| | |
|_____|__,|_| |_| |___|_| |_  |
                          |___|
```

Battery - R6Class based component architecture for Shiny apps with testing framework

# Components

Components are based on R6Class to create new component you create new R6 class that inherit
from battery::Component

```R
Button <- R6Class("Button",
  inherit = batter::Component,
  public = list(
    static = {
      e <- env.new()
      e$count <- 0
      e
    },
    count = NULL,
    ## constructor is artifical method so you don't need to call super
    ## which you may forget to add
    constructor = function(canEdit = TRUE) {
      self$connect("click", self$ns("button"))
      self$count <- 0
      self$on("click", function(e = NULL, target = NULL) {
        self$count <- self$count + 1
      }, enabled = canEdit)
      self$output[[self$ns("buttonOutput")]] <- renderUI({
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
        uiOutput(self$ns("buttonOutput"))
      )
    }
  )
)

Panel <- R6Class("Panel",
  inherit = batter::Component,
  public = list(
    static = {
      e <- new.env()
      e$count <- 0
      e
    },
    constructor = function(title) {
      self$title <- title
      btn <- Button$new(parent = self)
      self$appendChild("button", btn)
      self$output[[self$ns("button")]] <- renderUI({
        btn$render()
      })
    },
    render = function() {
      tags$div(
        tags$h2(self$title),
        tags$div(uiOutput(self$ns("button")))
      )
    }
  )
)
```

Root component that don't have parent need to be called with input output and session.

```R
root <- Root$new(input = input, output = output, session = session, canEdit = FALSE)
output$root <- renderUI({
  root$render()
})
```

Every other component only need parent attribute and you don't need to specify those parameters
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
work properly.

# Testing Components

when testing Components you can use this mocks instead of running whole shiny app. So you can
test single component in isolation.

Here is quick summary for how to use testing framework. See `./tests/` directory to see how
to tests you own components.

## Mocks

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
