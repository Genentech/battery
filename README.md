```
 _____     _   _
| __  |___| |_| |_ ___ ___ _ _
| __ -| .'|  _|  _| -_|  _| | |
|_____|__,|_| |_| |___|_| |_  |
                          |___|
```

Battery - R6Class based component architecture for Shiny apps with testing framework.
The components design is based on AngularJS that can emit event from root it it's children
and broadcast events from child to parents.

# Components

Components are based on R6Class to create new component you call battery::component function or use
$extend method on any component. You can also create new R6 class that inherit from battery::Component
but this should not be used because you will lost access to static variable inside R6Class methods.
(you will need to access them using `self$static`)

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
