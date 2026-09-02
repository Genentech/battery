# Basic function to create battery components.

Use this function to create new battery class object.

## Usage

``` r
component(
  classname,
  public = NULL,
  private = NULL,
  static = NULL,
  inherit = BaseComponent,
  ...
)
```

## Arguments

- classname:

  \- name of the class as string

- public:

  \- list of public functions and properties

- private:

  \- list of private functions and properties

- static:

  \- list of fields that will stay the same for every instance of the
  component

- inherit:

  \- base class - if not specifed it will inherit from Base class
  [`BaseComponent`](https://genentech.github.io/battery/reference/BaseComponent.md)

- ...:

  \- reset option passed to
  [`R6Class`](https://r6.r-lib.org/reference/R6Class.html) constructor

## Value

[`R6Class`](https://r6.r-lib.org/reference/R6Class.html), with battery
specific methods (see
[`BaseComponent`](https://genentech.github.io/battery/reference/BaseComponent.md)).

## Examples

``` r
if (FALSE) { # \dontrun{

Button <- battery::component(
  classname = "Button",
  public = list(
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
Panel <- battery::component(
  classname = "Panel",
  public = list(
    title = NULL,
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

server <- function(input, output, session) {
   ## this is entry point into batter component tree
   root <- Panel$new(title = "Hello", input = i, output = o, session = s)
   renderUI({
       root$render()
   })
}
} # }
```
