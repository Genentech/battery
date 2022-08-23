<h1 align="center">
  <img src="https://github.com/Genentech/battery/blob/master/inst/extra/battery-logo.svg?raw=true"
       alt="Battery R package Logo" />
</h1>

[R6Class based component architecture framework for Shiny apps](https://github.com/Genentech/battery)


The components design is based on AngularJS that can emit event from root it it's children
and broadcast events from child to parents. It give better structure of non trivial shiny apps,
that need to have lots of different parts.

## Installation

```sh
git clone https://github.com/Genentech/battery.git
R CMD INSTALL battery
```

## Basic usage

```R
Button <- battery::component(
  classname = "Button",
  label = NULL,
  constructor = function(label = NULL) {
    self$label = label
  },
  render = function() {
    shiny::tags$button(self$label)
  }
)

App <- battery::component(
  classname = "App",
  public = list(
    constructor = function() {
      btn <- Button$new(label = "Click me", parent = self, component.name = "button")
      self$output[[ self$ns("root") ]] <- shiny::renderUI({
        shiny::tags$div(
          shiny::tags$p("click the button"),
          btn$render()
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
```

## Documentation

For full documentation see [Battery Components](vignettes/battery-components.Rmd) Vignette.

You can also read tutorial about the framework features at DEV.to:<br/>
[Architecture for Non-Trivial R Shiny Applications](https://dev.to/jcubic/architecture-for-non-trivial-r-shiny-applications-3816)


## Contributors
* [Jakub T. Jankiewicz](https://github.com/jcubic) - main author
* Michał Jakubczak

## License
Copyright (c) 2019-2021 Genentech, Inc.<br/>
Released under MIT license
