<div align="center">

# ![Battery R package Logo](https://github.com/Genentech/battery/blob/master/inst/extra/battery-logo.svg?raw=true)

[![test](https://github.com/Genentech/battery/actions/workflows/test.yaml/badge.svg)](https://github.com/Genentech/battery/actions/workflows/test.yaml)
[![Coverage Status](https://coveralls.io/repos/github/Genentech/battery/badge.svg?branch=master)](https://coveralls.io/github/Genentech/battery?branch=master)
[![battery GitHub repo](https://img.shields.io/badge/github-batery-orange?logo=github)](https://github.com/Genentech/battery)
[![LICENSE MIT](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/Genentech/battery/blob/master/COPYING)

</div>

## R6Class-based component architecture framework for Shiny apps

The component's design is based on AngularJS, which can emit events from the root to its children
and broadcast events from children to parents. It gives better structure to non-trivial Shiny apps
that need to have many different parts.

## Installation

From R:

```R
devtools::install_github("Genentech/battery")
```

From the source:

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
    self$label <- label
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
Released under the MIT License. See [COPYING](https://github.com/Genentech/battery/blob/master/COPYING) for the full text.
