library(shiny)

App <- battery::component(
  classname = "application",
  public = list(
    constructor = function() {
      self$on(self$ns("fatal"), function(value) {
        print("button fatal clicked")
        battery::signal(
          class = "fatal",
          message = paste("Fatal error", value)
        )
      }, input = TRUE)

      self$on(self$ns("error"), function(value) {
        print(paste("button error clicked", value))
        self$foo()
      }, input = TRUE)

      self$on(self$ns("silent"), function(value) {
        print(paste("button silent clicked", value))
        battery::signal(
          "silent",
          message = paste("msg:", value),
          data = list(
            pause = !self$input[[ self$ns('checkbox') ]]
          )
        )
        print("This is after the signal")
      }, input = TRUE)
    },

    foo = function() {
      self$bar()
    },

    bar = function() {
      stop('This is Error from stop')
    },

    render = function() {
      tagList(
        actionButton(
          self$ns("fatal"),
          "Generate fatal error"
        ),
        actionButton(
          self$ns("silent"),
          "Generate silent signal"
        ),
        checkboxInput(
          self$ns('checkbox'),
          "Continue silent execution"
        ),
        actionButton(
          self$ns("error"),
          "Generate R error with stop"
        )
      )
    }
  )
)

ui <- fluidPage(
  uiOutput("app")
)

server <- function(input, output, session) {
  battery::exceptions(
    list(
      fatal = function(cond) {
        print(cond$message)
        battery::error("I also want error")
      },

      silent = function(cond) {
        print(cond$message)
        if (cond$pause) {
          battery::pause()
        }
      },

      error = function(cond) {
        battery::stop()
      }
    )
  )

  app <- App$new(input, output, session)

  output$app <- renderUI({
    app$render()
  })


}

shinyApp(ui, server)
