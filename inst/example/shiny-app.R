library(shiny)

ui <- fluidPage(
  titlePanel('Components Demo'),
  mainPanel(uiOutput('output'))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  x <- '__'
  Button <- battery::component(
    classname = 'Button',
    private = list(
      text = NULL,
      x = NULL
    ),
    public = list(
      constructor = function(text) {
        private$text = text
      },
      render = function() {
        tags$button(paste0(private$text, x))
      }
    )
  )

  hello <- 'Hello'
  label <- 'button'

  HelloButton <- Button$extend(
    classname = 'HelloButton',
    private = list(
      label = NULL
    ),
    public = list(
      constructor = function() {
        super$constructor(hello)
      },
      render = function() {
        tags$div(
          tags$label(label),
          super$render()
        )
      }
    )
  )



  Panel <- battery::component(
    classname = 'Panel',
    private = list(
      name = NULL
    ),
    public = list(
      constructor = function(name = NULL) {
        private$name <- name
        btn <- HelloButton$new(component.name = 'btn', parent = self)
        base <- Button$new(text = name, component.name = "btn_base", parent = self)
        self$output[[self$ns('content')]] <- shiny::renderUI({
          tags$div(
            self$children$btn$render(),
            self$children$btn_base$render()
          )
        })
      },
      render = function() {
        tags$div(
          tags$h2(paste('Panel', private$name)),
          uiOutput(self$ns('content'))
        )
      }
    )
  )

  a <- Panel$new(name = "a", input = input, output = output, session = session)
  b <- Panel$new(name = "b", input = input, output = output, session = session)

  output$output <- shiny::renderUI({
    tags$div(
      a$render(),
      b$render()
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
