library(shiny)


ui <- fluidPage(
  uiOutput('app')
)

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
      self$on("click", function() {
        self$count <- self$count + 1
        if (self$count %% 2 == 0) {
          self$throw.error()
        }
      }, enabled = canEdit)
      self$output[[self$ns("buttonOutput")]] <- shiny::renderUI({
        self$events$click
        tags$div(
          tags$span(self$count),
          actionButton(self$ns("button"), "click")
        )
      })
    },
    throw.error = function() {
      x()
    },
    render = function() {
      tags$div(
        class = "button-component",
        tags$p(class = "button-label", self$label),
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

Panel <- battery::component(
  classname = "Panel",
  public = list(
    title = NULL,
    constructor = function(title) {
      self$title <- title
      Button$new(label = "click Me", component.name = "btn1", parent = self)
      HelloButton$new(component.name = "btn2", parent = self)
      ## inside each component you have access to output, input and session in self$
      ## to make component separated if more then one is used use self$ns to create unique id
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

server <- function(input, output, session) {

  ## Root component that don't have parent need to be called with input output and session.
  root <- App$new(
    input = input,
    output = output,
    session = session,
    error = function(cond, details) {
      message(cond$message)
      if (details$type == "method") {
        message(paste("  thrown from", details$name, "in", details$id))
      }
      return(FALSE)
    }
  )
  root$logger(c('info'), function(data) {
    #print(data$message)
  })

  output$app <- renderUI({
    root$render()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
