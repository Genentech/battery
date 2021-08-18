library(shiny)
library(battery)

App <- battery::component(
  classname = "application",
  public = list(
    constructor = function() {
      self$on("btn_fatal", function() {
        print("button clicked")
        battery::signal(
          class = "fatal",
          message = paste("Fatal error", self$input$btn_fatal)
        )
      }, input = TRUE) 
    },
    
    render = function() {
      actionButton(
        "btn_fatal",
        "Generate fatal error"
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
        stop()
      },
      
      error = function(cond) {
        print("This is error handler")
        stop(cond)
      }
    )
  )
  
  app <- App$new(input, output, session)
  
  output$app <- renderUI({
    app$render()
  })
  
  
}

shinyApp(ui, server)