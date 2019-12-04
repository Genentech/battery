
HttpService <- R6::R6Class(
  "HttpService",
  public = list(
    fetch = function(url) {
      req <- httr::GET(url)
      httr::content(req, "text")
    }
  )
)

App <- battery::component(
  "App",
  public = list(
    constructor = function() {
      self$addService("http", HttpService$new())
      self$addService("notify", battery::EventEmitter$new())

      MainChild$new(parent = self, component.name = "main")

      top <- OtherChild$new(parent = self, component.name = "child1")
      bottom <- OtherChild$new(parent = self, component.name = "child2")

      self$output[[self$ns("top")]] <- renderUI({
        top$render()
      })

      self$output[[self$ns("bottom")]] <- renderUI({
        bottom$render()
      })

    },
    render = function() {
      shiny::tags$div(
        ## main is static so it can b rendered directly
        self$children$main$render(),
        ## other children are reactive so it's better to not-render the whole tree on update
        shiny::tags$div(
          shiny::tags$h1("Pages"),
          shiny::uiOutput(self$ns("top")),
          shiny::uiOutput(self$ns("bottom"))
        )
      )
    }
  )
)
MainChild <- battery::component(
  "MainChild",
  public = list(
    constructor = function() {
      self$on(self$ns("fetch_button"), function() {
        print("click1")
        html <- self$services$http$fetch("https://google.com")
        self$services$notify$emit("httpContent", html)
      }, input = TRUE)
      self$on(self$ns("clear_button"), function() {
        print("click2")
        self$services$notify$emit("clear")
      }, input = TRUE)
    },
    render = function() {
      shiny::tagList(
        shiny::actionButton(self$ns("fetch_button"), "fetch"),
        shiny::actionButton(self$ns("clear_button"), "clear")
      )
    }
  )
)
reactiveProp <- function(name, obj) {
  values <- reactiveValues(value = obj[[name]])
  makeActiveBinding(name, env=obj, fun=function(v) {
    if (missing(v))
      values$value
    else
      values$value <- v
  })
}

OtherChild <- battery::component(
  "OtherChild",
  public = list(
    reactive = NULL,
    style = c(
      'color: rebeccapurple',
      'font-weight: bold',
      'border-bottom: 1px solid rebeccapurple',
      'margin-bottom: 20px'
    ),
    constructor = function() {
      self$reactive <- shiny::reactiveValues(html = NULL)
      self$services$notify$on("httpContent", function(data) {
        print("http")
        ## when reactive value is changed it will trigger render because it's in renderUI
        self$reactive$html <- data
      })
      self$services$notify$on("clear", function() {
        print("clear")
        self$reactive$html <- NULL
      })
    },
    render = function() {
      shiny::tagList(
        shiny::tags$div(style = paste(self$style, collapse = ";"), paste("Object", self$id)),
        shiny::HTML(self$reactive$html)
      )
    }
  )
)

ui <- fluidPage(
  uiOutput('output')
)

server <- function(input, output, session) {

  ## Root component that don't have parent need to be called with input output and session.
  root <- App$new(
    input = input,
    output = output,
    session = session
  )
  output$output <- shiny::renderUI({
    root$render()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
