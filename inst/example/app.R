
library(shiny)


ui <- fluidPage(
  uiOutput('output')
)

Foo <- R6::R6Class(
  "Foo",
  private = list(
    trigger = function(name, data = NULL) {
      if (name %in% ls(self$events)) {
        print(shiny::isolate(self$events[[name]]))
        if (is.null(data)) {
          self$events[[name]] <- shiny::isolate({
            if (is.logical(self$events[[name]])) {
              print("a")
              !self$events[[name]]
            } else if (is.null(self$events[[name]]$value)) {
              print("b")
              TRUE
            } else {
              print("c")
              !self$events[[name]]$value
            }
          })
        } else {
          print("d")
          new.data <- list(
            timestamp = as.numeric(Sys.time())*1000,
            value = data$value
          )
          print(paste("XXXXXXXXXXX", identical(shiny::isolate(self$events[[name]]), new.data)))
          #self$events[[name]] <- new.data
          
          private$force(function() {
            self$events[[name]] <- new.data
          })
          
        }
        print(shiny::isolate(self$events[[name]]))
      }
    },
    force = function(fn) {
      observeEvent(NULL, {
        invalidateLater(0)
        fn()
      }, once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE)
    }
  ),
  public = list(
    events = NULL,
    input = NULL,
    session = NULL,
    initialize = function(input, session, ...) {
      #session$manageInputs(list(shinyHack = TRUE))
      #observeEvent(input$shinyHack, {
      #})
      self$input <- input
      self$session <- session
      self$events <- new.env()
      if (!is.null(self$constructor)) {
        self$constructor(...)
      }
    },
    ## ------------------------------------------------------------------------
    createEvent = function(name, value = NULL) {
      print(paste("CREATE: ", name))
      if (!name %in% ls(self$events)) {
        shiny::makeReactiveBinding(name, env = self$events)
        if (is.logical(value) && value) {
          self$events[[name]] <- TRUE
        } else {
          data <- list(
            value = value,
            timestamp = as.numeric(Sys.time())*1000
          )
          self$events[[name]] <- data
        }
      }
    },
    ## ------------------------------------------------------------------------
    on = function(event, handler, init = FALSE, ...) {
      self$createEvent(event)

      shiny::observeEvent(self$events[[event]], {
        print(paste("::::", event))
        data <- self$events[[event]]
        ## invoke handler function with only argument it accept
        tryCatch({
          if (is.null(data) || is.logical(data)) {
            battery:::invoke(handler, NULL, NULL)
          } else {
            battery:::invoke(handler, data[["value"]], data[["target"]])
          }
        }, error = function(cond) {
          if (!inherits(cond, "shiny.silent.error")) {
            message(paste0("throw in ", self$id, "::on('", event, "', ...)"))
            message(cond$message)
            traceback(cond)
            stop(cond)
          }
        })
      }, ignoreInit = !init, ...)
    },
    ## ------------------------------------------------------------------------
    working = function() {
      shiny::makeReactiveBinding("bar", env = self$events)
      self$events$bar <- 10
      shiny::observeEvent(self$events$bar, {
        print("foo bar")
        print(self$events$bar)
      })
      self$events$bar <- 20
    },
    ## ------------------------------------------------------------------------
    not.working = function() {
      self$on("bar", function(value) {
        print(paste("CALL: ", value))
      })
      private$trigger("bar", list(value = "NORMAL"))
      
      self$session$manageInputs(list(shinyHack = TRUE))
      observeEvent(self$input$shinyHack, {
        private$trigger("bar", list(value = "WITH HACK"))
      })
    }
  )
)

server <- function(input, output, session) {

  ## Root component that don't have parent need to be called with input output and session.
  root <- Foo$new(input = input, session = session)
  root$working()
  root$not.working()
}

# Run the application
shinyApp(ui = ui, server = server)
