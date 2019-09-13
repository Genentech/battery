#' Event emitter is inspired by multiple implementation in JavaScript
#' it can be used indepentely of battery events to send data from one component
#' to different one, without the need to broadcast and emit events
#' it can be used with services, in addition to can also be used as reactive
#' values in shiny reactive context like renderUI or battery component render
#' function
#'
#' example:
#' e <- EventEmitter$new()
#' e$on("sessinCreated", function(value, emitter) {
#'   self$updateSessionComponent(value$name)
#' })
#'
#' and in different part of the application
#'
#' e$emit("sessinCreated", list(name = "My Session"))
#'
EventEmitter <- R6::R6Class(
  "EventEmitter",
  private = list(
    handlers = list(),
    observers = list(),
    ## -------------------------------------------------------------------------
    ## :: remove observe Event
    ## -------------------------------------------------------------------------
    unbind = function(event) {
      private$handlers[event] <- NULL
      private$observers[[event]]$observer$destroy()
      private$observers[event] <- NULL
    },
    ## -------------------------------------------------------------------------
    ## :: add new observe Event
    ## -------------------------------------------------------------------------
    bind = function(event, ...) {
      ## this function is guarded outside, double sanity check
      if (is.null(private$handlers[[event]])) {
        private$handlers[[event]] <- list()
      }
      if (is.null(private$observers[[event]])) {
        private$observers[[event]] <- battery::observeEvent(self$events[[event]], {
          value <- self$events[[event]]
          lapply(private$handlers[[event]], function(handler) {
            handler(value, self)
          })
        }, ...)
      }
    }
  ),
  public = list(
    events = NULL,
    ## -------------------------------------------------------------------------
    initialize = function() {
      self$events <- shiny::reactiveValues()
    },
    ## -------------------------------------------------------------------------
    ## :: create new observer if don't exists and add handler to the list
    ## :: to that observer, we use single observer so ... will be of no use
    ## :: on next handler on signle event, we keep it just in case it may be of use
    ## -------------------------------------------------------------------------
    on = function(event, handler, ...) {
      if (is.null(private$handlers[[event]])) {
        private$bind(event, ...)
      }

      private$handlers[[event]] <- append(
        private$handlers[[event]],
        handler
      )
    },
    ## -------------------------------------------------------------------------
    ## :: emit event add trigger all handlers added by on
    ## -------------------------------------------------------------------------
    emit = function(name, data = NULL) {
      if (is.null(data)) {
        self$events[[name]] <- shiny::isolate(!self$events[[name]])
      } else {
        data$timestamp <- as.numeric(Sys.time())*1000
        self$events[[name]] <- data
      }
    },
    ## -------------------------------------------------------------------------
    ## :: remove single handler or all handlers and observer
    ## :: if handler is used and it's last handler observer is also removed
    ## -------------------------------------------------------------------------
    off = function(event, handler = NULL) {
      if (is.null(handler)) {
        private$unbind(event)
      } else if (is.function(handler)) {
        idx <- which(sapply(private$handlers[[event]], identical, handler))
        private$handlers[[event]][idx] <- NULL
        if (length(private$handlers[[event]]) == 0) {
          private$unbind(event)
        }
      }
    }
  )
)
