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
#' @export
EventEmitter <- R6::R6Class(
  "EventEmitter",
  private = list(
    handlers = list(),
    observers = list(),
    ## -------------------------------------------------------------------------
    ## :: remove observe Event
    ## -------------------------------------------------------------------------
    unbind = function(event) {
      isolate({
        private$observers[[event]]$observer$destroy()
        private$handlers[event] <- NULL
        private$observers[event] <- NULL
      })
    },
    ## -------------------------------------------------------------------------
    ## :: helper function that check how many arguments handler function accept
    ## :: to prevent unuself argument error
    ## -------------------------------------------------------------------------
    invoke = function(event, value) {
      lapply(private$handlers[[event]], function(handler) {
        battery:::invoke(handler, value)
      })
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

        uuid <- uuid::UUIDgenerate()

        private$observers[[event]] <- battery::observeEvent(self$events[[event]], {
          data <- self$events[[event]]

          if (is.null(data) || is.logical(data)) {
            private$invoke(event, data)
          } else {
            private$invoke(event, data[["value"]])
          }
        }, observerName = uuid, ...)
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
        self$events[[name]] <- shiny::isolate({
          if (is.null(self$events[[name]])) {
            TRUE
          } else {
            !self$events[[name]]
          }
        })
      } else {
        self$events[[name]] <- list(
          value = data,
          timestamp <- as.numeric(Sys.time()) * 1000
        )
      }
      if (!(name %in% names(private$handlers))) {
        print(sprintf("WARN: event `%s` ignored - no listeners", name))
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
    },
    ## -------------------------------------------------------------------------
    finalize = function() {
      lapply(names(private$handlers), function(event) {
        private$unbind(event)
      })
    }
  )
)
