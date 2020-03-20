#' Event emitter is inspired by multiple implementation in JavaScript
#' it can be used indepentely of battery events to send data from one component
#' to different one, without the need to broadcast and emit events
#' it can be used with services, in addition to can also be used as reactive
#' values in shiny reactive context like renderUI or battery component render
#' function
#'
#' @examples
#'
#' e <- EventEmitter$new()
#' e$on("sessionCreated", function(value, emitter) {
#'   print(value$name)
#' })
#'
#' ## and in different part of the application
#'
#' e$emit("sessionCreated", list(name = "My Session"))
#'
#' @export
EventEmitter <- R6::R6Class(
  "EventEmitter",
  private = list(
    ..spying = FALSE,
    shiny = FALSE,
    data = NULL,
    handlers = NULL,
    observers = NULL,
    ## -------------------------------------------------------------------------
    .spy = function(name, ...) {
      if (private$..spying) {
        if (is.null(self$.calls[[name]])) {
          self$.calls[[name]] <- list()
        }
        args <- list(...)
        self$.calls[[name]] <- c(self$.calls[[name]], list(args))
      }
    },
    ## -------------------------------------------------------------------------
    ## :: remove observe Event
    ## -------------------------------------------------------------------------
    unbind = function(event) {
      private$.spy("unbind", event)
      if (private$shiny) {
        shiny::isolate({
          private$observers[[event]]$destroy()
          private$handlers[event] <- NULL
          private$observers[event] <- NULL
        })
      } else {
        private$handlers[event] <- NULL
      }
    },
    ## -------------------------------------------------------------------------
    ## :: helper function that check how many arguments handler function accept
    ## :: to prevent unuself argument error
    ## -------------------------------------------------------------------------
    invoke = function(event, value) {
      private$.spy("invoke", event, value)
      lapply(private$handlers[[event]], function(handler) {
        battery:::invoke(handler, value)
      })
    },
    ## -------------------------------------------------------------------------
    ## :: function check if biding is active
    ## -------------------------------------------------------------------------
    bound = function(event) {
      if (private$shiny) {
        !is.null(private$observers[[event]])
      } else {
        event %in% names(self$events) && bindingIsActive(event, self$events)
      }
    },
    ## -------------------------------------------------------------------------
    ## :: function used to set data in emit if there are no biding
    ## :: it's also used when activeBiding is used outside of shiny
    ## -------------------------------------------------------------------------
    set = function(event, data) {
      if (private$shiny) {
        self$events[[event]] <- data
      } else {
        private$data[[event]] <- data
      }
    },
    ## -------------------------------------------------------------------------
    ## :: add new observe Event
    ## -------------------------------------------------------------------------
    bind = function(event, ...) {
      private$.spy("bind", event, ...)
      ## this function is guarded outside, double sanity check
      if (is.null(private$handlers[[event]])) {
        private$handlers[[event]] <- list()
      }
      if (!private$bound(event)) {
        if (private$shiny) {
          if (is.null(private$observers[[event]])) {
            shiny::makeReactiveBinding(event, env = self$events)

            private$observers[[event]] <- shiny::observeEvent(self$events[[event]], {
              data <- self$events[[event]]

              if (is.null(data) || is.logical(data)) {
                private$invoke(event, data)
              } else {
                private$invoke(event, data[["value"]])
              }
            }, ...)
          }
        } else {
          makeActiveBinding(event, env = self$events, fun = function(data) {
            if (missing(data)) {
              private$data[[event]]
            } else {
              if (is.null(data) || is.logical(data)) {
                private$set(event, data)
                private$invoke(event, data)
              } else {
                private$set(event, data[["value"]])
                private$invoke(event, data[["value"]])
              }
            }
          })
        }
      }
    }
  ),
  public = list(
    .calls = NULL,
    events = NULL,
    ## -------------------------------------------------------------------------
    ## if shiny is used it will create ReactiveBinding with hack
    ## that will trigger in same cases the event (shiny bug)
    ## -------------------------------------------------------------------------
    initialize = function(spy = FALSE, shiny = FALSE) {
      self$events <- new.env()
      private$data <- new.env()
      private$shiny <- shiny
      if (shiny) {
        private$observers <- list()
      }
      private$handlers <- list()
      private$..spying <- spy
      if (private$..spying) {
        self$.calls <- list()
      }
    },
    ## -------------------------------------------------------------------------
    ## :: create new observer if don't exists and add handler to the list
    ## :: to that observer, we use single observer so ... will be of no use
    ## :: on next handler on signle event, we keep it just in case it may be of use
    ## -------------------------------------------------------------------------
    on = function(events, handler, ...) {
      for (event in events) {
        if (!private$bound(event)) {
          private$bind(event, ...)
        }

        private$handlers[[event]] <- append(
          private$handlers[[event]],
          handler
        )
      }
    },
    ## -------------------------------------------------------------------------
    ## :: emit event add trigger all handlers added by on
    ## -------------------------------------------------------------------------
    emit = function(name, data = NULL) {
      ## typecheck mainly to show proper error when name is missing
      if (!is.character(name)) {
        print("WARN(EventEmitter::emit) name argument is not string")
      } else {
        if (!private$bound(name) && !private$shiny) {
          private$set(name, data)
          return()
        }
        if (is.null(data)) {
          value <- if (private$shiny) {
            shiny::isolate(self$events[[name]])
          } else {
            self$events[[name]]
          }
          value <- if (is.null(value) || !is.logical(value)) {
            TRUE
          } else {
            !value
          }
        } else {
          value <- list(
            value = data,
            timestamp <- as.numeric(Sys.time()) * 1000
          )
        }
        if (private$shiny) {
          battery:::force(function() {
            self$events[[name]] <- value
          })
        } else {
          self$events[[name]] <- value
        }
      }
    },
    ## -------------------------------------------------------------------------
    ## :: remove single handler or all handlers and observer
    ## :: if handler is used and it's last handler observer is also removed
    ## -------------------------------------------------------------------------
    off = function(events, handler = NULL) {
      for (event in events) {
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
    },
    ## -------------------------------------------------------------------------
    finalize = function() {
      private$.spy("finalize")
      lapply(names(private$handlers), function(event) {
        private$unbind(event)
      })
    }
  )
)
