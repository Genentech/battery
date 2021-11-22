#' Event emitter is inspired by multiple implementation in JavaScript
#'
#' @description
#' it can be used indepentely of battery events to send data from one component
#' to different one, without the need to broadcast and emit events
#' it can be used with services, in addition to can also be used as reactive
#' values in shiny reactive context like renderUI or battery component render
#' function
#' @importFrom R6 R6Class
#' @docType class
#'
#' @field .calls - if EventEmitter is created with \code{spy = TRUE} it will add method calls to this list
#' @field events - environment with reactive objects if EventEmitter created wtih \code{shiny = TRUE},
#'        otherwise it's normal environment but with R active bindings that can't be used in shiny render functions
#'
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://stash.intranet.roche.com/stash/projects/DIVOS/repos/battery/browse}
#'   \item{\code{EventEmitter$new(...)}}{This method is used to create EventEmitter}
#'   \item{\code{on}}{Method add new handler for given event}
#'   \item{\code{emit}}{Method emit event add trigger all handlers added by \code{on}}
#'   \item{\code{off}}{Method removes single handler or all handlers and observer of no handler left}
#'   \item{\code{finalize}}{Destructor - clean up the data}
#' }
#'
#' @export
#' @examples
#'
#' e <- EventEmitter$new()
#' e$on("sessionCreated", function(value) {
#'   print(value$name)
#' })
#'
#' ## and in different part of the application
#'
#' e$emit("sessionCreated", list(name = "My Session"))
#'
EventEmitter <- R6::R6Class(
  "EventEmitter",
  private = list(
    ..spying = FALSE,
    shiny = FALSE,
    data = NULL,
    handlers = NULL,
    observers = NULL,
    ## -------------------------------------------------------------------------
    ## Internal method that logs emitter function calls if spy is used
    ##
    ## @param name - name of the method
    ## @param ... - arguments of function call added
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
    ## Method removes observe Event if in shiny mode or just clean up the handler
    ##
    ## @param event - character string with name of the event
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
    ## Helper method that invoke all the handler with optional argument
    ##
    ## @param event - name of the event
    ## @param value - value that is used to call the handler
    ## -------------------------------------------------------------------------
    invoke = function(event, value) {
      private$.spy("invoke", event, value)
      lapply(private$handlers[[event]], function(handler) {
        battery:::invoke(handler, value, event)
      })
    },
    ## -------------------------------------------------------------------------
    ## Method check if biding is active
    ##
    ## @param event - name of the event
    ## -------------------------------------------------------------------------
    bound = function(event) {
      if (private$shiny) {
        !is.null(private$observers[[event]])
      } else {
        event %in% names(self$events) && bindingIsActive(event, self$events)
      }
    },
    ## -------------------------------------------------------------------------
    ## @description
    ## Method used to set data in emit if there are no biding
    ##
    ## It's also used when activeBiding is used outside of shiny
    ##
    ## @param event - name of the event to set the value
    ## @param data - any value that is assign to event
    ## -------------------------------------------------------------------------
    set = function(event, data) {
      if (private$shiny) {
        self$events[[event]] <- data
      } else {
        private$data[[event]] <- data
      }
    },
    ## -------------------------------------------------------------------------
    ## Method add new observe Event or activeBinding depend on the mode
    ## @param event - name of the event
    ## @param ... - additional arguments passed to observeEvent
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

            private$observers[[event]] <- battery:::observeWrapper(self$events[[event]], {
              data <- self$events[[event]]

              if (is.null(data) || is.logical(data)) {
                private$invoke(event, data)
              } else {
                private$invoke(event, data[["value"]])
              }
            },
            ignoreInit = TRUE,
            event.env = environment(),
            handler.env = environment(),
            ...)
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
    #' @description
    #' R6Class Constructor. If shiny option is used it will create ReactiveBinding
    #' with hack that always trigger reactive context (the same cases the
    #' \code{\link{component}} events - this is shiny bug)
    #'
    #' @param spy - if set to \code{TRUE} it will log all method calls - for debugging
    #' @param shiny - use this option if you need reactive value to trigger render handlers in shiny
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
    #' @description
    #' Method add new handler for given event
    #'
    #' create new observer if doesn't exists and add handler to the list
    #' to that observer, we use single observer so ... will be of no use
    #' on next handler on single event, we keep it just in case it may be of use
    #'
    #' @param events - string or character vector with name of the events
    #' @param handler - function used as handler for give event
    #' @param ... - used only once for observeEvent if shiny mode is used
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
    #' @description
    #' Method emit event add trigger all handlers added by \code{on}
    #'
    #' @param name - character - name of the event to fire
    #' @param data - optional data that can be accessed in handler
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
    #' @description
    #' Method removes single handler or all handlers and observer of no handler left
    #'
    #' @param events - string or character vector with name of the events to destroy
    #' @param handler - optional handler if not NULL it will remove only given handler
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
    #' @description
    #' R6Class destructor. Cleans up event handlers
    ## -------------------------------------------------------------------------
    finalize = function() {
      private$.spy("finalize")
      lapply(names(private$handlers), function(event) {
        private$unbind(event)
      })
    }
  )
)
