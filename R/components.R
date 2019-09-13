#' Global varialbe added to base component that hold each classes and component list
#' for getById method
global <- list2env(list(components = list(), classes = list(), session.end = NULL))

#' Component base class
#'
#' Button <- battery::component(
#'   public = list(
#'     count = NULL,
#'     ## constructor is artifical method so you don't need to call super
#'     ## which you may forget to add
#'     constructor = function(canEdit = TRUE) {
#'       super$initialize(input, output, parent)
#'       self$connect('click', self$ns('button'))
#'       self$count <- 0
#'       self$on('click', function(e = NULL, target = NULL) {
#'         self$count <- self$count + 1
#'       }, enabled = canEdit)
#'       self$output[[self$ns('buttonOutput')]] <- renderUI({
#'         self$events$click
#'         tags$div(
#'           tags$span(self$count),
#'           actionButton(self$ns('button'), 'click')
#'         )
#'       })
#'     },
#'     render = function() {
#'       tags$div(
#'         class = 'button-component',
#'         uiOutput(self$ns('buttonOutput'))
#'       )
#'     }
#'   )
#' )
#' Panel <- battery::component(
#'   public = list(
#'     constructor = function(title) {
#'       self$title <- title
#'       btn <- Button$new()
#'       self$appendChild('button', btn)
#'       self$output[[self$ns('button')]] <- renderUI({
#'         btn$render()
#'       })
#'     },
#'     render = function() {
#'       tags$div(
#'         tags$h2(self$title),
#'         tags$div(uiOutput(self$ns('button')))
#'       )
#'     }
#'   )
#' )
#'
#' Root component that don't have parent need to be called with input output and session
#'
#' root <- Root$new(input = input, output = output, session = session, canEdit = FALSE)
#' output$root <- renderUI({
#'    root$render()
#' })
#'
#' the code will invoke initialize R6 class constructor and call constructor method
#' with remaining parameters added when creating new object
#'
#' Base class for components
#' @export
Component <- R6::R6Class(
  classname = 'Component',
  private = list(
    handlers = NULL,
    ..spying = NULL,
    observers = NULL,
    global = global,
    ## ---------------------------------------------------------------
    trigger = function(name, data) {
      if (name %in% ls(self$events)) {
        if (is.null(data)) {
          self$events[[name]] <- shiny::isolate(!self$events[[name]])
        } else {
          data$timestamp <- as.numeric(Sys.time())*1000
          self$events[[name]] <- data
        }
      }
    }
  ),
  ## -----------------------------------------------------------------
  public = list(
    id = NULL,
    name = NULL,
    events = NULL,
    parent = NULL,
    children = NULL,
    input = NULL,
    output = NULL,
    session = NULL,
    ## each subclass need to copy this field which is used as static fields
    ## right now only one static filed is used which is counter for instances
    ## of the class (for id used in getById and ns namespace)
    static = list2env(list(count = 0)),
    ## ---------------------------------------------------------------
    ## :: native R6 class constructor
    ## ---------------------------------------------------------------
    initialize = function(input = NULL, output = NULL, session = NULL,
                          parent = NULL, component.name = NULL,
                          spy = FALSE, ...) {
      if (is.null(parent) && (is.null(input) || is.null(output) ||
                              is.null(session))) {
        stop(paste('Components without parent need to define input, output ',
                   ' and session in constructor'))
      } else {
        if (is.null(input)) {
          self$input <- parent$input
        } else {
          self$input <- input
        }
        if (is.null(output)) {
          self$output <- parent$output
        } else {
          self$output <- output
        }
        if (is.null(session)) {
          self$session <- parent$session
        } else {
          self$session <- session
        }
      }
      private$..spying <- spy
      private$handlers <- list()
      private$observers <- list()
      self$static$count <- self$static$count + 1
      self$events <- new.env()
      private$global$components <- append(private$global$components, list(
        self
      ))
      self$parent <- parent

      self$id <- paste0(head(class(self), 1), self$static$count)

      self$children <- list()
      if (!is.null(self$constructor)) {
        self$constructor(...)
      }
      if (!is.null(component.name)) {
        parent$appendChild(component.name, self)
      }
      ## global reset component counter - execute once for session
      if (!is.null(self$session) &&
          is.function(self$session$onSessionEnded) &&
          is.null(global$session.end)) {
        global$session.end <- self$session$onSessionEnded(function() {
          ## we need to clear the handler so it can be registerd again
          ## on next session
          global$session.end <- NULL
          reset.counters()
        })
      }
    },
    ## ---------------------------------------------------------------
    ## :: return component with specific id
    ## :: id are created using class name and counter
    ## ---------------------------------------------------------------
    getById = function(id) {
      ## components is one reference for every instance (static)
      for (component in private$global$components) {
        if (component$id == id) {
          return(component)
        }
      }
      NULL
    },
    ## ---------------------------------------------------------------
    ## :: method remove child component alternative to appendChild
    ## :: this function is called automatically on parent
    ## :: when destroy is called
    ## ---------------------------------------------------------------
    removeChild = function(name = NULL, child) {
      if (!is.null(name)) {
         self$children[[name]] <- NULL
      } else {
        for (name in names(self$children)) {
          if (self$children[[name]]$id == child$id) {
            self$children[[name]] <- NULL
            break
          }
        }
      }
    },
    ## ---------------------------------------------------------------
    ## :: you need to invoke this method on each child you're creating
    ## :: so event propagation work correctly
    ## ---------------------------------------------------------------
    appendChild = function(name, child) {
      if (!is.null(self$children[[name]])) {
        stop(sprintf('Child with name `%s` already exists', name))
      } else {
        self$children[[name]] <- child
      }
    },
    ## ---------------------------------------------------------------
    ns = function(name) {
      paste0(self$id, '_', name)
    },
    ## ---------------------------------------------------------------
    ## :: create internal event that can be used in renderUI or render function
    ## :: to trigger rendering
    ## ---------------------------------------------------------------
    createEvent = function(name, value = NULL) {
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
    ## ---------------------------------------------------------------
    ## :: propagate evets from child to parent
    ## ---------------------------------------------------------------
    emit = function(name, value = NULL, target = NULL, include.self = FALSE) {
      if (is.null(target)) {
        target <- self$id
      }
      if (include.self) {
        private$trigger(name, list(value = value, target = target))
      }
      if (!is.null(self$parent)) {
        self$parent$emit(name, value, target = target, include.self = TRUE)
      }
    },
    ## ---------------------------------------------------------------
    ## :: propagate events from parent to all children
    ## ---------------------------------------------------------------
    broadcast = function(name, value = NULL, target = NULL, include.self = FALSE) {
      if (is.null(target)) {
        target <- self$id
      }
      if (include.self) {
        private$trigger(name, list(value = value, target = target))
      }

      lapply(self$children, function(child) {
        child$broadcast(name, value, self$id, include.self = TRUE)
      })
    },
    ## ---------------------------------------------------------------
    ## :: create bidning between input browser event and comonent
    ## :: event system
    ## ---------------------------------------------------------------
    connect = function(event, elementId) {

      self$createEvent(event)

      uuid <- uuid::UUIDgenerate()
      observer <- battery::observeEvent(self$input[[elementId]], {
        self$emit(event, self$input[[elementId]], include.self = TRUE)
      }, observerName = uuid)

      private$observers[[elementId]] <- list(
        observer = observer,
        uuid = uuid
      )
    },
    ## ---------------------------------------------------------------
    ## :: remove binding between input element and compnents events
    ## ---------------------------------------------------------------
    disconnect = function(elementId) {
      private$observers[[elementId]]$observer$observer$destroy()
    },
    ## ---------------------------------------------------------------
    ## :: add event listener to given internal event or native input
    ## ::
    ## :: usage:
    ## ::   self$on(self$ns('input'), function(value = NULL, target = NULL) {
    ## ::   }, input = TRUE)
    ## :: or
    ## ::   self$on('event', function(value = NULL, target = NULL) {
    ## ::   })
    ## :: second event will be triggered if any of child components have
    ## :: called Component::connect or use parent component call
    ## :: Component::broadcast or child component Component::emit
    ## :: if component only waiting for internal events it need to call
    ## :: Component::createEvent('name')
    #'
    #' @param event - name of internal event or input id
    #' @param handler - function that should have value and target parameters
    #' @param input - boolean that's indicate if event should be added to input
    #' @param enabled - boolean that enable event to easy toggle event
    #' @param init - indicate if event should be triggered on init
    ## ---------------------------------------------------------------
    on = function(event, handler, input = FALSE, enabled = TRUE, init = FALSE, ...) {
      if (enabled) {
        if (is.null(private$handlers[[event]])) {
          private$handlers[[event]] <- list()
        }
        uuid <- uuid::UUIDgenerate()
        observer <- if (input) {
          battery::observeEvent(self$input[[event]], {
            handler(self$input[[event]], self)
          }, observerName = uuid, ignoreInit = !init, ...)
        } else {
          self$createEvent(event)

          battery::observeEvent(self$events[[event]], {
            data <- self$events[[event]]
            if (is.null(data) || is.logical(data)) {
              handler()
            } else {
              handler(data[["value"]], data[["target"]])
            }
          }, observerName = uuid, ignoreInit = !init, ...)
        }

        private$handlers[[event]] <- append(private$handlers[[event]], list(
          list(
            handler = handler,
            uuid = uuid,
            observer = observer$observer
          )
        ))
      }
    },
    ## ---------------------------------------------------------------
    ## :: remove event listener(s) for internal event
    ## :: if handler is null it will remove all listeners for a given
    ## :: event
    ## ---------------------------------------------------------------
    off = function(event, handler = NULL) {
      if (is.null(handler)) {
        lapply(private$handlers[[event]], function(e) {
          e$observer$destroy()
        })
        private$handlers[event] <- NULL
      } else {
        flags <- sapply(private$handlers[[event]], function(e) {
          if (identical(e$handler, handler)) {
            e$observer$destroy()
            FALSE
          } else {
            TRUE
          }
        })
        private$handlers[[event]] <- private$handlers[[event]][flags]
      }
    },
    ## ---------------------------------------------------------------
    ## :: Method remove all observers created for this component
    ## ---------------------------------------------------------------
    destroy = function() {
      for (event in names(private$handlers)) {
        self$off(event)
      }
      for (handler in names(private$observers)) {
        self$disconnect(handler)
      }
      self$parent$removeChild(name = self$name, self)
    },
    ## ---------------------------------------------------------------
    ## :: Helper method that create HTML template with self as default
    ## :: variable to be used in html (inside {{ }})
    ## ---------------------------------------------------------------
    template = function(filename, ...) {
      do.call(shiny::htmlTemplate, c(filename = filename, self = self, list(...)))
    },
    ## ---------------------------------------------------------------
    render = function() {
      stop('this function need to be overwritten in child class')
    }
  )
)

#' Helper function for defining components with additional static field. It can also be used
#' so you don't confuse battery component with normal R6Class
#'
#' @param public - list of public functions and properties
#' @param private - list of private functions and properties
#' @param static - list of fields that will stay the same for every instance of the component
#' @param inherit - base class - if not specifed it will inherit from Base class (battery::Component)
#'
#' @export
component <- function(classname,
                      public = NULL,
                      private = NULL,
                      static = NULL,
                      inherit = battery::Component,
                      ...) {
  static.env <- list2env(list(count = 0))
  if (!is.null(static)) {
    for (name in names(static)) {
      static.env[[name]] <- static[[name]]
    }
  }
  class <- R6::R6Class(
    classname = classname,
    inherit = inherit,
    public = list(
      .calls = list()
    ),
    private = list(
      .spy = function(name, ...) {
        if (is.null(self$.calls[[name]])) {
          self$.calls[[name]] <- list()
        }
        args <- list(...)
        self$.calls[[name]] <- c(self$.calls[[name]], list(args))
      }),
    ...
  )
  class$static <- static.env
  global$classes <- append(global$classes, list(class))
  class$set('public', 'static', static.env)
  r6.class.add(class, public)
  r6.class.add(class, private)
  ## we also need extend when base class called without extend
  class$extend <- make.extend(class)
  class
}

#' helper function for adding properties to R6Class
r6.class.add <- function(class, seq) {

  prop.name <- as.character(substitute(seq)) # so we don't need to write name as string
  lapply(names(seq), function(name) {
    if (is.function(seq[[name]])) {
      ## the only way to have scope from when function was create with self and private
      ## eval substitute simply circument R6 encapsulation and use scope from where function
      ## was created (closure) and env.fn patch the env of inner function so it get self
      ## and private as magic names - this is done so component function the same as
      ## when R6Class is created inline - so component is referencial transparent and can
      ## be replaced with R6Class
      fn <- eval(substitute(function(...) {
        fn <- fn.expr # fn.expr will be inline function expression
        ## patch function env
        current <- environment()
        ## we don't overwrite function environment so
        ## you can nest one constructor in another constructor
        env <- new.env(parent = environment(fn))
        env$self <- get('self', current)
        env$static <- get('self', current)$static
        env$super <- get('super', current)
        env$private <- get('private', current)
        environment(fn) <- env
        if (env$private$..spying) {
          env$private$.spy(name = name, ...)
        }
        fn(...)
      }, list(fn.expr = seq[[name]], name = name)))
      class$set(prop.name, name, fn)
    } else {
      class$set(prop.name, name, seq[[name]])
    }
  })
}

#' higher order function for creating extend static method on every battery::Component
make.extend <- function(class) {
  function(...) {
    child <- component(inherit = class, ...)
    child$extend <- make.extend(child)
    child
  }
}

## init extend on base battery component
Component$extend <- make.extend(Component)

#' Function used by base battery component to reset all the counters when components
#' are created outside of server function (which should be most of the time) and
#' if shiny sessions runs in same R process (this will not be needed for shiny in Docker,
#' but it would not cause any harm)
reset.counters <- function() {
  for (class in global$classes) {
    class$static$count <- 0
  }
}
