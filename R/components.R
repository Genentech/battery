#' Main file with base of battery components


#' Global env generator function - added to each component one per root (component without parent)
#' This env is used so you can share services for one tree of components. You can't send data between
#' Two trees of components.
#' @name new.global.env
new.global.env <- function() {
  list2env(list(
    components = list(),
    classes = list(),
    session.end = NULL,
    ## we need env inside env because we use it outside of global but we also need
    ## a single reference so we can reset it
    services = new.env()
  ))
}

#' Helper where new static default values can be added
#' this static env should be the same for each Component per shiny session
#' @name new.static.env
new.static.env <- function() {
  list2env(list(count = 0))
}

#' Global varialbe added to base component that hold each classes and component list
#' for getById method
#' @name global
global <- new.global.env()
global$sessions <- list()
global$exceptions <- list(
  global = list(),
  tokens = list()
)

#' Root component that have no parent,
#'
#' @description
#'
#' need to be called with input, output and session. it should not be used directly,
#' only using \code{\link{component}} function.
#'
#' @name BaseComponent
#' @importFrom R6 R6Class
#' @keywords components architecture structure
#'
#' @field id - string that
#' @field name - component instance name, set using \code{parent$appendChild(name)} or
#'        \code{component$new(parent = self, component.name = name)}
#' @field services - environment that hold static services - objects shared across battery components
#'        tree. Services can be added using \code{component$addService(name, ANY)}
#' @field events - environment that will hold reactive values added by on or createEvent method
#' @field parent - parent component
#' @field children - list of components that are children of the component, this list will be used to
#'        when using \code{component$broadcast("name")}
#' @field input - shiny input object added in constructor of root class or inherited from parent
#' @field output - shiny output object added in constructor of root class or inherited from parent
#' @field session - shiny session object added in constructor of root class or inherited from parent
#' @field static - environment that can be used to save property into class, it will be shared
#'        with all instances of same battery component.
#' @section Methods:
#' \describe{
#'   \item{Documentation}{For full documentation of each method go to https://stash.intranet.roche.com/stash/projects/DIVOS/repos/battery/browse}
#'   \item{\code{BaseComponent$new(...)}}{This method is used to create base battery object, it should never be created directly. Battery components should be created as inherited from this BaseComponent, but this should be done only using \code{component} function}
#'   \item{\code{getById}}{Method return component with specific id}
#'   \item{\code{appendChild}}{Method add battery component as child this current component}
#'   \item{\code{removeChild}}{Method remove child component complementary to appendChild}
#'   \item{\code{ns}}{Method used to create namespaced identifier}
#'   \item{\code{createEvent}}{Method will create battery event}
#'   \item{\code{emit}}{Propagate events from child to parent}
#'   \item{\code{broadcast}}{Propagate events from parent to all children}
#'   \item{\code{connect}}{Helper method that will create binding between input event from shiny and battery event}
#'   \item{\code{disconnect}}{Method remove binding between input element and compnents events}
#'   \item{\code{on}}{Add event listener to given internal event or native input}
#'   \item{\code{off}}{Method removes event listener(s) added by \code{on}}
#'   \item{\code{class}}{Method return name of this class - same as classname when crating the class}
#'   \item{\code{destroy}}{Method remove all observers created for this component}
#'   \item{\code{finalize}}{R6Class method that will be called when object is destroyed}
#'   \item{\code{addService}}{Method dynamically add service to battery component system}
#'   \item{\code{template}}{Helper method that create \code{shiny::htmlTemplate} with self and private as defaults variables}
#'   \item{\code{path}}{Method return path to the object in battery components tree}
#'   \item{\code{log}}{Method log messages that can be listen to with \code{logger} helper}
#'   \item{\code{logger}}{Shortcut function to add listener to logger}
#'   \item{\code{render}}{Function that should be overwritten in battery component}
#' }
BaseComponent <- R6::R6Class(
  classname = 'BaseComponent',
  private = list(
    .handlers = NULL,
    .spying = NULL,
    .observers = NULL,
    .global = NULL,
    ## ---------------------------------------------------------------
    ## :: setter/getter for pending counter for given internal events
    ## :: used to check if there was no pending events that was not
    ## :: triggered. This shouldn't be needed, it's just in case
    ## ---------------------------------------------------------------
    .pending = function(name, value = NULL, increment = NULL, fn = NULL) {
      if (!is.null(private$.handlers[[name]])) {
        if (!is.null(value)) {
          for (i in seq_along(private$.handlers[[name]])) {
            handler <- private$.handlers[[name]][[i]]
            if (!handler$input && (is.null(fn) || identical(handler$handler, fn))) {
              private$.handlers[[name]][[i]]$pending <- value
            }
          }
        } else if (!is.null(increment)) {
          for (i in seq_along(private$.handlers[[name]])) {
            handler <- private$.handlers[[name]][[i]]
            if (!handler$input && (is.null(fn) || identical(handler$handler, fn))) {
              private$.handlers[[name]][[i]]$pending <- handler$pending + increment
            }
          }
        } else {
          sapply(private$.handlers[[name]], '[[', 'pending')
        }
      }
    },
    ## -------------------------------------------------------------------------
    .indent = function() {
      if (self$static$.global$.level > 0) {
        strrep(" ", self$static$.global$.level * 2)
      } else {
        ""
      }
    },
    ## -------------------------------------------------------------------------
    ## :: Method checks if expression is self$ns('xxx')
    ## -------------------------------------------------------------------------
    .is.ns = function(expr) {
      length(expr) == 2 && class(expr[[1]]) == 'call' && deparse(expr[[1]]) == 'self$on'
    }
  ),
  ## -----------------------------------------------------------------
  public = list(
    id = NULL,
    name = NULL,
    ## every component in a tree share same services
    services = NULL,
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
    #' native R6 class constructor
    #'
    #' this should never be overwriten by child components, they should only
    #' overwrite constructor that is not as problematic when not called super
    #'
    #' @param input - shiny input object added in constructor of root class or inherited from parent
    #' @param output - shiny output object added in constructor of root class or inherited from parent
    #' @param session - shiny session object added in constructor of root class or inherited from parent
    #' @param parent - parent battery component, if used you don't need to add
    #'                 \code{input}, \code{output} and \code{session}
    #' @param component.name - name of the component to be used in component$parent$children
    #' @param services - list of any static services that can be created on component initialization
    #' @param spy - used in unit test to record component method calls (only user methods are recorded)
    #' @param ... - everything else is passed to \code{constructor} method that should be used in
    #'              user components
    initialize = function(input = NULL, output = NULL, session = NULL,
                          parent = NULL, component.name = NULL,
                          error = NULL, services = NULL, spy = FALSE, ...) {
      ## shiny values parent inheritance
      if (is.null(parent) && is.null(session)) {
        stop(paste('Components without parent need to define session in constructor'))
      } else if (!is.null(parent)) {
        self$session <- parent$session
        self$input <- parent$input
        self$output <- parent$output
      } else {
        self$session <- session
        if (is.null(input)) {
          self$input <- session$input
        } else {
          self$input <- input
        }
        if (is.null(output)) {
          self$output <- session$output
        } else {
          self$output <- output
        }
      }

      self$parent <- parent
      classname <- self$class()$classname
      token <- self$session$token

      ## init static env, one per shiny session per component
      if (!is.null(token)) {
        if (!token %in% names(global$sessions)) {
          global$sessions[[token]] <- new.env()
        }
        if (!classname %in% names(global$sessions[[token]])) {
          global$sessions[[token]][[classname]] <- new.static.env()
        }
        self$static <- global$sessions[[token]][[classname]]
      }

      ## init global env, used by services, one per root component
      if (is.null(parent)) {
        self$static$.global <- new.global.env()
        self$static$.global$.level <- 0
      } else {
        self$static$.global <- self$parent$static$.global
      }

      private$.spying <- spy
      private$.handlers <- list()
      private$.observers <- list()
      self$static$count <- self$static$count + 1
      self$events <- new.env()
      self$static$.global$components <- append(
        self$static$.global$components,
        list(
          self
        )
      )

      self$id <- paste0(classname, self$static$count)

      ## TODO: Optimize children use env
      self$children <- list()

      ## Services init
      self$services <- self$static$.global$services
      ## logger
      if (is.null(parent)) {
        self$services$.log <- EventEmitter$new()
      }
      ## global error handler
      if (!is.null(error) && is.null(parent)) {
        self$static$.global$.error <- error
      }
      if (length(services) > 0) {
        for (serviceName in names(services)) {
          self$addService(serviceName, services[[serviceName]])
        }
      }
      if (!is.null(component.name)) {
        self$name <- component.name
        if (!is.null(parent)) {
          parent$appendChild(component.name, self)
        }
      }
      ## global reset component counter - execute once for session
      if (!is.null(self$session) &&
          is.function(self$session$onSessionEnded)) {
        self$session$onSessionEnded(function() {
          self$destroy()
        })
      }

      if (!is.null(self$constructor)) {
        ## TODO: improve stack trce in Battery and make it stable
        ## this stimetimes prints no stack trace aviable
        ## add to handlers ($on) and fn(...) in building method
        ## with scope at the end
        battery::withExceptions({
          self$constructor(...)
        },
        session = self$session,
        error = function(cond) {
          create.error(cond, list(
            type = "constructor",
            origin = paste0(self$id, "::constructor"),
            args = list(...)
          ))
        })
      }
    },
    ## ---------------------------------------------------------------
    #' Method return component with specific id
    #'
    #' it will search the tree of components find name with specific id
    #'
    #' @param id - string - id of the component to search
    #' @return Battery component
    ## ---------------------------------------------------------------
    getById = function(id) {
      ## components is one reference for every instance (static)
      for (component in self$static$.global$components) {
        if (component$id == id) {
          return(component)
        }
      }
      NULL
    },
    ## ---------------------------------------------------------------
    #' Method add battery component as child this current component
    #'
    #' this function is called if you pass component.name to constructor
    #' otherwise it should be called to create proper tree. This is required
    #' so \code{component$broadcast} and \code{component$emit} work properly
    #'
    #' @param name - string to be used as name
    #' @param child - battery component
    ## ---------------------------------------------------------------
    appendChild = function(name, child) {
      if (!is.null(self$children[[name]])) {
        stop(sprintf('Child with name `%s` already exists', name))
      } else {
        self$children[[name]] <- child
      }
    },
    ## ---------------------------------------------------------------
    #' Method remove child component complementary to appendChild
    #'
    #' it can be used with name or the component
    #'
    #' @param name - name of the component to remove
    #' @param child - battery component to remove
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
    #' Method used to create namespaced identifier
    #'
    #' @param name - name to be used inside shiny input or output
    #' @examples
    #' \dontrun{
    #'  battery::component(
    #'    classname = "Plot",
    #'    public = list(
    #'      constructor = function() {
    #'        self$output[[ self$ns("plot") ]] <- renderPlot({
    #'          ## ...
    #'        })
    #'     },
    #'     render = function() {
    #'       shiny::div(
    #'         class = "container",
    #'         plotOutput(self$ns("plot"))
    #'       )
    #'     }
    #'   )
    #' )
    #' }
    ## ---------------------------------------------------------------
    ns = function(name) {
      paste0(self$id, '_', name)
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method will create battery event
    #'
    #' this event can be triggered from R code it can also be broadcasted
    #' this function is called automatically when using on to create observer
    #'
    #' @param name - string, name of the event
    #' @param value - initial value of the event reactive variable
    ## ---------------------------------------------------------------
    createEvent = function(name, value = NULL) {
      self$log("battery", "createEvent", name = name, type = "createEvent")
      if (!name %in% ls(self$events)) {
        self$log("battery", "makeReactiveBinding", name = name, type = "createEvent")
        shiny::makeReactiveBinding(name, env = self$events)
        if (is.logical(value) && value) {
          self$events[[name]] <- TRUE
        } else {
          data <- list(
            value = value,
            timestamp = battery:::now()
          )
          self$events[[name]] <- data
        }
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method will trigger the event. It call every observer and invalidate
    #' every reactive context
    #'
    #' @param name - name of the event to fire
    #' @param data - data to be used to trigger the event if function use
    #' @param .force - internal option to disable forcing of reactive events
    #' @param .level - internal option for logger, that is used to created indent
    ## ---------------------------------------------------------------
    trigger = function(name, data = NULL, .force = TRUE, .level = 0) {
      indent <- .level * 2

      msg <- battery:::indent(indent, "trigger")
      self$log("battery", msg, name = name, target = self$id, type = "trigger")

      if (name %in% ls(self$events)) {
        update <- if (is.null(data)) {
          function() {
            msg <- battery:::indent(indent, "trigger::force (NULL)")
            self$log("battery", msg, name = name, force = .force, target = self$id, type = "trigger")

            self$events[[name]] <- shiny::isolate({
              if (is.logical(self$events[[name]])) {
                !self$events[[name]]
              } else if (is.list(self$events[[name]])) {
                list(
                  timestamp = battery:::now(),
                  target = self$id,
                  value = self$events[[name]]$value
                )
              } else {
                message(paste(
                  "[WARN]",
                  self$id,
                  "- trigger: wrong data type (if used event data need to be list or boolean)"
                ))
                NULL
              }
            })
          }
        } else if (is.list(data)) {
          function() {
            data$timestamp <- battery:::now()
            if (is.null(data$target)) {
              data$target <- self$id
            }
            msg <- battery:::indent(indent, "trigger::force (list)")
            self$log("battery", msg, name = name, force = .force, target = self$id, type = "trigger")
            self$events[[name]] <- data
          }
        } else {
          function() {
            message(paste(
              "[WARN]",
              self$id,
              "- trigger: wrong data type (if used event data need to be list or boolean)"
            ))
          }
        }
        private$.pending(name, increment = 1)
        ## force is hack that always trigger the event, even if shiny decide not to
        if (.force) {
          battery:::force(update)
        } else {
          update()
        }
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Propagate events from child to parent
    #'
    #' it will recursivly walk whole tree, and trigger only events that
    #' have reactive values added with \code{createEvent} it will also trigger
    #' all observers added with \code{on}
    #'
    #' @param name - name of the event to propagate
    #' @param value - optional value to to set on reactive values (it will be
    #'        access from component$events or inside observer
    #' @param target - optioanl target that should be passed along the event
    #'        can only be access from event handler added by \code{component$on}
    #' @param include.self - shoult it also trigger on this component or only
    #'        on children
    #' @param .level - internal option for logger, that is used to created indent
    #'
    #' @examples
    #' \dontrun{
    #' App <- battery::component(
    #'   classname = "App",
    #'   public = list(
    #'     constructor = function() {
    #'       self$on("update", function() {
    #'         print("I need to update")
    #'       })
    #'       panel <- Panel$new(parent = self, component.name = "panel")
    #'       self$outptu[[self$ns("panel")]] <- renderUI({
    #'          panel$render()
    #'       })
    #'     },
    #'     render = function() {
    #'       shiny::tags$div(
    #'         #...
    #'         uiOutput(self$ns("panel"))
    #'       )
    #'     }
    #'   )
    #' )
    #' Panel <- battery::component(
    #'   classname = "Panel",
    #'   public = list(
    #'     constructor = function() {
    #'       self$on(self$ns("button"), function() {
    #'         self$emit("update")
    #'       }, input = TRUE)
    #'     },
    #'     render = function() {
    #'        shiny::tags$div(
    #'           #...
    #'           actionButton(self$ns("button"), "Click Me")
    #'        )
    #'     }
    #'   )
    #' )
    #' ## clicking on button will emit the event to the parent and print the message
    #' }
    ## ---------------------------------------------------------------
    emit = function(name, value = NULL, target = NULL, include.self = FALSE, .level = 0) {
      if (is.null(target)) {
        target <- self$id
      }

      msg <- battery:::indent(.level * 2, "emit")
      self$log("battery", msg, name = name, value = value, target = target, type = "emit")

      if (include.self) {
        self$trigger(name, list(value = value, target = target), .level = .level)
      }
      if (!is.null(self$parent)) {
        self$parent$emit(name, value, target = target, include.self = TRUE, .level = .level + 1)
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Propagate events from parent to all children
    #'
    #' methods similar to \code{emit} but it propagete event to children
    #' if called on root component it will send message to all components
    #' inside the tree.
    #'
    #' @param name - string - name of the component to trigger
    #' @param value - default value adde to component$events
    #' @param target - string that indicate which battery component trigger the event
    #'        it can be omited if so it will use same object that called the method
    #' @param include.self - flag that indicate if event should also be called on self
    #' @param .level - internal option for logger, that is used to created indent
    #'
    #' @examples
    #' \dontrun{
    #' App <- battery::component(
    #'   classname = "App",
    #'   public = list(
    #'     count = 0,
    #'     constructor = function() {
    #'       self$label <- label
    #'       self$on(self$ns("button"), function() {
    #'         self$count <- self$count + 2
    #'         self$broadcast("update", paste0("Update_number_", count))
    #'       }, input = TRUE)
    #'       counter <- Counter$new(parent = self, component.name = "counter")
    #'       self$outptu[[self$ns("counter")]] <- renderUI({
    #'          panel$render()
    #'       })
    #'     },
    #'     inc = function(count) {
    #'       self$label <- paste0("label_", count)
    #'     },
    #'     render = function() {
    #'       shiny::tags$div(
    #'         actionButton(self$ns("button"), "Click Me"),
    #'         uiOutput(self$ns("counter"))
    #'       )
    #'     }
    #'   )
    #' )
    #' Counter <- battery::component(
    #'   classname = "Counter",
    #'   public = list(
    #'     counter = 0,
    #'     constructor = function() {
    #'       self$createEvent("update")
    #'     },
    #'     render = function() {
    #'        self$counter <- self$counter + 1
    #'        shiny::tags$div(
    #'           paste("Counter", self$counter),
    #'           self$events$update$value
    #'        )
    #'     }
    #'   )
    #' )
    #' ## first it will render the child with "Counter 1" (the value of events reactive
    #' ## reactive variable will be NULL, default value of events)
    #' ## after clicking the button it will increase the count in App by 2
    #' ## send event to children and it will in turn trigger render child again
    #' ## so it will display "Counter 2" and "Update_number_2"
    #' ##
    #' ## child render will be called twice and event handler on button once
    #' }
    ## ---------------------------------------------------------------
    broadcast = function(name, value = NULL, target = NULL, include.self = FALSE, .level = 0) {
      if (is.null(target)) {
        target <- self$id
      }

      msg <- battery:::indent(.level * 2, "broadcast")
      self$log("battery", msg, name = name, value = value, target = target, type = "broadcast")

      if (include.self) {
        self$trigger(name, list(value = value, target = target), .level = .level)
      }
      lapply(self$children, function(child) {
        child$broadcast(name, value, self$id, include.self = TRUE, .level = .level + 1)
      })
    },

    ## ---------------------------------------------------------------
    #' @description
    #' Helper method that will create binding between input event from shiny and battery event
    #'
    #' @param event - name of the event
    #' @param elementId - id of the HTML element (shiny input it should be value from \code{self$ns})
    ## ---------------------------------------------------------------
    connect = function(event, elementId) {

      self$log("battery", "connect", event = event, type = "connect")

      if (is.null(private$.observers[[elementId]])) {
        self$createEvent(event)

        observer <- shiny::observeEvent(self$input[[elementId]], {
          ..BATTERY <- FALSE
          self$emit(event, self$input[[elementId]], include.self = TRUE)
        })

        private$.observers[[elementId]] <- observer
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method remove binding between input element and compnents events
    #'
    #' complementary to connect
    #'
    #' @param elementId - it of the input element
    ## ---------------------------------------------------------------
    disconnect = function(elementId) {
      private$.observers[[elementId]]$destroy()
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Add event listener to given internal event or native input
    #'
    #' @param events - character or character vector of internal event or input id
    #' @param handler - function that can have value and target parameters (optional)
    #' @param input - boolean that's indicate if event should be added to input
    #'        otherwise it's internal battery event
    #' @param enabled - boolean that enable event to easy toggle event
    #' @param single - if used it will create only one event, it will always destroy old one
    #' @param init - indicate if event should be triggered on init
    #' @param ... - any additional arguments are passed into shiny::observeEvent
    #' @examples
    #' \dontrun{
    #'
    #' self$on(self$ns("inputValue"), function(value) {
    #'    print(paste("Input value is ", value))
    #' }, input = TRUE)
    #'
    #' self$on(self$ns("save"), function() {
    #'    print("user click save")
    #' }, input = TRUE)
    #'
    #'
    #' self$on("event", function(value, target) {
    #'   ## this event can be fired with trigger/emit/broadcast
    #' })
    #' }
    ## ---------------------------------------------------------------
    on = function(events, handler, input = FALSE, enabled = TRUE, single = TRUE, init = FALSE, ...) {
      if (private$.is.ns(substitute(events)) && !input) {
        print(paste(
          "[WARN]",
          self$id,
          "- you should use input = TRUE when using self$ns to create event on",
          "shiny input element. You should not use self$ns with battery events."
        ))
      }
      for (event in events) {
        self$log("battery", "on", event = event, type = "on")
      }
      ## HACK: for avengersApps to detect battery in shiny::observeEvent monkey patch
      ##
      ##       avengersApps use: is.battery <- any(grepl("..BATTERY <- FALSE", sys.call()))
      ##       to detect if the function was called in battery
      ##
      ## TODO: remove the hack and refactor avengersApps::observeEvent
      ##       to use option single = TRUE in IDA to force destroy of old observer,
      ##       so it will not affect battery, that have same expression for multiple events
      ##       on same component or input (without this only last handler remain)
      ##       battery can't use observerName because it need to work with original
      ##       shiny::observeEvent that don't have that option
      ..BATTERY <- TRUE
      if (enabled) {
        if (!is.function(handler)) {
          stop(sprintf("battery::component::on handler for `%s` is not a function", event))
        }
        for (event in events) {
          if (is.null(private$.handlers[[event]])) {
            private$.handlers[[event]] <- list()
          } else if (single && length(private$.handlers[[event]]) > 0) {
            ## destroy old event
            for (i in seq_along(private$.handlers[[event]])) {
              if (identical(private$.handlers[[event]][[i]]$handler, handler)) {
                private$.handlers[[event]][[i]]$observer$destroy()
                private$.handlers[[event]][i] <- NULL
              }
            }
          }

          observer <- if (input) {
            shiny::observeEvent(self$input[[event]], {
              ..BATTERY <- FALSE
              battery::withExceptions({
                space <- private$.indent()
                self$log(
                  c("battery", "info"),
                  paste0(space, "on::trigger::before(N)"),
                  event = event,
                  input = input,
                  type = "on"
                )
                self$static$.global$.level <- self$static$.global$.level + 1
                battery:::invoke(handler, self$input[[event]], self$id)
                self$log(
                  c("battery", "info"),
                  paste0(space, "on::trigger::after(N)"),
                  event = event,
                  input = input,
                  type = "on"
                )
              }, error = function(cond) {
                create.error(cond, list(
                  type = "event",
                  origin = paste0(self$id, "::on('", event, "', ...)"),
                  event = event,
                  input = input
                ))
              }, finally = function() {
                self$static$.global$.level = self$static$.global$.level - 1
              }, session = self$session)
            }, ignoreInit = !init, ...)
          } else {
            self$createEvent(event)

            shiny::observeEvent(self$events[[event]], {
              ..BATTERY <- FALSE
              data <- self$events[[event]]
              ## invoke handler function with only argument it accept
              battery::withExceptions({
                space <- private$.indent()
                self$log(
                  c("battery", "info"),
                  paste0(space, "on::trigger::before(B)"),
                  event = event,
                  input = input,
                  type = "on"
                )
                self$static$.global$.level = self$static$.global$.level + 1
                private$.pending(event, increment = -1, fn = handler)
                if (is.null(data) || is.logical(data)) {
                  battery:::invoke(handler, NULL, NULL)
                } else {
                  battery:::invoke(handler, data[["value"]], data[["target"]])
                }
                self$log(
                  c("battery", "info"),
                  paste0(space, "on::trigger::after(B)"),
                  event = event,
                  input = input,
                  type = "on"
                )
              }, error = function(cond) {
                create.error(cond, list(
                  type = "event",
                  event = event,
                  origin = paste0(self$id, "::on('", event, "', ...)"),
                  input = input
                ))
              }, finally = function() {
                self$static$.global$.level <- self$static$.global$.level - 1
              }, session = self$session)
            }, ignoreInit = !init, ...)
          }

          private$.handlers[[event]] <- append(private$.handlers[[event]], list(
            list(
              input = input,
              handler = handler,
              pending = 0,
              observer = observer
            )
          ))
        }
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method removes event listener(s) added by \code{on}
    #'
    #' if handler is NULL it will remove all listeners for a given event name
    #'
    #' @param events - vector or string with names of events to remove
    #' @param handler - optional event handler
    ## ---------------------------------------------------------------
    off = function(events, handler = NULL) {
      for (event in events) {
        self$log("battery", "off", event = event, handler = handler, type = "off")
        if (is.null(handler)) {
          for (e in private$.handlers[[event]]) {
            if (e$pending != 0) {
              print(paste(
                "[WARN]", self$id, "- event", event, "was not called,",
                "you can try to call this event with force"
              ))
            }
            e$observer$destroy()
          }
          private$.handlers[event] <- NULL
        } else {
          flags <- sapply(private$.handlers[[event]], function(e) {
            if (identical(e$handler, handler)) {
              if (e$pending != 0) {
                print(paste(
                  "[WARN]", self$id,"- event", event, "was not called,",
                  "you can try to call this event with force"
                ))
              }
              e$observer$destroy()
              FALSE
            } else {
              TRUE
            }
          })
          private$.handlers[[event]] <- private$.handlers[[event]][flags]
        }
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method return name of this class - same as classname when crating the class
    #'
    #' @return string - class name
    ## ---------------------------------------------------------------
    class = function() {
      private$.class
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method dfestroy component
    #'
    #' It removes all observers created for this component also clear
    #' it also use other clean ups.
    ## ---------------------------------------------------------------
    destroy = function() {
      self$log("battery", "destroy", type = "destroy")
      for (event in names(private$.handlers)) {
        self$off(event)
      }
      self$static$count <- 0
      ## case when componets are not proper tree
      cls <- self$class()
      if (!is.null(cls) && !is.null(cls$static) && !is.null(cls$static$.global)) {
        cls$static$.global$services <- new.env()
      }
      ## clear data for different users in one R process
      if (!is.null(self$session$token)) {
        global$sessions[[ self$session$token ]] <- NULL
      }
      for (handler in names(private$.observers)) {
        self$disconnect(handler)
      }
      if (!is.null(self$parent)) {
        self$parent$removeChild(name = self$name, self)
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' R6Class method that will be called when object is destroyed
    #' it just calls \code{destroy}
    ## ---------------------------------------------------------------
    finalize = function() {
      self$destroy()
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method dynamically add service to battery component system
    #'
    #' only one service with giben name can be added to the tree
    #' same object will be accessed in every component in the tree.
    #' there is one default service logger that is \code{EventEmitter}
    #'
    #' @param name - name of the service by which you access the service
    #'        e.g. self$service$foo
    #' @param service - object used as service - it can be any object
    ## ---------------------------------------------------------------
    addService = function(name, service) {
      self$log("info", "addService", name = name, type = "addService")
      if (name %in% names(self$services)) {
        stop(sprintf("[%s] Service '%s' already exists ", self$id, name))
      }
      self$services[[name]] <- service
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Helper method that create \code{shiny::htmlTemplate}
    #' with self and private as defaults variables to be used in html (inside {{ }})
    #'
    #' @param filename - name of the template
    #' @param ... - any number of arguments that will be accessible in
    #'        template
    ## ---------------------------------------------------------------
    template = function(filename, ...) {
      do.call(shiny::htmlTemplate, c(
        filename = filename,
        private = private,
        self = self,
        list(...)))
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method return path to the object in battery components tree
    #'
    #' @return vector of strings of id of the components in the tree
    ## ---------------------------------------------------------------
    path = function() {
      path <- c(self$id)
      node = self
      while (!is.null(node$parent)) {
        node <- node$parent
        path <- c(node$id, path)
      }
      path
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Method log message that can be listen to, best way to add listener
    #' is to use self$logger("name", fn) in root component constructor
    #' each event is triggered with list(id, type, path, message, args)
    #'
    #' @param levels - vector of characters to listen (default names in battery
    #'        are "battery" and "info")
    #' @param message - message to log
    #' @param type - default battery - additional value to distinguish the message
    #'        in battery type is name of the method - or "method" inside user method
    #' @param ... - any arguments are added into args property
    ## ---------------------------------------------------------------
    log = function(levels, message, type = "battery", ...) {
      path <- paste(self$path(), collapse = "/")
      data <- list(
        id = self$id,
        type = type,
        path = path,
        message = message,
        args = list(...)
      )
      for (level in levels) {
        self$services$.log$emit(level, data)
      }
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Shortcut function to add listener to logger
    #'
    #' @param level - character vector or character of log levels to listen
    #' @param fn - function handler
    ## ---------------------------------------------------------------
    logger = function(level, fn) {
      self$services$.log$on(level, fn)
    },
    ## ---------------------------------------------------------------
    #' @description
    #' Function that should be overwritten in battery component
    #'
    #' this is convention that this function should return HTML (shiny tags)
    #' this function can have reactive value self$events.
    #' render function should not have children render if possible becasue
    #' update of parent will rerender the children. The proper way is to use
    #' renderUI in constructor and renderUI in render function for the children.
    #'
    #' @return overwriten render by convention should return shiny tags
    ## ---------------------------------------------------------------
    render = function() {
      stop('render function need to be overwritten in child class')
    }
  )
)

#' Basic function to create battery components.
#'
#' @description Use this function to create new battery class object.
#'
#' @return \code{\link{R6Class}}, with battery specific methods (see \code{\link{BaseComponent}}).
#' @param classname - name of the class as string
#' @param public - list of public functions and properties
#' @param private - list of private functions and properties
#' @param static - list of fields that will stay the same for every instance of the component
#' @param inherit - base class - if not specifed it will inherit from Base class \code{\link{BaseComponent}}
#' @param ... - reset option passed to \code{\link{R6Class}} constructor
#' @export
#' @examples
#' \dontrun{
#'
#' Button <- battery::component(
#'   classname = "Button",
#'   public = list(
#'     count = NULL,
#'     ## constructor is artifical method so you don't need to call super
#'     ## which you may forget to add
#'     constructor = function(canEdit = TRUE) {
#'       self$connect("click", self$ns("button"))
#'       self$count <- 0
#'       self$on("click", function(e = NULL, target = NULL) {
#'         self$count <- self$count + 1
#'       }, enabled = canEdit)
#'       self$output[[self$ns("buttonOutput")]] <- renderUI({
#'         self$events$click
#'         tags$div(
#'           tags$span(self$count),
#'           actionButton(self$ns("button"), "click")
#'         )
#'       })
#'     },
#'     render = function() {
#'       tags$div(
#'         class = "button-component",
#'         uiOutput(self$ns("buttonOutput"))
#'       )
#'     }
#'   )
#' )
#' Panel <- battery::component(
#'   classname = "Panel",
#'   public = list(
#'     title = NULL,
#'     constructor = function(title) {
#'       self$title <- title
#'       btn <- Button$new(parent = self)
#'       self$appendChild("button", btn)
#'       self$output[[self$ns("button")]] <- renderUI({
#'         btn$render()
#'       })
#'     },
#'     render = function() {
#'       tags$div(
#'         tags$h2(self$title),
#'         tags$div(uiOutput(self$ns("button")))
#'       )
#'     }
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'    ## this is entry point into batter component tree
#'    root <- Panel$new(title = "Hello", input = i, output = o, session = s)
#'    renderUI({
#'        root$render()
#'    })
#' }
#' }
component <- function(classname,
                      public = NULL,
                      private = NULL,
                      static = NULL,
                      inherit = battery:::BaseComponent,
                      ...) {
  static.env <- new.static.env()
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
  class$set('public', 'static', static.env)
  class$set('private', '.class', class)
  r6.class.add(class, public)
  r6.class.add(class, private)
  ## we also need extend when base class called without extend
  class$extend <- make.extend(class)
  class
}

#' Helper function for adding properties to R6Class
#' @param class - battery component constructor
#' @param seq - named list of properties and functions methods
#'
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
        if (env$private$.spying) {
          env$private$.spy(name = name, ...)
        }
        battery::withExceptions({
          space <- env$private$.indent()
          env$self$static$.global$.level = env$self$static$.global$.level + 1
          env$self$log("info", paste0(space, name, "::before"), type = "method")
          ret <- fn(...)
          env$self$log("info", paste0(space, name, "::after"), type = "method")
          ret
        }, error = function(cond) {
          create.error(cond, list(
            type = "method",
            origin = paste0(env$self$id, "::", name),
            name = name,
            args = list(...)
          ))
        }, finally = function() {
          env$self$static$.global$.level = env$self$static$.global$.level - 1
        }, session = env$self$session)
      }, list(fn.expr = seq[[name]], name = name)))
      class$set(prop.name, name, fn)
    } else {
      class$set(prop.name, name, seq[[name]])
    }
  })
}

#' function allows to add global exception handler for all battery components
#' @param handler - list of handlers
#' @param rest - indicate if old handlers should be removed
#' @param session - optional shiny session where to register the exception handler
#' @export
exceptions <- function(handler = NULL, reset = FALSE, session = NULL) {
  if (reset) {
    if (is.null(session)) {
      if (is.null(handler)) {
        global$exceptions$sessions <- list()
        global$exceptions$global <- list()
      } else {
        global$exceptions$global <- handler
      }
    } else {
      token <- session$token
      global$exceptions$sessions[[ token ]] <- if (is.null(handler)) {
        list()
      } else {
        handler
      }
    }
  } else if (is.null(handler)) {
    stop("battery::excpetion list require NULL given")
  } else if (is.null(session)) {
    exceptions <- global$exceptions$global
    global$exceptions$global <- if (is.null(exceptions)) {
      handler
    } else {
      modifyList(exceptions, handler)
    }
  } else {
    token <- session$token
    exceptions <- global$exceptions$sessions[[ token ]]
    global$exceptions$sessions[[ token ]] <- if (is.null(exceptions)) {
      handler
    } else {
      modifyList(exceptions, handler)
    }
  }
}

#' global handler for errors that print exception if user didn't process it
handle.error <- function(error, finally = NULL, session = NULL) {
  if (!handle.exceptions(error, finally = finally, session = session)) {
    message(paste("thrown in", error$origin))
    message(error$message)
    stop(cond)
  }
}

#' function that invoke global exception handler based on cond data
#' @param cond - structure with classes that indicate exception
handle.exceptions <- function(cond, finally = NULL, session = NULL) {
  result <- TRUE
  if (!is.null(cond$class)) {
    for (c in cond$class) {
      exceptions <- if (is.null(session)) {
        global$exceptions$global
      } else {
        global$exceptions$sessions[[ session$token ]]
      }
      if (is.function(exceptions[[ c ]])) {
        battery::withExceptions({
          ret <- battery:::invoke(exceptions[[ c ]], cond)
          if (identical(ret, FALSE) && result) {
            result <- FALSE
          }
        }, error = function(cond) {
          if (cond$message == "") {
            stop()
          } else if (c == "error") {
            message("[WARN] prevent recursive error, error exception thrown an error")
            message(cond$message)
            stop(cond)
          } else {
            create.error(cond, list(
              type = "exception",
              name = c
            ))
          }
        }, session = session)
      }
    }
  }
  if (is.function(finally)) {
    finally()
  }
  result
}

#' global exception handler that is used in battery instead of tryCatch
#' @param expr - any expression
#' @param error - function that will be triggered on error default NULL
#'                if added it should return add add meta data create.error(cond, list(...))
#'                it is used internally by battery, it can safely ignored.
#' @param finally - function that is always executed after exception is handled
#' @param session - optional shiny session to create exception handler only for given session
#' @export
withExceptions <- function(expr, error = NULL, finally = NULL, session = NULL) {
  invisible(withCallingHandlers({
    withRestarts(
      expr = expr,
      battery__ignore = function() {
        ## this is left empty on purpose
      }
    )
  },
  error = function(cond) {
    if (!inherits(cond, "shiny.silent.error")) {
      if (is.function(error)) {
        err <- battery:::invoke(error, cond)
        if (is.list(err) && err$class == "error") {
          handle.error(err, finally, session = session)
        }
      } else {
        err <- create.error(cond, list(
          type = "exception",
          name = c
        ))
        handle.error(err, finally, session = session)
      }
      invokeRestart("battery__ignore")
    }
  },
  battery__exception = function(cond) {
    if (!handle.exceptions(cond, finally, session = session)) {
      invokeRestart("battery__ignore")
    }
  }))
}


#' create structure that can be used to signal error in applications
#' @param cond - input from withCallingHandlers it should be unexpected error in app
#' @param meta - addition extra data that should be added into meta property
#' @export
create.error <- function(cond, meta) {
  c(cond, list(class = "error", meta = meta))
}

#' signal exception in applications
#' @param class - string vector that indicate class of the exception
#' @param message - string that indicate given exception
#' @param ... - eny extra data that should be added to the exception
#' @export
signal <- function(class, message, ...) {
  exception <- structure(
    c(list(...), list(message = message, class = class)),
    class = c("battery__exception", "condition")
  )
  signalCondition(exception)
}


#' higher order function for creating extend static method on every battery::Component
#' @param class - battery component constructor
#' @return function
make.extend <- function(class) {
  function(...) {
    child <- component(inherit = class, ...)
    child$extend <- make.extend(child)
    child
  }
}

## init extend on base battery component
BaseComponent$extend <- make.extend(BaseComponent)
