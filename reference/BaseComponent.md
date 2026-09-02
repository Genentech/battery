# Root component that have no parent,

need to be called with input, output and session. it should not be used
directly, only using
[`component`](https://genentech.github.io/battery/reference/component.md)
function.

## Methods

- Documentation:

  For full documentation of each method go to
  https://stash.intranet.roche.com/stash/projects/DIVOS/repos/battery/browse

- `BaseComponent$new(...)`:

  This method is used to create base battery object, it should never be
  created directly. Battery components should be created as inherited
  from this BaseComponent, but this should be done only using
  `component` function

- `getById`:

  Method return component with specific id

- `appendChild`:

  Method add battery component as child this current component

- `removeChild`:

  Method remove child component complementary to appendChild

- `ns`:

  Method used to create namespaced identifier

- `createEvent`:

  Method will create battery event

- `emit`:

  Propagate events from child to parent

- `broadcast`:

  Propagate events from parent to all children

- `connect`:

  Helper method that will create binding between input event from shiny
  and battery event

- `disconnect`:

  Method remove binding between input element and compnents events

- `on`:

  Add event listener to given internal event or native input

- `off`:

  Method removes event listener(s) added by `on`

- `class`:

  Method return name of this class - same as classname when crating the
  class

- `destroy`:

  Method remove all observers created for this component

- `finalize`:

  R6Class method that will be called when object is destroyed

- `addService`:

  Method dynamically add service to battery component system

- `template`:

  Helper method that create
  [`shiny::htmlTemplate`](https://rstudio.github.io/htmltools/reference/htmlTemplate.html)
  with self and private as defaults variables

- `path`:

  Method return path to the object in battery components tree

- `log`:

  Method log messages that can be listen to with `logger` helper

- `logger`:

  Shortcut function to add listener to logger

- `render`:

  Function that should be overwritten in battery component

## Public fields

- `id`:

  \- string that

- `name`:

  \- component instance name, set using `parent$appendChild(name)` or
  `component$new(parent = self, component.name = name)`

- `services`:

  \- environment that hold static services - objects shared across
  battery components tree. Services can be added using
  `component$addService(name, ANY)`

- `events`:

  \- environment that will hold reactive values added by on or
  createEvent method

- `parent`:

  \- parent component

- `children`:

  \- list of components that are children of the component, this list
  will be used to when using `component$broadcast("name")`

- `input`:

  \- shiny input object added in constructor of root class or inherited
  from parent

- `output`:

  \- shiny output object added in constructor of root class or inherited
  from parent

- `session`:

  \- shiny session object added in constructor of root class or
  inherited from parent

- `static`:

  \- environment that can be used to save property into class, it will
  be shared with all instances of same battery component.

## Methods

### Public methods

- [`BaseComponent$new()`](#method-BaseComponent-new)

- [`BaseComponent$getById()`](#method-BaseComponent-getById)

- [`BaseComponent$appendChild()`](#method-BaseComponent-appendChild)

- [`BaseComponent$removeChild()`](#method-BaseComponent-removeChild)

- [`BaseComponent$ns()`](#method-BaseComponent-ns)

- [`BaseComponent$createEvent()`](#method-BaseComponent-createEvent)

- [`BaseComponent$trigger()`](#method-BaseComponent-trigger)

- [`BaseComponent$emit()`](#method-BaseComponent-emit)

- [`BaseComponent$broadcast()`](#method-BaseComponent-broadcast)

- [`BaseComponent$connect()`](#method-BaseComponent-connect)

- [`BaseComponent$disconnect()`](#method-BaseComponent-disconnect)

- [`BaseComponent$on()`](#method-BaseComponent-on)

- [`BaseComponent$off()`](#method-BaseComponent-off)

- [`BaseComponent$class()`](#method-BaseComponent-class)

- [`BaseComponent$destroy()`](#method-BaseComponent-destroy)

- [`BaseComponent$addService()`](#method-BaseComponent-addService)

- [`BaseComponent$template()`](#method-BaseComponent-template)

- [`BaseComponent$path()`](#method-BaseComponent-path)

- [`BaseComponent$log()`](#method-BaseComponent-log)

- [`BaseComponent$logger()`](#method-BaseComponent-logger)

- [`BaseComponent$render()`](#method-BaseComponent-render)

- [`BaseComponent$clone()`](#method-BaseComponent-clone)

------------------------------------------------------------------------

### Method `new()`

R6Class method that will be called when object is destroyed it just
calls `destroy` native R6 class constructor

this should never be overwriten by child components, they should only
overwrite constructor that is not as problematic when not called super

#### Usage

    BaseComponent$new(
      input = NULL,
      output = NULL,
      session = NULL,
      parent = NULL,
      component.name = NULL,
      services = NULL,
      spy = FALSE,
      ...
    )

#### Arguments

- `input`:

  \- shiny input object added in constructor of root class or inherited
  from parent

- `output`:

  \- shiny output object added in constructor of root class or inherited
  from parent

- `session`:

  \- shiny session object added in constructor of root class or
  inherited from parent

- `parent`:

  \- parent battery component, if used you don't need to add `input`,
  `output` and `session`

- `component.name`:

  \- name of the component to be used in component\$parent\$children

- `services`:

  \- list of any static services that can be created on component
  initialization

- `spy`:

  \- used in unit test to record component method calls (only user
  methods are recorded)

- `...`:

  \- everything else is passed to `constructor` method that should be
  used in user components Method return component with specific id

  it will search the tree of components find name with specific id

------------------------------------------------------------------------

### Method `getById()`

#### Usage

    BaseComponent$getById(id)

#### Arguments

- `id`:

  \- string - id of the component to search

#### Returns

Battery component Method add battery component as child this current
component

this function is called if you pass component.name to constructor
otherwise it should be called to create proper tree. This is required so
`component$broadcast` and `component$emit` work properly

------------------------------------------------------------------------

### Method `appendChild()`

#### Usage

    BaseComponent$appendChild(name, child)

#### Arguments

- `name`:

  \- string to be used as name

- `child`:

  \- battery component Method remove child component complementary to
  appendChild

  it can be used with name or the component

------------------------------------------------------------------------

### Method `removeChild()`

#### Usage

    BaseComponent$removeChild(name = NULL, child)

#### Arguments

- `name`:

  \- name of the component to remove

- `child`:

  \- battery component to remove Method used to create namespaced
  identifier

------------------------------------------------------------------------

### Method `ns()`

#### Usage

    BaseComponent$ns(name)

#### Arguments

- `name`:

  \- name to be used inside shiny input or output

#### Examples

    \dontrun{
     battery::component(
       classname = "Plot",
       public = list(
         constructor = function() {
           self$output[[ self$ns("plot") ]] <- renderPlot({
             ## ...
           })
        },
        render = function() {
          shiny::div(
            class = "container",
            plotOutput(self$ns("plot"))
          )
        }
      )
    )
    }

------------------------------------------------------------------------

### Method `createEvent()`

Method will create battery event

this event can be triggered from R code it can also be broadcasted this
function is called automatically when using on to create observer

#### Usage

    BaseComponent$createEvent(name, value = NULL)

#### Arguments

- `name`:

  \- string, name of the event

- `value`:

  \- initial value of the event reactive variable

------------------------------------------------------------------------

### Method `trigger()`

Method will trigger the event. It call every observer and invalidate
every reactive context

#### Usage

    BaseComponent$trigger(name, data = NULL, .force = TRUE, .level = 0)

#### Arguments

- `name`:

  \- name of the event to fire

- `data`:

  \- data to be used to trigger the event if function use

- `.force`:

  \- internal option to disable forcing of reactive events

- `.level`:

  \- internal option for logger, that is used to created indent

------------------------------------------------------------------------

### Method `emit()`

Propagate events from child to parent

it will recursivly walk whole tree, and trigger only events that have
reactive values added with `createEvent` it will also trigger all
observers added with `on`

#### Usage

    BaseComponent$emit(
      name,
      value = NULL,
      target = NULL,
      include.self = FALSE,
      .level = 0
    )

#### Arguments

- `name`:

  \- name of the event to propagate

- `value`:

  \- optional value to to set on reactive values (it will be access from
  component\$events or inside observer

- `target`:

  \- optioanl target that should be passed along the event can only be
  access from event handler added by `component$on`

- `include.self`:

  \- shoult it also trigger on this component or only on children

- `.level`:

  \- internal option for logger, that is used to created indent

#### Examples

    \dontrun{
    App <- battery::component(
      classname = "App",
      public = list(
        constructor = function() {
          self$on("update", function() {
            print("I need to update")
          })
          panel <- Panel$new(parent = self, component.name = "panel")
          self$outptu[[self$ns("panel")]] <- renderUI({
             panel$render()
          })
        },
        render = function() {
          shiny::tags$div(
            #...
            uiOutput(self$ns("panel"))
          )
        }
      )
    )
    Panel <- battery::component(
      classname = "Panel",
      public = list(
        constructor = function() {
          self$on(self$ns("button"), function() {
            self$emit("update")
          }, input = TRUE)
        },
        render = function() {
           shiny::tags$div(
              #...
              actionButton(self$ns("button"), "Click Me")
           )
        }
      )
    )
    ## clicking on button will emit the event to the parent and print the message
    }

------------------------------------------------------------------------

### Method `broadcast()`

Propagate events from parent to all children

methods similar to `emit` but it propagete event to children if called
on root component it will send message to all components inside the
tree.

#### Usage

    BaseComponent$broadcast(
      name,
      value = NULL,
      target = NULL,
      include.self = FALSE,
      .level = 0
    )

#### Arguments

- `name`:

  \- string - name of the component to trigger

- `value`:

  \- default value adde to component\$events

- `target`:

  \- string that indicate which battery component trigger the event it
  can be omited if so it will use same object that called the method

- `include.self`:

  \- flag that indicate if event should also be called on self

- `.level`:

  \- internal option for logger, that is used to created indent

#### Examples

    \dontrun{
    App <- battery::component(
      classname = "App",
      public = list(
        count = 0,
        constructor = function() {
          self$label <- label
          self$on(self$ns("button"), function() {
            self$count <- self$count + 2
            self$broadcast("update", paste0("Update_number_", count))
          }, input = TRUE)
          counter <- Counter$new(parent = self, component.name = "counter")
          self$outptu[[self$ns("counter")]] <- renderUI({
             panel$render()
          })
        },
        inc = function(count) {
          self$label <- paste0("label_", count)
        },
        render = function() {
          shiny::tags$div(
            actionButton(self$ns("button"), "Click Me"),
            uiOutput(self$ns("counter"))
          )
        }
      )
    )
    Counter <- battery::component(
      classname = "Counter",
      public = list(
        counter = 0,
        constructor = function() {
          self$createEvent("update")
        },
        render = function() {
           self$counter <- self$counter + 1
           shiny::tags$div(
              paste("Counter", self$counter),
              self$events$update$value
           )
        }
      )
    )
    ## first it will render the child with "Counter 1" (the value of events reactive
    ## reactive variable will be NULL, default value of events)
    ## after clicking the button it will increase the count in App by 2
    ## send event to children and it will in turn trigger render child again
    ## so it will display "Counter 2" and "Update_number_2"
    ##
    ## child render will be called twice and event handler on button once
    }

------------------------------------------------------------------------

### Method `connect()`

Helper method that will create binding between input event from shiny
and battery event

#### Usage

    BaseComponent$connect(event, elementId)

#### Arguments

- `event`:

  \- name of the event

- `elementId`:

  \- id of the HTML element (shiny input it should be value from
  `self$ns`)

------------------------------------------------------------------------

### Method `disconnect()`

Method remove binding between input element and compnents events

complementary to connect

#### Usage

    BaseComponent$disconnect(elementId)

#### Arguments

- `elementId`:

  \- it of the input element

------------------------------------------------------------------------

### Method `on()`

Add event listener to given internal event or native input

#### Usage

    BaseComponent$on(
      events,
      handler,
      input = FALSE,
      enabled = TRUE,
      single = TRUE,
      debounceMillis = NULL,
      once = FALSE,
      ignoreNULL = TRUE,
      init = FALSE
    )

#### Arguments

- `events`:

  \- character or character vector of internal event or input id

- `handler`:

  \- function that can have value and target parameters (optional)

- `input`:

  \- boolean that's indicate if event should be added to input otherwise
  it's internal battery event

- `enabled`:

  \- boolean that enable event to easy toggle event

- `single`:

  \- if used it will create only one event, it will always destroy old
  one

- `debounceMillis`:

  \- if not NULL it will use
  [`shiny::debounce`](https://rdrr.io/pkg/shiny/man/debounce.html) on
  the function

- `once`:

  \- argument works the same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- `ignoreNULL`:

  \- argument works the same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- `init`:

  \- indicate if event should be triggered on init

#### Examples

    \dontrun{

    self$on(self$ns("inputValue"), function(value) {
       print(paste("Input value is ", value))
    }, input = TRUE)

    self$on(self$ns("save"), function() {
       print("user click save")
    }, input = TRUE)


    self$on("event", function(value, target) {
      ## this event can be fired with trigger/emit/broadcast
    })
    }

------------------------------------------------------------------------

### Method `off()`

Method removes event listener(s) added by `on`

if handler is NULL it will remove all listeners for a given event name

#### Usage

    BaseComponent$off(events, handler = NULL)

#### Arguments

- `events`:

  \- vector or string with names of events to remove

- `handler`:

  \- optional event handler

------------------------------------------------------------------------

### Method [`class()`](https://rdrr.io/r/base/class.html)

Method return name of this class - same as classname when crating the
class

#### Usage

    BaseComponent$class()

#### Returns

string - class name

------------------------------------------------------------------------

### Method `destroy()`

Method dfestroy component

It removes all observers created for this component also clear it also
use other clean ups.

#### Usage

    BaseComponent$destroy()

------------------------------------------------------------------------

### Method `addService()`

Method dynamically add service to battery component system

only one service with giben name can be added to the tree same object
will be accessed in every component in the tree. there is one default
service logger that is `EventEmitter`

#### Usage

    BaseComponent$addService(name, service)

#### Arguments

- `name`:

  \- name of the service by which you access the service e.g.
  self\$service\$foo

- `service`:

  \- object used as service - it can be any object

------------------------------------------------------------------------

### Method `template()`

Helper method that create
[`shiny::htmlTemplate`](https://rstudio.github.io/htmltools/reference/htmlTemplate.html)
with self and private as defaults variables to be used in html (inside
`{{ }}`)

#### Usage

    BaseComponent$template(filename, ...)

#### Arguments

- `filename`:

  \- name of the template

- `...`:

  \- any number of arguments that will be accessible in template

------------------------------------------------------------------------

### Method `path()`

Method return path to the object in battery components tree

#### Usage

    BaseComponent$path()

#### Returns

vector of strings of id of the components in the tree

------------------------------------------------------------------------

### Method [`log()`](https://rdrr.io/r/base/Log.html)

Method log message that can be listen to, best way to add listener is to
use self\$logger("name", fn) in root component constructor each event is
triggered with list(id, type, path, message, args)

#### Usage

    BaseComponent$log(levels, message, type = "battery", ...)

#### Arguments

- `levels`:

  \- vector of characters to listen (default names in battery are
  "battery" and "info")

- `message`:

  \- message to log

- `type`:

  \- default battery - additional value to distinguish the message in
  battery type is name of the method - or "method" inside user method

- `...`:

  \- any arguments are added into args property

------------------------------------------------------------------------

### Method `logger()`

Shortcut function to add listener to logger

#### Usage

    BaseComponent$logger(level, fn)

#### Arguments

- `level`:

  \- character vector or character of log levels to listen

- `fn`:

  \- function handler

------------------------------------------------------------------------

### Method `render()`

Function that should be overwritten in battery component

this is convention that this function should return HTML (shiny tags)
this function can have reactive value self\$events. render function
should not have children render if possible becasue update of parent
will rerender the children. The proper way is to use renderUI in
constructor and renderUI in render function for the children.

#### Usage

    BaseComponent$render()

#### Returns

overwriten render by convention should return shiny tags

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    BaseComponent$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

## ------------------------------------------------
## Method `BaseComponent$ns`
## ------------------------------------------------

if (FALSE) { # \dontrun{
 battery::component(
   classname = "Plot",
   public = list(
     constructor = function() {
       self$output[[ self$ns("plot") ]] <- renderPlot({
         ## ...
       })
    },
    render = function() {
      shiny::div(
        class = "container",
        plotOutput(self$ns("plot"))
      )
    }
  )
)
} # }

## ------------------------------------------------
## Method `BaseComponent$emit`
## ------------------------------------------------

if (FALSE) { # \dontrun{
App <- battery::component(
  classname = "App",
  public = list(
    constructor = function() {
      self$on("update", function() {
        print("I need to update")
      })
      panel <- Panel$new(parent = self, component.name = "panel")
      self$outptu[[self$ns("panel")]] <- renderUI({
         panel$render()
      })
    },
    render = function() {
      shiny::tags$div(
        #...
        uiOutput(self$ns("panel"))
      )
    }
  )
)
Panel <- battery::component(
  classname = "Panel",
  public = list(
    constructor = function() {
      self$on(self$ns("button"), function() {
        self$emit("update")
      }, input = TRUE)
    },
    render = function() {
       shiny::tags$div(
          #...
          actionButton(self$ns("button"), "Click Me")
       )
    }
  )
)
## clicking on button will emit the event to the parent and print the message
} # }

## ------------------------------------------------
## Method `BaseComponent$broadcast`
## ------------------------------------------------

if (FALSE) { # \dontrun{
App <- battery::component(
  classname = "App",
  public = list(
    count = 0,
    constructor = function() {
      self$label <- label
      self$on(self$ns("button"), function() {
        self$count <- self$count + 2
        self$broadcast("update", paste0("Update_number_", count))
      }, input = TRUE)
      counter <- Counter$new(parent = self, component.name = "counter")
      self$outptu[[self$ns("counter")]] <- renderUI({
         panel$render()
      })
    },
    inc = function(count) {
      self$label <- paste0("label_", count)
    },
    render = function() {
      shiny::tags$div(
        actionButton(self$ns("button"), "Click Me"),
        uiOutput(self$ns("counter"))
      )
    }
  )
)
Counter <- battery::component(
  classname = "Counter",
  public = list(
    counter = 0,
    constructor = function() {
      self$createEvent("update")
    },
    render = function() {
       self$counter <- self$counter + 1
       shiny::tags$div(
          paste("Counter", self$counter),
          self$events$update$value
       )
    }
  )
)
## first it will render the child with "Counter 1" (the value of events reactive
## reactive variable will be NULL, default value of events)
## after clicking the button it will increase the count in App by 2
## send event to children and it will in turn trigger render child again
## so it will display "Counter 2" and "Update_number_2"
##
## child render will be called twice and event handler on button once
} # }

## ------------------------------------------------
## Method `BaseComponent$on`
## ------------------------------------------------

if (FALSE) { # \dontrun{

self$on(self$ns("inputValue"), function(value) {
   print(paste("Input value is ", value))
}, input = TRUE)

self$on(self$ns("save"), function() {
   print("user click save")
}, input = TRUE)


self$on("event", function(value, target) {
  ## this event can be fired with trigger/emit/broadcast
})
} # }
```
