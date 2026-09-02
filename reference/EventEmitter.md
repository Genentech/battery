# Event emitter is inspired by multiple implementation in JavaScript

it can be used indepentely of battery events to send data from one
component to different one, without the need to broadcast and emit
events it can be used with services, in addition to can also be used as
reactive values in shiny reactive context like renderUI or battery
component render function

## Methods

- Documentation:

  For full documentation of each method go to
  https://stash.intranet.roche.com/stash/projects/DIVOS/repos/battery/browse

- `EventEmitter$new(...)`:

  This method is used to create EventEmitter

- `on`:

  Method add new handler for given event

- `emit`:

  Method emit event add trigger all handlers added by `on`

- `off`:

  Method removes single handler or all handlers and observer of no
  handler left

- `finalize`:

  Destructor - clean up the data

## Public fields

- `.calls`:

  \- if EventEmitter is created with `spy = TRUE` it will add method
  calls to this list

- `events`:

  \- environment with reactive objects if EventEmitter created wtih
  `shiny = TRUE`, otherwise it's normal environment but with R active
  bindings that can't be used in shiny render functions

## Methods

### Public methods

- [`EventEmitter$new()`](#method-EventEmitter-new)

- [`EventEmitter$on()`](#method-EventEmitter-on)

- [`EventEmitter$emit()`](#method-EventEmitter-emit)

- [`EventEmitter$off()`](#method-EventEmitter-off)

- [`EventEmitter$clone()`](#method-EventEmitter-clone)

------------------------------------------------------------------------

### Method `new()`

R6Class destructor. Cleans up event handlers

R6Class Constructor. If shiny option is used it will create
ReactiveBinding with hack that always trigger reactive context (the same
cases the
[`component`](https://genentech.github.io/battery/reference/component.md)
events - this is shiny bug)

#### Usage

    EventEmitter$new(spy = FALSE, shiny = FALSE)

#### Arguments

- `spy`:

  \- if set to `TRUE` it will log all method calls - for debugging

- `shiny`:

  \- use this option if you need reactive value to trigger render
  handlers in shiny

------------------------------------------------------------------------

### Method `on()`

Method add new handler for given event

create new observer if doesn't exists and add handler to the list to
that observer, we use single observer so ... will be of no use on next
handler on single event, we keep it just in case it may be of use

#### Usage

    EventEmitter$on(events, handler, ...)

#### Arguments

- `events`:

  \- string or character vector with name of the events

- `handler`:

  \- function used as handler for give event

- `...`:

  \- used only once for observeEvent if shiny mode is used

------------------------------------------------------------------------

### Method `emit()`

Method emit event add trigger all handlers added by `on`

#### Usage

    EventEmitter$emit(name, data = NULL)

#### Arguments

- `name`:

  \- character - name of the event to fire

- `data`:

  \- optional data that can be accessed in handler

------------------------------------------------------------------------

### Method `off()`

Method removes single handler or all handlers and observer of no handler
left

#### Usage

    EventEmitter$off(events, handler = NULL)

#### Arguments

- `events`:

  \- string or character vector with name of the events to destroy

- `handler`:

  \- optional handler if not NULL it will remove only given handler

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    EventEmitter$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

e <- EventEmitter$new()
e$on("sessionCreated", function(value) {
  print(value$name)
})

## and in different part of the application

e$emit("sessionCreated", list(name = "My Session"))
#> [1] "My Session"
```
