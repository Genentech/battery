# Package index

## Components

Building the tree of components. Every component is created with
[`component()`](https://genentech.github.io/battery/reference/component.md)
and inherits the methods documented in `BaseComponent`.

- [`component()`](https://genentech.github.io/battery/reference/component.md)
  : Basic function to create battery components.
- [`BaseComponent`](https://genentech.github.io/battery/reference/BaseComponent.md)
  : Root component that have no parent,

## Events

Event emitters that can be used as services shared by the whole tree.

- [`EventEmitter`](https://genentech.github.io/battery/reference/EventEmitter.md)
  : Event emitter is inspired by multiple implementation in JavaScript

## Exceptions

Signals and errors raised anywhere in the component tree are handled in
one place, see the
[`vignette("battery-components")`](https://genentech.github.io/battery/articles/battery-components.md)
for the whole picture.

- [`exceptions()`](https://genentech.github.io/battery/reference/exceptions.md)
  : function allows to add global exception handler for all battery
  components
- [`withExceptions()`](https://genentech.github.io/battery/reference/withExceptions.md)
  : global exception handler that is used in battery instead of tryCatch
- [`signal()`](https://genentech.github.io/battery/reference/signal.md)
  : signal exception in applications
- [`error()`](https://genentech.github.io/battery/reference/error.md) :
  helper function that can be used in exception handler to trigger error
  handler
- [`create.error()`](https://genentech.github.io/battery/reference/create.error.md)
  : create structure that can be used to signal error in applications
- [`pause()`](https://genentech.github.io/battery/reference/pause.md) :
  helper function that can be used in exception handler to pause just
  this context
- [`end()`](https://genentech.github.io/battery/reference/end.md) :
  helper function that can be used in exception handler to stop whole
  application
- [`capture_signal_messages()`](https://genentech.github.io/battery/reference/capture_signal_messages.md)
  : helper function that capture all signal messages into a vector it
  can be used in unit tests to check if the function sent proper
  messages

## Testing

Mocks that replace the shiny reactive machinery so components can be
tested without a running shiny session.

- [`Session`](https://genentech.github.io/battery/reference/Session.md)
  : Shiny Session Mock class
- [`useMocks()`](https://genentech.github.io/battery/reference/useMocks.md)
  : Helper function to be used in test files that overwrite shiny
  functions (after test you should run clearMocks since they are global
  and if you run shiny application after test that use mocks it will
  break)
- [`clearMocks()`](https://genentech.github.io/battery/reference/clearMocks.md)
  : Helper function to be used at the end of test files (useful if same
  session is used to run test and application e.g. RStudio)
- [`activeInput()`](https://genentech.github.io/battery/reference/activeInput.md)
  : Function create mock for shiny input
- [`activeOutput()`](https://genentech.github.io/battery/reference/activeOutput.md)
  : Function create active binding output mock to be used with renderUI
  mock
- [`renderUIMock()`](https://genentech.github.io/battery/reference/renderUIMock.md)
  : RenderUI just send exression to output active prop, the prop need to
  be added first if renderUI is called in constructor and it use
  self\$ns() you can pass component.id to constructor so instance will
  have same id and you can generate the name before constructor is
  called - using: output\$new(name) the output will parse the expression
  and bind input and output note: input can have different name for
  instance events in components
- [`makeReactiveBinding()`](https://genentech.github.io/battery/reference/makeReactiveBinding.md)
  : Mock for shiny::makeReactiveBinding to be injected into components
  limiation it can't be called after value is added to environment (it
  will work in components)
- [`isolate()`](https://genentech.github.io/battery/reference/isolate.md)
  : name used only inside renderUI in substitute phase
- [`is.active.binding()`](https://genentech.github.io/battery/reference/is.active.binding.md)
  : Function check if name is active biding inside environment
  activeInput or activeOutput
- [`is.active.input()`](https://genentech.github.io/battery/reference/is.active.input.md)
  : Function for checking if object is actie input - used by
  extractActiveInputs
- [`is.active.output()`](https://genentech.github.io/battery/reference/is.active.output.md)
  : Function check if obj is enviroment that is result of activeOutput
  function

## Utilities

- [`load()`](https://genentech.github.io/battery/reference/load.md) :
  Function load all R file components from directory, component classes
  will be global
- [`%>%`](https://genentech.github.io/battery/reference/classicPipe.md)
  : Pipe operator
- [`is`](https://genentech.github.io/battery/reference/is.md) : is
  generic function
