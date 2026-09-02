## 0.6.1
### Bugfix
* fix warnings about public destructor
* report errors that no exception handler processed, before they were silently discarded
* always invoke `finally` in `battery::withExceptions` exactly once, also when the
  expression finished without an error (it fixes ever growing indentation in the logs)
* fix example application that passed removed `error` argument to the root component
* fix mocks that failed on expressions with an empty argument (e.g. `m[1, ]`)
* fix `observeWrapper` that referred to an undefined `observer` when `once` was used
  without `exitHandler`
* drop undeclared `stringr` dependency, `battery::error` uses base R to read the
  bubble counter
* declare `utils::modifyList` and `uuid::UUIDgenerate` imports
* use `is.name()` instead of comparing `class()` to a string in the mocks
* correct the license declaration to `MIT + file LICENSE`, the full license text moved
  to `COPYING` (which is shipped with the package) because R requires `LICENSE` to
  hold only the year and the copyright holder
* document all arguments so `R CMD check` passes without warnings and notes

## 0.6.0
### Features
* add second argument (event name) to event emitter handler
### Bugfix
* fix multiple invocation of signal handlers on nested components

## 0.5.1
### Bugfix
* fix error handler mechanism

## 0.5.0
### Features
* add global error handler
* allow to use only session in root battery constructor
* simplify mocking session/input/output with `battery::Session$new()`

## 0.4.2
### Bugfix
* fix testing framework when parsing expression with inline functions

## 0.4.1
### Bugfix
* fix false warning about not triggered handlers on input events
* remove the hack for observeEvent

## 0.4.0
### Features
* eventEmitter based logging system
* load function (for loading components in R files)
* two types of eventEmitter (one is reactive so it can be used to trigger render functions)
### Bugfix
* fix calling trigger without arg and with NULL after auto-creating event with `on`
* fix reactive values that is not calling reactive observer (probably shiny bug)

## 0.3.1
### Bugfix
* refactor static services

## 0.3.0
### Features
* Event Emitters and static services

## 0.2.0
### Features
* new function clearMocks
### Bugfix
* fix mocks used with testthat::test_file

## 0.1.1
### Bugfix
* Small bug fix with error hanling

## 0.1.0
* First version
