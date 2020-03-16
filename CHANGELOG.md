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
