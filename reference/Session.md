# Shiny Session Mock class

Mock for Session object, only some tests require this right now but more
tests may require this session object in the future Right now only
services need session\$destroy() method to clear services so you can
create same service in more then one test

with version 0.5.0 session mock can be used as the only object passed to
root component constructor.

## Public fields

- `token`:

  \- string used by battery to distinguish users

- `input`:

  \- shiny input

- `output`:

  \- shiny output

## Methods

### Public methods

- [`Session$new()`](#method-Session-new)

- [`Session$destroy()`](#method-Session-destroy)

- [`Session$onSessionEnded()`](#method-Session-onSessionEnded)

- [`Session$clone()`](#method-Session-clone)

------------------------------------------------------------------------

### Method `new()`

Session mock constructor

#### Usage

    Session$new(
      token = NULL,
      input = battery::activeInput(),
      output = battery::activeOutput()
    )

#### Arguments

- `token`:

  \- optional token used for testing to create different users that
  should get different data for services and globals

- `input`:

  \- mock for shiny input

- `output`:

  \- mock for shiny output

------------------------------------------------------------------------

### Method `destroy()`

Mock for destroy session

it will trigger handlers added by `onSessionEnded`

#### Usage

    Session$destroy()

------------------------------------------------------------------------

### Method `onSessionEnded()`

Mock for the function that add handler on session destroy

#### Usage

    Session$onSessionEnded(fn)

#### Arguments

- `fn`:

  \- function that will be called when `destroy` is called

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Session$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r

TestComponent <- battery::component(
  "TestComponent",
  public = list(
    constructor = function() {
       print(self$session$token)
    }
  )
)
session <- battery::Session$new("Test")
input <- activeInput()
output <- activeOutput()
component <- TestComponent$new(input = input, output = output, session = session)
#> [1] "Test"
```
