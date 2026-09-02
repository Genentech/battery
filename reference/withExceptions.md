# global exception handler that is used in battery instead of tryCatch

global exception handler that is used in battery instead of tryCatch

## Usage

``` r
withExceptions(expr, error = NULL, finally = NULL, meta = NULL, session = NULL)
```

## Arguments

- expr:

  \- any expression

- error:

  \- function that will be triggered on error default NULL if added it
  should return add add meta data create.error(cond, list(...)) it is
  used internally by battery, it can safely ignored.

- finally:

  \- function that is always executed when the expression is done, no
  matter if it raised an error, signalled an exception or finished
  normally (it is always invoked exactly once)

- meta:

  \- context of the expression (origin, type, ...) passed to the
  handlers

- session:

  \- optional shiny session to create exception handler only for given
  session
