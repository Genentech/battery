# global handler for errors that print exception if user didn't process it

global handler for errors that print exception if user didn't process it

## Usage

``` r
handle.error(error, meta = NULL, session = NULL)
```

## Arguments

- error:

  \- error structure created by `create.error`

- meta:

  \- context of the error (origin, type, ...)

- session:

  \- optional shiny session used to find the exception handlers
