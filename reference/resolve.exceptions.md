# helper function that return exception handlers that apply to given session

handlers registered for the session take precedence, if the session has
no handlers it falls back to the global ones

## Usage

``` r
resolve.exceptions(session = NULL)
```

## Arguments

- session:

  \- optional shiny session
