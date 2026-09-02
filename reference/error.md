# helper function that can be used in exception handler to trigger error handler

helper function that can be used in exception handler to trigger error
handler

## Usage

``` r
error(message = NULL, bubble = FALSE)
```

## Arguments

- message:

  \- optional message that should be character string

- bubble:

  \- if set to TRUE the message is marked so it can be tracked while it
  propagates through nested handlers
