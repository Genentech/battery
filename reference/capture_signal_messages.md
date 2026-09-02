# helper function that capture all signal messages into a vector it can be used in unit tests to check if the function sent proper messages

helper function that capture all signal messages into a vector it can be
used in unit tests to check if the function sent proper messages

## Usage

``` r
capture_signal_messages(expr, signal, session = NULL)
```

## Arguments

- expr:

  \- any expression

- signal:

  \- charcater vector with signals that should be captured

- session:

  \- optional shiny session used to find the exception handlers
