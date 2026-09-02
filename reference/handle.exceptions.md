# function that invoke global exception handler based on cond data

function that invoke global exception handler based on cond data

## Usage

``` r
handle.exceptions(cond, meta = NULL, session = NULL)
```

## Arguments

- cond:

  \- structure with classes that indicate exception

- meta:

  \- context of the exception (origin, type, ...)

- session:

  \- optional shiny session used to find the exception handlers
