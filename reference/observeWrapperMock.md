# Function used the same as `battery:::observeWrapper` (based on `shiny::observeEvent`)

Function used the same as `battery:::observeWrapper` (based on
[`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html))

## Usage

``` r
observeWrapperMock(
  eventExpr,
  handlerExpr,
  event.env = parent.frame(),
  handler.env = parent.frame(),
  ignoreInit = FALSE,
  ignoreNULL = TRUE,
  once = FALSE,
  ...
)
```

## Arguments

- eventExpr:

  \- same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- handlerExpr:

  \- same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- event.env:

  \- same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- handler.env:

  \- same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- ignoreInit:

  \- same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- ignoreNULL:

  \- same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- once:

  \- same as in
  [`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

- ...:

  \- reset the params from `battery:::observeWrapper`

## Value

list with destroy method - same as
[`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)
