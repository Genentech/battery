# wrapper over observe that works similar to `shiny::observeEvent`

wrapper over observe that works similar to
[`shiny::observeEvent`](https://rdrr.io/pkg/shiny/man/observeEvent.html)

## Usage

``` r
observeWrapper(
  eventExpr,
  handlerExpr,
  event.env = parent.frame(),
  handler.env = parent.frame(),
  ignoreNULL = TRUE,
  ignoreInit = FALSE,
  exitHandler = NULL,
  once = FALSE,
  debounceMillis = NULL
)
```

## Arguments

- eventExpr:

  \- reactive expression

- handlerExpr:

  \- expression that react to reactive expression

- event.env:

  \- environment used for reactive expression

- handler.env:

  \- environment used for handler expression

- ignoreNULL:

  \- if set to TRUE it will not invoke the handler if reactive value is
  NULL

- ignoreInit:

  \- if set to FALSE it will run handler expression on init

- exitHandler:

  \- additional function that will be called if once is set

- once:

  \- if set to TRUE it will invoke the handler once nad destroy observer

- debounceMillis:

  \- if not NULL it will use the value as time for
  [`shiny::debounce`](https://rdrr.io/pkg/shiny/man/debounce.html)

## Value

result of `shiny::obseve`
