# Force of reactive value trigger

Force of reactive value trigger

## Usage

``` r
force(fn, session = shiny::getDefaultReactiveDomain())
```

## Arguments

- fn:

  \- function that can have reactive value asignment that will alway
  invalidate reactive context

- session:

  \- session object - in battery there is only one session object
