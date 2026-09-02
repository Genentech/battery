# Traverse substitute expressions and function invocation and extract all references to active elements created by activeInput and uiOutput

Traverse substitute expressions and function invocation and extract all
references to active elements created by activeInput and uiOutput

## Usage

``` r
extractActiveNames(arg)
```

## Arguments

- arg:

  \- named list: expr - substitute expr, env - parent env (value
  returned from battery::renderUI mock)
