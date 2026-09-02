# Mock for shiny::makeReactiveBinding to be injected into components limiation it can't be called after value is added to environment (it will work in components)

Mock for shiny::makeReactiveBinding to be injected into components
limiation it can't be called after value is added to environment (it
will work in components)

## Usage

``` r
makeReactiveBinding(name, env)
```

## Arguments

- name:

  \- name of the binding

- env:

  \- enviroment that will be used to create a binding
