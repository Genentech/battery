# Helper function to be used in test files that overwrite shiny functions (after test you should run clearMocks since they are global and if you run shiny application after test that use mocks it will break)

Helper function to be used in test files that overwrite shiny functions
(after test you should run clearMocks since they are global and if you
run shiny application after test that use mocks it will break)

## Usage

``` r
useMocks()
```
