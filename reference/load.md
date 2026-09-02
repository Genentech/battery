# Function load all R file components from directory, component classes will be global

Function load all R file components from directory, component classes
will be global

## Usage

``` r
load(
  path,
  pattern = "*.R$",
  ignore = NULL,
  recursive = TRUE,
  ignore.case = TRUE,
  local = TRUE
)
```

## Arguments

- path:

  \- path to directory

- pattern:

  \- pattern used to selected the files (default all R files)

- ignore:

  \- string or vector of strings with fillanme files that should be
  ignored

- recursive:

  \- if set to TRUE (default) loading of files will be recursive

- ignore.case:

  \- should it ignore case of pattern when searching for files

- local:

  \- TRUE, FALSE or enviroment use in source - if value is TRUE it will
  use parent.frame()
