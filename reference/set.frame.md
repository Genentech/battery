# Function overwrite value in parent frame enviroment It start searching for variable starting from frame parameter

Function overwrite value in parent frame enviroment It start searching
for variable starting from frame parameter

## Usage

``` r
set.frame(value, name = NULL, frame = 1)
```

## Arguments

- value:

  \- value that will be added to enviroment

- name:

  \- name of the variable if not set it will use variable name from
  value

- frame:

  \- integer that indicate from which parent it should start searching
  for variable
