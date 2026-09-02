# Function is parsing code like \`foo() + bar()\` that have type of language and it also detect cases of \`foo()\`, \`x\$foo()\` or \`x\[\[name\]\]()\` We do this to find functions and methods calls and extract names from inside of the functions bodies - body return same data as substitute so we can call extractActiveNames on body

Function is parsing code like \`foo() + bar()\` that have type of
language and it also detect cases of \`foo()\`, \`x\$foo()\` or
\`x\[\[name\]\]()\` We do this to find functions and methods calls and
extract names from inside of the functions bodies - body return same
data as substitute so we can call extractActiveNames on body

## Usage

``` r
parse.function(item, env)
```

## Arguments

- item:

  \- deparse expression

- env:

  \- enviroment
