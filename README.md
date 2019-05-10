```                            
 _____     _   _               
| __  |___| |_| |_ ___ ___ _ _ 
| __ -| .'|  _|  _| -_|  _| | |
|_____|__,|_| |_| |___|_| |_  |
                          |___|
```

Battery - R6Class based component architecture for Shiny apps


## Mocks

creating empty input

```R
input <- activeInput()
```

input with single ative binding

```R
input <- activeInput(foo = function(value) {
  if (missing(value)) {
    self[["__foo"]]
  } else {
    self[["__foo"]] <- value
  }
})
```

NULL will create default setter/getter and inital active binding

```R
input <- activeInput(foo = NULL)
```

### ObserveEvent mock

```R
observeEvent(input$foo, {
  print(paste0("value set to ", input$foo))
})
```

observeEvent just call input$on and it will create listener for reactive value
it will not create active binding so you need to call new first just in case you
don't know what the value is before observeEvent is called (can be called in component
constructor with `self$ns` as name the you can ten get call self$ns on component to create
actual biding after component is created)


this will create active binding with default setter/getter

```R
input$new("foo")
```

```R
input$new(component$ns("save"))
```

reactive value with that may have different logic here same code as default


```R
input#new("foo", function(value) {
   if (missing(value)) {
    self[["__foo"]]
  } else {
    self[["__foo"]] <- value
  }
})
```

if you set the value

```R
input$foo <- 10
```

observer expression will be avaluated also input$listeners will have data for each listener
that will have data about each call to each listener with old and new values

usually there will be single listener for single active value

### Output

Output and connection between input and output can be explained using this example code

```R
input <- activeInput(foo = NULL)


input$foo <- 100

output <- activeOutput(bar = NULL)

output$bar <- renderUI({ input$foo + 10 })

print(output$bar) ## 110

input$foo <- 200

print(input$foo) ## 200
print(output$bar) ## 210
```
