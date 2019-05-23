#'
#' shiny: input, output observeEvent and renderUI mocks with active binding and exosed data
#'
#' usage:
#'
#' empty input
#'
#' input <- activeInput()
#'
#' input with single ative binding
#'
#' input <- activeInput(foo = function(value) {
#'    if (missing(value)) {
#'      self[["__foo"]]
#'    } else {
#'      self[["__foo"]] <- value
#'    }
#' })
#'
#' NULL will create default setter/getter and inital active binding
#'
#' input <- activeInput(foo = NULL)
#'
#' observeEvent(input$foo, {
#'    print(paste0("value set to ", input$foo))
#' })
#'
#' observeEvent just call input$on and it will create listener for reactive value
#' it will not create active binding so you need to call new first just in case you
#' don't know what the value is before observeEvent is called (can be called in component
#' constructor with self$ns as name the you can ten get call self$ns on component to create
#' actual biding after component is created)
#'
#' input$new("foo") ## this will create active binding with default setter/getter
#'
#' input$new(component$ns("save"))
#'
#' reactive value with that may have different logic here same code as default
#'
#' input$new("foo", function(value) {
#'    if (missing(value)) {
#'      self[["__foo"]]
#'    } else {
#'      self[["__foo"]] <- value
#'    }
#' })
#'
#' if you set the value
#'
#' input$foo <- 10
#'
#' observer expression will be avaluated also input$.listeners will have data for each listener
#' that will have data about each call to each listener with old and new values
#'
#' usually there will be single listener for single active value
#' TODO: remove old listener after same expression is called again
#'
#' Output and connection between input and output can be explained using this example code
#'
#' ##
#' input <- activeInput(foo = NULL)
#'
#' input$foo <- 100
#'
#' output <- activeOutput(bar = NULL)
#'
#' output$bar <- renderUI({ input$foo + 10 })
#'
#' print(output$bar) ## 110
#' input$foo <- 200
#' print(input$foo) ## 200
#' print(output$bar) ## 210
#'

#' Function create mock for shiny input
#'
#' @param args - initial active names
#' @export
activeInput <- function(env = new.env(), ...) {
  input <- list(...)
  init.binding <- function() {
    for (name in names(input)) {
      if (name == "self") {
        stop("You can't use self as name")
      }
      env$new(name, fn = input[[name]])
    }
  }
  if (is.active.input(env)) {
    init.binding()
    return(env)
  }
  env$.active.symbols = list()
  env$.calls <- list()
  env$.listeners <- list()

  env$on <- function(event.name,
                     fn,
                     expr = NULL,
                     ignoreNULL = TRUE,
                     debounceMillis = NULL,
                     observerName = NULL,
                     ...) {
    if (is.null(env$.listeners[[event.name]])) {
      env$.listeners[[event.name]] <- list()
    }
    if (is.null(env$calls[[event.name]])) {
      env$.calls[[event.name]] <- list()
    }

    if (!is.null(debounceMillis)) {
      fn <- shiny::debounce(fn, debounceMillis)
    }

    env$.listeners[[event.name]] <- c(
      env$.listeners[[event.name]],
      list(
        list(
          expr = expr,
          ignoreNULL = ignoreNULL,
          calls = list(),
          observerName = observerName,
          fn = fn
        )
      )
    )
  }
  env$off <- function(event.name, expr, observerName = NULL) {
    if (is.null(expr)) {
      env$.listeners[[event.name]] <- list()
    } else {
      for (i in seq_along(env$.listeners[[event.name]])) {
        listener <- env$.listeners[[event.name]][[i]]
        if (listener$expr == expr || identical(observerName, listener$observerName)) {
          env$.listeners[[event.name]][i] <- NULL
        }
      }
    }
  }
  ## read only prop to test if this env is activeInput
  makeActiveBinding(
    sym = "__reactive__",
    fun = function(value) {
      if (missing(value)) {
        TRUE
      }
    },
    env = env
  )

  make.default.fn <- function(name) {
    function(value) {
      if (missing(value)) {
        env[[paste0("__", name)]]
      } else {
        env[[paste0("__", name)]] <- value
      }
    }
  }
  env$new <- function(event.name, fn = NULL) {
    if (is.null(fn)) {
      fn <- make.default.fn(event.name)
    } else {
      environment(fn) <- env
    }
    env$.active.symbols = append(env$.active.symbols, list(event.name))
    makeActiveBinding(
      sym = event.name,
      fun = function(value) {
        if (missing(value)) {
          fn(value)
        } else {
          old <- env[[event.name]]
          ret <- fn(value)
          if (!is.null(env$.listeners[[event.name]])) {
            ## invoke listeners added by on and add args to list of args to check later
            for (i in seq_along(env$.listeners[[event.name]])) {
              listener <- env$.listeners[[event.name]][[i]]
              if (!identical(old, value) &&
                  (listener$ignoreNULL && is.null(value) || !is.null(value))) {
                listener$fn(old, value)
                args <- list(old = old, new = value)
                env$.calls[[event.name]] <- append(env$.calls[[event.name]], list(args))
                ## case when observeEvent remove event (once option)
                if (!is.null(env$.listeners[[event.name]][i][[1]])) {
                  env$.listeners[[event.name]][[i]]$calls <- append(listener$calls, list(args))
                }
              }
            }
          }
          ret
        }
      },
      env = env
    )
  }
  env$self <- env
  init.binding()
  env
}

#' name used only inside renderUI in substitute phase
#' @export
isolate <- function(x) x

#' Function for checking if object is actie input - used by extractActiveInputs
is.active.input <- function(obj) {
  if (is.environment(obj)) {
    ## test read only prop to be sure
    if (!is.null(obj[['__reactive__']])) {
      obj[['__reactive__']] <- FALSE
      if (obj[['__reactive__']]) {
        return(TRUE)
      }
    }
  }
  FALSE
}

is.active.binding <- function(name, env) {
  is.active.input(env) && name %in% env$.active.symbols
}

#' Function used the same as battery::observeEvent (based on shiny::observeEvent)
#' that use active binding input mocks - the work almost the same as shiny::observeEvent but it
#' destroy previous created observer, so there are no duplicates
#'
#' @export
observeEventMock <- function(eventExpr,
                             handlerExpr,
                             handler.env = parent.frame(),
                             ignoreInit = FALSE,
                             ignoreNULL = TRUE,
                             observerName = NULL,
                             once = FALSE,
                             ...) {
  sub <- substitute(eventExpr)
  expr <- substitute(handlerExpr)
  ## check if this is self$name - we don't check every corner case
  ## we will only use self$events (in components) and maybe
  ## just in case input$foo or input[[name]] with this mock
  activeEnv <- if (length(sub[[2]]) > 1) {
    prop <- sub[[2]]
    if (prop[[1]] == '$') {
      obj <- get(deparse(prop[[2]]), handler.env)
      obj[[deparse(prop[[3]])]]
    }
  } else {
    get(deparse(sub[[2]]), handler.env)
  }
  name <- if (sub[[1]] == '$') {
    as.character(sub[[3]])
  } else if (sub[[1]] == '[[') {
    if (class(sub[[3]]) == 'name') {
      get(deparse(sub[[3]]), handler.env)
    } else {
      sub[[3]]
    }
  }
  initValue <- activeEnv[[name]]
  if (!ignoreInit && !(ignoreNULL && is.null(initValue))) {
    eval(expr, env = handler.env)
  }

  if (is.active.input(activeEnv)) {
    activeEnv$off(name, expr, observerName = observerName)
    activeEnv$on(name, function(old, value) {
      eval(expr, env = handler.env)
      if (once) {
        activeEnv$off(name, expr, observerName = observerName)
      }
    }, observerName = observerName, expr = expr, ...)
  }
}

#' Function create active binding output mock to be used with renderUI mock
#'
#' @param args - named initial active inputs
#' @export
activeOutput <- function(...) {
  input <- list(...)
  env <- new.env()
  env$.listeners <- list()
  make.default.fn <- function(name) {
    privateName <- paste0("__", name)
    function(value) {
      if (missing(value)) {
        env[[privateName]]
      } else {
        data <- extractActiveNames(value)
        ## for Each uiOutput we add reactive value
        ## and new function to environement that will return value of that variable
        lapply(data$output, function(data) {
          env$new(data$name)
          data$env$uiOutput <- make.uiOutput(env)
        })
        ## we use lapply to create closure
        lapply(data$input, function(var) {
          ## remove previous listener for given expression (when called twice)
          var$active$off(var$prop, value$expr)
          ## evaluate renderUI expression when extracted active input value changes
          ## this is listener the input need to be created with input$new
          var$active$on(var$prop, function(oldInputVal, newInputVal) {
            ## store renderUI expression output in private variable (exposed)
            ## so you can get it using output[[name]]
            env[[privateName]] <- eval(value$expr, env = value$env)
          }, expr = value$expr)
        })
        ## initial value
        env[[privateName]] <- eval(value$expr, env = value$env)
      }
    }
  }
  env$new <- function(name) {
    init.value <- NULL
    if (!is.null(env[[name]]) && is.environment(env[[name]]$env)) {
      init.value <- env[[name]]
      env[[name]] <- NULL
    }
    fn <- make.default.fn(name)
    makeActiveBinding(
      sym = name,
      fun = fn,
      env = env
    )
    if (!is.null(init.value)) {
      fn(init.value)
    }
  }
  for (name in names(input)) {
    env$new(name)
  }
  env
}

#' RenderUI just send exression to output active prop, the prop need to be added first
#' if renderUI is called in constructor and it use self$ns() you can pass component.id to
#' constructor so instance will have same id and you can generate the name before constructor
#' is called - using: output$new(name)
#' the output will parse the expression and bind input and output
#' note: input can have different name for instance events in components
#'
#' @param expr - any expression
#' @export
renderUI <- function(expr) {
  list(
    expr = substitute(expr),
    env =  parent.frame()
  )
}

merge.props <- function(desc, src) {
  lapply(names(src), function(name) {
    desc[[name]] <<- append(desc[[name]], src[[name]])
  })
  desc
}

safe.get <- function(name, env) {
  if (exists(name, env)) {
    get(name, env)
  }
}

#' Traverse substitute expressions and function invocation and extract all references to active elements
#' created by activeInput and uiOutput
#'
#' @param data - named list: expr - substitute expr, env - parent env (value returned from renderUI)
extractActiveNames <- function(data) {
  s <- data$expr
  closure <- NULL
  if (is.environment(data$env)) {
    env <- data$env
  } else {
    closure <- data$env$closure
    env <- data$env$env
  }
  ## traverse
  result <- list(
    input = list(),
    output = list()
  )
  isolate <- FALSE
  for (i in seq_along(s)) {
    item <- s[[i]]
    ## detect isolate() or self$isolate used in components active values should be ignored
    if ((is.symbol(item) && item == 'isolate') ||
        (length(item) > 1 && item[[1]] == '$' && item[[3]] == 'isolate')) {
      isolate <- TRUE
      next
    }
    if (length(item) > 1 && item[[1]] == 'uiOutput') {
      ## invoke self$ns that is insisde uiOutput to get the string
      args <- list(
        name = eval(item[[2]], env = env),
        env = `if`(is.null(closure), env, closure)
      )
      result$output <- append(result$output, list(args))
      next
    }
    ## include function - needed for components - where there is `fn <- fn.expr`
    ## it's inside `r6.class.add` function where `fn.expr` is inline function
    ## injected by substitute - but it's not function but closure
    if (typeof(item) == 'closure') {
      ## with include function you need to evaluate it to get the body but not with closure
      body.fn <- body(item)
      ## here we need two environments - we can't merge them because we need to
      ## modify one of them in activeOutput setter - this case is for render() method
      ## that have uiOutput inside - which is quite common
      ## one env have self (env) and function env is closure that have values from where it
      ## was defined - it can call local function defined outside of component
      fn.env <- list(
        closure = environment(item),
        env = env
      )
      ## body(fn) give same result as substitute so we can use recursion here
      result <- merge.props(result, extractActiveNames(list(expr = body.fn, env = fn.env)))
      next
    }
    if (length(item) > 1 && !isolate) {
      result <- merge.props(result, extractActiveNames(list(expr = item, env = data$env)))
    } else if (typeof(item) == "language") {
      ## code like `foo() + bar()` have type of language
      ## here we detect case of foo() x$foo() or x[[name]]()
      ## We do this to find functions and methods calls and extract names
      ## from inside of the functions body - body return same data
      ## as substitute so we can use recursion here
      fn <- NULL
      if (is.symbol(item[[1]])) {
        fn <- env[[deparse(item[[1]])]]
      } else if (length(item[[1]]) > 1) {
        expr <- item[[1]]
        obj <- env[[deparse(expr[[2]])]]
        prop <- if (expr[[1]] == '$') {
          deparse(expr[[3]])
        } else if (expr[[1]] == '[[') {
          if (is.name(expr[[3]])) {
            varName <- deparse(expr[[3]])
            env[[varName]] ## get [[ name ]] from env
          } else if (is.character(expr[[3]])) {
            expr[[3]]
          }
        }
        if (!is.null(prop)) {
          fn <- obj[[prop]]
        }
      }
      if (!is.null(fn)) {
        ## detect active names from inside functions
        body.fn <- body(fn)
        env.fn <- environment(fn)
        result <- merge.props(result, extractActiveNames(list(expr = body.fn, env = env.fn)))
      }
    } else if (is.name(item)) {
      ## here we find active bindings
      metaData <- NULL
      ## sub-expression foo$name
      if (item == "$") {
        name <- deparse(s[[i + 1]])
        value <- safe.get(name, env) ## get foo from env - it will return NULL and not throw like get
        if (!is.null(value) && is.active.input(value)) {
          metaData <- list(
            name = name,
            active = value,
            prop = s[[i + 2]]
          )
        }
      } else if (item == '[[') {
        arg <- s[[i + 2]]
        ## sub-expression is foo[[ name ]] or foo[[ "name" ]]
        ## first name need to be extracted from environment
        ## second is return as is because it's string
        name <- deparse(s[[i + 1]])
        prop <- if (is.name(arg)) {
          varName <- deparse(arg)
          env[[varName]] ## get [[ name ]] from env
        } else if (is.character(arg)) {
          arg
        }
        if (!is.null(prop)) {
          value <- get(name, env)
          if (!is.null(value) && is.active.input(value)) {
            metaData <- list(
              name = name,
              active = value,
              prop = prop
            )
          }
        }
      }
      if (!is.null(metaData)) {
        result$input <- append(result$input, list(metaData))
      }
    }
  }
  names(result$input) <- sapply(result$input, function(meta) {
    meta$name
  })
  result
}

#' Mock for shiny::makeReactiveBinding to be injected into components
#' limiation it can't be called after value is added to environment (it will work in components)
#'
#' @export
makeReactiveBinding <- function(name, env) {
  activeInput(env = env)
  if (!is.active.binding(name, env)) {
    env$new(name)
  }
  env
}

#' function that return uiOutput function for given enviromnet
#'
#' @param env - enviroment that should be active output
make.uiOutput <- function(env) {
  function(name) {
    ## return same div as shiny
    shiny::tags$div(
      class = "shiny-html-output shiny-bound-output",
      env[[name]]
    )
  }
}
