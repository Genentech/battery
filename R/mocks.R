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
#'      self[['__foo']]
#'    } else {
#'      self[['__foo']] <- value
#'    }
#' })
#'
#' NULL will create default setter/getter and inital active binding
#'
#' input <- activeInput(foo = NULL)
#'
#' observeEvent(input$foo, {
#'    print(paste0('value set to ', input$foo))
#' })
#'
#' observeEvent just call input$on and it will create listener for reactive value
#' it will not create active binding so you need to call new first just in case you
#' don't know what the value is before observeEvent is called (can be called in component
#' constructor with self$ns as name the you can ten get call self$ns on component to create
#' actual biding after component is created)
#'
#' input$new('foo') ## this will create active binding with default setter/getter
#'
#' input$new(component$ns('save'))
#'
#' reactive value with that may have different logic here same code as default
#'
#' input$new('foo', function(value) {
#'    if (missing(value)) {
#'      self[['__foo']]
#'    } else {
#'      self[['__foo']] <- value
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
#' @param env - enviroment that will be turn into activeInput
#' @param ... - list of named inital values
#' @export
activeInput <- function(env = new.env(), ...) {
  input <- list(...)
  init.binding <- function() {
    for (name in names(input)) {
      if (name == 'self') {
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
      env$.listeners[[event.name]] <- Filter(function(listener) {
        !(listener$expr == expr || identical(observerName, listener$observerName))
      }, env$.listeners[[event.name]])
    }
  }
  ## read only prop to test if this env is activeInput
  makeActiveBinding(
    sym = '__reactive_input__',
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
        env[[paste0('__', name)]]
      } else {
        env[[paste0('__', name)]] <- value
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
          args <- list(old = old, new = value)
          env$.calls[[event.name]] <- append(env$.calls[[event.name]], list(args))
          if (!is.null(env$.listeners[[event.name]])) {
            ## invoke listeners added by on and add args to list of args to check later
            for (i in seq_along(env$.listeners[[event.name]])) {
              listener <- env$.listeners[[event.name]][[i]]
              if (!identical(old, value) &&
                  (listener$ignoreNULL && is.null(value) || !is.null(value))) {
                listener$fn(old, value)
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

# -----------------------------------------------------------------------------
#' name used only inside renderUI in substitute phase
#' @param x - reactive value
#' @export
isolate <- function(x) x

# -----------------------------------------------------------------------------
#' Function for checking if object is actie input - used by extractActiveInputs
#' @param obj - any objecct
is.active.input <- function(obj) {
  if (is.environment(obj)) {
    ## test read only prop to be sure
    if (!is.null(obj[['__reactive_input__']])) {
      obj[['__reactive_input__']] <- FALSE
      if (obj[['__reactive_input__']]) {
        return(TRUE)
      }
    }
  }
  FALSE
}

# -----------------------------------------------------------------------------
#' Function check if obj is enviroment that is result of activeOutput function
#' @param obj - any object
is.active.output <- function(obj) {
  if (is.environment(obj)) {
    ## test read only prop to be sure
    if (!is.null(obj[['__reactive_output__']])) {
      obj[['__reactive_output__']] <- FALSE
      if (obj[['__reactive_output__']]) {
        return(TRUE)
      }
    }
  }
  FALSE
}

# -----------------------------------------------------------------------------
#' Function check if name is active biding inside environment activeInput or Output
#' @param name - string
#' @param env - active environment to test
is.active.binding <- function(name, env) {
  (is.active.input(env) || is.active.output(env)) && name %in% env$.active.symbols
}

# -----------------------------------------------------------------------------
#' Function used the same as battery::observeEvent (based on shiny::observeEvent)
#' that use active binding input mocks - the work almost the same as shiny::observeEvent but it
#' destroy previous created observer, so there are no duplicates
#' @param eventExpr - same as in shiny::observeEvent
#' @param handlerExpr - same as in shiny::observeEvent
#' @param handler.env - same as in shiny::observeEvent
#' @param ignoreInit - same as in shiny::observeEvent
#' @param ignoreNULL - same as in shiny::observeEvent
#' @param observerName - name that will distinguish observers with same code
#' @param once - same as in shiny::observeEvent
#' @param ... - reset the params from shiny::observeEvent
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
  expr <- substitute(handlerExpr)
  sub <- substitute(eventExpr)
  if (deparse(sub) == "NULL") {
    if (!ignoreInit && !ignoreNULL) {
      eval(expr, envir = handler.env)
    }
    return(list(
      observer = list(
        destroy = function() NULL
      )
    ))
  }
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
    eval(expr, envir = handler.env)
  }
  if (is.active.input(activeEnv)) {
    activeEnv$off(name, expr, observerName = observerName)
    activeEnv$on(name, function(old, value) {
      eval(expr, envir = handler.env)
      if (once) {
        activeEnv$off(name, expr, observerName = observerName)
      }
    }, observerName = observerName, expr = expr, ...)
  }
  list(
    observer = list(
      destroy = function() {
        activeEnv$off(name, expr, observerName = observerName)
      }
    )
  )
}

# -----------------------------------------------------------------------------
#' Function create active binding output mock to be used with renderUI mock
#'
#' @param ... - named initial active inputs
#'
#' @export
activeOutput <- function(...) {
  input <- list(...)
  env <- new.env()
  env$.listeners <- list()
  makeActiveBinding(
    sym = '__reactive_output__',
    fun = function(value) {
      if (missing(value)) {
        TRUE
      }
    },
    env = env
  )
  env$.active.symbols = list()
  make.default.fn <- function(name) {
    privateName <- paste0('__', name)
    function(value) {
      if (missing(value)) {
        env[[privateName]]
      } else {
        active.names <- extractActiveNames(value)
        ## for Each uiOutput we add reactive value
        ## and new function to environement that will return value of that variable
        lapply(active.names$output, function(name) {
          name$env$uiOutput <- make.uiOutput(env)
        })
        ## we use lapply to create closure
        lapply(active.names$input, function(var) {
          ## remove previous listener for given expression (when called twice)
          var$active$off(var$prop, value$expr)
          ## evaluate renderUI expression when extracted active input value changes
          ## this is listener the input need to be created with input$new
          var$active$on(var$prop, function(oldInputVal, newInputVal) {
            ## store renderUI expression output in private variable (exposed)
            ## so you can get it using output[[name]]
            env[[privateName]] <- eval(value$expr, envir = value$env)
          }, expr = value$expr)
        })
        ## initial value
        env[[privateName]] <- eval(value$expr, envir = value$env)
      }
    }
  }
  env$new <- function(name) {
    init.value <- NULL
    if (name %in% env$.active.symbols) {
      return(NULL)
    }
    env$.active.symbols <- append(env$.active.symbols, list(name))
    if (name %in% ls(env) && is.environment(env[[name]]$env)) {
      init.value <- env[[name]]
      remove(list = name, envir = env)
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

# -----------------------------------------------------------------------------
#' RenderUI just send exression to output active prop, the prop need to be added first
#' if renderUI is called in constructor and it use self$ns() you can pass component.id to
#' constructor so instance will have same id and you can generate the name before constructor
#' is called - using: output$new(name)
#' the output will parse the expression and bind input and output
#' note: input can have different name for instance events in components
#'
#' @param expr - any expression
#'
#' @export
renderUIMock <- function(expr) {
  list(
    expr = substitute(expr),
    env =  parent.frame()
  )
}

# -----------------------------------------------------------------------------
#' Merge two lists into desc list - destructive
#'
#' @param desc - detination list
#' @param src - source lise
merge.props <- function(desc, src) {
  lapply(names(src), function(name) {
    desc[[name]] <<- append(desc[[name]], src[[name]])
  })
  desc
}

# -----------------------------------------------------------------------------
#' function saftly return name from enviroment(s) and don't throw error
#' if name don't exists
#'
#' @param name - string
#' @param env - environment or list of environments
safe.get <- function(name, env) {
  if (is.environment(env)) {
    if (exists(name, env)) {
      get(name, env)
    }
  } else {
    for (e in env) {
      value <- safe.get(name, e)
      if (!is.null(value)) {
        return(value)
      }
    }
  }
}

# -----------------------------------------------------------------------------
#' function check if expression from environment is function call
#'
#' @param expr - expression from substitute
#' @param env - enviroment in which to check expression
is.function.call <- function(expr, env) {
  if (typeof(expr) == 'language' && is.symbol(expr[[1]]) && expr[[1]] != '{') {
    re <- paste0('^', escapeRegex(expr[[1]]), '\\(')

    if (any(grepl(re, deparse(expr)))) {
      value <- safe.get(deparse(expr[[1]]), env)
      if (typeof(value) == 'closure') {
        ## if value environment is base env it mean it's native function
        ## this prevent infinite recursion - maybe other part of the code
        ## have some bug that it create initite loop but this fixed the issue
        return(!identical(environment(value), environment(environment)))
      }
    }
  }
  FALSE
}

# -----------------------------------------------------------------------------
is.method.call <- function(expr) {
  typeof(expr) == 'language' && typeof(expr[[1]]) == 'language' &&
  expr[[1]][[1]] == '$' &&
    grepl(paste0('^', escapeRegex(deparse(expr[[1]])), '\\('), deparse(expr))
}

# -----------------------------------------------------------------------------
#' function taken from Hmisc package source code (no need to whole package)
#' @param string - string that will be escaped
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}

# -----------------------------------------------------------------------------
#' Function return true if function is internal - used to prevent infinite recursion
#'
#' @param fn.body - body of the function
is.internal.function <- function(fn.body) {
  length(fn.body) > 1 && fn.body[[1]] == '.Internal'
}

# -----------------------------------------------------------------------------
#' Function is parsing code like `foo() + bar()` that have type of language
#' and it also detect cases of `foo()`, `x$foo()` or `x[[name]]()`
#' We do this to find functions and methods calls and extract names
#' from inside of the functions bodies - body return same data
#' as substitute so we can call extractActiveNames on body
#' @param item - deparse expression
#' @param env - enviroment
parse.function <- function(item, env) {
  result <- list(
    input = list(),
    output = list()
  )
  fn <- NULL
  ## if value is foo() it's firt value is symbol
  ## with case of foo$bar() and foo[[name]] the first element is
  ## structure with 3 elements
  ## eg. foo$bar(input$name) it have structure like this:
  ## list(list('$', 'foo', 'bar'), list('$', 'input', 'name'))
  if (is.symbol(item[[1]])) {
    fn <- safe.get(deparse(item[[1]]), env)
  } else if (length(item[[1]]) > 1) {
    expr <- item[[1]]
    obj <- safe.get(deparse(expr[[2]]), env)
    prop <- if (expr[[1]] == '$') {
      deparse(expr[[3]])
    } else if (expr[[1]] == '[[') {
      if (is.name(expr[[3]])) {
        varName <- deparse(expr[[3]])
        safe.get(varName, env) ## get [[ name ]] from env
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
    if (!is.null(body.fn) && !is.internal.function(body.fn) && length(body.fn) > 1) {
      if (is.environment(env)) {
        env.fn <- merge.env(environment(fn), env)
      } else {
        env.fn <- list(closure = env$closure, merge.env(environment(fn), env$env))
      }
      result <- extractActiveNames(list(expr = body.fn, env = env.fn))
    }
  }
  result
}

#' Helper function that create single environment from environments in arguments
#'
#' @param ... - any number of  environments objects
merge.env <- function(...) {
  args <- list(...)
  if (length(args) > 1) {
    as.environment(do.call('c', lapply(args, as.list)))
  }
}

#' Helper function used for debugging it convert nested expression structure tree to list
#'
#' @param expr - substitute expression
recur.list <- function(expr) {
  if (length(expr) == 1) {
    deparse(expr)
  } else {
    lapply(as.list(expr), function(e) {
      recur.list(e)
    })
  }
}

#' Higher order function for creating functions for checking if item is foo$bar or foo[[bar]]
#'
#' @param chr - character to match as first item in expression
is.variable <- function(chr) {
  function(item) {
    typeof(item) == 'language' && length(item) == 3 && item[[1]] == chr
  }
}

#' Function check if expression is foo$bar
#'
#' @param item - substitute expression
is.dolar.variable <- is.variable('$')

#' Function check if argumnet is expression foo[[bar]]
#'
#' @param item - substitute expression
is.dbbracket.variable <- is.variable('[[')


#' Recursive function that get nested object and prop name for give object access exression
#'
#' @param item - substitute expression
#' @param env - environment for this expression
get.object <- function(item, env) {
  ## substitute( self$x$input[['foo']]$foo ) have this structure:
  ## ['$',['[[',['$',['$','self','x'],'input'],'"foo"'],'foo']
  ## the function parses this nested expression structure
  if (typeof(item) == 'language' && length(item) == 3 &&
      deparse(item[[1]]) %in% c('[[', '$')) {
    obj <- if (is.name(item[[2]])) {
      name <- deparse(item[[2]])
      safe.get(name, env)
    } else if (is.dolar.variable(item[[2]]) || is.dbbracket.variable(item[[2]])) {
      x <- get.object(item[[2]], env)
      x$obj[[x$prop]]
    }
    if (!is.null(obj)) {
      if (item[[1]] == '$') {
        list(
          obj = obj,
          prop = deparse(item[[3]])
        )
      } else if (item[[1]] == '[[') {
        arg <- item[[3]]
        prop <- if (is.name(arg)) {
          varName <- deparse(arg)
          safe.get(varName,  env) ## get [[ name ]] from env
        } else if (is.character(arg)) {
          arg
        }
        if (!is.null(prop)) {
          list(
            obj = obj,
            prop = prop
          )
        }
      }
    }
  }
}

# -----------------------------------------------------------------------------
#' Traverse substitute expressions and function invocation and extract all references to active elements
#' created by activeInput and uiOutput
#'
#' @param arg - named list: expr - substitute expr, env - parent env (value returned from battery::renderUI mock)
extractActiveNames <- function(arg) {
  s <- arg$expr
  closure <- NULL
  if (is.environment(arg$env)) {
    env <- arg$env
  } else {
    closure <- arg$env$closure
    env <- arg$env$env
  }
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
      ## we save environment for wrapper function so we can pach it
      ## it with uiOUtput function in activeOutput
      output.args <- list(
        env = `if`(is.null(closure), env, closure)
      )
      result$output <- append(result$output, list(output.args))
      next
    }
    ## include function - needed for components - where there is `fn <- fn.expr`
    ## it's inside `r6.class.add` function where `fn.expr` is inline function
    ## injected by substitute - but it's not function but closure
    if (typeof(item) == 'closure') {
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
    } else if (is.function.call(item, arg$env) || is.method.call(item)) {
      result <- merge.props(result, parse.function(item, arg$env))
      if (length(item) > 1) {
        result <- merge.props(result, extractActiveNames(list(expr = item, env = arg$env)))
      }
    } else if (length(item) > 1 && !isolate &&
               !(is.dolar.variable(item) || is.dbbracket.variable(item))) {
      result <- merge.props(result, extractActiveNames(list(expr = item, env = arg$env)))
    } else if (typeof(item) == 'language') {
      if (is.dolar.variable(item) || is.dbbracket.variable(item)) {
        ## foo$bar... and foo[[]] ...
        object.data <- get.object(item, arg$env)
        if (is.active.input(object.data$obj)) {
          metaData <- list(
            active = object.data$obj,
            prop = object.data$prop
          )
          result$input <- append(result$input, list(metaData))
        }
      } else {
        result <- merge.props(result, parse.function(item, arg$env))
      }
    }
  }
  names(result$input) <- sapply(result$input, function(meta) {
    meta$name
  })
  result
}

# -----------------------------------------------------------------------------
#' Mock for shiny::makeReactiveBinding to be injected into components
#' limiation it can't be called after value is added to environment (it will work in components)
#'
#' @param name - name of the binding
#' @param env - enviroment that will be used to create a binding
#' @export
makeReactiveBinding <- function(name, env) {
  activeInput(env = env)
  if (!is.active.binding(name, env)) {
    env$new(name)
  }
  env
}

# -----------------------------------------------------------------------------
#' function that return uiOutput function for given enviromnet
#'
#' @param env - enviroment that should be active output
make.uiOutput <- function(env) {
  function(name) {
    if (is.active.output(env)) {
      env$new(name)
    }
    ## return same div as shiny
    shiny::tags$div(
      id = name,
      class = 'shiny-html-output',
      env[[name]]
    )
  }
}
originals <- new.env()

#' Helper function to be used in test files that overwrite shiny functions (after test
#' you should run clearMocks since they are global and if you run shiny application
#' after test that use mocks it will break)
#'
#' @export
useMocks <- function() {
  env <- parent.frame()
  mock('observeEvent', battery::observeEventMock, env)
  mock('isolate', battery::isolate, env)
  mock('makeReactiveBinding', battery::makeReactiveBinding, env)
  mock('renderUI', battery::renderUIMock, env)
}

#' function create single mock for shiny function
mock <- function(name, mock, env) {
  originals[[name]] <- getFromNamespace(name, "shiny")
  utils::assignInNamespace(name, mock, 'shiny')
  env[[name]] <- mock
}

#' function restore original shiny function
clearMock <- function(name, env) {
  utils::assignInNamespace(name, originals[[name]], 'shiny')
  env[[name]] <- originals[[name]]
}


#' Helper function to be used at the end of test files (useful if same session is used
#' to run test and application e.g. RStudio)
#'
#' @export
clearMocks <- function() {
  env <- parent.frame()
  for (name in c('observeEvent', 'isolate', 'makeReactiveBinding', 'renderUI')) {
    clearMock(name, env)
  }
}


#' Function overwrite value in parent frame enviroment
#' It start searching for variable starting from frame parameter
#' @param value - value that will be added to enviroment
#' @param name - name of the variable if not set it will use variable name from value
#' @param frame - integer that indicate from which parent it should start searching for variable
set.frame <- function(value, name = NULL, frame = 1) {
  if (is.null(name)) {
    s <- substitute(value)
    if (class(s) == "name") {
      name <- as.character(s)
    } else {
      stop("set.frame: variable or name required")
    }
  }
  top <- globalenv()
  repeat {
    env <- parent.frame(n = frame)
    global <- identical(top, env)
    if (name %in% names(env) || global) {
      env[[name]] <- value
    }
    frame <- frame + 1
    if (global) {
      break;
    }
  }
}

#' Mock for Session object, only some tests require this right now
#' but more tests may require this session object in the future
#' Right now only services need session$destroy() method to clear
#' services so you can create same service in more then one test
#' @export
Session <- R6::R6Class(
  classname = 'Session',
  private = list(
    .destroy = list()
  ),
  public = list(
    token = NULL,
    initialize = function(token = NULL) {
      self$token <- token
    },
    destroy = function() {
      invisible(lapply(private$.destroy, do.call, args = list()))
    },
    onSessionEnded = function(fn) {
      private$.destroy <- append(private$.destroy, list(fn))
    }
  )
)
