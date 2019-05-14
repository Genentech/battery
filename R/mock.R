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
#' observer expression will be avaluated also input$listeners will have data for each listener
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
#' @params args - initial active names
#' @export
activeInput <- function(...) {
  input <- list(...)
  env <- new.env()
  env$calls <- list()
  env$listeners <- list()

  env$on <- function(event.name,
                     fn,
                     expr = NULL,
                     ignoreNULL = TRUE,
                     debounceMillis = NULL,
                     observerName = NULL,
                     ...) {
    if (is.null(env$listeners[[event.name]])) {
      env$listeners[[event.name]] <- list()
    }
    if (is.null(env$calls[[event.name]])) {
      env$calls[[event.name]] <- list()
    }

    if (!is.null(debounceMillis)) {
      fn <- fn %>% shiny::debounce(debounceMillis)
    }

    env$listeners[[event.name]] <- c(
      env$listeners[[event.name]],
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
      env$listeners[[event.name]] <- list()
    } else {
      for (i in seq_along(env$listeners[[event.name]])) {
        listener <- env$listeners[[event.name]][[i]]
        if (listener$expr == expr || identical(observerName, listener$observerName)) {
          env$listeners[[event.name]][i] <- NULL
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
    makeActiveBinding(
      sym = event.name,
      fun = function(value) {
        if (missing(value)) {
          fn(value)
        } else {
          old <- env[[event.name]]
          ret <- fn(value)
          if (!is.null(env$listeners[[event.name]])) {
            ## invoke listeners added by on and add args to list of args to check later
            for (i in seq_along(env$listeners[[event.name]])) {
              listener <- env$listeners[[event.name]][[i]]
              if (!identical(old, value) &&
                  (listener$ignoreNULL && is.null(value) || !is.null(value))) {
                listener$fn(old, value)
                args <- list(old = old, value = value)
                env$calls[[event.name]] <- append(listener$calls, list(args))
                ## case when observeEvent remove event (once option)
                if (!is.null(env$listeners[[event.name]][i][[1]])) {
                  env$listeners[[event.name]][[i]]$calls <- append(listener$calls, list(args))
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
  for (name in names(input)) {
    if (name == "self") {
      stop("You can't use self as name")
    }
    env$new(name, fn = input[[name]])
  }
  env
}

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
  s <- substitute(eventExpr)
  expr <- substitute(handlerExpr)
  activeEnv <- handler.env[[deparse(s[[2]])]]
  name <- if (s[[1]] == '$') {
    as.character(s[[3]])
  } else if (s[[1]] == '[[') {
    if (class(s[[3]]) == 'name') {
      handler.env[[deparse(s[[3]])]]
    } else {
      s[[3]]
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
#' @params args - named initial active inputs
#' @export
activeOutput <- function(...) {
  input <- list(...)
  env <- new.env()
  env$listeners <- list()
  make.default.fn <- function(name) {
    privateName <- paste0("__", name)
    function(value) {
      if (missing(value)) {
        env[[privateName]]
      } else {
        vars <- extractActiveInputs(value)
        ## we use lapply to create closure
        lapply(vars, function(var) {
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
    makeActiveBinding(
      sym = name,
      fun = make.default.fn(name),
      env = env
    )
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
#' @params expr - any expression
#' @export
renderUI <- function(expr) {
  list(
    expr = substitute(expr),
    env =  parent.frame()
  )
}

#' Traverse substitute expression and extract all references to active elements
#' created by activeInput
#'
#' @params s - named list: expr - substitute expr, env - parent env (value returned from renderUI)
extractActiveInputs <- function(data) {
  s <- data$expr
  env <- data$env
  ## traverse
  result <- list()
  for (i in seq_along(s)) {
    if (length(s[[i]]) > 1) {
      ret <- extractActiveInputs(list(expr = s[[i]], env = env))
      if (length(ret) > 0) {
        result <- append(result, ret)
      }
    }
    if (is.name(s[[i]])) {
      metaData <- NULL
      ## sub-expression foo$name
      if (s[[i]] == "$") {
        name <- as.character(s[[i + 1]])
        value <- env[[name]] ## get foo from env
        if (!is.null(value) && is.active.input(value)) {
          metaData <- list(
            name = name,
            active = value,
            prop = s[[i + 2]]
          )
        }
      } else if (s[[i]] == '[[') {
        ## sub-expression is foo[[ name ]] or foo[[ "name" ]]
        ## first name need to be extracted from environment
        ## second is return as is because it's string
        name <- as.character(s[[i + 1]])
        prop <- if (is.name(s[[i + 2]])) {
          varName <- as.character(s[[i + 2]])
          env[[varName]] ## get [[ name ]] from env
        } else if (is.character(s[[i + 2]])) {
          s[[ i + 2]]
        }
        if (!is.null(prop)) {
          value <- env[[name]] ## get input from env
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
        result <- append(result, list(metaData))
      }
    }
  }
  names(result) <- sapply(result, function(meta) {
    meta$name
  })
  result
}
