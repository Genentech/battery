#' Function call function fn with the only arguments it accept
#' @param fn - function to be called
#' @param ... - list of arguments
invoke <- function(fn, ...) {
  if (!is.function(fn)) {
    base::stop("invoke: argument need to be a function")
  }
  count <- length(formals(fn))
  do.call(fn, utils::head(list(...), count))
}

#' helper that print args
#' @param ... - arguments to print
debug <- function(...) {
  print(paste(list(...), collapse = " "))
}

#' Function load all R file components from directory, component classes will be global
#' @param path - path to directory
#' @param pattern - pattern used to selected the files (default all R files)
#' @param ignore - string or vector of strings with fillanme files that should be ignored
#' @param ignore.case - should it ignore case of pattern when searching for files
#' @param recursive - if set to TRUE (default) loading of files will be recursive
#' @param local - TRUE, FALSE or enviroment use in source - if value is TRUE it will use parent.frame()
#' @export
load <- function(path,
                 pattern = "*.R$",
                 ignore = NULL,
                 recursive = TRUE,
                 ignore.case = TRUE,
                 local = TRUE) {
  components <- list.files(
    path = path,
    pattern = pattern,
    recursive = recursive,
    ignore.case = ignore.case,
    full.names = TRUE
  )
  if (isTRUE(local)) {
    local <- parent.frame()
  }
  if (is.null(ignore)) {
    for (path in components) {
      source(path, local = local)
    }
  } else {
    for (path in components) {
      if (all(path != ignore)) {
        source(path, local = local)
      }
    }
  }
}

#' Return correct timestamp as string
now <- function() {
  format(as.numeric(Sys.time())*1000, scientific = FALSE)
}

#' Force of reactive value trigger
#' @param fn - function that can have reactive value asignment that will alway invalidate reactive context
#' @param session - session object - in battery there is only one session object
#'
## TODO: waiting for better solution (reply on SO or GitHub)
## https://github.com/rstudio/shiny/issues/2488
## https://stackoverflow.com/q/60654485/387194
force <- function(fn, session = shiny::getDefaultReactiveDomain()) {
  ## marker/hack for monkey patch of shiny::observeEvent
  ..BATTERY <- TRUE
  shinyHack <- shiny::isolate(session$input$battery_shinyHack)
  shiny::observeEvent(session$input$battery_shinyHack, {
    ..BATTERY <- FALSE
    fn()
  }, once = TRUE)
  session$manageInputs(list(battery_shinyHack = `if`(is.null(shinyHack), TRUE, !shinyHack)))
}

#' add spaces before the string
#' @param n - number of spaces
#' @param string - string that will be added after spaces
indent <- function(n, string) {
  spaces <- strrep(" ", n)
  paste0(spaces, string)
}


#' Function helper taken from \code{shiny::observeEvent} implementation
#' from https://github.com/rstudio/shiny/blob/master/R/reactives.R
#' @param value - value from calling output of \code{shiny::exprToFunction}
isNullEvent <- function(value) {
  is.null(value) || (inherits(value, 'shinyActionButtonValue') && value == 0)
}


#' wrapper over observe that works similar to \code{shiny::observeEvent}
#' @param eventExpr - reactive expression
#' @param handlerExpr - expression that react to reactive expression
#' @param event.env - environment used for reactive expression
#' @param handler.env - environment used for handler expression
#' @param ignoreNULL - if set to TRUE it will not invoke the handler if reactive
#'        value is NULL
#' @param ignoreInit - if set to FALSE it will run handler expression on init
#' @param exitHandler - additional function that will be called if once is set
#' @param once - if set to TRUE it will invoke the handler once nad destroy observer
#' @param debounceMillis - if not NULL it will use the value as time for \code{shiny::debounce}
#' @return result of \code{shiny::obseve}
observeWrapper <- function(eventExpr,
                           handlerExpr,
                           event.env = parent.frame(),
                           handler.env = parent.frame(),
                           ignoreNULL = TRUE,
                           ignoreInit = FALSE,
                           exitHandler = NULL,
                           once = FALSE,
                           debounceMillis = NULL) {

  exprFun <- shiny::exprToFunction(eventExpr, event.env)
  handlerFun <- shiny::exprToFunction(handlerExpr, handler.env)

  ## debounce need to be on function that is reactive
  if (!is.null(debounceMillis)) {
    exprFun <- exprFun %>% shiny::debounce(debounceMillis)
  }

  initialized <- FALSE
  shiny::observe({
    e <- exprFun()
    if (ignoreInit && !initialized) {
      initialized <<- TRUE
      return()
    }

    if (once) {
      on.exit({
        if (is.null(exitHandler)) {
          observer$destroy()
        } else if (is.function(exitHandler)) {
          battery:::invoke(exitHandler, observer)
        }
      })
    }

    if (ignoreNULL && isNullEvent(e)) {
      return()
    }

    shiny::isolate(handlerFun())
  })
}
