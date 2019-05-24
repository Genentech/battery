#code taken from https://github.com/rstudio/shiny/blob/master/R/reactives.R and modified
isNullEvent <- function(value) {
  is.null(value) || (inherits(value, 'shinyActionButtonValue') && value == 0)
}

wrapFunctionLabel <- function(func, name, ..stacktraceon = FALSE) {
  if (name == "name" || name == "func" || name == "relabelWrapper") {
    stop("Invalid name for wrapFunctionLabel: ", name)
  }
  assign(name, func, environment())
  registerDebugHook(name, environment(), name)

  relabelWrapper <- eval(substitute(
    function(...) {
      if (!exists("errorReactions")){
        errorReactions <<- list()
      }

      # This `f` gets renamed to the value of `name`. Note that it may not
      # print as the new name, because of source refs stored in the function.
      handleErrors({
        if (..stacktraceon)
          ..stacktraceon..(f(...))
        else
          f(...)
      }, errorReactions)
    },
    list(f = as.name(name))
  ))

  relabelWrapper
}

registerDebugHook <- function(name, where, label) {
  if (exists("registerShinyDebugHook", mode = "function")) {
    registerShinyDebugHook <- get("registerShinyDebugHook", mode = "function")
    params <- new.env(parent = emptyenv())
    params$name <- name
    params$where <- where
    params$label <- label
    registerShinyDebugHook(params)
  }
}

#' @export
observeEvent <- function(eventExpr, handlerExpr,
                         event.env = parent.frame(), event.quoted = FALSE,
                         handler.env = parent.frame(), handler.quoted = FALSE,
                         suspended = FALSE, priority = 0, observerName = NULL,
                         domain = getDefaultReactiveDomain(), autoDestroy = TRUE,
                         ignoreNULL = TRUE, ignoreInit = FALSE, once = FALSE,
                         debounceMillis = NULL, canRunIt = NULL) {

  if (!exists(".observers")){
    .observers <<- new.env()
  }

  eventFunc <- shiny::exprToFunction(eventExpr, event.env, event.quoted)
  label <- sprintf('observeEvent(%s)', paste(deparse(body(eventFunc)), collapse = '\n'))
  eventFunc <- wrapFunctionLabel(eventFunc, "observeEventExpr", ..stacktraceon = TRUE)

  handlerFunc <- exprToFunction(handlerExpr, handler.env, handler.quoted)

  handlerFunc <- wrapFunctionLabel(handlerFunc, "observeEventHandler", ..stacktraceon = TRUE)
  initialized <- FALSE
  if (is.null(observerName)) {
    observerName <- as.character(eval(
      expr = substitute(substitute(eventExpr)),
      envir = parent.frame()
    ))[[3]]
  }

  handlerBody <- substitute(handlerExpr)
  if (observerExists(.observers[[observerName]], handlerBody)) {
    .observers[[observerName]]$observer$destroy()
    rm(list = observerName, envir = .observers)
  }
  if (!is.null(debounceMillis)) {
    eventFunc <- eventFunc %>% shiny::debounce(debounceMillis)
  }

  o <- shiny::observe({
    e <- eventFunc()
    if (ignoreInit && !initialized) {
      initialized <<- TRUE
      return()
    }

    if (ignoreNULL && isNullEvent(e)) {
      return()
    }

    if (once) {
      on.exit(o$destroy())
    }

    if (!is.null(canRunIt) > 0 && initialized) {
      if (!canRunIt) {
        warning(paste("Current user is not allowed to run", observerName))
      }
      shiny::req(canRunIt)
    }

    shiny::isolate(handlerFunc())
  }, label = label, suspended = suspended, priority = priority, domain = domain,
  autoDestroy = TRUE, ..stacktraceon = FALSE)
  .observers[[observerName]] <<- list(observer = invisible(o), handlerBody = handlerBody)

}

observerExists <- function(observer, handlerBody) {
  !is.null(observer) && observer$handlerBody == handlerBody
}
