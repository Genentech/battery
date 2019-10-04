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

#' Event handler (modified shiny code to be used as replacement for shiny::observeEvent)
#'
#' Respond to "event-like" reactive inputs, values, and expressions.
#'
#' Shiny's reactive programming framework is primarily designed for calculated
#' values (reactive expressions) and side-effect-causing actions (observers)
#' that respond to \emph{any} of their inputs changing. That's often what is
#' desired in Shiny apps, but not always: sometimes you want to wait for a
#' specific action to be taken from the user, like clicking an
#' \code{\link{actionButton}}, before calculating an expression or taking an
#' action. A reactive value or expression that is used to trigger other
#' calculations in this way is called an \emph{event}.
#'
#' These situations demand a more imperative, "event handling" style of
#' programming that is possible--but not particularly intuitive--using the
#' reactive programming primitives \code{\link{observe}} and
#' \code{\link{isolate}}. \code{observeEvent} and \code{eventReactive} provide
#' straightforward APIs for event handling that wrap \code{observe} and
#' \code{isolate}.
#'
#' Use \code{observeEvent} whenever you want to \emph{perform an action} in
#' response to an event. (Note that "recalculate a value" does not generally
#' count as performing an action--see \code{eventReactive} for that.) The first
#' argument is the event you want to respond to, and the second argument is a
#' function that should be called whenever the event occurs.
#'
#' Use \code{eventReactive} to create a \emph{calculated value} that only
#' updates in response to an event. This is just like a normal
#' \link[=reactive]{reactive expression} except it ignores all the usual
#' invalidations that come from its reactive dependencies; it only invalidates
#' in response to the given event.
#'
#' @section ignoreNULL and ignoreInit:
#'
#' Both \code{observeEvent} and \code{eventReactive} take an \code{ignoreNULL}
#' parameter that affects behavior when the \code{eventExpr} evaluates to
#' \code{NULL} (or in the special case of an \code{\link{actionButton}},
#' \code{0}). In these cases, if \code{ignoreNULL} is \code{TRUE}, then an
#' \code{observeEvent} will not execute and an \code{eventReactive} will raise a
#' silent \link[=validate]{validation} error. This is useful behavior if you
#' don't want to do the action or calculation when your app first starts, but
#' wait for the user to initiate the action first (like a "Submit" button);
#' whereas \code{ignoreNULL=FALSE} is desirable if you want to initially perform
#' the action/calculation and just let the user re-initiate it (like a
#' "Recalculate" button).
#'
#' Likewise, both \code{observeEvent} and \code{eventReactive} also take in an
#' \code{ignoreInit} argument. By default, both of these will run right when they
#' are created (except if, at that moment, \code{eventExpr} evaluates to \code{NULL}
#' and \code{ignoreNULL} is \code{TRUE}). But when responding to a click of an action
#' button, it may often be useful to set \code{ignoreInit} to \code{TRUE}. For
#' example, if you're setting up an \code{observeEvent} for a dynamically created
#' button, then \code{ignoreInit = TRUE} will guarantee that the action (in
#' \code{handlerExpr}) will only be triggered when the button is actually clicked,
#' instead of also being triggered when it is created/initialized. Similarly,
#' if you're setting up an \code{eventReactive} that responds to a dynamically
#' created button used to refresh some data (then returned by that \code{eventReactive}),
#' then you should use \code{eventReactive([...], ignoreInit = TRUE)} if you want
#' to let the user decide if/when they want to refresh the data (since, depending
#' on the app, this may be a computationally expensive operation).
#'
#' Even though \code{ignoreNULL} and \code{ignoreInit} can be used for similar
#' purposes they are independent from one another. Here's the result of combining
#' these:
#'
#' \describe{
#'   \item{\code{ignoreNULL = TRUE} and \code{ignoreInit = FALSE}}{
#'      This is the default. This combination means that \code{handlerExpr}/
#'      \code{valueExpr} will run every time that \code{eventExpr} is not
#'      \code{NULL}. If, at the time of the creation of the
#'      \code{observeEvent}/\code{eventReactive}, \code{eventExpr} happens
#'      to \emph{not} be \code{NULL}, then the code runs.
#'   }
#'   \item{\code{ignoreNULL = FALSE} and \code{ignoreInit = FALSE}}{
#'      This combination means that \code{handlerExpr}/\code{valueExpr} will
#'      run every time no matter what.
#'   }
#'   \item{\code{ignoreNULL = FALSE} and \code{ignoreInit = TRUE}}{
#'      This combination means that \code{handlerExpr}/\code{valueExpr} will
#'      \emph{not} run when the \code{observeEvent}/\code{eventReactive} is
#'      created (because \code{ignoreInit = TRUE}), but it will run every
#'      other time.
#'   }
#'   \item{\code{ignoreNULL = TRUE} and \code{ignoreInit = TRUE}}{
#'      This combination means that \code{handlerExpr}/\code{valueExpr} will
#'      \emph{not} run when the \code{observeEvent}/\code{eventReactive} is
#'      created (because  \code{ignoreInit = TRUE}). After that,
#'      \code{handlerExpr}/\code{valueExpr} will run every time that
#'      \code{eventExpr} is not \code{NULL}.
#'   }
#' }
#'
#' @param eventExpr A (quoted or unquoted) expression that represents the event;
#'   this can be a simple reactive value like \code{input$click}, a call to a
#'   reactive expression like \code{dataset()}, or even a complex expression
#'   inside curly braces
#' @param handlerExpr The expression to call whenever \code{eventExpr} is
#'   invalidated. This should be a side-effect-producing action (the return
#'   value will be ignored). It will be executed within an \code{\link{isolate}}
#'   scope.
#' @param valueExpr The expression that produces the return value of the
#'   \code{eventReactive}. It will be executed within an \code{\link{isolate}}
#'   scope.
#' @param event.env The parent environment for \code{eventExpr}. By default,
#'   this is the calling environment.
#' @param event.quoted Is the \code{eventExpr} expression quoted? By default,
#'   this is \code{FALSE}. This is useful when you want to use an expression
#'   that is stored in a variable; to do so, it must be quoted with
#'   \code{quote()}.
#' @param handler.env The parent environment for \code{handlerExpr}. By default,
#'   this is the calling environment.
#' @param handler.quoted Is the \code{handlerExpr} expression quoted? By
#'   default, this is \code{FALSE}. This is useful when you want to use an
#'   expression that is stored in a variable; to do so, it must be quoted with
#'   \code{quote()}.
#' @param value.env The parent environment for \code{valueExpr}. By default,
#'   this is the calling environment.
#' @param value.quoted Is the \code{valueExpr} expression quoted? By default,
#'   this is \code{FALSE}. This is useful when you want to use an expression
#'   that is stored in a variable; to do so, it must be quoted with \code{quote()}.
#' @param label A label for the observer or reactive, useful for debugging.
#' @param suspended If \code{TRUE}, start the observer in a suspended state. If
#'   \code{FALSE} (the default), start in a non-suspended state.
#' @param priority An integer or numeric that controls the priority with which
#'   this observer should be executed. An observer with a given priority level
#'   will always execute sooner than all observers with a lower priority level.
#'   Positive, negative, and zero values are allowed.
#' @param domain See \link{domains}.
#' @param autoDestroy If \code{TRUE} (the default), the observer will be
#'   automatically destroyed when its domain (if any) ends.
#' @param ignoreNULL Whether the action should be triggered (or value
#'   calculated, in the case of \code{eventReactive}) when the input is
#'   \code{NULL}. See Details.
#' @param ignoreInit If \code{TRUE}, then, when this \code{observeEvent} is
#'   first created/initialized, ignore the \code{handlerExpr} (the second
#'   argument), whether it is otherwise supposed to run or not. The default is
#'   \code{FALSE}. See Details.
#' @param once Whether this \code{observeEvent} should be immediately destroyed
#'   after the first time that the code in \code{handlerExpr} is run. This
#'   pattern is useful when you want to subscribe to a event that should only
#'   happen once.
#' @param debounceMillis - automatic debouce of event with this option
#' @param canRunIt - option to turn off event handler code without the need
#'   to wrap the whole expression in if
#'
#' @return \code{observeEvent} returns an observer reference class object (see
#'   \code{\link{observe}}). \code{eventReactive} returns a reactive expression
#'   object (see \code{\link{reactive}}).
#'
#' @seealso \code{\link{actionButton}}
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'
#'   ## App 1: Sample usage
#'   shinyApp(
#'     ui = fluidPage(
#'       column(4,
#'         numericInput("x", "Value", 5),
#'         br(),
#'         actionButton("button", "Show")
#'       ),
#'       column(8, tableOutput("table"))
#'     ),
#'     server = function(input, output) {
#'       # Take an action every time button is pressed;
#'       # here, we just print a message to the console
#'       observeEvent(input$button, {
#'         cat("Showing", input$x, "rows\n")
#'       })
#'       # Take a reactive dependency on input$button, but
#'       # not on any of the stuff inside the function
#'       df <- eventReactive(input$button, {
#'         head(cars, input$x)
#'       })
#'       output$table <- renderTable({
#'         df()
#'       })
#'     }
#'   )
#'
#'   ## App 2: Using `once`
#'   shinyApp(
#'     ui = basicPage( actionButton("go", "Go")),
#'     server = function(input, output, session) {
#'       observeEvent(input$go, {
#'         print(paste("This will only be printed once; all",
#'               "subsequent button clicks won't do anything"))
#'       }, once = TRUE)
#'     }
#'   )
#'
#'   ## App 3: Using `ignoreInit` and `once`
#'   shinyApp(
#'     ui = basicPage(actionButton("go", "Go")),
#'     server = function(input, output, session) {
#'       observeEvent(input$go, {
#'         insertUI("#go", "afterEnd",
#'                  actionButton("dynamic", "click to remove"))
#'
#'         # set up an observer that depends on the dynamic
#'         # input, so that it doesn't run when the input is
#'         # created, and only runs once after that (since
#'         # the side effect is remove the input from the DOM)
#'         observeEvent(input$dynamic, {
#'           removeUI("#dynamic")
#'         }, ignoreInit = TRUE, once = TRUE)
#'       })
#'     }
#'   )
#' }
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

  handlerFunc <- shiny::exprToFunction(handlerExpr, handler.env, handler.quoted)

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

originalObserveEvent <- observeEvent
