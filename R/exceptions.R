#' function allows to add global exception handler for all battery components
#' @param handler - list of handlers
#' @param reset - indicate if old handlers should be removed
#' @param session - optional shiny session where to register the exception handler
#' @export
exceptions <- function(handler = NULL, reset = FALSE, session = NULL) {
  if (reset) {
    if (is.null(handler)) {
      reset.exceptions(session)
    } else {
      set.exceptions(handler, session)
    }
  } else if (is.null(handler)) {
    stop("battery::excpetion handler argument need to be list, NULL given")
  } else {
    extend.exceptions(handler, session)
  }
}

#' global handler for errors that print exception if user didn't process it
#' @param error - error structure created by \code{create.error}
#' @param meta - context of the error (origin, type, ...)
#' @param session - optional shiny session used to find the exception handlers
handle.error <- function(error, meta = NULL, session = NULL) {
  ## we need to know if the user registered an error handler before we invoke it,
  ## the return value is not enough - a handler that returns NULL (e.g. one that
  ## ends with a call to message) is still a handler that processed the error
  handled <- !is.null(error$class) &&
    is.function(resolve.exceptions(session)[["error"]])
  ret <- handle.exceptions(error, meta = meta, session = session)
  if (identical(ret, battery::end())) {
    report.error(error, meta)
    battery::error(error$message)
  } else if (!handled) {
    ## nothing processed the error, report it so it's not silently discarded
    report.error(error, meta)
    message(error$message)
  }
}

#' helper function that print the context in which the error was thrown
#' @param error - error structure created by \code{create.error}
#' @param meta - context of the error (origin, type, ...)
report.error <- function(error, meta = NULL) {
  if (!is.null(meta) && !is.null(meta$origin)) {
    message("thrown in ", meta$origin)
  }
}

#' helper function that return exception handlers that apply to given session
#'
#' handlers registered for the session take precedence, if the session has no
#' handlers it falls back to the global ones
#'
#' @param session - optional shiny session
resolve.exceptions <- function(session = NULL) {
  if (is.null(session)) {
    global$exceptions$global
  } else {
    session.exceptions <- global$exceptions$sessions[[ session$token ]]
    if (is.null(session.exceptions)) {
      ## fallback if no session exceptions
      global$exceptions$global
    } else {
      session.exceptions
    }
  }
}

#' function that invoke global exception handler based on cond data
#' @param cond - structure with classes that indicate exception
#' @param meta - context of the exception (origin, type, ...)
#' @param session - optional shiny session used to find the exception handlers
handle.exceptions <- function(cond, meta = NULL, session = NULL) {
  result <- NULL
  if (!is.null(cond$class) && !is.battery.error(cond)) {
    exceptions <- resolve.exceptions(session)
    for (cls in cond$class) {
      if (is.function(exceptions[[ cls ]])) {
        battery::withExceptions({
          ret <- invoke(exceptions[[ cls ]], clean(cond), meta)
          if (is.logical(ret) && is.null(result)) {
            result <- ret
          }
        },
        meta = meta,
        error = function(cond) {
          if (cls == "error") {
            message("[WARN] error in ", cls, " handler")
            if (!is.null(cond$message)) {
              message("       ", cond$message)
            }
            battery::error(cond$message)
          } else {
            err <- create.error(cond, c(list(
              type = "exception",
              name = cls
            ), meta))
            handle.error(err, meta = meta, session = session)
          }
        }, session = session)
      }
    }
  }
  result
}

#' global exception handler that is used in battery instead of tryCatch
#' @param expr - any expression
#' @param error - function that will be triggered on error default NULL
#'                if added it should return add add meta data create.error(cond, list(...))
#'                it is used internally by battery, it can safely ignored.
#' @param finally - function that is always executed when the expression is done,
#'                  no matter if it raised an error, signalled an exception or
#'                  finished normally (it is always invoked exactly once)
#' @param meta - context of the expression (origin, type, ...) passed to the handlers
#' @param session - optional shiny session to create exception handler only for given session
#' @export
withExceptions <- function(expr, error = NULL, finally = NULL, meta = NULL, session = NULL) {
  ## finally needs to run on every exit path, signal handlers are calling
  ## handlers so they resume the expression and must not trigger it early
  if (is.function(finally)) {
    on.exit(finally(), add = TRUE)
  }
  invisible(withCallingHandlers({
    withRestarts(
      expr = expr,
      battery__ignore = function() {
        ## this is left empty on purpose
      }
    )
  },
  error = function(cond) {
    if (is.battery.error(cond)) {
      battery::error(cond$message, bubble = TRUE)
    } else if (!is.silent(cond)) {
      if (is.function(error)) {
        invoke(error, cond)
      } else {
        err <- create.error(cond, c(list(
          type = "exception",
          name = c
        ), meta))
        handle.error(err, meta = meta, session = session)
      }
      invokeRestart("battery__ignore")
    }
  },
  battery__exception = function(cond) {
    ret <- handle.exceptions(cond, meta = meta, session = session)
    if (identical(ret, battery::pause())) {
      invokeRestart("battery__ignore")
    } else if (identical(ret, battery::end())) {
      battery::error()
    } else {
      continue()
    }
  }))
}

#' create structure that can be used to signal error in applications
#' @param cond - input from withCallingHandlers it should be unexpected error in app
#' @param meta - addition extra data that should be added into meta property
#' @export
create.error <- function(cond, meta = NULL) {
  cond <- clean.error(cond)
  cond$class <- "error"
  cond$meta <- meta
  cond
}

#' Helper function that removes battery marker from error message
#' @param cond - condition object
clean.error <- function(cond) {
  if (is.battery.error(cond)) {
    cls <- class(cond)
    class(cond) <- cls[cls != "battery.error"]
  }
  cond
}

#' helper function that check if error was triggered by battery::error function
#' @param x - condition object
is.battery.error <- function(x) inherits(x, "battery.error")

#' helper function that check if error that should be ignored
#' @param x - condition object
is.silent <- function(x) {
  inherits(x, "shiny.silent.error")
}

#' helper function that can be used in exception handler to trigger error handler
#' @param message - optional message that should be character string
#' @param bubble - if set to TRUE the message is marked so it can be tracked
#'        while it propagates through nested handlers
#' @export
error <- function(message = NULL, bubble = FALSE) {
  if (is.null(message)) {
    message <- "__<1>__BUBBLE__"
  } else if (bubble) {
    re <- "__<[0-9]+>__"
    message <- if (grepl(re, message)) {
      num <- as.numeric(regmatches(message, regexpr('[0-9]+', message)))
      gsub(re, paste0("__<", num + 1, ">__"), message)
    } else {
      paste0("__<1>__", message)
    }
  }
  e <- condition(c("battery.error", "error"), message)
  stop(e)
}

#' Helper condition object
#' @param subclass - string vector for the subclass
#' @param message - message string of a given condition
#' @param call - for stack trace
#' @param ... - any extra attributes that should be added to the condition
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call),
    ...
  )
}

#' signal exception in applications
#' @param class - string vector that indicate class of the exception
#' @param message - string that indicate given exception
#' @param data - optional data that should be used as exception
#' @param call - call for stack trace
#' @param ... - eny extra data that should be added to the exception
#' @export
signal <- function(class, message = NULL, data = NULL, call = sys.call(-1), ...) {
  exception <- if (is.list(data)) {
    if (is.null(message)) {
      modifyList(data, list(class = class, call = call))
    } else {
      modifyList(data, list(message = message, class = class, call = call))
    }
  } else {
    modifyList(list(...), list(message = message, class = class, call = call))
  }
  withRestarts({
    signalCondition(structure(
      exception,
      class = c("battery__exception", "condition")
    ))
  }, battery__continue = function() {
    ## this should be empty, it will continue execution the code after the signal
  })
}

#' function restart the evaluation of the code after the signal to stop
#' the propagation of event handlers in nested withHandlers
continue <- function() {
  r <- findRestart("battery__continue")
  if (is.null(r)) return()
  invokeRestart(r)
}

#' helper function that clean message and remove classes from cond object
#' @param cond - condition object
clean <- function(cond) {
  structure(clean.error(cond), class = NULL)
}


#' helper function that capture all signal messages into a vector
#' it can be used in unit tests to check if the function sent proper messages
#' @param expr - any expression
#' @param signal - charcater vector with signals that should be captured
#' @param session - optional shiny session used to find the exception handlers
#' @export
capture_signal_messages <- function(expr, signal, session = NULL) {
  data <- c()

  exceptions <- list()
  for (name in signal) {
    exceptions[[name]] <- function(cond) {
      data <<- c(data, cond$message)
    }
  }

  old.exceptions <- get.exceptions(session)

  battery::exceptions(exceptions, reset = TRUE, session = session)
  battery::withExceptions(expr, session = session)

  set.exceptions(old.exceptions, session)

  data
}

#' helper function that returns exception handler
#' @param session - optional shiny session, if omitted the global handlers are used
get.exceptions <- function(session = NULL) {
  if (is.null(session)) {
    global$exceptions$global
  } else {
    global$exceptions$sessions[[ session$token ]]
  }
}

#' helper function that reset exceptions
#' @param session - optional shiny session, if omitted the global handlers are reset
reset.exceptions <- function(session = NULL) {
  if (is.null(session)) {
    global$exceptions$sessions <- list()
    global$exceptions$global <- list()
  } else {
    global$exceptions$sessions[[ session$token ]] <- list()
  }
}

#' helper function that extend existing exception handler
#' note that if same handler function is used it will be overwritten
#' @param handler - named list of exception handlers
#' @param session - optional shiny session, if omitted the global handlers are extended
extend.exceptions <- function(handler, session = NULL) {
  exceptions <- get.exceptions(session)
  exceptions <- if (is.null(exceptions)) {
    handler
  } else {
    modifyList(exceptions, handler)
  }
  set.exceptions(exceptions, session)
}

#' helper function that set exception uncoditionaly, old handler is discarded
#' @param handler - named list of exception handlers
#' @param session - optional shiny session, if omitted the global handlers are set
set.exceptions <- function(handler, session = NULL) {
  if (is.null(session)) {
    global$exceptions$global <- handler
  } else {
    global$exceptions$sessions[[ session$token ]] <- handler
  }
}

#' helper function that can be used in exception handler to pause just this context
#' @export
pause <- function() {
  structure(TRUE, class = "battery.pause")
}


#' helper function that can be used in exception handler to stop whole application
#' @export
end <- function() {
  structure(FALSE, class = "battery.end")
}
