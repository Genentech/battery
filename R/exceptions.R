#' function allows to add global exception handler for all battery components
#' @param handler - list of handlers
#' @param rest - indicate if old handlers should be removed
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
handle.error <- function(error, finally = NULL, session = NULL) {
  ret <- handle.exceptions(error, finally = finally, session = session)
  if (identical(ret, battery::end())) {
    if (!(is.null(error) || is.null(error$meta))) {
      meta <- error$meta
      message(paste("thrown in", meta$origin))
    }
    stop(error$message)
  }
}

#' function that invoke global exception handler based on cond data
#' @param cond - structure with classes that indicate exception
handle.exceptions <- function(cond, finally = NULL, session = NULL) {
  result <- NULL
  if (!is.null(cond$class)) {
    exceptions <- if (is.null(session)) {
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
    for (c in cond$class) {
      if (is.function(exceptions[[ c ]])) {
        battery::withExceptions({
          ret <- battery:::invoke(exceptions[[ c ]], battery:::clean(cond))
          if (is.logical(ret) && is.null(result)) {
            result <- ret
          }
        }, error = function(err.cond) {
          ## trigger error user handler
          error.exception <- function() {
            create.error(err.cond, list(
              type = "exception",
              name = c
            ))
          }
          if (is.battery.error(err.cond)) {
            error.exception()
          } else if (length(err.cond) == 0 || length(err.cond$message) == 0) {
            stop()
          } else if (c == "error") {
            message("[WARN] prevent recursive error, error exception thrown an error")
            if (!is.null(cond$meta)) {
              message(paste("       error thrown in", cond$meta$origin))
            }
            message(err.cond$message)
            stop(NULL)
          } else {
            error.exception()
          }
        }, session = session)
      }
    }
  }
  if (is.function(finally)) {
    finally()
  }
  result
}

#' global exception handler that is used in battery instead of tryCatch
#' @param expr - any expression
#' @param error - function that will be triggered on error default NULL
#'                if added it should return add add meta data create.error(cond, list(...))
#'                it is used internally by battery, it can safely ignored.
#' @param finally - function that is always executed after exception is handled
#' @param session - optional shiny session to create exception handler only for given session
#' @export
withExceptions <- function(expr, error = NULL, finally = NULL, session = NULL) {
  invisible(withCallingHandlers({
    withRestarts(
      expr = expr,
      battery__ignore = function() {
        ## this is left empty on purpose
      }
    )
  },
  error = function(cond) {
    if (!inherits(cond, "shiny.silent.error")) {
      if (is.function(error)) {
        err <- battery:::invoke(error, cond)
        if (is.list(err) && identical(err$class, "error")) {
          handle.error(err, finally, session = session)
        }
      } else {
        err <- create.error(cond, list(
          type = "exception",
          name = c
        ))
        handle.error(err, finally, session = session)
      }
      invokeRestart("battery__ignore")
    }
  },
  battery__exception = function(cond) {
    ret <- handle.exceptions(cond, finally, session = session)
    if (identical(ret, battery::pause())) {
      invokeRestart("battery__ignore")
    } else if (identical(ret, battery::end())) {
      shiny::stopApp()
    }
  }))
}

error.marker <- "__battery_error__"

#' create structure that can be used to signal error in applications
#' @param cond - input from withCallingHandlers it should be unexpected error in app
#' @param meta - addition extra data that should be added into meta property
#' @export
create.error <- function(cond, meta) {
  cond <- clean.error(cond)
  cond$class <- "error"
  cond$meta <- meta
  cond
}

#' Helper function that removes battery marker from error message
clean.error <- function(cond) {
  if (is.battery.error(cond)) {
    cond$message <- substring(cond$message, nchar(error.marker) + 1)
  }
  cond
}

#' helper function that check if error was triggered by battery::error function
is.battery.error <- function(cond) {
  is.character(cond$message) && grepl(paste0("^", error.marker), cond$message)
}

#' helper function that can be used in exception handler to trigger error handler
#' @param message - optional message that should be character string
#' @export
error <- function(message = NULL) {
  stop(paste0(error.marker, message))
}

#' signal exception in applications
#' @param class - string vector that indicate class of the exception
#' @param message - string that indicate given exception
#' @param data - optional data that should be used as exception
#' @param ... - eny extra data that should be added to the exception
#' @export
signal <- function(class, message = NULL, data = NULL, ...) {
  exception <- if (is.list(data)) {
    if (is.null(message)) {
      c(data, list(class = class))
    } else {
      c(data, list(message = message, class = class))
    }
  } else {
    c(list(...), list(message = message, class = class))
  }

  signalCondition(structure(
    exception,
    class = c("battery__exception", "condition")
  ))
}

#' helper function that clean message and remove classes from cond object
clean <- function(cond) {
  structure(clean.error(cond), class = NULL)
}


#' helper function that capture all signal messages into a vector
#' it can be used in unit tests to check if the function sent proper messages
#' @param expr - any expression
#' @param signal - charcater vector with signals that should be captured
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
get.exceptions <- function(session = NULL) {
  if (is.null(session)) {
    global$exceptions$global
  } else {
    global$exceptions$sessions[[ session$token ]]
  }
}

#' helper function that reset exceptions
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
  TRUE
}


#' helper function that can be used in exception handler to stop whole application
#' @export
end <- function() {
  FALSE
}
