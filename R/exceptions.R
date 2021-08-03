#' function allows to add global exception handler for all battery components
#' @param handler - list of handlers
#' @param rest - indicate if old handlers should be removed
#' @param session - optional shiny session where to register the exception handler
#' @export
exceptions <- function(handler = NULL, reset = FALSE, session = NULL) {
  if (reset) {
    if (is.null(session)) {
      if (is.null(handler)) {
        global$exceptions$sessions <- list()
        global$exceptions$global <- list()
      } else {
        global$exceptions$global <- handler
      }
    } else {
      token <- session$token
      global$exceptions$sessions[[ token ]] <- if (is.null(handler)) {
        list()
      } else {
        handler
      }
    }
  } else if (is.null(handler)) {
    stop("battery::excpetion list require NULL given")
  } else if (is.null(session)) {
    exceptions <- global$exceptions$global
    global$exceptions$global <- if (is.null(exceptions)) {
      handler
    } else {
      modifyList(exceptions, handler)
    }
  } else {
    token <- session$token
    exceptions <- global$exceptions$sessions[[ token ]]
    global$exceptions$sessions[[ token ]] <- if (is.null(exceptions)) {
      handler
    } else {
      modifyList(exceptions, handler)
    }
  }
}

#' global handler for errors that print exception if user didn't process it
handle.error <- function(error, finally = NULL, session = NULL) {
  if (!handle.exceptions(error, finally = finally, session = session)) {
    message(paste("thrown in", error$origin))
    message(error$message)
    stop(cond)
  }
}

#' function that invoke global exception handler based on cond data
#' @param cond - structure with classes that indicate exception
handle.exceptions <- function(cond, finally = NULL, session = NULL) {
  result <- TRUE
  if (!is.null(cond$class)) {
    for (c in cond$class) {
      exceptions <- if (is.null(session)) {
        global$exceptions$global
      } else {
        global$exceptions$sessions[[ session$token ]]
      }
      if (is.function(exceptions[[ c ]])) {
        battery::withExceptions({
          ret <- battery:::invoke(exceptions[[ c ]], cond)
          if (identical(ret, FALSE) && result) {
            result <- FALSE
          }
        }, error = function(cond) {
          if (cond$message == "") {
            stop()
          } else if (c == "error") {
            message("[WARN] prevent recursive error, error exception thrown an error")
            message(cond$message)
            stop(cond)
          } else {
            create.error(cond, list(
              type = "exception",
              name = c
            ))
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
        if (is.list(err) && err$class == "error") {
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
    message(paste0('battery::', cond$class[1], cond$message))
    if (!handle.exceptions(cond, finally, session = session)) {
      invokeRestart("battery__ignore")
    }
  }))
}

#' create structure that can be used to signal error in applications
#' @param cond - input from withCallingHandlers it should be unexpected error in app
#' @param meta - addition extra data that should be added into meta property
#' @export
create.error <- function(cond, meta) {
  c(cond, list(class = "error", meta = meta))
}

#' signal exception in applications
#' @param class - string vector that indicate class of the exception
#' @param message - string that indicate given exception
#' @param ... - eny extra data that should be added to the exception
#' @export
signal <- function(class, message, ...) {
  exception <- structure(
    c(list(...), list(message = message, class = class)),
    class = c("battery__exception", "condition")
  )
  signalCondition(exception)
}

