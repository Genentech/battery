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
    stop("battery::excpetion list require NULL given")
  } else {
    extend.exceptions(handler, session)
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
    message(paste("battery::", cond$message))
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
