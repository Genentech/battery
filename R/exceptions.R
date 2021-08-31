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
handle.error <- function(error, finally = NULL, meta = NULL, session = NULL) {
  ret <- handle.exceptions(error, finally = finally, meta = meta, session = session)
  if (identical(ret, battery::end())) {
    if (!is.null(meta)) {
      message("thrown in ", meta$origin)
    }
    battery::error(error$message)
  }
}

#' function that invoke global exception handler based on cond data
#' @param cond - structure with classes that indicate exception
handle.exceptions <- function(cond, finally = NULL, meta = NULL, session = NULL) {
  result <- NULL
  if (!is.null(cond$class) && !is.battery.error(cond)) {
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
    for (cls in cond$class) {
      if (is.function(exceptions[[ cls ]])) {
        battery::withExceptions({
          ret <- battery:::invoke(exceptions[[ cls ]], battery:::clean(cond), meta)
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
            handle.error(err, finally, meta = meta, session = session)
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
withExceptions <- function(expr, error = NULL, finally = NULL, meta = NULL, session = NULL) {
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
    } else if (!inherits(cond, "shiny.silent.error")) {
      if (is.function(error)) {
        battery:::invoke(error, cond)
      } else {
        err <- create.error(cond, c(list(
          type = "exception",
          name = c
        ), meta))
        handle.error(err, finally, meta = meta, session = session)
      }
      invokeRestart("battery__ignore")
    }
  },
  battery__exception = function(cond) {
    ret <- handle.exceptions(cond, finally, meta = meta, session = session)
    if (identical(ret, battery::pause())) {
      invokeRestart("battery__ignore")
    } else if (identical(ret, battery::end())) {
      battery::error()
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
clean.error <- function(cond) {
  if (is.battery.error(cond)) {
    cls <- class(cond)
    class(cond) <- cls[cls != "battery.error"]
  }
  cond
}

#' helper function that check if error was triggered by battery::error function
is.battery.error <- function(x) inherits(x, "battery.error")

#' helper function that can be used in exception handler to trigger error handler
#' @param message - optional message that should be character string
#' @export
error <- function(message = NULL, bubble = FALSE) {
  if (is.null(message)) {
    message <- "__<1>__BUBBLE__"
  } else if (bubble) {
    re <- "__<[0-9]+>__"
    message <- if (grepl(re, message)) {
      num <- as.numeric(stringr::str_extract(message, '[0-9]+'))
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
  structure(TRUE, class = "battery.pause")
}


#' helper function that can be used in exception handler to stop whole application
#' @export
end <- function() {
  structure(FALSE, class = "battery.end")
}
