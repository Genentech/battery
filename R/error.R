#' Helper function used by observeEvent
#'
#' @param expr - expression
#' @param reactionsPerType - list of named callbacks
#'
#' @export
handleErrors <- function(expr, reactionsPerType = list()){
  withCallingHandlers(
    expr = expr,
    error = function(e){
      if (!is(e, "shiny.silent.error")){
        # Shiny produces silent error on falsy req()
        callReaction(
          type = "unexpected",
          reactions = reactionsPerType,
          message = e$message
        )
        stop(e)
      }
    },
    message = function(m){
      message <- strsplit(
        x = m$message,
        split = "^<%|%>"
      )[[1]]

      if (length(message) > 1){
        types <- strsplit(message[[2]], ",")[[1]]
        message <- message[[3]]

        for (type in types){
          callReaction(
            type = type,
            reactions = reactionsPerType,
            message = message
          )
        }

        isSilent <- "silent" %in% types

        if (!isSilent){
          isLogOnly <- "logonly" %in% types

          logMessage(
            msgData = m,
            msgType = 'if'(isLogOnly, "log", "error"),
            stackTrace = shiny::printStackTrace(m)
          )

          if (!isLogOnly){
            stop(message)
          }
        }
      }
    }
  )
}
#' Function call callback function from list with argument message
#'
#' @param type - type of reaction
#' @param reactions - list of named callback functions
#' @param message - argument to call the function with
callReaction <- function(type, reactions, message){
  if (type %in% names(reactions)){
    reactions[[type]](message)
  }
}

#' Function print message with type and optional stack trace
#' @param msgData - list
#' @param msgType - strings
#' @param stackTrace - stack frames
#' @export
logMessage <- function(msgData, msgType, stackTrace = NULL) {
  time <- Sys.time()
  msg <- msgData$message
  expr <- paste(trimws(deparse(msgData$call)), collapse = "")

  if (msgType == "log"){
    message(sprintf("[%s] %s\n", time, msg))
  } else {
    message(sprintf("[%s] %s in:\n `%s`:\n%s\n", time, msgType, expr, msg))

    if (!is.null(stackTrace)) {
      message("Traceback:\n")
      message(paste(shiny::formatStackTrace(stackTrace), collapse = "\n"))
    }
  }
}
