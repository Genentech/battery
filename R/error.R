#' @export
handleErrors <- function(expr, reactionsPerType = list()){
  withCallingHandlers(
    expr = expr,
    error = function(e){
      callReaction(
        type = "unexpected",
        reactions = reactionsPerType,
        message = e$message
      )

      stop(e)
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
