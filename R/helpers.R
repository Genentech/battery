#' Function call function fn with the only arguments it accept
#' @param fn - function to be called
#' @param ... - list of arguments
invoke <- function(fn, ...) {
  if (!is.function(fn)) {
    stop("invoke: argument need to be a function")
  }
  count <- length(formals(fn))
  do.call(fn, utils::head(list(...), count))
}

#' helper that print args
#' @param ... - arguments to print
debug <- function(...) {
  print(paste(list(...), collapse = " "))
}

#' renderUI wrapper with force argument (all params are the )
#' @param expr - same as in shiny::renderUI
#' @param env - same as in shiny::renderUI
#' @param quoted  - same as in shiny::renderUI
#' @param outputArgs - same as in shiny::renderUI
#' @param force - it will always call renderUI on init, even if shiny think that it don't need to
#' @export
renderUI <- function(expr,
                     env = parent.frame(),
                     quoted = FALSE,
                     outputArgs = list(),
                     force = FALSE) {
  if (!force) {
    shiny::renderUI(
      expr,
      env = env,
      quoted = quoted,
      outputArgs = outputArgs
    )
  } else {
    fn <- shiny::installExprFunction(expr, "func", env, quoted)
    ## reactive value should force to render even if expr
    ## didn't changed
    dummy.env <- new.env()
    name <- paste0("force_", uuid::UUIDgenerate())
    shiny::makeReactiveBinding(name, dummy.env)
    env[[name]] <- TRUE
    shiny::createRenderFunction(
      function() {
        dummy.env[[name]]
        fn()
      },
      shiny::uiOutput, outputArgs
    )
  }
}
