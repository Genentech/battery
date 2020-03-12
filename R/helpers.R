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

#' Function load all R file components from directory, component classes will be global
#' @param path - path to directory
#' @param pattern - pattern used to selected the files (default all R files)
#' @param ignore - string or vector of strings with fillanme files that should be ignored
#' @param recursive - if set to TRUE (default) loading of files will be recursive
#' @export
load <- function(path, pattern = "*.R$", ignore = NULL, recursive = TRUE, ignore.case = TRUE) {
  components <- list.files(
    path = path,
    pattern = pattern,
    recursive = recursive,
    ignore.case = ignore.case,
    full.names = TRUE
  )
  if (is.null(ignore)) {
    for (path in components) {
      source(path, local = FALSE)
    }
  } else {
    for (path in components) {
      if (all(path != ignore)) {
        source(path, local = FALSE)
      }
    }
  }
}

#' Return correct timestamp as string
now <- function() {
  format(as.numeric(Sys.time())*1000, scientific = FALSE)
}

#' Force of reactive value trigger
## TODO: waiting for better solution (reply on SO or GitHub)
## https://github.com/rstudio/shiny/issues/2488
## https://stackoverflow.com/q/60654485/387194
force <- function(fn, session = getDefaultReactiveDomain()) {
  if (battery:::isReactiveContext()) {
    fn()
  } else {
    shinyHack <- shiny::isolate(session$input$battery_shinyHack)
    shiny::observeEvent(session$input$battery_shinyHack, {
      fn()
    }, once = TRUE)
    session$manageInputs(list(battery_shinyHack = `if`(is.null(shinyHack), TRUE, !shinyHack)))
  }
}

#' function test if we are in shiny reactive context
isReactiveContext <- function() {
  ## access of reactive value outside of reactive context will throw exception
  tryCatch({
    e <- new.env()
    shiny::makeReactiveBinding("dummy", env = e)
    e$dummy <- 10
    e$test <- e$dummy + 10
    TRUE
  }, error = function() {
    FALSE
  })
}

#' create string with repeated input string
str.repeat <- function(n, str) {
  if (n == 0) {
    ''
  } else if (n == 1) {
    str
  } else {
    paste(rep(str, n), collapse = "")
  }
}

#' add spaces before the string
indent <- function(n, string) {
  spaces <- battery:::str.repeat(n, " ")
  paste0(spaces, string)
}
