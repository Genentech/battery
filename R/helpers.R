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
#' @param ignore.case - should it ignore case of pattern when searching for files
#' @param recursive - if set to TRUE (default) loading of files will be recursive
#' @param local - TRUE, FALSE or enviroment use in source - if value is TRUE it will use parent.frame()
#' @export
load <- function(path,
                 pattern = "*.R$",
                 ignore = NULL,
                 recursive = TRUE,
                 ignore.case = TRUE,
                 local = TRUE) {
  components <- list.files(
    path = path,
    pattern = pattern,
    recursive = recursive,
    ignore.case = ignore.case,
    full.names = TRUE
  )
  if (isTRUE(local)) {
    local <- parent.frame()
  }
  if (is.null(ignore)) {
    for (path in components) {
      source(path, local = local)
    }
  } else {
    for (path in components) {
      if (all(path != ignore)) {
        source(path, local = local)
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
  shinyHack <- shiny::isolate(session$input$battery_shinyHack)
  shiny::observeEvent(session$input$battery_shinyHack, {
    fn()
  }, once = TRUE)
  session$manageInputs(list(battery_shinyHack = `if`(is.null(shinyHack), TRUE, !shinyHack)))
}

#' add spaces before the string
indent <- function(n, string) {
  spaces <- strrep(" ", n)
  paste0(spaces, string)
}
