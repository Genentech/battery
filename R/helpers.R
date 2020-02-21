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
