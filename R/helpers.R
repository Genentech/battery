#' Function call function fn with the only arguments it accept
invoke <- function(fn, ...) {
  if (!is.function(fn)) {
    stop("invoke: argument need to be a function")
  }
  count <- length(formals(fn))
  do.call(fn, head(list(...), count))
}

#' helper that print args
debug <- function(...) {
  print(paste(list(...), collapse = " "))
}
