% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exceptions.R
\name{capture_signal_messages}
\alias{capture_signal_messages}
\title{helper function that capture all signal messages into a vector
it can be used in unit tests to check if the function sent proper messages}
\usage{
capture_signal_messages(expr, signal, session = NULL)
}
\arguments{
\item{expr}{- any expression}

\item{signal}{- charcater vector with signals that should be captured}
}
\description{
helper function that capture all signal messages into a vector
it can be used in unit tests to check if the function sent proper messages
}
