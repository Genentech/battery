% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/components.R
\name{new.global.env}
\alias{new.global.env}
\title{Main file with base of battery components
Global env generator function - added to each component one per root (component without parent)
This env is used so you can share services for one tree of components. You can't send data between
Two trees of components.}
\usage{
new.global.env()
}
\description{
Main file with base of battery components
Global env generator function - added to each component one per root (component without parent)
This env is used so you can share services for one tree of components. You can't send data between
Two trees of components.
}
