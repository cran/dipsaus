% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-rstudio.R
\name{rs_save_all}
\alias{rs_save_all}
\title{Save all documents in 'RStudio'}
\usage{
rs_save_all()
}
\description{
Perform "safe" save-all action with backward
compatibility: check whether 'RStudio' is running and whether
\code{rstudioapi} has function \code{documentSaveAll}.
}
