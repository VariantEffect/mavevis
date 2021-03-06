% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/async.R
\name{check.async.progress}
\alias{check.async.progress}
\title{Check progress on asynchronous job}
\usage{
check.async.progress(jobID)
}
\arguments{
\item{jobID}{the ID of the job}
}
\value{
the contents of the stdout log for the given job, 
   or \code{NULL} if none exists
}
\description{
Retrieves the standard out log of an asynchronously launched job, if it exists.
If the job doesn't exist or hasn't written any log output, the function 
returns \code{NULL}.
}
