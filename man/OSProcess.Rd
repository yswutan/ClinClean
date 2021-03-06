% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ClinFunction.R
\name{OSProcess}
\alias{OSProcess}
\title{OSProcess}
\usage{
OSProcess(sheet, ID, type, Ceiling = FALSE)
}
\arguments{
\item{sheet}{a combined sheet}

\item{ID}{a character of patient IDs}

\item{type}{chose max}

\item{Ceiling}{chose TRUE}
}
\value{
a data.frame with overall survival and patient state (0: survival, 1: dead)
}
\description{
OSProcess
}
\examples{
OSAll <- OSProcess(sheet, ID, "max", Ceiling=TRUE)
}
