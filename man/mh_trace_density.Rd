% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mh_trace_density.r
\name{mh_trace_density}
\alias{mh_trace_density}
\title{Trace plots and density plots}
\usage{
mh_trace_density(
  mcmc,
  param = NULL,
  i = NULL,
  j = NULL,
  k = NULL,
  l = NULL,
  smooth = TRUE,
  nrow = 5,
  ncol = 1,
  file = NULL,
  stacked = TRUE,
  ...
)
}
\arguments{
\item{mcmc}{An "stacked" set of MCMC chains, \emph{or} an object of class \code{mcmc} or \code{mcmc.list}, \emph{or} a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.}

\item{param}{Name of the variable(s). The outcome depends on the definitions of \code{i}, \code{j}, \code{k}, and \code{l}. Please see the help for \code{\link{mh_param}} for more explanation on how to specify this argument, plus \code{i}, \code{j}, \code{k}, and \code{l}.}

\item{i, j, k, l}{Indices used to specify variable names. Please see the help for \code{\link{mh_param}}.}

\item{smooth}{Logical. If \code{TRUE} (default), draw a LOESS line for each chain in a trace plot.}

\item{nrow, ncol}{Number of rows and columns of graphs to display.}

\item{file}{Either \code{NULL} (default) or name of a file to which to save (including the file type suffix, like \code{.png} or \code{.pdf}). Specifying a PDF file is especially helpful for cases where there are many plots and multiple pages need to be made to display them.}

\item{stacked}{\code{FALSE}, in which case \code{mcmc} is assumed to be an object of class \code{mcmc}, \code{mcmc.list}, or a \code{list}, or \code{TRUE} (default), in which case it is a "stacked" MCMC table. This argument is usually used by other functions in this package, so can often be ignored. However, if your MCMC chains have a lot of iterations or variables, then you can speed things up by "stacking" the chains using \code{\link{mh_stack}}, then using that for \code{mcmc}.}

\item{...}{Arguments to pass to \code{\link[ggplot2]{ggsave}}.}
}
\value{
A \pkg{ggplot2} \code{ggplot} graphic object and (possibly) a file saved.
}
\description{
Trace plots and density plots
}
