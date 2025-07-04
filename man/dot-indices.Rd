% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared_args.r
\name{.indices}
\alias{.indices}
\title{Shared argument(s)}
\arguments{
\item{indices}{A \code{list} of \code{lists}. If you want to select multiple parameters, which each have their own indexing (\code{i}, \code{j}, \code{k}, and/or \code{l}), you can specify them with this argument. Each sub-\code{list} should have the indices you wish to use for the given value of \code{param}. Sub-\code{list}s will be matched with the respective value of \code{param}. For example, if you want to select parameters \code{alpha0}, \code{alpha1} and \code{beta[1]} and \code{beta[2]} from an \code{mcmc} chain, you could use \code{hammer_subset(mcmc, param = c('alpha', 'beta'), indices = list(list(i = 0:1), list(j = 1:2)))}. You can't use \code{i}, \code{j}, \code{k}, or \code{l} in combination with \code{indices}.}
}
\description{
Shared argument(s)
}
