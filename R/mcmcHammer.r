#' mcmcHammer: Analysis of MCMC chains
#'
#' Helper functions for analyzing MCMC chains. Includes trace plots and density plots. This package automates extraction of specific parameters, including indexed coefficients such as "beta[1, 1]", "beta[1, 2]", "beta[1, 3]", and similar.
#'
#' Create an issue on \href{https://github.com/adamlilith/mcmcHammer/issues}{GitHub}.
#'
#' @details
#' @section MCMC diagnostics:
#' 		\code{\link{emch_trace_dens}} Trace plots and density plots \cr
#' 
#' @section MCMC chain manipulation:
#' 		\code{\link{emch_stack}} "Stack" multiple MCMC chains on one another \cr
#'
#' @section Helper functions:
#' 		\code{\link{emch_param}} Create variable names (e.g., with indices) \cr
#' 
#' @docType package
#' @author Adam B. Smith
#' @name mcmcHammer
#' @keywords internal
NULL
