#' mcmcHammer: Analysis of MCMC chains
#'
#' Functions for analyzing MCMC chains. Includes trace plots and density plots. This package automates extraction of specific parameters, including indexed coefficients with names such as "`alpha0`", "`alpha1`", or "`beta[1, 1]`", "`beta[1, 2]`", "`beta[1, 3]`", and similar, without the user needing to, for example, use `paste()` to create the variable names or to remember how many `beta`s there were.
#'
#' Find a bug? Please report it and how it occurred on [GitHub](https://github.com/adamlilith/mcmcHammer/issues).
#'
#' @details
#' ## Extracting:
#'  * [hammer_extract()]: Mean, median, or lower or upper quantiles from the posterior distribution of a parameter.
#'  * [hammer_samples()]: Extract an `mcmc.list` object with MCMC samples.
#'  * [hammer_summaries()]: Extract an "summary" list of matrices.
#'  * [hammer_summary()]: Extract an "all=chains" summary matrix.
#'
#' ## MCMC chains:
#'  * [hammer_combine()]: Combine multiple `mcmc` and/or `mcmc.list` objects.
#'
#' ## Helper functions:
#'  * [hammer_combine()]: Combine two or more MCMC objects
#'  * [mh_param()]: Create variable names (e.g., with indices)
#'
#' @author Adam B. Smith
#' @name mcmcHammer
#' @keywords internal
"_PACKAGE"
