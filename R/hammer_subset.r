#' Subset MCMC chains to specific parameters
#'
#' @description Often, MCMC chains contain columns for parameters that we do not want to examine for a particular purpose. This function subsets MCMC chains to a specific set.
#' 
#' @param mcmc An object of class `mcmc` or `mcmc.list`, *or* a `list`. If a `list`, the function searches down the first element to see if it can find an `mcmc` or `mcmc.list` object, then uses this if it can.
#'
#' @param param Character vector: Name of the variable(s).
#'
#' @param i,j,k,l Indices used to specify variable names:
#' * `i` refers to indices that occur immediately after the variable's "base" name. For example, if `param` is `beta`, and `i = 0:2', then the function will return `beta0`, `beta1`, and `beta2`.
#'  * `j` refers to the *first* index in square brackets. For example, if `param` is `beta` and `j = 1:3`, then the output will be \code{beta[1]}, \code{beta[2]}, and \code{beta[3]}.
#'  * `k` refers to the *second* index in square brackets. If `k` is not `NULL`, then `j` cannot be `NULL`. For example, if `param` is `beta`, `j = 1:2`, and `k = 1:3`, then the output will be like \code{beta[1, 1]}, \code{beta[2, 1]}, \code{beta[1, 2]}, \code{beta[2, 2]}, \code{beta[1, 3]}, and \code{beta[2, 3]}.
#' * `l` refers to the *third* index in square brackets. You must also define `j` and `k` is you provide `l`. For example, if `param` is `beta`, `j = 1:2`, `k = 4:5`, and `l = 9:10`, then the output will be like \code{beta[1, 4, 9]}, \code{beta[2, 4, 9]}, \code{beta[1, 5, 9]}, \code{beta[2, 5, 9]}, \code{beta[1, 4, 10]}, \code{beta[2, 4, 10]}, and so on.
#' * You can also define `i` when you define `j` alone, `j` and `k`, or `j`, `k`, and `l` to get outputs like \code{beta0[1]}`, or like \code{beta0[1, 1]}, or like \code{beta0[1, 2, 3]}`.
#' You must specify all of the indices that would appear in square brackets. For example, for the pattern \code{param[j, k, l]}, you need to define `j`, `k`, and `l`. You can't just define `k = 1:2`. You need to set `j` and `l` equal to numeric/integer vectors and/or logical values.
#'
#' @param keep Logical: If `TRUE` (default), columns with names in `param` are retained. If `FALSE`, they are discarded.
#'
#' @returns An `mcmc` or  `mcmc.list`.
#'
#' @examples
#'
#' data(mcmc)
#' 
#' param  <- 'alpha'
#' subsetted <- hammer_subset(mcmc, param, i = TRUE)
#' head(subsetted)
#'
#' @export
hammer_subset <- function(
	mcmc,
	param,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	keep = TRUE
) {

	if (FALSE) {

		param
		i <- NULL
		j <- NULL
		k <- NULL
		l <- NULL
		keep = TRUE

	}

	mcmc_samples <- hammer_samples(mcmc)
	mcmc_summaries <- hammer_summaries(mcmc, fail = FALSE)

	params <- hammer_param(param, i = i, j = j, k = k, l = l, mcmc)

	n_chains <- length(mcmc_samples)
	cnames <- colnames(mcmc_samples[[1]])
	for (n_chain in 1:n_chains) {
		if (keep) {
			mcmc_samples[[n_chain]] <- mcmc_samples[[n_chain]][ , params, drop = FALSE]
		} else {
			mcmc_samples[[n_chain]] <- mcmc_samples[[n_chain]][ , !(cnames %in% params), drop = FALSE]
		}
	}

	if (!is.null(mcmc_summaries)) {
		n <- length(mcmc_summaries)
		cnames <- colnames(mcmc_samples[[1]])
		for (n_chain in 1:n) {
			if (keep) {
				mcmc_summaries[[n_chain]] <- mcmc_summaries[[n_chain]][params, , drop = FALSE]
			} else {
				mcmc_summaries[[n_chain]] <- mcmc_summaries[[n_chain]][!(cnames %in% params), , drop = FALSE]
			}
		}
	}

	list(
		samples = mcmc_samples,
		summary = mcmc_summaries
	)

}
