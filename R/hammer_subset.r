#' Subset MCMC chains to specific parameters
#'
#' @description Often, MCMC chains contain columns for parameters that we do not want to examine for a particular purpose. This function subsets MCMC chains to a specific set.
#' 
#' @param mcmc	An object of class `mcmc` or `mcmc.list`, *or* a `list`. If a `list`, the function searches down the first element to see if it can find an `mcmc` or `mcmc.list` object, then uses this if it can.
#' @param param Name of the variable(s). The outcome depends on the definitions of `i`, `j`, `k`, and `l`:
#' * `param = NULL`: All variables in the MCMC object. An object must be supplied to the `mcmc` argument.
#' * `param =` a character vector and `i` is a numeric vector: Variables with the pattern "`param*`" where `*` is `i`. For example: `beta1`, `beta2`, and `beta3`.
#' * `param =` a character vector and `i` is `TRUE`: Finds in the MCMC object all variables with the pattern "`param*`" where `*` is numeric. You must supply `mcmc` in this case.
#' * `param =` a character vector and `j` is a numeric vector, but `k` is `NULL`: Variables with the pattern "`param[j]`".
#' * `param =` a character vector and `j` is `TRUE`, but `k` is `NULL`: Variables with the pattern "`param[j]`". You must supply `mcmc` in this case.
#' * `param =` a character vector and `j` and `k` are a numeric vectors but `l` is `NULL`: Variables with the pattern "`beta[j, k]`".
#' * `param =` a character vector and `j` and/or `k` are `TRUE` but `l` is `NULL`: Variables with the pattern "`beta[*, k]`", "`beta[j, *]`", or "`beta[*, *]`" (depending on values of `j` and `k`. You must supply `mcmc` in this case.
#' * `param =` a character vector and `j`, `k`, and `l` are a numeric vectors: Variables with the pattern "`beta[j, k, l]`".
#' * `param =` a character vector and one or more of `j`, `k`, and `l` are a numeric vectors and/or `TRUE`: Variables with the pattern "`beta[*, k, l]`", "`beta[j, *, l]`", "`beta[j, k, *]`", "`beta[*, *, l]`", "`beta[*, k, *]`",  "`beta[j, *, *]`", or "`beta[*, *, *]`". You must supply `mcmc` in this case.
#'
#' @param i,j,k,l Indices used to specify variable names. Please see the help for [hammer_param()].
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

	}

	mcmc_samples <- hammer_samples(mcmc)
	mcmc_summaries <- hammer_summaries(mcmc, fail = FALSE)

	params <- hammer_param(param, i = i, j = j, k = k, l = l, mcmc)

	n_chains <- length(mcmc_samples)
	for (n_chain in 1:n_chains) {
		if (keep) {
			mcmc_samples[[n_chain]] <- mcmc_samples[[n_chain]][ , params, drop = FALSE]
		} else {
			mcmc_samples[[n_chain]] <- mcmc_samples[[n_chain]][ , !(nc %in% params), drop = FALSE]
		}
	}

	if (!is.null(mcmc_summaries)) {
		n <- length(mcmc_summaries)
		for (n_chain in 1:n) {
			if (keep) {
				mcmc_summaries[[n_chain]] <- mcmc_summaries[[n_chain]][params, , drop = FALSE]
			} else {
				mcmc_summaries[[n_chain]] <- mcmc_summaries[[n_chain]][!(nc %in% params), , drop = FALSE]
			}
		}
	}

	list(
		samples = mcmc_samples,
		summary = mcmc_summaries
	)

}
