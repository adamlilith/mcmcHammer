#' Extract posterior mean, median, or quantiles from an mcmc or mcmc.list
#'
#' Extracts posterior mean, median, standard deviation, or quantiles from an `mcmc` or `mcmc.list` object.
#' 
#' @param mcmc	An object of class `mcmc` or `mcmc.list`, *or* a `list`. If a `list`, the function searches down the first element to see if it can find an `mcmc` or `mcmc.list` object, then uses this if it can.
#' @param param Name of the variable(s). The outcome depends on the definitions of `i`, `j`, `k`, and `l`:
#' * `param = NULL`: All variables in the MCMC object. An object must be supplied to the `mcmc` argument.
#' * `param =` a character vector and `i` is a numeric vector: Variables with the pattern '`param*`' where `*` is `i`. For example: `beta1`, `beta2`, and `beta3`.
#' * `param =` a character vector and `i` is `TRUE`: Finds in the MCMC object all variables with the pattern '`param*`' where `*` is numeric. You must supply `mcmc` in this case.
#' * `param =` a character vector and `j` is a numeric vector, but `k` is `NULL`: Variables with the pattern '`param[j]`'.
#' * `param =` a character vector and `j` is `TRUE`, but `k` is `NULL`: Variables with the pattern '`param[j]`'. You must supply `mcmc` in this case.
#' * `param =` a character vector and `j` and `k` are a numeric vectors but `l` is `NULL`: Variables with the pattern '`beta[j, k]`'.
#' * `param =` a character vector and `j` and/or `k` are `TRUE` but `l` is `NULL`: Variables with the pattern '`beta[*, k]`', '`beta[j, *]`', or '`beta[*, *]`' (depending on values of `j` and `k`. You must supply `mcmc` in this case.
#' * `param =` a character vector and `j`, `k`, and `l` are a numeric vectors: Variables with the pattern '`beta[j, k, l]`'.
#' * `param =` a character vector and one or more of `j`, `k`, and `l` are a numeric vectors and/or `TRUE`: Variables with the pattern '`beta[*, k, l]`', '`beta[j, *, l]`', '`beta[j, k, *]`', '`beta[*, *, l]`', '`beta[*, k, *]`',  '`beta[j, *, *]`', or '`beta[*, *, *]`'. You must supply `mcmc` in this case.
#'
#' @param i,j,k,l Indices used to specify variable names. Please see the help for [hammer_param()].
#' @param stat Character: Indicates which statistic to extract. Options are `'mean'` (default), `'median'`, `'sd'`, `'lower'`, '`upper'`. The `lower` and `upper` options extract the lower and upper 2.5th and 97.5th quantiles, respectively. Partial matching is allowed.
#'
#' @returns An `mcmc.list` object or a `list`.
#'
#' @examples
#'
#' data(mcmc)
#' hammer_extract(mcmc, param = 'beta', j = 1:3)
#' hammer_extract(mcmc, param = 'beta', j = 1:3, stat = 'median')
#' hammer_extract(mcmc, param = 'z_hat', j = TRUE, k = TRUE)
#'
#' @export hammer_extract
hammer_extract <- function(mcmc, param, i = NULL, j = NULL, k = NULL, l = NULL, stat = 'mean') {

	if (FALSE) {

		param
		i <- NULL
		j <- NULL
		k <- NULL
		l <- NULL

	}

	mcmc_summary <- hammer_summary(mcmc, fail = TRUE)
	params <- hammer_param(param, i = i, j = j, k = k, l = l, mcmc = mcmc)

	stat <- tolower(stat)
	stat <- omnibus::pmatchSafe(stat, c('mean', 'median', 'sd', 'lower', 'upper'), nmax = 1)

	if (stat == 'mean') {
		out <- mcmc_summary[params, 'Mean', drop = TRUE]
	} else if (stat == 'median') {
		out <- mcmc_summary[params, 'Median', drop = TRUE]
	} else if (stat == 'sd') {
		out <- mcmc_summary[params, 'St.Dev.', drop = TRUE]
	} else if (stat == 'lower') {
		out <- mcmc_summary[params, '95%CI_low', drop = TRUE]
	} else if (stat == 'upper') {
		out <- mcmc_summary[params, '95%CI_upp', drop = TRUE]
	} else {
		stop('Invalid value for `stat`.')
	}

	names(out) <- params
	out
	
}

