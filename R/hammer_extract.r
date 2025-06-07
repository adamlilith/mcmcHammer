#' Extract posterior mean, median, or quantiles from an mcmc or mcmc.list
#'
#' Extracts posterior mean, median, standard deviation, or quantiles from an `mcmc` or `mcmc.list` object.
#' 
#' @param mcmc	An object of class `mcmc` or `mcmc.list`, *or* a `list`. If a `list`, the function searches down the first element to see if it can find an `mcmc` or `mcmc.list` object, then uses this if it can.
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

