#' Extract posterior mean, median, or quantiles from an mcmc or mcmc.list
#'
#' Extracts posterior mean, median, standard deviation, or quantiles from an `mcmc` or `mcmc.list` object.
#' 
#' @inheritParams .mcmc
#'
#' @param param Character vector: Name of the variable(s).
#'
#' @inheritParams .ijkl
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
hammer_extract <- function(mcmc, param, i = NULL, j = NULL, k = NULL, l = NULL, stat = 'mean', quant = 0.5) {

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

