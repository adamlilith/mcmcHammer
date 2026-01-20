#' Number of chains in an mcmc object
#'
#' @description Returns the number of chains in an `mcmc` list.
#'
#' @inheritParams .mcmc
#'
#' @returns Numeric.
#'
#' @examples
#'
#' data(mcmc)
#' mc_n_chains(mcmc)
#'
#' @export
mc_n_chains <- function(mcmc) {

	samps <- mc_samples(mcmc)
	if (inherits(samps, c('mcmc.list', 'list'))) {
		out <- length(samps)
	} else if (inherits(samps, 'mcmc')) {
		out <- 1
	} else {
		stop('Object cannot be parsed.')
	}
	out

}
