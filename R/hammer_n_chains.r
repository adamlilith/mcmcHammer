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
#' hammer_n_chains(mcmc)
#'
#' @export
hammer_n_chains <- function(mcmc) {

	samps <- hammer_samples(mcmc)
	if (inherits(samps, c('mcmc.list', 'list'))) {
		out <- length(samps)
	} else if (inherits(samps, 'mcmc')) {
		out <- 1
	} else {
		stop('Object cannot be parsed.')
	}
	out

}
