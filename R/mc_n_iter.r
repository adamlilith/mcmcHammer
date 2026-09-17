#' Number of iterations in an mcmc object
#'
#' @description Returns the number of iterations in an `mcmc` list.
#'
#' @inheritParams .mcmc
#' @param total Logical. If `TRUE`, then the value is the total number of iterations across all chains. If `FALSE` (default), then the value is the number in one chain.
#'
#' @returns Numeric.
#'
#' @examples
#'
#' data(mcmc)
#' mc_n_iter(mcmc)
#'
#' @export
mc_n_iter <- function(mcmc, total = FALSE) {

	if (inherits(mcmc, 'list')) {
		out <- nrow(mcmc$samples[[1]])
	} else if (inherits(mcmc, 'mcmc.list')) {
		out <- nrow(mcmc[[1]])
	} else if (inherits(mcmc, 'mcmc')) {
		out <- nrow(mcmc)
	} else {
		stop('Object cannot be parsed.')
	}
	if (total) {
		out <- out * mc_n_chains(mcmc)
	}
	out

}
