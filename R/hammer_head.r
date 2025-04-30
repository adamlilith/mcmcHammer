#' Display the top or bottom rows of an MCMC chain or its summary.
#'
#' @description Display the topmost or bottommost rows of an MCMC object.
#' 
#' @param x An `mcmc` or `mcmc.list` object.
#' @param param Character vector: Characters to keep or discard.
#' @param i,j,k,l Indices used to specify variable names. Please see [mh_param()].
#' @param chain Any of:
#' * `NULL` (default): Display head/tail of each chain.
#' * A numeric vector with integers: Display head/tail of these chains.
#' @param n Number of rows to display. Default is 6.
#'
#' @returns A `list` object with the head or tail of each chain.
#'
#' @examples
#'
#' data(mcmc)
#' hammer_head(mcmc, 1)
#' hammer_tail(mcmc, 1)
#' 
#' @export
hammer_head <- function(
	x,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	n = 6
) {

	if (inherits(x, 'mcmc')) x <- list(x)
	nchains <- length(x)

	out

}
