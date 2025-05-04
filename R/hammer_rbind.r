#' "Stack" multiple MCMC chains on one another
#'
#' This function takes an `mcmc.list` and `rbind()`'s' them onto one another to create a single, large matrix.
#'
#' @param mcmc	An object of class `mcmc`` or `mcmc.list``, or a `list``. If a `list``, the function searches down the first element to see if it can find an `mcmc` or `mcmc.list` object, then plots this if it can.
#'
#' @param chains Logical: If `TRUE`, then the output will have a column named `chain` with the chain number of each row.
#'
#' @returns Matrix.
#'
#' @examples
#' 
#' data(mcmc)
#' rbinded <- hammer_rbind(mcmc)
#' head(rbinded)
#' dim(rbinded)
#'
#' @rdname hammer_rbind
#' @export
hammer_rbind <- function(mcmc, chains = FALSE) {

	mcmc_chains <- hammer_samples(mcmc, fail = TRUE)

	out <- mcmc_chains[[1]]
	if (chains) {
		ch <- matrix(1, nrow = nrow(mcmc_chains[[1]]), ncol = 1, dimnames = list(NULL, 'chain'))
		out <- cbind(ch, out)
	}

	n_chains <- length(mcmc_chains)
	if (n_chains > 1) {
		for (i in 2:n_chains) {

			this_chain <- mcmc_chains[[i]]
			if (chains) {
				ch <- matrix(i, nrow = nrow(mcmc_chains[[i]]), ncol = 1, dimnames = list(NULL, 'chain'))
				this_chain <- cbind(ch, this_chain)
			}

			out <- rbind(out, this_chain)
		}
	}
	out
	
}
