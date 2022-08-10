#' "Stack" multiple MCMC chains on one another
#'
#' This function takes a list of MCMC chains and "stacks" them onto one another. This can be useful for plotting.
#'
#' @param x	An object of class \code{mcmc} or \code{mcmc.list}, or a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param recursive If \code{TRUE} (default), then recursively search \code{x} (assuming it is a list), for all elements that are objects of class \code{mcmc} and stack them. Note that if different list elements contain MCMC chains from different models (i.e., variables are not the same), this will fail.
#'
#' @return Matrix.
#' @export

mh_stack <- function(x, recursive = TRUE) {

	# "stacking" all chains on one another
	if (inherits(x, 'mcmc.list')) {
		
		# add first chain
		mcmc <- mh_stack(x[[1L]])
		
		# add subsequent chains
		if (length(x) > 1L) {
			for (count_chain in 2L:length(x)) {

				this_mcmc <- mh_stack(x[[count_chain]])
				this_mcmc[ , 'chain'] <- count_chain
				
				mcmc <- rbind(mcmc, this_mcmc)
			}
		}
	
	} else if (inherits(x, 'mcmc')) {

		mcmc <- x
		max_iter <- nrow(mcmc)
		iter <- 1L:max_iter
		n_chain <- rep(1L, max_iter)
		add <- matrix(
			c(iter, n_chain),
			ncol=2L,
			dimnames = list(iter, c('iter', 'chain'))
		)
		mcmc <- cbind(add, mcmc)

	} else if (recursive & inherits(x, 'list')) {
	
		x <- rlist::list.flatten(x, classes=c('mcmc'))

		# add first chain
		mcmc <- mh_stack(x[[1L]])
		
		# add subsequent chains
		if (length(x) > 1L) {
			for (count_chain in 2L:length(x)) {

				this_mcmc <- mh_stack(x[[count_chain]])
				this_mcmc[ , 'chain'] <- count_chain
				
				mcmc <- rbind(mcmc, this_mcmc)
			}
		}
		
	} else {
		stop('Argument "x" must be an object of class "mcmc" or "mcmc.list".')
	}

	if (!is.na(mcmc)) mcmc <- data.table::as.data.table(mcmc)
	mcmc
	
}
