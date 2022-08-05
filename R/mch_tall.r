#' "Stack" multiple MCMC chains on one another
#'
#' This function takes a list of MCMC chains and "stacks" them onto one another. This can be useful for plotting.
#'
#' @param x	An object of class \code{mcmc} or \code{mcmc.list}, or a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#'
#' @return Matrix.
#' @export

mch_tall <- function(x) {

	# "stacking" all chains on one another
	if (inherits(x, 'mcmc.list')) {
		
		# add first chain
		mcmc <- mch_tall(x[[1L]])
		# max_iter <- nrow(mcmc)
		# iter <- 1L:max_iter
		# chain <- rep(1L, max_iter)
		# add <- matrix(
			# c(iter, chain),
			# ncol=2L,
			# dimnames = list(iter, c('iter', 'chain'))
		# )
		# mcmc <- cbind(add, mcmc)
		
		# add subsequent chains
		if (length(x) > 1L) {
			for (count_chain in 2L:length(x)) {

				# this_mcmc <- x[[count_chain]]
				
				# max_iter <- nrow(this_mcmc)
				# iter <- 1L:max_iter
				# chain <- rep(count_chain, max_iter)
				# add <- matrix(
					# c(iter, chain),
					# ncol=2L,
					# dimnames = list(iter, c('iter', 'chain'))
				# )
				this_mcmc <- mch_tall(x[[count_chain]])
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

	} else if (inherits(x, 'list')) {
	
		is_mcmc <- FALSE
		count <- 1L
		while (!is_mcmc) {
			if (inherits(x[[count]], c('mcmc', 'mcmc.list'))) {
				x <- x[[count]]
				mcmc <- mch_tall(x)
				is_mcmc <- TRUE
			}
			count <- count + 1L
		}
	} else {
		stop('Argument "x" must be an object of class "mcmc", "mcmc.list", or "list".')
	}

	mcmc <- data.table::as.data.table(mcmc)
	mcmc
	
}
