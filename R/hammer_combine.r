#' Combine MCMC chains
#'
#' @description Sometimes, you need to run different MCMC chains in different instances of **R**, then want to combine them. This function does that by creating a `list`, typical of outputs from **JAGS**, **nimble**, **stan**, and other MCMC sampler software.
#' 
#' @param ... Two or more `list`s with `mcmc.list` objects therein, or a `list` with an `mcmc` object (i.e., a single chain).
#'
#' @returns A `list` with the following structure:
#' ```
#' .$samples$chain1
#' .$samples$chain2
#' .$samples$chain3
#' .$samples$chain4
#' (etc.)
#' .$summary$chain1
#' .$summary$chain2
#' .$summary$chain3
#' .$summary$chain4
#' (etc.)
#' .$summary$all.chains
#' ```
#' Chains in the input will be renumbered as needed in the output.
#'
#' @examples
#'
#' data(mcmc)
#' combo <- hammer_combine(mcmc, mcmc) # combine "mcmc" with itself
#' str(combo, 2)
#'
#' @export
hammer_combine <- function(...) {

	x <- list(...)
	
	# add first object
	# single chain... first object is an mcmc
	if (inherits(x[[1]][[1]], 'mcmc')) {
		
		out <- list()
		out$samples$chain1 <- x[[1]]$samples
	
	} else if (inherits(x[[1]][[1]], 'mcmc.list')) {
	
		out <- x[[1]]
		n_chains_out <- length(out$samples)
		names(out$samples) <- paste0('chain', 1:n_chains_out)

	} else {
		stop('Input must be a list which has two top elements, one named `samples` with either an `mcmc` or `mcmc.list` object, and the other named `summary` with a list of summary matrices.')
	}
	
	n_vars <- ncol(out$samples$chain1)
	n_iter <- nrow(out$samples$chain1)

	# add subsequent objects
	length_input <- length(x)
	out_names <- colnames(out$samples[[1]])
	if (length_input > 1) {
	
		n_chains_out <- length(out$samples)
	
		for (i in 2:length_input) {
			
			# next input is just an mcmc
			if (inherits(x[[i]][[1]], 'mcmc')) {
				
				if (ncol(x[[i]]$samples) != n_vars) stop('Input has unequal number of columns. Are these from the same MCMC model?')
				if (nrow(x[[i]]$samples) != n_iter) stop('Input has unequal number of iterations. Are these from the same MCMC model?')
				
				new_names <- colnames(x[[i]]$samples)
				if (any(out_names != new_names)) stop('Column names do not match across objects. Are these from the same MCMC model?')
				
				out$samples$TEMP <- x[[i]]$samples
				# out$summary$TEMP <- x[[i]]$summary

				n_chains_out <- n_chains_out + 1
				names(out$samples)[length(out$samples)] <- paste0('chain', n_chains_out)
				# names(out$summary)[length(out$summary)] <- paste0('chain', n_chains_out)
			
			# next input is an mcmc.list
			} else if (inherits(x[[i]][[1]], 'mcmc.list')) {
			
				n_chains_in <- length(x[[i]]$samples)
				for (j in 1:n_chains_in) {
				
					if (ncol(x[[i]][[1]][[j]]) != n_vars) stop('Input has unequal number of columns.')
					if (nrow(x[[i]][[1]][[j]]) != n_iter) stop('Input has unequal number of iterations.')

					new_names <- colnames(x[[i]]$samples[[j]])
					if (any(out_names != new_names)) stop('Column names do not match across objects. Are these from the same MCMC model?')

					out$samples$TEMP <- x[[i]]$samples[[j]]

					n_chains_out <- n_chains_out + 1
					names(out$samples)[length(out$samples)] <- paste0('chain', n_chains_out)
				
				}
			
			} else {
				stop('Input must be a list which has two top elements, one named `samples` with either an `mcmc` or `mcmc.list` object, and the other named `summary` with a list of summary matrices.')
			}
		
		}
	
	}
	
	out <- hammer_resummarize(out)
	out$samples <- coda::as.mcmc.list(out$samples)
	out

}
