#' Re-calculate summaries of MCMC chains
#'
#' @description This function re-calculates the summary statistics of MCMC chains.
#' 
#' @param mcmc An `mcmc` or `mcmc.list` object, or a list of such objects.
#'
#' @returns A `list` with the following structure:
#' ```
#' .$summary$chain1 <matrix>
#' .$summary$chain2 <matrix>
#' .$summary$chain3 <matrix>
#' .$summary$chain4 <matrix>
#' (etc.)
#' .$summary$all.chains <matrix>
#' ```
#' @examples
#'
#' data(mcmc)
#' summ <- hammer_resummarize(mcmc)
#' str(summ)
#'
#' @export
hammer_resummarize <- function(mcmc) {

	if (inherits(mcmc, 'mcmc')) {
		mcmc <- list(list(mcmc))
	} else if (inherits(mcmc, 'mcmc.list')) {
		mcmc <- list(mcmc)
	}

	n_chains <- length(mcmc[[1]])
	n_iter <- nrow(mcmc[[1]][[1]])
	n_vars <- ncol(mcmc[[1]][[1]])
	vars <- colnames(mcmc[[1]][[1]])

	out <- list()
	for (n_chain in 1:n_chains) {
	
		out[[n_chain]] <- matrix(NA_real_, nrow = n_vars, ncol = 5, dimnames = list(vars, c('Mean', 'Median', 'St.Dev.', '95%CI_low', '95%CI_upp')))

		out[[n_chain]][ , 'Mean'] <- colMeans(mcmc[[1]][[n_chain]])
		out[[n_chain]][ , 'Median'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::median)
		out[[n_chain]][ , 'St.Dev.'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::sd)
		out[[n_chain]][ , '95%CI_low'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::quantile, 0.025)
		out[[n_chain]][ , '95%CI_upp'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::quantile, 0.975)

	
	}
	names(out) <- paste0('chain', 1:n_chains)

	out$all.chains <- matrix(NA_real_, nrow = n_vars, ncol = 5, dimnames = list(vars, c('Mean', 'Median', 'St.Dev.', '95%CI_low', '95%CI_upp')))
	
	stack <- do.call('rbind', mcmc[[1]])
	row_sums <- rowSums(stack)
	nas <- anyNA(row_sums)
	if (nas) warning('Some MCMC samples are NA.')
	
	out$all.chains[ , 'Mean'] <- colMeans(stack)
	out$all.chains[ , 'Median'] <- apply(stack, 2, FUN = stats::median)
	out$all.chains[ , 'St.Dev.'] <- apply(stack, 2, FUN = stats::sd)
	out$all.chains[ , '95%CI_low'] <- apply(stack, 2, FUN = stats::quantile, 0.025, na.rm = TRUE)
	out$all.chains[ , '95%CI_upp'] <- apply(stack, 2, FUN = stats::quantile, 0.975, na.rm = TRUE)

	out

}
