#' Re-calculate summaries of MCMC chains
#'
#' @description This function re-calculates the summary statistics of MCMC chains.
#' 
#' @param mcmc An `mcmc` or `mcmc.list` object, or a list of such objects.
#' @param na.rm Logical: If `FALSE` (default), then summary statistics will be `NA` if any value in any iteration is `NA` (and it probably should be `NA`). However, if you set this to `TRUE`, then `NA` values are ignored.
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
hammer_resummarize <- function(mcmc, na.rm = FALSE) {

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

		out[[n_chain]][ , 'Mean'] <- colMeans(mcmc[[1]][[n_chain]], na.rm = na.rm)
		out[[n_chain]][ , 'Median'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::median, na.rm = na.rm)
		out[[n_chain]][ , 'St.Dev.'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::sd, na.rm = na.rm)
		out[[n_chain]][ , '95%CI_low'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::quantile, probs = 0.025, na.rm = na.rm)
		out[[n_chain]][ , '95%CI_upp'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::quantile, probs = 0.975, na.rm = na.rm)

	
	}
	names(out) <- paste0('chain', 1:n_chains)

	out$all.chains <- matrix(NA_real_, nrow = n_vars, ncol = 5, dimnames = list(vars, c('Mean', 'Median', 'St.Dev.', '95%CI_low', '95%CI_upp')))
	
	stack <- do.call('rbind', mcmc[[1]])
	row_sums <- rowSums(stack)
	nas <- anyNA(row_sums)
	if (nas) warning('Some MCMC samples are NA. Sampling may be invalid.')
	
	out$all.chains[ , 'Mean'] <- colMeans(stack, na.rm = na.rm)
	out$all.chains[ , 'Median'] <- apply(stack, 2, FUN = stats::median, na.rm = na.rm)
	out$all.chains[ , 'St.Dev.'] <- apply(stack, 2, FUN = stats::sd, na.rm = na.rm)
	out$all.chains[ , '95%CI_low'] <- apply(stack, 2, FUN = stats::quantile, probs = 0.025, na.rm = na.rm)
	out$all.chains[ , '95%CI_upp'] <- apply(stack, 2, FUN = stats::quantile, probs = 0.975, na.rm = na.rm)

	out

}
