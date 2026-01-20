#' Re-calculate summaries of MCMC chains
#'
#' @description This function re-calculates the summary statistics of MCMC chains.
#' 
#' @inheritParams .mcmc
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
#' summ <- mc_resummarize(mcmc)
#' str(summ)
#'
#' @export
mc_resummarize <- function(mcmc, na.rm = FALSE) {

	if (inherits(mcmc, 'mcmc')) {
		mcmc <- list(samples = list(mcmc))
	} else if (inherits(mcmc, 'mcmc.list')) {
		mcmc <- list(samples = mcmc)
	}

	n_chains <- length(mcmc[[1]])
	n_iter <- nrow(mcmc[[1]][[1]])
	n_vars <- ncol(mcmc[[1]][[1]])
	vars <- colnames(mcmc[[1]][[1]])

	out <- list(samples = mcmc[[1]], summary = list())
	for (n_chain in 1:n_chains) {
	
		out$summary[[n_chain]] <- matrix(NA_real_, nrow = n_vars, ncol = 5, dimnames = list(vars, c('Mean', 'Median', 'St.Dev.', '95%CI_low', '95%CI_upp')))

		out$summary[[n_chain]][ , 'Mean'] <- colMeans(mcmc[[1]][[n_chain]], na.rm = na.rm)
		out$summary[[n_chain]][ , 'Median'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::median, na.rm = na.rm)
		out$summary[[n_chain]][ , 'St.Dev.'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::sd, na.rm = na.rm)
		out$summary[[n_chain]][ , '95%CI_low'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::quantile, probs = 0.025, na.rm = na.rm)
		out$summary[[n_chain]][ , '95%CI_upp'] <- apply(mcmc[[1]][[n_chain]], 2, FUN = stats::quantile, probs = 0.975, na.rm = na.rm)

	
	}
	names(out$summary) <- paste0('chain', 1:n_chains)

	out$summary$all.chains <- matrix(NA_real_, nrow = n_vars, ncol = 5, dimnames = list(vars, c('Mean', 'Median', 'St.Dev.', '95%CI_low', '95%CI_upp')))
	
	stack <- do.call('rbind', mcmc[[1]])
	row_sums <- rowSums(stack)
	nas <- anyNA(row_sums)
	if (nas) warning('Some MCMC samples are NA. Sampling may be invalid.')
	
	out$summary$all.chains[ , 'Mean'] <- colMeans(stack, na.rm = na.rm)
	out$summary$all.chains[ , 'Median'] <- apply(stack, 2, FUN = stats::median, na.rm = na.rm)
	out$summary$all.chains[ , 'St.Dev.'] <- apply(stack, 2, FUN = stats::sd, na.rm = na.rm)
	out$summary$all.chains[ , '95%CI_low'] <- apply(stack, 2, FUN = stats::quantile, probs = 0.025, na.rm = na.rm)
	out$summary$all.chains[ , '95%CI_upp'] <- apply(stack, 2, FUN = stats::quantile, probs = 0.975, na.rm = na.rm)

	out

}
