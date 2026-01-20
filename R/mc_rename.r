#' Rename a variable in ann MCMC object
#'
#' Sometimes we just goof up and name a variable something we later regret. Using this function, you can correct that error by renaming the variable in an MCMC object.
#' 
#' @inheritParams .mcmc
#'
#' @param param_from Character: Name of the variable to rename.
#' @param param_to Character: New name.
#'
#' @inheritParams .ijkl
#'
#' @returns An `mcmc.list` object or a `list`.
#'
#' @examples
#'
#' data(mcmc)
#'
#' # simple extraction
#' mc_param(mcmc)
#' mcmc_new <- mc_rename(
#'    mcmc, param_from = 'beta', param_to = 'gamma', j = 1:3
#' )
#' mc_param(mcmc_new)
#'
#' @export mc_rename
mc_rename <- function(mcmc, param_from, param_to, i = NULL, j = NULL, k = NULL, l = NULL) {

	if (FALSE) {

		param_from
		param_to <- 'new_var'
		i <- NULL
		j <- NULL
		k <- NULL
		l <- NULL

	}

	if (is.logical(i) | is.logical(j) | is.logical(k) | is.logical(l)) stop('Logical values cannot be used in mc_rename(). Please use either `NULL` or numeric indices. If you are unsure of the indexing, you can use mc_param(mcmc, ...) to get a vector of the parameters and inspect the indexing.')

	if (length(param_from) != 1) stop('Can only supply one value for `param_from`.')
	if (length(param_to) != 1) stop('Can only supply one value for `param_to`.')

	params_from <- mc_param(mcmc = mcmc, param = param_from, i = i, j = j, k = k, l = l)
	params_to <- mc_param(param = param_to, i = i, j = j, k = k, l = l)

	if (length(param_from) != length(param_to)) stop('')

	n_chains <- mc_n_chains(mcmc)

	# rename samples
	cols <- colnames(mcmc$samples[[1]])
	for (chain in seq_len(n_chains)) {
	
		index <- match(params_from, cols)
		colnames(mcmc$samples[[chain]])[index] <- params_to
	
	}

	# rename summary
	if (any(names(mcmc) == 'summary')) {
	
		if (any(names(mcmc$summary) == 'chain1')) {
		
			rows <- rownames(mcmc$summary$chain1)
			for (chain in seq_len(n_chains)) {
			
				index <- match(params_from, rows)
				rownames(mcmc$summary[[paste0('chain', chain)]])[index] <- params_to
			
			}
		
		}

		if (any(names(mcmc$summary) == 'all.chains')) {

			rows <- rownames(mcmc$summary$all.chains)
			index <- match(params_from, rows)
			rownames(mcmc$summary$all.chains)[index] <- params_to
			
		}

	}
	mcmc

}

