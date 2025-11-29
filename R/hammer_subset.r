#' Subset MCMC chains to specific parameters
#'
#' @description Often, MCMC chains contain columns for parameters that we do not want to examine for a particular purpose. This function subsets MCMC chains to a specific set.
#' 
#' @inheritParams .mcmc
#'
#' @param param Character vector: Name of the variable(s).
#'
#' @inheritParams .ijkl
#' @inheritParams .indices
#'
#' @param keep Logical: If `TRUE` (default), columns with names in `param` are retained. If `FALSE`, they are discarded. This cannot be set to `FALSE` when the `indices` argument is used.
#'
#' @param na.rm Logical: If `FALSE` (default), then summary statistics will be `NA` if any value in any iteration is `NA` (and it probably should be `NA`). However, if you set this to `TRUE`, then `NA` values are ignored.
#'
#' @returns An `mcmc` matrix, an `mcmc.list`, or  a list of list, one of which is an `mcmc.list`.
#'
#' @examples
#'
#' data(mcmc)
#' 
#' # simple subset
#' param  <- 'alpha'
#' simple <- hammer_subset(mcmc, param, i = TRUE)
#' head(simple)
#'
#' # subsetting with different indices for each parameter
#' indices <- list(list(i = TRUE), list(j = TRUE))
#' params <- c('alpha', 'beta')
#' complex <- hammer_subset(mcmc, param = params, indices = indices)
#' head(complex)
#'
#' @export
hammer_subset <- function(
	mcmc,
	param,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	indices = NULL,
	keep = TRUE,
	na.rm = FALSE
) {

	if (FALSE) {

		mcmc
		param
		i <- NULL
		j <- NULL
		k <- NULL
		l <- NULL
		indices <- NULL
		keep <- TRUE

	}

	if (any(c(!is.null(i), !is.null(j), !is.null(k), !is.null(l))) & !is.null(indices)) stop('You cannot use `i`, `j`, `k`, or `l` *and* `indices`.')

	### using `indices`
	if (!is.null(indices)) {
	
		if (!keep) stop('The value of `keep` cannot be `FALSE` when `indices` is used.')
		for (count_param in seq_along(param)) {
		
			args <- list(mcmc = mcmc, param = param[count_param], keep = keep)
			args <- c(args, indices[[count_param]])
			subsetted <- do.call(hammer_subset, args = args)

			if (count_param == 1) {
				out <- subsetted
			} else {
				out <- hammer_cbind(out, subsetted)
			}
		
		}
		return(out)
	
	}

	mcmc_samples <- hammer_samples(mcmc)
	mcmc_summaries <- hammer_summaries(mcmc, fail = FALSE)

	if (!inherits(mcmc_samples, 'mcmc.list')) mcmc_samples <- as.mcmc.list(mcmc_samples)

	params <- hammer_param(param, i = i, j = j, k = k, l = l, mcmc = mcmc)

	n_chains <- hammer_n_chains(mcmc_samples)
	cnames <- colnames(mcmc_samples[[1]])
	for (n_chain in 1:n_chains) {
		if (keep) {

			this_chain <- if (inherits(mcmc_samples, c('mcmc.list', 'list'))) {
				mcmc_samples[[n_chain]]
			} else if (inherits(mcmc_samples, 'mcmc')) {
				mcmc_samples
			}
			this_chain <- as.matrix(this_chain)
			this_chain <- this_chain[ , params, drop = FALSE]

		} else {
		
			this_chain <- mcmc_samples[[n_chain]]
			this_chain <- as.matrix(this_chain)
			this_chain <- this_chain[ , !(cnames %in% params), drop = FALSE]

		}
		this_chain <- coda::as.mcmc(this_chain)
		mcmc_samples[[n_chain]] <- this_chain
	}

	if (!is.null(mcmc_summaries)) {
		mcmc_summaries <- hammer_resummarize(mcmc_samples, na.rm = na.rm)
		# n <- length(mcmc_summaries)
		# cnames <- colnames(mcmc_samples[[1]])
		# for (n_chain in 1:n) {
		# 	if (keep) {
		# 		mcmc_summaries[[n_chain]] <- mcmc_summaries[[n_chain]][params, , drop = FALSE]
		# 	} else {
		# 		mcmc_summaries[[n_chain]] <- mcmc_summaries[[n_chain]][!(cnames %in% params), , drop = FALSE]
		# 	}
		# }
	}

	list(
		samples = mcmc_samples,
		summary = mcmc_summaries
	)

}
