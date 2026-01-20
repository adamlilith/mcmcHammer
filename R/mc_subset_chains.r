#' Select specific chains in an MCMC object
#'
#' @description Select specific chains in an MCMC object
#' 
#' @inheritParams .mcmc
#' @param x Numeric vector of chains.
#' @param keep Logical: If `TRUE` (default), chains named by `x` are retained. If `FALSE`, they are discarded.
#' @param collapse Logical: If `TRUE` (default), and just one chain is returned, then "collapse" the MCMC object (e.g., so it is structured just as `mcmc$samples` and `mcmc$summary`, versus the "uncollapsed" case where we would have `mcmc$samples$chain1`, and `mcmc$summary$chain1` and `mcmc$summary$all.chains`).
#' @param na.rm Logical: If `TRUE`, ignore `NA` values when re-summarizing retained chains. Default if `FALSE`.
#'
#' @returns An `mcmc` matrix, an `mcmc.list`, or  a list of list, one of which is an `mcmc.list`.
#'
#' @examples
#'
#' data(mcmc)
#' 
#' # retaining just one chain and collapsing
#' collapse <- mc_subset_chains(mcmc, 1)
#' str(collapse)
#'
#' # retaining just one chain but not collapsing
#' no_collapse <- mc_subset_chains(mcmc, 1, collapse = FALSE)
#' str(no_collapse)
#'
#' # discarding
#' disc <- mc_subset_chains(mcmc, 2, keep = FALSE)
#' str(disc)
#'
#' @export
mc_subset_chains <- function(
	mcmc,
	x,
	keep = TRUE,
	collapse = TRUE,
	na.rm = FALSE
) {

	if (FALSE) {

		mcmc
		x <- 1:2
		keep <- TRUE
		collapse <- TRUE

	}

	nc <- mc_n_chains(mcmc)
	if (all(1:nc %in% x) & !keep) {
		if (!keep) {
			return(NULL)
		} else {
			return(mcmc)
		}
	}

	if (!keep) x <- {1:nc}[-x]

	samples <- mcmc$samples[x]
	n_new_chains <- mc_n_chains(samples)
	names(samples) <- paste0('chain', 1:n_new_chains)
	out <- mc_resummarize(samples, na.rm = na.rm)

	# collapse
	if (length(x) == 1 & collapse) {
	
		out <- list(
			samples = out$samples$chain1,
			summary = out$summary$chain1
		)
	
	}

	out

}
