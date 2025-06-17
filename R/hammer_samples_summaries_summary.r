#' Extracts a "samples" mcmc.list from a list, or the "summary" of an mcmc.list
#'
#' These functions extract the `samples` or `summary` part of a list object:
#' * `hammer_samples()`: Extracts a list of MCMC samples.
#' * `hammer_summaries()`: Extracts a list of MCMC summary matrices.
#' * `hammer_summary()`: Extracts the "all-chains" summary matrix.
#' 
#' @inheritParams .mcmc
#' @param fail Logical: If `TRUE` (default), and the object being searched for cannot be found, then fail with an error. If `FALSE`, then return `NULL`.
#'
#' @returns An `mcmc.list` object or a `list`.
#'
#' @examples
#'
#' data(mcmc)
#' str(hammer_samples(mcmc))
#' str(hammer_summaries(mcmc))
#' hammer_summary(mcmc)
#'
#' @export hammer_samples
hammer_samples <- function(mcmc, fail = TRUE) {

	if (inherits(mcmc, 'mcmc')) {
		out <- list(chain1 = mcmc)
	} else if (inherits(mcmc, 'mcmc.list')) {
		out <- mcmc
	} else {
	
		n <- length(mcmc)
		if (n == 0) {
			if (fail) {
				stop('No object of class `mcmc` or `mcmc.list` found.')
			} else {
				return(out)
			}
		}

		is_mcmc_list <- FALSE
		i <- 1
		while (!is_mcmc_list & i <= n) {
		
			if (inherits(mcmc[[i]], c('mcmc', 'mcmc.list'))) {
				out <- mcmc[[i]]
				is_mcmc_list <- TRUE
			}
			i <- i + 1
			
		}

		if (!is_mcmc_list) {
			if (fail) {
				stop('No object of class `mcmc.list` found.')
			} else {
				return(out)
			}
		}
	
	}
	out
	
}

#' @describeIn hammer_samples Extracts a "samples" list from a list
#' @aliases hammer_summaries
#' @export
hammer_summaries <- function(mcmc, fail = TRUE) {

	n_elements <- length(mcmc)
	if (n_elements == 0) {
		if (fail) {
			stop('Object appears to have no `summary` element.')
		} else {
			return(NULL)
		}
	}

	found_summary <- FALSE
	i <- 1
	while (!found_summary & i <= n_elements) {
	
		if (names(mcmc)[i] == 'summary') {
			out <- mcmc[[i]]
			found_summary <- TRUE
		}
		i <- i + 1
	
	}
	if (!found_summary) {
		if (fail) {
			stop('Object appears to have no `summary` element.')
		} else {
			return(NULL)
		}
	}
	out
	
}

#' @describeIn hammer_samples Extracts a grand "summary" matrix from a list
#' @aliases hammer_summary
#' @export
hammer_summary <- function(mcmc, fail = TRUE) {

	n_elements <- length(mcmc)
	if (n_elements == 0) {
		if (fail) {
			stop('Object appears to have no `summary` element.')
		} else {
			return(NULL)
		}
	}

	found_summary <- FALSE
	i <- 1
	while (!found_summary & i <= n_elements) {
	
		if (names(mcmc)[i] == 'summary') {
			if (any(names(mcmc[[i]]) == 'all.chains')) {
				out <- mcmc[[i]]$all.chains
				found_summary <- TRUE
			}
		}
		i <- i + 1
	
	}

	if (!found_summary) {
		if (fail) {
			stop('Object appears to have no `summary` element.')
		} else {
			return(NULL)
		}
	}
	out
	
}

