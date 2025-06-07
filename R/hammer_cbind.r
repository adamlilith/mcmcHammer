#' Combine multiple MCMC chains with different monitored variables
#'
#' This function takes an `mcmc.list` and `cbind()`'s' them onto one another to create an `mcmc.list` that has all the columns of the inputs. It also combines the summary statistics.
#'
#' @param ... Two or more objects of class `mcmc` or `mcmc.list``, or a `list` with the first element being an `mcmc.list`. All inputs must be of the same class (e.g., you can't combine an `mcmc` object with an `mcmc.list`).
#'
#' @param summarize Logical: If `TRUE` (default), and the input is a set of lists, then calculate summary statistics.
#'
#' @returns An `mcmc` object, an `mcmc.list`, or a `list`.
#'
#' @examples
#' 
#' data(mcmc)
#' cbinded <- hammer_rbind(mcmc)
#' head(cbinded)
#' dim(cbinded)
#'
#' @rdname hammer_cbind
#' @export
hammer_cbind <- function(..., summarize = TRUE) {

	mcmcs <- list(...)
	out <- mcmcs[[1]]
	if (inherits(out, 'mcmc')) {
		format <- 'mcmc'
	} else if (inherits(out, 'mcmc.list')) {
		format <- 'mcmc.list'
	} else if (inherits(out, 'list')) {
		format <- 'list'
		if (any(names(out) == 'WAIC')) out$WAIC <- NULL
	}

	if (length(mcmcs) > 1) {
	
		classes <- sapply(mcmcs, class)
		if (any(classes[1] != classes[2:length(mcmcs)])) stop('Inputs must all be of the same type (mcmc, mcmc.list, or a list with an mcmc.list as the first element.)')

		# for each input
		for (i in 2:length(mcmcs)) {
		
			this <- mcmcs[[i]]
			if (format == 'mcmc') {

				if (nrow(out) != nrow(this)) stop('Input ', i, ' does not have same number of rows as preceding inputs.')

				this_names <- colnames(this)
				out_names <- colnames(out)
				if (any(this_names %in% out_names)) stop('Duplicated names in input ', i, '.')

				out <- cbind(out, this)

			} else if (format ==  'mcmc.list') {
				
				if (length(out) != length(this)) stop('Input ', i, ' does not have the same number of chains as the preceding inputs.')
				
				for (chain in seq_along(out)) {

					if (nrow(out[[chain]]) != nrow(this[[chain]])) stop('Input ', i, ', chain ', chain, ' does not have same number of rows as preceding inputs.')

					this_names <- colnames(this[[chain]])
					out_names <- colnames(out[[chain]])
					if (any(this_names %in% out_names)) stop('Duplicated names in item ', i, ', chain ', chain, '.')

					out[[chain]] <- cbind(out[[chain]], this[[chain]])

				}

			} else if (format == 'list') {

				if (length(out[[1]]) != length(this[[1]])) stop('Input ', i, ' does not have the same number of chains as the preceding inputs.')
				
				for (chain in seq_along(out[[1]])) {

					if (nrow(out[[1]][[chain]]) != nrow(this[[1]][[chain]])) stop('Input ', i, ' does not have same number of rows as preceding inputs.')

					this_names <- colnames(this[[1]][[chain]])
					out_names <- colnames(out[[1]][[chain]])
					if (any(this_names %in% out_names)) stop('Duplicated names in item ', i, ', chain ', chain, '.')

					out[[1]][[chain]] <- cbind(out[[1]][[chain]], this[[1]][[chain]])

				}

			}		
		
		} # next input
	
		if (summarize & format == 'list') {
			if (any(names(out) == 'summary')) {
				out$summary <- NULL
				out <- hammer_resummarize(out)
			}
		} else if (format == 'list') {
			if (any(names(out) == 'summary')) out$summary <- NULL
		}

	}
	out

}
