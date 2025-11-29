#' Match variable names to results from an MCMC
#'
#' This function automates finding of indexed variable in an MCMC object. These include, for example, variables like `beta1`, `beta2`, `beta3`, etc. or variables like \code{beta[1]}, \code{beta[2]}, \code{beta[1]}, etc.
#'
#' @param param Name of the variable(s).
#' * `param = NULL`: All variables in the MCMC object. An object must be supplied to the `mcmc` argument.
#' * `param =` a character vector and any of `i`, `j`, `k`, and/or `l` are not `NULL`: Returns a vector of variable names dependent on which indices are not `NULL`. You define `i`, `j`, `k`, and/or `l` as numeric vectors. For example, `hammer_param('beta', i = 1:3)` will return `beta1`, `beta2`, and `beta3`. You can also define any index as `TRUE` or `FALSE` (they have the same effect), in which case the function will return the names of all variables in the `mcmc` object that match the given index.
#'
#' @inheritParams .ijkl
#' @inheritParams .mcmc
#'
#' @returns Character vector of variables.
#' @examples
#'
#' # Just making variable names:
#' param <- 'gamma'
#' hammer_param(param, i = 0:1)
#' hammer_param(param, j = 1:2)
#' hammer_param(param, i = 0:1, j = 1:2)
#' hammer_param(param, j = 1:2, k = 1:3)
#' hammer_param(param, i = 0:1, j = 1:2, k = 1:2)
#' hammer_param(param, j = 1:2, k = 1:2, l = 1:2)
#' hammer_param(param, i = 0:1, j = 1:2, k = 1:2, l = 1:2)
#'
#' # Getting variable names that are also in the MCMC object:
#' data(mcmc)
#' param <- 'beta'
#' hammer_param(param = NULL, mcmc = mcmc) # all variables
#' hammer_param(param, j = 3:4, mcmc = mcmc)
#'
#' # Fuzzy finding of indexed variables:
#' hammer_param('beta', i = TRUE, mcmc = mcmc) # none with names beta0, etc.
#' hammer_param('beta', j = TRUE, mcmc = mcmc)
#' hammer_param('z_hat', j = TRUE, k = 1:2, mcmc = mcmc)
#' hammer_param('z_hat', j = TRUE, k = TRUE, mcmc = mcmc)
#' 
#' @export
hammer_param <- function(
	param,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	mcmc = NULL
) {

	# for debugging
	if (FALSE) {

		param
		i <- NULL
		j <- NULL
		k <- NULL
		l <- NULL
		mcmc <- NULL

	}

	if (is.null(j) & (!is.null(k) | !is.null(l))) stop('Argument `j` cannot be NULL and k and/or l non-NULL.')

	if (is.null(mcmc)) {
		mcmc_samples <- NULL
	} else if (!inherits(mcmc, 'mcmc.list')) {
		mcmc_samples <- hammer_samples(mcmc, fail = TRUE)
		mcmc_samples <- coda::as.mcmc.list(mcmc_samples)
	} else {
		mcmc_samples <- mcmc
	}

	### get variable names
	if (is.null(param) & is.null(mcmc_samples)) {
		stop('Both `param` and `mcmc` are NULL. Please provide one or the other, or both.')
	} else if (is.null(param)) {

		out <- colnames(mcmc_samples[[1]])
	
	# parameters supplied, mcmc may be supplied
	} else {
		
		### multiple parameters?
		if (length(param) > 1L) {
		
			params <- param
			out <- character()
			for (this_param in params) {
				
				this_out <- hammer_param(
					param = this_param,
					i = i, j = j, k = k, l = l,
					mcmc = mcmc_samples
				)
				out <- c(out, this_out)
				
			}
		
		### just one parameter
		} else {

			# missing indices
			if (is.null(i) & is.null(j)) {
				out <- param
			# just i
			} else if (!is.null(i) & is.null(j)) {
				
				if (!is.null(mcmc_samples)) {

					if (is.logical(i)) {
						pattern <- paste0('^', param, '\\d+$')
					} else {
						pattern <- paste0(param, i)
					}

					cols <- colnames(mcmc_samples[[1]])
					if (length(pattern) > 1) pattern <- paste(pattern, collapse = '|')
					has <- grepl(x = cols, pattern = pattern)
					out <- cols[has]

				} else {
					out <- paste0(param, i)
				}
			
			# just j
			} else if (is.null(i) & !is.null(j) & is.null(k)) {

				if (!is.null(mcmc_samples)) {
				
					if (is.logical(j)) {
						pattern <- paste0('^', param, '\\[\\d+\\]$')
					} else {
						pattern <- paste0(param, '\\[', j, '\\]$')
					}

					cols <- colnames(mcmc_samples[[1]])
					if (length(pattern) > 1) pattern <- paste(pattern, collapse ='|')
					has <- grepl(x = cols, pattern = pattern)
					out <- cols[has]

				} else {
					out <- paste0(param, '[', j, ']')
				}

			# i and j
			} else if (!is.null(i) & !is.null(j) & is.null(k)) {
			
				if (!is.null(mcmc_samples)) {

					if (is.logical(i) & is.logical(j)) {
						pattern <- paste0('^', param, '\\d+\\[\\d+\\]$')
					} else if (!is.logical(i) & is.logical(j)) {
						pattern <- paste0('^', param, i, '[\\d+\\]$')
					} else if (is.logical(i) & !is.logical(j)) {
						pattern <- paste0('^', param, '\\d+\\[', j, '\\]$')
					} else {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = j, brack2 = '\\]', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					}

					cols <- colnames(mcmc_samples[[1]])
					if (length(pattern) > 1) pattern <- paste(pattern, collapse ='|')
					has <- grepl(x = cols, pattern = pattern)
					out <- cols[has]

				} else {
					pattern <- expand.grid(param = param, i = i, brack1 = '[', j = j, brack2 = ']', stringsAsFactors = FALSE)
					out <- .grid_to_vector(pattern)
				}

			# not i but j and k
			} else if (is.null(i) & !is.null(j) & !is.null(k) & is.null(l)) {
			
				if (!is.null(mcmc_samples)) {
				
					if (is.logical(j) & is.logical(k)) {
						pattern <- paste0('^', param, '\\[\\d+, \\d+\\]$')
					} else if (!is.logical(j) & is.logical(k)) {
						pattern <- paste0('^', param, '\\[', j, ', \\d+\\]$')
					} else if (is.logical(j) & !is.logical(k)) {
						pattern <- paste0('^', param, '\\[\\d+, ', k, '\\]$')
					} else {
						
						pattern <- expand.grid(param = param, brack1 = '\\[', j = j, comma = ', ', k = k, brack2 = ']$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)

					}

					cols <- colnames(mcmc_samples[[1]])
					if (length(pattern) > 1) pattern <- paste(pattern, collapse ='|')
					has <- grepl(x = cols, pattern = pattern)
					out <- cols[has]

				} else {
					pattern <- expand.grid(param = param, brack1 = '[', j = j, comma = ', ', k = k, brack2 = ']', stringsAsFactors = FALSE)
					out <- .grid_to_vector(pattern)
				}

			# i, j, and k
			} else if (!is.null(i) & !is.null(j) & !is.null(k) & is.null(l)) {
				
				if (!is.null(mcmc_samples)) {
				
					if (is.logical(i) & is.logical(j) & is.logical(k)) {
						pattern <- paste0('^', param, '\\d+\\[\\d+, \\d+\\]$')
					} else if (!is.logical(i) & is.logical(j) & is.logical(k)) {
						pattern <- paste0('^', param, i, '\\[\\d+, \\d+\\]$')
					} else if (is.logical(i) & !is.logical(j) & is.logical(k)) {
						pattern <- paste0('^', param, '\\d+\\[', j, ', \\d+\\]$')
					} else if (!is.logical(i) & is.logical(j) & !is.logical(k)) {
						pattern <- paste0('^', param, '\\d+\\[\\d+, ', k, '\\]$')
					} else if (!is.logical(i) & !is.logical(j) & is.logical(k)) {
						pattern <- expand.grid(hat = '^', param = param, i = i, brack1 = '\\[', j = j, comma = ', ', k = '\\d+', remainder = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (is.logical(i) & !is.logical(j) & !is.logical(k)) {
						pattern <- expand.grid(hat = '^', param = param, i = '\\d+', brack1 = '\\[', j = j, comma = ', ', k = k, remainder = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & is.logical(j) & !is.logical(k)) {
						pattern <- expand.grid(hat = '^', param = param, i = i, brack1 = '\\[', j = '\\d+', comma = ', ', k = k, remainder = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & !is.logical(j) & !is.logical(k)) {
						pattern <- expand.grid(hat = '^', param = param, i = i, brack1 = '\\[', j = j, comma = ', ', k = k, remainder = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					}

					cols <- colnames(mcmc_samples[[1]])
					if (length(pattern) > 1) pattern <- paste(pattern, collapse ='|')
					has <- grepl(x = cols, pattern = pattern)
					out <- cols[has]

				} else {
					pattern <- expand.grid(param = param, i = i, brack1 = '[', j = j, comma = ', ', k = k, brack2 = ']', stringsAsFactors = FALSE)
					out <- .grid_to_vector(pattern)
				}

			# not i, but j, k, l
			} else if (is.null(i) & !is.null(j) & !is.null(k) & !is.null(l)) {
				
				if (!is.null(mcmc_samples)) {
				
					if (is.logical(j) & is.logical(k) & is.logical(l)) {
						pattern <- paste0('^', param, '\\[\\d+, \\d+, \\d+\\]$')
					} else if (!is.logical(j) & is.logical(k) & is.logical(l)) {
						pattern <- paste0('^', param, '\\[', j, ', \\d+, \\d+\\]$')
					} else if (is.logical(j) & !is.logical(k) & is.logical(l)) {
						pattern <- paste0('^', param, '\\[\\d+, ', k = k, ', \\d+\\]$')
					} else if (is.logical(j) & is.logical(k) & !is.logical(l)) {
						pattern <- paste0('^', param, '\\[\\d+, \\d+, ', l, '\\]$')
					} else if (!is.logical(j) & !is.logical(k) & is.logical(l)) {
						pattern <- expand.grid(param = param, brack1 = '\\[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = '\\d+', brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(j) & is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, brack1 = '\\[', j = j, comma1 = ', ', k = '\\d+', comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (is.logical(j) & !is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, brack1 = '\\[', j = '\\d+', comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(j) & !is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, brack1 = '\\[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					}

					cols <- colnames(mcmc_samples[[1]])
					if (length(pattern) > 1) pattern <- paste(pattern, collapse ='|')
					has <- grepl(x = cols, pattern = pattern)
					out <- cols[has]

				} else {
					pattern <- expand.grid(param = param, brack1 = '[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = ']', stringsAsFactors = FALSE)
					out <- .grid_to_vector(pattern)
				}

			# all of i, j, k, l
			} else if (!is.null(i) & !is.null(j) & !is.null(k) & !is.null(l)) {
				
				if (!is.null(mcmc_samples)) {
				
					if (is.logical(i) & is.logical(j) & is.logical(k) & is.logical(l)) {
						pattern <- paste0('^', param, '\\d+\\[\\d+, \\d+, \\d+\\]$')
					} else if (!is.logical(i) & is.logical(j) & is.logical(k) & is.logical(l)) {
						pattern <- paste0('^', param, i, '\\[\\d+, \\d+, \\d+\\]$')
					} else if (is.logical(i) & !is.logical(j) & is.logical(k) & is.logical(l)) {
						pattern <- paste0('^', param, '\\d+\\[', j, ', \\d+, \\d+\\]$')
					} else if (is.logical(i) & is.logical(j) & !is.logical(k) & is.logical(l)) {
						pattern <- paste0('^', param, '\\d+\\[\\d+, ', k, ', \\d+\\]$')
					} else if (is.logical(i) & is.logical(j) & is.logical(k) & !is.logical(l)) {
						pattern <- paste0('^', param, '\\d+\\[\\d+, \\d+, ', l, '\\]$')
					
					} else if (!is.logical(i) & !is.logical(j) & is.logical(k) & is.logical(l)) {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = j, comma1 = ', ', k = '\\d+', comma2 = ', ', l = '\\d+', brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & is.logical(j) & !is.logical(k) & is.logical(l)) {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = '\\d+', comma1 = ', ', k = k, comma2 = ', ', l = '\\d+', brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & is.logical(j) & is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = '\\d+', comma1 = ', ', k = '\\d+', comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (is.logical(i) & !is.logical(j) & !is.logical(k) & is.logical(l)) {
						pattern <- expand.grid(param = param, i = '\\d+', brack1 = '\\[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = '\\d+', brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (is.logical(i) & !is.logical(j) & is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, i = '\\d+', brack1 = '\\[', j = j, comma1 = ', ', k = '\\d+', comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (is.logical(i) & is.logical(j) & !is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, i = '\\d+', brack1 = '\\[', j = '\\d+', comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (is.logical(i) & !is.logical(j) & !is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, i = '\\d+', brack1 = '\\[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & is.logical(j) & !is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = '\\d+', comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & !is.logical(j) & is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = j, comma1 = ', ', k = '\\d+', comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & !is.logical(j) & !is.logical(k) & is.logical(l)) {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = '\\d+', brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					} else if (!is.logical(i) & !is.logical(j) & !is.logical(k) & !is.logical(l)) {
						pattern <- expand.grid(param = param, i = i, brack1 = '\\[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = '\\]$', stringsAsFactors = FALSE)
						pattern <- .grid_to_vector(pattern)
					}

					cols <- colnames(mcmc_samples[[1]])
					if (length(pattern) > 1) pattern <- paste(pattern, collapse ='|')
					has <- grepl(x = cols, pattern = pattern)
					out <- cols[has]

				} else {
					pattern <- expand.grid(param = param, i, brack1 = '[', j = j, comma1 = ', ', k = k, comma2 = ', ', l = l, brack2 = ']', stringsAsFactors = FALSE)
					out <- .grid_to_vector(pattern)
				}

			}

		} # if just one parameter
			
	} # if user-specified column names
	out
	
}

### functions for combining rows of "pattern" into character vector without undesired spaces
.paste0_fx <- function(...) { paste0(...) }
.grid_to_vector <- function(pattern) {
	args <- as.list(pattern)
	do.call(.paste0_fx, args = args)
}