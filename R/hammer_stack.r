#' Combine chains into one matrix
#'
#' [rbind()]s chains onto one matrix.
#' 
#' @inheritParams .mcmc
#'
#' @param param Character vector: Name of the variable(s).
#'
#' @inheritParams .ijkl
#' @inheritParams .indices
#'
#' @returns A `matrix`.
#'
#' @examples
#'
#' data(mcmc)
#'
#' stacked <- hammer_stack(mcmc, param = 'beta', j = 1:2)
#' dim(stacked)
#' head(stacked)
#' tail(stacked)
#' 
#' params <- c('beta', 'z_hat')
#' indices <- list(list(j = 5:7), list(j = 1:2, k = 1:3))
#' stacked <- hammer_stack(mcmc, param = params, indices = indices)
#' dim(stacked)
#' colnames(stacked)
#' head(stacked)
#' tail(stacked)
#'
#' @export hammer_stack
hammer_stack <- function(mcmc, param, i = NULL, j = NULL, k = NULL, l = NULL, indices = NULL) {

	if (FALSE) {

		param
		i <- NULL
		j <- NULL
		k <- NULL
		l <- NULL
		indices <- NULL

	}

	if (any(c(!is.null(i), !is.null(j), !is.null(k), !is.null(l))) & !is.null(indices)) stop('You cannot use `i`, `j`, `k`, or `l` *and* `indices`.')

	if (!is.null(indices)) {
	
		for (count_param in seq_along(param)) {
		
			args <- list(mcmc = mcmc, param = param[count_param])
			args <- c(args, indices[[count_param]])
			this_out <- do.call(hammer_stack, args = args)

			if (count_param == 1) {
				out <- this_out
			} else {
				out <- cbind(out, this_out)
			}
		
		}
		return(out)
	
	}

	params <- hammer_param(param, i = i, j = j, k = k, l = l, mcmc = mcmc)
	selected <- hammer_subset(mcmc = mcmc, param = param, i = i, j = j, k = k, l = l, indices = NULL)
	nc <- hammer_n_chains(mcmc)
	
	for (i in seq_len(nc)) {
	
		this_out <- selected$samples[[i]]
		if (exists('out', inherits = FALSE)) {
			out <- rbind(out, this_out)
		} else {
			out <- this_out
		}
	
	}

	out
	
}

