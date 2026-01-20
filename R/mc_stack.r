#' Combine chains into one matrix
#'
#' [rbind()]s chains onto one matrix.
#' 
#' @inheritParams .mcmc
#'
#' @returns A `matrix`.
#'
#' @examples
#'
#' data(mcmc)
#'
#' stacked <- mc_stack(mcmc)
#' dim(stacked)
#' head(stacked)
#' tail(stacked)
#' 
#' @export mc_stack
mc_stack <- function(mcmc) {

	# if (FALSE) {

	# 	# param
	# 	# i <- NULL
	# 	# j <- NULL
	# 	# k <- NULL
	# 	# l <- NULL
	# 	# indices <- NULL

	# }

	nc <- mc_n_chains(mcmc)	
	for (i in seq_len(nc)) {
	
		this_out <- mcmc$samples[[i]]
		if (exists('out', inherits = FALSE)) {
			out <- rbind(out, this_out)
		} else {
			out <- this_out
		}
	
	}

	out
	
}

