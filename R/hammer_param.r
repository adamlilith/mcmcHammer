#' Match variable names to results from an MCMC
#'
#' This function automates finding of indexed variable in an MCMC object. These include, for example, variables like `beta1`, `beta2`, `beta3`, etc. or variables like `beta[1]`, `beta[2]`, `beta[1]`, etc. Only variable named in the MCMC object are returned.
#'
#' @param param Name of the variable(s). The outcome depends on the definitions of `i`, `j`, `k`, and `l`.
#' * `param = NULL`: All variables in the MCMC object. An object must be supplied to the `mcmc` argument.
#' * `param =` a character vector and `i` is a numeric vector: Variables with the pattern "`param*`" where `*` is `i`. For example: `beta1`, `beta2`, and `beta3`.
#' * `param =` a character vector and `i` is `TRUE`: Finds in the MCMC object all variables with the pattern "`param*`" where `*` is numeric. You must supply `mcmc` in this case.
#'     * `param =` a character vector and `j` is a numeric vector, but `k` is `NULL`: Variables with the pattern "`param[j]`".
#'     * `param =` a character vector and `j` is `TRUE`, but `k` is `NULL`: Variables with the pattern "`param[j]`". You must supply `mcmc` in this case.
#'     * `param =` a character vector and `j` and `k` are a numeric vectors but `l` is `NULL`: Variables with the pattern "`beta[j, k]`".
#'     * `param =` a character vector and `j` and/or `k` are `TRUE` but `l` is `NULL`: Variables with the pattern "`beta[*, k]`", "`beta[j, *]`", or "`beta[*, *]`" (depending on values of `j` and `k`. You must supply `mcmc` in this case.
#'     * `param =` a character vector and `j`, `k`, and `l` are a numeric vectors: Variables with the pattern "`beta[j, k, l]`".
#'     * `param =` a character vector and one or more of `j`, `k`, and `l` are a numeric vectors and/or `TRUE`: Variables with the pattern "`beta[*, k, l]`", "`beta[j, *, l]`", "`beta[j, k, *]`", "`beta[*, *, l]`", "`beta[*, k, *]`",  "`beta[j, *, *]`", or "`beta[*, *, *]`". You must supply `mcmc` in this case.
#'
#' @param i,j,k,l Indices used to specify variable names. Please see the help for [hammer_param()].
#' @param mcmc	An object of class `mcmc` or `mcmc.list`, *or* a `list`. If a `list`, the function searches down the first element to see if it can find an `mcmc` or `mcmc.list` object, then uses this if it can.
#'
#' @returns Character vector of variables.
#' @examples
#'
#' # Just making variable names:
#' param <- 'gamma'
#' hammer_param(param)
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
#' hammer_param('gamma', j = TRUE, k = 1:2, mcmc = mcmc)
#' hammer_param('gamma', j = TRUE, k = 1:2, mcmc = mcmc)
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

	if (!inherits(mcmc, 'mcmc.list')) {
		mcmc_samples <- hammer_samples(mcmc, fail = TRUE)
	} else {
		mcmc_samples <- mcmc
	}

	### get variable names
	if (is.null(param)) {

		if (!is.null(mcmc_samples))	{
			param <- colnames(mcmc_samples[[1]])
		} else {
			stop('Both `param` and `mcmc` are NULL. Please provide one or the other, or both.')
		}
		
	} else {
		
		### multiple parameters?
		if (length(param) > 1L) {
		
			params_base <- param
			param <- character()
			for (param_base in params_base) {
				
				param_this <- hammer_param(
					param = param_base,
					i = i, j = j, k = k, l = l,
					mcmc = mcmc_samples
				)
				
				param <- c(param, param_this)
				
			}
		
		### just one parameter
		} else {
			
			if (is.logical(j) & is.logical(k)) {
				warning('Extracting variable names can take a long time if at least two of `j`, `k`, and `l` are logical. Consider using numeric indices instead.')
			}

			### get indices
			i <- .get_indices(id = i, mcmc_samples = mcmc_samples)
			j <- .get_indices(id = j, mcmc_samples = mcmc_samples)
			k <- .get_indices(id = k, mcmc_samples = mcmc_samples)
			l <- .get_indices(id = l, mcmc_samples = mcmc_samples)
			
			len_i <- length(i)
			len_j <- length(j)
			len_k <- length(k)
			len_l <- length(l)
			
			### get candidate variable names
			if (!is.null(i)) param <- paste0(rep(param, len_i), i)

			if (!is.null(j) & is.null(k)) {
			
				indices <- expand.grid(param = param, j = j)
				params <- paste0(indices$param, '[', indices$j, ']')
					
			} else if (!is.null(j) & !is.null(k) & is.null(l)) {
				
				indices <- expand.grid(param = param, j = j, k = k)
				params <- paste0(indices$param, '[', indices$j, ', ', indices$k, ']')

			} else if (!is.null(j) & !is.null(k) & !is.null(l)) {

				indices <- expand.grid(param = param, j = j, k = k, l = l)
				params <- paste0(indices$param, '[', indices$j, ', ', indices$k, ', ', indices$l, ']')
			
			}
			
		} # if just one parameter
			
	} # if user-specified column names
	
	# valid names?
	if (!is.null(mcmc_samples)) {
		cols <- colnames(mcmc_samples[[1]])
		if (length(mcmc_samples) > 0) {
			params <- params[params %in% cols]
		} else {
			params <- params[params %in% cols]
		}
	}
	
	params
	
}

### get indices
.get_indices <- function(id, mcmc_samples) {

	# id		i, j, k, or l
	# mcmc_samples		MCMC matrix

	if (!is.null(id)) {
		if (inherits(id, 'logical')) {
			if (id) {
				if (inherits(mcmc_samples, 'mcmc.list')) {
					id <- 0L:ncol(mcmc_samples[[1]])
				} else {
					id <- 0L:ncol(mcmc_samples)
				}
			} else {
				id <- NULL
			}
		}
	}
	id
	
}
