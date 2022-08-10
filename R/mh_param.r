#' Match variable names to results from an MCMC
#'
#' This function automates finding of indexed variable in an MCMC object. These include, for example, variables like \code{beta1}, \code{beta2}, \code{beta3}, etc. or variables like \code{beta[1]}, \code{beta[2]}, \code{beta[1]}, etc. Only variable named in the MCMC object are returned.
#'
#' @param param Name of the variable(s). The outcome depends on the definitions of \code{i}, \code{j}, \code{k}, and \code{l}.
#' \itemize{
#' 		\item \code{param = NULL}: All variables in the MCMC object. An object must be supplied to the \code{mcmc} argument.
#' 		\item \code{param =} a character vector and \code{i} is a numeric vector: Variables with the pattern "\code{param*}" where \code{*} is \code{i}. For example: \code{beta1}, \code{beta2}, and \code{beta3}.
#' 		\item \code{param =} a character vector and \code{i} is \code{TRUE}: Finds in the MCMC object all variables with the pattern "\code{param*}" where \code{*} is numeric. You must supply \code{mcmc} in this case.
#'		\item \code{param =} a character vector and \code{j} is a numeric vector, but \code{k} is \code{NULL}: Variables with the pattern "\code{param[j]}".
#'		\item \code{param =} a character vector and \code{j} is \code{TRUE}, but \code{k} is \code{NULL}: Variables with the pattern "\code{param[j]}". You must supply \code{mcmc} in this case.
#'		\item \code{param =} a character vector and \code{j} and \code{k} are a numeric vectors but \code{l} is \code{NULL}: Variables with the pattern "\code{beta[j, k]}".
#'		\item \code{param =} a character vector and \code{j} and/or \code{k} are \code{TRUE} but \code{l} is \code{NULL}: Variables with the pattern "\code{beta[*, k]}", "\code{beta[j, *]}", or "\code{beta[*, *]}" (depending on values of \code{j} and \code{k}. You must supply \code{mcmc} in this case.
#'		\item \code{param =} a character vector and \code{j}, \code{k}, and \code{l} are a numeric vectors: Variables with the pattern "\code{beta[j, k, l]}".
#'		\item \code{param =} a character vector and one or more of \code{j}, \code{k}, and \code{l} are a numeric vectors and/or \code{TRUE}: Variables with the pattern "\code{beta[*, k, l]}", "\code{beta[j, *, l]}", "\code{beta[j, k, *]}", "\code{beta[*, *, l]}", "\code{beta[*, k, *]}",  "\code{beta[j, *, *]}", or "\code{beta[*, *, *]}". You must supply \code{mcmc} in this case.
#' } 
#' @param i,j,k,l Indices used to specify variable names. Please see the help for \code{\link{mh_param}}.
#' @param mcmc	An "stacked" set of MCMC chains, \emph{or} an object of class \code{mcmc} or \code{mcmc.list}, \emph{or} a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param stacked \code{FALSE}, in which case \code{mcmc} is assumed to be an object of class \code{mcmc}, \code{mcmc.list}, or a \code{list}, or \code{TRUE} (default), in which case it is a "stacked" MCMC table. This argument is usually used by other functions in this package, so can often be ignored. However, if your MCMC chains have a lot of iterations or variables, then you can speed things up by "stacking" the chains using \code{\link{mh_stack}}, then using that for \code{mcmc}.
#'
#' @return Character vector of variables.
#' 
#' data(mcmc)
#'
#' # Just getting variable names:
#' param <- 'beta'
#' mh_param(param)
#' mh_param(param, i=0:1)
#' mh_param(param, j=1:2)
#' mh_param(param, i=0:1, j=1:2)
#' mh_param(param, j=1:2, k=1:3)
#' mh_param(param, i=0:1, j=1:2, k=1:2)
#' mh_param(param, j=1:2, k=1:2, l=1:2)
#' mh_param(param, i=0:1, j=1:2, k=1:2, l=1:2)
#'
#' # Getting variable names that are also in the MCMC object:
#' param <- 'beta'
#' mh_param(param, mcmc=mcmc)
#' mh_param(param, i=0:1, mcmc=mcmc)
#' mh_param(param, j=1:2, mcmc=mcmc)
#' mh_param(param, i=0:1, j=1:2, mcmc=mcmc)
#' mh_param(param, j=1:2, k=1:3, mcmc=mcmc)
#' mh_param(param, i=0:1, j=1:2, k=1:2, mcmc=mcmc)
#' mh_param(param, j=1:2, k=1:2, l=1:2, mcmc=mcmc)
#' mh_param(param, i=0:1, j=1:2, k=1:2, l=1:2, mcmc=mcmc)
#'
#' # Fuzzy finding of indexed variables:
#' mh_param(param, i=TRUE, mcmc=mcmc)
#' mh_param(param, j=TRUE, mcmc=mcmc)
#' mh_param(param, j=TRUE, k=1:2, mcmc=mcmc)
#' mh_param(param, j=TRUE, k=1:2, mcmc=mcmc)
#' 
#' @export

mh_param <- function(
	param,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	mcmc = NULL,
	stacked = TRUE
) {

	### stack MCMC object
	if (!is.null(mcmc)) {
		if (!stacked) {
			mcmc <- mh_stack(mcmc)
			stacked <- TRUE
		}
	}

	### get variable names
	if (is.null(param)) {

		if (is.null(mcmc)) stop ('If "param" is not specified, you need to supply a "stacked" MCMC object to argument "mcmc".')
		param <- colnames(mcmc)
		
	} else {
		
		### multiple parameters?
		if (length(param) > 1L) {
		
			params_base <- param
			param <- character()
			for (param_base in params_base) {
				
				param_this <- mh_param(
					param = param_base,
					i = i, j = j, k = k, l = l,
					mcmc = mcmc, stacked = stacked
				)
				
				param <- c(param, param_this)
				
			}
		
		### just one parameter
		} else {
			
			
			### get indices
			i <- .get_indices(id = i, mcmc = mcmc)
			j <- .get_indices(id = j, mcmc = mcmc)
			k <- .get_indices(id = k, mcmc = mcmc)
			l <- .get_indices(id = l, mcmc = mcmc)
			
			len_i <- length(i)
			len_j <- length(j)
			len_k <- length(k)
			len_l <- length(l)
			
			### get candidate variable names
			if (!is.null(i)) param <-
				paste0(rep(param, len_i), i)

			if (!is.null(j) & is.null(k)) {
			
				indices <- expand.grid(param = param, j = j)
				param <- paste0(indices$param, '[', indices$j, ']')
					
			} else if (!is.null(j) & !is.null(k) & is.null(l)) {
				
				indices <- expand.grid(param = param, j = j, k = k)
				param <- paste0(indices$param, '[', indices$j, ', ', indices$k, ']')

			} else if (!is.null(j) & !is.null(k) & !is.null(l)) {

				indices <- expand.grid(param = param, j = j, k = k, l = l)
				param <- paste0(indices$param, '[', indices$j, ', ', indices$k, ', ', indices$l, ']')
			
			}
			
		} # if just one parameter
			
	} # if user-specified column names
	
	# valid names?
	if (!is.null(mcmc)) param <- param[param %in% colnames(mcmc)]
	# param <- sort(param)
	
	param
	
}

### get indices
.get_indices <- function(id, mcmc) {

	# id		i, j, k, or l
	# mcmc		MCMC matrix

	if (!is.null(id)) {
		if (inherits(id, 'logical')) {
			if (id) {
				id <- 0L:ncol(mcmc)
			} else {
				id <- NULL
			}
		}
	}
	id
	
}
