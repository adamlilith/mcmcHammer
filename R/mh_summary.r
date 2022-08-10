#' Extract summary statistics for variables or across variables
#'
#' These functions extract summary statistics across MCMC chains (e.g., mean, median, 2.5% percentile, etc.). \code{mh_summary_by_param} provides summary statistics for each parameter in \code{param} (e.g., the average across of a parameter across chains and iterations). \code{mh_summary_by_iter} summarizes across parameters for each iteration in each chain (e.g., the sum of variables during each iteration).
#'
#' @param mcmc	An "stacked" set of MCMC chains, \emph{or} an object of class \code{mcmc} or \code{mcmc.list}, \emph{or} a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param fun A function that generates the summary statistic. This is the actual function, not the "name" of the function. For example, \code{mean}, not \code{"mean"}. The default is to calculate the mean.  The function is applied across all values of the variable across all chains at once. You can supply additional arguments to the function in \code{...}.
#' @param param Name of the variable(s). The outcome depends on the definitions of \code{i}, \code{j}, \code{k}, and \code{l}. Please see the help for \code{\link{mh_param}} for more explanation on how to specify this argument, plus \code{i}, \code{j}, \code{k}, and \code{l}.
#' @param i,j,k,l Indices used to specify variable names. Please see the help for \code{\link{mh_param}}.
#' @param stacked \code{FALSE}, in which case \code{mcmc} is assumed to be an object of class \code{mcmc}, \code{mcmc.list}, or a \code{list}, or \code{TRUE} (default), in which case it is a "stacked" MCMC table. This argument is usually used by other functions in this package, so can often be ignored. However, if your MCMC chains have a lot of iterations or variables, then you can speed things up by "stacking" the chains using \code{\link{mh_stack}}, then using that for \code{mcmc}.
#' @param ... Arguments to pass to a function named in \code{fun}.
#'
#' @return A named numeric vector.
#' @export

mh_summary_by_param <- function(
	mcmc,
	fun = mean,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	stacked = TRUE,
	...
) {

	# compile MCMC
	if (!stacked) mcmc <- mh_stack(mcmc)
	
	# get variables
	param <- mh_param(
		param = param,
		i = i,
		j = j,
		k = k,
		l = l,
		mcmc = mcmc,
		stacked = TRUE
	)
	
	# summarize
	mcmc <- mcmc[ , ..param]
	out <- mcmc[ , lapply(.SD, fun), .SDcols = param]
	out <- unlist(as.vector(out))
	out

}

# mh_summary_by_iter <- function(
	# mcmc,
	# fun = mean,
	# param = NULL,
	# i = NULL,
	# j = NULL,
	# k = NULL,
	# l = NULL,
	# stacked = TRUE,
	# ...
# ) {

	# # compile MCMC
	# if (!stacked) mcmc <- mh_stack(mcmc)
	
	# # get variables
	# param <- mh_param(
		# param = param,
		# i = i,
		# j = j,
		# k = k,
		# l = l,
		# mcmc = mcmc,
		# stacked = TRUE
	# )
	
	# # summarize
	# cols <- c('chain', 'iter', param)
	# mcmc <- mcmc[ , ..cols]
	# # out <- mcmc[ , .summary := fun, by=c(iter, chain)]
	# out <- mcmc[ , .summary := fun(.SD), .SDcols = param, by=iter]
	
	
	# out <- data.table(
		# iter = mcmc$iter,
	# apply(mcmc, 1, fun, ...)
	# apply(mcmc, 1, fun, ...)
	# out

# }
