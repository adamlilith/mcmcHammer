#' Extract summary statistics on variables across MCMC chains
#'
#' This function extracts summary statistics across MCMC chains (e.g., mean, median, 2.5% percentile, etc.).
#'
#' @param mcmc	An object of class \code{mcmc} or \code{mcmc.list}, or a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object.
#' @param fun A function that generates the summary statistic. This is the actual function, not the "name" of the function. For example, \code{mean}, not \code{"mean"}. The default is to calculate the mean.  The function is applied across all values of the variable across all chains at once. You can supply additional arguments to the function in \code{...}.
#' @param param Name of the variable(s). The outcome depends on the definitions of \code{i}, \code{j}, \code{k}, and \code{l}. Please see the help for \code{\link{mch_param}} for more explanation on how to specify this argument, plus \code{i}, \code{j}, \code{k}, and \code{l}.
#' @param i,j,k,l Indices used to specify variable names.
#' @param mcmch_stacked \code{FALSE} (default), in which case \code{mcmc} is assumed to be an object of class \code{mcmc}, \code{mcmc.list}, or a \code{list}, or \code{TRUE}, in which case it is a "stacked" MCMC table. This argument is usually used by other functions in this package, so can often be ignored. However, if your MCMC chains have a lot of iterations or variables, then you can speed things up by "stacking" the chains using \code{\link{mch_stack}}, then using that for \code{mcmc}.
#' @param ... Arguments to pass to a function named in \code{fun}.
#'
#' @return A named numeric vector.
#' @export

mch_summarize <- function(
	mcmc,
	fun = mean,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	mcmch_stacked = FALSE,
	...
) {

	# compile MCMC
	if (!mcmch_stacked) mcmc <- mch_stack(mcmc)
	
	# get variables
	param <- mch_param(
		param = param,
		i = i,
		j = j,
		k = k,
		l = l,
		mcmc = mcmc,
		mcmch_stacked = TRUE
	)
	
	
	# summarize
	mcmc <- mcmc[ , ..param]
	out <- apply(mcmc, 2, FUN=fun, ...)
	out

}
