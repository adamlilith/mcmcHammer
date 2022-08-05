#' Autocorrelation in chains
#'
#' @param mcmc	An "tall" set of MCMC chains, \emph{or} an object of class \code{mcmc} or \code{mcmc.list}, \emph{or} a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param param Name of the variable(s). The outcome depends on the definitions of \code{i}, \code{j}, \code{k}, and \code{l}. Please see the help for \code{\link{mch_param}} for more explanation on how to specify this argument, plus \code{i}, \code{j}, \code{k}, and \code{l}.
#' @param i,j,k,l Indices used to specify variable names. Please see the help for \code{\link{mch_param}}.
#' @param lags Numeric vector of at what iteration lag(s) to calculate autocorrelation. By default, the function plots correlation within chains at 1, 2, 4, 8, ..., 256 iterations.
#' @param graph If \code{TRUE}, the function creates plots of autocorrelation within chains. If \code{FALSE}, it returns a \coed{data.frame} with autocorrelation results.
#' @param nrow,ncol Number of rows and columns of graphs to display.
#' @param file Either \code{NULL} (default) or name of a file to which to save (including the file type suffix, like \code{.png} or \code{.pdf}). Specifying a PDF file is especially helpful for cases where there are many plots and multiple pages need to be made to display them.
#' @param tall \code{FALSE}, in which case \code{mcmc} is assumed to be an object of class \code{mcmc}, \code{mcmc.list}, or a \code{list}, or \code{TRUE} (default), in which case it is a "tall" MCMC table. This argument is usually used by other functions in this package, so can often be ignored. However, if your MCMC chains have a lot of iterations or variables, then you can speed things up by "stacking" the chains using \code{\link{mch_tall}}, then using that for \code{mcmc}.
#' @param ... Arguments to pass to \code{\link[ggplot2]{ggsave}}.
#'
#' @return A \pkg{ggplot2} \code{ggplot} graphic object, or a file saved, or a \coed{data.frame}.
#' @export

mch_autocor <- function(
	mcmc,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	lags = 2^(1:8),
	graph = TRUE,
	nrow = 5,
	ncol = 2,
	file = NULL,
	tall = TRUE,
	...
) {

	### compile mcmc
	if (!tall) mcmc <- mch_tall(mcmc)
	
	param <- mch_param(
		param = param,
		i = i,
		j = j,
		k = k,
		l = l,
		mcmc = mcmc,
		tall = TRUE
	)
	
	max_iter <- max(mcmc$iter)
	nchains <- max(mcmc$chain)
	
	# for ()
	
	
	### return
	if (length(graphs) == 1L) {
		out <- graphs[[1L]]
	} else {
		nrow <- min(nrow, length(graphs))
		out <- cowplot::plot_grid(plotlist = graphs, ncol = ncol, nrow = nrow) # byrow=FALSE --> error
	}
	
	if (!is.null(file)) {
		ggplot2::ggsave(plot=out, filename=file, ...)
	} else {
		out
	}

}
