#' Trace plots and density plots
#'
#' @param mcmc	An "stacked" set of MCMC chains, \emph{or} an object of class \code{mcmc} or \code{mcmc.list}, \emph{or} a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param param Name of the variable(s). The outcome depends on the definitions of \code{i}, \code{j}, \code{k}, and \code{l}. Please see the help for \code{\link{mh_param}} for more explanation on how to specify this argument, plus \code{i}, \code{j}, \code{k}, and \code{l}.
#' @param i,j,k,l Indices used to specify variable names. Please see the help for \code{\link{mh_param}}.
#' @param smooth Logical. If \code{TRUE} (default), draw a LOESS line for each chain in a trace plot.
#' @param nrow,ncol Number of rows and columns of graphs to display.
#' @param file Either \code{NULL} (default) or name of a file to which to save (including the file type suffix, like \code{.png} or \code{.pdf}). Specifying a PDF file is especially helpful for cases where there are many plots and multiple pages need to be made to display them.
#' @param stacked \code{FALSE}, in which case \code{mcmc} is assumed to be an object of class \code{mcmc}, \code{mcmc.list}, or a \code{list}, or \code{TRUE} (default), in which case it is a "stacked" MCMC table. This argument is usually used by other functions in this package, so can often be ignored. However, if your MCMC chains have a lot of iterations or variables, then you can speed things up by "stacking" the chains using \code{\link{mh_stack}}, then using that for \code{mcmc}.
#' @param ... Arguments to pass to \code{\link[ggplot2]{ggsave}}.
#'
#' @return A \pkg{ggplot2} \code{ggplot} graphic object and (possibly) a file saved.
#' @export

mh_trace_density <- function(
	mcmc,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	smooth = TRUE,
	nrow = 5,
	ncol = 1,
	file = NULL,
	stacked = TRUE,
	...
) {

	### compile mcmc
	if (!stacked) mcmc <- mh_stack(mcmc)
	
	param <- mh_param(
		param = param,
		i = i,
		j = j,
		k = k,
		l = l,
		mcmc = mcmc,
		stacked = TRUE
	)
	
	### convert chains into stacked format
	mcmc_tall <- .mcmc_to_tall(mcmc=mcmc, param=param)
	
	### plot
	graphs <- list()
	for (this_param in param) {

		this_mcmc_tall <- mcmc_tall[mcmc_tall$param == this_param]
		trace_graph <- ggplot2::ggplot(this_mcmc_tall) +
			ggplot2::aes(x=iter, y=estimate, color=chain) +
			ggplot2::geom_line() +
			ggplot2::xlab('iteration') +
			ggplot2::ggtitle(this_param)
			
		if (smooth) trace_graph  <- trace_graph  +
			ggplot2::geom_smooth(se=FALSE, size=1.4)

		density_graph <- ggplot2::ggplot(this_mcmc_tall) +
				ggplot2::aes(x=estimate, color=chain, fill=chain) +
				ggplot2::geom_density(alpha = 0.2)
	
		graphs[[length(graphs) + 1L]] <- trace_graph + density_graph
		
	}
	
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

mh_trace <- function(
	mcmc,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	smooth = TRUE,
	nrow = 5,
	ncol = 1,
	file = NULL,
	stacked = TRUE,
	...
) {

	### compile mcmc
	if (!stacked) mcmc <- mh_stack(mcmc)
	
	param <- mh_param(
		param = param,
		i = i,
		j = j,
		k = k,
		l = l,
		mcmc = mcmc,
		stacked = TRUE
	)
	
	### convert to stacked format
	mcmc_tall <- .mcmc_to_tall(mcmc=mcmc, param=param)

	### plot
	graphs <- list()
	for (this_param in param) {

		this_mcmc_tall <- mcmc_tall[mcmc_tall$param == this_param]
		graphs[[length(graphs) + 1L]] <- ggplot2::ggplot(this_mcmc_tall) +
			ggplot2::aes(x=iter, y=estimate, color=chain) +
			ggplot2::geom_line() +
			ggplot2::xlab('iteration') +
			ggplot2::ggtitle(this_param)
			
		if (smooth) graphs[[count_graph]] <- graphs[[count_graph]] +
			ggplot2::geom_smooth(se=FALSE, size=2)
		
	}
	
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

mh_density <- function(
	mcmc,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	nrow = 5,
	ncol = 1,
	file = NULL,
	stacked = TRUE,
	...
) {

	### compile mcmc
	if (!stacked) mcmc <- mh_stack(mcmc)
	
	param <- mh_param(
		param = param,
		i = i,
		j = j,
		k = k,
		l = l,
		mcmc = mcmc,
		stacked = TRUE
	)
	
	### convert chains into stacked format
	mcmc_tall <- .mcmc_to_tall(mcmc=mcmc, param=param)
	
	### plot
	graphs <- list()
	for (this_param in param) {

		this_mcmc_tall <- mcmc_tall[mcmc_tall$param == this_param]
		graphs[[length(graphs) + 1L]] <- ggplot2::ggplot(this_mcmc_tall) +
			ggplot2::aes(x=estimate, color=chain, fill=chain) +
			ggplot2::geom_density(alpha = 0.2) +
			ggplot2::ggtitle(this_param)
		
	}
	
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

### convert stacked MCMC to "stacked" format for plotting
.mcmc_to_tall <- function(mcmc, param) {

	### convert chains into stacked format
	valid_param <- param[param %in% colnames(mcmc)]
	mcmc <- mcmc[ , c('iter', 'chain', ..valid_param)]
	
	stacked <- mcmc[ , c('iter', 'chain', ..valid_param[1L])]
	stacked <- data.table::as.data.table(stacked)
	max_iter <- nrow(stacked)
	row_names <- 1L:max_iter
	stacked <- cbind(
		data.table::data.table(param = rep(valid_param[1L], max_iter)),
		stacked
	)
	names(stacked)[colnames(stacked) == valid_param[1L]] <- 'estimate'
	
	if (length(valid_param) > 1L) {
	
		for (valid_param in valid_param[2L:length(valid_param)]) {
		
			add <- cbind(
				data.table::data.table(param = rep(valid_param, max_iter)),
				mcmc[ , c('iter', 'chain', ..valid_param)]
			)
			names(add)[names(add) == valid_param] <- 'estimate'
			
			stacked <- rbind(stacked, add)
			
		} # next variable
		
	} # if >1 valid variable
	rownames(stacked) <- 1L:nrow(stacked)
	stacked$chain <- factor(stacked$chain)
	stacked

}
