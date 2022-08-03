#' Trace plots and density plots
#'
#' @param mcmc	An object of class \code{mcmc} or \code{mcmc.list}, or a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param param Name of the variable(s). The outcome depends on the definitions of \code{i}, \code{j}, \code{k}, and \code{l}. Please see the help for \code{\link{mch_param}} for more explanation on how to specify this argument, plus \code{i}, \code{j}, \code{k}, and \code{l}.
#' @param i,j,k,l Indices used to specify variable names.
#' @param trace Logical. If \code{TRUE} (default), generate a trace plot.
#' @param density Logical. If \code{TRUE} (default), generate a density plot.
#' @param smooth Logical. If \code{TRUE} (default), draw a LOESS line for each chain in a trace plot.
#' @param nrows,ncols Number of rows and columns of graphs to display per "page".
#' @param file Either \code{NULL} (default) or name of a file to which to save (including the file type suffix, like \code{.png} or \code{.pdf}). Specifying a PDF file is especially helpful for cases where there are many plots and multiple pages need to be made to display them.
#' @param stacked \code{FALSE}, in which case \code{mcmc} is assumed to be an object of class \code{mcmc}, \code{mcmc.list}, or a \code{list}, or \code{TRUE} (default), in which case it is a "stacked" MCMC table. This argument is usually used by other functions in this package, so can often be ignored. However, if your MCMC chains have a lot of iterations or variables, then you can speed things up by "stacking" the chains using \code{\link{mch_stack}}, then using that for \code{mcmc}.
#' @param ... Arguments to pass to \code{\link[ggplot2]{ggsave}}.
#'
#' @return A \pkg{ggplot2} \code{ggplot} graphic object and (possibly) a file saved.
#' @export

mch_trace_density <- function(
	mcmc,
	param = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	trace = TRUE,
	smooth = TRUE,
	density = TRUE,
	nrows = 5,
	ncols = 1,
	file = NULL,
	stacked = TRUE,
	...
) {

	### compile mcmc
	if (!stacked) mcmc <- mch_stack(mcmc)
	
	param <- mch_param(
		param = param,
		i = i,
		j = j,
		k = k,
		l = l,
		mcmc = mcmc,
		stacked = TRUE
	)
	
	### convert chains into tall format
	valid_params <- param[param %in% colnames(mcmc)]
	mcmc <- mcmc[ , c('iter', 'chain', ..valid_params)]
	
	tall <- mcmc[ , c('iter', 'chain', ..valid_params[1L])]
	tall <- as.data.table(tall)
	max_iter <- nrow(tall)
	row_names <- 1L:max_iter
	tall <- cbind(
		data.table::data.table(param = rep(valid_params[1L], max_iter)),
		tall
	)
	names(tall)[colnames(tall) == valid_params[1L]] <- 'estimate'
	
	if (length(valid_params) > 1L) {
	
		for (valid_var in valid_params[2L:length(valid_params)]) {
		
			add <- cbind(
				data.table::data.table(param = rep(valid_var, max_iter)),
				mcmc[ , c('iter', 'chain', ..valid_var)]
			)
			names(add)[names(add) == valid_var] <- 'estimate'
			
			tall <- rbind(tall, add)
			
		} # next variable
		
	} # if >1 valid variable
	rownames(tall) <- 1L:nrow(tall)
	tall$chain <- factor(tall$chain)

	### plot
	graphs <- list()
	count_graph <- 1L
	for (valid_var in valid_params) {

		# trace plot
		if (trace) {
		
			graphs[[count_graph]] <- ggplot2::ggplot(tall[tall$param == valid_var]) +
				ggplot2::aes(x=iter, y=estimate, color=chain) +
				ggplot2::geom_line() +
				ggplot2::xlab('iteration') +
				ggplot2::facet_wrap(~param)
				
			if (smooth) graphs[[count_graph]] <- graphs[[count_graph]] +
				ggplot2::geom_smooth(se=FALSE)
				
		}
		
		# density plot
		if (density) {
		
			this_graph <- ggplot2::ggplot(tall[tall$param == valid_var]) +
					ggplot2::aes(x=estimate, color=chain, fill=chain) +
					ggplot2::geom_density(alpha = 0.2) +
					ggplot2::facet_wrap(~param)
				
			if (trace) {
				graphs[[count_graph]] <- graphs[[count_graph]] + this_graph
			} else {
				graphs[[count_graph]] <- this_graph
			}
				
		}
		
		count_graph <- count_graph + 1L
		
	}
	
	### return
	if (length(graphs) == 1L) {
		out <- graphs[[1L]]
	} else {
		nrows <- min(nrows, length(graphs))
		out <- cowplot::plot_grid(plotlist = graphs, ncol = ncols, nrow = nrows) # byrow=FALSE --> error
	}
	
	if (!is.null(file)) {
		ggplot2::ggsave(plot=out, filename=file, ...)
	} else {
		out
	}

}
