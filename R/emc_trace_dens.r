#' Trace plots and density plots
#'
#' This function creates trace plots and/or density plots. It automates finding of indexed variable. These include, for example, variables like \code{beta1}, \code{beta2}, \code{beta3}, etc. or variables like \code{beta[1]}, \code{beta[2]}, \code{beta[1]}, etc.
#'
#' @param x	An object of class \code{mcmc} or \code{mcmc.list}, or a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param var Name of the variable(s) to plot. The outcome depends on the definitions of \code{i}, \code{j}, or \code{k}:
#' \itemize{
#' 		\item \code{var = NULL}: Plot all monitored parameters.
#' 		\item \code{var =} a character vector and \code{i}, \code{j}, and \code{k} are all \code{NULL}: Plot the variable(s) exactly as named.
#' 		\item \code{var =} a character vector and \code{i} is a numeric vector: Plot all parameters with the pattern "\code{var*}" where \code{*} is \code{i}. For example: \code{beta1}, \code{beta2}, and \code{beta3}.
#' 		\item \code{var =} a character vector and \code{i} is \code{TRUE}: Plot all parameters with the pattern \code{beta*} where \code{*} is 1 through the maximum value found in the \code{mcmc} object.
#'		\item \code{var =} a character vector and \code{j} is a numeric vector: Plot all parameters with the pattern "\code{beta[j]}".
#'		\item \code{var =} a character vector and \code{j} and \code{k} are a numeric vectors: Plot all parameters with the pattern "\code{beta[j, k]}".
#'		\item \code{var =} a character vector and \code{j}, \code{k}, and \code{l} are a numeric vectors: Plot all parameters with the pattern "\code{beta[j, k, l]}".
#'		\item \code{var =} a character vector and \code{i}, \code{j}, \code{k}, and \code{l} are a numeric vectors: Plot all parameters with the pattern "\code{beta*[j, k, l]}" where \code{*} is \code{i}.
#'		\item \code{var =} a character vector and any of \code{i}, \code{j}, \code{k}, and/or \code{l} are \code{TRUE}: Plot the given pattern up the the maximum value of \code{i}, \code{j}, \code{k}, and/or \code{l}.
#' } 
#' @param i,j,kl These can be either \code{NULL} (default), integer vectors, or \code{TRUE}.
#' @param trace Logical. If \code{TRUE} (default), generate a trace plot.
#' @param density Logical. If \code{TRUE} (default), generate a density plot.
#' @param loess Logical. If \code{TRUE} (default), draw a LOESS line for each chain in a trace plot.
#' @param nrows Number of rows of graphs to display per "page".
#' @param file Either \code{NULL} (default) or name of a file to which to save. Specifying a PDF file is especially helpful for cases where the 
#' @param ... Arguments to pass to \code{\link[ggplot2]{ggsave}}.
#'
#' @return A \pkg{ggplot2} \code{ggplot} graphic object and (possibly) a file saved.
#' @export

emc_trace_dens <- function(
	x,
	var = NULL,
	i = NULL,
	j = NULL,
	k = NULL,
	l = NULL,
	trace = TRUE,
	loess = TRUE,
	nrows = 5,
	file = NULL,
	...
) {

	### compile mcmc
	mcmc <- emc_stack_chains(x)
	
	### get variable names
	if (is.null(var)) {
		var <- colnames(mcmc)
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
		if (!is.null(i)) var <-
			paste0(rep(var, len_i), i)

		if (!is.null(j) & is.null(k)) {
		
			indices <- expand.grid(var = var, j = j)
			var <- paste0(indices$var, '[', indices$j, ']')
				
		} else if (!is.null(j) & !is.null(k) & is.null(l)) {
			
			indices <- expand.grid(var = var, j = j, k = k)
			var <- paste0(indices$var, '[', indices$j, ', ', indices$k, ']')

		} else {

			indices <- expand.grid(var = var, j = j, k = k, l = l)
			var <- paste0(indices$var, '[', indices$j, ', ', indices$k, ', ', indices$l, ']')
		
		}
		
		var <- sort(var)
		
	} # if user-specified column names
	
	### convert chains into tall format
	valid_vars <- var[var %in% colnames(mcmc)]
	mcmc <- mcmc[  , c('iter', 'chain', valid_vars), drop=FALSE]
	
	tall <- mcmc[ , c('iter', 'chain', valid_vars[1L])]
	tall <- as.data.frame(tall)
	max_iter <- nrow(tall)
	row_names <- 1L:max_iter
	tall <- cbind(
		data.frame(var = rep(valid_vars[1L], max_iter)),
		tall
	)
	names(tall)[colnames(tall) == valid_vars[1L]] <- 'estimate'
	
	if (length(valid_vars) > 1L) {
	
		for (valid_var in valid_vars[2L:length(valid_vars)]) {
		
			add <- cbind(
				data.frame(var = rep(valid_var, max_iter)),
				mcmc[ , c('iter', 'chain', valid_var)]
			)
			names(add)[names(add) == valid_var] <- 'estimate'
			
			tall <- rbind(tall, add)
			
		} # next variable
		
	} # if >1 valid variable
	rownames(tall) <- 1L:nrow(tall)
	tall$chain <- factor(tall$chain)

	### plot
	graphs <- list()
	count_graph <- 1
	for (valid_var in valid_vars) {

		# trace plot
		if (trace) {
		
			graphs[[count_graph]] <- ggplot2::ggplot(tall[tall$var == valid_var, ]) +
				ggplot2::aes(x=iter, y=estimate, color=chain) +
				ggplot2::geom_line() +
				ggplot2::xlab('iteration') +
				ggplot2::facet_wrap(~var)
				
			if (loess) graphs[[count_graph]] <- graphs[[count_graph]] +
				ggplot2::geom_smooth(se=FALSE)
				
		}
		
		# density plot
		if (density) {
		
			graphs[[count_graph]] <- graphs[[count_graph]] + 
				ggplot2::ggplot(tall[tall$var == valid_var, ]) +
					ggplot2::aes(x=estimate, color=chain, fill=chain) +
					ggplot2::geom_density(alpha = 0.2) +
					ggplot2::facet_wrap(~var)
					
		}
		
		count_graph <- count_graph + 1L
		
	}
	
	### return
	if (length(graphs) == 1L) {
		out <- graphs[[1]]
	} else {
		nrows <- min(nrows, length(graphs))
		out <- cowplot::plot_grid(plotlist = graphs, nrow=nrows, ncol=1L)
	}
	
	if (!is.null(file)) ggplot2::ggsave(out, file, ...)
	out

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
