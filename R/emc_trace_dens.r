#' Trace plots and density plots
#'
#' This function creates trace plots and/or density plots. It automates finding of indexed variable. These include, for example, variables like \code{beta1}, \code{beta2}, \code{beta3}, etc. or variables like \code{beta[1]}, \code{beta[2]}, \code{beta[1]}, etc.
#'
#' @param x	An object of class \code{mcmc} or \code{mcmc.list}, or a \code{list}. If a \code{list}, the function searches down the first element to see if it can find an \code{mcmc} or \code{mcmc.list} object, then plots this if it can.
#' @param param Name of the parameter(s) to plot. The outcome depends on the definitions of \code{i}, \code{j}, or \code{k}:
#' \itemize{
#' 		\item \code{param = NULL}: Plot all monitored parameters.
#' 		\item \code{param =} a character vector and \code{i}, \code{j}, and \code{k} are all \code{NULL}: Plot the parameter(s) exactly as named.
#' 		\item \code{param =} a character vector and \code{i} is a numeric vector: Plot all parameters with the pattern "\code{param*}" where \code{* is \code{i}. For example: \code{beta1}, \code{beta2}, and \code{beta3}.
#' 		\item \code{param =} a character vector and \code{i} is \code{TRUE}: Plot all parameters with the pattern \code{beta*} where \code{*} is 1 through the maximum value found in the \code{mcmc} object.
#'		\item \code{param =} a character vector and \code{j} is a numeric vector: Plot all parameters with the pattern "\code{beta[j]}".
#'		\item \code{param =} a character vector and \code{j} and \code{k} are a numeric vectors: Plot all parameters with the pattern "\code{beta[j, k]}".
#'		\item \code{param =} a character vector and \code{j}, \code{k}, and \code{l} are a numeric vectors: Plot all parameters with the pattern "\code{beta[j, k, l]}".
#'		\item \code{param =} a character vector and \code{i}, \code{j}, \code{k}, and \code{l} are a numeric vectors: Plot all parameters with the pattern "\code{beta*[j, k, l]}" where \code{*} is \code{i}.
#'		\item \code{param =} a character vector and any of \code{i}, \code{j}, \code{k}, and/or \code{l} are \code{TRUE}: Plot the given pattern up the the maximum value of \code{i}, \code{j}, \code{k}, and/or \code{l}.
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
	param = NULL,
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
	
	### get parameter names
	if (is.null(param)) {
		param <- colnames(mcmc)
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
		
		### get candidate parameter names
		if (!is.null(i)) param <-
			paste0(rep(param, len_i), i)

		if (!is.null(j) & is.null(k)) {
		
			indices <- expand.grid(param = param, j = j)
			param <- paste0(indices$param, '[', indices$j, ']')
				
		} else if (!is.null(j) & !is.null(k) & is.null(l)) {
			
			indices <- expand.grid(param = param, j = j, k = k)
			param <- paste0(indices$param, '[', indices$j, ', ', indices$k, ']')

		} else {

			indices <- expand.grid(param = param, j = j, k = k, l = l)
			param <- paste0(indices$param, '[', indices$j, ', ', indices$k, ', ', indices$l, ']')
		
		}
		
		param <- sort(param)
		
	} # if user-specified column names
	
	### convert chains into tall format
	valid_params <- param[param %in% colnames(mcmc)]
	mcmc <- mcmc[  , c('iter', 'chain', valid_params), drop=FALSE]
	
	tall <- mcmc[ , c('iter', 'chain', valid_params[1L])]
	tall <- as.data.frame(tall)
	max_iter <- nrow(tall)
	row_names <- 1L:max_iter
	tall <- cbind(
		data.frame(param = rep(valid_params[1L], max_iter)),
		tall
	)
	names(tall)[colnames(tall) == valid_params[1L]] <- 'estimate'
	
	if (length(valid_params) > 1L) {
	
		for (valid_param in valid_params[2L:length(valid_params)]) {
		
			add <- cbind(
				data.frame(param = rep(valid_param, max_iter)),
				mcmc[ , c('iter', 'chain', valid_param)]
			)
			names(add)[names(add) == valid_param] <- 'estimate'
			
			tall <- rbind(tall, add)
			
		} # next parameter
		
	} # if >1 valid parameter
	rownames(tall) <- 1L:nrow(tall)
	tall$chain <- factor(tall$chain)

	### plot
	graphs <- list()
	count_graph <- 1
	for (valid_param in valid_params) {

		# trace plot
		if (trace) {
		
			graphs[[count_graph]] <- ggplot2::ggplot(tall[tall$param == valid_param, ]) +
				ggplot2::aes(x=iter, y=estimate, color=chain) +
				ggplot2::geom_line() +
				ggplot2::xlab('iteration') +
				ggplot2::facet_wrap(~param)
				
			if (loess) graphs[[count_graph]] <- graphs[[count_graph]] +
				ggplot2::geom_smooth(se=FALSE)
				
		}
		
		# density plot
		if (density) {
		
			graphs[[count_graph]] <- graphs[[count_graph]] + 
				ggplot2::ggplot(tall[tall$param == valid_param, ]) +
					ggplot2::aes(x=estimate, color=chain, fill=chain) +
					ggplot2::geom_density(alpha = 0.2) +
					ggplot2::facet_wrap(~param)
					
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
