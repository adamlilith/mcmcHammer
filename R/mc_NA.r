# #' Determine if any MC samples are 'NA'
# #'
# #' `mc_NA()` returns a list with row/column indices of each `NA` in an MCMC list object.
# #' `mc_count_NA()` returns the number of `NA`s in an MCMC list object.
# #' `mc_anyNA()` returns `TRUE` or `FALSE` if there is any `NA` in the MCMC list object.
# #'
# #' @inheritParams .mcmc
# #' @param summary Logical: If `FALSE` (default), then only analyze the `samples` part of the MCMC list object. If `TRUE`, only analyze the `summary` part.
# #'
# #' @returns `mc_NA()` returns a named list. `mc_count_NA()` returns a named vector (one value per chain or item in the `summary`). `mc_anyNA()` returns vector of logical values, one per chain or item in the `summary`.
# #'
# #' @examples
# #' 
# #' data(mcmc)
# #' rbinded <- mc_rbind(mcmc)
# #' head(rbinded)
# #' dim(rbinded)
# #'
# #' @rdname mc_NA
# #' @export
# mc_NA <- function(mcmc, summary = FALSE) {

# 	out <- list()
# 	if (!summary) {

# 		if (inherits(mcmc[[1]], 'mcmc') {

# 			na_cols <- apply(1, anyNA)
# 			if (any(na_cols)) {

# 				col_names <- colnames(mcmc$samples)
# 				na_cols <- which(na_cols)
# 				for (na_col in na_cols) {
					
# 					out <- c(
# 						out,
# 						list(which(is.na(mcmc$samples[ , na_col])))
# 					)
# 					names(out)[length(out)] <- col_names[na_col]

# 				} # next column with NAs
# 			} # any columns with NAs

# 		} else if (inherits(mcmc[[1]], 'mcmc.list')) {

# 			nchains <- mc_n_chains(mcmc)
# 			for (i in seq_len(nchains)) {

# 				mcmc_one_chain <- mcmc
# 				mcmc_one_chain$samples <- mcmc_one_chain$samples[[i]]
# 				this_out <- mc_NA(mcmc_one_chain)

# 				out <- c(
# 					out,
# 					list(this_out)
# 				)
# 				names(length(out)) <- names(mcmc$samples)[i]

# 			} # next chain

# 		} # MCMC has an mcmc/list

# 	} else if (summary) {

# 		out <- list()
# 		if (inherits(mcmc$summary, 'matrix')) {

# 			na_rows <- which(!complete.cases(mcmc$summary))
# 			names(na_rows) <- rownames(mcmc$summary)[na.rows]
# 			out <- na_rows

# 		} else {

# 			n <- length(mcmc$summary)
# 			for (i in seq_len(n)) {

# 				this_mcmc <- mcmc$summary
# 				this_mcmc$summary <- this_mcmc$summary[[i]]

# 				this_out <- mc_NA(this_mcmc, summary = TRUE)
# 				out <- c(out, this_out)
# 				names(out) <- names(mcmc$summary)[i]

# 			}

# 		}

# 	}
# 	out

# }