#' Combine a set of MCMC iterations created by 'mc_nimble_rolling()'
#' 
#' `mc_compile_chain()` compiles a single chain from the files created by [mc_nimble_rolling()] and deposited into one folder, and `mc_compile_chains()` compiles a series of chains from several folders, each populated with files created by [mc_nimble_rolling()].
#' 
#' The function [mc_nimble_rolling()] creates a series of files, each with a successive set of MCMC iterations (e.g., MCMC samples 1 through 1000, 1001 through 2000, etc.). This function combines them into a single `mcmc` object (see [coda::as.mcmc()]). The files will be assumed ot be named like `chain_set_ZZZ.rds` where `ZZZ` is a number starting at 1 and going to the maximum number of sets, with zero-padding (e.g., `chain_set_001.rds`, `chain_set_002.rds`, etc.).
#' 
#' @param model_dir Folder in which the model output is saved.
#' @param model_dirs Folders in which output from multiple models is saved.
#' 
#' @returns An `mcmc` object.
#' 
#' @seealso [mc_nimble_rolling()]
#' 
#' @examples man/examples/ex_nimble_rolling.r
#' 
#' @export mc_compile_chain
mc_compile_chain <- function(model_dir) {

	chain_files <- omnibus::listFiles(model_dir, pattern = 'chain_set_')
   n_sets <- length(chain_files)

   if (n_sets == 0) stop('No MCMC set files found in this folder.')
   niter <- 0

   # collate MCMC samples together
   for (set in 1:n_sets) {

      mcmc_file <- paste0(model_dir, '/chain_set_', omnibus::prefix(set, 3), '.rds')
      samples <- readRDS(mcmc_file)

      niter <- niter + nrow(samples)
      
      if (set == 1) {
         chain <- samples
      } else {
         chain <- rbind(chain, samples)
      }

   }

   chain <- coda::as.mcmc(chain, start = 1, end = niter, thin = 1)
   chain

}

#' @rdname mc_compile_chain
#' @export mc_compile_chains
mc_compile_chains <- function(model_dirs) {

   chains <- list()
   chains$samples <- list()

   # collate chains
   n_chains <- length(model_dirs)
   for (i in seq_len(n_chains)) {

      chains$samples[[i]] <- mc_compile_chain(model_dirs[i])

   }

   # clip chain to the smallest number of iterations
   n_iter <- rep(NA_real_, n_chains)
   min_iter <- Inf
   for (i in seq_len(n_chains)) {
      n_iter[i] <- nrow(chains$samples[[i]])
      min_iter <- min(min_iter, n_iter[i])
   }

   if (diff(range(n_iter)) > 0) {
      for (i in seq_len(n_chains)) {
         chain <- chains$samples[[i]]
         chain <- chain[seq_len(min_iter), ]
         chain <- coda::as.mcmc(chain, start = 1, end = min_niter, thin = 1)
         chains$samples[[i]] <- chain
      }
   }

   chains$samples <- coda::as.mcmc.list(chains$samples)
   chains <- mc_resummarize(chains)

   chains

}
