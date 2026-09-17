#' Combine a set of MCMC iterations created by 'mc_nimble_rolling()'
#' 
#' The function [mc_nimble_rolling()] creates a series of files, each with a successive set of MCMC iterations (e.g., MCMC samples 1 through 1000, 1001 through 2000, etc.). This function combines them into a single `mcmc` object (see [coda::as.mcmc()]). The files will be assumed ot be named like `chain_set_ZZZ.rds` where `ZZZ` is a number starting at 1 and going to the maximum number of sets, with zero-padding (e.g., `chain_set_001.rds`, `chain_set_002.rds`, etc.).
#' 
#' @param model_dir Folder in which the model output is saved.
#' 
#' @returns An `mcmc` object.
#' 
#' @seealso [mc_nimble_rolling()]
#' 
#' @examples man/examples/ex_nimble_rolling.r
#' 
#' @export
mc_combine_rolling_sets <- function(model_dir) {

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
