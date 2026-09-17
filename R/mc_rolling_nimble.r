#' Run a restartable 'nimble' model
#' 
#' This function runs a restartable **nimble** model. Assuming it is starting "fresh" where no prior MCMC samples have been created, it first creates an output folder, generates MCMC samples, then saves them in a file that has the format `chain_set_XYZ.rds` where `XYZ` indicates the set number. The first set will thus be saved in the file `chain_set_001.rds`, the second in `chain_set_002.rds`, and so on. Successive sets are generated using the state of the sampler at the end of the previous set. This way, sets can be combined (e.g., using [mc_combine_rolling_sets()]) to create a coherent chain. The state of the sampler is saved in files named like `.model_state_of_set_001.rds`, `.model_state_of_set_002.rds`, etc.
#' 
#' The function will also restart where it last left off if it is stopped or **R** is stopped by loading the last `.model_state_of_set_XYZ.rds` file and continuing from there.
#' 
#' The function can only be run for a single chain at a time. If you wish to have more than one chain, create another folder for this chain and run the function in another instance of **R**. Owing to this, WAIC cannot be calculated. The function also assumes a thinning rate of 1 (samples can be thinned post hoc).
#' 
#' Currently, restarting is supported for these samplers:
#' * 'RW'
#' * 'RW_block'
#' * 'slice'
#' * 'AF_slice'
#' * 'RW_block_lkj_corr_cholesky' (Cholesky sampler from [nimble::dlkj_corr_cholesky()])).
#' Other samplers cannot be restarted :(
#' 
#' @param model_dir Folder in which the model output is to be saved.
#' @param code Output from [nimble::nimbleCode()].
#' @param config Output from [nimble::configureMCMC()].
#' @param compiled Output from [nimble::compileNimble()]
#' @param niter_per_set Numeric (integer): Number of iterations per set. No thinning is done.
#' @param max_sets Numeric (integer): Total number of sets to do.
#' @param inits Either `NULL` (default) or a `list` of initialization values. OK to leave as `NULL` if not restarting.
#' @param verbose Logical: If `TRUE`, display progress.
#' 
#' @returns Nothing (saves files to disk).
#' 
#' @seealso [mc_combine_rolling_sets()] for combining the MCMC set files into a single MCMC chain.
#' 
#' @example man/examples/ex_mc_rolling_nimble.r
#' 
#' @export
mc_rolling_nimble <- function(
	model_dir,
	code,
	config,
	compiled,
	niter_per_set,
	max_sets,
	inits = NULL,
	verbose = TRUE
) {

	if (!file.exists(model_dir)) omnibus::dirCreate(model_dir)
	chains_files <- omnibus::listFiles(model_dir, pattern = 'chain_set_')

	### starting model for the first time
	if (length(chains_files) == 0) {

		start_fresh <- TRUE # starting model from iteration 1
		set <- 1

	### starting model from previous state
	} else {

		# get starting set from existing set files
		chains_file <- chains_files[length(chains_files)]
		chains_file <- basename(chains_file)
		chains_file <- gsub(chains_file, pattern = '.rds', replacement = '')
		sets <- substr(chains_file, 11, nchar(chains_files))
		sets <- as.numeric(sets)
		end_set <- max(sets)
		set <- end_set + 1
		start_fresh <- FALSE

		# load the saved model state from the previous completed set
		state_file <- paste0(model_dir, '/.model_state_of_set_', omnibus::prefix(end_set, 3), '.rds')
		if (!file.exists(state_file)) stop('Could not find saved model state: ', state_file)
		set_state <- readRDS(state_file)

	}

	### main loop over each set of MCMC iterations
	# do MCMC
	# save MCMC
	# remember state of the sampler and RNG
	while (set <= max_sets) {

		if (verbose) {

			omnibus::say('MCMC set ', set, ' of ', max_sets, ' with ', niter_per_set, ' iterations per set | ', date(), pre = 2)
			omnibus::say('model_dir: ', model_dir, post = 2)

		}

		if (start_fresh) {
				
			### NOT restarting
			chains <- runMCMC(
				compiled$build,
				niter = niter_per_set,
				nburnin = 0,
				thin = 1,
				nchains = 1,
				inits = inits,
				progressBar = verbose,
				samplesAsCodaMCMC = FALSE,
				summary = FALSE,
				WAIC = FALSE,
				perChainWAIC = FALSE
			)

			start_fresh <- FALSE

		} else if (!start_fresh) {
		### restarting from same or previous R session

			.Random.seed <- set_state$seed
			
			nodes <- names(set_state$stochastic_nodes)
			for (node in nodes) compiled$model[[node]] <- set_state$stochastic_nodes[[node]]

			compiled$model$calculate()
			for (name in names(compiled$build$mvSaved$sizes)) {
				compiled$build$mvSaved[[name]] <- compiled$model[[name]]
			}

			resize(compiled$build$mvSamples, 0)

			### restore sampler states
			for (i in seq_along(set_state$sampler_states)) {

				sampler_state <- set_state$sampler_states[[i]]$state
				if (is.null(sampler_state) || !length(sampler_state)) next

				for (state_name in names(sampler_state)) {
					valueInCompiledNimbleFunction(
						compiled$build$samplerFunctions[[i]],
						state_name,
						sampler_state[[state_name]]
					)
				}

			}

			### restarting
			compiled$build$run(
				reset = FALSE,
				resetWAIC = FALSE,
				niter = niter_per_set,
				nburnin = 0,
				thin = 1
			)

			chains <- as.matrix(compiled$build$mvSamples)

		}

		saveRDS(chains, paste0(model_dir, '/chain_set_', omnibus::prefix(set, 3), '.rds'))

		### save state
		##############
		# adapted from https://danielturek.github.io/public/restartingMCMC/restartingMCMC.html

		set_state <- list()
		set_state$seed <- .Random.seed

		### save stochastic nodes
		nodes <- compiled$model$getNodeNames(stochOnly = TRUE, includeData = FALSE)
		stoch_nodes <- compiled$model$getVarNames(nodes = nodes)

		set_state$stochastic_nodes <- list()
		for (node in stoch_nodes) set_state$stochastic_nodes[[node]] <- compiled$model[[node]]

		### save sampler states
		set_state$sampler_states <- list()
		# conf$printSamplers()
		sampler_confs <- conf$getSamplers()

		samplers <- lapply(sampler_confs, function(s) list(name = s$name, target = s$target, control = s$control))
		sampler_types <- vapply(samplers, function(s) as.character(s$name), character(1))

		for (i in seq_along(samplers)) {

			sampler_fun <- compiled$build$samplerFunctions[[i]]
			sampler_type <- sampler_types[i]

			set_state$sampler_states[[i]] <- samplers[[i]]
			names(set_state$sampler_states)[i] <- paste(samplers[[i]]$target, collapse = '_&_')

			if (sampler_type == 'RW') {

				set_state$sampler_states[[i]]$state <- list(
					scale = valueInCompiledNimbleFunction(sampler_fun, 'scale'),
					timesRan = valueInCompiledNimbleFunction(sampler_fun, 'timesRan'),
					timesAccepted = valueInCompiledNimbleFunction(sampler_fun, 'timesAccepted'),
					timesAdapted = valueInCompiledNimbleFunction(sampler_fun, 'timesAdapted'),
					gamma1 = valueInCompiledNimbleFunction(sampler_fun, 'gamma1')
				)

			} else if (sampler_type == 'RW_block') {

				set_state$sampler_states[[i]]$state <- list(
					scale = valueInCompiledNimbleFunction(sampler_fun, 'scale'),
					propCov = valueInCompiledNimbleFunction(sampler_fun, 'propCov'),
					chol_propCov = valueInCompiledNimbleFunction(sampler_fun, 'chol_propCov'),
					chol_propCov_scale = valueInCompiledNimbleFunction(sampler_fun, 'chol_propCov_scale'),
					timesRan = valueInCompiledNimbleFunction(sampler_fun, 'timesRan'),
					timesAccepted = valueInCompiledNimbleFunction(sampler_fun, 'timesAccepted'),
					timesAdapted = valueInCompiledNimbleFunction(sampler_fun, 'timesAdapted')
				)

			} else if (sampler_type == 'RW_block_lkj_corr_cholesky') {

				set_state$sampler_states[[i]]$state <- list(
					scale = valueInCompiledNimbleFunction(sampler_fun, 'scale'),
					propCov = valueInCompiledNimbleFunction(sampler_fun, 'propCov'),
					chol_propCov = valueInCompiledNimbleFunction(sampler_fun, 'chol_propCov'),
					chol_propCov_scale = valueInCompiledNimbleFunction(sampler_fun, 'chol_propCov_scale'),
					empirSamp = valueInCompiledNimbleFunction(sampler_fun, 'empirSamp'),
					z = valueInCompiledNimbleFunction(sampler_fun, 'z'),
					y = valueInCompiledNimbleFunction(sampler_fun, 'y'),
					partialSums = valueInCompiledNimbleFunction(sampler_fun, 'partialSums'),
					logDetJac = valueInCompiledNimbleFunction(sampler_fun, 'logDetJac'),
					timesRan = valueInCompiledNimbleFunction(sampler_fun, 'timesRan'),
					timesAccepted = valueInCompiledNimbleFunction(sampler_fun, 'timesAccepted'),
					timesAdapted = valueInCompiledNimbleFunction(sampler_fun, 'timesAdapted')
				)

			} else if (sampler_type == 'slice') {

				set_state$sampler_states[[i]]$state <- list(
					width = valueInCompiledNimbleFunction(sampler_fun, 'width'),
					timesRan = valueInCompiledNimbleFunction(sampler_fun, 'timesRan'),
					timesAdapted = valueInCompiledNimbleFunction(sampler_fun, 'timesAdapted'),
					sumJumps = valueInCompiledNimbleFunction(sampler_fun, 'sumJumps')
				)
			
			} else if (sampler_type == 'AF_slice') {

				set_state$sampler_states[[i]]$state <- list(
					gammaMatrix = valueInCompiledNimbleFunction(sampler_fun, 'gammaMatrix'),
					empirCov = valueInCompiledNimbleFunction(sampler_fun, 'empirCov'),
					empirSamp = valueInCompiledNimbleFunction(sampler_fun, 'empirSamp'),
					widthVec = valueInCompiledNimbleFunction(sampler_fun, 'widthVec'),
					nExpansions = valueInCompiledNimbleFunction(sampler_fun, 'nExpansions'),
					nContracts = valueInCompiledNimbleFunction(sampler_fun, 'nContracts'),
					adaptFactorMaxIter = valueInCompiledNimbleFunction(sampler_fun, 'adaptFactorMaxIter'),
					factorCounter = valueInCompiledNimbleFunction(sampler_fun, 'factorCounter'),
					factorTimesAdapted = valueInCompiledNimbleFunction(sampler_fun, 'factorTimesAdapted'),
					allWidthsAdapted = valueInCompiledNimbleFunction(sampler_fun, 'allWidthsAdapted'),
					widthCounter = valueInCompiledNimbleFunction(sampler_fun, 'widthCounter'),
					adaptWidthMaxIter = valueInCompiledNimbleFunction(sampler_fun, 'adaptWidthMaxIter'),
					adaptWidthInterval = valueInCompiledNimbleFunction(sampler_fun, 'adaptWidthInterval'),
					widthIndicatorVec = valueInCompiledNimbleFunction(sampler_fun, 'widthIndicatorVec')
				)

			}

		} # next node

		saveRDS(set_state, paste0(model_dir, '/.model_state_of_set_', omnibus::prefix(set, 3), '.rds'))
		set <- set + 1

	} # next set

} # EOF
