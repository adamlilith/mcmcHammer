#' Check a `nimble` model nodes for infinite or NaN likelihoods
#' 
#' Checks each node of a `nimble` model object created from [nimble::nimbleModel()] for likelihoods that are `NaN` or infinite.
#' 
#' @param model Output from [nimble::nimbleModel()]. The model must have all nodes initialized or [nimble::calculate()] must have been run on the model to ensure all nodes could have non-missing values.
#' 
#' @param fail Logical: If `FALSE`, simply reports the node(s) that fail the checks. If `TRUE`, then reports the node and exits with an error.
#' 
#' @param verbose Logical: If `TRUE`, report progress.
#'
#' @returns Invisibly returns `TRUE` if there are no nodes with infinite or `NaN` likelihood values. Otherwise, reports the node(s) that fail these checks.
#' 
#' @example man/examples/ex_mc_nimble_check.r
#' @export
mc_nimble_check <- function(model, fail = FALSE, verbose = FALSE) {

	# check for infinite or NaN likelihoods

	# get all nodes
	all_nodes <- model$getNodeNames()

	# check each stochastic node
	stochastic_nodes <- model$getNodeNames(stochOnly = TRUE)
	if (verbose) omnibus::say('Checking ', length(stochastic_nodes), ' stochastic nodes...')

	for (node in stochastic_nodes) {
		node_calc <- model$calculate(node)
		if (is.na(node_calc) || is.infinite(node_calc)) {
			msg <- paste0('Problem with node: ', node, ' = ', node_calc)
			if (fail) {
				stop(msg)
			} else {
				warning(msg)
			}
		}
	}

	# check top-level nodes
	top_nodes <- model$getNodeNames(topOnly = TRUE)
	if (verbose) omnibus::say('Checking ', length(top_nodes), ' top-level nodes...')

	for (node in top_nodes) {
		node_calc <- model$calculate(node)
		if (is.na(node_calc) || is.infinite(node_calc)) {
			msg <- paste0('Problem with top node: ', node, ' = ', node_calc)
			if (fail) {
				stop(msg)
			} else {
				warning(msg)
			}
		}
	}

	# check data nodes
	data_nodes <- model$getNodeNames(dataOnly = TRUE)
	if (verbose) omnibus::say('Checking ', length(data_nodes), ' data nodes...')

	for (node in data_nodes) {
		node_calc <- model$calculate(node)
		if (is.na(node_calc) || is.infinite(node_calc)) {
			msg <- paste0('Problem with data node: ', node, ' = ', node_calc)
			if (fail) {
				stop(msg)
			} else {
				warning(msg)
			}
		}
	}

	invisible(TRUE)

}
