# mcmcHammer
Tools for managing MCMC chains and restarting `nimble` models

<img align="right" src="mcmcHammer.png" height="250"/>

This package has two functionalities:

1) **mcmcHammer** has functions for creating a "rolling" **nimble** model that generates a set of MCMC samples, saves the output, then restarts where it left off to create another set, etc. These sets can be combined to create a single chain. This is helpful for cases where the computer on which the model is being run may restart, or if the total number of iterations needed for convergence is unknown.

2) **mcmcHammer** is also a "helper" package for working with Monte Carlo Markov Chain objects produced by popular Bayesian **R** packages like **rstan**, **rjags**, and **nimble**. These package produce `list` objects that have this kind of structure:

```
mcmc$samples:
mcmc$samples$chain1 <matrix of posterior samples, one per MCMC iteration>
mcmc$samples$chain2 <matrix of posterior samples, one per MCMC iteration>
mcmc$summary:
mcmc$summary$chain1 <matrix of summary statistics for chain 1>
mcmc$summary$chain2 <matrix of summary statistics for chain 2>
mcmc$summary$all.chains <matrix of summary statistics for all chains>
```

Functions in this package do *not* create plots or calculate statistics. Rather, its specialty is making extraction of posteriors, subsetting chains, and combining MCMC objects easy (no need to look up regex expressions, no need to do `paste(beta[', 1:207, '])'`, etc.).

As an example, assume your set of MCMC chains have variables named `beta0`, `beta1`, and `beta2`, as well as `gamma[1, 1]`, `gamma[1, 2]`, `gamma[2, 1]` and `gamma[2, 2]`, in addition to thousands of other variables. You can easily subset the chains to just these variables:

`mc_subset(mcmc, 'beta', i = 0:2)`  
`mc_subset(mcmc, 'gamma', j = 1:2, k = 1:2)`  

If you don't recall how many of each variable there are, you can use `TRUE` to indicate all of the relevant variables:

`mc_subset(mcmc, 'beta', i = TRUE)`  
`mc_subset(mcmc, 'gamma', j = TRUE, k = TRUE)`  

To get the posterior mean, median, or quantiles, you can use the `mc_extract()` function. For example, if you want to extract the posterior mean or median of the `beta` variables, you can do so with:

`mc_extract(mcmc, 'beta', i = 0:2) # means (default)`  
`mc_extract(mcmc, 'beta', i = 0:2, stat = 'lower') # lower value of inner 95th quantile`  
`mc_extract(mcmc, 'beta', i = 0:2, stat = 'upper') # upper value of inner 95th quantile`  

In all of the functions, you can use the shortcut (like `i = TRUE`) if you don't remember how many `beta` variables there are.

# Installation

You can install **mcmcHammer** from GitHub using:
 
`remotes::install_github('adamlilith/mcmcHammer', dependencies = TRUE)`  

You may need to install the `remotes` package first, using:

`install.packages('remotes')`

# Functions

All functions begin with `mc_` to assist finding them with automated code-completion.

### Restartable **nimble** models and helpful stuff
* `mc_combine_rolling_sets()`: Combine a set of MCMC iterations created by `mc_nimble_rolling()`.
* `mc_nimble_check()`: Check a **nimble** model for nodes with `NaN` or infinite likelihoods.
* `mc_nimble_rolling()`: Run a restartable **nimble** model.

### Extraction and subsetting
* `mc_extract()`: Posterior summary statistics (mean, median, S.D., lower/upper quantiles).
* `mc_samples()`: Get the "samples" part of an MCMC list.
* `mc_stack()`: Combine MCMC samples using `rbind()`.
* `mc_summaries()`: Get the by-chain summary from an MCMC list.
* `mc_summary()`: Get the "`all.chains`" summary from an MCMC list.
* `mc_subset()`: Subset MCMC chains by variable name and/or index.
* `mc_subset_chains()`: Select specific chains in an MCMC object.

### MCMC chain manipulation
* `mc_cbind()`: Combine side-to-side compatible `mcmc`, `mcmc.list`, or `list`s of `mcmc.list`s.
* `mc_combine()`: Combine two or more MCMC objects and calculate new summary statistics.
* `mc_rbind()`: "Stack" MCMC chains into a single matrix.
* `mc_rename()`: Rename one or more variables in an `mcmc` object.

### Helper functions
* `mc_n_chains()`: Number of chains.
* `mc_n_iter()`: Number of iterations.
* `mc_param()`: Match variables names to MCMC columns.
* `mc_resummarize()` / `mc_resummarise()`: Per-chain and all-chains summary matrices.

*Can't touch this.*
