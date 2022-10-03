# mcmcHammer
 Tools for analysis of MCMC chains

<img align="right" src="mcmcHammer.png" height="250"/>

 
`mcmcHammer` is just like all the other packages for analyzing MCMC chains from Bayesian analysis, except that it's not.  Like the others, it can create trace plots and density plots.  But unlike them, it automates "extracting" variables, especially variables with indices.  For example, say your set of MCMC chains have variables named `beta0`, `beta1`, and `beta2`, as well as `gamma[1, 1]`, `gamma[1, 2]`, `gamma[2, 1]` and `gamma[2, 2]`. You can easily create trace plots and density plots for each of these with minimal "manual" tweaking of variable names. Perforce:
 
`mh_trace(mcmc, 'beta', i=0:2)`  
`mh_density(mcmc, 'gamma', j=1:2, k=1:2)`  

...will make trace and density plots for the `beta` variables and the `gamma` variables.  It can even do so if you can't recall how many variants of a variable there are. For example,

`mh_trace(mcmc, 'beta', i=TRUE)`  
`mh_density(mcmc, 'gamma', j=TRUE, k=TRUE)`  

will make trace or density plots for all variables of the pattern `beta*` and `gamma[*, *]`.

All plots are in `ggplot2` package format, so they can be further manipulated using the grammar of graphics tools in that package.

# Installation

You can install `mcmcHammer` from GitHub using:
 
`remotes::install_github('adamlilith/mcmcHammer', dependencies=TRUE)`  

You may need to install the `remotes` package first, using:

`install.packages('remotes')`

# Usage

Most function require either an object of class `mcmc` or `mcmc.list`, which are often the format of output from modeling functions in Bayesian packages. However, they can also be created by the `coda` package. Functions in `mcmcHammer` will need to "stack" `mcmc` objects before they can be used. Here, a "stack" is the product of simply `rbind`-ing one `mcmc` matrix on top of the next. Stacking can be done automatically by each function if the argument `stacked` is `FALSE`, but if you are using the functions a lot, it's easier and often much faster to stack the `mcmc` chains first then use this as the input into functions:

`library(mcmcHammer)`  
`data(mcmc)`

`mcmc_stacked <- mh_stack(mcmc)`

Now, we can call summary and diagnostic functions with the stacked version of the MCMC chains:

`mh_trace_density(mcmc_stacked, param='beta', i=0:5)`  
`mh_summary_by_param(mcmc_stacked, fun=mean, param='gamma', j=1:2, k=1:2)`

If we hadn't stacked the chains, we could still use these functions, albeit with some overhead:

`mh_trace_density(mcmc, param='beta', i=0:5, stacked=FALSE)`  
`mh_summary_by_param(mcmc, fun=mean, param='gamma', j=1:2, k=1:2, stacked=FALSE)`

# Functions

### MCMC diagnostics and summaries
* `mh_trace_density`: Trace plots and density plots
* `mh_density`: Density plots
* `mh_trace`: Trace plots
* `mh_summary_by_param`: Calculate summary statistics for each variable

### MCMC chain manipulation
* `mh_stack`: "Stack" multiple MCMC chains on one another

### Helper functions
* `mh_param` Create variable names with or without indices


Can't touch this.

~ Adam ~
