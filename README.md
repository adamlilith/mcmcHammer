# mcmcHammer
 Tools for analysis of MCMC chains

<img align="right" src="mcmcHammer.png" height="250"/>

 
`mcmcHammer` is just like all the other packages for analyzing MCMC chains from Bayesian analysis, except that it's not.  Like the others, it can create trace plots and density plots.  But unlike them, it automates "extracting" variables, especially variables with indices.  For example, say your set of MCMC chains have variables named `beta0`, `beta1`, and `beta2`, as well as `gamma[1, 1]`, `gamma[1, 2]`, `gamma[2, 1]` and `gamma[2, 2]`. You can easily create trace plots and density plots for each of these with minimal "manual" tweaking of variable names. Perforce:
 
`mch_trace_dens(mcmc, 'beta', i=0:2)`  
`mch_trace_dens(mcmc, 'gamma', j=1:2, k=1:2)`  

...will make trace and density plots for `beta0`, `beta1`, and `beta2`, and for `gamma[1, 1]`, `gamma[1, 2]`, `gamma[2, 1]` and `gamma[2, 2]`, respectively. These objects are in `ggplot2` package format, so they can be further manipulated using the grammar of graphics tools in that package.

### Installation

You can install `mcmcHammer` from GitHub using:
 
`remotes::install_github('adamlilith/mcmcHammer', dependencies=TRUE)`  

You may need to install the `remotes` package first, using:

`install.packages('remotes')`

# Functions

### MCMC diagnostics and summaries
* `mch_trace_dens`: Trace plots and density plots
* `mch_summarize`: Calculate summary statistics for each variable

### MCMC chain manipulation
* `mch_stack`: "Stack" multiple MCMC chains on one another

### Helper functions
* `mch_param` Create variable names with or without indices

Can't touch this.

~ Adam ~
