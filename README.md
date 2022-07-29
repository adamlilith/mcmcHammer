# emcemc
 Tools for analysis of MCMC chains
 
`emcemc` is just like all the other packages for analyzing MCMC chains from Bayesian analysis, except that it's not.  Like the others, it can create trace plots and density plots.  But unlike them, it automates "extracting" variables, especially variables with indices.  For example, say your set of MCMC chains have variables named `beta0`, `beta1`, and `beta2`, as well as `gamma[1, 1]`, `gamma[1, 2]`, `gamma[2, 1]` and `gamma[2, 2]`. You can easily create trace plots and density plots for each of these with minimal "manual" tweaking of variable names. Perforce:
 
`emc_trace_dens(mcmc, 'beta', i=0:2)`  
`emc_trace_dens(mcmc, 'gamma', j=1:2, k=1:2)`  

...will make trace and density plots for `beta0`, `beta1`, and `beta2`, and for `gamma[1, 1]`, `gamma[1, 2]`, `gamma[2, 1]` and `gamma[2, 2]`, respectively. These objects are in `ggplot2` package format, so they can be further manipulated using the grammar of graphics tools in that package.

### Installation

You can install `emcemc` from GitHub using:
 
`remotes::install_github('adamlilith/emcemc', dependencies=TRUE)`  

You may need to install the `remotes` package first, using

`install.packages('remotes')

# Functions

### MCMC chain manipulation
* `emc_stack_chains`: "Stack" multiple MCMC chains on one another

### MCMC diagnostics
* `emc_trace_dens`; Trace plots and density plots

~ Adam ~
