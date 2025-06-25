# mcmHammer 2.0.2 (2025-06-24)
o `hammer_extract()` now allows extracting multiple parameters with different index patterns using the `indices` argument.  
o Fixed sometimes-fatal bug in `hammer_subset()`.  

# mcmHammer 2.0.1 (2025-06-16)
o `hammer_subset()` now allows selecting multiple parameters with different index patterns using the `indices` argument.  
o Fix bug in `hammer_summarize()` where `$summary` tag was left out and summaries put into top-level element.  
o Esoteric for developer: Shared `param` definitions.  

# mcmHammer 2.0.0 (2025-06-06)

Complete package re-vamp. Removed plotting functions because redundant with other packages. Package now focuses on MCMC chain and chain-like object manipulations like subsetting, extraction, and `cbind()`ing and `rbind()`ing chains.
