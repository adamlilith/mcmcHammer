# Arguments shared across many functions in mcmcHammer

#' @name .ijkl
#' @title Shared argument(s)
#'
#' @param i,j,k,l Indices used to specify variable names:
#' * `i` refers to indices that occur immediately after the variable's "base" name. For example, if `param` is "`beta`", and `i = 0:2`, then the function will return `beta0`, `beta1`, and `beta2`.
#'  * `j` refers to the *first* index in square brackets. For example, if `param` is `beta` and `j = 1:3`, then the output will be \code{beta[1]}, \code{beta[2]}, and \code{beta[3]}.
#'  * `k` refers to the *second* index in square brackets. If `k` is defined (not `NULL`), then `j` must be defined. For example, if `param` is `beta`, `j = 1:2`, and `k = 1:3`, then the output will be like \code{beta[1, 1]}, \code{beta[2, 1]}, \code{beta[1, 2]}, \code{beta[2, 2]}, \code{beta[1, 3]}, and \code{beta[2, 3]}.
#' * `l` refers to the *third* index in square brackets. If you provide `l`, you must also define `j` and `k`. For example, if `param` is `beta`, `j = 1:2`, `k = 4:5`, and `l = 9:10`, then the output will be like \code{beta[1, 4, 9]}, \code{beta[2, 4, 9]}, \code{beta[1, 5, 9]}, \code{beta[2, 5, 9]}, \code{beta[1, 4, 10]}, \code{beta[2, 4, 10]}, and so on.
#' * You can also define `i` when you define `j` alone, `j` and `k`, or `j`, `k`, and `l`. This will return patterns like \code{beta0[1]}, or like \code{beta0[1, 1]}, or like \code{beta0[1, 2, 3]}.
#' You must specify all of the indices that would appear in square brackets. For example, for the pattern \code{param[j, k, l]}, you need to define `j`, `k`, and `l`. You can't just define `k = 1:2`. You need to set `j` and `l` equal to numeric/integer vectors and/or logical values.
NULL

#' @name .indices
#' @title Shared argument(s)
#'
#' @param indices A `list` of `lists`. If you want to select multiple parameters, which each have their own indexing (`i`, `j`, `k`, and/or `l`), you can specify them with this argument. Each sub-`list` should have the indices you wish to use for the given value of `param`. Sub-`list`s will be matched with the respective value of `param`. For example, if you want to select parameters `alpha0`, `alpha1` and `beta[1]` and `beta[2]` from an `mcmc` chain, you could use `mc_subset(mcmc, param = c('alpha', 'beta'), indices = list(list(i = 0:1), list(j = 1:2)))`. You can't use `i`, `j`, `k`, or `l` in combination with `indices`.
NULL

#' @name .mcmc
#' @title Shared argument(s)
#'
#' @param mcmc	An object of class `mcmc` or `mcmc.list`, *or* a `list`. If a `list`, the function searches down the first element to see if it can find an `mcmc` or `mcmc.list` object, then uses this if it can.
NULL
