#' Variant of do call with that preserves argument names.
#'
#' @param .fn Function to call.
#' @param .args List of arguments to \code{.fn}.
#' @param ... Further arguments to \code{.fn}.
#' @param .env The environment where the call is to be evaluated.
#' @return The effect of calling \code{.fn} with the supplied arguments in the
#' specified environment.

do_call = function(.fn, .args = NULL, ..., .env = parent.frame()) {
  call_ = as.call(c(.fn, .args, alist2(...)))
  eval(call_, envir = .env)
}

#' Make lazy list from arguments
#'
#' Works with passed \code{...} parameters.
#' @param ... Parameters to put into the list.
#' @return A lazy list.

alist2 = function(...) as.list(substitute((...)))[-1]

#' Plot with \code{type = "l", bty = "l"} as standard arguments.
#'
#' @param ... Arguments passed to \code{plot}

lplot = function(...) {
  dots = alist2(...)
  if(is.null(dots$type)) dots$type = "l"
  if(is.null(dots$bty)) dots$bty = "l"
  if(is.null(dots$lwd)) dots$lwd = 1.5
  do_call(plot, dots)
}

#' Jitters data away from 0 and 1.
#'
#' @param data Data to jitter. Contained in [0, 1].
#' @param eps Epsilon to jitter with.
#' @return Jittered data

unit_jitter = function(data, eps = .Machine$double.eps^0.25) {
  one_indices = (data == 1)
  zero_indices = (data == 0)
  data[one_indices] = 1 - runif(sum(one_indices), 0, eps)
  data[zero_indices] = runif(sum(zero_indices), 0, eps)
  data
}

#' Adds named elements to a list when they are not there already.
#'
#' @param input List. The input list to manipulate.
#' @param ... Key value pairs to add to the list provided the key is not already
#' used.
#' @param .eager Logical; Should the \code{value}s be evaluated?
#' @return A modified list.

add_elements = function(input, ..., .eager = TRUE) {
  dots = if(.lazy) list2(...) else list(...)
  names = names(dots)
  N = length(names)

  for(i in 1:N) {
    if(is.null(input[[names[i]]])) input[[names[i]]] = dots[[i]]
  }

  input
}

