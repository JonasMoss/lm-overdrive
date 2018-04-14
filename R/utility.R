#' Variant of do call with that preserves argument names.
#'
#' @param .fn Function to call.
#' @param .args List of arguments to \code{.fn}.
#' @param ... Further arguments to \code{.fn}.
#' @param .env The environment where the call is to be evaluated.
#' @return The effect of calling \code{.fn} with the supplied arguments in the
#' specified environment.

do_call = function(.fn, .args = NULL, ..., .env = parent.frame()) {
  eval(as.call(c(.fn, .args, alist2(...))), envir = .env)
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
  if(is.null(dots$bty))  dots$bty = "l"
  if(is.null(dots$lwd))  dots$lwd = 1.5
  do_call(plot, dots)
}

#' Adds named elements to a list when they are not there already.
#'
#' @param input List. The input list to manipulate.
#' @param ... Key value pairs to add to the list provided the key is not already
#' used.
#' @param .eager Logical; Should the \code{value}s be evaluated?
#' @return A modified list.

add_elements = function(input, ..., .eager = TRUE) {

  dots = if(.lazy) alist2(...) else list(...)
  names = names(dots)
  N = length(names)

  for(i in 1:N) if(is.null(input[[names[i]]])) input[[names[i]]] = dots[[i]]

  input
}

#' Is a formula tow-sided?
#'
#' @param formula A formula.
#' @return Logical; \code{TRUE} if the formula is two-sided, an error otherwise.

is_two_sided = function(formula) {
  formula_str = paste(as.character(formula)[2], "~", as.character(formula)[3])
  msg = paste0("The formula must be two-sided, got '", formula_str, "'.")
  assertthat::assert_that(length(formula) == 3, msg = msg)
}

#' Get the argument list from a \code{call}.
#'
#' @param call An object of class \code{call}.
#' @return The named argument list from the \code{call}.

extract_arguments = function(call) {
  assertthat::assert_that(inherits(call, "call"), msg = paste0("The 'call'",
    " argument must have class 'call', but has class '", class(call), "'."))
  k = length(call) - 1
  arguments = lapply(2:(k + 1), function(i) call[[i]])
  names(arguments) = names(call)[2:(k + 1)]
  arguments
}

#' Get the function \code{name} from a \code{call}.
#'
#' @param call An object of class \code{call}.
#' @return The function of the call \code{call}, a \code{name} object.

extract_function_name = function(call) {
  assertthat::assert_that(inherits(call, "call"), msg = paste0("The 'call'",
    " argument must have class 'call', but has class '", class(call), "'."))
  call[[1]]
}
