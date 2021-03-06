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

#' Make an \code{alist} with the specified \code{names}.
#'
#' @param names A list or character vector with argument names.
#' @return An \code{alist} containing named elements decided by \code{names}.

make_alist = function(names) {
  lazy_list = lapply(1:length(names), function(x) substitute())
  names(lazy_list) = unlist(names)
  lazy_list
}

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


#' Match formals to a list of arugments
#'
#' Given a list \code{formals} of function arguments, check whether the
#' arguments provided in \code{...} and matches the signature given by the list.
#' If it matches, return the arguments in the same order as formals. If it does
#' not match, raise an exception.
#'
#' @param formals A list of formal arguments to match.
#' @param ... Arguments to match against \code{formals}.
#' @param .args Optional list of arguments to match against \code{formals}.
#' @param .force Logical; If \code{TRUE}, forces evaluation of the arguments.
#' @return A unlisted list containing correctly ordered arguments from
#' \code{...} and \code{.args}. Throws an error if this is not possible to do.

match_formals = function(formals, ..., .args = NULL, .force = TRUE) {
  fun = function() {
    args = arguments()

    evaled_args = sapply(names(formals), function(arg) {
      eval(force(parse(text = arg)[[1]]))
    })

    names(evaled_args) = names(formals)

    if(.force) evaled_args else args
  }

  formals(fun) = formals
  unlist(do_call(fun, .args = .args, ...))
}

#' Get term labels from a formula.
#'
#' The function \code{attr(stats::terms(formula), "term.labels")} does not include
#' the intercept.
#'
#' @param formula A \code{formula} object.
#' @param include_intercept
#' @return A character vector including all terms of the formula.

formula_labels = function(formula, include_intercept = TRUE) {

  msg = do_call(paste, .args = c("The object 'formula' must be an object of",
                                  "class 'formula'. Got an object of class:",
                                  class(formula)), collapse = " ")
  assertthat::assert_that(inherits(formula, "formula"), msg = msg)

  labels = attr(stats::terms(formula), "term.labels")

  if(!include_intercept) return(labels)

  if(attr(stats::terms(formula), "intercept") == 1) c("(Intercept)", labels)
  else labels

}

#' Returns the argument list given to the current function
#'
#' @return The argument list

arguments = function() as.list(match.call(sys.function(-1), sys.call(-1))[-1])
