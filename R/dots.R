#' Call functions
#'
#' Pass the elipsis arguemnt to one or more functions
#'
#' @param f vector containing one or more functions or function names
#' @param ... arguments to be passed to the functions named in \code{f}
#' @param consume if true, each function removes is arguments prior to the next
#'   call
#' @param quote passed to \code{\link{do.call}}
#' @param envir passed to \code{\link{do.call}}
#'
#' @details Each function named in \code{f} is executed using
#'   \code{\link{do.call}}. The additional arguments given in "..." will be
#'   matched against the formals of the subfunction. Each function will only be
#'   passed arguments matching its \code{\link{formals}}. Argument matching is
#'   recorded and any unused arguments are returned.
#'
#'   If \code{consume} is true, then each function called in order will remove
#'   its matching arguments from those available to the next function.
#'
#'   The ... arguments will be evaluated prior to \code{\link{do.call}}, so you
#'   must \code{\link{quote}} them if you want to avoid early evaluation.
#'
#' @return A list of return values from each function named in \code{f}. Each
#'   list element will be named for the function called or the name used in
#'   constructing the vector \code{f}. All unused arguments will returned in the
#'   "..." list element.
#'
#' @examples
#' f = function(a, b) c(a, b)
#' g = function(c, d) c(c, d)
#'
#' h = function(...) { f(...); g(...) }
#' try({ h(a = 1, b = 2, c = 3, d = 4) })
#'
#' do_dots(c("f", "g"), a = 1, b = 2, c = 3, d = 4, e = 5)
#'
#' j = function(a, d) c(a, d)
#' do_dots(c("f", "j"), a = 1, b = 2, c = 3, d = 4)
#' try({ do_dots(c("f", "j"), a = 1, b = 2, c = 3, d = 4, consume = TRUE) })
#'
#' # primitive functions use implicit ... args
#' do_dots("sum", rnorm(100))
#'
#' @export
do_dots = function(f, ..., consume = FALSE, quote = FALSE, envir = parent.frame())
{
  out = list()
  dots = list(...)
  f = substitute(c(f))
  f = deparse(f, nlines = 1)
  f = parse_deparse(f)
  if (is.null(f) ||
      (!all(sapply(f, check_function, envir = envir))))
        stop("f must be one or more functions or function names")
  f = set_names(f, f, keep_existing = TRUE)
  if (is.null(unlist(dots)))
    return(lapply(f, do.call, args = list(), quote = quote, envir = envir))
  if (is.null(names(dots)))
    dots = list("..." = dots)
  dots = set_names(dots, NA, keep_existing = TRUE)
  i = is.na(names(dots))
  if (any(i))
    dots = c(dots[!i], "..." = set_names(dots[i], ""))
  used = rep(FALSE, length(dots))
  for (fn in names(f))
  {
    ff = f[[fn]]
    if (is_primitive(ff, envir = envir)) args = "..."
    else args = names(get_formals(ff, envir = envir))
    i = names(dots) %in% args
    dots_i = if(consume) dots[(!used) & i] else dots[i]
    j = names(dots_i) %in% "..."
    if (any(j))
      dots_i = c(dots_i[!j], dots_i[[which(j)]])
    out[[fn]] = do.call(ff, dots_i, quote = quote, envir = envir)
    used = used | i
  }
  out[["..."]] = dots[!used]
  return(out)
}

