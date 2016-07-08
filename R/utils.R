is_empty_string = function(x)
{
  if (!is.character(x))
    stop("empty_string called on non-character object")
  return(is.na(x) | (!nzchar(x)))
}

parse_deparse = function(x, trim_quotes = TRUE)
{
  x = extract_enlisted(as.character(x))
  if (is.na(x)) stop("Invalid input string")
  x = unlist(stringr::str_split(x, ", "))
  x = stringr::str_split(x, " = ")
  x = sapply(x, function(a) switch(length(a), c(NA, a), a))
  x1 = if(trim_quotes) trim_quotes(x[1, ]) else x[1, ]
  x2 = if(trim_quotes) trim_quotes(x[2, ]) else x[2, ]
  x = set_names(x2, x1)
  return(x)
}

trim_quotes = function(x)
{
  quotes = "[\"'`]+"
  x = stringr::str_trim(x)
  pattern = stringr::str_c("\\A", quotes, "|", quotes, "\\Z")
  stringr::str_replace_all(x, pattern, "")
}

extract_enlisted = function(x)
{
  x = stringr::str_extract(x, "\\([^\\(\\)]*\\)")
  stringr::str_replace_all(x, "[\\(\\)]", "")
}

set_names = function(x, n, keep_existing = FALSE)
{
  nn = names(x)
  n = as.character(n)
  n = rep(n, length.out = length(x))
  if (keep_existing)
    if (is.null(nn))
      return(set_names(x, n, FALSE))
    else
      names(x) = ifelse(is_empty_string(nn), n, nn)
  else
    names(x) = n
  return(x)
}

check_function = function(x, envir = parent.frame())
{
  if (!is.character(x))
    x = deparse(substitute(x))
  x = tryCatch(get(x, envir = envir, mode = "function"),
               error = function(e) NA)
  return(is.function(x))
}

get_formals = function(x, envir = parent.frame())
{
  if (!is.character(x))
    x = deparse(substitute(x))
  x = get(x, mode = "function", envir = envir)
  eval(formals(x), envir = envir)
}

is_primitive = function(x, envir = parent.frame())
{
  if (!is.character(x))
    x = deparse(substitute(x))
  x = get(x, mode = "function", envir = envir)
  return(is.primitive(x))
}
