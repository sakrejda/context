
has_require <- function(context) {
  if ('require' %in% names(context)) 
    return(TRUE)
  else
    return(FALSE)
}

has_require_library <- function(context) {
  if (has_require(context) && ('library' %in% names(context[['require']]))) 
    return(TRUE)
  else
    return(FALSE)
}

has_require_function <- function(context) {
  if (has_require(context) && ('function' %in% names(context[['require']]))) 
    return(TRUE)
  else
    return(FALSE)
}

