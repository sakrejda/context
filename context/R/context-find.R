proper_context_names <- function(names) grepl(pattern='[A-Za-z\\-]+::[A-Za-z\\-]+', x=names)

check_context_names <- function(names) {
  if (!all(proper_context_names(names))) {
    bad_names <- names[!proper_context_names(names)]
    msg <- paste0(
      "Some context names not properly formatted.  Malformed:\n",
      paste0("\t", bad_names, "\n", collapse='')
    )
    stop(msg)
  }
  return(NULL)
}

find_contexts <- function(context, requested) {
  check_context_names(requested)
  context_list <- context[names(context) %in% requested]
  found <- requested[requested %in% names(context_list)]
  if (!all(requested %in% found)) {
    missing_keys <- requested[!(requested %in% found)]
    msg <- paste0(
      "Some context keys not found.  Missing:\n",
      paste0("\t", missing_keys, "\n", collapse='')
    )
    warning(msg)
  }
  return(context_list)
}





