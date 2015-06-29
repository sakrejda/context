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

#context_list_side_effects <- function(context_list) {
#  for (t in seq_along(context_list)) {
#    for (n in seq_along(context_list[[t]])) {
#      ## In other words, if a function called "do_<type>_<name>" is defined,
#      ## use it on the sub-context specified by type and name.
#      f <- try(eval(parse(text=paste('do',t,n, sep='_'))), silent=TRUE)
#      for ( i in seq_along(context_list[[t]][[n]])) {
#        if (class(f) != 'try-error' && is.function(f)) 
#          f(context=context_list[[t]][[n]][[i]])
#      }
#      if (class(f) != 'try-error' && is.function(f)) f(finalize=TRUE)
#    }
#  }
#  return(context)
#}

merge_contexts <- function(context_list) {
  data <- new.env()
  contexts <- grow_graph(context_list, data=data)[['graph']]
  context <- re_tree(data)
  context[['graph']] <- contexts
  return(context)
}







