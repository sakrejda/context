proper_context_names <- function(names) grepl(pattern='[A-Za-z\\-]+::[A-Za-z\\-]+', x=names)
make_key <- function(type, name) paste(type, name, sep='::')
get_key_components <- function(key) data.frame(key=key) %>% separate(key, c('type','name'), '::', remove=FALSE)

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
  context_list <- list()
  found <- vector()
  for ( i in seq_along(context)) {
    type <- context[[i]][['type']]
    name <- context[[i]][['name']]
    key <- make_key(type, name)
    if (key %in% requested) {
      context_list <- c(context_list,context[i])
      found <- c(found,key) 
    }
    if (all(requested %in% found))  
      break ## Break when all requested are in context_list.
  }
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
  contexts <- list()
  for ( i in seq_along(context_list)) {
    contexts[[i]] <- grow_graph(context_list[[i]], data=data)[['graph']]
  }
  context <- re_tree(data)
  context[['graph']] <- do.call(what=igraph:::union, args=contexts)
  return(context)
}







