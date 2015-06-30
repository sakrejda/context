#context_list_side_effects <- function(context, BLAH) {
#      ## In other words, if a function called "do_tag" is defined,
#      ## use it on the sub-context specified by type and name.
#      f <- try(eval(parse(text=paste('do',t,n, sep='_'))), silent=TRUE)
#        if (class(f) != 'try-error' && is.function(f)) 
#          f(context=context_list[[t]][[n]][[i]])
#      if (class(f) != 'try-error' && is.function(f)) f(finalize=TRUE)
#}


merge_contexts <- function(context_list) {
  data <- new.env()
  contexts <- grow_graph(context_list, data=data)[['graph']]
  context <- re_tree(data)
  context[['data']] <- data
  context[['index']] <- ls(data)
  context[['graph']] <- contexts
  return(context)
}







