flatten_context <- function(context) {
  flat_context <- unlist(context)
  names(flat_context) <- strsplit(x=names(flat_context), split='.', fixed=TRUE) %>% sapply(`[`,2)
  return(flat_context)
}

process_context <- function(
  context='context.json', requested=c('project::root', 'language::R'), 
  modules=list(ALL_project_to_head, ALL_project_paths, R_options, R_libraries, R_functions)
) {
  whole_context <- fromJSON(descending_search(context), simplifyVector=TRUE, simplifyDataFrame=FALSE)
  context_list <- find_contexts(whole_context, requested)
  context_data <- merge_contexts(context_list)
  context <- context_data[['master']]
  context[['name']] <- context[['name']][length(context[['name']])]
  context[['graph']] <- context_data[['graph']]
  context[['values']] <- context_data[['data']]
  context[['index']] <- substr(context_data[['index']],9,nchar(context_data[['index']]))

  for ( module in modules ) {
    context <- module(context)
  }

  return(context)
}



