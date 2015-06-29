flatten_context <- function(context) {
  flat_context <- unlist(context)
  names(flat_context) <- strsplit(x=names(flat_context), split='.', fixed=TRUE) %>% sapply(`[`,2)
  return(flat_context)
}

process_context <- function(
  context='context.json', requested=c('project::root', 'language::R')
) {
  whole_context <- fromJSON(descending_search(context), simplifyVector=TRUE, simplifyDataFrame=FALSE)
  context_list <- find_contexts(whole_context, requested)
  context_data <- merge_contexts(context_list)
  context <- context_data[['master']]
  context[['name']] <- context[['name']][length(context[['name']])]
  context_graph <- context_data[['graph']]

  flat_project <- flatten_context(context[['project']])
  if (any(names(flat_project) %in% names(context))) {
    perps <- names(flat_project)[names(flat_project) %in% names(context)]
    msg <- paste("Name clash between 'project' context and root:\n",
           paste0("\t", perps, collapse="\n"), sep='')
    warning(msg)
  }
  context <- c(flat_project, context)
  norm_root <- normalizePath(context[['root']])
  if ('group' %in% names(context))
    group <- context[['group']]
  else 
    group <- ''
  name <- names(context[['project']])[names(context[['project']]) != 'root'][1]
  context[['data']] <- file.path(norm_root, context[['data_dir']], group, name) %>% test_normalize_create_dir
  context[['input']] <- file.path(norm_root, context[['input_dir']], group, name) %>% test_normalize_create_dir
  context[['processed_input']] <- file.path(context[['input']], 'processed') %>% test_normalize_create_dir
  context[['output']] <- file.path(norm_root, context[['output_dir']], group, name) %>% test_normalize_create_dir
  context[['processed_output']] <- file.path(context[['output']], 'processed') %>% test_normalize_create_dir
  context[['graph']] <- context_graph
  return(context)
}



