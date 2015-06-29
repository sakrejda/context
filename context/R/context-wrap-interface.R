
process_context <- function(
  context='context.json', requested=c('project::root', 'language::R')
) {
  whole_context <- fromJSON(descending_search(context), simplifyVector=TRUE, simplifyDataFrame=FALSE)
  context_list <- find_contexts(whole_context, requested)
  context_data <- merge_contexts(context_list)
  context <- context_data[['master']]
  context[['name']] <- context[['name']][length(context[['name']])]
  context_graph <- context_data[['graph']]
  norm_root <- normalizePath(context[['root']])
  group <- context[['group']]
  name <- context[['name']]
  context[['data']] <- file.path(norm_root, context[['data_dir']], group, name) %>% test_normalize_create_dir
  context[['input']] <- file.path(norm_root, context[['input_dir']], group, name) %>% test_normalize_create_dir
  context[['processed_input']] <- file.path(context[['input']], 'processed') %>% test_normalize_create_dir
  context[['output']] <- file.path(norm_root, context[['output_dir']], group, name) %>% test_normalize_create_dir
  context[['processed_output']] <- file.path(context[['output']], 'processed') %>% test_normalize_create_dir
  return(context)
}



