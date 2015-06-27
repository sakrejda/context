
process_context <- function(
  context='context.json', requested=c('root', 'R'),
  load_libraries=TRUE, define_functions=TRUE, set_options=TRUE
) {
  whole_context <- fromJSON(descending_search(context), simplifyVector=FALSE)
  context_list <- find_contexts(whole_context, requested)
  context <- merge_contexts(context_list)
  if (load_libraries) load_libraries(context)
  if (define_functions) define_functions(context)
  if (set_options) set_options(context)
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

