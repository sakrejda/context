library(context)
ordered_contexts <- c('project::root','language::R','database::salmonids', 'project::wb-ats-data')
context <- process_context(context='context.json', requested=ordered_contexts)


