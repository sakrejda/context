library(context)
ordered_contexts <- c('root','R','wb-ats-data')
context <- process_context(context='context.json', requested=ordered_contexts)
link <- db_connector(context[['credentials']])


