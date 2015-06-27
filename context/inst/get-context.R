library(context)
whole_context <- fromJSON(descending_search('context.json'), simplifyVector=FALSE)
local_context <- find_named_context(whole_context, "wb-ats-data")
loaded_libraries <- load_libraries(local_context)
opts <- set_options(context)
context <- process_context(local_context)
link <- db_connector(context[['credentials']])


