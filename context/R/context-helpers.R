find_contexts <- function(context, requested) {
  found <- list()
  for ( i in seq_along(context)) {
    name <- context[[i]][['name']]
    if (name %in% requested)
      found[[name]] <- context[[i]]
    if (all(requested %in% names(found)))  
      break ## Break when all requested are found.
  }
  for ( i in requested) {
    if (!(i %in% names(found))) {
      found[[i]] <- NA
    }
  }
  return(found)
}

merge_contexts <- function(context_list) {
  context <- NULL
  for ( name in names(context_list)) {
    if (is.null(context)) {
      context <- context_list[[name]]
    } else {
      for (section in names(context_list[[name]])) {
        context[[section]] <- context_list[[name]][[section]]
      }
    }
  }
  return(context)
}

load_libraries <- function(context) {
  if (!has_require_library(context)) return(NULL)
  required_libraries <- context[['require']][names(context[['require']]) == 'library']
  have_libraries <- sapply(required_libraries, require, character.only=TRUE)
  names(have_libraries) <- required_libraries
  return(have_libraries)
}

define_functions <- function(context, envir=.GlobalEnv) {
  if (!has_require_function(context)) return(NULL)
  required_functions <- context[['require']][names(context[['require']]) == 'function']
  have_functions <- rep(FALSE,length(required_functions))
  names(have_functions) <- required_functions
  for (i in seq_along(required_functions)) {
    name <- required_functions[[i]][['name']]
    lib <- required_functions[[i]][['library']]
    f <- eval(parse(text=paste(lib,name, sep=':::')))
    assign(x=name, value=f, envir=envir)
    if (class(f) != 'try-error') have_functions[i] <- TRUE
  }
  return(have_functions)
}

set_options <- function(context) {
  if ('options' %in% names(context)) 
    do.call(what=options, args=context[['options']])
  return(options())
}
  

