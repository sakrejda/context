load_libraries <- function(libraries) {
  if (length(libraries) == 0) return(NULL)
  have_libraries <- sapply(libraries, require, character.only=TRUE)
  names(have_libraries) <- libraries
  return(have_libraries)
}

define_functions <- function(functions, envir=.GlobalEnv) {
  have_functions <- rep(FALSE,length(functions))
  names(have_functions) <- functions
  for (i in seq_along(functions)) {
    f <- try(eval(parse(text=functions[i])), silent=TRUE)
    if (class(f) != 'try-error') {
      have_functions[i] <- TRUE
      assign(x=name, value=f, envir=envir)
    }
  }
  return(have_functions)
}

set_options <- function(opts) {
  do.call(what=options, args=opts)
  return(options())
}
  
make_db_connections <- function(credentials) {
  link <- db_connector(credentials)
}

reconcile_libraries <- function(library_list) {
  load <- library_list[!grepl('^-',library_list)]
  drops <- library_list[grepl('^-', library_list)] %>% substr(start=2,stop=nchar(.)-1)
  do_load <- load[!(load %in% drops)]
  return(do_load)
}

reconcile_functions <- reconcile_libraries
reconcile_options <- function(option_list) return(option_list)

## This is funny.  Do you remember what it does?
remembrator_factory <- function() {
  remembrator <- function(f, remember) {
    if (!is.null(remember)) {
      for (r in remember) {
        attempt <- class(try(expr=get(x=r, envir=environment(), inherits=FALSE), silent=TRUE))
        if (attempt == 'try-error')
          assign(x=r, value=list(), envir=environment())
      }
    }
    if (is.list(f)) {
      for ( i in seq_along(f) ) {
        environment(f[[i]]) <- environment()
      }
    } else {
      environment(f) <- environment()
    }
    return(f)
  }
  return(remembrator)
}

language_R_remembrator <- remembrator_factory()

do_language_R <- function(context=NULL, finalize=FALSE) {
  if (is.null(context) && !isTRUE(finalize)) return()
  if (!is.null(context)) {
    new_libraries <- context[['require']][names(context[['require']]) == 'library']
    library_list <<- c(library_list,new_libraries)
    new_functions <- context[['require']][names(context[['require']]) == 'function']
    function_list <<- c(function_list, new_functions)
    option_list <<- c(option_list, context[['options']])
  }
  library_list <<- reconcile_libraries(library_list)
  function_list <<- reconcile_functions(function_list)
  option_list <<- reconcile_options(option_list)
  if (isTRUE(finalize)) {  
    loaded_libs <- load_libraries(library_list)   
    defined_functions <- define_functions(function_list)
    defined_options <- set_options(option_list)
  }
}

do_language_R <- language_R_remembrator(f=do_language_R, remember=c('library_list', 'function_list', 'option_list'))


