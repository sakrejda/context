path_split <- function (path) {
    path <- normalizePath(path)
    path <- strsplit(path, "/", fixed = TRUE)
    return(path)
}

descending_search <- function (file = "context.json", directory=getwd()) {
  file <- file.path(directory, file)
  if (file.exists(file)) {
    return(file)
  }
  else {
    new_directory <- path_split(directory)[[1]]
    nc <- length(new_directory)
    if (nc < 3)          
      stop("model-data.sh not found.")
    new_directory <- do.call(
      what = file.path, 
      args = as.list(new_directory[1:(nc-1)])
    )
    return(descending_search(file=basename(file), directory=new_directory))
  }
}

find_named_context <- function(context, name) {
  root_context <- NULL
  local_context <- NULL
  for ( i in seq_along(context)) {
    if (!is.null(context[[i]][['name']]) && context[[i]][['name']] == name)
      local_context <- context[[i]]
    if (!is.null(context[[i]][['type']]) && context[[i]][['type']] == 'root')
      root_context <- context[[i]]
    if (!is.null(local_context) && !is.null(root_context))
      break
  }
  if (is.null(local_context))
    stop(paste0("Context named '", name, "' is missing."))
  if (is.null(root_context))
    stop(paste0("Root context is missing."))
  merged_context <- root_context   
  for (name in names(local_context))
    merged_context[[name]] <- local_context[[name]]
  return(merged_context)
}

load_libraries <- function(context) {
  required_libraries <- context[['require']][names(context[['require']]) == 'library']
  have_libraries <- sapply(required_libraries, require, character.only=TRUE)
  names(have_libraries) <- required_libraries
  return(have_libraries)
}

define_functions <- function(context, envir=.GlobalEnv) {
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
  
test_normalize_create_dir <- function(path) {
  if (file.exists(path)) {
    return(normalizePath(path))
  } else {
    dir.create(path=path, showWarnings=FALSE, recursive=TRUE, mode="0750")
    return(normalizePath(path))
  }
}

process_context <- function(context) {
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

