
get_path_head <- function(x) strsplit(x,'::', fixed=TRUE)[[1]][1]

get_path_tail <- function(x) {
  head <- get_path_head(x)
  start <- nchar(head)+2+1
  stop <- nchar(x)
  return(substr(x,start,stop))
}

find_elements <- function(context, tag) {
  paths <- context[['index']][grepl(pattern=tag, x=context[['index']])]
  return(paths)
}

grab_element <- function(context, path = '') {
  if (path == '') 
    return(context)
  else {
    path_head <- get_path_head(path)
    path_tail <- get_path_tail(path)
    if (is.list(context)) {
      o <- grab_element(context[[path_head]], path_tail)
      return(o)
    } else 
      stop("Path does not lead to an element.")
  }
  stop("Bad bird Max, bad bird!")
}

get_elements <- function(context, tag) {
  paths <- find_elements(context, tag)
  elements <- list()
  for (path in paths) {
    elements[[path]] <- grab_element(context, path)
  }
  return(elements)
}




