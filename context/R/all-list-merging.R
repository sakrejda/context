
grow_graph <- function(x, g=NULL, last='master', data=new.env()) {
  if (is.null(g)) {
    g <- make_empty_graph()
    g <- add_vertices(g,1,attr=list(name=last))
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      node_name <- paste(last,names(x)[i],sep='::')
      g <- add_vertices(g,1,attr=list(name=node_name))
      g <- add_edges(g,c(last,node_name))
      g <- grow_graph(x[[i]],g,last=node_name, data=data)[['graph']]
    }
  } else {
    if (!is.null(x) && !is.environment(x)) {
      old_x <- try(get(x=last, envir=data), silent=TRUE)
      if (class(old_x) != 'try-error')
        assign(x=last, value=c(old_x,x), envir=data)
      else
        assign(x=last, value=x, envir=data)
    }
  }
  return(list(graph=g, data=data))
}

## #ReallyUglyRFunctions
re_tree <- function(data) {
  tree <- list()
  node_names <- ls(data) 
  split_names <- strsplit(x=node_names, split='::', fixed=TRUE)
  for ( i in seq_along(split_names)) {
    for (j in 1:length(split_names[[i]])) {
      check_this <- paste0(
      "is.null(tree[['", paste(split_names[[i]][1:j], collapse="']][['"), "']])")
      do_this <- paste0(
      "tree[['", paste(split_names[[i]][1:j], collapse="']][['"), "']] <- list()")
      if(eval(parse(text=check_this))) {
        eval(parse(text=do_this))
      }
    }
    do_this <- paste0(
      "tree[['", paste(split_names[[i]], collapse="']][['"), "']] <- get(x=node_names[i], envir=data)")
    eval(parse(text=do_this))
  }
  return(tree)
}




