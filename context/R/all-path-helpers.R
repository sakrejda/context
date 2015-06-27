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

test_normalize_create_dir <- function(path) {
  if (file.exists(path)) {
    return(normalizePath(path))
  } else {
    dir.create(path=path, showWarnings=FALSE, recursive=TRUE, mode="0750")
    return(normalizePath(path))
  }
}



