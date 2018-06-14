load.csv <- function(data.path,
                      fn,
                      header = TRUE){
  ## Load csv data and return the data frame
  read.csv(file.path(data.path, fn),
           comment.char = "#",
           header = header,
           stringsAsFactors = FALSE)
}
