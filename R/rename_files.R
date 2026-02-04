rename_files <- function(file) {
  new_names <- paste0(dirname(file$datapath), "/", file$name)
  file.rename(file$datapath, new_names)
  new_names
}
