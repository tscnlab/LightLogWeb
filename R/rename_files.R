rename_files <- function(file) {
  new_names <- file.path(dirname(file$datapath), file$name)

  renamed <- file.rename(file$datapath, new_names)

  if (any(!renamed)) {
    copied <- file.copy(file$datapath[!renamed], new_names[!renamed], overwrite = TRUE)
    if (any(!copied)) {
      rlang::abort("Could not prepare uploaded files for import.")
    }
    unlink(file$datapath[!renamed])
  }

  new_names
}
