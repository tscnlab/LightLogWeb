set_Id_preview <- function(id, Id_manual, Id_extract, filename) {
  if(id == "manual") {
    Id_manual

  } else if(id == "extract") {
    Ids <-
      tryCatch({
        filename |>
          tools::file_path_sans_ext() |>
          stringr::str_extract(pattern = Id_extract)
      },
      error = \(x) numeric())
    Ids

  } else if(id == "automated") {
    min_char <-
      filename |>
      tools::file_path_sans_ext() |>
      min_unique_prefix_length()
    Ids <- stringr::str_trunc(filename, min_char, ellipsis = "")
    Ids
  }

}
