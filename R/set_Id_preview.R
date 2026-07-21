set_Id_preview <- function(id, Id_manual, Id_extract, filename) {
  mapping <- new_filename_id_mapping(
    original_names = filename,
    id_mode = id,
    manual_id = Id_manual,
    extract_pattern = Id_extract
  )
  mapping$proposed_id
}
