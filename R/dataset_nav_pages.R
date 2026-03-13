new_dataset_page <- function(dataset_name, id) {
  list(
    nav_value = paste0("dataset_", id),
    module_id = paste0("dashboard_dataset_", id),
    dataset_name = reactiveVal(dataset_name)
  )
}

reconcile_dataset_pages <- function(pages, dataset_names, selected_dataset = NULL) {
  existing_names <- names(pages)
  added <- setdiff(dataset_names, existing_names)
  removed <- setdiff(existing_names, dataset_names)

  renamed <- NULL
  if (length(added) == 1 &&
      length(removed) == 1 &&
      !is.null(selected_dataset) &&
      identical(selected_dataset, added)) {
    renamed <- list(from = removed, to = added)
  }

  list(
    added = added,
    removed = removed,
    renamed = renamed
  )
}
