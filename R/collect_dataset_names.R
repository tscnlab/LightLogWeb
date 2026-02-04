collect_dataset_names <- function(datasets, selected_dataset){
  names_datasets <-
    reactiveValuesToList(datasets) |>
    purrr::map_lgl(is.null)
  names_datasets <-
  names_datasets |> subset(subset = !names_datasets) |> names()

  #if dataset_names updates, make certain a valid dataset name is chosen
  if (length(names_datasets) == 0) {
    selected_dataset(NULL)
  } else if (is.null(selected_dataset()) || !selected_dataset() %in% names_datasets) {
    selected_dataset(names_datasets[1])
  }
  #return
  names_datasets
}
