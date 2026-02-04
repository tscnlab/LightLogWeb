adding_dataset <- function(datasets, new_dataset) {
  dataset_name <- new_dataset()$name

  if (is.null(dataset_name) || dataset_name == "") {
    showNotification("Please provide a dataset name before saving.", type = "error")
    return()
  }

  if (!is.null(datasets[[dataset_name]])) {
    showNotification("A dataset with this name already exists.", type = "error")
    return()
  }

  datasets[[dataset_name]] <- list(
    data = new_dataset()$data,
    metadata = list(variable = NULL,
                    variable_name = "",
                    variable_unit = "",
                    tz = new_dataset()$tz,
                    device = new_dataset()$device)
  )

  showNotification(
    p("Dataset ", strong(dataset_name), " added to the library."),
    type = "message")

  nav_select("main_nav", selected = "prepare")

}
