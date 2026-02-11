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

  datasets[[dataset_name]] <-
    build_metadata_hull(data = new_dataset()$data,
                        tz = new_dataset()$tz,
                        ds.name = new_dataset()$name,
                        device = new_dataset()$device,
                        variable = new_dataset()$variable)

  variable <- rlang::sym(new_dataset()$variable)

  datasets[[dataset_name]]$summaries$overview <-
    summary_overview(datasets[[dataset_name]]$data,
                     !!variable,
                     threshold.missing =
                       datasets[[dataset_name]]$metadata$threshold.missing
    )

  datasets[[dataset_name]]$summaries$has_gaps <-
    datasets[[dataset_name]]$data |> has_gaps()

  datasets[[dataset_name]]$summaries$has_irregulars <-
    datasets[[dataset_name]]$data |> has_irregulars()

  datasets[[dataset_name]]$summaries$dominantEpoch <-
    datasets[[dataset_name]]$data |> dominant_epoch()

  showNotification(
    p("Dataset ", strong(dataset_name), " added to the library."),
    type = "message")

  nav_select("main_nav", selected = "dashboard")

}
