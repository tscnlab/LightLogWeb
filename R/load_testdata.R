load_testdata <- function(datasets, selected_dataset, notifications = TRUE) {
  if(notifications) {
  if (!is.null(datasets[["sample.data.environment"]])) {
    showNotification("A dataset with this name already exists.", type = "error")
    return()
  }
  }

  datasets[["sample.data.environment"]] <-
    build_metadata_hull(
      data = LightLogR::sample.data.environment,
      variable = "MEDI",
      variable_name = "melanopic EDI",
      variable_unit = "lx",
      variable_factor = 1,
      variable_offset = 0,
      variable_min = 0,
      variable_max = 10^5,
      variable_scaling = "symlog",
      latitude = 48.52,
      longitude = 9.06,
      country = "Germany",
      site = "T\u{00fc}bingen",
      tz = "Europe/Berlin",
      device = "ActLumus",
      ds.name = "sample.data.environment")

  datasets[["sample.data.environment"]]$summaries$overview <-
    summary_overview(datasets[["sample.data.environment"]]$data,
                     MEDI,
                     c(datasets[["sample.data.environment"]]$metadata$latitude,
                       datasets[["sample.data.environment"]]$metadata$longitude
                       ),
                     threshold.missing =
                       datasets[["sample.data.environment"]]$metadata$threshold.missing
                     )

  datasets[["sample.data.environment"]]$summaries$has_gaps <-
    datasets[["sample.data.environment"]]$data |> has_gaps()

  datasets[["sample.data.environment"]]$summaries$has_irregulars <-
    datasets[["sample.data.environment"]]$data |> has_irregulars()

  datasets[["sample.data.environment"]]$summaries$dominantEpoch <-
    datasets[["sample.data.environment"]]$data |> dominant_epoch()

  if(notifications) {

    selected_dataset("sample.data.environment")
  showNotification(
    p("Test dataset",
             strong("sample.data.environment"),
             "added to the library."),
    type = "message")
  }

}
