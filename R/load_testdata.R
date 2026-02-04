load_testdata <- function(datasets, notifications = TRUE) {
  if(notifications) {
  if (!is.null(datasets[["sample.data.environment"]])) {
    showNotification("A dataset with this name already exists.", type = "error")
    return()
  }
  }

  datasets[["sample.data.environment"]] <-
    list(
      data = LightLogR::sample.data.environment,
      metadata = list(variable = "MEDI",
                      variable_name = "melanopic EDI",
                      variable_unit = "lx",
                      variable_factor = 1,
                      variable_offset = 0,
                      latitude = 48.52,
                      longitude = 9.06,
                      country = "Germany",
                      site = "Tübingen",
                      tz = "Europe/Berlin",
                      device = "ActLumus"
                      )
    )
  if(notifications) {
  showNotification(
    p("Test dataset",
             strong("sample.data.environment"),
             "added to the library."),
    type = "message")
  }

}
