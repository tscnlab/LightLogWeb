import_notification <- function(file, device, tz, dataset_name) {
  if(!list(file, device, tz) |>
     purrr::map_lgl(is.null) |>
     any() & device != "" & dataset_name != ""
  ){
    showNotification(
      paste("Import is in progress. A message will be shown upon successfull import."),
      type = "message",
      duration = 5
    ) } else {
      showNotification(
        "Please specify files, device, time zone, and name",
        type = "error",
        duration = 5
      )
    }
}

import_add_notification <- function(import_result) {
  res <- tryCatch(
    import_result(),
    error = function(e) NULL
  )

  if (is.null(res) || is.null(res$data) || !is.data.frame(res$data)) {
    showNotification(
      "Please provide a valid import.",
      type = "warning",
      duration = 5
    )}

}
