prepare_quality_metrics_general <- function(dataset, data) {

  #checks for gap and epoch
  data$has_gaps <- dataset()$data |> has_gaps()

  data$has_irregulars <- dataset()$data |> has_irregulars()

  data$dominantEpoch <-
    dataset()$data |>
    dominant_epoch() |>
    dplyr::count(dominant.epoch, sort = TRUE)
}
