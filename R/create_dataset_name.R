create_dataset_name <- function(device){
  paste0(device,
         ".",
         lubridate::now() |>
           lubridate::format_ISO8601(precision = "ymdhm")
  ) |> make.names()
}
