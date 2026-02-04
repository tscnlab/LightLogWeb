get_versions <- function(device) {
  supported_versions(device) |>
    dplyr::pull(Version) |>
    c("default") |>
    rev()
}

get_version_description <- function(device, version) {
  supported_versions(device) |>
    dplyr::filter(Version == version | ((version == "default") & Default)) |>
    dplyr::select(Version, Description) |>
    dplyr::mutate(.keep = "none",
                  desc = paste(paste0("v.", Version),
                               Description, sep = ": ")
                  ) |>
    dplyr::pull(desc)
}
