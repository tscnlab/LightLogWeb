get_versions <- function(device) {
  if (
    is.null(device) ||
      !is.character(device) ||
      length(device) != 1L ||
      !nzchar(device)
  ) {
    return(stats::setNames("", "Select a device first"))
  }
  versions <- raw_import_supported_versions(device)
  labels <- paste0(
    versions$Version,
    ifelse(versions$Default, " (current LightLogR default)", "")
  )
  c(
    "Use the current LightLogR default" = "default",
    stats::setNames(versions$Version, labels)
  )
}

get_version_description <- function(device, version) {
  if (
    is.null(device) ||
      !is.character(device) ||
      length(device) != 1L ||
      !nzchar(device)
  ) {
    return("Select a device before choosing a format version.")
  }
  versions <- raw_import_supported_versions(device)
  version <- normalize_raw_import_version(device, version)
  selected <- if (identical(version, "default")) {
    versions[versions$Default, , drop = FALSE]
  } else {
    versions[versions$Version == version, , drop = FALSE]
  }
  if (nrow(selected) == 0L) {
    return("No version description is available.")
  }
  paste0(
    "LightLogR format ",
    selected$Version[[1L]],
    ": ",
    selected$Description[[1L]]
  )
}
