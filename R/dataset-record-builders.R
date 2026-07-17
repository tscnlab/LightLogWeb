installed_package_version <- function(package) {
  tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(cnd) NA_character_
  )
}

new_imported_dataset_record <- function(imported) {
  if (!is.list(imported)) {
    abort_llw("`imported` must be a named list.", type = "validation")
  }
  required <- c("name", "data", "device", "tz", "variable")
  missing <- setdiff(required, names(imported))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "Imported dataset is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  arguments <- imported$import_arguments %||% list()
  source_files <- imported$source_files %||% NULL
  if (
    is.null(source_files) ||
      !inherits(source_files, "llw_staged_uploads")
  ) {
    abort_llw(
      "Imported source-file provenance is missing.",
      type = "validation"
    )
  }
  manifest <- new_source_manifest(
    source_type = "raw_upload",
    original_filenames = source_files$original_name,
    hashes = source_files$sha256,
    import_arguments = arguments,
    source_timezone = imported$tz,
    details = list(
      device = imported$device,
      size_bytes = source_files$size_bytes,
      session_files = source_files$staged_path
    )
  )
  new_dataset_record(
    raw_data = imported$data,
    display_name = imported$name,
    source_manifest = manifest,
    factual_metadata = list(device = imported$device),
    analysis_settings = list(
      primary_variable = imported$variable,
      analysis_timezone = imported$tz
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR")
    )
  )
}

sample_dataset_record <- function() {
  data <- LightLogR::sample.data.environment
  manifest <- new_source_manifest(
    source_type = "package_sample",
    source_timezone = "Europe/Berlin",
    details = list(
      package = "LightLogR",
      object = "sample.data.environment"
    )
  )
  new_dataset_record(
    raw_data = data,
    display_name = "sample.data.environment",
    source_manifest = manifest,
    factual_metadata = list(
      device = "ActLumus",
      latitude = 48.52,
      longitude = 9.06,
      country = "Germany",
      site = "T\u00fcbingen"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "Europe/Berlin",
      display_scale = "symlog"
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR")
    )
  )
}
