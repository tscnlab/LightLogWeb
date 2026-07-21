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
  quality <- imported$quality %||%
    summarize_raw_import_quality(imported$data, imported$tz)
  if (!inherits(quality, "llw_raw_import_quality")) {
    abort_llw(
      "Imported raw-data quality provenance is invalid.",
      type = "validation"
    )
  }
  eligibility <- imported$eligibility %||% quality$eligibility
  if (!inherits(eligibility, "llw_variable_eligibility")) {
    abort_llw(
      "Imported primary-variable eligibility is invalid.",
      type = "validation"
    )
  }
  selected <- eligibility[
    eligibility$variable == imported$variable,
    ,
    drop = FALSE
  ]
  if (nrow(selected) != 1L || !isTRUE(selected$eligible[[1L]])) {
    abort_llw(
      paste0("Primary variable `", imported$variable, "` is not eligible."),
      type = "validation",
      public_message = paste0(
        "`",
        imported$variable,
        "` cannot be used as the primary measurement. Review the eligibility reasons and choose another variable."
      )
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
      version = imported$version %||% arguments$version %||% "default",
      size_bytes = source_files$size_bytes,
      preflight = imported$preflight %||% NULL
    )
  )
  new_dataset_record(
    raw_data = imported$data,
    display_name = imported$name,
    source_manifest = manifest,
    factual_metadata = list(
      device = imported$device,
      source_files = source_files$original_name
    ),
    analysis_settings = list(
      primary_variable = imported$variable,
      analysis_timezone = imported$tz
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR"),
      raw_import_quality = quality,
      primary_variable_eligibility = eligibility
    )
  )
}

sample_dataset_record <- function() {
  data <- LightLogR::sample.data.environment
  quality <- summarize_raw_import_quality(data, "Europe/Berlin")
  manifest <- new_source_manifest(
    source_type = "package_sample",
    source_timezone = "Europe/Berlin",
    details = list(
      package = "LightLogR",
      package_version = installed_package_version("LightLogR"),
      object = "sample.data.environment",
      units = list(MEDI = "lux (melanopic EDI)")
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
      site = "T\u00fcbingen",
      variables = list(
        MEDI = list(
          label = "Melanopic equivalent daylight illuminance",
          unit = "lux"
        )
      )
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "Europe/Berlin",
      display_scale = "symlog"
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR"),
      raw_import_quality = quality,
      primary_variable_eligibility = quality$eligibility
    )
  )
}

lightlogweb_development_root <- function(start = getwd()) {
  assert_scalar_string(start, "start")
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    description <- file.path(current, "DESCRIPTION")
    if (file.exists(description)) {
      package_line <- readLines(description, n = 1L, warn = FALSE)
      if (identical(package_line, "Package: LightLogWeb")) return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) return(normalizePath(start, winslash = "/"))
    current <- parent
  }
}

development_large_dataset_path <- function(project_root = NULL) {
  if (is.null(project_root)) {
    project_root <- lightlogweb_development_root()
  } else {
    assert_scalar_string(project_root, "project_root")
  }
  file.path(
    project_root,
    "dev",
    "fixtures",
    "melidos-iztech-light-glasses-1minute.rds"
  )
}

melidos_iztech_snapshot_contract <- function() {
  list(
    sha256 = "sha256:939bea8c78578db3b9840e20a505b0366e3c3ee347c1d838f2c6781844a65ab8",
    rows = 151200L,
    columns = 37L,
    participants = 17L,
    timezone = "Europe/Istanbul",
    primary_variable = "MEDI",
    names = c(
      "Id",
      "Datetime",
      "EVENT",
      "TEMPERATURE",
      "ORIENTATION",
      "PIM",
      "PIMn",
      "TAT",
      "TATn",
      "ZCM",
      "ZCMn",
      "LIGHT",
      "IR.LIGHT",
      "CAP_SENS_1",
      "CAP_SENS_2",
      "F1",
      "F2",
      "F3",
      "F4",
      "F5",
      "F6",
      "F7",
      "F8",
      "MEDI",
      "CLEAR",
      "STATE",
      "MS",
      "EXT.TEMPERATURE",
      "AMB.LIGHT",
      "RED.LIGHT",
      "GREEN.LIGHT",
      "BLUE.LIGHT",
      "UVA.LIGHT",
      "UVB.LIGHT",
      "file.name",
      "position",
      "is.implicit"
    )
  )
}

melidos_iztech_dataset_record <- function(
  path = development_large_dataset_path()
) {
  assert_scalar_string(path, "path")
  if (!file.exists(path) || dir.exists(path)) {
    abort_llw(
      paste0("Development dataset snapshot is missing at `", path, "`."),
      type = "unavailable_feature",
      public_message = paste(
        "The optional IZTECH development snapshot is not available in this checkout.",
        "Use the deterministic LightLogR sample or regenerate the development fixture."
      )
    )
  }
  contract <- melidos_iztech_snapshot_contract()
  snapshot_sha256 <- sha256_file(path)
  if (!identical(snapshot_sha256, contract$sha256)) {
    abort_llw(
      paste0(
        "The IZTECH development snapshot checksum is `",
        snapshot_sha256,
        "`; expected `",
        contract$sha256,
        "`."
      ),
      type = "resource",
      public_message = paste(
        "The optional IZTECH development snapshot does not match its pinned checksum.",
        "Restore or deliberately regenerate the fixture before use."
      )
    )
  }
  data <- tryCatch(readRDS(path), error = identity)
  if (inherits(data, "error")) {
    abort_llw(
      paste0("Could not read development snapshot: ", conditionMessage(data)),
      type = "resource",
      public_message = "The optional IZTECH development snapshot is unreadable; regenerate it before use.",
      diagnostics = list(original_message = conditionMessage(data)),
      parent = data
    )
  }
  if (
    !is.data.frame(data) ||
      !identical(dim(data), c(contract$rows, contract$columns)) ||
      !identical(names(data), contract$names) ||
      !inherits(data$Datetime, "POSIXct") ||
      !identical(lubridate::tz(data$Datetime), contract$timezone) ||
      length(unique(data$Id)) != contract$participants
  ) {
    abort_llw(
      "The IZTECH development snapshot does not match its pinned data contract.",
      type = "validation",
      public_message = paste(
        "The optional IZTECH development snapshot has an unexpected shape,",
        "schema, participant count, or source time zone. Restore or deliberately",
        "regenerate the fixture before use."
      )
    )
  }
  timezone <- contract$timezone
  quality <- summarize_raw_import_quality(data, timezone)
  eligible <- quality$eligibility$variable[quality$eligibility$eligible]
  if (length(eligible) == 0L) {
    abort_llw(
      "The IZTECH development snapshot has no eligible primary measurement.",
      type = "validation"
    )
  }
  if (!contract$primary_variable %in% eligible) {
    abort_llw(
      "The pinned IZTECH primary measurement is not eligible.",
      type = "validation"
    )
  }
  primary <- contract$primary_variable
  filename <- basename(path)
  manifest <- new_source_manifest(
    source_type = "development_snapshot",
    original_filenames = filename,
    hashes = snapshot_sha256,
    import_arguments = list(
      package = "melidosData",
      package_version = "1.0.6",
      call = "melidosData::load_data('light_glasses_1minute', site = 'IZTECH')"
    ),
    source_timezone = timezone,
    details = list(
      site = "IZTECH",
      source_repository = "MeLiDosProject/DidikogluEtAl_Dataset_2025",
      source_dataset_version = "1.0.1",
      source_commit = "1f83aa1563603610a4f43afe2a0fbe30f805191c",
      source_blob = "8a6aef8c7c0e842eaa167d22fef634848b57508a",
      source_url = paste0(
        "https://raw.githubusercontent.com/MeLiDosProject/",
        "DidikogluEtAl_Dataset_2025/",
        "1f83aa1563603610a4f43afe2a0fbe30f805191c/",
        "data/imported/light/",
        "light_glasses_1minute.RData"
      ),
      license = "CC BY 4.0",
      license_url = "https://creativecommons.org/licenses/by/4.0/",
      attribution = paste(
        "Didikoglu, A., Akgun, S. G., Aydin, S. N., Kayar, Z.,",
        "Zauner, J., & Spitschan, M. (2025). Personal light exposure",
        "dataset for Izmir, T\u00fcrkiye (Version 1.0.1).",
        "https://doi.org/10.5281/zenodo.16568109"
      ),
      snapshot_transform = paste(
        "Loaded through melidosData 1.0.6 and serialized as an xz-compressed",
        "RDS version 3 file; observations and columns were not changed."
      ),
      purpose = "Milestone 3 large development example; excluded from package builds"
    )
  )
  new_dataset_record(
    raw_data = data,
    display_name = "IZTECH light glasses (1 minute)",
    source_manifest = manifest,
    factual_metadata = list(
      study = "MeLiDos",
      site = "IZTECH",
      institution = "Izmir Institute of Technology",
      country = "T\u00fcrkiye"
    ),
    analysis_settings = list(
      primary_variable = primary,
      analysis_timezone = timezone,
      display_scale = "symlog"
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR"),
      melidosData_version = "1.0.6",
      snapshot_sha256 = snapshot_sha256,
      raw_import_quality = quality,
      primary_variable_eligibility = quality$eligibility
    )
  )
}
