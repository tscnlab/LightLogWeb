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

glc_dataset_display_name <- function(full_import) {
  repository <- full_import$source$repository %||% "GLC package"
  repository <- sub("^.*/", "", as.character(repository))
  revision <- as.character(full_import$source$resolved_revision %||% "")
  revision <- if (nzchar(revision)) substr(revision, 1L, 8L) else "revision"
  paste0(repository, " (", revision, ")")
}

glc_dataset_source_timezone <- function(data) {
  if (!is.data.frame(data) || !"Datetime" %in% names(data)) {
    abort_llw(
      "The complete GLC handoff has no canonical `Datetime` column.",
      type = "validation",
      public_message = paste(
        "The collected GLC result cannot be added to the dashboard because",
        "its canonical timestamp column is unavailable."
      )
    )
  }
  timezone <- lubridate::tz(data$Datetime)
  if (
    !is.character(timezone) ||
      length(timezone) != 1L ||
      is.na(timezone) ||
      !nzchar(timezone) ||
      !timezone %in% OlsonNames()
  ) {
    abort_llw(
      "The complete GLC handoff has no valid IANA timestamp time zone.",
      type = "validation",
      public_message = paste(
        "The collected GLC result has no unambiguous time zone for the",
        "dashboard. No dataset was added."
      )
    )
  }
  timezone
}

glc_dataset_primary_variable <- function(data, full_import, eligibility) {
  if (!inherits(eligibility, "llw_variable_eligibility")) {
    abort_llw(
      "The complete GLC handoff has invalid variable eligibility evidence.",
      type = "validation"
    )
  }
  eligible <- as.character(
    eligibility$variable[eligibility$eligible %in% TRUE]
  )
  preferred <- intersect(
    as.character(full_import$selected_source_variables),
    eligible
  )
  candidates <- unique(c(preferred, eligible))
  if (length(candidates) == 0L) {
    abort_llw(
      "The complete GLC handoff has no eligible numeric measurement.",
      type = "validation",
      public_message = paste(
        "The complete GLC result has no finite numeric measurement that can",
        "open as the dashboard's primary variable. Choose a numeric output",
        "variable and apply the import again."
      )
    )
  }
  candidates[[1L]]
}

glc_dataset_column_metadata <- function(full_import, data_names) {
  columns <- full_import$output_columns
  declared <- full_import$metadata$tables$file_group_variables
  if (!is.data.frame(columns) || !"name" %in% names(columns)) {
    columns <- data.frame(name = character(), stringsAsFactors = FALSE)
  }
  if (!is.data.frame(declared) || !"name" %in% names(declared)) {
    declared <- data.frame(name = character(), stringsAsFactors = FALSE)
  }
  columns <- columns[as.character(columns$name) %in% data_names, , drop = FALSE]
  declared <- declared[
    as.character(declared$name) %in% data_names,
    ,
    drop = FALSE
  ]
  variable_names <- unique(c(
    as.character(columns$name),
    as.character(declared$name)
  ))
  if (length(variable_names) == 0L) return(list())
  first_text <- function(rows, candidates) {
    candidate <- intersect(candidates, names(rows))
    if (length(candidate) == 0L) return(NA_character_)
    for (field in candidate) {
      values <- as.character(rows[[field]])
      values <- values[!is.na(values) & nzchar(trimws(values))]
      if (length(values) > 0L) return(values[[1L]])
    }
    NA_character_
  }
  variables <- lapply(variable_names, function(name) {
    output_rows <- columns[as.character(columns$name) == name, , drop = FALSE]
    declared_rows <- declared[
      as.character(declared$name) == name,
      ,
      drop = FALSE
    ]
    label <- first_text(
      declared_rows,
      c("description", "label", "display_name", "canonical_label")
    )
    if (is.na(label)) {
      label <- first_text(
        output_rows,
        c("description", "label", "display_name", "canonical_label")
      )
    }
    unit <- first_text(
      declared_rows,
      c("unit", "units", "declared_unit")
    )
    if (is.na(unit)) {
      unit <- first_text(
        output_rows,
        c("unit", "units", "declared_unit")
      )
    }
    calibration <- first_text(declared_rows, "calibration")
    metadata <- list()
    if (!is.na(label)) metadata$label <- label
    if (!is.na(unit)) metadata$unit <- unit
    if (!is.na(calibration)) metadata$calibration <- calibration
    metadata
  })
  names(variables) <- variable_names
  variables
}

glc_dataset_location_metadata <- function(full_import) {
  validate_glc_selected_metadata(full_import$metadata)
  datasets <- full_import$metadata$tables$datasets
  context <- full_import$metadata$file_group_context
  from_datasets <- datasets[,
    c(
      "dataset_id",
      "timezone",
      "latitude",
      "longitude"
    ),
    drop = FALSE
  ]
  from_context <- context[,
    c(
      "dataset_id",
      "dataset_timezone",
      "dataset_latitude",
      "dataset_longitude"
    ),
    drop = FALSE
  ]
  names(from_context) <- names(from_datasets)
  selected_ids <- as.character(full_import$metadata$selected_ids$dataset_ids)
  missing_ids <- setdiff(selected_ids, as.character(from_datasets$dataset_id))
  from_context <- from_context[
    as.character(from_context$dataset_id) %in% missing_ids,
    ,
    drop = FALSE
  ]
  locations <- rbind(from_datasets, from_context)
  if (nrow(locations) == 0L) {
    return(data.frame(
      dataset_id = character(),
      timezone = character(),
      latitude = numeric(),
      longitude = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  locations$dataset_id <- as.character(locations$dataset_id)
  locations$timezone <- as.character(locations$timezone)
  locations$latitude <- suppressWarnings(as.numeric(locations$latitude))
  locations$longitude <- suppressWarnings(as.numeric(locations$longitude))
  locations <- locations[!duplicated(locations$dataset_id), , drop = FALSE]
  selected_rows <- match(selected_ids, locations$dataset_id)
  if (anyNA(selected_rows)) {
    abort_llw(
      "Selected GLC dataset metadata is missing a fixed dataset identifier.",
      type = "validation"
    )
  }
  locations[selected_rows, , drop = FALSE]
}

glc_dataset_unique_location <- function(locations) {
  if (!is.data.frame(locations) || nrow(locations) == 0L) return(NULL)
  complete <- !is.na(locations$latitude) & !is.na(locations$longitude)
  coordinates <- unique(locations[
    complete,
    c("latitude", "longitude"),
    drop = FALSE
  ])
  if (nrow(coordinates) != 1L) return(NULL)
  list(
    latitude = as.numeric(coordinates$latitude[[1L]]),
    longitude = as.numeric(coordinates$longitude[[1L]])
  )
}

new_glc_dataset_record <- function(full_import, display_name = NULL) {
  if (!inherits(full_import, "llw_glc_full_import_result")) {
    abort_llw(
      "Only a complete full-import handoff can enter the dataset library.",
      type = "validation",
      public_message = paste(
        "Run Apply full import before adding this selection to the dataset",
        "library. Preview rows are never promoted."
      )
    )
  }
  validate_glc_full_import_result(full_import)
  if (
    !isTRUE(full_import$assurance$fresh_full_read) ||
      !identical(full_import$assurance$preview_result_used, FALSE) ||
      !isTRUE(full_import$assurance$complete_collected_handoff)
  ) {
    abort_llw(
      "The GLC handoff does not prove a fresh complete import.",
      type = "validation",
      public_message = paste(
        "The result cannot enter the dataset library because it is not a",
        "verified fresh full import. Preview rows were not accepted."
      )
    )
  }

  data <- full_import$data
  timezone <- glc_dataset_source_timezone(data)
  quality <- summarize_raw_import_quality(data, timezone)
  eligibility <- quality$eligibility
  primary <- glc_dataset_primary_variable(data, full_import, eligibility)
  display_name <- display_name %||% glc_dataset_display_name(full_import)
  display_name <- clean_dataset_display_name(display_name)
  locations <- glc_dataset_location_metadata(full_import)
  unique_location <- glc_dataset_unique_location(locations)

  reviewed_paths <- as.character(
    full_import$reviewed$file_membership$declared_path
  )
  reviewed_paths <- reviewed_paths[
    !is.na(reviewed_paths) & nzchar(reviewed_paths)
  ]
  size_complete <- identical(
    as.integer(full_import$resource$unknown_file_count),
    0L
  ) &&
    is.numeric(full_import$resource$known_declared_bytes) &&
    length(full_import$resource$known_declared_bytes) == 1L &&
    is.finite(full_import$resource$known_declared_bytes)
  details <- list(
    repository = full_import$source$repository,
    resolved_revision = full_import$source$resolved_revision,
    registry_url = full_import$source$registry_url,
    registry_generated_at = full_import$source$registry_generated_at,
    validation_state = full_import$source$validation_state,
    attestation_verified = full_import$source$attestation_verified,
    schema_version = full_import$source$schema_version,
    glcdp_version = full_import$source$glcdp_version,
    compatibility_id = full_import$fixed$compatibility_id,
    unit_id = full_import$fixed$unit_id,
    file_group_ids = full_import$fixed$file_group_ids,
    actual_input_membership = full_import$actual$input_membership,
    selected_source_variables = full_import$selected_source_variables,
    read_columns = full_import$read_columns,
    output_columns = full_import$output_columns,
    execution_mode = full_import$execution_mode,
    declared_size_evidence = full_import$resource,
    full_import_fingerprint = full_import$fingerprint,
    metadata = list(
      schema = full_import$metadata$metadata_schema,
      version = full_import$metadata$metadata_version,
      upstream_schema = full_import$metadata$upstream_schema,
      upstream_version = full_import$metadata$upstream_version,
      upstream_fingerprint = full_import$metadata$upstream_fingerprint,
      selected_metadata_fingerprint = full_import$metadata$fingerprint,
      table_rows = vapply(
        full_import$metadata$tables,
        nrow,
        integer(1)
      )
    ),
    units = lapply(
      glc_dataset_column_metadata(full_import, names(data)),
      function(metadata) metadata$unit %||% NA_character_
    )
  )
  if (size_complete) {
    details$size_bytes <- as.numeric(
      full_import$resource$known_declared_bytes
    )
  }
  manifest <- new_source_manifest(
    source_type = if (identical(full_import$execution_mode, "runtime")) {
      "glc_registry_full_import"
    } else {
      "glc_fixture_full_import"
    },
    original_filenames = reviewed_paths,
    import_arguments = list(
      selected_source_variables = full_import$selected_source_variables,
      read_columns = full_import$read_columns,
      output_columns = full_import$output_columns,
      n_max = Inf,
      problems = full_import$problems_policy,
      progress = FALSE,
      standardize = "lightlogr",
      discovery_terms_applied = FALSE,
      preview_result_used = FALSE
    ),
    source_timezone = timezone,
    details = details
  )

  handoff_provenance <- full_import
  handoff_provenance$data <- NULL
  variables <- glc_dataset_column_metadata(full_import, names(data))
  factual_metadata <- list(
    source = "GLC registry full import",
    repository = full_import$source$repository,
    exact_revision = full_import$source$resolved_revision,
    variables = variables,
    glc_metadata = full_import$metadata,
    dataset_metadata = locations
  )
  if (!is.null(unique_location)) {
    factual_metadata$latitude <- unique_location$latitude
    factual_metadata$longitude <- unique_location$longitude
  }
  record <- new_dataset_record(
    raw_data = data,
    display_name = display_name,
    source_manifest = manifest,
    factual_metadata = factual_metadata,
    analysis_settings = list(
      primary_variable = primary,
      analysis_timezone = timezone
    ),
    provenance = list(
      LightLogR_version = installed_package_version("LightLogR"),
      glcdp = glcdp_runtime_provenance(),
      raw_import_quality = quality,
      primary_variable_eligibility = eligibility,
      glc_full_import = handoff_provenance,
      glc_library_integration = list(
        schema = "llw-glc-library-integration",
        version = "1.0.0",
        source_full_import_fingerprint = full_import$fingerprint,
        complete_handoff_used = TRUE,
        preview_result_used = FALSE,
        session_library_added = TRUE,
        persisted_outside_session = FALSE,
        promoted = FALSE,
        downloaded = FALSE
      )
    ),
    metadata_bundle = full_import$source_metadata_snapshot$bundle
  )
  validate_dataset_record(record)
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

development_devicefile_path <- function(
  key = c("actlumus_synthetic", "actlumus_4789"),
  project_root = NULL
) {
  key <- match.arg(key)
  if (is.null(project_root)) {
    project_root <- lightlogweb_development_root()
  } else {
    assert_scalar_string(project_root, "project_root")
  }
  switch(
    key,
    actlumus_synthetic = file.path(
      project_root,
      "dev",
      "fixtures",
      "raw",
      "P01_actlumus.txt"
    ),
    actlumus_4789 = file.path(
      project_root,
      "testdevices",
      "ActLumus",
      "4789.txt"
    )
  )
}

development_testdevice_paths <- function(
  key = c("actlumus_all", "speccy_all", "veet_02"),
  project_root = NULL
) {
  key <- match.arg(key)
  if (is.null(project_root)) {
    project_root <- lightlogweb_development_root()
  } else {
    assert_scalar_string(project_root, "project_root")
  }
  paths <- switch(
    key,
    actlumus_all = list.files(
      file.path(project_root, "testdevices", "ActLumus"),
      pattern = "[.]txt$",
      full.names = TRUE
    ),
    speccy_all = list.files(
      file.path(project_root, "testdevices", "Speccy"),
      pattern = "[.]csv$",
      full.names = TRUE
    ),
    veet_02 = {
      zip_path <- file.path(
        project_root,
        "testdevices",
        "VEET",
        "02_VEET_L.csv.zip"
      )
      plain_path <- file.path(
        project_root,
        "testdevices",
        "VEET",
        "02_VEET_L.csv"
      )
      if (file.exists(zip_path)) zip_path else plain_path
    }
  )
  sort(as.character(paths))
}

dataset_example_catalog <- function(project_root = NULL) {
  if (is.null(project_root)) {
    project_root <- lightlogweb_development_root()
  } else {
    assert_scalar_string(project_root, "project_root")
  }
  paths <- list(
    sample = NA_character_,
    actlumus_synthetic = development_devicefile_path(
      "actlumus_synthetic",
      project_root
    ),
    actlumus_4789 = development_devicefile_path(
      "actlumus_4789",
      project_root
    ),
    iztech = development_large_dataset_path(project_root),
    actlumus_all = development_testdevice_paths("actlumus_all", project_root),
    speccy_all = development_testdevice_paths("speccy_all", project_root),
    veet_02_als = development_testdevice_paths("veet_02", project_root),
    veet_02_pho = development_testdevice_paths("veet_02", project_root)
  )
  available <- vapply(
    paths,
    function(path) {
      length(path) > 0L && (all(is.na(path)) || all(file.exists(path)))
    },
    logical(1)
  )
  data.frame(
    key = names(paths),
    label = c(
      "LightLogR test data (small)",
      "ActLumus synthetic device file (small)",
      "ActLumus 4789 provided device file",
      "IZTECH light glasses (151,200 rows)",
      "All provided ActLumus files (20 participants)",
      "All provided Speccy files (ID01, ID02, ID04)",
      "VEET 02 ambient-light sensor (ALS)",
      "VEET 02 spectral sensor (PHO)"
    ),
    description = c(
      paste(
        "Deterministic LightLogR sample with two participants, MEDI metadata,",
        "and Europe/Berlin source time."
      ),
      paste(
        "Already configured import of the fictional ActLumus file under",
        "dev/fixtures; useful for fast append tests."
      ),
      paste(
        "Already configured development import of the repository-local",
        "ActLumus 4789 export (4,686 rows in the reviewed checkout)."
      ),
      paste(
        "Pinned MeLiDos IZTECH development snapshot with 17 participants",
        "and Europe/Istanbul source time."
      ),
      paste(
        "All 20 repository-provided ActLumus exports imported together with",
        "filename stems as participant IDs and Europe/Berlin source time."
      ),
      paste(
        "All eight repository-provided Speccy exports imported together.",
        "The expression ^(ID[0-9]{2}) groups file parts into ID01, ID02,",
        "and ID04. UTC is an explicit development placeholder because the",
        "files do not record an IANA source timezone."
      ),
      paste(
        "Repository-provided 02_VEET_L imported as the ambient-light-sensor",
        "modality with Europe/Berlin source time."
      ),
      paste(
        "The same repository-provided 02_VEET_L source imported as the",
        "spectral-sensor modality with Europe/Berlin source time."
      )
    ),
    available = available,
    unavailable_reason = ifelse(
      available,
      NA_character_,
      "This optional development file is not present in the checkout."
    ),
    stringsAsFactors = FALSE
  )
}

dataset_example_choices <- function(project_root = NULL) {
  catalog <- dataset_example_catalog(project_root)
  catalog <- catalog[catalog$available, , drop = FALSE]
  stats::setNames(catalog$key, catalog$label)
}

development_devicefile_dataset_record <- function(
  path,
  display_name,
  source_timezone,
  example_key,
  version = "default"
) {
  development_devicefiles_dataset_record(
    paths = path,
    display_name = display_name,
    source_timezone = source_timezone,
    example_key = example_key,
    device = "ActLumus",
    version = version,
    id_mode = "automated",
    preferred_primary = c("MEDI")
  )
}

development_devicefiles_dataset_record <- function(
  paths,
  display_name,
  source_timezone,
  example_key,
  device,
  version = "default",
  id_mode = c("automated", "manual", "extract"),
  extract_pattern = NULL,
  veet_modality = NULL,
  preferred_primary = character(),
  max_bytes = 1024^3
) {
  id_mode <- match.arg(id_mode)
  arguments <- list(
    display_name = display_name,
    source_timezone = source_timezone,
    example_key = example_key,
    version = version,
    device = device
  )
  for (argument in names(arguments)) {
    assert_scalar_string(arguments[[argument]], argument)
  }
  assert_character_vector(paths, "paths", allow_empty = FALSE)
  assert_character_vector(preferred_primary, "preferred_primary")
  unavailable <- !file.exists(paths) | dir.exists(paths)
  if (any(unavailable)) {
    abort_llw(
      paste0(
        "Development device file(s) are missing: ",
        paste(paths[unavailable], collapse = ", "),
        "."
      ),
      type = "unavailable_feature",
      public_message = paste(
        "That optional ready-to-use device example is unavailable in this",
        "checkout.",
        "Choose another example or use the import workflow."
      )
    )
  }
  stage_root <- tempfile("llw-ready-device-")
  dir.create(stage_root, recursive = TRUE, mode = "0700")
  on.exit(unlink(stage_root, recursive = TRUE, force = TRUE), add = TRUE)
  staged <- stage_import_files(
    data.frame(
      name = basename(paths),
      datapath = paths,
      stringsAsFactors = FALSE
    ),
    stage_root
  )
  request <- new_raw_import_request(
    device = device,
    staged_files = staged,
    timezone = source_timezone,
    not_before = as.Date("2001-01-01"),
    version = version,
    id_mode = id_mode,
    extract_pattern = extract_pattern,
    veet_modality = veet_modality,
    max_bytes = max_bytes
  )
  result <- suppressWarnings(raw_import_worker(request, spec = NULL))
  eligible <- result$eligibility$variable[result$eligibility$eligible]
  if (length(eligible) == 0L) {
    abort_llw(
      "The ready-to-use device file has no eligible primary measurement.",
      type = "validation"
    )
  }
  preferred <- intersect(preferred_primary, eligible)
  primary <- if (length(preferred) > 0L) preferred[[1L]] else eligible[[1L]]
  record <- new_imported_dataset_record(list(
    name = display_name,
    data = result$data,
    device = device,
    version = request$preflight$version,
    tz = source_timezone,
    variable = primary,
    import_arguments = raw_import_manifest_arguments(request),
    source_files = request$source_files,
    preflight = result$preflight,
    quality = result$quality,
    eligibility = result$eligibility
  ))
  if (identical(primary, "MEDI")) {
    record$factual_metadata$variables$MEDI <- list(
      label = "Melanopic equivalent daylight illuminance",
      unit = "lux",
      calibration = "Unknown; retain source- and device-specific interpretation"
    )
  }
  if (identical(primary, "Photopic.lux") || identical(primary, "Lux")) {
    record$factual_metadata$variables[[primary]] <- list(
      label = "Photopic illuminance",
      unit = "lux",
      calibration = "Unknown; retain source- and device-specific interpretation"
    )
  }
  record$source_manifest$details$development_example <- list(
    key = example_key,
    configured_source_timezone = source_timezone,
    note = paste(
      "Development-only one-click import configuration. The configured source",
      "timezone is an explicit fixture assumption, not inferred from file bytes."
    ),
    participant_id_mapping = unclass(request$id_mapping),
    modality = veet_modality
  )
  record$provenance$development_example <- list(
    key = example_key,
    loader = "LightLogWeb::development_devicefiles_dataset_record",
    source_timezone_decision = source_timezone
  )
  validate_dataset_record(record)
}

load_dataset_example <- function(key, project_root = NULL) {
  assert_scalar_string(key, "key")
  catalog <- dataset_example_catalog(project_root)
  selected <- catalog[catalog$key == key, , drop = FALSE]
  if (nrow(selected) != 1L) {
    abort_llw(
      paste0("Unknown ready-to-use dataset key `", key, "`."),
      type = "validation"
    )
  }
  if (!isTRUE(selected$available[[1L]])) {
    abort_llw(
      selected$unavailable_reason[[1L]],
      type = "unavailable_feature",
      public_message = selected$unavailable_reason[[1L]]
    )
  }
  switch(
    key,
    sample = sample_dataset_record(),
    actlumus_synthetic = development_devicefile_dataset_record(
      path = development_devicefile_path("actlumus_synthetic", project_root),
      display_name = "ActLumus synthetic device-file example",
      source_timezone = "UTC",
      example_key = key
    ),
    actlumus_4789 = development_devicefile_dataset_record(
      path = development_devicefile_path("actlumus_4789", project_root),
      display_name = "ActLumus 4789 device-file example",
      source_timezone = "Europe/Berlin",
      example_key = key
    ),
    iztech = melidos_iztech_dataset_record(
      development_large_dataset_path(project_root)
    ),
    actlumus_all = development_devicefiles_dataset_record(
      paths = development_testdevice_paths("actlumus_all", project_root),
      display_name = "All provided ActLumus files",
      source_timezone = "Europe/Berlin",
      example_key = key,
      device = "ActLumus",
      preferred_primary = c("MEDI", "LIGHT")
    ),
    speccy_all = development_devicefiles_dataset_record(
      paths = development_testdevice_paths("speccy_all", project_root),
      display_name = "All provided Speccy files",
      source_timezone = "UTC",
      example_key = key,
      device = "Speccy",
      id_mode = "extract",
      extract_pattern = "^(ID[0-9]{2})",
      preferred_primary = c("MEDI", "Photopic.lux")
    ),
    veet_02_als = development_devicefiles_dataset_record(
      paths = development_testdevice_paths("veet_02", project_root),
      display_name = "VEET 02 - ambient-light sensor (ALS)",
      source_timezone = "Europe/Berlin",
      example_key = key,
      device = "VEET",
      veet_modality = "ALS",
      preferred_primary = c("Lux", "visValue")
    ),
    veet_02_pho = development_devicefiles_dataset_record(
      paths = development_testdevice_paths("veet_02", project_root),
      display_name = "VEET 02 - spectral sensor (PHO)",
      source_timezone = "Europe/Berlin",
      example_key = key,
      device = "VEET",
      veet_modality = "PHO",
      preferred_primary = c("Clear", "s480")
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
