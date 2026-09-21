glcdp_export_contract <- function() {
  c(
    "glc_packages",
    "glc_search_packages",
    "glc_open",
    "glc_schema_versions",
    "glc_summary",
    "glc_datasets",
    "glc_files",
    "glc_variables",
    "glc_resources",
    "glc_metadata",
    "glc_search_metadata",
    "extract_metadata",
    "add_metadata",
    "glc_collection_plan",
    "glc_collection_refine",
    "glc_read",
    "glc_collect",
    "glc_download"
  )
}

is_glc_repository <- function(repository) {
  is.character(repository) &&
    length(repository) == 1L &&
    !is.na(repository) &&
    grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository) &&
    !any(strsplit(repository, "/", fixed = TRUE)[[1L]] %in% c(".", ".."))
}

assert_glc_repository <- function(repository, arg = "repository") {
  if (!is_glc_repository(repository)) {
    abort_llw(
      paste0(
        "`",
        arg,
        "` must use the conservative `owner/repository` form."
      ),
      type = "validation"
    )
  }
  invisible(repository)
}

new_glc_source_spec <- function(
  registry_url,
  repository,
  requested_revision = c("latest_pass", "exact"),
  resolved_revision,
  validation_state = c("passed", "failed", "unknown"),
  attestation_verified = NA,
  schema_version,
  glcdp_version,
  cache_dir,
  registry_generated_at = NA_character_
) {
  requested_revision <- match.arg(requested_revision)
  validation_state <- match.arg(validation_state)
  assert_scalar_string(registry_url, "registry_url")
  assert_glc_repository(repository)
  assert_scalar_string(resolved_revision, "resolved_revision")
  if (!grepl("^[0-9a-fA-F]{40}$", resolved_revision)) {
    abort_llw(
      "`resolved_revision` must be an exact 40-character commit SHA.",
      type = "validation"
    )
  }
  if (
    !is.logical(attestation_verified) ||
      length(attestation_verified) != 1L
  ) {
    abort_llw(
      "`attestation_verified` must be `TRUE`, `FALSE`, or `NA`.",
      type = "validation"
    )
  }
  assert_scalar_string(schema_version, "schema_version")
  assert_scalar_string(glcdp_version, "glcdp_version")
  assert_scalar_string(cache_dir, "cache_dir")
  if (
    !is.character(registry_generated_at) ||
      length(registry_generated_at) != 1L ||
      (!is.na(registry_generated_at) && !nzchar(registry_generated_at))
  ) {
    abort_llw(
      "`registry_generated_at` must be one string or `NA_character_`.",
      type = "validation"
    )
  }
  if (
    identical(requested_revision, "latest_pass") &&
      !identical(validation_state, "passed")
  ) {
    abort_llw(
      "A `latest_pass` source specification must have validation state `passed`.",
      type = "validation"
    )
  }

  spec <- structure(
    list(
      registry_url = registry_url,
      registry_generated_at = registry_generated_at,
      repository = repository,
      requested_revision = requested_revision,
      resolved_revision = tolower(resolved_revision),
      validation_state = validation_state,
      attestation_verified = attestation_verified,
      schema_version = schema_version,
      glcdp_version = glcdp_version,
      cache_dir = normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
    ),
    class = c("llw_glc_source_spec", "list")
  )
  assert_serializable_value(spec, "GLC source specification")
  spec
}

validate_glc_source_spec <- function(source) {
  if (!inherits(source, "llw_glc_source_spec") || !is.list(source)) {
    abort_llw(
      "`source` must be created by `new_glc_source_spec()`.",
      type = "validation"
    )
  }
  required <- c(
    "registry_url",
    "registry_generated_at",
    "repository",
    "requested_revision",
    "resolved_revision",
    "validation_state",
    "attestation_verified",
    "schema_version",
    "glcdp_version",
    "cache_dir"
  )
  missing <- setdiff(required, names(source))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "GLC source specification is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  new_glc_source_spec(
    registry_url = source$registry_url,
    repository = source$repository,
    requested_revision = source$requested_revision,
    resolved_revision = source$resolved_revision,
    validation_state = source$validation_state,
    attestation_verified = source$attestation_verified,
    schema_version = source$schema_version,
    glcdp_version = source$glcdp_version,
    cache_dir = source$cache_dir,
    registry_generated_at = source$registry_generated_at
  )
}

new_glc_selection <- function(
  dataset_ids = character(),
  file_group_ids = character(),
  files = character(),
  resources = character(),
  source_variables = character(),
  semantic_terms = character(),
  primary_only = FALSE,
  preview_n_max = 1000L,
  problem_policy = c("error", "warn"),
  standardization = c("lightlogr", "none")
) {
  assert_character_vector(dataset_ids, "dataset_ids")
  assert_character_vector(file_group_ids, "file_group_ids")
  assert_character_vector(files, "files")
  assert_character_vector(resources, "resources")
  assert_character_vector(source_variables, "source_variables")
  assert_character_vector(semantic_terms, "semantic_terms")
  assert_flag(primary_only, "primary_only")
  if (
    !is.numeric(preview_n_max) ||
      length(preview_n_max) != 1L ||
      is.na(preview_n_max) ||
      preview_n_max < 0 ||
      is.finite(preview_n_max) && preview_n_max != floor(preview_n_max)
  ) {
    abort_llw(
      "`preview_n_max` must be one non-negative whole number or `Inf`.",
      type = "validation"
    )
  }
  problem_policy <- match.arg(problem_policy)
  standardization <- match.arg(standardization)
  selection <- structure(
    list(
      dataset_ids = unique(dataset_ids),
      file_group_ids = unique(file_group_ids),
      files = unique(files),
      resources = unique(resources),
      source_variables = unique(source_variables),
      semantic_terms = unique(semantic_terms),
      primary_only = primary_only,
      preview_n_max = preview_n_max,
      problem_policy = problem_policy,
      standardization = standardization
    ),
    class = c("llw_glc_selection", "list")
  )
  assert_serializable_value(selection, "GLC selection")
  selection
}

validate_glc_selection <- function(selection) {
  if (!inherits(selection, "llw_glc_selection") || !is.list(selection)) {
    abort_llw(
      "`selection` must be created by `new_glc_selection()`.",
      type = "validation"
    )
  }
  required <- c(
    "dataset_ids",
    "file_group_ids",
    "files",
    "resources",
    "source_variables",
    "semantic_terms",
    "primary_only",
    "preview_n_max",
    "problem_policy",
    "standardization"
  )
  missing <- setdiff(required, names(selection))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "GLC selection is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  new_glc_selection(
    dataset_ids = selection$dataset_ids,
    file_group_ids = selection$file_group_ids,
    files = selection$files,
    resources = selection$resources,
    source_variables = selection$source_variables,
    semantic_terms = selection$semantic_terms,
    primary_only = selection$primary_only,
    preview_n_max = selection$preview_n_max,
    problem_policy = selection$problem_policy,
    standardization = selection$standardization
  )
}

new_glc_preview <- function(
  summary,
  datasets,
  files,
  variables,
  resources,
  metadata = list(),
  transfer_bytes = NA_real_,
  availability = NA,
  compatibility_partitions = list(),
  warnings = character(),
  estimate_uncertain = TRUE
) {
  assert_flag(estimate_uncertain, "estimate_uncertain")
  assert_character_vector(warnings, "warnings")
  preview <- structure(
    list(
      summary = summary,
      datasets = datasets,
      files = files,
      variables = variables,
      resources = resources,
      metadata = metadata,
      transfer_bytes = transfer_bytes,
      availability = availability,
      compatibility_partitions = compatibility_partitions,
      warnings = warnings,
      estimate_uncertain = estimate_uncertain
    ),
    class = c("llw_glc_preview", "list")
  )
  assert_serializable_value(preview, "GLC preview")
  preview
}

glcdp_api <- function(api = NULL) {
  exports <- glcdp_export_contract()
  if (is.null(api)) {
    package <- paste0("glc", "dp")
    namespace <- tryCatch(loadNamespace(package), error = identity)
    if (inherits(namespace, "error")) {
      abort_llw(
        "The optional `glcdp` package is not installed in this runtime.",
        type = "unavailable_feature",
        public_message = paste(
          "GLC access is unavailable in this runtime.",
          "Raw-file and project workflows remain usable."
        ),
        diagnostics = list(original_message = conditionMessage(namespace)),
        parent = namespace
      )
    }
    api <- stats::setNames(
      lapply(exports, function(name) getExportedValue(package, name)),
      exports
    )
  }
  if (!is.list(api)) {
    abort_llw(
      "`api` must be a named list of glcdp functions.",
      type = "validation"
    )
  }
  missing <- setdiff(exports, names(api))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "glcdp API seam is missing export(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  invalid <- !vapply(api[exports], is.function, logical(1))
  if (any(invalid)) {
    abort_llw(
      paste0(
        "glcdp API entries must be functions: ",
        paste(exports[invalid], collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  api[exports]
}

glcdp_relationship_field_contract <- function() {
  list(
    file_group = c(
      "dataset_internal_id",
      "dataset_timezone",
      "participant_internal_id",
      "participant_age",
      "participant_sex",
      "participant_gender",
      "study_internal_id",
      "study_title",
      "study_short_description",
      "study_setting",
      "study_geographical_location",
      "device_internal_id",
      "device_manufacturer",
      "device_model",
      "device_serial_number",
      "device_calibration_date",
      "device_datasheet_id",
      "datasheet_id",
      "datasheet_manufacturer",
      "datasheet_model",
      "datasheet_calibration_method",
      "datasheet_calibration_accuracy",
      "datasheet_calibration_range",
      "datasheet_calibration_notes"
    ),
    dataset = c(
      "dataset_internal_id",
      "dataset_timezone",
      "participant_internal_id",
      "participant_age",
      "participant_sex",
      "participant_gender",
      "study_internal_id",
      "study_title",
      "study_short_description",
      "study_setting",
      "study_geographical_location"
    )
  )
}

glcdp_metadata_field_names <- function(metadata) {
  if (is.data.frame(metadata)) return(names(metadata))
  if (!is.list(metadata)) return(character())
  unique(unlist(
    lapply(metadata, glcdp_metadata_field_names),
    use.names = FALSE
  ))
}

glcdp_extract_relationship_table <- function(
  keys,
  handle,
  fields,
  by,
  api,
  resource = NULL
) {
  if (!is.data.frame(keys) || !by %in% names(keys)) {
    abort_llw("Relationship extraction keys are invalid.", type = "validation")
  }
  keys <- keys[!duplicated(as.character(keys[[by]])), , drop = FALSE]
  if (length(fields) == 0L) {
    return(list(table = keys[, by, drop = FALSE], warnings = character()))
  }
  warnings <- character()
  value <- withCallingHandlers(
    api$extract_metadata(
      dataset = keys,
      metadata = handle,
      fields = fields,
      by = by,
      resource = resource
    ),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  if (!is.data.frame(value) || !by %in% names(value)) {
    abort_llw(
      "Exported glcdp relationship extraction returned an invalid table.",
      type = "validation"
    )
  }
  expected <- as.character(keys[[by]])
  observed <- as.character(value[[by]])
  if (!identical(observed, expected)) {
    abort_llw(
      paste(
        "Exported glcdp relationship extraction changed key membership or",
        "first-occurrence order."
      ),
      type = "validation"
    )
  }
  list(table = value, warnings = unique(warnings))
}

glcdp_combine_relationship_tables <- function(primary, secondary, by) {
  if (
    !is.data.frame(primary) ||
      !is.data.frame(secondary) ||
      !by %in% names(primary) ||
      !by %in% names(secondary) ||
      !identical(
        as.character(primary[[by]]),
        as.character(secondary[[by]])
      )
  ) {
    abort_llw(
      "Exported metadata relationship fragments have different identities.",
      type = "validation"
    )
  }
  additions <- setdiff(names(secondary), by)
  conflicts <- intersect(additions, names(primary))
  if (length(conflicts) > 0L) {
    abort_llw(
      paste0(
        "Exported metadata relationship fragments conflict on: ",
        paste(conflicts, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  cbind(primary, secondary[additions], stringsAsFactors = FALSE)
}

glcdp_extract_relationship_context <- function(
  handle,
  files,
  metadata,
  api = NULL
) {
  api <- glcdp_api(api)
  if (
    !is.data.frame(files) ||
      !all(c("dataset_id", "file_group_id") %in% names(files))
  ) {
    abort_llw(
      "GLC relationship extraction requires dataset and file-group inventory.",
      type = "validation"
    )
  }
  file_keys <- unique(data.frame(
    Id = as.character(files$dataset_id),
    file_group_id = as.character(files$file_group_id),
    stringsAsFactors = FALSE
  ))
  dataset_keys <- unique(file_keys["Id"])
  available <- glcdp_metadata_field_names(metadata)
  contract <- glcdp_relationship_field_contract()
  file_fields <- intersect(contract$file_group, available)
  dataset_fields <- intersect(contract$dataset, available)
  participant_identity <- "participant_internal_id"
  file_primary_fields <- setdiff(file_fields, participant_identity)
  dataset_primary_fields <- setdiff(dataset_fields, participant_identity)
  file_groups <- glcdp_extract_relationship_table(
    file_keys,
    handle,
    file_primary_fields,
    "file_group_id",
    api
  )
  datasets <- glcdp_extract_relationship_table(
    dataset_keys,
    handle,
    dataset_primary_fields,
    "Id",
    api
  )
  participant_warnings <- character()
  if (participant_identity %in% file_fields) {
    participant_file_groups <- glcdp_extract_relationship_table(
      file_keys,
      handle,
      participant_identity,
      "file_group_id",
      api,
      resource = "participants"
    )
    file_groups$table <- glcdp_combine_relationship_tables(
      file_groups$table,
      participant_file_groups$table,
      "file_group_id"
    )
    participant_warnings <- c(
      participant_warnings,
      participant_file_groups$warnings
    )
  }
  if (participant_identity %in% dataset_fields) {
    participant_datasets <- glcdp_extract_relationship_table(
      dataset_keys,
      handle,
      participant_identity,
      "Id",
      api,
      resource = "participants"
    )
    datasets$table <- glcdp_combine_relationship_tables(
      datasets$table,
      participant_datasets$table,
      "Id"
    )
    participant_warnings <- c(
      participant_warnings,
      participant_datasets$warnings
    )
  }
  new_metadata_relationship_context(
    file_groups = file_groups$table,
    datasets = datasets$table,
    requested_fields = unique(c(file_fields, dataset_fields)),
    warnings = unique(c(
      file_groups$warnings,
      datasets$warnings,
      participant_warnings
    )),
    provenance = list(
      extractor = "glcdp::extract_metadata",
      default_anchor = "file_group_id",
      dataset_anchor = "Id",
      ambiguity_resolution = list(
        field = participant_identity,
        resource = "participants"
      ),
      package_reopened_after_inspection = FALSE,
      measurement_contents_read = FALSE
    )
  )
}

glcdp_reopen_source <- function(source, api = NULL) {
  source <- validate_glc_source_spec(source)
  api <- glcdp_api(api)
  api$glc_open(
    source = source$repository,
    ref = source$resolved_revision,
    cache_dir = source$cache_dir,
    registry = source$registry_url,
    quiet = TRUE
  )
}

glcdp_inventory_preview <- function(handle, selection, api) {
  dataset_id <- if (length(selection$dataset_ids) == 0L) {
    NULL
  } else {
    selection$dataset_ids
  }
  file_group <- if (length(selection$file_group_ids) == 0L) {
    NULL
  } else {
    selection$file_group_ids
  }
  datasets <- api$glc_datasets(handle, dataset_id = dataset_id)
  files <- api$glc_files(
    handle,
    dataset_id = dataset_id,
    file_group = file_group
  )
  variables <- api$glc_variables(
    handle,
    dataset_id = dataset_id,
    file_group = file_group,
    primary = if (selection$primary_only) TRUE else NULL
  )
  resources <- api$glc_resources(handle)
  metadata <- if (length(selection$resources) == 0L) {
    list()
  } else {
    api$glc_metadata(handle, resources = selection$resources)
  }
  expected_bytes <- if ("expected_bytes" %in% names(files)) {
    as.numeric(files$expected_bytes)
  } else {
    numeric()
  }
  availability <- if ("available" %in% names(files)) {
    as.logical(files$available)
  } else {
    NA
  }
  new_glc_preview(
    summary = api$glc_summary(handle),
    datasets = datasets,
    files = files,
    variables = variables,
    resources = resources,
    metadata = metadata,
    transfer_bytes = if (
      length(expected_bytes) == 0L || all(is.na(expected_bytes))
    ) {
      NA_real_
    } else {
      sum(expected_bytes, na.rm = TRUE)
    },
    availability = availability,
    compatibility_partitions = list(),
    warnings = character(),
    estimate_uncertain = length(expected_bytes) == 0L || anyNA(expected_bytes)
  )
}

new_glc_preview_allocation <- function(total_budget, file_ids) {
  if (
    !is.numeric(total_budget) ||
      length(total_budget) != 1L ||
      is.na(total_budget) ||
      total_budget < 0 ||
      is.finite(total_budget) && total_budget != floor(total_budget)
  ) {
    abort_llw(
      "`total_budget` must be one non-negative whole number or `Inf`.",
      type = "validation"
    )
  }
  assert_character_vector(file_ids, "file_ids", allow_empty = FALSE)
  file_ids <- unique(file_ids)
  available_file_count <- length(file_ids)

  if (is.infinite(total_budget)) {
    preview_file_count <- available_file_count
    per_file_n_max <- Inf
    maximum_rows <- Inf
    unused_budget <- 0
  } else if (identical(total_budget, 0) || total_budget == 0) {
    preview_file_count <- available_file_count
    per_file_n_max <- 0
    maximum_rows <- 0
    unused_budget <- 0
  } else {
    preview_file_count <- min(available_file_count, total_budget)
    per_file_n_max <- floor(total_budget / preview_file_count)
    maximum_rows <- preview_file_count * per_file_n_max
    unused_budget <- total_budget - maximum_rows
  }

  preview_file_count <- as.integer(preview_file_count)
  allocation <- structure(
    list(
      total_budget = total_budget,
      available_file_count = available_file_count,
      preview_file_count = preview_file_count,
      preview_files = file_ids[seq_len(preview_file_count)],
      omitted_files = if (preview_file_count < available_file_count) {
        file_ids[seq.int(preview_file_count + 1L, available_file_count)]
      } else {
        character()
      },
      per_file_n_max = per_file_n_max,
      maximum_rows = maximum_rows,
      unused_budget = unused_budget
    ),
    class = c("llw_glc_preview_allocation", "list")
  )
  assert_serializable_value(allocation, "GLC preview allocation")
  allocation
}

glcdp_preview_file_ids <- function(handle, selection, api) {
  if (length(selection$files) > 0L) {
    return(unique(selection$files))
  }

  files <- api$glc_files(
    handle,
    dataset_id = if (length(selection$dataset_ids) == 0L) {
      NULL
    } else {
      selection$dataset_ids
    },
    file_group = if (length(selection$file_group_ids) == 0L) {
      NULL
    } else {
      selection$file_group_ids
    }
  )
  if (!is.data.frame(files) || !"path" %in% names(files)) {
    abort_llw(
      "The glcdp file inventory does not provide stable file paths.",
      type = "validation"
    )
  }
  file_ids <- as.character(files$path)
  assert_character_vector(file_ids, "glcdp file paths", allow_empty = FALSE)
  unique(file_ids)
}

glcdp_preview_read_plan <- function(handle, selection, api) {
  selection <- validate_glc_selection(selection)
  new_glc_preview_allocation(
    total_budget = selection$preview_n_max,
    file_ids = glcdp_preview_file_ids(handle, selection, api)
  )
}

glcdp_read_selection <- function(handle, selection, api) {
  allocation <- glcdp_preview_read_plan(handle, selection, api)
  collection <- api$glc_read(
    x = handle,
    dataset_id = if (length(selection$dataset_ids) == 0L) {
      "all"
    } else {
      selection$dataset_ids
    },
    file_group = if (length(selection$file_group_ids) == 0L) {
      NULL
    } else {
      selection$file_group_ids
    },
    files = allocation$preview_files,
    variables = if (length(selection$source_variables) == 0L) {
      NULL
    } else {
      selection$source_variables
    },
    terms = if (length(selection$semantic_terms) == 0L) {
      NULL
    } else {
      selection$semantic_terms
    },
    primary_only = selection$primary_only,
    n_max = allocation$per_file_n_max,
    problems = selection$problem_policy,
    progress = FALSE
  )
  api$glc_collect(collection, standardize = selection$standardization)
}

glcdp_download_selection <- function(
  handle,
  selection,
  destination,
  source,
  api
) {
  assert_scalar_string(destination, "destination")
  created <- dir.create(destination, recursive = TRUE, showWarnings = FALSE)
  if (!created && !dir.exists(destination)) {
    abort_llw(
      "The GLC download directory could not be created.",
      type = "resource"
    )
  }
  destination <- normalizePath(destination, winslash = "/", mustWork = TRUE)
  cache_root <- normalizePath(
    source$cache_dir,
    winslash = "/",
    mustWork = FALSE
  )
  if (
    !identical(destination, cache_root) &&
      !startsWith(paste0(destination, "/"), paste0(cache_root, "/"))
  ) {
    abort_llw(
      "GLC downloads must remain inside the session-scoped cache directory.",
      type = "resource"
    )
  }
  api$glc_download(
    x = handle,
    dest_dir = destination,
    include = "data",
    dataset_id = if (length(selection$dataset_ids) == 0L) {
      NULL
    } else {
      selection$dataset_ids
    },
    file_group = if (length(selection$file_group_ids) == 0L) {
      NULL
    } else {
      selection$file_group_ids
    },
    resources = if (length(selection$resources) == 0L) {
      NULL
    } else {
      selection$resources
    },
    files = if (length(selection$files) == 0L) NULL else selection$files,
    overwrite = FALSE
  )
}

glcdp_worker_execute <- function(
  operation = c("discovery", "read", "download"),
  source,
  selection,
  destination = NULL,
  api = NULL
) {
  operation <- match.arg(operation)
  source <- validate_glc_source_spec(source)
  selection <- validate_glc_selection(selection)
  assert_serializable_value(source, "GLC source specification")
  assert_serializable_value(selection, "GLC selection")
  api <- glcdp_api(api)

  tryCatch(
    {
      handle <- glcdp_reopen_source(source, api = api)
      result <- switch(
        operation,
        discovery = glcdp_inventory_preview(handle, selection, api),
        read = glcdp_read_selection(handle, selection, api),
        download = glcdp_download_selection(
          handle,
          selection,
          destination,
          source,
          api
        )
      )
      assert_serializable_value(result, "glcdp worker result")
      result
    },
    llw_error = function(cnd) stop(cnd),
    error = function(cnd) stop(map_glcdp_condition(cnd))
  )
}

new_glcdp_task_payload <- function(
  operation = c("discovery", "read", "download"),
  source,
  selection,
  destination = NULL
) {
  operation <- match.arg(operation)
  source <- validate_glc_source_spec(source)
  selection <- validate_glc_selection(selection)
  payload <- list(
    operation = operation,
    source = source,
    selection = selection,
    destination = destination
  )
  assert_serializable_value(payload, "glcdp task payload")
  payload
}

glcdp_task_worker <- function(payload, spec) {
  glcdp_worker_execute(
    operation = payload$operation,
    source = payload$source,
    selection = payload$selection,
    destination = payload$destination
  )
}

glc_inspection_required_columns <- function() {
  list(
    summary = c(
      "schema_version",
      "dataset_count",
      "file_count",
      "declared_bytes"
    ),
    datasets = c(
      "dataset_id",
      "schema_version",
      "study_id",
      "participant_id",
      "timezone",
      "file_group_count",
      "file_count",
      "modalities",
      "device_ids",
      "primary_variables"
    ),
    files = c(
      "dataset_id",
      "file_group_id",
      "path",
      "encoding",
      "timezone",
      "role",
      "data_state",
      "modalities",
      "device_id",
      "temporal_type",
      "temporal_value",
      "temporal_unit",
      "storage",
      "expected_bytes",
      "available"
    ),
    variables = c(
      "dataset_id",
      "file_group_id",
      "name",
      "label",
      "description",
      "unit",
      "type",
      "term",
      "term_name",
      "primary",
      "factor_values",
      "factor_labels",
      "factor_descriptions"
    ),
    resources = c(
      "resource",
      "path",
      "core",
      "format",
      "media_type"
    )
  )
}

validate_glc_inspection_table <- function(value, name, required) {
  if (!is.data.frame(value)) {
    abort_llw(
      paste0("The glcdp ", name, " inventory is not tabular."),
      type = "validation"
    )
  }
  missing <- setdiff(required, names(value))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "The glcdp ",
        name,
        " inventory is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  invisible(value)
}

glc_nonempty_unique <- function(value) {
  value <- as.character(value)
  value <- trimws(value[!is.na(value)])
  sort(unique(value[nzchar(value)]), method = "radix")
}

glc_term_catalogue <- function(variables) {
  required <- c(
    "dataset_id",
    "file_group_id",
    "name",
    "label",
    "description",
    "term",
    "term_name"
  )
  validate_glc_inspection_table(variables, "variables", required)
  terms <- as.character(variables$term)
  keep <- !is.na(terms) & nzchar(trimws(terms))
  term_ids <- sort(unique(enc2utf8(terms[keep])), method = "radix")

  empty <- function() {
    value <- data.frame(
      term = character(),
      label = character(),
      label_consistent = logical(),
      dataset_count = integer(),
      file_group_count = integer(),
      variable_count = integer(),
      stringsAsFactors = FALSE
    )
    value$term_names <- I(list())
    value$source_names <- I(list())
    value$source_labels <- I(list())
    value$descriptions <- I(list())
    value
  }
  if (length(term_ids) == 0L) {
    return(empty())
  }

  rows <- lapply(term_ids, function(term_id) {
    indices <- which(terms == term_id)
    term_names <- glc_nonempty_unique(variables$term_name[indices])
    source_names <- glc_nonempty_unique(variables$name[indices])
    source_labels <- glc_nonempty_unique(variables$label[indices])
    descriptions <- glc_nonempty_unique(variables$description[indices])
    human_term_names <- term_names[
      tolower(term_names) != tolower(term_id)
    ]
    human_labels <- if (length(human_term_names) > 0L) {
      human_term_names
    } else {
      source_labels
    }
    row <- data.frame(
      term = term_id,
      label = if (length(human_labels) == 1L) human_labels else term_id,
      label_consistent = length(human_labels) <= 1L,
      dataset_count = length(unique(as.character(
        variables$dataset_id[indices]
      ))),
      file_group_count = length(unique(as.character(
        variables$file_group_id[indices]
      ))),
      variable_count = length(indices),
      stringsAsFactors = FALSE
    )
    row$term_names <- list(term_names)
    row$source_names <- list(source_names)
    row$source_labels <- list(source_labels)
    row$descriptions <- list(descriptions)
    row
  })
  catalogue <- do.call(rbind, rows)
  ordering <- order(
    tolower(enc2utf8(catalogue$label)),
    enc2utf8(catalogue$term),
    method = "radix"
  )
  rownames(catalogue) <- NULL
  catalogue[ordering, , drop = FALSE]
}

new_glc_inspection_request <- function(
  registry_url,
  repository,
  resolved_revision,
  validation_state = "passed",
  attestation_verified = NA,
  glcdp_version,
  cache_dir,
  registry_generated_at = NA_character_
) {
  assert_scalar_string(registry_url, "registry_url")
  assert_glc_repository(repository)
  assert_scalar_string(resolved_revision, "resolved_revision")
  if (!grepl("^[0-9a-fA-F]{40}$", resolved_revision)) {
    abort_llw(
      "resolved_revision must be an exact 40-character commit SHA.",
      type = "validation"
    )
  }
  if (!identical(validation_state, "passed")) {
    abort_llw(
      "Package inspection requires a passing registry revision.",
      type = "validation"
    )
  }
  if (
    !is.logical(attestation_verified) ||
      length(attestation_verified) != 1L
  ) {
    abort_llw(
      "attestation_verified must be TRUE, FALSE, or NA.",
      type = "validation"
    )
  }
  assert_scalar_string(glcdp_version, "glcdp_version")
  assert_scalar_string(cache_dir, "cache_dir")
  if (
    !is.character(registry_generated_at) ||
      length(registry_generated_at) != 1L ||
      (!is.na(registry_generated_at) && !nzchar(registry_generated_at))
  ) {
    abort_llw(
      "registry_generated_at must be one string or NA_character_.",
      type = "validation"
    )
  }

  request <- structure(
    list(
      registry_url = registry_url,
      registry_generated_at = registry_generated_at,
      repository = repository,
      resolved_revision = tolower(resolved_revision),
      validation_state = validation_state,
      attestation_verified = attestation_verified,
      glcdp_version = glcdp_version,
      cache_dir = normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
    ),
    class = c("llw_glc_inspection_request", "list")
  )
  assert_serializable_value(request, "GLC inspection request")
  request
}

validate_glc_inspection_request <- function(request) {
  if (!inherits(request, "llw_glc_inspection_request") || !is.list(request)) {
    abort_llw(
      "request must be created by new_glc_inspection_request().",
      type = "validation"
    )
  }
  required <- c(
    "registry_url",
    "registry_generated_at",
    "repository",
    "resolved_revision",
    "validation_state",
    "attestation_verified",
    "glcdp_version",
    "cache_dir"
  )
  missing <- setdiff(required, names(request))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "GLC inspection request is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  new_glc_inspection_request(
    registry_url = request$registry_url,
    repository = request$repository,
    resolved_revision = request$resolved_revision,
    validation_state = request$validation_state,
    attestation_verified = request$attestation_verified,
    glcdp_version = request$glcdp_version,
    cache_dir = request$cache_dir,
    registry_generated_at = request$registry_generated_at
  )
}

new_glc_inspection_source <- function(request, schema_version) {
  request <- validate_glc_inspection_request(request)
  assert_scalar_string(schema_version, "schema_version")
  source <- structure(
    list(
      registry_url = request$registry_url,
      registry_generated_at = request$registry_generated_at,
      repository = request$repository,
      requested_revision = "latest_pass",
      resolved_revision = request$resolved_revision,
      validation_state = request$validation_state,
      attestation_verified = request$attestation_verified,
      schema_version = schema_version,
      glcdp_version = request$glcdp_version
    ),
    class = c("llw_glc_inspection_source", "list")
  )
  assert_serializable_value(source, "GLC inspection source")
  source
}

validate_glc_inspection_source <- function(source) {
  if (
    !inherits(source, "llw_glc_inspection_source") ||
      !is.list(source)
  ) {
    abort_llw(
      "source must be created by new_glc_inspection_source().",
      type = "validation"
    )
  }
  required <- c(
    "registry_url",
    "registry_generated_at",
    "repository",
    "requested_revision",
    "resolved_revision",
    "validation_state",
    "attestation_verified",
    "schema_version",
    "glcdp_version"
  )
  missing <- setdiff(required, names(source))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "GLC inspection source is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  request <- new_glc_inspection_request(
    registry_url = source$registry_url,
    repository = source$repository,
    resolved_revision = source$resolved_revision,
    validation_state = source$validation_state,
    attestation_verified = source$attestation_verified,
    glcdp_version = source$glcdp_version,
    cache_dir = tempdir(),
    registry_generated_at = source$registry_generated_at
  )
  if (!identical(source$requested_revision, "latest_pass")) {
    abort_llw(
      "The inspected source must identify the latest passing revision.",
      type = "validation"
    )
  }
  new_glc_inspection_source(request, source$schema_version)
}

new_glc_inspection <- function(
  source,
  summary,
  datasets,
  files,
  variables,
  resources,
  metadata,
  metadata_snapshot = NULL,
  terms = glc_term_catalogue(variables),
  warnings = character()
) {
  source <- validate_glc_inspection_source(source)
  required <- glc_inspection_required_columns()
  values <- list(
    summary = summary,
    datasets = datasets,
    files = files,
    variables = variables,
    resources = resources
  )
  for (name in names(required)) {
    validate_glc_inspection_table(values[[name]], name, required[[name]])
  }
  if (
    !is.list(metadata) ||
      (length(metadata) > 0L && is.null(names(metadata)))
  ) {
    abort_llw(
      "GLC core metadata must be a named list.",
      type = "validation"
    )
  }
  if (is.null(metadata_snapshot)) {
    metadata_snapshot <- new_glc_source_metadata_snapshot(
      source = source,
      resources = resources,
      metadata = metadata,
      datasets = datasets,
      variables = variables
    )
  } else {
    metadata_snapshot <- validate_glc_source_metadata_snapshot(
      metadata_snapshot
    )
  }
  if (
    !identical(
      metadata_snapshot$source$repository,
      source$repository
    ) ||
      !identical(
        metadata_snapshot$source$resolved_revision,
        source$resolved_revision
      )
  ) {
    abort_llw(
      "The retained metadata snapshot identifies another GLC source.",
      type = "validation"
    )
  }
  validate_glc_inspection_table(
    terms,
    "term catalogue",
    c(
      "term",
      "label",
      "label_consistent",
      "dataset_count",
      "file_group_count",
      "variable_count",
      "term_names",
      "source_names",
      "source_labels",
      "descriptions"
    )
  )
  assert_character_vector(warnings, "warnings")
  expected_bytes <- as.numeric(files$expected_bytes)
  if (
    any(is.infinite(expected_bytes), na.rm = TRUE) ||
      any(expected_bytes < 0, na.rm = TRUE)
  ) {
    abort_llw(
      "The GLC file inventory contains invalid declared byte sizes.",
      type = "validation"
    )
  }

  inspection <- structure(
    list(
      source = source,
      summary = summary,
      datasets = datasets,
      files = files,
      variables = variables,
      resources = resources,
      metadata = metadata,
      metadata_snapshot = metadata_snapshot,
      terms = terms,
      warnings = unique(warnings)
    ),
    class = c("llw_glc_inspection", "list")
  )
  assert_serializable_value(inspection, "GLC inspection")
  inspection
}

validate_glc_inspection <- function(inspection) {
  if (!inherits(inspection, "llw_glc_inspection") || !is.list(inspection)) {
    abort_llw(
      "inspection must be created by new_glc_inspection().",
      type = "validation"
    )
  }
  required <- c(
    "source",
    "summary",
    "datasets",
    "files",
    "variables",
    "resources",
    "metadata",
    "metadata_snapshot",
    "terms",
    "warnings"
  )
  missing <- setdiff(required, names(inspection))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "GLC inspection is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  new_glc_inspection(
    source = inspection$source,
    summary = inspection$summary,
    datasets = inspection$datasets,
    files = inspection$files,
    variables = inspection$variables,
    resources = inspection$resources,
    metadata = inspection$metadata,
    metadata_snapshot = inspection$metadata_snapshot,
    terms = inspection$terms,
    warnings = inspection$warnings
  )
}

glcdp_schema_inspection_warnings <- function(summary, api) {
  validate_glc_inspection_table(
    summary,
    "summary",
    glc_inspection_required_columns()$summary
  )
  versions <- api$glc_schema_versions()
  validate_glc_inspection_table(
    versions,
    "schema versions",
    c("version", "status", "notes")
  )
  schema_version <- as.character(summary$schema_version[[1L]])
  matches <- which(as.character(versions$version) == schema_version)
  if (length(matches) != 1L) {
    abort_llw(
      paste0(
        "Schema ",
        schema_version,
        " is not uniquely supported by this glcdp runtime."
      ),
      type = "validation"
    )
  }
  status <- as.character(versions$status[[matches]])
  if (identical(status, "stable")) {
    return(character())
  }
  paste0(
    "Schema ",
    schema_version,
    " is reported as ",
    status,
    " by glcdp: ",
    as.character(versions$notes[[matches]])
  )
}

glc_task_progress_empty <- function() {
  data.frame(
    phase = character(),
    label = character(),
    status = character(),
    elapsed_seconds = numeric(),
    detail = character(),
    stringsAsFactors = FALSE
  )
}

glc_task_progress_snapshot <- function(
  completed,
  active_phase = NULL,
  active_label = NULL,
  active_detail = NULL,
  active_status = "running"
) {
  value <- if (length(completed) == 0L) {
    glc_task_progress_empty()
  } else {
    do.call(rbind, completed)
  }
  if (is.null(active_phase)) {
    rownames(value) <- NULL
    return(value)
  }
  value <- rbind(
    value,
    data.frame(
      phase = active_phase,
      label = active_label,
      status = active_status,
      elapsed_seconds = NA_real_,
      detail = active_detail,
      stringsAsFactors = FALSE
    )
  )
  rownames(value) <- NULL
  value
}

write_glc_task_progress <- function(
  progress_path,
  completed,
  active_phase = NULL,
  active_label = NULL,
  active_detail = NULL,
  active_status = "running"
) {
  if (
    is.null(progress_path) ||
      !is.character(progress_path) ||
      length(progress_path) != 1L ||
      is.na(progress_path) ||
      !dir.exists(dirname(progress_path))
  ) {
    return(invisible(FALSE))
  }
  progress <- glc_task_progress_snapshot(
    completed,
    active_phase = active_phase,
    active_label = active_label,
    active_detail = active_detail,
    active_status = active_status
  )
  written <- tryCatch(
    {
      temporary <- tempfile("glc-progress-", tmpdir = dirname(progress_path))
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      saveRDS(progress, temporary, version = 2)
      if (!file.rename(temporary, progress_path)) {
        file.copy(temporary, progress_path, overwrite = TRUE)
      } else {
        TRUE
      }
    },
    error = function(cnd) FALSE
  )
  invisible(isTRUE(written))
}

read_glc_task_progress <- function(progress_path) {
  if (
    is.null(progress_path) ||
      !is.character(progress_path) ||
      length(progress_path) != 1L ||
      is.na(progress_path) ||
      !file.exists(progress_path)
  ) {
    return(NULL)
  }
  progress <- tryCatch(readRDS(progress_path), error = function(cnd) NULL)
  required <- c(
    "phase",
    "label",
    "status",
    "elapsed_seconds",
    "detail"
  )
  if (!is.data.frame(progress) || !all(required %in% names(progress))) {
    return(NULL)
  }
  progress
}

new_glc_phase_log <- function(progress_path = NULL) {
  if (!is.null(progress_path)) {
    assert_scalar_string(progress_path, "progress_path")
    progress_path <- normalizePath(
      progress_path,
      winslash = "/",
      mustWork = FALSE
    )
  }
  state <- new.env(parent = emptyenv())
  state$completed <- list()
  state$active_phase <- NULL
  state$active_label <- NULL
  state$active_started <- NULL

  begin <- function(phase, label, detail) {
    assert_scalar_string(phase, "phase")
    assert_scalar_string(label, "label")
    assert_scalar_string(detail, "detail")
    state$active_phase <- phase
    state$active_label <- label
    state$active_started <- Sys.time()
    write_glc_task_progress(
      progress_path,
      state$completed,
      active_phase = phase,
      active_label = label,
      active_detail = detail
    )
    invisible(NULL)
  }

  complete <- function(detail) {
    assert_scalar_string(detail, "detail")
    if (is.null(state$active_phase) || is.null(state$active_started)) {
      abort_llw(
        "No active GLC task phase can be completed.",
        type = "validation"
      )
    }
    elapsed <- as.numeric(difftime(
      Sys.time(),
      state$active_started,
      units = "secs"
    ))
    state$completed[[length(state$completed) + 1L]] <- data.frame(
      phase = state$active_phase,
      label = state$active_label,
      status = "complete",
      elapsed_seconds = round(elapsed, 3),
      detail = detail,
      stringsAsFactors = FALSE
    )
    state$active_phase <- NULL
    state$active_label <- NULL
    state$active_started <- NULL
    write_glc_task_progress(progress_path, state$completed)
    invisible(NULL)
  }

  fail <- function(detail) {
    detail <- as.character(detail)[[1L]]
    if (!is.null(state$active_phase)) {
      write_glc_task_progress(
        progress_path,
        state$completed,
        active_phase = state$active_phase,
        active_label = state$active_label,
        active_detail = detail,
        active_status = "error"
      )
    }
    invisible(NULL)
  }

  list(begin = begin, complete = complete, fail = fail)
}

glc_inspection_source_spec <- function(inspection, cache_dir) {
  inspection <- validate_glc_inspection(inspection)
  assert_scalar_string(cache_dir, "cache_dir")
  source <- inspection$source
  new_glc_source_spec(
    registry_url = source$registry_url,
    repository = source$repository,
    requested_revision = "exact",
    resolved_revision = source$resolved_revision,
    validation_state = source$validation_state,
    attestation_verified = source$attestation_verified,
    schema_version = source$schema_version,
    glcdp_version = source$glcdp_version,
    cache_dir = cache_dir,
    registry_generated_at = source$registry_generated_at
  )
}

glc_value_contains_path <- function(value, path) {
  if (is.environment(value)) {
    return(TRUE)
  }
  if (is.character(value) && length(value) > 0L) {
    normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
    return(any(startsWith(value[!is.na(value)], normalized)))
  }
  if (is.list(value)) {
    return(any(vapply(value, glc_value_contains_path, logical(1), path = path)))
  }
  FALSE
}

assert_glc_worker_result_safe <- function(value, cache_dir) {
  assert_serializable_value(value, "GLC worker result")
  if (glc_value_contains_path(value, cache_dir)) {
    abort_llw(
      "A GLC worker result retained session transport or cache state.",
      type = "validation"
    )
  }
  invisible(value)
}

glcdp_inspect_source <- function(
  request,
  progress_path = NULL,
  api = NULL
) {
  request <- validate_glc_inspection_request(request)
  api <- glcdp_api(api)
  progress <- new_glc_phase_log(progress_path)
  tryCatch(
    {
      progress$begin(
        "open",
        "Opening exact revision",
        paste(
          "Opening",
          request$repository,
          "at",
          request$resolved_revision,
          "through the validated registry entry."
        )
      )
      handle <- api$glc_open(
        source = request$repository,
        ref = request$resolved_revision,
        cache_dir = request$cache_dir,
        registry = request$registry_url,
        quiet = TRUE
      )
      progress$complete("The exact passing revision was opened.")

      progress$begin(
        "description",
        "Reading package description",
        "Reading the validated package summary and schema information."
      )
      summary <- api$glc_summary(handle)
      schema_warnings <- glcdp_schema_inspection_warnings(summary, api)
      progress$complete("The package description and schema were validated.")

      progress$begin(
        "inventory",
        "Loading public inventories",
        paste(
          "Reading package datasets, file groups, files, variables,",
          "and metadata resources. Measurement contents are not read."
        )
      )
      datasets <- api$glc_datasets(handle)
      files <- api$glc_files(handle)
      variables <- api$glc_variables(handle)
      resources <- api$glc_resources(handle)
      progress$complete("The public inventory tables were loaded.")

      progress$begin(
        "metadata",
        "Loading validated metadata",
        paste(
          "Reading the package's public core metadata.",
          "Measurement files remain unopened."
        )
      )
      metadata <- api$glc_metadata(handle, resources = NULL)
      progress$complete("The public core metadata were loaded.")

      progress$begin(
        "metadata_relationships",
        "Resolving metadata relationships",
        paste(
          "Using exported glcdp metadata traversal once while the exact",
          "package is already open. Measurement files remain unopened."
        )
      )
      relationships <- glcdp_extract_relationship_context(
        handle = handle,
        files = files,
        metadata = metadata,
        api = api
      )
      progress$complete(
        "File-group and explicit dataset-level relationships were retained."
      )

      progress$begin(
        "metadata_index",
        "Indexing retained metadata",
        paste(
          "Building the versioned immutable-source contract and its",
          "in-memory search index. The package remains metadata-only."
        )
      )
      source_metadata_snapshot <- new_glc_source_metadata_snapshot(
        source = new_glc_inspection_source(
          request,
          as.character(summary$schema_version[[1L]])
        ),
        resources = resources,
        metadata = metadata,
        datasets = datasets,
        variables = variables,
        relationships = relationships
      )
      progress$complete(
        "The retained metadata and in-memory search index are ready."
      )

      progress$begin(
        "terms",
        "Building semantic-term catalogue",
        paste(
          "Deduplicating canonical term identifiers and calculating",
          "dataset and file-group coverage."
        )
      )
      terms <- glc_term_catalogue(variables)
      expected_bytes <- as.numeric(files$expected_bytes)
      byte_warnings <- if (
        length(expected_bytes) > 0L && anyNA(expected_bytes)
      ) {
        paste(
          sum(is.na(expected_bytes)),
          "measurement file size(s) are unavailable."
        )
      } else {
        character()
      }
      label_warnings <- if (any(!terms$label_consistent)) {
        paste(
          sum(!terms$label_consistent),
          paste(
            "canonical term(s) have inconsistent human labels;",
            "their canonical identifiers are shown as the primary label."
          )
        )
      } else {
        character()
      }
      progress$complete("Canonical terms and their coverage were indexed.")

      progress$begin(
        "validation",
        "Validating inspection result",
        paste(
          "Removing the live package boundary and validating the",
          "metadata-only inspection result."
        )
      )
      schema_version <- as.character(summary$schema_version[[1L]])
      inspection <- new_glc_inspection(
        source = new_glc_inspection_source(request, schema_version),
        summary = summary,
        datasets = datasets,
        files = files,
        variables = variables,
        resources = resources,
        metadata = metadata,
        metadata_snapshot = source_metadata_snapshot,
        terms = terms,
        warnings = c(schema_warnings, byte_warnings, label_warnings)
      )
      assert_glc_worker_result_safe(inspection, request$cache_dir)
      progress$complete("The metadata-only inspection result is ready.")
      inspection
    },
    error = function(cnd) {
      progress$fail(
        "This phase did not complete. Review the task error and retry."
      )
      if (inherits(cnd, "llw_error")) {
        stop(cnd)
      }
      stop(map_glcdp_condition(cnd))
    }
  )
}

new_glcdp_inspection_payload <- function(request, progress_path = NULL) {
  request <- validate_glc_inspection_request(request)
  if (!is.null(progress_path)) {
    assert_scalar_string(progress_path, "progress_path")
    progress_path <- normalizePath(
      progress_path,
      winslash = "/",
      mustWork = FALSE
    )
  }
  payload <- list(request = request, progress_path = progress_path)
  assert_serializable_value(payload, "glcdp inspection task payload")
  payload
}

glcdp_inspection_task_worker <- function(payload, spec) {
  if (!is.list(payload) || !"request" %in% names(payload)) {
    abort_llw(
      "The glcdp inspection task payload is incomplete.",
      type = "validation"
    )
  }
  glcdp_inspect_source(
    request = payload$request,
    progress_path = payload$progress_path %||% NULL
  )
}

glc_collection_plan_required_tables <- function() {
  list(
    units = c(
      "unit_id",
      "compatibility_id",
      "preferred",
      "dataset_count",
      "file_group_count",
      "variable_count",
      "file_count",
      "declared_bytes",
      "known_file_count",
      "unknown_file_count",
      "declared_bytes_complete",
      "harmonization_required",
      "harmonized_variables",
      "diagnostic_ids",
      "file_group_ids"
    ),
    groups = c(
      "status",
      "unit_id",
      "compatibility_id",
      "dataset_id",
      "file_group",
      "file_group_id",
      "study_id",
      "participant_id",
      "participant_associated",
      "study_link_status",
      "participant_link_status",
      "device_id",
      "device_link_status",
      "datasheet_id",
      "device_location",
      "device_location_type",
      "description",
      "instructions",
      "instrument_declared",
      "dataset_timezone",
      "dataset_latitude",
      "dataset_longitude",
      "format",
      "timezone",
      "modalities",
      "modality_other",
      "modality_other_type",
      "role",
      "data_state",
      "temporal_type",
      "temporal_value",
      "temporal_unit",
      "header_row",
      "preprocessing",
      "datetime_source",
      "datetime_date",
      "datetime_format",
      "datetime_time",
      "datetime_time_format",
      "selected_variables",
      "reason_codes",
      "messages"
    ),
    variables = c(
      "unit_id",
      "dataset_id",
      "file_group_id",
      "position",
      "name",
      "label",
      "description",
      "unit",
      "calibration",
      "type",
      "term",
      "term_name",
      "primary",
      "factor_values",
      "factor_labels",
      "factor_descriptions",
      "selection_origin"
    ),
    read_columns = c(
      "unit_id",
      "dataset_id",
      "file_group_id",
      "position",
      "name",
      "declared_type",
      "origin",
      "selected_for_output",
      "automatic",
      "message"
    ),
    output_columns = c(
      "unit_id",
      "position",
      "name",
      "source_declared_type",
      "expected_type",
      "origin",
      "automatic",
      "runtime_validation_required",
      "collision_validation_required",
      "message"
    ),
    files = c(
      "status",
      "unit_id",
      "dataset_id",
      "file_group_id",
      "position",
      "declared_path",
      "format",
      "encoding",
      "declared_bytes",
      "bytes_known"
    ),
    compatibility = c(
      "unit_id",
      "selected_names",
      "declared_types",
      "factor_values",
      "factor_labels",
      "factor_descriptions",
      "harmonization_required",
      "harmonized_variables",
      "diagnostic_ids",
      "timezone",
      "modalities",
      "role",
      "data_state",
      "datetime_source",
      "datetime_signature",
      "datetime_date",
      "datetime_format",
      "datetime_time",
      "datetime_time_format",
      "collection_values_ignored",
      "relationship_rule",
      "device_rule",
      "standardize",
      "standardize_affects_partition"
    ),
    extensions = c("dataset_id", "file_group_id", "metadata"),
    compatibility_sets = c(
      "compatibility_id",
      "dataset_count",
      "file_group_count",
      "variable_count",
      "file_count",
      "declared_bytes",
      "known_file_count",
      "unknown_file_count",
      "declared_bytes_complete",
      "harmonization_required",
      "harmonized_variables",
      "diagnostic_ids",
      "final_unit_count",
      "final_selection_required",
      "constraint_codes",
      "constraint_messages",
      "file_group_ids",
      "final_unit_ids",
      "selected_names",
      "declared_types",
      "factor_values",
      "factor_labels",
      "factor_descriptions",
      "timezone",
      "modalities",
      "role",
      "data_state",
      "datetime_source",
      "datetime_signature",
      "datetime_date",
      "datetime_format",
      "datetime_time",
      "datetime_time_format",
      "collection_values_ignored",
      "relationship_rule",
      "device_rule",
      "standardize_affects_structure"
    ),
    compatibility_diagnostics = c(
      "diagnostic_id",
      "selection_scope",
      "classification",
      "code",
      "variable_name",
      "applies_to_current_plan",
      "prospective_scopes",
      "message",
      "affected_group_count",
      "affected_structure_count",
      "affected_unit_count",
      "file_group_ids",
      "compatibility_ids",
      "unit_ids",
      "union_values",
      "union_labels",
      "union_descriptions"
    ),
    compatibility_diagnostic_groups = c(
      "diagnostic_id",
      "dataset_id",
      "file_group_id",
      "current_status",
      "compatibility_id",
      "unit_id",
      "variable_present",
      "selected_by_request",
      "declaration_position",
      "declared_type",
      "factor_values",
      "factor_labels",
      "factor_descriptions"
    )
  )
}

glc_collection_plan_metadata_tables <- function() {
  list(
    resource_status = c("resource", "declared", "status", "record_count"),
    studies = c(
      "study_id",
      "schema_version",
      "title",
      "short_description",
      "preregistration",
      "registration",
      "ethics",
      "sample",
      "intervention",
      "setting",
      "geographical_location",
      "study_type",
      "funding_sources",
      "keywords",
      "dataset_ids"
    ),
    study_groups = c(
      "study_id",
      "position",
      "name",
      "description",
      "size",
      "inclusion",
      "exclusion",
      "dataset_ids"
    ),
    study_contributors = c(
      "study_id",
      "position",
      "full_name",
      "roles",
      "email",
      "orcid",
      "institution_name",
      "institution_city",
      "institution_country"
    ),
    contributors = c(
      "contributor_id",
      "position",
      "full_name",
      "roles",
      "email",
      "orcid",
      "institution_name",
      "institution_city",
      "institution_country"
    ),
    datasets = c(
      "dataset_id",
      "schema_version",
      "study_id",
      "study_link_status",
      "participant_id",
      "participant_associated",
      "participant_link_status",
      "timezone",
      "latitude",
      "longitude",
      "file_group_count",
      "file_count",
      "modalities",
      "device_ids",
      "primary_variables"
    ),
    dataset_terms = c("dataset_id", "position", "term", "label"),
    participants = c("participant_id", "age", "sex", "gender"),
    participant_characteristics = c(
      "participant_id",
      "participant_link_status",
      "characteristic_position",
      "value_position",
      "name",
      "value",
      "value_type",
      "unit",
      "description"
    ),
    devices = c(
      "device_id",
      "schema_version",
      "manufacturer",
      "model",
      "serial_number",
      "calibration_date",
      "firmware_version",
      "datasheet_id",
      "datasheet_link_status"
    ),
    device_sensors = c(
      "device_id",
      "position",
      "sensor_type",
      "datasheet_id",
      "datasheet_link_status"
    ),
    datasheets = c(
      "datasheet_id",
      "schema_version",
      "datasheet_version",
      "manufacturer",
      "type",
      "modalities",
      "modality_other",
      "model",
      "calibration_interval",
      "calibration_method",
      "calibration_accuracy",
      "calibration_range",
      "calibration_notes",
      "calibration_spectral_sensitivity",
      "calibration_linearity",
      "calibration_directional_response"
    ),
    datasheet_parameters = c(
      "datasheet_id",
      "position",
      "name",
      "value",
      "value_type",
      "unit",
      "description"
    ),
    datasheet_channels = c(
      "datasheet_id",
      "position",
      "channel_number",
      "name",
      "description",
      "unit"
    ),
    instruments = c(
      "dataset_id",
      "file_group_id",
      "instrument_type",
      "instrument_name",
      "collection_method",
      "recorded_by",
      "software_name"
    ),
    file_group_variables = c(
      "dataset_id",
      "file_group_id",
      "declaration_position",
      "selected_by_request",
      "selected_for_output",
      "selection_origin",
      "name",
      "label",
      "description",
      "unit",
      "calibration",
      "type",
      "term",
      "term_name",
      "primary",
      "factor_level_count"
    ),
    file_group_factor_levels = c(
      "dataset_id",
      "file_group_id",
      "variable_position",
      "variable_name",
      "level_position",
      "value",
      "label",
      "description"
    ),
    extensions = c(
      "resource",
      "entity_type",
      "entity_id",
      "parent_id",
      "position",
      "metadata"
    )
  )
}

glc_metadata_ids <- function(values) {
  values <- unlist(values, recursive = TRUE, use.names = FALSE)
  glc_nonempty_unique(values)
}

glc_metadata_rows <- function(table, column, ids) {
  if (!is.data.frame(table) || !column %in% names(table)) {
    abort_llw(
      paste0("Normalized metadata table is missing `", column, "`."),
      type = "validation"
    )
  }
  if (length(ids) == 0L) return(table[FALSE, , drop = FALSE])
  table[as.character(table[[column]]) %in% ids, , drop = FALSE]
}

new_glc_selected_metadata <- function(parent_plan, selected_groups) {
  if (
    !inherits(parent_plan, "glc_collection_plan") ||
      !is.list(parent_plan) ||
      !is.list(parent_plan$metadata) ||
      !is.data.frame(selected_groups)
  ) {
    abort_llw(
      "Selected GLC metadata requires one validated public collection plan.",
      type = "validation"
    )
  }
  required_group_columns <- c(
    "dataset_id",
    "file_group_id",
    "study_id",
    "participant_id",
    "device_id",
    "datasheet_id",
    "device_location",
    "device_location_type",
    "dataset_timezone",
    "dataset_latitude",
    "dataset_longitude"
  )
  if (!all(required_group_columns %in% names(selected_groups))) {
    abort_llw(
      "The selected GLC groups lack public metadata context.",
      type = "validation"
    )
  }
  metadata_contract <- glc_collection_plan_metadata_tables()
  if (
    !all(names(metadata_contract) %in% names(parent_plan$metadata)) ||
      !is.character(parent_plan$provenance$metadata_fingerprint) ||
      length(parent_plan$provenance$metadata_fingerprint) != 1L ||
      is.na(parent_plan$provenance$metadata_fingerprint) ||
      !grepl(
        "^[0-9a-f]{64}$",
        parent_plan$provenance$metadata_fingerprint
      )
  ) {
    abort_llw(
      "The parent plan has invalid normalized metadata provenance.",
      type = "validation"
    )
  }

  metadata <- parent_plan$metadata
  dataset_ids <- glc_metadata_ids(selected_groups$dataset_id)
  file_group_ids <- glc_metadata_ids(selected_groups$file_group_id)
  datasets <- glc_metadata_rows(metadata$datasets, "dataset_id", dataset_ids)
  study_ids <- glc_metadata_ids(c(selected_groups$study_id, datasets$study_id))
  participant_ids <- glc_metadata_ids(c(
    selected_groups$participant_id,
    datasets$participant_id
  ))
  device_ids <- glc_metadata_ids(c(
    selected_groups$device_id,
    datasets$device_ids
  ))
  devices <- glc_metadata_rows(metadata$devices, "device_id", device_ids)
  device_sensors <- glc_metadata_rows(
    metadata$device_sensors,
    "device_id",
    device_ids
  )
  datasheet_ids <- glc_metadata_ids(c(
    selected_groups$datasheet_id,
    devices$datasheet_id,
    device_sensors$datasheet_id
  ))
  # The public package-level contributors table has no study or dataset
  # foreign key. Retain it as package provenance instead of silently dropping
  # records that cannot be scoped more narrowly.
  contributor_ids <- glc_metadata_ids(metadata$contributors$contributor_id)
  selected_entity_ids <- glc_metadata_ids(c(
    dataset_ids,
    file_group_ids,
    study_ids,
    participant_ids,
    device_ids,
    datasheet_ids,
    contributor_ids
  ))
  extension_rows <- metadata$extensions[
    as.character(metadata$extensions$entity_id) %in%
      selected_entity_ids |
      as.character(metadata$extensions$parent_id) %in% selected_entity_ids,
    ,
    drop = FALSE
  ]
  study_contributors <- glc_metadata_rows(
    metadata$study_contributors,
    "study_id",
    study_ids
  )
  tables <- list(
    resource_status = metadata$resource_status,
    studies = glc_metadata_rows(metadata$studies, "study_id", study_ids),
    study_groups = glc_metadata_rows(
      metadata$study_groups,
      "study_id",
      study_ids
    ),
    study_contributors = study_contributors,
    contributors = metadata$contributors,
    datasets = datasets,
    dataset_terms = glc_metadata_rows(
      metadata$dataset_terms,
      "dataset_id",
      dataset_ids
    ),
    participants = glc_metadata_rows(
      metadata$participants,
      "participant_id",
      participant_ids
    ),
    participant_characteristics = glc_metadata_rows(
      metadata$participant_characteristics,
      "participant_id",
      participant_ids
    ),
    devices = devices,
    device_sensors = device_sensors,
    datasheets = glc_metadata_rows(
      metadata$datasheets,
      "datasheet_id",
      datasheet_ids
    ),
    datasheet_parameters = glc_metadata_rows(
      metadata$datasheet_parameters,
      "datasheet_id",
      datasheet_ids
    ),
    datasheet_channels = glc_metadata_rows(
      metadata$datasheet_channels,
      "datasheet_id",
      datasheet_ids
    ),
    instruments = metadata$instruments[
      as.character(metadata$instruments$dataset_id) %in%
        dataset_ids &
        as.character(metadata$instruments$file_group_id) %in% file_group_ids,
      ,
      drop = FALSE
    ],
    file_group_variables = metadata$file_group_variables[
      as.character(metadata$file_group_variables$dataset_id) %in%
        dataset_ids &
        as.character(metadata$file_group_variables$file_group_id) %in%
          file_group_ids,
      ,
      drop = FALSE
    ],
    file_group_factor_levels = metadata$file_group_factor_levels[
      as.character(metadata$file_group_factor_levels$dataset_id) %in%
        dataset_ids &
        as.character(metadata$file_group_factor_levels$file_group_id) %in%
          file_group_ids,
      ,
      drop = FALSE
    ],
    extensions = extension_rows
  )
  selected <- structure(
    list(
      metadata_schema = "llw-glc-selected-metadata",
      metadata_version = "1.0.0",
      upstream_schema = parent_plan$metadata$schema,
      upstream_version = parent_plan$metadata$version,
      upstream_fingerprint = parent_plan$provenance$metadata_fingerprint,
      selected_ids = list(
        file_group_ids = file_group_ids,
        dataset_ids = dataset_ids,
        study_ids = study_ids,
        participant_ids = participant_ids,
        device_ids = device_ids,
        datasheet_ids = datasheet_ids
      ),
      file_group_context = selected_groups[,
        required_group_columns,
        drop = FALSE
      ],
      tables = tables
    ),
    class = c("llw_glc_selected_metadata", "list")
  )
  selected$fingerprint <- glc_transfer_value_fingerprint(selected)
  validate_glc_selected_metadata(selected)
  selected
}

validate_glc_selected_metadata <- function(metadata) {
  if (!inherits(metadata, "llw_glc_selected_metadata") || !is.list(metadata)) {
    abort_llw("The selected GLC metadata is invalid.", type = "validation")
  }
  required <- c(
    "metadata_schema",
    "metadata_version",
    "upstream_schema",
    "upstream_version",
    "upstream_fingerprint",
    "selected_ids",
    "file_group_context",
    "tables",
    "fingerprint"
  )
  id_names <- c(
    "file_group_ids",
    "dataset_ids",
    "study_ids",
    "participant_ids",
    "device_ids",
    "datasheet_ids"
  )
  context_columns <- c(
    "dataset_id",
    "file_group_id",
    "study_id",
    "participant_id",
    "device_id",
    "datasheet_id",
    "device_location",
    "device_location_type",
    "dataset_timezone",
    "dataset_latitude",
    "dataset_longitude"
  )
  if (
    !all(required %in% names(metadata)) ||
      !identical(metadata$metadata_schema, "llw-glc-selected-metadata") ||
      !identical(metadata$metadata_version, "1.0.0") ||
      !identical(metadata$upstream_schema, "glc-package-metadata") ||
      !identical(metadata$upstream_version, "1.0.0") ||
      !is.character(metadata$upstream_fingerprint) ||
      length(metadata$upstream_fingerprint) != 1L ||
      is.na(metadata$upstream_fingerprint) ||
      !grepl("^[0-9a-f]{64}$", metadata$upstream_fingerprint) ||
      !is.list(metadata$selected_ids) ||
      !identical(names(metadata$selected_ids), id_names) ||
      !is.data.frame(metadata$file_group_context) ||
      !all(context_columns %in% names(metadata$file_group_context)) ||
      !is.list(metadata$tables)
  ) {
    abort_llw(
      "The selected GLC metadata contract is incomplete.",
      type = "validation"
    )
  }
  metadata_contract <- glc_collection_plan_metadata_tables()
  if (!all(names(metadata_contract) %in% names(metadata$tables))) {
    abort_llw(
      "The selected GLC metadata lacks normalized tables.",
      type = "validation"
    )
  }
  for (name in names(metadata_contract)) {
    validate_glc_inspection_table(
      metadata$tables[[name]],
      paste("selected GLC metadata", name),
      metadata_contract[[name]]
    )
  }
  ids <- lapply(metadata$selected_ids, glc_metadata_ids)
  if (
    length(ids$file_group_ids) == 0L ||
      length(ids$dataset_ids) == 0L ||
      !identical(
        glc_metadata_ids(metadata$file_group_context$file_group_id),
        ids$file_group_ids
      ) ||
      !identical(
        glc_metadata_ids(metadata$file_group_context$dataset_id),
        ids$dataset_ids
      ) ||
      any(
        !as.character(metadata$tables$datasets$dataset_id) %in%
          ids$dataset_ids
      ) ||
      any(
        !as.character(metadata$tables$dataset_terms$dataset_id) %in%
          ids$dataset_ids
      ) ||
      any(
        !as.character(metadata$tables$instruments$file_group_id) %in%
          ids$file_group_ids
      ) ||
      any(
        !as.character(metadata$tables$file_group_variables$file_group_id) %in%
          ids$file_group_ids
      ) ||
      any(
        !as.character(
          metadata$tables$file_group_factor_levels$file_group_id
        ) %in%
          ids$file_group_ids
      )
  ) {
    abort_llw(
      "The selected GLC metadata does not match its fixed membership.",
      type = "validation"
    )
  }
  latitude <- suppressWarnings(as.numeric(c(
    metadata$file_group_context$dataset_latitude,
    metadata$tables$datasets$latitude
  )))
  longitude <- suppressWarnings(as.numeric(c(
    metadata$file_group_context$dataset_longitude,
    metadata$tables$datasets$longitude
  )))
  if (
    any(
      !is.na(latitude) &
        (!is.finite(latitude) | latitude < -90 | latitude > 90)
    ) ||
      any(
        !is.na(longitude) &
          (!is.finite(longitude) | longitude < -180 | longitude > 180)
      )
  ) {
    abort_llw(
      "The selected GLC metadata contains invalid coordinates.",
      type = "validation"
    )
  }
  fingerprint_value <- metadata
  fingerprint_value$fingerprint <- NULL
  if (
    !is.character(metadata$fingerprint) ||
      length(metadata$fingerprint) != 1L ||
      is.na(metadata$fingerprint) ||
      !identical(
        metadata$fingerprint,
        glc_transfer_value_fingerprint(fingerprint_value)
      )
  ) {
    abort_llw(
      "The selected GLC metadata fingerprint is invalid.",
      type = "validation"
    )
  }
  assert_serializable_value(metadata, "Selected GLC metadata")
  invisible(metadata)
}

new_glc_planning_request <- function(
  term = NULL,
  modality = NULL,
  file_group_ids = character(),
  variable_scope = NULL,
  source_variables = character()
) {
  normalize_optional_scalar <- function(value, name) {
    if (is.null(value)) return(NULL)
    assert_scalar_string(value, name)
    value <- trimws(enc2utf8(value))
    if (!nzchar(value)) return(NULL)
    value
  }
  term <- normalize_optional_scalar(term, "term")
  modality <- normalize_optional_scalar(modality, "modality")
  file_group_ids <- glc_nonempty_unique(file_group_ids)
  raw_source_variables <- as.character(source_variables)
  raw_source_variables <- trimws(enc2utf8(
    raw_source_variables[!is.na(raw_source_variables)]
  ))
  raw_source_variables <- raw_source_variables[nzchar(raw_source_variables)]
  if (anyDuplicated(raw_source_variables)) {
    abort_llw(
      "Custom variables must use unique exact source names.",
      type = "validation"
    )
  }
  source_variables <- sort(raw_source_variables, method = "radix")
  if (is.null(variable_scope)) {
    variable_scope <- "all"
  }
  assert_scalar_string(variable_scope, "variable_scope")
  variable_scope <- match.arg(variable_scope, c("matched", "all", "selected"))
  if (is.null(term) && is.null(modality) && length(file_group_ids) == 0L) {
    abort_llw(
      paste(
        "Choose a canonical term, a modality, or stable file groups before",
        "planning."
      ),
      type = "validation"
    )
  }
  if (!is.null(modality) && length(file_group_ids) == 0L) {
    abort_llw(
      "The selected modality has no stable file-group identifiers.",
      type = "validation"
    )
  }
  if (identical(variable_scope, "matched") && is.null(term)) {
    abort_llw(
      "Matched variable scope requires a canonical term.",
      type = "validation"
    )
  }
  if (identical(variable_scope, "selected") && length(source_variables) == 0L) {
    abort_llw(
      "Custom variable scope requires at least one exact source name.",
      type = "validation"
    )
  }
  if (!identical(variable_scope, "selected") && length(source_variables) > 0L) {
    abort_llw(
      "Exact source variables are valid only for custom variable scope.",
      type = "validation"
    )
  }
  request <- structure(
    list(
      term = term,
      modality = modality,
      file_group_ids = file_group_ids,
      variable_scope = variable_scope,
      source_variables = source_variables
    ),
    class = c("llw_glc_planning_request", "list")
  )
  assert_serializable_value(request, "GLC planning request")
  request
}

validate_glc_planning_request <- function(request) {
  if (!inherits(request, "llw_glc_planning_request") || !is.list(request)) {
    abort_llw(
      "Planning request must be created by new_glc_planning_request().",
      type = "validation"
    )
  }
  rebuilt <- new_glc_planning_request(
    term = request$term,
    modality = request$modality,
    file_group_ids = request$file_group_ids,
    variable_scope = request$variable_scope,
    source_variables = request$source_variables
  )
  if (!identical(request, rebuilt)) {
    abort_llw("The GLC planning request is not canonical.", type = "validation")
  }
  request
}

validate_glc_collection_plan <- function(plan, source, request) {
  source <- validate_glc_source_spec(source)
  request <- validate_glc_planning_request(request)
  if (!inherits(plan, "glc_collection_plan") || !is.list(plan)) {
    abort_llw(
      "glcdp did not return a glc_collection_plan object.",
      type = "validation"
    )
  }
  required <- c(
    "plan_schema",
    "plan_version",
    "provenance",
    "request",
    "assurance",
    "preferred_unit_id",
    names(glc_collection_plan_required_tables()),
    "metadata",
    "refinement_input"
  )
  missing <- setdiff(required, names(plan))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "GLC collection plan is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  if (
    !identical(plan$plan_schema, "glc-collection-plan") ||
      !identical(plan$plan_version, "1.2.0")
  ) {
    abort_llw(
      "The glcdp collection-plan schema is not supported.",
      type = "validation"
    )
  }
  provenance_required <- c(
    "repository",
    "source_revision",
    "verification",
    "metadata_fingerprint",
    "planner_schema",
    "planner_version"
  )
  if (
    !is.list(plan$provenance) ||
      !all(provenance_required %in% names(plan$provenance))
  ) {
    abort_llw(
      "The glcdp plan provenance is incomplete.",
      type = "validation"
    )
  }
  if (
    !identical(plan$provenance$repository, source$repository) ||
      !identical(
        tolower(plan$provenance$source_revision),
        source$resolved_revision
      )
  ) {
    abort_llw(
      "The collection plan does not match the selected exact source.",
      type = "validation"
    )
  }
  expected_terms <- request$term %||% character()
  expected_file_groups <- request$file_group_ids
  expected_variables <- if (identical(request$variable_scope, "selected")) {
    request$source_variables
  } else {
    character()
  }
  if (
    !is.list(plan$request) ||
      !identical(plan$request$variable_scope, request$variable_scope) ||
      !identical(as.character(plan$request$terms), expected_terms) ||
      !identical(
        sort(as.character(plan$request$requested_variables), method = "radix"),
        expected_variables
      ) ||
      !identical(as.character(plan$request$file_group), expected_file_groups) ||
      !identical(plan$request$standardize, "lightlogr")
  ) {
    abort_llw(
      "The collection plan does not match the requested discovery selection.",
      type = "validation"
    )
  }
  if (
    !is.list(plan$assurance) ||
      !identical(plan$assurance$measurement_contents_transferred, FALSE) ||
      !identical(plan$assurance$measurement_contents_inspected, FALSE) ||
      !identical(plan$assurance$interactive_filtering_network_access, FALSE) ||
      !identical(plan$assurance$refinement_network_access, FALSE)
  ) {
    abort_llw(
      "The collection plan lacks the required metadata-only assurance.",
      type = "validation"
    )
  }
  table_contract <- glc_collection_plan_required_tables()
  for (name in names(table_contract)) {
    validate_glc_inspection_table(
      plan[[name]],
      paste("collection plan", name),
      table_contract[[name]]
    )
  }
  metadata_contract <- glc_collection_plan_metadata_tables()
  if (
    !is.list(plan$metadata) ||
      !identical(plan$metadata$schema, "glc-package-metadata") ||
      !identical(plan$metadata$version, "1.0.0")
  ) {
    abort_llw(
      "The glcdp collection plan has unsupported normalized metadata.",
      type = "validation"
    )
  }
  missing_metadata <- setdiff(
    names(metadata_contract),
    names(plan$metadata)
  )
  if (length(missing_metadata) > 0L) {
    abort_llw(
      paste0(
        "Normalized GLC metadata is missing table(s): ",
        paste(missing_metadata, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  for (name in names(metadata_contract)) {
    validate_glc_inspection_table(
      plan$metadata[[name]],
      paste("collection plan metadata", name),
      metadata_contract[[name]]
    )
  }
  if (
    !is.list(plan$refinement_input) ||
      !identical(
        plan$refinement_input$schema,
        "glc-collection-refinement-input"
      ) ||
      !identical(plan$refinement_input$version, "1.1.0") ||
      !is.character(plan$refinement_input$fingerprint) ||
      length(plan$refinement_input$fingerprint) != 1L ||
      is.na(plan$refinement_input$fingerprint) ||
      !grepl("^[0-9a-f]{64}$", plan$refinement_input$fingerprint)
  ) {
    abort_llw(
      "The glcdp collection plan has invalid refinement input.",
      type = "validation"
    )
  }
  unit_ids <- as.character(plan$units$unit_id)
  if (
    anyNA(unit_ids) ||
      any(!nzchar(unit_ids)) ||
      anyDuplicated(unit_ids)
  ) {
    abort_llw(
      "The collection plan contains invalid stable unit identifiers.",
      type = "validation"
    )
  }
  if (
    nrow(plan$units) > 0L &&
      !is.na(plan$preferred_unit_id) &&
      !plan$preferred_unit_id %in% unit_ids
  ) {
    abort_llw(
      "The preferred collection unit is not present in the plan.",
      type = "validation"
    )
  }
  compatibility_ids <- as.character(plan$compatibility_sets$compatibility_id)
  if (
    anyNA(compatibility_ids) ||
      any(!grepl("^glcc_[0-9a-f]{64}$", compatibility_ids)) ||
      anyDuplicated(compatibility_ids)
  ) {
    abort_llw(
      "The collection plan contains invalid structural compatibility identifiers.",
      type = "validation"
    )
  }
  diagnostic_ids <- as.character(plan$compatibility_diagnostics$diagnostic_id)
  if (
    anyNA(diagnostic_ids) ||
      any(!nzchar(diagnostic_ids)) ||
      anyDuplicated(diagnostic_ids)
  ) {
    abort_llw(
      "The collection plan contains invalid compatibility diagnostics.",
      type = "validation"
    )
  }
  diagnostic_group_ids <- as.character(
    plan$compatibility_diagnostic_groups$diagnostic_id
  )
  if (
    anyNA(diagnostic_group_ids) ||
      any(!diagnostic_group_ids %in% diagnostic_ids)
  ) {
    abort_llw(
      "Compatibility diagnostic groups do not map to public diagnostics.",
      type = "validation"
    )
  }
  included_groups <- plan$groups$status == "included"
  if (
    any(included_groups) &&
      any(!plan$groups$compatibility_id[included_groups] %in% compatibility_ids)
  ) {
    abort_llw(
      "Included file groups do not map to a structural compatibility set.",
      type = "validation"
    )
  }
  assert_glc_worker_result_safe(plan, source$cache_dir)
  invisible(plan)
}

glc_scope_parent_identity <- function(
  source,
  parent_request,
  parent_plan,
  parent_compatibility_id,
  parent_refinement
) {
  source <- validate_glc_source_spec(source)
  parent_request <- validate_glc_planning_request(parent_request)
  validate_glc_collection_plan(parent_plan, source, parent_request)
  assert_scalar_string(
    parent_compatibility_id,
    "parent_compatibility_id"
  )
  if (
    !inherits(parent_refinement, "glc_collection_refinement") ||
      !is.list(parent_refinement) ||
      !is.list(parent_refinement$request)
  ) {
    abort_llw(
      "Output projection requires a valid finalized metadata refinement.",
      type = "validation"
    )
  }
  refined_file_group_ids <- glc_nonempty_unique(
    parent_refinement$request$file_group %||% character()
  )
  validate_glc_collection_refinement(
    parent_refinement,
    parent_plan,
    parent_compatibility_id,
    refined_file_group_ids
  )
  if (length(refined_file_group_ids) == 0L) {
    abort_llw(
      "Output projection requires a finalized metadata refinement.",
      type = "validation"
    )
  }
  list(
    source = source,
    request = parent_request,
    plan = parent_plan,
    compatibility_id = parent_compatibility_id,
    refinement = parent_refinement,
    refined_file_group_ids = refined_file_group_ids
  )
}

glc_projection_refined_unit_map <- function(
  groups,
  parent_refinement
) {
  required_group_columns <- c("file_group_id", "unit_id")
  if (
    !is.data.frame(groups) ||
      !all(required_group_columns %in% names(groups)) ||
      !is.data.frame(parent_refinement$groups) ||
      !all(required_group_columns %in% names(parent_refinement$groups)) ||
      !is.data.frame(parent_refinement$units) ||
      !all(c("unit_id", "file_group_ids") %in% names(parent_refinement$units))
  ) {
    abort_llw(
      "The refined unit membership is incomplete.",
      type = "validation"
    )
  }

  group_ids <- as.character(groups$file_group_id)
  refined_groups <- parent_refinement$groups[
    match(group_ids, parent_refinement$groups$file_group_id),
    required_group_columns,
    drop = FALSE
  ]
  selected_unit_ids <- glc_nonempty_unique(parent_refinement$units$unit_id)
  if (
    anyNA(refined_groups$file_group_id) ||
      anyNA(refined_groups$unit_id) ||
      anyDuplicated(parent_refinement$groups$file_group_id) ||
      !identical(as.character(refined_groups$file_group_id), group_ids) ||
      length(selected_unit_ids) == 0L ||
      any(!grepl("^glcu_[0-9a-f]{64}$", refined_groups$unit_id)) ||
      any(!refined_groups$unit_id %in% selected_unit_ids)
  ) {
    abort_llw(
      "The refined file groups do not identify stable public units.",
      type = "validation"
    )
  }

  mapping <- data.frame(
    file_group_id = group_ids,
    parent_unit_id = as.character(groups$unit_id),
    refined_unit_id = as.character(refined_groups$unit_id),
    stringsAsFactors = FALSE
  )
  if (
    anyNA(mapping$parent_unit_id) ||
      any(!grepl("^glcu_[0-9a-f]{64}$", mapping$parent_unit_id))
  ) {
    abort_llw(
      "The retained parent groups do not identify stable public units.",
      type = "validation"
    )
  }

  for (unit_id in selected_unit_ids) {
    unit_row <- which(parent_refinement$units$unit_id == unit_id)
    mapped_group_ids <- mapping$file_group_id[
      mapping$refined_unit_id == unit_id
    ]
    if (
      length(unit_row) != 1L ||
        !setequal(
          glc_nonempty_unique(
            parent_refinement$units$file_group_ids[[unit_row]]
          ),
          mapped_group_ids
        )
    ) {
      abort_llw(
        "The refined unit table contradicts its public group membership.",
        type = "validation"
      )
    }
    parent_ids <- glc_nonempty_unique(
      mapping$parent_unit_id[mapping$refined_unit_id == unit_id]
    )
    if (length(parent_ids) != 1L) {
      abort_llw(
        paste(
          "A refined unit does not map to one retained public column",
          "contract. Reconfirm the metadata selection before continuing."
        ),
        type = "validation"
      )
    }
  }

  list(
    groups = mapping,
    selected_unit_ids = selected_unit_ids
  )
}

new_glc_projection_review <- function(
  source,
  source_variables,
  parent_request,
  parent_plan,
  parent_compatibility_id,
  parent_refinement,
  metadata_filters = list(),
  source_metadata_snapshot = NULL
) {
  source <- validate_glc_source_spec(source)
  parent <- glc_scope_parent_identity(
    source,
    parent_request,
    parent_plan,
    parent_compatibility_id,
    parent_refinement
  )
  refined_file_group_ids <- parent$refined_file_group_ids
  if (!identical(parent_request$variable_scope, "all")) {
    abort_llw(
      paste(
        "Variable projection requires a Slice 2A plan that validated the",
        "complete file schema."
      ),
      type = "validation"
    )
  }
  source_variables <- trimws(enc2utf8(as.character(source_variables)))
  source_variables <- source_variables[
    !is.na(source_variables) & nzchar(source_variables)
  ]
  if (length(source_variables) == 0L) {
    abort_llw(
      "Choose at least one exact source variable for the output projection.",
      type = "validation"
    )
  }
  if (anyDuplicated(source_variables)) {
    abort_llw(
      "Output projection variables must use unique exact source names.",
      type = "validation"
    )
  }
  source_variables <- sort(source_variables, method = "radix")
  if (!is.list(metadata_filters)) {
    abort_llw(
      "Metadata-filter provenance must be a serializable list.",
      type = "validation"
    )
  }
  assert_serializable_value(metadata_filters, "Metadata-filter provenance")

  groups <- parent_plan$groups[
    parent_plan$groups$status == "included" &
      parent_plan$groups$compatibility_id == parent_compatibility_id &
      parent_plan$groups$file_group_id %in% refined_file_group_ids,
    ,
    drop = FALSE
  ]
  group_ids <- glc_nonempty_unique(groups$file_group_id)
  if (
    length(group_ids) == 0L ||
      !setequal(group_ids, refined_file_group_ids)
  ) {
    abort_llw(
      "The retained Slice 2A structure does not match the refined groups.",
      type = "validation"
    )
  }
  unit_map <- glc_projection_refined_unit_map(groups, parent_refinement)
  selected_unit_ids <- unit_map$selected_unit_ids
  group_unit_rows <- match(
    groups$file_group_id,
    unit_map$groups$file_group_id
  )
  groups$unit_id <- unit_map$groups$refined_unit_id[group_unit_rows]

  variable_rows <- parent_plan$variables$file_group_id %in% group_ids
  available <- parent_plan$variables[variable_rows, , drop = FALSE]
  availability <- vapply(
    source_variables,
    function(name) {
      setequal(
        glc_nonempty_unique(available$file_group_id[available$name == name]),
        group_ids
      )
    },
    logical(1)
  )
  if (any(!availability)) {
    abort_llw(
      paste0(
        "Exact source variable(s) are not available in every retained file ",
        "group: ",
        paste(source_variables[!availability], collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }

  files <- parent_plan$files[
    parent_plan$files$status == "included" &
      parent_plan$files$file_group_id %in% group_ids,
    ,
    drop = FALSE
  ]
  file_unit_rows <- match(files$file_group_id, unit_map$groups$file_group_id)
  if (
    anyNA(file_unit_rows) ||
      any(files$unit_id != unit_map$groups$parent_unit_id[file_unit_rows])
  ) {
    abort_llw(
      "The retained file table contradicts the parent unit membership.",
      type = "validation"
    )
  }
  files$unit_id <- unit_map$groups$refined_unit_id[file_unit_rows]
  read_columns <- parent_plan$read_columns[
    parent_plan$read_columns$file_group_id %in%
      group_ids &
      (parent_plan$read_columns$name %in%
        source_variables |
        parent_plan$read_columns$automatic %in% TRUE),
    ,
    drop = FALSE
  ]
  read_unit_rows <- match(
    read_columns$file_group_id,
    unit_map$groups$file_group_id
  )
  if (
    anyNA(read_unit_rows) ||
      any(
        read_columns$unit_id != unit_map$groups$parent_unit_id[read_unit_rows]
      )
  ) {
    abort_llw(
      "The retained read-column contract contradicts its parent units.",
      type = "validation"
    )
  }
  read_columns$unit_id <-
    unit_map$groups$refined_unit_id[read_unit_rows]

  output_columns <- do.call(
    rbind,
    lapply(selected_unit_ids, function(unit_id) {
      parent_unit_id <- glc_nonempty_unique(
        unit_map$groups$parent_unit_id[
          unit_map$groups$refined_unit_id == unit_id
        ]
      )
      rows <- parent_plan$output_columns[
        parent_plan$output_columns$unit_id == parent_unit_id &
          (parent_plan$output_columns$name %in%
            source_variables |
            parent_plan$output_columns$automatic %in% TRUE),
        ,
        drop = FALSE
      ]
      rows$unit_id <- unit_id
      rows
    })
  )
  rownames(output_columns) <- NULL
  diagnostic_rows <- vapply(
    parent_plan$compatibility_diagnostics$compatibility_ids,
    function(ids) parent_compatibility_id %in% as.character(ids),
    logical(1)
  )
  diagnostics <- parent_plan$compatibility_diagnostics[
    diagnostic_rows,
    ,
    drop = FALSE
  ]
  diagnostic_groups <- parent_plan$compatibility_diagnostic_groups[
    parent_plan$compatibility_diagnostic_groups$diagnostic_id %in%
      diagnostics$diagnostic_id,
    ,
    drop = FALSE
  ]
  known_bytes <- as.numeric(
    files$declared_bytes[!is.na(files$bytes_known) & files$bytes_known]
  )
  known_bytes <- known_bytes[!is.na(known_bytes)]
  if (any(!is.finite(known_bytes)) || any(known_bytes < 0)) {
    abort_llw(
      "The retained plan contains invalid listed file sizes.",
      type = "validation"
    )
  }
  known_total <- sum(known_bytes)
  bytes_complete <- nrow(files) > 0L &&
    all(!is.na(files$bytes_known)) &&
    all(files$bytes_known)
  selected_metadata <- new_glc_selected_metadata(parent_plan, groups)
  if (is.null(source_metadata_snapshot)) {
    source_metadata_snapshot <- new_selected_glc_source_metadata_snapshot(
      source = source,
      metadata = selected_metadata,
      selected_groups = groups,
      variables = available
    )
  } else {
    source_metadata_snapshot <- new_scoped_glc_source_metadata_snapshot(
      snapshot = source_metadata_snapshot,
      selected_metadata = selected_metadata,
      selected_groups = groups,
      variables = available
    )
  }
  if (
    !identical(
      source_metadata_snapshot$source$repository,
      source$repository
    ) ||
      !identical(
        source_metadata_snapshot$source$resolved_revision,
        source$resolved_revision
      )
  ) {
    abort_llw(
      "The output projection metadata snapshot identifies another source.",
      type = "validation"
    )
  }

  review <- structure(
    list(
      review_schema = "llw-glc-variable-projection-review",
      review_version = "1.2.0",
      source = list(
        registry_url = source$registry_url,
        registry_generated_at = source$registry_generated_at,
        repository = source$repository,
        resolved_revision = source$resolved_revision,
        validation_state = source$validation_state,
        attestation_verified = source$attestation_verified,
        schema_version = source$schema_version,
        glcdp_version = source$glcdp_version
      ),
      refined_file_group_ids = refined_file_group_ids,
      parent = list(
        plan_schema = parent_plan$plan_schema,
        plan_version = parent_plan$plan_version,
        plan_fingerprint = parent_plan$refinement_input$fingerprint,
        metadata_fingerprint = parent_plan$provenance$metadata_fingerprint,
        request = parent_request,
        compatibility_id = parent_compatibility_id,
        refinement_schema = parent_refinement$refinement_schema,
        refinement_version = parent_refinement$refinement_version,
        refinement_parent_fingerprint = parent_refinement$parent$fingerprint,
        refinement_fingerprint = digest::digest(
          parent_refinement,
          algo = "sha256",
          serialize = TRUE
        ),
        refinement_file_group_ids = refined_file_group_ids
      ),
      selected_source_variables = source_variables,
      metadata_filters = metadata_filters,
      selected_compatibility_id = parent_compatibility_id,
      selected_unit_ids = selected_unit_ids,
      selected_groups = groups,
      metadata = selected_metadata,
      source_metadata_snapshot = source_metadata_snapshot,
      diagnostics = diagnostics,
      diagnostic_groups = diagnostic_groups,
      read_columns = read_columns,
      output_columns = output_columns,
      files = files,
      byte_evidence = list(
        file_count = nrow(files),
        known_file_count = sum(files$bytes_known %in% TRUE),
        unknown_file_count = sum(!files$bytes_known %in% TRUE),
        known_declared_bytes = known_total,
        declared_bytes_complete = bytes_complete,
        declared_total_bytes = if (bytes_complete) known_total else NA_real_
      ),
      assurance = list(
        projection_only = TRUE,
        compatibility_replanned = FALSE,
        compatibility_identity_changed = FALSE,
        metadata_only = TRUE,
        measurement_contents_transferred = FALSE,
        measurement_contents_inspected = FALSE,
        runtime_validation_required = TRUE
      )
    ),
    class = c("llw_glc_projection_review", "list")
  )
  assert_serializable_value(review, "GLC variable projection review")
  validate_glc_projection_review(review)
  if (glc_value_contains_path(review, source$cache_dir)) {
    abort_llw(
      "The variable projection retained session transport state.",
      type = "validation"
    )
  }
  review
}

validate_glc_projection_review <- function(review) {
  if (!inherits(review, "llw_glc_projection_review") || !is.list(review)) {
    abort_llw(
      "The output projection review is invalid.",
      type = "validation"
    )
  }
  required <- c(
    "review_schema",
    "review_version",
    "source",
    "refined_file_group_ids",
    "parent",
    "selected_source_variables",
    "metadata_filters",
    "selected_compatibility_id",
    "selected_unit_ids",
    "selected_groups",
    "metadata",
    "source_metadata_snapshot",
    "diagnostics",
    "diagnostic_groups",
    "read_columns",
    "output_columns",
    "files",
    "byte_evidence",
    "assurance"
  )
  missing <- setdiff(required, names(review))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "The output projection review is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  if (
    !identical(review$review_schema, "llw-glc-variable-projection-review") ||
      !identical(review$review_version, "1.2.0")
  ) {
    abort_llw(
      "The output projection review schema is not supported.",
      type = "validation"
    )
  }
  source_required <- c(
    "registry_url",
    "registry_generated_at",
    "repository",
    "resolved_revision",
    "validation_state",
    "attestation_verified",
    "schema_version",
    "glcdp_version"
  )
  if (
    !is.list(review$source) ||
      !all(source_required %in% names(review$source)) ||
      !is_glc_repository(review$source$repository) ||
      !is.character(review$source$resolved_revision) ||
      length(review$source$resolved_revision) != 1L ||
      is.na(review$source$resolved_revision) ||
      !grepl("^[0-9a-f]{40}$", review$source$resolved_revision)
  ) {
    abort_llw(
      "The output projection does not identify one exact validated source.",
      type = "validation"
    )
  }
  parent_required <- c(
    "plan_schema",
    "plan_version",
    "plan_fingerprint",
    "metadata_fingerprint",
    "request",
    "compatibility_id",
    "refinement_schema",
    "refinement_version",
    "refinement_parent_fingerprint",
    "refinement_fingerprint",
    "refinement_file_group_ids"
  )
  if (
    !is.list(review$parent) ||
      !all(parent_required %in% names(review$parent)) ||
      !identical(review$parent$plan_schema, "glc-collection-plan") ||
      !identical(review$parent$plan_version, "1.2.0") ||
      !identical(
        review$parent$refinement_schema,
        "glc-collection-refinement"
      ) ||
      !identical(review$parent$refinement_version, "1.1.0") ||
      !all(vapply(
        review$parent[c(
          "plan_fingerprint",
          "metadata_fingerprint",
          "refinement_parent_fingerprint",
          "refinement_fingerprint"
        )],
        function(value) {
          is.character(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            grepl("^[0-9a-f]{64}$", value)
        },
        logical(1)
      )) ||
      !identical(
        review$parent$plan_fingerprint,
        review$parent$refinement_parent_fingerprint
      )
  ) {
    abort_llw(
      "The output projection parent provenance is invalid.",
      type = "validation"
    )
  }
  group_ids <- glc_nonempty_unique(review$refined_file_group_ids)
  if (
    length(group_ids) == 0L ||
      !identical(
        group_ids,
        glc_nonempty_unique(review$parent$refinement_file_group_ids)
      )
  ) {
    abort_llw(
      "The output projection file-group membership is invalid.",
      type = "validation"
    )
  }
  validate_glc_selected_metadata(review$metadata)
  source_metadata_snapshot <- validate_glc_source_metadata_snapshot(
    review$source_metadata_snapshot
  )
  if (
    !identical(
      source_metadata_snapshot$source$repository,
      review$source$repository
    ) ||
      !identical(
        source_metadata_snapshot$source$resolved_revision,
        review$source$resolved_revision
      ) ||
      !identical(
        review$metadata$upstream_fingerprint,
        review$parent$metadata_fingerprint
      ) ||
      !identical(
        glc_metadata_ids(review$metadata$selected_ids$file_group_ids),
        group_ids
      ) ||
      !identical(
        glc_metadata_ids(review$metadata$selected_ids$dataset_ids),
        glc_metadata_ids(review$selected_groups$dataset_id)
      )
  ) {
    abort_llw(
      "The output projection metadata does not match its parent selection.",
      type = "validation"
    )
  }
  variables <- as.character(review$selected_source_variables)
  if (
    length(variables) == 0L ||
      anyNA(variables) ||
      any(!nzchar(variables)) ||
      anyDuplicated(variables) ||
      !identical(variables, sort(variables, method = "radix"))
  ) {
    abort_llw(
      "The output projection must retain unique exact source names.",
      type = "validation"
    )
  }
  if (
    !is.character(review$selected_compatibility_id) ||
      length(review$selected_compatibility_id) != 1L ||
      is.na(review$selected_compatibility_id) ||
      !grepl("^glcc_[0-9a-f]{64}$", review$selected_compatibility_id) ||
      !identical(
        review$selected_compatibility_id,
        review$parent$compatibility_id
      )
  ) {
    abort_llw(
      "The output projection compatibility identity is invalid.",
      type = "validation"
    )
  }
  unit_ids <- glc_nonempty_unique(review$selected_unit_ids)
  if (
    length(unit_ids) == 0L ||
      any(!grepl("^glcu_[0-9a-f]{64}$", unit_ids))
  ) {
    abort_llw(
      "The output projection unit identity is invalid.",
      type = "validation"
    )
  }
  table_contract <- list(
    selected_groups = c(
      "unit_id",
      "compatibility_id",
      "dataset_id",
      "file_group_id"
    ),
    read_columns = c(
      "unit_id",
      "dataset_id",
      "file_group_id",
      "name",
      "selected_for_output",
      "automatic"
    ),
    output_columns = c("unit_id", "name", "automatic"),
    files = c(
      "unit_id",
      "dataset_id",
      "file_group_id",
      "position",
      "declared_path",
      "declared_bytes",
      "bytes_known"
    )
  )
  for (table_name in names(table_contract)) {
    table <- review[[table_name]]
    missing_columns <- setdiff(table_contract[[table_name]], names(table))
    if (!is.data.frame(table) || length(missing_columns) > 0L) {
      abort_llw(
        paste0(
          "The output projection ",
          table_name,
          " table is incomplete."
        ),
        type = "validation"
      )
    }
  }
  if (
    !setequal(
      glc_nonempty_unique(review$selected_groups$file_group_id),
      group_ids
    ) ||
      any(
        review$selected_groups$compatibility_id !=
          review$selected_compatibility_id
      ) ||
      any(!review$selected_groups$unit_id %in% unit_ids) ||
      !setequal(glc_nonempty_unique(review$files$file_group_id), group_ids) ||
      any(!review$files$unit_id %in% unit_ids) ||
      !setequal(
        glc_nonempty_unique(review$read_columns$file_group_id),
        group_ids
      ) ||
      any(!review$read_columns$unit_id %in% unit_ids) ||
      any(!review$output_columns$unit_id %in% unit_ids)
  ) {
    abort_llw(
      "The projection tables do not match the fixed Slice 2A membership.",
      type = "validation"
    )
  }
  read_names <- glc_nonempty_unique(review$read_columns$name)
  output_names <- glc_nonempty_unique(review$output_columns$name)
  explicitly_selected_read <-
    review$read_columns$selected_for_output %in%
    TRUE |
    !review$read_columns$automatic %in% TRUE
  explicitly_selected_output <- !review$output_columns$automatic %in% TRUE
  each_unit_has_contract <- vapply(
    unit_ids,
    function(unit_id) {
      all(
        variables %in%
          review$read_columns$name[
            review$read_columns$unit_id == unit_id
          ]
      ) &&
        all(
          variables %in%
            review$output_columns$name[
              review$output_columns$unit_id == unit_id
            ]
        )
    },
    logical(1)
  )
  if (
    !all(variables %in% read_names) ||
      !all(variables %in% output_names) ||
      any(!each_unit_has_contract) ||
      any(!review$read_columns$name[explicitly_selected_read] %in% variables) ||
      any(
        !review$output_columns$name[explicitly_selected_output] %in% variables
      )
  ) {
    abort_llw(
      "The projection column contract does not match the selected variables.",
      type = "validation"
    )
  }
  if (
    !is.list(review$assurance) ||
      !isTRUE(review$assurance$projection_only) ||
      !identical(review$assurance$compatibility_replanned, FALSE) ||
      !identical(review$assurance$compatibility_identity_changed, FALSE) ||
      !isTRUE(review$assurance$metadata_only) ||
      !identical(review$assurance$measurement_contents_transferred, FALSE) ||
      !identical(review$assurance$measurement_contents_inspected, FALSE) ||
      !isTRUE(review$assurance$runtime_validation_required)
  ) {
    abort_llw(
      "The output projection assurance is invalid.",
      type = "validation"
    )
  }
  bytes <- as.numeric(
    review$files$declared_bytes[review$files$bytes_known %in% TRUE]
  )
  if (anyNA(bytes) || any(!is.finite(bytes)) || any(bytes < 0)) {
    abort_llw(
      "The output projection contains invalid known byte evidence.",
      type = "validation"
    )
  }
  expected_bytes <- list(
    file_count = nrow(review$files),
    known_file_count = sum(review$files$bytes_known %in% TRUE),
    unknown_file_count = sum(!review$files$bytes_known %in% TRUE),
    known_declared_bytes = sum(bytes),
    declared_bytes_complete = nrow(review$files) > 0L &&
      all(review$files$bytes_known %in% TRUE)
  )
  if (
    !is.list(review$byte_evidence) ||
      !all(names(expected_bytes) %in% names(review$byte_evidence)) ||
      !identical(
        as.integer(review$byte_evidence$file_count),
        as.integer(expected_bytes$file_count)
      ) ||
      !identical(
        as.integer(review$byte_evidence$known_file_count),
        as.integer(expected_bytes$known_file_count)
      ) ||
      !identical(
        as.integer(review$byte_evidence$unknown_file_count),
        as.integer(expected_bytes$unknown_file_count)
      ) ||
      !isTRUE(all.equal(
        as.numeric(review$byte_evidence$known_declared_bytes),
        as.numeric(expected_bytes$known_declared_bytes)
      )) ||
      !identical(
        review$byte_evidence$declared_bytes_complete,
        expected_bytes$declared_bytes_complete
      )
  ) {
    abort_llw(
      "The output projection byte evidence is inconsistent.",
      type = "validation"
    )
  }
  assert_serializable_value(review, "GLC output projection review")
  invisible(review)
}

glc_transfer_value_fingerprint <- function(value) {
  digest::digest(value, algo = "sha256", serialize = TRUE)
}

glc_transfer_canonical_files <- function(files) {
  required <- c(
    "unit_id",
    "dataset_id",
    "file_group_id",
    "position",
    "declared_path",
    "declared_bytes",
    "bytes_known"
  )
  if (!is.data.frame(files) || !all(required %in% names(files))) {
    abort_llw("The preview file contract is incomplete.", type = "validation")
  }
  files <- as.data.frame(files, stringsAsFactors = FALSE)
  paths <- as.character(files$declared_path)
  if (
    nrow(files) == 0L ||
      anyNA(paths) ||
      any(!nzchar(paths)) ||
      anyDuplicated(paths)
  ) {
    abort_llw(
      paste(
        "A bounded preview requires unique declared file paths for the",
        "selected unit."
      ),
      type = "validation"
    )
  }
  order_index <- order(
    enc2utf8(as.character(files$dataset_id)),
    enc2utf8(as.character(files$file_group_id)),
    as.integer(files$position),
    enc2utf8(paths),
    method = "radix",
    na.last = TRUE
  )
  files[order_index, , drop = FALSE]
}

new_glc_transfer_review <- function(
  projection_review,
  total_preview_rows = 20L,
  problems = c("error", "warn"),
  available_session_bytes = Inf
) {
  validate_glc_projection_review(projection_review)
  problems <- match.arg(problems)
  total_preview_rows <- validate_glc_transfer_preview_row_budget(
    total_preview_rows
  )
  if (
    !is.numeric(available_session_bytes) ||
      length(available_session_bytes) != 1L ||
      is.na(available_session_bytes) ||
      available_session_bytes < 0
  ) {
    abort_llw(
      "The available session resource budget is invalid.",
      type = "validation"
    )
  }
  if (length(projection_review$selected_unit_ids) != 1L) {
    abort_llw(
      "Choose one finalized Slice 2A unit before reviewing a transfer.",
      type = "validation"
    )
  }
  files <- glc_transfer_canonical_files(projection_review$files)
  allocation <- new_glc_preview_allocation(
    total_budget = as.integer(total_preview_rows),
    file_ids = as.character(files$declared_path)
  )
  allocated <- files[
    match(allocation$preview_files, files$declared_path),
    ,
    drop = FALSE
  ]
  known <- allocated$bytes_known %in% TRUE
  known_bytes <- as.numeric(allocated$declared_bytes[known])
  selected_known <- files$bytes_known %in% TRUE
  selected_known_bytes <- as.numeric(files$declared_bytes[selected_known])
  if (
    anyNA(known_bytes) ||
      any(!is.finite(known_bytes)) ||
      any(known_bytes < 0) ||
      anyNA(selected_known_bytes) ||
      any(!is.finite(selected_known_bytes)) ||
      any(selected_known_bytes < 0)
  ) {
    abort_llw(
      "The selected transfer contains invalid known byte evidence.",
      type = "validation"
    )
  }
  known_total <- sum(known_bytes)
  selected_known_total <- sum(selected_known_bytes)
  resource_blocked <- is.finite(available_session_bytes) &&
    known_total > available_session_bytes
  full_import_blocked <- is.finite(available_session_bytes) &&
    selected_known_total > available_session_bytes
  review <- structure(
    list(
      review_schema = "llw-glc-transfer-review",
      review_version = "1.2.0",
      projection = projection_review,
      projection_fingerprint = glc_transfer_value_fingerprint(
        projection_review
      ),
      source = projection_review$source,
      fixed = list(
        compatibility_id = projection_review$selected_compatibility_id,
        unit_id = projection_review$selected_unit_ids[[1L]],
        file_group_ids = projection_review$refined_file_group_ids,
        plan_fingerprint = projection_review$parent$plan_fingerprint,
        metadata_fingerprint = projection_review$parent$metadata_fingerprint,
        refinement_fingerprint = projection_review$parent$refinement_fingerprint
      ),
      selected_source_variables = projection_review$selected_source_variables,
      automatic_read_inputs = projection_review$read_columns[
        projection_review$read_columns$automatic %in% TRUE,
        ,
        drop = FALSE
      ],
      automatic_result_columns = projection_review$output_columns[
        projection_review$output_columns$automatic %in% TRUE,
        ,
        drop = FALSE
      ],
      selected_groups = projection_review$selected_groups,
      files = files,
      allocation = allocation,
      allocated_files = allocated,
      problems_policy = problems,
      limits = list(
        total_preview_rows = glc_transfer_preview_row_limit()
      ),
      byte_evidence = list(
        selected_file_count = nrow(files),
        selected_known_file_count = sum(files$bytes_known %in% TRUE),
        selected_unknown_file_count = sum(!files$bytes_known %in% TRUE),
        selected_known_declared_bytes = projection_review$byte_evidence$known_declared_bytes,
        allocated_file_count = nrow(allocated),
        allocated_known_file_count = sum(known),
        allocated_unknown_file_count = sum(!known),
        allocated_known_declared_bytes = known_total,
        allocated_declared_bytes_complete = nrow(allocated) > 0L && all(known)
      ),
      resource = list(
        available_session_bytes = available_session_bytes,
        known_allocated_bytes = known_total,
        blocked = resource_blocked,
        size_evidence_incomplete = any(!known),
        known_selected_bytes = selected_known_total,
        full_import_blocked = full_import_blocked,
        full_import_size_evidence_incomplete = any(!selected_known)
      ),
      assurance = list(
        explicit_action_required = TRUE,
        default_problems_policy = "error",
        package_reopened = FALSE,
        network_accessed = FALSE,
        measurement_contents_transferred = FALSE,
        compatibility_replanned = FALSE,
        fixed_membership = TRUE,
        bounded_preview_available = TRUE,
        full_import_available = TRUE,
        result_ephemeral = TRUE
      )
    ),
    class = c("llw_glc_transfer_review", "list")
  )
  review$fingerprint <- glc_transfer_value_fingerprint(review)
  validate_glc_transfer_review(review)
  review
}

glc_transfer_preview_row_limit <- function() {
  10000L
}

validate_glc_transfer_preview_row_budget <- function(value) {
  limit <- glc_transfer_preview_row_limit()
  invalid <- !is.numeric(value) ||
    length(value) != 1L ||
    is.na(value) ||
    !is.finite(value) ||
    value < 1 ||
    value != floor(value)
  if (invalid) {
    abort_llw(
      "The preview row budget must be one finite positive whole number.",
      type = "validation",
      public_message = paste(
        "Enter a whole-number preview-row budget from 1 through",
        "10,000."
      )
    )
  }
  if (value > limit) {
    abort_llw(
      paste0(
        "The preview row budget exceeds the product limit of ",
        format(limit, big.mark = ","),
        " rows."
      ),
      type = "validation",
      public_message = paste(
        "Enter a whole-number preview-row budget from 1 through",
        "10,000."
      )
    )
  }
  as.integer(value)
}

validate_glc_transfer_review <- function(review) {
  if (!inherits(review, "llw_glc_transfer_review") || !is.list(review)) {
    abort_llw("The bounded transfer review is invalid.", type = "validation")
  }
  required <- c(
    "review_schema",
    "review_version",
    "projection",
    "projection_fingerprint",
    "source",
    "fixed",
    "selected_source_variables",
    "automatic_read_inputs",
    "automatic_result_columns",
    "selected_groups",
    "files",
    "allocation",
    "allocated_files",
    "problems_policy",
    "limits",
    "byte_evidence",
    "resource",
    "assurance",
    "fingerprint"
  )
  if (!all(required %in% names(review))) {
    abort_llw(
      "The bounded transfer review is incomplete.",
      type = "validation"
    )
  }
  if (
    !identical(review$review_schema, "llw-glc-transfer-review") ||
      !identical(review$review_version, "1.2.0")
  ) {
    abort_llw(
      "The bounded transfer review schema is not supported.",
      type = "validation"
    )
  }
  validate_glc_projection_review(review$projection)
  if (
    !identical(
      review$projection_fingerprint,
      glc_transfer_value_fingerprint(review$projection)
    ) ||
      !identical(review$source, review$projection$source)
  ) {
    abort_llw(
      "The bounded transfer review projection provenance changed.",
      type = "validation"
    )
  }
  expected_fixed <- list(
    compatibility_id = review$projection$selected_compatibility_id,
    unit_id = review$projection$selected_unit_ids[[1L]],
    file_group_ids = review$projection$refined_file_group_ids,
    plan_fingerprint = review$projection$parent$plan_fingerprint,
    metadata_fingerprint = review$projection$parent$metadata_fingerprint,
    refinement_fingerprint = review$projection$parent$refinement_fingerprint
  )
  if (
    !identical(review$fixed, expected_fixed) ||
      !identical(
        review$selected_source_variables,
        review$projection$selected_source_variables
      ) ||
      !review$problems_policy %in% c("error", "warn")
  ) {
    abort_llw(
      "The bounded transfer review changed the accepted parent contract.",
      type = "validation"
    )
  }
  files <- glc_transfer_canonical_files(review$projection$files)
  if (!identical(review$files, files)) {
    abort_llw(
      "The bounded transfer file order is not canonical.",
      type = "validation"
    )
  }
  row_budget <- validate_glc_transfer_preview_row_budget(
    review$allocation$total_budget
  )
  if (
    !is.list(review$limits) ||
      !identical(
        as.integer(review$limits$total_preview_rows),
        glc_transfer_preview_row_limit()
      )
  ) {
    abort_llw(
      "The bounded transfer row limit is inconsistent.",
      type = "validation"
    )
  }
  allocation <- review$allocation
  expected_allocation <- new_glc_preview_allocation(
    total_budget = row_budget,
    file_ids = as.character(files$declared_path)
  )
  expected_allocated_files <- files[
    match(expected_allocation$preview_files, files$declared_path),
    ,
    drop = FALSE
  ]
  if (
    !inherits(allocation, "llw_glc_preview_allocation") ||
      !identical(allocation, expected_allocation) ||
      !identical(review$allocated_files, expected_allocated_files) ||
      !identical(
        allocation$preview_files,
        as.character(review$allocated_files$declared_path)
      ) ||
      !identical(
        allocation$available_file_count,
        nrow(review$files)
      ) ||
      !identical(
        allocation$preview_file_count,
        nrow(review$allocated_files)
      )
  ) {
    abort_llw(
      "The bounded transfer allocation is inconsistent.",
      type = "validation"
    )
  }
  known <- review$allocated_files$bytes_known %in% TRUE
  known_bytes <- as.numeric(review$allocated_files$declared_bytes[known])
  known_total <- sum(known_bytes)
  expected_blocked <- is.finite(review$resource$available_session_bytes) &&
    known_total > review$resource$available_session_bytes
  selected_known <- review$files$bytes_known %in% TRUE
  selected_known_bytes <- as.numeric(
    review$files$declared_bytes[selected_known]
  )
  selected_known_total <- sum(selected_known_bytes)
  expected_full_import_blocked <-
    is.finite(review$resource$available_session_bytes) &&
    selected_known_total > review$resource$available_session_bytes
  if (
    anyNA(known_bytes) ||
      any(!is.finite(known_bytes)) ||
      any(known_bytes < 0) ||
      anyNA(selected_known_bytes) ||
      any(!is.finite(selected_known_bytes)) ||
      any(selected_known_bytes < 0) ||
      !isTRUE(all.equal(
        as.numeric(review$resource$known_allocated_bytes),
        known_total
      )) ||
      !identical(review$resource$blocked, expected_blocked) ||
      !identical(
        review$resource$size_evidence_incomplete,
        any(!known)
      ) ||
      !isTRUE(all.equal(
        as.numeric(review$resource$known_selected_bytes),
        selected_known_total
      )) ||
      !identical(
        review$resource$full_import_blocked,
        expected_full_import_blocked
      ) ||
      !identical(
        review$resource$full_import_size_evidence_incomplete,
        any(!selected_known)
      )
  ) {
    abort_llw(
      "The bounded transfer resource evidence is inconsistent.",
      type = "validation"
    )
  }
  if (
    !is.list(review$assurance) ||
      !isTRUE(review$assurance$explicit_action_required) ||
      !identical(review$assurance$default_problems_policy, "error") ||
      !identical(review$assurance$package_reopened, FALSE) ||
      !identical(review$assurance$network_accessed, FALSE) ||
      !identical(review$assurance$measurement_contents_transferred, FALSE) ||
      !identical(review$assurance$compatibility_replanned, FALSE) ||
      !isTRUE(review$assurance$fixed_membership) ||
      !isTRUE(review$assurance$bounded_preview_available) ||
      !isTRUE(review$assurance$full_import_available) ||
      !isTRUE(review$assurance$result_ephemeral)
  ) {
    abort_llw(
      "The bounded transfer review assurance is invalid.",
      type = "validation"
    )
  }
  fingerprint_value <- review
  fingerprint_value$fingerprint <- NULL
  if (
    !is.character(review$fingerprint) ||
      length(review$fingerprint) != 1L ||
      is.na(review$fingerprint) ||
      !identical(
        review$fingerprint,
        glc_transfer_value_fingerprint(fingerprint_value)
      )
  ) {
    abort_llw(
      "The bounded transfer review fingerprint is invalid.",
      type = "validation"
    )
  }
  assert_serializable_value(review, "GLC bounded transfer review")
  invisible(review)
}

new_glc_transfer_task_payload <- function(
  source,
  transfer_review,
  progress_path = NULL
) {
  source <- validate_glc_source_spec(source)
  validate_glc_transfer_review(transfer_review)
  if (!is.null(progress_path)) {
    assert_scalar_string(progress_path, "progress_path")
  }
  expected_source <- transfer_review$source
  if (
    !identical(source$registry_url, expected_source$registry_url) ||
      !identical(source$repository, expected_source$repository) ||
      !identical(source$resolved_revision, expected_source$resolved_revision) ||
      !identical(source$schema_version, expected_source$schema_version) ||
      !identical(source$glcdp_version, expected_source$glcdp_version)
  ) {
    abort_llw(
      "The preview source no longer matches the accepted exact revision.",
      type = "validation"
    )
  }
  payload <- list(
    source = source,
    transfer_review = transfer_review,
    progress_path = progress_path
  )
  assert_serializable_value(payload, "GLC bounded preview task payload")
  payload
}

glc_transfer_resolve_declared_path <- function(path, declared_paths) {
  normalized <- gsub("\\\\", "/", path)
  exact <- declared_paths == path
  suffix <- endsWith(normalized, paste0("/", declared_paths))
  candidates <- which(exact | suffix)
  if (length(candidates) == 0L) {
    basename_matches <- which(basename(declared_paths) == basename(path))
    if (length(basename_matches) == 1L) {
      candidates <- basename_matches
    }
  }
  if (length(candidates) != 1L) {
    abort_llw(
      "A returned preview file could not be mapped to one declared path.",
      type = "validation"
    )
  }
  declared_paths[[candidates[[1L]]]]
}

glc_transfer_resolved_file_membership <- function(
  collection,
  allocated_files
) {
  required <- c("dataset_id", "file_group_id", "files")
  if (
    !is.data.frame(collection) ||
      !all(required %in% names(collection)) ||
      !is.data.frame(allocated_files) ||
      !all(
        c("dataset_id", "file_group_id", "declared_path") %in%
          names(allocated_files)
      )
  ) {
    abort_llw(
      "glcdp did not return public file evidence for the bounded preview.",
      type = "validation"
    )
  }
  dataset_ids <- as.character(collection$dataset_id)
  file_group_ids <- as.character(collection$file_group_id)
  declared_paths <- as.character(allocated_files$declared_path)
  if (
    nrow(collection) == 0L ||
      anyNA(dataset_ids) ||
      any(!nzchar(dataset_ids)) ||
      anyNA(file_group_ids) ||
      any(!nzchar(file_group_ids))
  ) {
    abort_llw(
      "glcdp returned incomplete membership for the bounded preview.",
      type = "validation"
    )
  }
  rows <- lapply(seq_len(nrow(collection)), function(index) {
    paths <- as.character(collection$files[[index]])
    if (length(paths) == 0L || anyNA(paths) || any(!nzchar(paths))) {
      abort_llw(
        "glcdp returned incomplete file evidence for the bounded preview.",
        type = "validation"
      )
    }
    data.frame(
      declared_path = vapply(
        paths,
        glc_transfer_resolve_declared_path,
        character(1),
        declared_paths = declared_paths
      ),
      dataset_id = rep(dataset_ids[[index]], length(paths)),
      file_group_id = rep(file_group_ids[[index]], length(paths)),
      stringsAsFactors = FALSE
    )
  })
  actual <- do.call(rbind, rows)
  rownames(actual) <- NULL
  if (anyDuplicated(actual$declared_path)) {
    abort_llw(
      "glcdp returned duplicate file evidence for the bounded preview.",
      type = "validation"
    )
  }
  expected <- allocated_files[,
    c(
      "declared_path",
      "dataset_id",
      "file_group_id"
    ),
    drop = FALSE
  ]
  expected <- as.data.frame(expected, stringsAsFactors = FALSE)
  expected[] <- lapply(expected, as.character)
  rownames(expected) <- NULL
  actual <- actual[match(declared_paths, actual$declared_path), , drop = FALSE]
  rownames(actual) <- NULL
  if (anyNA(actual$declared_path) || !identical(actual, expected)) {
    abort_llw(
      paste(
        "The returned preview file-to-dataset and file-group membership",
        "differs from the canonical allocation."
      ),
      type = "validation"
    )
  }
  actual
}

glc_transfer_resolved_file_ids <- function(collection, declared_paths) {
  if (!is.data.frame(collection) || !"files" %in% names(collection)) {
    abort_llw(
      "glcdp did not return public file evidence for the bounded preview.",
      type = "validation"
    )
  }
  resolved <- unlist(collection$files, use.names = FALSE)
  resolved <- as.character(resolved)
  if (length(resolved) == 0L || anyNA(resolved) || any(!nzchar(resolved))) {
    abort_llw(
      "glcdp returned incomplete file evidence for the bounded preview.",
      type = "validation"
    )
  }
  unique(vapply(
    resolved,
    glc_transfer_resolve_declared_path,
    character(1),
    declared_paths = declared_paths
  ))
}

glc_transfer_actual_input_membership <- function(
  collection,
  allocated_files,
  execution_mode = c("runtime", "fixture")
) {
  execution_mode <- match.arg(execution_mode)
  reviewed <- glc_transfer_resolved_file_membership(
    collection,
    allocated_files
  )
  if (
    identical(execution_mode, "fixture") &&
      !"runtime_input_ids" %in% names(collection)
  ) {
    abort_llw(
      paste(
        "The deterministic preview fixture did not report its actual",
        "runtime input identifiers."
      ),
      type = "validation"
    )
  }
  rows <- lapply(seq_len(nrow(collection)), function(index) {
    paths <- as.character(collection$files[[index]])
    declared_paths <- vapply(
      paths,
      glc_transfer_resolve_declared_path,
      character(1),
      declared_paths = as.character(allocated_files$declared_path)
    )
    input_ids <- if (identical(execution_mode, "fixture")) {
      as.character(collection$runtime_input_ids[[index]])
    } else {
      declared_paths
    }
    if (
      length(input_ids) != length(declared_paths) ||
        anyNA(input_ids) ||
        any(!nzchar(input_ids))
    ) {
      abort_llw(
        "The bounded preview reported incomplete runtime input evidence.",
        type = "validation"
      )
    }
    data.frame(
      declared_path = declared_paths,
      input_id = input_ids,
      dataset_id = rep(
        as.character(collection$dataset_id[[index]]),
        length(input_ids)
      ),
      file_group_id = rep(
        as.character(collection$file_group_id[[index]]),
        length(input_ids)
      ),
      stringsAsFactors = FALSE
    )
  })
  evidence <- do.call(rbind, rows)
  evidence <- evidence[
    match(as.character(allocated_files$declared_path), evidence$declared_path),
    ,
    drop = FALSE
  ]
  rownames(evidence) <- NULL
  if (
    anyNA(evidence$declared_path) ||
      anyDuplicated(evidence$declared_path) ||
      anyDuplicated(evidence$input_id) ||
      !identical(
        evidence[, c("declared_path", "dataset_id", "file_group_id")],
        reviewed
      )
  ) {
    abort_llw(
      "The bounded preview runtime inputs do not match the reviewed allocation.",
      type = "validation"
    )
  }
  if (
    identical(execution_mode, "fixture") &&
      any(evidence$input_id %in% as.character(allocated_files$declared_path))
  ) {
    abort_llw(
      paste(
        "Fixture runtime identifiers must remain distinct from reviewed",
        "live-package paths."
      ),
      type = "validation"
    )
  }
  evidence[, c("input_id", "dataset_id", "file_group_id"), drop = FALSE]
}

new_glc_transfer_preview_result <- function(
  transfer_review,
  collection,
  collected,
  warnings = character(),
  execution_mode = c("runtime", "fixture")
) {
  execution_mode <- match.arg(execution_mode)
  validate_glc_transfer_review(transfer_review)
  if (!is.data.frame(collection)) {
    abort_llw(
      "glcdp did not return a bounded preview collection.",
      type = "validation"
    )
  }
  required <- c("dataset_id", "file_group_id", "files", "data")
  if (!all(required %in% names(collection))) {
    abort_llw(
      "The bounded preview collection is missing public provenance.",
      type = "validation"
    )
  }
  if (!is.data.frame(collected)) {
    abort_llw(
      "glcdp did not return a collected preview table.",
      type = "validation"
    )
  }
  reviewed_file_membership <- glc_transfer_resolved_file_membership(
    collection,
    transfer_review$allocated_files
  )
  reviewed_file_ids <- reviewed_file_membership$declared_path
  if (!setequal(reviewed_file_ids, transfer_review$allocation$preview_files)) {
    abort_llw(
      "The returned preview files do not match the canonical allocation.",
      type = "validation"
    )
  }
  actual_input_membership <- glc_transfer_actual_input_membership(
    collection,
    transfer_review$allocated_files,
    execution_mode = execution_mode
  )
  actual_input_ids <- as.character(actual_input_membership$input_id)
  actual_groups <- glc_nonempty_unique(actual_input_membership$file_group_id)
  allowed_groups <- glc_nonempty_unique(
    transfer_review$allocated_files$file_group_id
  )
  if (
    length(actual_groups) == 0L ||
      any(!actual_groups %in% allowed_groups) ||
      any(
        !glc_nonempty_unique(collection$dataset_id) %in%
          glc_nonempty_unique(transfer_review$allocated_files$dataset_id)
      )
  ) {
    abort_llw(
      "The returned preview membership differs from the canonical allocation.",
      type = "validation"
    )
  }
  maximum_rows <- transfer_review$allocation$maximum_rows
  if (is.finite(maximum_rows) && nrow(collected) > maximum_rows) {
    abort_llw(
      "The returned preview exceeds its reviewed row bound.",
      type = "validation"
    )
  }
  expected_output_names <- glc_nonempty_unique(
    transfer_review$projection$output_columns$name
  )
  missing_output_names <- setdiff(expected_output_names, names(collected))
  if (length(missing_output_names) > 0L) {
    abort_llw(
      paste0(
        "The bounded preview is missing expected result column(s): ",
        paste(missing_output_names, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  warnings <- unique(as.character(warnings))
  warnings <- warnings[!is.na(warnings) & nzchar(warnings)]
  column_types <- data.frame(
    name = names(collected),
    class = vapply(
      collected,
      function(column) paste(class(column), collapse = "/"),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  result <- structure(
    list(
      result_schema = "llw-glc-bounded-preview-result",
      result_version = "1.1.0",
      transfer_review_fingerprint = transfer_review$fingerprint,
      source = transfer_review$source,
      fixed = transfer_review$fixed,
      selected_source_variables = transfer_review$selected_source_variables,
      allocation = transfer_review$allocation,
      problems_policy = transfer_review$problems_policy,
      reviewed = list(
        file_ids = reviewed_file_ids,
        file_count = length(reviewed_file_ids),
        file_membership = reviewed_file_membership,
        selected_package_paths_opened = identical(execution_mode, "runtime")
      ),
      actual = list(
        input_ids = actual_input_ids,
        input_count = length(actual_input_ids),
        input_membership = actual_input_membership,
        file_group_ids = actual_groups,
        file_group_count = length(actual_groups),
        dataset_ids = glc_nonempty_unique(collection$dataset_id),
        dataset_count = length(glc_nonempty_unique(collection$dataset_id)),
        row_count = nrow(collected),
        column_count = ncol(collected),
        column_types = column_types
      ),
      warnings = warnings,
      problems = warnings,
      execution_mode = execution_mode,
      disclosure = list(
        per_file_n_max = transfer_review$allocation$per_file_n_max,
        maximum_possible_rows = maximum_rows,
        omitted_file_count = length(transfer_review$allocation$omitted_files),
        omitted_file_ids = transfer_review$allocation$omitted_files,
        per_file_limit_applied = is.finite(
          transfer_review$allocation$per_file_n_max
        ),
        network_transfer_may_exceed_parsed_rows = TRUE,
        unread_files_runtime_validated = FALSE,
        preview_is_not_dataset = TRUE
      ),
      data = collected,
      assurance = list(
        package_reopened_at_exact_revision = identical(
          execution_mode,
          "runtime"
        ),
        measurement_contents_accessed = identical(execution_mode, "runtime"),
        selected_package_files_opened = identical(execution_mode, "runtime"),
        network_accessed = identical(execution_mode, "runtime"),
        no_live_measurement_access = identical(execution_mode, "fixture"),
        deterministic_fixture = identical(execution_mode, "fixture"),
        public_api_contract_exercised = TRUE,
        compatibility_replanned = FALSE,
        bounded_glc_read = TRUE,
        bounded_glc_collect = TRUE,
        standardize = "lightlogr",
        preview_only = TRUE,
        ephemeral = TRUE,
        imported = FALSE,
        persisted = FALSE,
        promoted = FALSE
      )
    ),
    class = c("llw_glc_transfer_preview_result", "list")
  )
  result$fingerprint <- glc_transfer_value_fingerprint(result)
  validate_glc_transfer_preview_result(result, transfer_review)
  result
}

validate_glc_transfer_preview_result <- function(
  result,
  transfer_review = NULL
) {
  if (
    !inherits(result, "llw_glc_transfer_preview_result") ||
      !is.list(result)
  ) {
    abort_llw("The bounded preview result is invalid.", type = "validation")
  }
  required <- c(
    "result_schema",
    "result_version",
    "transfer_review_fingerprint",
    "source",
    "fixed",
    "selected_source_variables",
    "allocation",
    "problems_policy",
    "reviewed",
    "actual",
    "warnings",
    "problems",
    "execution_mode",
    "disclosure",
    "data",
    "assurance",
    "fingerprint"
  )
  if (!all(required %in% names(result))) {
    abort_llw("The bounded preview result is incomplete.", type = "validation")
  }
  if (
    !identical(result$result_schema, "llw-glc-bounded-preview-result") ||
      !identical(result$result_version, "1.1.0") ||
      !is.data.frame(result$data) ||
      !is.list(result$reviewed) ||
      !is.data.frame(result$reviewed$file_membership) ||
      !all(
        c("declared_path", "dataset_id", "file_group_id") %in%
          names(result$reviewed$file_membership)
      ) ||
      !identical(
        as.character(result$reviewed$file_ids),
        as.character(result$reviewed$file_membership$declared_path)
      ) ||
      !identical(
        as.integer(result$reviewed$file_count),
        nrow(result$reviewed$file_membership)
      ) ||
      !is.list(result$actual) ||
      !is.data.frame(result$actual$input_membership) ||
      !all(
        c("input_id", "dataset_id", "file_group_id") %in%
          names(result$actual$input_membership)
      ) ||
      !identical(
        as.character(result$actual$input_ids),
        as.character(result$actual$input_membership$input_id)
      ) ||
      !identical(
        as.integer(result$actual$input_count),
        nrow(result$actual$input_membership)
      ) ||
      !identical(
        result$actual$input_membership[, c("dataset_id", "file_group_id")],
        result$reviewed$file_membership[, c("dataset_id", "file_group_id")]
      ) ||
      !is.data.frame(result$actual$column_types) ||
      !identical(nrow(result$data), as.integer(result$actual$row_count)) ||
      !identical(ncol(result$data), as.integer(result$actual$column_count)) ||
      !identical(
        names(result$data),
        as.character(result$actual$column_types$name)
      )
  ) {
    abort_llw(
      "The bounded preview result content is inconsistent.",
      type = "validation"
    )
  }
  if (!is.null(transfer_review)) {
    validate_glc_transfer_review(transfer_review)
    if (
      !identical(
        result$transfer_review_fingerprint,
        transfer_review$fingerprint
      ) ||
        !identical(result$source, transfer_review$source) ||
        !identical(result$fixed, transfer_review$fixed) ||
        !identical(
          result$selected_source_variables,
          transfer_review$selected_source_variables
        ) ||
        !identical(result$allocation, transfer_review$allocation) ||
        !identical(result$problems_policy, transfer_review$problems_policy) ||
        !identical(
          result$reviewed$file_membership,
          transfer_review$allocated_files[,
            c(
              "declared_path",
              "dataset_id",
              "file_group_id"
            ),
            drop = FALSE
          ]
        )
    ) {
      abort_llw(
        "The bounded preview result no longer matches its reviewed contract.",
        type = "validation"
      )
    }
  }
  if (
    !result$execution_mode %in% c("runtime", "fixture") ||
      !is.list(result$disclosure) ||
      !identical(
        result$disclosure$per_file_n_max,
        result$allocation$per_file_n_max
      ) ||
      !identical(
        result$disclosure$maximum_possible_rows,
        result$allocation$maximum_rows
      ) ||
      !identical(
        as.integer(result$disclosure$omitted_file_count),
        length(result$allocation$omitted_files)
      ) ||
      !identical(
        result$disclosure$omitted_file_ids,
        result$allocation$omitted_files
      ) ||
      !identical(
        result$disclosure$per_file_limit_applied,
        is.finite(result$allocation$per_file_n_max)
      ) ||
      !isTRUE(result$disclosure$network_transfer_may_exceed_parsed_rows) ||
      !identical(result$disclosure$unread_files_runtime_validated, FALSE) ||
      !isTRUE(result$disclosure$preview_is_not_dataset) ||
      !is.list(result$assurance) ||
      !identical(
        result$assurance$package_reopened_at_exact_revision,
        identical(result$execution_mode, "runtime")
      ) ||
      !identical(
        result$assurance$measurement_contents_accessed,
        identical(result$execution_mode, "runtime")
      ) ||
      !identical(
        result$assurance$selected_package_files_opened,
        identical(result$execution_mode, "runtime")
      ) ||
      !identical(
        result$assurance$network_accessed,
        identical(result$execution_mode, "runtime")
      ) ||
      !identical(
        result$assurance$no_live_measurement_access,
        identical(result$execution_mode, "fixture")
      ) ||
      !identical(
        result$assurance$deterministic_fixture,
        identical(result$execution_mode, "fixture")
      ) ||
      !isTRUE(result$assurance$public_api_contract_exercised) ||
      !identical(result$assurance$compatibility_replanned, FALSE) ||
      !isTRUE(result$assurance$bounded_glc_read) ||
      !isTRUE(result$assurance$bounded_glc_collect) ||
      !identical(result$assurance$standardize, "lightlogr") ||
      !isTRUE(result$assurance$preview_only) ||
      !isTRUE(result$assurance$ephemeral) ||
      !identical(result$assurance$imported, FALSE) ||
      !identical(result$assurance$persisted, FALSE) ||
      !identical(result$assurance$promoted, FALSE)
  ) {
    abort_llw(
      "The bounded preview result assurance is invalid.",
      type = "validation"
    )
  }
  if (
    !identical(
      result$reviewed$selected_package_paths_opened,
      identical(result$execution_mode, "runtime")
    ) ||
      anyDuplicated(as.character(result$reviewed$file_ids)) > 0L ||
      anyDuplicated(as.character(result$actual$input_ids)) > 0L ||
      !identical(
        result$actual$file_group_ids,
        glc_nonempty_unique(result$actual$input_membership$file_group_id)
      ) ||
      !identical(
        as.integer(result$actual$file_group_count),
        length(result$actual$file_group_ids)
      ) ||
      !identical(
        result$actual$dataset_ids,
        glc_nonempty_unique(result$actual$input_membership$dataset_id)
      ) ||
      !identical(
        as.integer(result$actual$dataset_count),
        length(result$actual$dataset_ids)
      ) ||
      (identical(result$execution_mode, "fixture") &&
        any(
          as.character(result$actual$input_ids) %in%
            as.character(result$reviewed$file_ids)
        ))
  ) {
    abort_llw(
      "The bounded preview execution provenance is contradictory.",
      type = "validation"
    )
  }
  maximum_rows <- as.numeric(result$disclosure$maximum_possible_rows)
  if (
    length(maximum_rows) != 1L ||
      is.na(maximum_rows) ||
      maximum_rows < 0 ||
      (is.finite(maximum_rows) && nrow(result$data) > maximum_rows)
  ) {
    abort_llw(
      "The bounded preview result exceeds its reviewed row limit.",
      type = "validation"
    )
  }
  fingerprint_value <- result
  fingerprint_value$fingerprint <- NULL
  if (
    !is.character(result$fingerprint) ||
      length(result$fingerprint) != 1L ||
      is.na(result$fingerprint) ||
      !identical(
        result$fingerprint,
        glc_transfer_value_fingerprint(fingerprint_value)
      )
  ) {
    abort_llw(
      "The bounded preview result fingerprint is invalid.",
      type = "validation"
    )
  }
  assert_serializable_value(result, "GLC bounded preview result")
  invisible(result)
}

glcdp_transfer_preview_execute <- function(
  payload,
  api = NULL,
  execution_mode = c("runtime", "fixture")
) {
  execution_mode <- match.arg(execution_mode)
  if (
    !is.list(payload) ||
      !all(
        c(
          "source",
          "transfer_review",
          "progress_path"
        ) %in%
          names(payload)
      )
  ) {
    abort_llw(
      "The bounded preview task payload is invalid.",
      type = "validation"
    )
  }
  source <- validate_glc_source_spec(payload$source)
  review <- payload$transfer_review
  validate_glc_transfer_review(review)
  validate_glc_transfer_preview_row_budget(review$allocation$total_budget)
  if (
    !is.finite(review$allocation$maximum_rows) ||
      review$allocation$maximum_rows > glc_transfer_preview_row_limit()
  ) {
    abort_llw(
      "The bounded preview allocation exceeds the product row limit.",
      type = "validation",
      public_message = paste(
        "The preview allocation must remain within the 10,000-row limit.",
        "Review the row budget before retrying."
      )
    )
  }
  new_glc_transfer_task_payload(
    source,
    review,
    progress_path = payload$progress_path
  )
  if (isTRUE(review$resource$blocked)) {
    abort_llw(
      paste(
        "Known bytes for the bounded preview exceed the available session",
        "resource budget."
      ),
      type = "resource"
    )
  }
  if (identical(execution_mode, "fixture") && is.null(api)) {
    abort_llw(
      "A deterministic preview fixture requires an explicitly injected API.",
      type = "validation"
    )
  }
  api <- glcdp_api(api)
  progress <- new_glc_phase_log(payload$progress_path)
  warnings <- character()
  tryCatch(
    withCallingHandlers(
      {
        progress$begin(
          "validate",
          "Validating the accepted review",
          paste(
            "Checking exact revision, fingerprints, fixed membership,",
            "allocation, and resource evidence before transfer."
          )
        )
        validate_glc_transfer_review(review)
        progress$complete("The immutable transfer review is valid.")

        progress$begin(
          "open",
          if (identical(execution_mode, "fixture")) {
            "Preparing the deterministic preview fixture"
          } else {
            "Reopening the exact revision"
          },
          if (identical(execution_mode, "fixture")) {
            paste(
              "Using injected local records to exercise the public preview",
              "contract. No package or measurement file is opened."
            )
          } else {
            "Reopening the accepted package revision in the session cache."
          }
        )
        handle <- glcdp_reopen_source(source, api = api)
        progress$complete(
          if (identical(execution_mode, "fixture")) {
            "The deterministic fixture handle is ready."
          } else {
            "The exact accepted revision was reopened."
          }
        )

        progress$begin(
          "read",
          if (identical(execution_mode, "fixture")) {
            "Exercising the bounded read contract"
          } else {
            "Transferring the bounded preview"
          },
          if (identical(execution_mode, "fixture")) {
            paste(
              "Applying the reviewed file and row allocation to injected",
              "local fixture records."
            )
          } else {
            paste(
              "Reading only the reviewed canonical file subset with the",
              "per-file parsed-row limit."
            )
          }
        )
        allocated <- review$allocated_files
        collection <- api$glc_read(
          x = handle,
          dataset_id = unique(as.character(allocated$dataset_id)),
          file_group = unique(as.character(allocated$file_group_id)),
          files = as.character(allocated$declared_path),
          variables = review$selected_source_variables,
          terms = NULL,
          primary_only = FALSE,
          n_max = review$allocation$per_file_n_max,
          problems = review$problems_policy,
          progress = FALSE
        )
        progress$complete(
          if (identical(execution_mode, "fixture")) {
            "The injected fixture subset was parsed."
          } else {
            "The bounded file subset was parsed."
          }
        )

        progress$begin(
          "collect",
          if (identical(execution_mode, "fixture")) {
            "Validating the fixture collection contract"
          } else {
            "Validating the bounded collection"
          },
          if (identical(execution_mode, "fixture")) {
            paste(
              "Applying the public LightLogR standardization contract to",
              "the injected fixture subset."
            )
          } else {
            paste(
              "Collecting only the preview subset with LightLogR",
              "standardization. Unread files are not runtime-validated."
            )
          }
        )
        collected <- api$glc_collect(
          collection,
          standardize = "lightlogr"
        )
        progress$complete("The bounded preview subset was collected.")

        progress$begin(
          "serialize",
          "Preparing the ephemeral result",
          paste(
            "Removing live transport state and returning bounded preview",
            "data with public provenance and limitations."
          )
        )
        result <- new_glc_transfer_preview_result(
          review,
          collection,
          collected,
          warnings = warnings,
          execution_mode = execution_mode
        )
        assert_glc_worker_result_safe(result, source$cache_dir)
        progress$complete("The serializable bounded preview is ready.")
        result
      },
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cnd) {
      progress$fail(
        "This phase did not complete. Review the task error and retry."
      )
      if (inherits(cnd, "llw_error")) {
        stop(cnd)
      }
      stop(map_glcdp_condition(cnd))
    }
  )
}

glcdp_transfer_preview_task_worker <- function(payload, spec) {
  glcdp_transfer_preview_execute(payload)
}

new_glc_full_import_task_payload <- function(
  source,
  transfer_review,
  progress_path = NULL
) {
  source <- validate_glc_source_spec(source)
  validate_glc_transfer_review(transfer_review)
  if (!is.null(progress_path)) {
    assert_scalar_string(progress_path, "progress_path")
  }
  expected_source <- transfer_review$source
  if (
    !identical(source$registry_url, expected_source$registry_url) ||
      !identical(source$repository, expected_source$repository) ||
      !identical(source$resolved_revision, expected_source$resolved_revision) ||
      !identical(source$schema_version, expected_source$schema_version) ||
      !identical(source$glcdp_version, expected_source$glcdp_version)
  ) {
    abort_llw(
      "The full import source no longer matches the accepted exact revision.",
      type = "validation"
    )
  }
  payload <- list(
    operation = "full_import",
    source = source,
    transfer_review = transfer_review,
    progress_path = progress_path
  )
  assert_serializable_value(payload, "GLC full import task payload")
  payload
}

validate_glc_full_import_task_payload <- function(payload) {
  required <- c(
    "operation",
    "source",
    "transfer_review",
    "progress_path"
  )
  if (
    !is.list(payload) ||
      !setequal(names(payload), required) ||
      !identical(payload$operation, "full_import")
  ) {
    abort_llw(
      paste(
        "The full import task accepts only immutable source, review, and",
        "progress inputs. Preview results cannot be supplied."
      ),
      type = "validation"
    )
  }
  new_glc_full_import_task_payload(
    payload$source,
    payload$transfer_review,
    payload$progress_path
  )
  invisible(payload)
}

glc_full_import_factor_evidence <- function(data) {
  if (!is.data.frame(data)) {
    abort_llw("The full import table is invalid.", type = "validation")
  }
  factor_names <- names(data)[vapply(data, is.factor, logical(1))]
  if (length(factor_names) == 0L) {
    return(data.frame(
      name = character(),
      level_count = integer(),
      levels = I(list()),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    name = factor_names,
    level_count = as.integer(vapply(
      data[factor_names],
      function(column) length(levels(column)),
      integer(1)
    )),
    levels = I(lapply(data[factor_names], levels)),
    stringsAsFactors = FALSE
  )
}

new_glc_full_import_result <- function(
  transfer_review,
  collection,
  collected,
  warnings = character(),
  elapsed_seconds = 0,
  execution_mode = c("runtime", "fixture")
) {
  execution_mode <- match.arg(execution_mode)
  validate_glc_transfer_review(transfer_review)
  if (
    !is.data.frame(collection) ||
      !all(
        c("dataset_id", "file_group_id", "files", "data") %in%
          names(collection)
      )
  ) {
    abort_llw(
      "glcdp did not return complete public provenance for the full import.",
      type = "validation"
    )
  }
  if (!is.data.frame(collected)) {
    abort_llw(
      "glcdp did not return a complete collected import table.",
      type = "validation"
    )
  }
  if (
    !is.numeric(elapsed_seconds) ||
      length(elapsed_seconds) != 1L ||
      is.na(elapsed_seconds) ||
      !is.finite(elapsed_seconds) ||
      elapsed_seconds < 0
  ) {
    abort_llw("The full import elapsed time is invalid.", type = "validation")
  }
  reviewed_file_membership <- glc_transfer_resolved_file_membership(
    collection,
    transfer_review$files
  )
  actual_input_membership <- glc_transfer_actual_input_membership(
    collection,
    transfer_review$files,
    execution_mode = execution_mode
  )
  expected_groups <- glc_nonempty_unique(
    transfer_review$files$file_group_id
  )
  actual_groups <- glc_nonempty_unique(
    actual_input_membership$file_group_id
  )
  expected_datasets <- glc_nonempty_unique(transfer_review$files$dataset_id)
  actual_datasets <- glc_nonempty_unique(actual_input_membership$dataset_id)
  if (
    !identical(
      as.character(reviewed_file_membership$declared_path),
      as.character(transfer_review$files$declared_path)
    ) ||
      !setequal(actual_groups, expected_groups) ||
      !setequal(actual_datasets, expected_datasets)
  ) {
    abort_llw(
      "The full import runtime membership differs from the reviewed unit.",
      type = "validation"
    )
  }
  expected_output_names <- glc_nonempty_unique(
    transfer_review$projection$output_columns$name
  )
  missing_output_names <- setdiff(expected_output_names, names(collected))
  if (length(missing_output_names) > 0L) {
    abort_llw(
      paste0(
        "The full import is missing expected result column(s): ",
        paste(missing_output_names, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  warnings <- unique(as.character(warnings))
  warnings <- warnings[!is.na(warnings) & nzchar(warnings)]
  column_types <- data.frame(
    name = names(collected),
    class = vapply(
      collected,
      function(column) paste(class(column), collapse = "/"),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  result <- structure(
    list(
      result_schema = "llw-glc-full-import-handoff",
      result_version = "1.2.0",
      transfer_review_fingerprint = transfer_review$fingerprint,
      projection_fingerprint = transfer_review$projection_fingerprint,
      source = transfer_review$source,
      fixed = transfer_review$fixed,
      selected_source_variables = transfer_review$selected_source_variables,
      read_columns = transfer_review$projection$read_columns,
      output_columns = transfer_review$projection$output_columns,
      metadata = transfer_review$projection$metadata,
      source_metadata_snapshot = transfer_review$projection$source_metadata_snapshot,
      problems_policy = transfer_review$problems_policy,
      reviewed = list(
        file_ids = as.character(reviewed_file_membership$declared_path),
        file_count = nrow(reviewed_file_membership),
        file_membership = reviewed_file_membership,
        selected_package_paths_opened = identical(execution_mode, "runtime")
      ),
      actual = list(
        input_ids = as.character(actual_input_membership$input_id),
        input_count = nrow(actual_input_membership),
        input_membership = actual_input_membership,
        file_group_ids = actual_groups,
        file_group_count = length(actual_groups),
        dataset_ids = actual_datasets,
        dataset_count = length(actual_datasets),
        row_count = nrow(collected),
        column_count = ncol(collected),
        column_types = column_types,
        factor_evidence = glc_full_import_factor_evidence(collected)
      ),
      warnings = warnings,
      problems = warnings,
      execution_mode = execution_mode,
      runtime = list(
        n_max = Inf,
        terms_applied = FALSE,
        primary_only = FALSE,
        progress = FALSE,
        standardize = "lightlogr",
        elapsed_seconds = as.numeric(elapsed_seconds)
      ),
      resource = list(
        available_session_bytes = transfer_review$resource$available_session_bytes,
        known_declared_bytes = transfer_review$resource$known_selected_bytes,
        unknown_file_count = transfer_review$byte_evidence$selected_unknown_file_count,
        known_bytes_within_budget = !isTRUE(
          transfer_review$resource$full_import_blocked
        ),
        size_evidence_incomplete = transfer_review$resource$full_import_size_evidence_incomplete
      ),
      data = collected,
      assurance = list(
        fresh_full_read = TRUE,
        preview_result_used = FALSE,
        full_selected_membership_requested = TRUE,
        package_reopened_at_exact_revision = identical(
          execution_mode,
          "runtime"
        ),
        measurement_contents_accessed = identical(execution_mode, "runtime"),
        selected_package_files_opened = identical(execution_mode, "runtime"),
        network_accessed = identical(execution_mode, "runtime"),
        no_live_measurement_access = identical(execution_mode, "fixture"),
        deterministic_fixture = identical(execution_mode, "fixture"),
        public_api_contract_exercised = TRUE,
        compatibility_replanned = FALSE,
        full_glc_read = TRUE,
        full_glc_collect = TRUE,
        complete_collected_handoff = TRUE,
        ephemeral = TRUE,
        persisted = FALSE,
        promoted = FALSE,
        integrated = FALSE,
        downloaded = FALSE
      )
    ),
    class = c("llw_glc_full_import_result", "list")
  )
  result$fingerprint <- glc_transfer_value_fingerprint(result)
  validate_glc_full_import_result(result, transfer_review)
  result
}

validate_glc_full_import_result <- function(
  result,
  transfer_review = NULL
) {
  if (!inherits(result, "llw_glc_full_import_result") || !is.list(result)) {
    abort_llw("The full import handoff is invalid.", type = "validation")
  }
  required <- c(
    "result_schema",
    "result_version",
    "transfer_review_fingerprint",
    "projection_fingerprint",
    "source",
    "fixed",
    "selected_source_variables",
    "read_columns",
    "output_columns",
    "metadata",
    "source_metadata_snapshot",
    "problems_policy",
    "reviewed",
    "actual",
    "warnings",
    "problems",
    "execution_mode",
    "runtime",
    "resource",
    "data",
    "assurance",
    "fingerprint"
  )
  if (!all(required %in% names(result))) {
    abort_llw("The full import handoff is incomplete.", type = "validation")
  }
  if (
    !identical(result$result_schema, "llw-glc-full-import-handoff") ||
      !identical(result$result_version, "1.2.0") ||
      !result$execution_mode %in% c("runtime", "fixture") ||
      !is.data.frame(result$data) ||
      !is.data.frame(result$reviewed$file_membership) ||
      !is.data.frame(result$actual$input_membership) ||
      !is.data.frame(result$actual$column_types) ||
      !is.data.frame(result$actual$factor_evidence) ||
      !identical(nrow(result$data), as.integer(result$actual$row_count)) ||
      !identical(ncol(result$data), as.integer(result$actual$column_count)) ||
      !identical(
        names(result$data),
        as.character(result$actual$column_types$name)
      ) ||
      !identical(
        result$actual$input_membership[, c("dataset_id", "file_group_id")],
        result$reviewed$file_membership[, c("dataset_id", "file_group_id")]
      )
  ) {
    abort_llw(
      "The full import handoff content is inconsistent.",
      type = "validation"
    )
  }
  validate_glc_selected_metadata(result$metadata)
  validate_glc_source_metadata_snapshot(result$source_metadata_snapshot)
  if (!is.null(transfer_review)) {
    validate_glc_transfer_review(transfer_review)
    expected_membership <- transfer_review$files[,
      c("declared_path", "dataset_id", "file_group_id"),
      drop = FALSE
    ]
    if (
      !identical(
        result$transfer_review_fingerprint,
        transfer_review$fingerprint
      ) ||
        !identical(
          result$projection_fingerprint,
          transfer_review$projection_fingerprint
        ) ||
        !identical(result$source, transfer_review$source) ||
        !identical(result$fixed, transfer_review$fixed) ||
        !identical(
          result$selected_source_variables,
          transfer_review$selected_source_variables
        ) ||
        !identical(
          result$read_columns,
          transfer_review$projection$read_columns
        ) ||
        !identical(
          result$output_columns,
          transfer_review$projection$output_columns
        ) ||
        !identical(result$metadata, transfer_review$projection$metadata) ||
        !identical(
          result$source_metadata_snapshot,
          transfer_review$projection$source_metadata_snapshot
        ) ||
        !identical(result$problems_policy, transfer_review$problems_policy) ||
        !identical(result$reviewed$file_membership, expected_membership)
    ) {
      abort_llw(
        "The full import handoff no longer matches its reviewed contract.",
        type = "validation"
      )
    }
  }
  if (
    !is.list(result$runtime) ||
      !identical(result$runtime$n_max, Inf) ||
      !identical(result$runtime$terms_applied, FALSE) ||
      !identical(result$runtime$primary_only, FALSE) ||
      !identical(result$runtime$progress, FALSE) ||
      !identical(result$runtime$standardize, "lightlogr") ||
      !is.numeric(result$runtime$elapsed_seconds) ||
      length(result$runtime$elapsed_seconds) != 1L ||
      is.na(result$runtime$elapsed_seconds) ||
      !is.finite(result$runtime$elapsed_seconds) ||
      result$runtime$elapsed_seconds < 0 ||
      !is.list(result$resource) ||
      !isTRUE(result$resource$known_bytes_within_budget) ||
      !is.list(result$assurance) ||
      !isTRUE(result$assurance$fresh_full_read) ||
      !identical(result$assurance$preview_result_used, FALSE) ||
      !isTRUE(result$assurance$full_selected_membership_requested) ||
      !isTRUE(result$assurance$public_api_contract_exercised) ||
      !identical(result$assurance$compatibility_replanned, FALSE) ||
      !isTRUE(result$assurance$full_glc_read) ||
      !isTRUE(result$assurance$full_glc_collect) ||
      !isTRUE(result$assurance$complete_collected_handoff) ||
      !isTRUE(result$assurance$ephemeral) ||
      !identical(result$assurance$persisted, FALSE) ||
      !identical(result$assurance$promoted, FALSE) ||
      !identical(result$assurance$integrated, FALSE) ||
      !identical(result$assurance$downloaded, FALSE)
  ) {
    abort_llw(
      "The full import handoff assurance is invalid.",
      type = "validation"
    )
  }
  runtime_mode <- identical(result$execution_mode, "runtime")
  if (
    !identical(result$reviewed$selected_package_paths_opened, runtime_mode) ||
      !identical(
        result$assurance$package_reopened_at_exact_revision,
        runtime_mode
      ) ||
      !identical(
        result$assurance$measurement_contents_accessed,
        runtime_mode
      ) ||
      !identical(
        result$assurance$selected_package_files_opened,
        runtime_mode
      ) ||
      !identical(result$assurance$network_accessed, runtime_mode) ||
      !identical(result$assurance$no_live_measurement_access, !runtime_mode) ||
      !identical(result$assurance$deterministic_fixture, !runtime_mode) ||
      (identical(result$execution_mode, "fixture") &&
        any(
          as.character(result$actual$input_ids) %in%
            as.character(result$reviewed$file_ids)
        ))
  ) {
    abort_llw(
      "The full import execution provenance is contradictory.",
      type = "validation"
    )
  }
  fingerprint_value <- result
  fingerprint_value$fingerprint <- NULL
  if (
    !is.character(result$fingerprint) ||
      length(result$fingerprint) != 1L ||
      is.na(result$fingerprint) ||
      !identical(
        result$fingerprint,
        glc_transfer_value_fingerprint(fingerprint_value)
      )
  ) {
    abort_llw(
      "The full import handoff fingerprint is invalid.",
      type = "validation"
    )
  }
  assert_serializable_value(result, "GLC full import handoff")
  invisible(result)
}

glcdp_full_import_execute <- function(
  payload,
  api = NULL,
  execution_mode = c("runtime", "fixture")
) {
  execution_mode <- match.arg(execution_mode)
  validate_glc_full_import_task_payload(payload)
  source <- validate_glc_source_spec(payload$source)
  review <- payload$transfer_review
  validate_glc_transfer_review(review)
  if (isTRUE(review$resource$full_import_blocked)) {
    abort_llw(
      paste(
        "Known bytes for the full selected file set exceed the available",
        "session resource budget."
      ),
      type = "resource",
      public_message = paste(
        "Apply is blocked because the known selected bytes exceed the",
        "active session resource limit. Narrow the selection before retrying."
      )
    )
  }
  if (identical(execution_mode, "fixture") && is.null(api)) {
    abort_llw(
      "A deterministic full import fixture requires an injected public API.",
      type = "validation"
    )
  }
  api <- glcdp_api(api)
  progress <- new_glc_phase_log(payload$progress_path)
  warnings <- character()
  started <- proc.time()[["elapsed"]]
  tryCatch(
    withCallingHandlers(
      {
        progress$begin(
          "validate",
          "Validating the full import contract",
          paste(
            "Checking exact revision, parent fingerprints, fixed membership,",
            "variables, and full-set resource evidence before Apply."
          )
        )
        validate_glc_transfer_review(review)
        if (isTRUE(review$resource$full_import_blocked)) {
          abort_llw(
            "The full selected file set exceeds the session resource limit.",
            type = "resource"
          )
        }
        progress$complete("The immutable full import contract is valid.")

        progress$begin(
          "open",
          if (identical(execution_mode, "fixture")) {
            "Preparing the deterministic full import fixture"
          } else {
            "Reopening the exact revision"
          },
          if (identical(execution_mode, "fixture")) {
            paste(
              "Using injected local records to exercise a fresh full read.",
              "No selected package or measurement file is opened."
            )
          } else {
            "Reopening the accepted package revision in the session cache."
          }
        )
        handle <- glcdp_reopen_source(source, api = api)
        progress$complete(
          if (identical(execution_mode, "fixture")) {
            "The deterministic full import fixture is ready."
          } else {
            "The exact accepted revision was reopened."
          }
        )

        progress$begin(
          "read",
          if (identical(execution_mode, "fixture")) {
            "Exercising the fresh full read contract"
          } else {
            "Transferring all selected files"
          },
          paste(
            "Reading the complete fixed file set with no parsed-row limit.",
            "Bounded preview rows are not used as input."
          )
        )
        files <- review$files
        collection <- api$glc_read(
          x = handle,
          dataset_id = unique(as.character(files$dataset_id)),
          file_group = unique(as.character(files$file_group_id)),
          files = as.character(files$declared_path),
          variables = review$selected_source_variables,
          terms = NULL,
          primary_only = FALSE,
          n_max = Inf,
          problems = review$problems_policy,
          progress = FALSE
        )
        progress$complete("The complete fixed file set was parsed.")

        progress$begin(
          "collect",
          "Collecting the complete import handoff",
          paste(
            "Applying the public LightLogR standardization contract to the",
            "fresh full collection."
          )
        )
        collected <- api$glc_collect(collection, standardize = "lightlogr")
        progress$complete("The complete selected collection was standardized.")

        progress$begin(
          "serialize",
          "Preparing the ephemeral full handoff",
          paste(
            "Removing live transport state and returning the complete",
            "collected table with exact public provenance."
          )
        )
        elapsed <- max(0, proc.time()[["elapsed"]] - started)
        result <- new_glc_full_import_result(
          review,
          collection,
          collected,
          warnings = warnings,
          elapsed_seconds = elapsed,
          execution_mode = execution_mode
        )
        assert_glc_worker_result_safe(result, source$cache_dir)
        progress$complete("The serializable full import handoff is ready.")
        result
      },
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(cnd) {
      progress$fail(
        "This phase did not complete. Review the task error and retry."
      )
      if (inherits(cnd, "llw_error")) {
        stop(cnd)
      }
      stop(map_glcdp_condition(cnd))
    }
  )
}

glcdp_full_import_task_worker <- function(payload, spec) {
  glcdp_full_import_execute(payload)
}

glc_collection_refinement_required_tables <- function() {
  list(
    units = glc_collection_plan_required_tables()$units,
    groups = c(
      "status",
      "unit_id",
      "compatibility_id",
      "dataset_id",
      "file_group",
      "file_group_id",
      "study_id",
      "participant_id",
      "participant_associated",
      "study_link_status",
      "participant_link_status",
      "device_id",
      "device_link_status",
      "reason_codes",
      "messages"
    ),
    constraints = c("compatibility_id", "code", "message", "resolved")
  )
}

validate_glc_collection_refinement <- function(
  refinement,
  plan,
  compatibility_id,
  file_group_ids
) {
  assert_scalar_string(compatibility_id, "compatibility_id")
  if (!grepl("^glcc_[0-9a-f]{64}$", compatibility_id)) {
    abort_llw(
      "The structural compatibility identifier is invalid.",
      type = "validation"
    )
  }
  if (
    !is.character(file_group_ids) ||
      length(file_group_ids) == 0L ||
      anyNA(file_group_ids) ||
      any(!nzchar(file_group_ids)) ||
      anyDuplicated(file_group_ids)
  ) {
    abort_llw(
      "Choose at least one unique stable file group before refinement.",
      type = "validation"
    )
  }
  if (!inherits(plan, "glc_collection_plan") || !is.list(plan)) {
    abort_llw("The parent collection plan is invalid.", type = "validation")
  }
  if (
    !inherits(refinement, "glc_collection_refinement") || !is.list(refinement)
  ) {
    abort_llw(
      "glcdp did not return a glc_collection_refinement object.",
      type = "validation"
    )
  }
  required <- c(
    "refinement_schema",
    "refinement_version",
    "parent",
    "provenance",
    "request",
    "assurance",
    "compatibility_id",
    "final_selection_required",
    "preferred_unit_id",
    names(glc_collection_refinement_required_tables())
  )
  missing <- setdiff(required, names(refinement))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "GLC collection refinement is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  if (
    !identical(refinement$refinement_schema, "glc-collection-refinement") ||
      !identical(refinement$refinement_version, "1.1.0") ||
      !identical(refinement$compatibility_id, compatibility_id)
  ) {
    abort_llw(
      "The glcdp collection-refinement schema is not supported.",
      type = "validation"
    )
  }
  fingerprint <- plan$refinement_input$fingerprint %||% NULL
  if (
    !is.list(refinement$parent) ||
      !identical(refinement$parent$plan_schema, plan$plan_schema) ||
      !identical(refinement$parent$plan_version, plan$plan_version) ||
      !identical(refinement$parent$fingerprint, fingerprint)
  ) {
    abort_llw(
      "The collection refinement does not match its retained parent plan.",
      type = "validation"
    )
  }
  if (
    !is.list(refinement$provenance) ||
      !identical(
        refinement$provenance$repository,
        plan$provenance$repository
      ) ||
      !identical(
        tolower(refinement$provenance$source_revision),
        tolower(plan$provenance$source_revision)
      )
  ) {
    abort_llw(
      "The collection refinement does not match the exact package source.",
      type = "validation"
    )
  }
  if (
    !is.list(refinement$request) ||
      !identical(refinement$request$compatibility_id, compatibility_id) ||
      !setequal(as.character(refinement$request$file_group), file_group_ids)
  ) {
    abort_llw(
      "The collection refinement does not match the selected file groups.",
      type = "validation"
    )
  }
  assurance <- refinement$assurance
  if (
    !is.list(assurance) ||
      !identical(assurance$package_reopened, FALSE) ||
      !identical(assurance$network_access, FALSE) ||
      !identical(assurance$remote_availability_probed, FALSE) ||
      !identical(assurance$measurement_contents_transferred, FALSE) ||
      !identical(assurance$measurement_contents_inspected, FALSE)
  ) {
    abort_llw(
      "The collection refinement lacks the required zero-access assurance.",
      type = "validation"
    )
  }
  table_contract <- glc_collection_refinement_required_tables()
  for (name in names(table_contract)) {
    validate_glc_inspection_table(
      refinement[[name]],
      paste("collection refinement", name),
      table_contract[[name]]
    )
  }
  if (
    !setequal(as.character(refinement$groups$file_group_id), file_group_ids)
  ) {
    abort_llw(
      "The collection refinement returned unexpected file-group membership.",
      type = "validation"
    )
  }
  assert_serializable_value(refinement, "GLC collection refinement")
  invisible(refinement)
}

glcdp_refine_collection <- function(
  plan,
  compatibility_id,
  file_group_ids,
  api = NULL
) {
  api <- glcdp_api(api)
  tryCatch(
    {
      refinement <- api$glc_collection_refine(
        plan = plan,
        file_group = file_group_ids,
        compatibility_id = compatibility_id
      )
      validate_glc_collection_refinement(
        refinement,
        plan,
        compatibility_id,
        file_group_ids
      )
      refinement
    },
    error = function(cnd) {
      if (inherits(cnd, "llw_error")) stop(cnd)
      stop(map_glcdp_condition(cnd))
    }
  )
}

new_glcdp_planning_payload <- function(
  source,
  request,
  progress_path = NULL
) {
  source <- validate_glc_source_spec(source)
  request <- validate_glc_planning_request(request)
  if (!is.null(progress_path)) {
    assert_scalar_string(progress_path, "progress_path")
    progress_path <- normalizePath(
      progress_path,
      winslash = "/",
      mustWork = FALSE
    )
  }
  payload <- list(
    source = source,
    request = request,
    progress_path = progress_path
  )
  assert_serializable_value(payload, "glcdp planning task payload")
  payload
}

glcdp_plan_source <- function(payload, api = NULL) {
  if (
    !is.list(payload) ||
      !all(c("source", "request") %in% names(payload))
  ) {
    abort_llw(
      "The glcdp planning task payload is incomplete.",
      type = "validation"
    )
  }
  source <- validate_glc_source_spec(payload$source)
  request <- validate_glc_planning_request(payload$request)
  api <- glcdp_api(api)
  progress <- new_glc_phase_log(payload$progress_path %||% NULL)

  tryCatch(
    {
      progress$begin(
        "open",
        "Reopening exact revision",
        paste(
          "Reopening",
          source$repository,
          "at",
          source$resolved_revision,
          "for metadata-only structural planning."
        )
      )
      handle <- api$glc_open(
        source = source$repository,
        ref = source$resolved_revision,
        cache_dir = source$cache_dir,
        registry = source$registry_url,
        quiet = TRUE
      )
      progress$complete("The exact inspected revision was reopened.")

      progress$begin(
        "plan",
        "Building collectable units",
        paste(
          "Applying glcdp's public structural-compatibility engine",
          if (!is.null(request$term) && !is.null(request$modality)) {
            "to the selected canonical term within the selected modality."
          } else if (!is.null(request$term)) {
            "to the selected canonical term."
          } else {
            "to the stable file groups using the selected modality."
          }
        )
      )
      plan <- api$glc_collection_plan(
        x = handle,
        terms = request$term,
        variable_scope = request$variable_scope,
        variables = if (identical(request$variable_scope, "selected")) {
          request$source_variables
        } else {
          NULL
        },
        dataset_id = NULL,
        file_group = if (length(request$file_group_ids) > 0L) {
          request$file_group_ids
        } else {
          NULL
        },
        standardize = "lightlogr"
      )
      progress$complete("Structural compatibility sets were built.")

      progress$begin(
        "validation",
        "Validating planner assurance",
        paste(
          "Checking exact-source provenance, metadata-only assurance,",
          "stable identifiers, and zero-access refinement support."
        )
      )
      validate_glc_collection_plan(plan, source, request)
      progress$complete("The collection plan is ready for metadata narrowing.")
      plan
    },
    error = function(cnd) {
      progress$fail(
        "This phase did not complete. Review the task error and retry."
      )
      if (inherits(cnd, "llw_error")) {
        stop(cnd)
      }
      stop(map_glcdp_condition(cnd))
    }
  )
}

glcdp_planning_task_worker <- function(payload, spec) {
  glcdp_plan_source(payload)
}
