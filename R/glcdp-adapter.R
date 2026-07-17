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
    "glc_read",
    "glc_collect",
    "glc_download"
  )
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
  assert_scalar_string(repository, "repository")
  repository_parts <- strsplit(repository, "/", fixed = TRUE)[[1L]]
  if (
    !grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repository) ||
      any(repository_parts %in% c(".", ".."))
  ) {
    abort_llw(
      "`repository` must use the conservative `owner/repository` form.",
      type = "validation"
    )
  }
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
  expected_size <- if ("expected_size" %in% names(files)) {
    as.numeric(files$expected_size)
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
    transfer_bytes = if (length(expected_size) == 0L) {
      NA_real_
    } else {
      sum(expected_size, na.rm = TRUE)
    },
    availability = availability,
    compatibility_partitions = list(),
    warnings = character(),
    estimate_uncertain = length(expected_size) == 0L || anyNA(expected_size)
  )
}

glcdp_read_selection <- function(handle, selection, api) {
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
    files = if (length(selection$files) == 0L) NULL else selection$files,
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
    n_max = selection$preview_n_max,
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
