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
      abort_llw("No active GLC task phase can be completed.", type = "validation")
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
        "terms",
        "Building semantic-term catalogue",
        paste(
          "Deduplicating canonical term identifiers and calculating",
          "dataset and file-group coverage."
        )
      )
      terms <- glc_term_catalogue(variables)
      expected_bytes <- as.numeric(files$expected_bytes)
      byte_warnings <- if (length(expected_bytes) > 0L && anyNA(expected_bytes)) {
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
        terms = terms,
        warnings = c(schema_warnings, byte_warnings, label_warnings)
      )
      assert_glc_worker_result_safe(inspection, request$cache_dir)
      progress$complete("The metadata-only inspection result is ready.")
      inspection
    },
    error = function(cnd) {
      progress$fail("This phase did not complete. Review the task error and retry.")
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
      "study_id", "schema_version", "title", "short_description",
      "preregistration", "registration", "ethics", "sample",
      "intervention", "setting", "geographical_location", "study_type",
      "funding_sources", "keywords", "dataset_ids"
    ),
    study_groups = c(
      "study_id", "position", "name", "description", "size",
      "inclusion", "exclusion", "dataset_ids"
    ),
    study_contributors = c(
      "study_id", "position", "full_name", "roles", "email", "orcid",
      "institution_name", "institution_city", "institution_country"
    ),
    contributors = c(
      "contributor_id", "position", "full_name", "roles", "email", "orcid",
      "institution_name", "institution_city", "institution_country"
    ),
    datasets = c(
      "dataset_id", "schema_version", "study_id", "study_link_status",
      "participant_id", "participant_associated", "participant_link_status",
      "timezone", "latitude", "longitude", "file_group_count", "file_count",
      "modalities", "device_ids", "primary_variables"
    ),
    dataset_terms = c("dataset_id", "position", "term", "label"),
    participants = c("participant_id", "age", "sex", "gender"),
    participant_characteristics = c(
      "participant_id", "participant_link_status", "characteristic_position",
      "value_position", "name", "value", "value_type", "unit", "description"
    ),
    devices = c(
      "device_id", "schema_version", "manufacturer", "model",
      "serial_number", "calibration_date", "firmware_version",
      "datasheet_id", "datasheet_link_status"
    ),
    device_sensors = c(
      "device_id", "position", "sensor_type", "datasheet_id",
      "datasheet_link_status"
    ),
    datasheets = c(
      "datasheet_id", "schema_version", "datasheet_version", "manufacturer",
      "type", "modalities", "modality_other", "model",
      "calibration_interval", "calibration_method", "calibration_accuracy",
      "calibration_range", "calibration_notes",
      "calibration_spectral_sensitivity", "calibration_linearity",
      "calibration_directional_response"
    ),
    datasheet_parameters = c(
      "datasheet_id", "position", "name", "value", "value_type", "unit",
      "description"
    ),
    datasheet_channels = c(
      "datasheet_id", "position", "channel_number", "name", "description",
      "unit"
    ),
    instruments = c(
      "dataset_id", "file_group_id", "instrument_type", "instrument_name",
      "collection_method", "recorded_by", "software_name"
    ),
    file_group_variables = c(
      "dataset_id", "file_group_id", "declaration_position",
      "selected_by_request", "selected_for_output", "selection_origin",
      "name", "label", "description", "unit", "calibration", "type",
      "term", "term_name", "primary", "factor_level_count"
    ),
    file_group_factor_levels = c(
      "dataset_id", "file_group_id", "variable_position", "variable_name",
      "level_position", "value", "label", "description"
    ),
    extensions = c(
      "resource", "entity_type", "entity_id", "parent_id", "position",
      "metadata"
    )
  )
}

new_glc_planning_request <- function(
  term = NULL,
  modality = NULL,
  file_group_ids = character()
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
  if (is.null(term) && is.null(modality)) {
    abort_llw(
      "Choose a canonical term, a modality, or both before planning.",
      type = "validation"
    )
  }
  if (!is.null(modality) && length(file_group_ids) == 0L) {
    abort_llw(
      "The selected modality has no stable file-group identifiers.",
      type = "validation"
    )
  }
  if (is.null(modality) && length(file_group_ids) > 0L) {
    abort_llw(
      "File-group restrictions require a selected modality.",
      type = "validation"
    )
  }
  request <- structure(
    list(
      term = term,
      modality = modality,
      file_group_ids = file_group_ids,
      variable_scope = if (is.null(term)) "all" else "matched"
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
    file_group_ids = request$file_group_ids
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
  if (
    !is.list(plan$request) ||
      !identical(plan$request$variable_scope, request$variable_scope) ||
      !identical(as.character(plan$request$terms), expected_terms) ||
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

glc_collection_refinement_required_tables <- function() {
  list(
    units = glc_collection_plan_required_tables()$units,
    groups = c(
      "status", "unit_id", "compatibility_id", "dataset_id", "file_group",
      "file_group_id", "study_id", "participant_id",
      "participant_associated", "study_link_status",
      "participant_link_status", "device_id", "device_link_status",
      "reason_codes", "messages"
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
  if (!inherits(refinement, "glc_collection_refinement") || !is.list(refinement)) {
    abort_llw(
      "glcdp did not return a glc_collection_refinement object.",
      type = "validation"
    )
  }
  required <- c(
    "refinement_schema", "refinement_version", "parent", "provenance",
    "request", "assurance", "compatibility_id", "final_selection_required",
    "preferred_unit_id", names(glc_collection_refinement_required_tables())
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
  if (!setequal(as.character(refinement$groups$file_group_id), file_group_ids)) {
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
        variables = NULL,
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
      progress$fail("This phase did not complete. Review the task error and retry.")
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
