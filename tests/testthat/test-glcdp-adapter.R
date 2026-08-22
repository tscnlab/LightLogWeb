fake_glcdp_api <- function(call_log = new.env(parent = emptyenv())) {
  call_log$opened_ref <- NULL
  api <- list(
    glc_packages = function(...) data.frame(),
    glc_search_packages = function(...) data.frame(),
    glc_open = function(source, ref, cache_dir, registry, quiet) {
      call_log$opened_ref <- ref
      structure(
        list(
          source = source,
          transport = new.env(parent = emptyenv())
        ),
        class = "glc_package"
      )
    },
    glc_schema_versions = function() data.frame(),
    glc_summary = function(x) data.frame(dataset_count = 1L),
    glc_datasets = function(x, dataset_id = NULL) {
      data.frame(dataset_id = "dataset-a")
    },
    glc_files = function(x, dataset_id = NULL, file_group = NULL) {
      data.frame(
        path = "data.csv",
        expected_bytes = 100,
        available = TRUE
      )
    },
    glc_variables = function(
      x,
      dataset_id = NULL,
      file_group = NULL,
      primary = NULL
    ) {
      data.frame(variable = "MEDI")
    },
    glc_resources = function(x) data.frame(resource = "datasets"),
    glc_metadata = function(x, resources = NULL) list(study = "Fixture"),
    glc_search_metadata = function(...) data.frame(),
    glc_collection_plan = function(...) stop("not used"),
    glc_collection_refine = function(...) stop("not used"),
    glc_read = function(...) {
      call_log$read <- list(...)
      structure(list(), class = "glc_data_collection")
    },
    glc_collect = function(x, standardize) m1_fixture_data(),
    glc_download = function(...) data.frame(path = "data.csv")
  )
  list(api = api, calls = call_log)
}

test_that("GLC source and selection objects are serializable data only", {
  cache_dir <- tempfile("glc-cache-")
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "latest_pass",
    resolved_revision = paste(rep("a", 40L), collapse = ""),
    validation_state = "passed",
    attestation_verified = TRUE,
    schema_version = "3.0.2",
    glcdp_version = "1.0.0",
    cache_dir = cache_dir
  )
  selection <- new_glc_selection(
    dataset_ids = "dataset-a",
    source_variables = "MEDI"
  )

  expect_s3_class(source, "llw_glc_source_spec")
  expect_s3_class(selection, "llw_glc_selection")
  expect_null(nonserializable_path(source))
  expect_null(nonserializable_path(selection))
  expect_type(serialize(source, NULL), "raw")

  live_handle <- structure(
    list(transport = new.env(parent = emptyenv())),
    class = "glc_package"
  )
  expect_error(
    assert_serializable_value(live_handle, "handle"),
    class = "llw_validation_error",
    regexp = "live `glc_package` handle"
  )
})

test_that("the GLC worker reopens the exact revision and returns no live handle", {
  fake <- fake_glcdp_api()
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = paste(rep("b", 40L), collapse = ""),
    validation_state = "passed",
    schema_version = "3.0.2",
    glcdp_version = "1.0.0",
    cache_dir = tempfile("glc-cache-")
  )
  selection <- new_glc_selection(dataset_ids = "dataset-a")

  preview <- glcdp_worker_execute(
    "discovery",
    source = source,
    selection = selection,
    api = fake$api
  )

  expect_identical(fake$calls$opened_ref, source$resolved_revision)
  expect_s3_class(preview, "llw_glc_preview")
  expect_equal(preview$transfer_bytes, 100)
  expect_null(nonserializable_path(preview))
  expect_type(serialize(preview, NULL), "raw")
})

test_that("the worker boundary revalidates modified GLC value objects", {
  fake <- fake_glcdp_api()
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("d", 40L),
    validation_state = "passed",
    schema_version = "3.0.2",
    glcdp_version = "1.0.0",
    cache_dir = tempfile("glc-cache-")
  )
  selection <- new_glc_selection(dataset_ids = "dataset-a")

  source$resolved_revision <- "moving-branch"
  expect_error(
    glcdp_worker_execute(
      "discovery",
      source = source,
      selection = selection,
      api = fake$api
    ),
    class = "llw_validation_error",
    regexp = "exact 40-character commit SHA"
  )
  expect_null(fake$calls$opened_ref)

  source$resolved_revision <- strrep("d", 40L)
  selection$preview_n_max <- -1L
  expect_error(
    new_glcdp_task_payload(
      "read",
      source = source,
      selection = selection
    ),
    class = "llw_validation_error",
    regexp = "non-negative whole number"
  )
})

test_that("declared transfer bytes use the glcdp 1.0 expected_bytes field", {
  fake <- fake_glcdp_api()
  fake$api$glc_files <- function(x, dataset_id = NULL, file_group = NULL) {
    data.frame(
      path = c("known.csv", "unknown.csv"),
      expected_bytes = c(125, NA_real_),
      available = c(TRUE, TRUE)
    )
  }
  preview <- glcdp_inventory_preview(
    structure(list(), class = "glc_package"),
    new_glc_selection(dataset_ids = "dataset-a"),
    fake$api
  )

  expect_equal(preview$transfer_bytes, 125)
  expect_true(preview$estimate_uncertain)

  fake$api$glc_files <- function(x, dataset_id = NULL, file_group = NULL) {
    data.frame(
      path = "unknown.csv",
      expected_bytes = NA_real_,
      available = TRUE
    )
  }
  unknown <- glcdp_inventory_preview(
    structure(list(), class = "glc_package"),
    new_glc_selection(dataset_ids = "dataset-a"),
    fake$api
  )
  expect_true(is.na(unknown$transfer_bytes))
  expect_true(unknown$estimate_uncertain)
})

test_that("a total preview budget becomes a safe deterministic per-file plan", {
  allocation <- new_glc_preview_allocation(
    total_budget = 10,
    file_ids = paste0("file-", 1:7, ".csv")
  )
  expect_s3_class(allocation, "llw_glc_preview_allocation")
  expect_equal(allocation$preview_file_count, 7L)
  expect_equal(allocation$per_file_n_max, 1)
  expect_equal(allocation$maximum_rows, 7)
  expect_equal(allocation$unused_budget, 3)
  expect_lte(allocation$maximum_rows, allocation$total_budget)
  expect_length(allocation$omitted_files, 0L)

  bounded <- new_glc_preview_allocation(
    total_budget = 3,
    file_ids = paste0("file-", 1:7, ".csv")
  )
  expect_equal(bounded$preview_files, paste0("file-", 1:3, ".csv"))
  expect_equal(bounded$omitted_files, paste0("file-", 4:7, ".csv"))
  expect_equal(bounded$per_file_n_max, 1)
  expect_equal(bounded$maximum_rows, 3)
  expect_null(nonserializable_path(bounded))
})

test_that("preview reads apply the derived file subset and per-file n_max", {
  fake <- fake_glcdp_api()
  fake$api$glc_files <- function(x, dataset_id = NULL, file_group = NULL) {
    data.frame(
      path = paste0("file-", 1:5, ".csv"),
      expected_bytes = rep(100, 5L),
      available = TRUE
    )
  }
  selection <- new_glc_selection(
    dataset_ids = "dataset-a",
    preview_n_max = 3L
  )
  result <- glcdp_read_selection(
    structure(list(), class = "glc_package"),
    selection,
    fake$api
  )

  expect_s3_class(result, "data.frame")
  expect_equal(fake$calls$read$files, paste0("file-", 1:3, ".csv"))
  expect_equal(fake$calls$read$n_max, 1)
  expect_identical(fake$calls$read$progress, FALSE)
  expect_identical(fake$calls$read$problems, "error")
})

test_that("documented glcdp conditions map to recoverable public errors", {
  cases <- list(
    discovery = "glcdp_registry_error",
    validation = "glcdp_schema_error",
    transport = "glcdp_http_error",
    lfs = "glcdp_lfs_hash_mismatch",
    import = "glcdp_datetime_parse",
    compatibility = "glcdp_incompatible_collection",
    resource = "glcdp_file_exists"
  )

  for (category in names(cases)) {
    source <- structure(
      list(message = paste("private", category, "detail"), call = NULL),
      class = c(cases[[category]], "error", "condition")
    )
    mapped <- map_glcdp_condition(source)
    expect_s3_class(mapped, paste0("llw_glcdp_", category, "_error"))
    expect_identical(mapped$diagnostics$category, category)
    expect_match(mapped$diagnostics$original_message, "private")
    expect_false(grepl("private", llw_public_message(mapped), fixed = TRUE))
  }
})

test_that("the installed glcdp seam matches the reviewed public export contract", {
  skip_if_not_installed("glcdp", minimum_version = "1.0.0.9000")
  exports <- getNamespaceExports("glcdp")

  expect_true(all(glcdp_export_contract() %in% exports))
  expect_identical(
    names(formals(glcdp::glc_open)),
    c("source", "ref", "token", "cache_dir", "registry", "quiet")
  )
  expect_identical(
    names(formals(glcdp::glc_read)),
    c(
      "x",
      "dataset_id",
      "file_group",
      "files",
      "variables",
      "terms",
      "primary_only",
      "n_max",
      "problems",
      "progress"
    )
  )
  expect_identical(
    names(formals(glcdp::glc_download)),
    c(
      "x",
      "dest_dir",
      "include",
      "dataset_id",
      "file_group",
      "resources",
      "files",
      "overwrite"
    )
  )
  provenance <- glcdp_runtime_provenance()
  expect_identical(provenance$version, "1.0.0.9000")
  expect_identical(
    provenance$revision,
    "e49fcf153a114916ce2680f8b40eb475e694e10d"
  )
})
test_that("task payload construction rejects a live glcdp package handle", {
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("c", 40L),
    validation_state = "passed",
    schema_version = "3.0.2",
    glcdp_version = "1.0.0",
    cache_dir = tempfile("glc-cache-")
  )
  selection <- new_glc_selection(dataset_ids = "dataset-a")
  handle <- structure(
    list(transport = new.env(parent = emptyenv())),
    class = "glc_package"
  )

  expect_error(
    new_glcdp_task_payload(
      "read",
      source = source,
      selection = selection,
      destination = handle
    ),
    class = "llw_validation_error",
    regexp = "live `glc_package` handle"
  )
})

glc_adapter_rich_api <- function() {
  fake <- fake_glcdp_api()
  fake$calls$read <- NULL
  fake$calls$collect <- NULL
  fake$calls$plan <- NULL
  fake$api$glc_schema_versions <- function() {
    data.frame(
      version = "3.0.2",
      status = "stable",
      notes = "Current stable schema.",
      stringsAsFactors = FALSE
    )
  }
  fake$api$glc_summary <- function(x) {
    data.frame(
      schema_version = "3.0.2",
      dataset_count = 2L,
      participant_count = 2L,
      file_count = 2L,
      declared_bytes = 300,
      stringsAsFactors = FALSE
    )
  }
  fake$api$glc_datasets <- function(x, dataset_id = NULL) {
    value <- data.frame(
      dataset_id = c("dataset-a", "dataset-b"),
      schema_version = "3.0.2",
      study_id = "study-a",
      participant_id = c("participant-a", "participant-b"),
      timezone = "UTC",
      file_group_count = 1L,
      file_count = 1L,
      stringsAsFactors = FALSE
    )
    value$modalities <- rep(list("light"), 2L)
    value$device_ids <- list("device-a", "device-b")
    value$primary_variables <- rep(list("MEDI"), 2L)
    value
  }
  fake$api$glc_files <- function(
    x,
    dataset_id = NULL,
    file_group = NULL
  ) {
    value <- data.frame(
      dataset_id = c("dataset-a", "dataset-b"),
      file_group_id = c("dataset-a:1", "dataset-b:1"),
      path = c("a.csv", "b.csv"),
      encoding = "UTF-8",
      timezone = "UTC",
      role = "measurement",
      data_state = "raw",
      device_id = c("device-a", "device-b"),
      temporal_type = "interval",
      temporal_value = 1,
      temporal_unit = "minute",
      storage = "git",
      expected_bytes = c(100, NA_real_),
      available = TRUE,
      stringsAsFactors = FALSE
    )
    value$modalities <- rep(list("light"), 2L)
    value
  }
  fake$api$glc_variables <- function(
    x,
    dataset_id = NULL,
    file_group = NULL,
    term = NULL,
    primary = NULL
  ) {
    value <- data.frame(
      dataset_id = c("dataset-a", "dataset-b", "dataset-a"),
      file_group_id = c("dataset-a:1", "dataset-b:1", "dataset-a:1"),
      name = c("MEDI", "melEDI", "Datetime"),
      label = c("Melanopic EDI", "Melanopic EDI", "Date and time"),
      description = c(
        "Melanopic equivalent daylight illuminance.",
        "Melanopic equivalent daylight illuminance.",
        "Measurement timestamp."
      ),
      unit = c("lx", "lx", NA_character_),
      type = c("number", "number", "datetime"),
      term = c("melanopic_edi", "melanopic_edi", "datetime"),
      term_name = c("Melanopic EDI", "Melanopic EDI", "Date and time"),
      primary = c(TRUE, TRUE, FALSE),
      stringsAsFactors = FALSE
    )
    value$factor_values <- rep(list(character()), 3L)
    value$factor_labels <- rep(list(character()), 3L)
    value$factor_descriptions <- rep(list(character()), 3L)
    value
  }
  fake$api$glc_resources <- function(x) {
    data.frame(
      resource = c("study", "participants"),
      path = c("metadata/study.json", "metadata/participants.csv"),
      core = TRUE,
      format = c("json", "csv"),
      media_type = c("application/json", "text/csv"),
      stringsAsFactors = FALSE
    )
  }
  fake$api$glc_metadata <- function(x, resources = NULL) {
    list(
      study = data.frame(
        study_id = "study-a",
        title = "Fixture study",
        stringsAsFactors = FALSE
      ),
      participants = data.frame(
        participant_id = c("participant-a", "participant-b"),
        age = c(30, NA_real_),
        stringsAsFactors = FALSE
      )
    )
  }
  fake$api$glc_read <- function(...) {
    fake$calls$read <- list(...)
    stop("measurement read must not run")
  }
  fake$api$glc_collect <- function(...) {
    fake$calls$collect <- list(...)
    stop("collection must not run")
  }
  fake
}

glc_adapter_empty_table <- function(columns, rows = 0L) {
  values <- stats::setNames(
    replicate(length(columns), rep(NA_character_, rows), simplify = FALSE),
    columns
  )
  as.data.frame(values, stringsAsFactors = FALSE)
}

glc_adapter_plan_fixture <- function(
  source,
  request = new_glc_planning_request(term = "melanopic_edi"),
  transfer = FALSE
) {
  request <- validate_glc_planning_request(request)
  term <- request$term %||% "melanopic_edi"
  compatibility_id <- paste0("glcc_", strrep("1", 64L))
  unit_id <- paste0("glcu_", strrep("2", 64L))

  contracts <- glc_collection_plan_required_tables()
  units <- glc_adapter_empty_table(contracts$units, 1L)
  units$unit_id <- unit_id
  units$compatibility_id <- compatibility_id
  units$preferred <- TRUE
  units$dataset_count <- 2L
  units$file_group_count <- 2L
  units$variable_count <- 2L
  units$file_count <- 2L
  units$declared_bytes <- 100
  units$known_file_count <- 1L
  units$unknown_file_count <- 1L
  units$declared_bytes_complete <- FALSE
  units$harmonization_required <- FALSE
  units$harmonized_variables <- list(character())
  units$diagnostic_ids <- list(character())
  units$file_group_ids <- list(c("dataset-a:1", "dataset-b:1"))

  groups <- glc_adapter_empty_table(contracts$groups, 2L)
  groups$status <- "included"
  groups$unit_id <- unit_id
  groups$compatibility_id <- compatibility_id
  groups$dataset_id <- c("dataset-a", "dataset-b")
  groups$file_group <- 1L
  groups$file_group_id <- c("dataset-a:1", "dataset-b:1")
  groups$study_id <- "study-a"
  groups$participant_id <- c("participant-a", "participant-b")
  groups$participant_associated <- TRUE
  groups$study_link_status <- "resolved"
  groups$participant_link_status <- "resolved"
  groups$device_id <- c("device-a", "device-b")
  groups$device_link_status <- "resolved"
  groups$device_location <- "wrist"
  groups$device_location_type <- "body_worn"
  groups$description <- c("Group A", "Group B")
  groups$format <- "csv"
  groups$timezone <- "UTC"
  groups$role <- "measurement"
  groups$data_state <- "raw"
  groups$modalities <- rep(list("light"), 2L)
  groups$modality_other <- NA_character_
  groups$modality_other_type <- NA_character_
  groups$selected_variables <- list("MEDI", "melEDI")
  groups$reason_codes <- rep(list("included"), 2L)
  groups$messages <- rep(list("Included."), 2L)

  variables <- glc_adapter_empty_table(contracts$variables, 2L)
  variables$unit_id <- unit_id
  variables$dataset_id <- c("dataset-a", "dataset-b")
  variables$file_group_id <- c("dataset-a:1", "dataset-b:1")
  variables$position <- 1L
  variables$name <- c("MEDI", "melEDI")
  variables$label <- "Melanopic EDI"
  variables$description <- "Melanopic equivalent daylight illuminance."
  variables$unit <- "lx"
  variables$type <- "number"
  variables$term <- term
  variables$term_name <- "Melanopic EDI"
  variables$primary <- TRUE
  variables$factor_values <- rep(list(character()), 2L)
  variables$factor_labels <- rep(list(character()), 2L)
  variables$factor_descriptions <- rep(list(character()), 2L)
  variables$selection_origin <- "matched"

  read_columns <- glc_adapter_empty_table(contracts$read_columns, 2L)
  read_columns$unit_id <- unit_id
  read_columns$dataset_id <- c("dataset-a", "dataset-b")
  read_columns$file_group_id <- c("dataset-a:1", "dataset-b:1")
  read_columns$position <- 1L
  read_columns$name <- c("MEDI", "melEDI")
  read_columns$declared_type <- "number"
  read_columns$origin <- "selected"
  read_columns$selected_for_output <- TRUE
  read_columns$automatic <- FALSE
  read_columns$message <- "Selected by canonical term."

  output_columns <- glc_adapter_empty_table(contracts$output_columns, 1L)
  output_columns$unit_id <- unit_id
  output_columns$position <- 1L
  output_columns$name <- "MEDI"
  output_columns$source_declared_type <- "number"
  output_columns$expected_type <- "numeric"
  output_columns$origin <- "selected"
  output_columns$automatic <- FALSE
  output_columns$runtime_validation_required <- TRUE
  output_columns$collision_validation_required <- TRUE
  output_columns$message <- "Validated after read."

  files <- glc_adapter_empty_table(contracts$files, 2L)
  files$status <- "included"
  files$unit_id <- unit_id
  files$dataset_id <- c("dataset-a", "dataset-b")
  files$file_group_id <- c("dataset-a:1", "dataset-b:1")
  files$position <- 1L
  files$declared_path <- c("a.csv", "b.csv")
  files$format <- "csv"
  files$encoding <- "UTF-8"
  files$declared_bytes <- c(100, NA_real_)
  files$bytes_known <- c(TRUE, FALSE)

  compatibility <- glc_adapter_empty_table(contracts$compatibility, 1L)
  compatibility$unit_id <- unit_id
  compatibility$timezone <- "UTC"
  compatibility$role <- "measurement"
  compatibility$data_state <- "raw"
  compatibility$standardize <- "lightlogr"
  compatibility$selected_names <- list(c("MEDI", "melEDI"))
  compatibility$declared_types <- list("number")
  compatibility$factor_values <- list(character())
  compatibility$factor_labels <- list(character())
  compatibility$factor_descriptions <- list(character())
  compatibility$harmonization_required <- FALSE
  compatibility$harmonized_variables <- list(character())
  compatibility$diagnostic_ids <- list(character())
  compatibility$modalities <- list("light")
  compatibility$standardize_affects_partition <- FALSE

  extensions <- glc_adapter_empty_table(contracts$extensions, 2L)
  extensions$dataset_id <- c("dataset-a", "dataset-b")
  extensions$file_group_id <- c("dataset-a:1", "dataset-b:1")
  extensions$metadata <- rep(list(list()), 2L)

  compatibility_sets <- glc_adapter_empty_table(
    contracts$compatibility_sets,
    1L
  )
  compatibility_sets$compatibility_id <- compatibility_id
  compatibility_sets$dataset_count <- 2L
  compatibility_sets$file_group_count <- 2L
  compatibility_sets$variable_count <- 2L
  compatibility_sets$file_count <- 2L
  compatibility_sets$declared_bytes <- 100
  compatibility_sets$known_file_count <- 1L
  compatibility_sets$unknown_file_count <- 1L
  compatibility_sets$declared_bytes_complete <- FALSE
  compatibility_sets$harmonization_required <- FALSE
  compatibility_sets$harmonized_variables <- list(character())
  compatibility_sets$diagnostic_ids <- list(character())
  compatibility_sets$final_unit_count <- 1L
  compatibility_sets$final_selection_required <- FALSE
  compatibility_sets$constraint_codes <- list(character())
  compatibility_sets$constraint_messages <- list(character())
  compatibility_sets$file_group_ids <- list(c("dataset-a:1", "dataset-b:1"))
  compatibility_sets$final_unit_ids <- list(unit_id)
  compatibility_sets$selected_names <- list(c("MEDI", "melEDI"))
  compatibility_sets$declared_types <- list("number")
  compatibility_sets$factor_values <- list(character())
  compatibility_sets$factor_labels <- list(character())
  compatibility_sets$factor_descriptions <- list(character())
  compatibility_sets$timezone <- "UTC"
  compatibility_sets$modalities <- list("light")
  compatibility_sets$role <- "measurement"
  compatibility_sets$data_state <- "raw"
  compatibility_sets$standardize_affects_structure <- FALSE

  compatibility_diagnostics <- glc_adapter_empty_table(
    contracts$compatibility_diagnostics
  )
  compatibility_diagnostic_groups <- glc_adapter_empty_table(
    contracts$compatibility_diagnostic_groups
  )

  metadata <- c(
    list(schema = "glc-package-metadata", version = "1.0.0"),
    lapply(
      glc_collection_plan_metadata_tables(),
      glc_adapter_empty_table
    )
  )

  structure(
    list(
      plan_schema = "glc-collection-plan",
      plan_version = "1.2.0",
      provenance = list(
        repository = source$repository,
        source_revision = source$resolved_revision,
        verification = "registry_latest_pass",
        metadata_fingerprint = strrep("e", 64L),
        planner_schema = "glc-collection-plan",
        planner_version = "1.2.0"
      ),
      request = list(
        terms = request$term %||% character(),
        variable_scope = request$variable_scope,
        requested_variables = request$source_variables,
        resolved_variables = request$source_variables,
        file_group = request$file_group_ids,
        standardize = "lightlogr"
      ),
      assurance = list(
        measurement_contents_transferred = transfer,
        measurement_contents_inspected = transfer,
        interactive_filtering_network_access = FALSE,
        refinement_network_access = FALSE
      ),
      preferred_unit_id = unit_id,
      units = units,
      groups = groups,
      variables = variables,
      read_columns = read_columns,
      output_columns = output_columns,
      files = files,
      compatibility = compatibility,
      extensions = extensions,
      compatibility_sets = compatibility_sets,
      compatibility_diagnostics = compatibility_diagnostics,
      compatibility_diagnostic_groups = compatibility_diagnostic_groups,
      metadata = metadata,
      refinement_input = list(
        schema = "glc-collection-refinement-input",
        version = "1.1.0",
        fingerprint = strrep("f", 64L)
      )
    ),
    class = c("glc_collection_plan", "list")
  )
}

glc_adapter_refinement_fixture <- function(
  plan,
  compatibility_id = plan$compatibility_sets$compatibility_id[[1L]],
  file_group_ids = plan$compatibility_sets$file_group_ids[[1L]]
) {
  group_rows <- plan$groups$file_group_id %in% file_group_ids
  groups <- plan$groups[
    group_rows,
    glc_collection_refinement_required_tables()$groups,
    drop = FALSE
  ]
  units <- plan$units[
    plan$units$compatibility_id == compatibility_id,
    glc_collection_refinement_required_tables()$units,
    drop = FALSE
  ]
  units$file_group_ids <- rep(list(sort(file_group_ids)), nrow(units))
  structure(
    list(
      refinement_schema = "glc-collection-refinement",
      refinement_version = "1.1.0",
      parent = list(
        plan_schema = plan$plan_schema,
        plan_version = plan$plan_version,
        fingerprint = plan$refinement_input$fingerprint
      ),
      provenance = plan$provenance,
      request = list(
        compatibility_id = compatibility_id,
        file_group = sort(file_group_ids)
      ),
      assurance = list(
        package_reopened = FALSE,
        network_access = FALSE,
        remote_availability_probed = FALSE,
        measurement_contents_transferred = FALSE,
        measurement_contents_inspected = FALSE
      ),
      compatibility_id = compatibility_id,
      final_selection_required = nrow(units) > 1L,
      preferred_unit_id = units$unit_id[[1L]],
      units = units,
      groups = groups,
      constraints = glc_adapter_empty_table(
        glc_collection_refinement_required_tables()$constraints
      )
    ),
    class = c("glc_collection_refinement", "list")
  )
}

glc_adapter_projection_fixture <- function() {
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("e", 40L),
    validation_state = "passed",
    attestation_verified = TRUE,
    schema_version = "3.0.2",
    glcdp_version = "1.0.0.9000",
    cache_dir = tempfile("glc-preview-cache-"),
    registry_generated_at = "2026-08-01T00:00:00Z"
  )
  request <- new_glc_planning_request(term = "melanopic_edi")
  plan <- glc_adapter_plan_fixture(source, request)
  compatibility_id <- plan$compatibility_sets$compatibility_id[[1L]]
  group_ids <- plan$compatibility_sets$file_group_ids[[1L]]

  plan$variables <- plan$variables[rep(1L, 2L), , drop = FALSE]
  plan$variables$dataset_id <- c("dataset-a", "dataset-b")
  plan$variables$file_group_id <- group_ids
  plan$variables$name <- "MEDI"
  plan$variables$selection_origin <- "all"

  read_columns <- glc_adapter_empty_table(
    glc_collection_plan_required_tables()$read_columns,
    4L
  )
  read_columns$unit_id <- plan$units$unit_id[[1L]]
  read_columns$dataset_id <- rep(c("dataset-a", "dataset-b"), each = 2L)
  read_columns$file_group_id <- rep(group_ids, each = 2L)
  read_columns$position <- rep(1:2, 2L)
  read_columns$name <- rep(c("MEDI", "Datetime"), 2L)
  read_columns$declared_type <- rep(c("number", "datetime"), 2L)
  read_columns$origin <- rep(c("selected", "required"), 2L)
  read_columns$selected_for_output <- rep(c(TRUE, FALSE), 2L)
  read_columns$automatic <- rep(c(FALSE, TRUE), 2L)
  read_columns$message <- rep(
    c("Selected output.", "Required time input."),
    2L
  )
  plan$read_columns <- read_columns

  output_columns <- glc_adapter_empty_table(
    glc_collection_plan_required_tables()$output_columns,
    2L
  )
  output_columns$unit_id <- plan$units$unit_id[[1L]]
  output_columns$position <- 1:2
  output_columns$name <- c("MEDI", "Datetime")
  output_columns$source_declared_type <- c("number", "datetime")
  output_columns$expected_type <- c("numeric", "POSIXct")
  output_columns$origin <- c("selected", "required")
  output_columns$automatic <- c(FALSE, TRUE)
  output_columns$runtime_validation_required <- TRUE
  output_columns$collision_validation_required <- TRUE
  output_columns$message <- c("Selected output.", "Automatic time column.")
  plan$output_columns <- output_columns

  refinement <- glc_adapter_refinement_fixture(
    plan,
    compatibility_id,
    group_ids
  )
  projection <- new_glc_projection_review(
    source = source,
    source_variables = "MEDI",
    parent_request = request,
    parent_plan = plan,
    parent_compatibility_id = compatibility_id,
    parent_refinement = refinement
  )
  list(
    source = source,
    request = request,
    plan = plan,
    refinement = refinement,
    projection = projection
  )
}

test_that("canonical term catalogue is deduplicated by identifier", {
  fake <- glc_adapter_rich_api()
  variables <- fake$api$glc_variables(structure(list(), class = "glc_package"))
  catalogue <- glc_term_catalogue(variables)

  expect_identical(
    catalogue$term,
    c("datetime", "melanopic_edi")
  )
  melanopic <- catalogue[catalogue$term == "melanopic_edi", , drop = FALSE]
  expect_identical(melanopic$label, "Melanopic EDI")
  expect_equal(melanopic$dataset_count, 2L)
  expect_equal(melanopic$file_group_count, 2L)
  expect_equal(melanopic$variable_count, 2L)
  expect_identical(
    melanopic$source_names[[1L]],
    c("MEDI", "melEDI")
  )

  variables$term_name[variables$term == "melanopic_edi"] <- NA_character_
  fallback <- glc_term_catalogue(variables)
  fallback <- fallback[fallback$term == "melanopic_edi", , drop = FALSE]
  expect_true(fallback$label_consistent)
  expect_identical(fallback$label, "Melanopic EDI")

  variables$term_name[variables$term == "melanopic_edi"] <- c(
    "Melanopic EDI",
    "Conflicting display label"
  )
  conflict <- glc_term_catalogue(variables)
  conflict <- conflict[conflict$term == "melanopic_edi", , drop = FALSE]
  expect_false(conflict$label_consistent)
  expect_identical(conflict$label, "melanopic_edi")
})

test_that("exact-package inspection reports phases and never reads measurements", {
  fake <- glc_adapter_rich_api()
  cache_dir <- tempfile("glc-inspection-cache-")
  dir.create(cache_dir)
  progress_path <- tempfile("glc-inspection-progress-", fileext = ".rds")
  request <- new_glc_inspection_request(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    resolved_revision = strrep("a", 40L),
    validation_state = "passed",
    attestation_verified = TRUE,
    glcdp_version = "1.0.0.9000",
    cache_dir = cache_dir,
    registry_generated_at = "2026-08-01T00:00:00Z"
  )

  inspection <- glcdp_inspect_source(
    request,
    progress_path = progress_path,
    api = fake$api
  )
  progress <- read_glc_task_progress(progress_path)

  expect_s3_class(inspection, "llw_glc_inspection")
  expect_identical(inspection$source$repository, "owner/repository")
  expect_identical(inspection$source$resolved_revision, strrep("a", 40L))
  expect_identical(inspection$terms$term, c("datetime", "melanopic_edi"))
  expect_identical(
    progress$phase,
    c("open", "description", "inventory", "metadata", "terms", "validation")
  )
  expect_true(all(progress$status == "complete"))
  expect_null(fake$calls$read)
  expect_null(fake$calls$collect)
  expect_false(glc_value_contains_path(inspection, cache_dir))
  expect_null(nonserializable_path(inspection))
  expect_type(serialize(inspection, NULL), "raw")
})

test_that("public collection planning preserves exact assurance and no transfer", {
  fake <- glc_adapter_rich_api()
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("b", 40L),
    validation_state = "passed",
    attestation_verified = TRUE,
    schema_version = "3.0.2",
    glcdp_version = "1.0.0.9000",
    cache_dir = tempfile("glc-plan-cache-"),
    registry_generated_at = "2026-08-01T00:00:00Z"
  )
  request <- new_glc_planning_request(term = "melanopic_edi")
  fake$api$glc_collection_plan <- function(...) {
    fake$calls$plan <- list(...)
    glc_adapter_plan_fixture(source, request)
  }
  progress_path <- tempfile("glc-plan-progress-", fileext = ".rds")
  payload <- new_glcdp_planning_payload(
    source,
    request,
    progress_path
  )

  plan <- glcdp_plan_source(payload, api = fake$api)
  progress <- read_glc_task_progress(progress_path)

  expect_s3_class(plan, "glc_collection_plan")
  expect_identical(fake$calls$opened_ref, strrep("b", 40L))
  expect_identical(fake$calls$plan$terms, "melanopic_edi")
  expect_identical(fake$calls$plan$variable_scope, "all")
  expect_identical(fake$calls$plan$standardize, "lightlogr")
  expect_identical(
    progress$phase,
    c("open", "plan", "validation")
  )
  expect_null(fake$calls$read)
  expect_null(fake$calls$collect)
  expect_null(nonserializable_path(plan))
  expect_type(serialize(plan, NULL), "raw")

  unsafe <- glc_adapter_plan_fixture(
    source,
    request,
    transfer = TRUE
  )
  expect_error(
    validate_glc_collection_plan(unsafe, source, request),
    class = "llw_validation_error",
    regexp = "metadata-only assurance"
  )

  stale_plan <- glc_adapter_plan_fixture(source, request)
  stale_plan$plan_version <- "1.1.0"
  stale_plan$provenance$planner_version <- "1.1.0"
  expect_error(
    validate_glc_collection_plan(stale_plan, source, request),
    class = "llw_validation_error",
    regexp = "schema is not supported"
  )

  stale_refinement_input <- glc_adapter_plan_fixture(source, request)
  stale_refinement_input$refinement_input$version <- "1.0.0"
  expect_error(
    validate_glc_collection_plan(stale_refinement_input, source, request),
    class = "llw_validation_error",
    regexp = "invalid refinement input"
  )
})

test_that("modality planning uses stable public file-group restrictions", {
  fake <- glc_adapter_rich_api()
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("c", 40L),
    validation_state = "passed",
    attestation_verified = TRUE,
    schema_version = "3.0.2",
    glcdp_version = "1.0.0.9000",
    cache_dir = tempfile("glc-plan-cache-"),
    registry_generated_at = "2026-08-01T00:00:00Z"
  )
  request <- new_glc_planning_request(
    modality = "light",
    file_group_ids = c("dataset-b:1", "dataset-a:1")
  )
  fake$api$glc_collection_plan <- function(...) {
    fake$calls$plan <- list(...)
    glc_adapter_plan_fixture(source, request)
  }

  plan <- glcdp_plan_source(
    new_glcdp_planning_payload(source, request),
    api = fake$api
  )

  expect_s3_class(plan, "glc_collection_plan")
  expect_null(fake$calls$plan$terms)
  expect_identical(fake$calls$plan$variable_scope, "all")
  expect_identical(
    fake$calls$plan$file_group,
    c("dataset-a:1", "dataset-b:1")
  )
  expect_null(fake$calls$read)
  expect_null(fake$calls$collect)

  combined <- new_glc_planning_request(
    term = "melanopic_edi",
    modality = "light",
    file_group_ids = c("dataset-a:1", "dataset-b:1")
  )
  expect_identical(combined$variable_scope, "all")
  expect_error(
    new_glc_planning_request(modality = "light"),
    class = "llw_validation_error",
    regexp = "stable file-group"
  )

  selected <- new_glc_planning_request(
    term = "melanopic_edi",
    file_group_ids = c("dataset-a:1", "dataset-b:1"),
    variable_scope = "selected",
    source_variables = c("melEDI", "MEDI")
  )
  expect_identical(selected$source_variables, c("MEDI", "melEDI"))
  expect_error(
    new_glc_planning_request(
      term = "melanopic_edi",
      variable_scope = "selected",
      source_variables = c("MEDI", "MEDI")
    ),
    class = "llw_validation_error",
    regexp = "unique exact source names"
  )
  expect_error(
    new_glc_planning_request(
      modality = "light",
      file_group_ids = "dataset-a:1",
      variable_scope = "matched"
    ),
    class = "llw_validation_error",
    regexp = "requires a canonical term"
  )
})

test_that("variable projection preserves the exact Slice 2A structure", {
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("e", 40L),
    validation_state = "passed",
    attestation_verified = TRUE,
    schema_version = "3.0.2",
    glcdp_version = "1.0.0.9000",
    cache_dir = tempfile("glc-plan-cache-"),
    registry_generated_at = "2026-08-01T00:00:00Z"
  )
  parent_request <- new_glc_planning_request(term = "melanopic_edi")
  parent_plan <- glc_adapter_plan_fixture(source, parent_request)
  parent_id <- parent_plan$compatibility_sets$compatibility_id[[1L]]
  refined_ids <- parent_plan$compatibility_sets$file_group_ids[[1L]]

  parent_plan$variables <- parent_plan$variables[rep(1L, 2L), , drop = FALSE]
  parent_plan$variables$dataset_id <- c("dataset-a", "dataset-b")
  parent_plan$variables$file_group_id <- refined_ids
  parent_plan$variables$name <- "MEDI"
  parent_plan$variables$selection_origin <- "all"

  read_columns <- glc_adapter_empty_table(
    glc_collection_plan_required_tables()$read_columns,
    4L
  )
  read_columns$unit_id <- parent_plan$units$unit_id[[1L]]
  read_columns$dataset_id <- rep(c("dataset-a", "dataset-b"), each = 2L)
  read_columns$file_group_id <- rep(refined_ids, each = 2L)
  read_columns$position <- rep(1:2, 2L)
  read_columns$name <- rep(c("MEDI", "Datetime"), 2L)
  read_columns$declared_type <- rep(c("number", "datetime"), 2L)
  read_columns$origin <- rep(c("selected", "required"), 2L)
  read_columns$selected_for_output <- rep(c(TRUE, FALSE), 2L)
  read_columns$automatic <- rep(c(FALSE, TRUE), 2L)
  read_columns$message <- rep(
    c("Selected output.", "Required time input."),
    2L
  )
  parent_plan$read_columns <- read_columns

  output_columns <- glc_adapter_empty_table(
    glc_collection_plan_required_tables()$output_columns,
    2L
  )
  output_columns$unit_id <- parent_plan$units$unit_id[[1L]]
  output_columns$position <- 1:2
  output_columns$name <- c("MEDI", "Datetime")
  output_columns$source_declared_type <- c("number", "datetime")
  output_columns$expected_type <- c("numeric", "POSIXct")
  output_columns$origin <- c("selected", "required")
  output_columns$automatic <- c(FALSE, TRUE)
  output_columns$runtime_validation_required <- TRUE
  output_columns$collision_validation_required <- TRUE
  output_columns$message <- c("Selected output.", "Automatic time column.")
  parent_plan$output_columns <- output_columns

  refinement <- glc_adapter_refinement_fixture(
    parent_plan,
    parent_id,
    refined_ids
  )

  token_map <- data.frame(
    original_token = c("14", "22"),
    numeric_value = c(14, 22),
    unit = c("points", "points"),
    stringsAsFactors = FALSE
  )
  review <- new_glc_projection_review(
    source = source,
    source_variables = "MEDI",
    parent_request = parent_request,
    parent_plan = parent_plan,
    parent_compatibility_id = parent_id,
    parent_refinement = refinement,
    metadata_filters = list(characteristic_numeric_tokens = token_map)
  )
  expect_s3_class(review, "llw_glc_projection_review")
  expect_identical(review$review_version, "1.0.0")
  expect_identical(review$refined_file_group_ids, refined_ids)
  expect_identical(review$parent$plan_fingerprint, strrep("f", 64L))
  expect_identical(review$selected_compatibility_id, parent_id)
  expect_identical(
    review$selected_unit_ids,
    parent_plan$units$unit_id[[1L]]
  )
  expect_identical(review$selected_source_variables, "MEDI")
  expect_identical(
    review$read_columns$name,
    c("MEDI", "Datetime", "MEDI", "Datetime")
  )
  expect_identical(review$output_columns$name, c("MEDI", "Datetime"))
  expect_identical(
    review$metadata_filters$characteristic_numeric_tokens,
    token_map
  )
  expect_true(review$assurance$projection_only)
  expect_false(review$assurance$compatibility_replanned)
  expect_false(review$assurance$compatibility_identity_changed)
  expect_false(review$assurance$measurement_contents_transferred)
  expect_null(nonserializable_path(review))
  expect_type(serialize(review, NULL), "raw")

  expect_error(
    new_glc_projection_review(
      source = source,
      source_variables = c("MEDI", "MEDI"),
      parent_request = parent_request,
      parent_plan = parent_plan,
      parent_compatibility_id = parent_id,
      parent_refinement = refinement
    ),
    class = "llw_validation_error",
    regexp = "unique exact source names"
  )
  expect_error(
    new_glc_projection_review(
      source = source,
      source_variables = "not_present",
      parent_request = parent_request,
      parent_plan = parent_plan,
      parent_compatibility_id = parent_id,
      parent_refinement = refinement
    ),
    class = "llw_validation_error",
    regexp = "not available in every retained file group"
  )

  stale_refinement <- refinement
  stale_refinement$parent$fingerprint <- strrep("0", 64L)
  expect_error(
    new_glc_projection_review(
      source = source,
      source_variables = "MEDI",
      parent_request = parent_request,
      parent_plan = parent_plan,
      parent_compatibility_id = parent_id,
      parent_refinement = stale_refinement
    ),
    class = "llw_validation_error",
    regexp = "retained parent plan"
  )

  matched_request <- new_glc_planning_request(
    term = "melanopic_edi",
    variable_scope = "matched"
  )
  matched_plan <- glc_adapter_plan_fixture(source, matched_request)
  matched_id <- matched_plan$compatibility_sets$compatibility_id[[1L]]
  matched_refinement <- glc_adapter_refinement_fixture(
    matched_plan,
    matched_id,
    matched_plan$compatibility_sets$file_group_ids[[1L]]
  )
  expect_error(
    new_glc_projection_review(
      source = source,
      source_variables = "MEDI",
      parent_request = matched_request,
      parent_plan = matched_plan,
      parent_compatibility_id = matched_id,
      parent_refinement = matched_refinement
    ),
    class = "llw_validation_error",
    regexp = "complete file schema"
  )
})

test_that("variable projection remaps contracts to refined public unit IDs", {
  fixture <- glc_adapter_projection_fixture()
  refinement <- fixture$refinement
  retained_id <- refinement$request$file_group[[1L]]
  refined_unit_id <- paste0("glcu_", strrep("9", 64L))

  refinement$request$file_group <- retained_id
  refinement$groups <- refinement$groups[
    refinement$groups$file_group_id == retained_id,
    ,
    drop = FALSE
  ]
  refinement$groups$unit_id <- refined_unit_id
  refinement$units <- refinement$units[1L, , drop = FALSE]
  refinement$units$unit_id <- refined_unit_id
  refinement$units$file_group_ids <- list(retained_id)
  refinement$units$file_group_count <- 1L
  refinement$units$dataset_count <- 1L
  refinement$units$file_count <- 1L
  refinement$preferred_unit_id <- refined_unit_id
  refinement$final_selection_required <- FALSE

  review <- new_glc_projection_review(
    source = fixture$source,
    source_variables = "MEDI",
    parent_request = fixture$request,
    parent_plan = fixture$plan,
    parent_compatibility_id = fixture$plan$compatibility_sets$compatibility_id[[
      1L
    ]],
    parent_refinement = refinement
  )

  expect_identical(review$selected_unit_ids, refined_unit_id)
  expect_identical(review$refined_file_group_ids, retained_id)
  expect_true(all(review$selected_groups$unit_id == refined_unit_id))
  expect_true(all(review$files$unit_id == refined_unit_id))
  expect_true(all(review$read_columns$unit_id == refined_unit_id))
  expect_true(all(review$output_columns$unit_id == refined_unit_id))
  expect_identical(
    review$selected_compatibility_id,
    fixture$projection$selected_compatibility_id
  )
  expect_true(review$assurance$projection_only)
  expect_false(review$assurance$compatibility_replanned)
  expect_null(nonserializable_path(review))
  expect_type(serialize(review, NULL), "raw")

  contradictory <- refinement
  contradictory$units$file_group_ids <- list("dataset-b:1")
  expect_error(
    new_glc_projection_review(
      source = fixture$source,
      source_variables = "MEDI",
      parent_request = fixture$request,
      parent_plan = fixture$plan,
      parent_compatibility_id = fixture$plan$compatibility_sets$compatibility_id[[
        1L
      ]],
      parent_refinement = contradictory
    ),
    class = "llw_validation_error",
    regexp = "contradicts"
  )
})

test_that("bounded transfer review fixes provenance and allocates canonically", {
  fixture <- glc_adapter_projection_fixture()
  review <- new_glc_transfer_review(
    fixture$projection,
    total_preview_rows = 1L,
    available_session_bytes = 100
  )

  expect_s3_class(review, "llw_glc_transfer_review")
  expect_identical(review$review_version, "1.2.0")
  expect_identical(review$limits$total_preview_rows, 10000L)
  expect_identical(review$problems_policy, "error")
  expect_identical(review$allocation$preview_files, "a.csv")
  expect_identical(review$allocation$omitted_files, "b.csv")
  expect_identical(review$allocation$per_file_n_max, 1)
  expect_identical(review$allocation$maximum_rows, 1)
  expect_identical(
    review$fixed$compatibility_id,
    fixture$projection$selected_compatibility_id
  )
  expect_identical(
    review$fixed$unit_id,
    fixture$projection$selected_unit_ids[[1L]]
  )
  expect_identical(
    review$fixed$metadata_fingerprint,
    strrep("e", 64L)
  )
  expect_identical(
    review$fixed$refinement_fingerprint,
    digest::digest(fixture$refinement, algo = "sha256", serialize = TRUE)
  )
  expect_identical(review$byte_evidence$allocated_known_declared_bytes, 100)
  expect_identical(review$byte_evidence$allocated_unknown_file_count, 0L)
  expect_false(review$resource$blocked)
  expect_true(review$resource$full_import_size_evidence_incomplete)
  expect_false(review$resource$full_import_blocked)
  expect_true(review$assurance$explicit_action_required)
  expect_true(review$assurance$bounded_preview_available)
  expect_true(review$assurance$full_import_available)
  expect_false(review$assurance$package_reopened)
  expect_false(review$assurance$measurement_contents_transferred)
  expect_null(nonserializable_path(review))
  expect_type(serialize(review, NULL), "raw")

  blocked <- new_glc_transfer_review(
    fixture$projection,
    total_preview_rows = 1L,
    available_session_bytes = 99
  )
  expect_true(blocked$resource$blocked)

  tampered <- review
  tampered$fixed$file_group_ids <- "dataset-z:1"
  expect_error(
    validate_glc_transfer_review(tampered),
    class = "llw_validation_error",
    regexp = "parent contract"
  )
})

test_that("full Apply uses a fresh complete read and never accepts preview rows", {
  fixture <- glc_adapter_projection_fixture()
  review <- new_glc_transfer_review(
    fixture$projection,
    total_preview_rows = 1L,
    problems = "error",
    available_session_bytes = 1000
  )
  calls <- new.env(parent = emptyenv())
  calls$open <- 0L
  calls$read <- 0L
  calls$collect <- 0L

  full_file_data <- list(
    data.frame(MEDI = c(10, 11), stringsAsFactors = FALSE),
    data.frame(MEDI = c(12, 13), stringsAsFactors = FALSE)
  )
  full_collection <- data.frame(
    dataset_id = review$files$dataset_id,
    file_group_id = review$files$file_group_id,
    files = I(lapply(
      review$files$declared_path,
      function(path) file.path("deterministic-fixture", path)
    )),
    runtime_input_ids = I(lapply(
      seq_len(nrow(review$files)),
      function(index) sprintf("fixture://full-input/%03d", index)
    )),
    data = I(full_file_data),
    stringsAsFactors = FALSE
  )
  full_data <- data.frame(
    MEDI = c(10, 11, 12, 13),
    Datetime = as.POSIXct("2026-08-01 12:00:00", tz = "UTC") + 0:3,
    EVENT = factor(c("0", "1", "4", "5")),
    stringsAsFactors = FALSE
  )

  exports <- glcdp_export_contract()
  api <- stats::setNames(
    lapply(exports, function(name) function(...) NULL),
    exports
  )
  api$glc_open <- function(...) {
    calls$open <- calls$open + 1L
    calls$open_args <- list(...)
    list(fixture = TRUE)
  }
  api$glc_read <- function(...) {
    calls$read <- calls$read + 1L
    calls$read_args <- list(...)
    full_collection
  }
  api$glc_collect <- function(x, standardize) {
    calls$collect <- calls$collect + 1L
    calls$collect_args <- list(x = x, standardize = standardize)
    full_data
  }

  payload <- new_glc_full_import_task_payload(
    fixture$source,
    review,
    progress_path = tempfile("glc-full-progress-", fileext = ".rds")
  )
  expect_identical(
    names(payload),
    c("operation", "source", "transfer_review", "progress_path")
  )
  expect_error(
    validate_glc_full_import_task_payload(c(
      payload,
      list(preview_result = list(tampered = TRUE))
    )),
    class = "llw_validation_error",
    regexp = "Preview results cannot be supplied"
  )

  first <- glcdp_full_import_execute(
    payload,
    api = api,
    execution_mode = "fixture"
  )
  arbitrary_preview <- list(
    data = data.frame(MEDI = -999),
    fingerprint = "tampered-preview"
  )
  arbitrary_preview$data$MEDI <- 999
  second <- glcdp_full_import_execute(
    new_glc_full_import_task_payload(fixture$source, review),
    api = api,
    execution_mode = "fixture"
  )

  expect_equal(calls$open, 2L)
  expect_equal(calls$read, 2L)
  expect_equal(calls$collect, 2L)
  expect_identical(calls$read_args$files, c("a.csv", "b.csv"))
  expect_identical(
    calls$read_args$file_group,
    c("dataset-a:1", "dataset-b:1")
  )
  expect_identical(calls$read_args$variables, "MEDI")
  expect_null(calls$read_args$terms)
  expect_false(calls$read_args$primary_only)
  expect_identical(calls$read_args$n_max, Inf)
  expect_identical(calls$read_args$problems, "error")
  expect_false(calls$read_args$progress)
  expect_identical(calls$collect_args$standardize, "lightlogr")
  expect_s3_class(first, "llw_glc_full_import_result")
  expect_identical(first$data, full_data)
  expect_identical(second$data, full_data)
  expect_identical(
    first$transfer_review_fingerprint,
    second$transfer_review_fingerprint
  )
  expect_false("preview_result" %in% names(payload))
  expect_identical(first$actual$row_count, 4L)
  expect_identical(first$actual$input_count, 2L)
  expect_identical(first$actual$file_group_count, 2L)
  expect_identical(first$actual$factor_evidence$name, "EVENT")
  expect_identical(
    first$actual$factor_evidence$levels[[1L]],
    c("0", "1", "4", "5")
  )
  expect_true(first$assurance$fresh_full_read)
  expect_false(first$assurance$preview_result_used)
  expect_true(first$assurance$complete_collected_handoff)
  expect_true(first$assurance$ephemeral)
  expect_false(first$assurance$persisted)
  expect_false(first$assurance$promoted)
  expect_false(first$assurance$integrated)
  expect_false(first$assurance$downloaded)
  expect_true(first$assurance$deterministic_fixture)
  expect_true(first$assurance$no_live_measurement_access)
  expect_null(nonserializable_path(first))
  expect_false(glc_value_contains_path(first, fixture$source$cache_dir))
  expect_type(serialize(first, NULL), "raw")
})

test_that("full Apply enforces full-set resources and rejects tampered handoffs", {
  fixture <- glc_adapter_projection_fixture()
  projection <- fixture$projection
  projection$files$declared_bytes <- c(60, 60)
  projection$files$bytes_known <- TRUE
  projection$byte_evidence$known_file_count <- 2L
  projection$byte_evidence$unknown_file_count <- 0L
  projection$byte_evidence$known_declared_bytes <- 120
  projection$byte_evidence$declared_bytes_complete <- TRUE
  projection$byte_evidence$declared_total_bytes <- 120
  review <- new_glc_transfer_review(
    projection,
    total_preview_rows = 1L,
    available_session_bytes = 100
  )
  expect_false(review$resource$blocked)
  expect_true(review$resource$full_import_blocked)
  expect_identical(review$resource$known_allocated_bytes, 60)
  expect_identical(review$resource$known_selected_bytes, 120)

  fake <- fake_glcdp_api()
  expect_error(
    glcdp_full_import_execute(
      new_glc_full_import_task_payload(fixture$source, review),
      api = fake$api,
      execution_mode = "fixture"
    ),
    class = "llw_resource_error",
    regexp = "full selected file set"
  )
  expect_null(fake$calls$opened_ref)
  expect_null(fake$calls$read)

  open_review <- new_glc_transfer_review(
    fixture$projection,
    total_preview_rows = 1L,
    available_session_bytes = 1000
  )
  collection <- data.frame(
    dataset_id = open_review$files$dataset_id,
    file_group_id = open_review$files$file_group_id,
    files = I(lapply(
      open_review$files$declared_path,
      function(path) file.path("deterministic-fixture", path)
    )),
    runtime_input_ids = I(list(
      "fixture://full-input/001",
      "fixture://full-input/002"
    )),
    data = I(list(
      data.frame(MEDI = 1),
      data.frame(MEDI = 2)
    )),
    stringsAsFactors = FALSE
  )
  result <- new_glc_full_import_result(
    open_review,
    collection,
    data.frame(
      MEDI = c(1, 2),
      Datetime = as.POSIXct(
        c("2026-08-01 12:00:00", "2026-08-01 12:00:01"),
        tz = "UTC"
      )
    ),
    execution_mode = "fixture"
  )
  tampered <- result
  tampered$assurance$preview_result_used <- TRUE
  tampered$fingerprint <- NULL
  tampered$fingerprint <- glc_transfer_value_fingerprint(tampered)
  expect_error(
    validate_glc_full_import_result(tampered, open_review),
    class = "llw_validation_error",
    regexp = "assurance"
  )
})

test_that("bounded transfer review enforces one authoritative row ceiling", {
  fixture <- glc_adapter_projection_fixture()
  expect_identical(glc_transfer_preview_row_limit(), 10000L)

  at_limit <- new_glc_transfer_review(
    fixture$projection,
    total_preview_rows = 10000L,
    available_session_bytes = Inf
  )
  expect_identical(at_limit$allocation$total_budget, 10000L)
  expect_identical(at_limit$allocation$maximum_rows, 10000)
  expect_identical(at_limit$limits$total_preview_rows, 10000L)

  invalid_values <- list(
    NULL,
    numeric(),
    NA_real_,
    NaN,
    Inf,
    -Inf,
    0,
    -1,
    1.5,
    "20"
  )
  for (value in invalid_values) {
    error <- tryCatch(
      new_glc_transfer_review(
        fixture$projection,
        total_preview_rows = value,
        available_session_bytes = Inf
      ),
      error = identity
    )
    expect_s3_class(error, "llw_validation_error")
    expect_match(
      llw_public_message(error),
      "1 through 10,000",
      fixed = TRUE
    )
  }

  for (value in c(10001, 1e9)) {
    error <- tryCatch(
      new_glc_transfer_review(
        fixture$projection,
        total_preview_rows = value,
        available_session_bytes = Inf
      ),
      error = identity
    )
    expect_s3_class(error, "llw_validation_error")
    expect_match(llw_public_message(error), "10,000", fixed = TRUE)
    expect_false(grepl(
      "Slice 2C-A",
      llw_public_message(error),
      fixed = TRUE
    ))
  }

  tampered <- at_limit
  tampered$allocation$total_budget <- 10001L
  tampered$allocation$maximum_rows <- 10001
  tampered$fingerprint <- glc_transfer_value_fingerprint(
    within(tampered, rm(fingerprint))
  )
  fake <- fake_glcdp_api()
  payload <- list(
    source = fixture$source,
    transfer_review = tampered,
    progress_path = tempfile("glc-preview-limit-", fileext = ".rds")
  )
  expect_error(
    glcdp_transfer_preview_execute(
      payload,
      api = fake$api,
      execution_mode = "fixture"
    ),
    class = "llw_validation_error"
  )
  expect_null(fake$calls$opened_ref)
  expect_null(fake$calls$read)
})

test_that("bounded preview uses only reviewed public read and collect inputs", {
  fixture <- glc_adapter_projection_fixture()
  review <- new_glc_transfer_review(
    fixture$projection,
    total_preview_rows = 4L,
    problems = "warn",
    available_session_bytes = 1000
  )
  fake <- fake_glcdp_api()
  fake$api$glc_read <- function(...) {
    fake$calls$read <- list(...)
    allocated <- review$allocated_files
    data.frame(
      dataset_id = allocated$dataset_id,
      file_group_id = allocated$file_group_id,
      files = I(lapply(
        allocated$declared_path,
        function(path) file.path(fixture$source$cache_dir, path)
      )),
      runtime_input_ids = I(lapply(
        seq_len(nrow(allocated)),
        function(index) sprintf("fixture://adapter-input/%03d", index)
      )),
      data = I(list(
        data.frame(MEDI = c(10, 11), stringsAsFactors = FALSE),
        data.frame(MEDI = c(12, 13), stringsAsFactors = FALSE)
      )),
      stringsAsFactors = FALSE
    )
  }
  fake$api$glc_collect <- function(x, standardize) {
    fake$calls$collect <- list(x = x, standardize = standardize)
    data.frame(
      MEDI = c(10, 11, 12, 13),
      Datetime = as.POSIXct(
        "2026-08-01 12:00:00",
        tz = "UTC"
      ) +
        0:3,
      dataset_id = rep(c("dataset-a", "dataset-b"), each = 2L),
      file_group_id = rep(c("dataset-a:1", "dataset-b:1"), each = 2L),
      stringsAsFactors = FALSE
    )
  }
  payload <- new_glc_transfer_task_payload(
    fixture$source,
    review,
    progress_path = tempfile("glc-preview-progress-", fileext = ".rds")
  )
  result <- glcdp_transfer_preview_execute(
    payload,
    api = fake$api,
    execution_mode = "fixture"
  )

  expect_s3_class(result, "llw_glc_transfer_preview_result")
  expect_identical(fake$calls$opened_ref, fixture$source$resolved_revision)
  expect_identical(fake$calls$read$dataset_id, c("dataset-a", "dataset-b"))
  expect_identical(
    fake$calls$read$file_group,
    c("dataset-a:1", "dataset-b:1")
  )
  expect_identical(fake$calls$read$files, c("a.csv", "b.csv"))
  expect_identical(fake$calls$read$variables, "MEDI")
  expect_null(fake$calls$read$terms)
  expect_false(fake$calls$read$primary_only)
  expect_identical(fake$calls$read$n_max, 2)
  expect_identical(fake$calls$read$problems, "warn")
  expect_false(fake$calls$read$progress)
  expect_identical(fake$calls$collect$standardize, "lightlogr")
  expect_identical(result$reviewed$file_ids, c("a.csv", "b.csv"))
  expect_identical(
    result$reviewed$file_membership,
    review$allocated_files[,
      c(
        "declared_path",
        "dataset_id",
        "file_group_id"
      ),
      drop = FALSE
    ]
  )
  expect_identical(
    result$actual$input_ids,
    c("fixture://adapter-input/001", "fixture://adapter-input/002")
  )
  expect_identical(result$actual$row_count, 4L)
  expect_identical(result$reviewed$file_count, 2L)
  expect_identical(result$actual$input_count, 2L)
  expect_identical(result$actual$file_group_count, 2L)
  expect_identical(result$disclosure$maximum_possible_rows, 4)
  expect_true(result$disclosure$preview_is_not_dataset)
  expect_false(result$disclosure$unread_files_runtime_validated)
  expect_true(result$assurance$bounded_glc_read)
  expect_true(result$assurance$bounded_glc_collect)
  expect_true(result$assurance$deterministic_fixture)
  expect_false(result$assurance$measurement_contents_accessed)
  expect_true(result$assurance$no_live_measurement_access)
  expect_false(result$assurance$selected_package_files_opened)
  expect_false(result$reviewed$selected_package_paths_opened)
  expect_false(result$assurance$imported)
  expect_false(result$assurance$persisted)
  expect_null(nonserializable_path(result))
  expect_false(glc_value_contains_path(result, fixture$source$cache_dir))
  expect_type(serialize(result, NULL), "raw")

  contradictory_inputs <- result
  contradictory_inputs$actual$input_ids <- result$reviewed$file_ids
  contradictory_inputs$actual$input_membership$input_id <-
    result$reviewed$file_ids
  contradictory_inputs$fingerprint <- NULL
  contradictory_inputs$fingerprint <- glc_transfer_value_fingerprint(
    contradictory_inputs
  )
  expect_error(
    validate_glc_transfer_preview_result(
      contradictory_inputs,
      review
    ),
    class = "llw_validation_error",
    regexp = "provenance"
  )

  contradictory_assurance <- result
  contradictory_assurance$assurance$no_live_measurement_access <- FALSE
  contradictory_assurance$fingerprint <- NULL
  contradictory_assurance$fingerprint <- glc_transfer_value_fingerprint(
    contradictory_assurance
  )
  expect_error(
    validate_glc_transfer_preview_result(
      contradictory_assurance,
      review
    ),
    class = "llw_validation_error",
    regexp = "assurance"
  )

  subclassed_projection <- fixture$projection
  class(subclassed_projection$files) <- c(
    "fixture_table",
    "data.frame"
  )
  subclassed_review <- new_glc_transfer_review(
    subclassed_projection,
    total_preview_rows = 4L,
    problems = "warn",
    available_session_bytes = 1000
  )
  expect_identical(class(subclassed_review$files), "data.frame")
  expect_identical(class(subclassed_review$allocated_files), "data.frame")
  transported_review <- unserialize(serialize(subclassed_review, NULL))
  expect_identical(
    transported_review$fingerprint,
    subclassed_review$fingerprint
  )
  validate_glc_transfer_review(transported_review)
  subclassed_result <- glcdp_transfer_preview_execute(
    new_glc_transfer_task_payload(fixture$source, transported_review),
    api = fake$api,
    execution_mode = "fixture"
  )
  expect_s3_class(subclassed_result, "llw_glc_transfer_preview_result")
  expect_identical(
    subclassed_result$transfer_review_fingerprint,
    subclassed_review$fingerprint
  )

  expect_error(
    glcdp_transfer_preview_execute(
      payload,
      execution_mode = "fixture"
    ),
    class = "llw_validation_error",
    regexp = "explicitly injected API"
  )

  mismatched <- fake_glcdp_api()
  mismatched$api$glc_read <- fake$api$glc_read
  mismatched$api$glc_collect <- fake$api$glc_collect
  original_read <- mismatched$api$glc_read
  mismatched$api$glc_read <- function(...) {
    value <- original_read(...)
    value$file_group_id <- rev(value$file_group_id)
    value
  }
  expect_error(
    glcdp_transfer_preview_execute(
      payload,
      api = mismatched$api,
      execution_mode = "fixture"
    ),
    class = "llw_validation_error",
    regexp = "file-to-dataset and file-group membership"
  )

  incomplete <- fake_glcdp_api()
  incomplete$api$glc_read <- fake$api$glc_read
  incomplete$api$glc_collect <- function(x, standardize) {
    data.frame(MEDI = c(10, 11, 12, 13))
  }
  expect_error(
    glcdp_transfer_preview_execute(
      payload,
      api = incomplete$api,
      execution_mode = "fixture"
    ),
    class = "llw_validation_error",
    regexp = "missing expected result column"
  )

  blocked <- new_glc_transfer_review(
    fixture$projection,
    total_preview_rows = 1L,
    available_session_bytes = 99
  )
  expect_error(
    glcdp_transfer_preview_execute(
      new_glc_transfer_task_payload(fixture$source, blocked),
      api = fake$api,
      execution_mode = "fixture"
    ),
    class = "llw_resource_error",
    regexp = "resource budget"
  )
})

test_that("public collection refinement is exact, serializable, and zero access", {
  fake <- fake_glcdp_api()
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("d", 40L),
    validation_state = "passed",
    attestation_verified = TRUE,
    schema_version = "3.0.2",
    glcdp_version = "1.0.0.9000",
    cache_dir = tempfile("glc-plan-cache-"),
    registry_generated_at = "2026-08-01T00:00:00Z"
  )
  request <- new_glc_planning_request(term = "melanopic_edi")
  plan <- glc_adapter_plan_fixture(source, request)
  compatibility_id <- plan$compatibility_sets$compatibility_id[[1L]]
  file_group_ids <- plan$compatibility_sets$file_group_ids[[1L]]
  fake$api$glc_collection_refine <- function(...) {
    fake$calls$refine <- list(...)
    glc_adapter_refinement_fixture(
      plan,
      compatibility_id,
      file_group_ids
    )
  }

  refinement <- glcdp_refine_collection(
    plan,
    compatibility_id,
    file_group_ids,
    api = fake$api
  )

  expect_s3_class(refinement, "glc_collection_refinement")
  expect_identical(fake$calls$refine$plan, plan)
  expect_identical(fake$calls$refine$file_group, file_group_ids)
  expect_identical(fake$calls$refine$compatibility_id, compatibility_id)
  expect_false(refinement$assurance$package_reopened)
  expect_false(refinement$assurance$network_access)
  expect_false(refinement$assurance$measurement_contents_transferred)
  expect_null(nonserializable_path(refinement))
  expect_type(serialize(refinement, NULL), "raw")
})

test_that("installed planner API matches the exact reviewed public contract", {
  skip_if_not_installed("glcdp", minimum_version = "1.0.0.9000")
  expect_identical(
    names(formals(glcdp::glc_collection_plan)),
    c(
      "x",
      "terms",
      "variable_scope",
      "variables",
      "dataset_id",
      "file_group",
      "standardize"
    )
  )
  expect_identical(
    names(formals(glcdp::glc_collection_refine)),
    c("plan", "file_group", "compatibility_id")
  )
  provenance <- glcdp_runtime_provenance()
  expect_identical(provenance$version, "1.0.0.9000")
  expect_identical(
    provenance$revision,
    "e49fcf153a114916ce2680f8b40eb475e694e10d"
  )
})
