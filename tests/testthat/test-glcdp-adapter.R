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
        metadata_fingerprint = "fixture-fingerprint",
        planner_schema = "glc-collection-plan",
        planner_version = "1.2.0"
      ),
      request = list(
        terms = request$term %||% character(),
        variable_scope = request$variable_scope,
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
  expect_identical(fake$calls$plan$variable_scope, "matched")
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
  expect_identical(combined$variable_scope, "matched")
  expect_error(
    new_glc_planning_request(modality = "light"),
    class = "llw_validation_error",
    regexp = "stable file-group"
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
