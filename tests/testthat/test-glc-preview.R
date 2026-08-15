glc_preview_test_selection <- function(
  repository = "fixture/validated",
  revision = strrep("a", 40L)
) {
  structure(
    list(
      registry_url = "fixture://registry",
      registry_generated_at = "2026-08-01T00:00:00Z",
      registry_id = sub(".*/", "", repository),
      repository = repository,
      requested_revision = "latest_pass",
      resolved_revision = revision,
      has_latest_pass = TRUE,
      current_validation_status = "pass",
      current_revision = revision,
      is_current_pass = TRUE,
      latest_pass_validated_at = "2026-08-01T00:00:00Z",
      attestation_verified = TRUE,
      glcdp_version = "1.0.0.9000",
      glcdp_revision = "e49fcf153a114916ce2680f8b40eb475e694e10d"
    ),
    class = c("llw_glc_registry_selection", "list")
  )
}

glc_preview_test_request <- function(
  selection,
  cache_dir = tempfile("glc-cache-")
) {
  new_glc_inspection_request(
    registry_url = selection$registry_url,
    repository = selection$repository,
    resolved_revision = selection$resolved_revision,
    validation_state = "passed",
    attestation_verified = selection$attestation_verified,
    glcdp_version = selection$glcdp_version,
    cache_dir = cache_dir,
    registry_generated_at = selection$registry_generated_at
  )
}

glc_preview_test_inspection <- function(
  selection = glc_preview_test_selection(),
  cache_dir = tempfile("glc-cache-")
) {
  request <- glc_preview_test_request(selection, cache_dir)
  summary <- data.frame(
    schema_version = "3.0.2",
    dataset_count = 2L,
    participant_count = 2L,
    file_count = 3L,
    declared_bytes = 450,
    stringsAsFactors = FALSE
  )
  datasets <- data.frame(
    dataset_id = c("dataset-a", "dataset-b"),
    schema_version = "3.0.2",
    study_id = "study-a",
    participant_id = c("participant-a", "participant-b"),
    timezone = "UTC",
    file_group_count = c(2L, 1L),
    file_count = c(2L, 1L),
    stringsAsFactors = FALSE
  )
  datasets$modalities <- rep(list("light"), 2L)
  datasets$device_ids <- list("device-a", "device-b")
  datasets$primary_variables <- rep(list("MEDI"), 2L)

  files <- data.frame(
    dataset_id = c("dataset-a", "dataset-a", "dataset-b"),
    file_group_id = c("dataset-a:1", "dataset-a:2", "dataset-b:1"),
    path = c("a-1.csv", "a-2.csv", "b-1.csv"),
    encoding = "UTF-8",
    timezone = "UTC",
    role = "measurement",
    data_state = "raw",
    device_id = c("device-a", "device-a", "device-b"),
    temporal_type = "interval",
    temporal_value = 1,
    temporal_unit = "minute",
    storage = "git",
    expected_bytes = c(100, 150, NA_real_),
    available = TRUE,
    stringsAsFactors = FALSE
  )
  files$modalities <- list("light", "activity", "light")

  variables <- data.frame(
    dataset_id = c(
      "dataset-a",
      "dataset-a",
      "dataset-a",
      "dataset-b",
      "dataset-b"
    ),
    file_group_id = c(
      "dataset-a:1",
      "dataset-a:2",
      "dataset-a:2",
      "dataset-b:1",
      "dataset-b:1"
    ),
    name = c("MEDI", "MEDI", "Datetime", "melEDI", "Datetime"),
    label = c(
      "Melanopic EDI",
      "Melanopic EDI",
      "Date and time",
      "Melanopic EDI",
      "Date and time"
    ),
    description = c(
      rep("Melanopic equivalent daylight illuminance.", 2L),
      "Measurement timestamp.",
      "Melanopic equivalent daylight illuminance.",
      "Measurement timestamp."
    ),
    unit = c("lx", "lx", NA_character_, "lx", NA_character_),
    type = c("number", "number", "datetime", "number", "datetime"),
    term = c(
      "melanopic_edi",
      "melanopic_edi",
      "datetime",
      "melanopic_edi",
      "datetime"
    ),
    term_name = c(
      "Melanopic EDI",
      "Melanopic EDI",
      "Date and time",
      "Melanopic EDI",
      "Date and time"
    ),
    primary = c(TRUE, TRUE, FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  variables$factor_values <- rep(list(character()), 5L)
  variables$factor_labels <- rep(list(character()), 5L)
  variables$factor_descriptions <- rep(list(character()), 5L)

  resources <- data.frame(
    resource = c("study", "participants"),
    path = c("metadata/study.json", "metadata/participants.csv"),
    core = TRUE,
    format = c("json", "csv"),
    media_type = c("application/json", "text/csv"),
    stringsAsFactors = FALSE
  )
  metadata <- list(
    study = data.frame(
      study_id = "study-a",
      title = "Fixture study",
      stringsAsFactors = FALSE
    ),
    participants = data.frame(
      participant_id = c("participant-a", "participant-b"),
      age = c(31, NA_real_),
      sex = c("female", NA_character_),
      stringsAsFactors = FALSE
    )
  )
  new_glc_inspection(
    source = new_glc_inspection_source(request, "3.0.2"),
    summary = summary,
    datasets = datasets,
    files = files,
    variables = variables,
    resources = resources,
    metadata = metadata,
    warnings = "One measurement file size is unavailable."
  )
}

glc_preview_test_modality_inspection <- function(
  selection = glc_preview_test_selection(),
  cache_dir = tempfile("glc-cache-")
) {
  inspection <- glc_preview_test_inspection(selection, cache_dir)
  variables <- inspection$variables
  context_row <- variables$file_group_id == "dataset-a:2" &
    variables$name == "MEDI"
  variables$term[context_row] <- "activity_context"
  variables$term_name[context_row] <- "Activity context"
  variables$label[context_row] <- "Activity context"
  new_glc_inspection(
    source = inspection$source,
    summary = inspection$summary,
    datasets = inspection$datasets,
    files = inspection$files,
    variables = variables,
    resources = inspection$resources,
    metadata = inspection$metadata,
    warnings = inspection$warnings
  )
}

glc_preview_empty_table <- function(columns, rows = 0L) {
  values <- stats::setNames(
    replicate(length(columns), rep(NA_character_, rows), simplify = FALSE),
    columns
  )
  as.data.frame(values, stringsAsFactors = FALSE)
}

glc_preview_test_plan <- function(
  inspection = glc_preview_test_inspection(),
  request = new_glc_planning_request(term = "melanopic_edi"),
  two_units = TRUE
) {
  request <- validate_glc_planning_request(request)
  term <- request$term %||% "melanopic_edi"
  unit_ids <- paste0("glcu_", c(strrep("1", 64L), strrep("2", 64L)))
  compatibility_ids <- paste0("glcc_", c(strrep("a", 64L), strrep("b", 64L)))
  if (!two_units) {
    unit_ids <- unit_ids[[1L]]
    compatibility_ids <- compatibility_ids[[1L]]
  }
  contracts <- glc_collection_plan_required_tables()
  units <- glc_preview_empty_table(contracts$units, length(unit_ids))
  units$unit_id <- unit_ids
  units$compatibility_id <- compatibility_ids
  units$preferred <- seq_along(unit_ids) == 1L
  units$dataset_count <- c(2L, rep(1L, length(unit_ids) - 1L))
  units$file_group_count <- c(2L, rep(1L, length(unit_ids) - 1L))
  units$variable_count <- c(2L, rep(1L, length(unit_ids) - 1L))
  units$file_count <- c(2L, rep(1L, length(unit_ids) - 1L))
  units$declared_bytes <- c(100, rep(150, length(unit_ids) - 1L))
  units$known_file_count <- c(1L, rep(1L, length(unit_ids) - 1L))
  units$unknown_file_count <- c(1L, rep(0L, length(unit_ids) - 1L))
  units$declared_bytes_complete <- c(FALSE, rep(TRUE, length(unit_ids) - 1L))
  units$harmonization_required <- FALSE
  units$harmonized_variables <- rep(list(character()), length(unit_ids))
  units$diagnostic_ids <- rep(list(character()), length(unit_ids))
  units$file_group_ids <- if (two_units) {
    list(c("dataset-a:1", "dataset-b:1"), "dataset-a:2")
  } else {
    list(c("dataset-a:1", "dataset-b:1"))
  }

  groups <- glc_preview_empty_table(contracts$groups, 3L)
  groups$status <- c(
    "included",
    "included",
    if (two_units) "included" else "excluded"
  )
  groups$unit_id <- c(
    unit_ids[[1L]],
    unit_ids[[1L]],
    if (two_units) unit_ids[[2L]] else NA_character_
  )
  groups$compatibility_id <- c(
    compatibility_ids[[1L]],
    compatibility_ids[[1L]],
    if (two_units) compatibility_ids[[2L]] else NA_character_
  )
  groups$dataset_id <- c("dataset-a", "dataset-b", "dataset-a")
  groups$file_group <- c(1L, 1L, 2L)
  groups$file_group_id <- c("dataset-a:1", "dataset-b:1", "dataset-a:2")
  groups$study_id <- "study-a"
  groups$participant_id <- c("participant-a", "participant-b", "participant-a")
  groups$participant_associated <- TRUE
  groups$study_link_status <- "resolved"
  groups$participant_link_status <- "resolved"
  groups$device_id <- c("device-a", "device-b", NA_character_)
  groups$device_link_status <- c("resolved", "resolved", "not_applicable")
  groups$device_location <- c("wrist", "chest", NA_character_)
  groups$device_location_type <- c("body_worn", "body_worn", NA_character_)
  groups$description <- c(
    "Wrist light recording",
    "Chest light recording",
    "Activity questionnaire"
  )
  groups$format <- "csv"
  groups$timezone <- "UTC"
  groups$role <- "measurement"
  groups$data_state <- "raw"
  groups$modalities <- list("light", "light", "activity")
  groups$modality_other <- NA_character_
  groups$modality_other_type <- NA_character_
  groups$selected_variables <- rep(list("MEDI"), 3L)
  groups$reason_codes <- list(
    "included",
    "included",
    if (two_units) "included" else "term_missing"
  )
  groups$messages <- list(
    "Included.",
    "Included.",
    if (two_units) "Included." else "The requested term is absent."
  )

  variables <- glc_preview_empty_table(contracts$variables, 3L)
  variables$unit_id <- c(
    unit_ids[[1L]],
    unit_ids[[1L]],
    if (two_units) unit_ids[[2L]] else unit_ids[[1L]]
  )
  variables$dataset_id <- c("dataset-a", "dataset-b", "dataset-a")
  variables$file_group_id <- c("dataset-a:1", "dataset-b:1", "dataset-a:2")
  variables$position <- 1L
  variables$name <- c("MEDI", "melEDI", "MEDI")
  variables$label <- "Melanopic EDI"
  variables$description <- "Melanopic equivalent daylight illuminance."
  variables$unit <- "lx"
  variables$type <- "number"
  variables$term <- term
  variables$term_name <- "Melanopic EDI"
  variables$primary <- TRUE
  variables$factor_values <- rep(list(character()), 3L)
  variables$factor_labels <- rep(list(character()), 3L)
  variables$factor_descriptions <- rep(list(character()), 3L)
  variables$selection_origin <- "matched"

  read_columns <- glc_preview_empty_table(contracts$read_columns, 3L)
  read_columns$unit_id <- variables$unit_id
  read_columns$dataset_id <- variables$dataset_id
  read_columns$file_group_id <- variables$file_group_id
  read_columns$position <- 1L
  read_columns$name <- variables$name
  read_columns$declared_type <- "number"
  read_columns$origin <- "selected"
  read_columns$selected_for_output <- TRUE
  read_columns$automatic <- FALSE
  read_columns$message <- "Selected by canonical term."
  output_columns <- glc_preview_empty_table(
    contracts$output_columns,
    length(unit_ids)
  )
  output_columns$unit_id <- unit_ids
  output_columns$position <- 1L
  output_columns$name <- "MEDI"
  output_columns$source_declared_type <- "number"
  output_columns$expected_type <- "numeric"
  output_columns$origin <- "selected"
  output_columns$automatic <- FALSE
  output_columns$runtime_validation_required <- TRUE
  output_columns$collision_validation_required <- TRUE
  output_columns$message <- "Validated after read."
  files <- glc_preview_empty_table(contracts$files, 3L)
  files$status <- groups$status
  files$unit_id <- groups$unit_id
  files$dataset_id <- groups$dataset_id
  files$file_group_id <- groups$file_group_id
  files$position <- 1L
  files$declared_path <- c("a-1.csv", "b-1.csv", "a-2.csv")
  files$format <- "csv"
  files$encoding <- "UTF-8"
  files$declared_bytes <- c(100, NA_real_, 150)
  files$bytes_known <- c(TRUE, FALSE, TRUE)
  compatibility <- glc_preview_empty_table(
    contracts$compatibility,
    length(unit_ids)
  )
  compatibility$unit_id <- unit_ids
  compatibility$timezone <- "UTC"
  compatibility$role <- "measurement"
  compatibility$data_state <- "raw"
  compatibility$standardize <- "lightlogr"
  compatibility$selected_names <- rep(list("MEDI"), length(unit_ids))
  compatibility$declared_types <- rep(list("number"), length(unit_ids))
  compatibility$factor_values <- rep(list(character()), length(unit_ids))
  compatibility$factor_labels <- rep(list(character()), length(unit_ids))
  compatibility$factor_descriptions <- rep(list(character()), length(unit_ids))
  compatibility$harmonization_required <- FALSE
  compatibility$harmonized_variables <- rep(list(character()), length(unit_ids))
  compatibility$diagnostic_ids <- rep(list(character()), length(unit_ids))
  compatibility$modalities <- rep(list("light"), length(unit_ids))
  compatibility$standardize_affects_partition <- FALSE
  extensions <- glc_preview_empty_table(contracts$extensions, 3L)
  extensions$dataset_id <- groups$dataset_id
  extensions$file_group_id <- groups$file_group_id
  extensions$metadata <- rep(list(list()), nrow(extensions))

  sets <- glc_preview_empty_table(
    contracts$compatibility_sets,
    length(compatibility_ids)
  )
  sets$compatibility_id <- compatibility_ids
  sets$dataset_count <- c(2L, rep(1L, length(compatibility_ids) - 1L))
  sets$file_group_count <- c(2L, rep(1L, length(compatibility_ids) - 1L))
  sets$variable_count <- c(2L, rep(1L, length(compatibility_ids) - 1L))
  sets$file_count <- c(2L, rep(1L, length(compatibility_ids) - 1L))
  sets$declared_bytes <- c(100, rep(150, length(compatibility_ids) - 1L))
  sets$known_file_count <- c(1L, rep(1L, length(compatibility_ids) - 1L))
  sets$unknown_file_count <- c(1L, rep(0L, length(compatibility_ids) - 1L))
  sets$declared_bytes_complete <- c(
    FALSE,
    rep(TRUE, length(compatibility_ids) - 1L)
  )
  sets$harmonization_required <- FALSE
  sets$harmonized_variables <- rep(list(character()), length(compatibility_ids))
  sets$diagnostic_ids <- rep(list(character()), length(compatibility_ids))
  sets$final_unit_count <- 1L
  sets$final_selection_required <- FALSE
  sets$constraint_codes <- rep(list(character()), length(compatibility_ids))
  sets$constraint_messages <- rep(list(character()), length(compatibility_ids))
  sets$file_group_ids <- if (two_units) {
    list(c("dataset-a:1", "dataset-b:1"), "dataset-a:2")
  } else {
    list(c("dataset-a:1", "dataset-b:1"))
  }
  sets$final_unit_ids <- lapply(unit_ids, identity)
  sets$selected_names <- rep(list("MEDI"), length(compatibility_ids))
  sets$declared_types <- rep(list("number"), length(compatibility_ids))
  sets$factor_values <- rep(list(character()), length(compatibility_ids))
  sets$factor_labels <- rep(list(character()), length(compatibility_ids))
  sets$factor_descriptions <- rep(list(character()), length(compatibility_ids))
  sets$timezone <- "UTC"
  sets$modalities <- if (two_units) list("light", "activity") else list("light")
  sets$role <- "measurement"
  sets$data_state <- "raw"
  sets$standardize_affects_structure <- FALSE

  compatibility_diagnostics <- glc_preview_empty_table(
    contracts$compatibility_diagnostics
  )
  compatibility_diagnostic_groups <- glc_preview_empty_table(
    contracts$compatibility_diagnostic_groups
  )

  metadata <- c(
    list(schema = "glc-package-metadata", version = "1.0.0"),
    lapply(glc_collection_plan_metadata_tables(), glc_preview_empty_table)
  )
  metadata$participants <- glc_preview_empty_table(
    glc_collection_plan_metadata_tables()$participants,
    2L
  )
  metadata$participants$participant_id <- c("participant-a", "participant-b")
  metadata$participants$age <- c(31, 44)
  metadata$participants$sex <- c("female", "male")
  metadata$participants$gender <- c("woman", "man")
  metadata$devices <- glc_preview_empty_table(
    glc_collection_plan_metadata_tables()$devices,
    2L
  )
  metadata$devices$device_id <- c("device-a", "device-b")
  metadata$devices$schema_version <- "3.0.2"
  metadata$devices$manufacturer <- c("Fixture", "Fixture")
  metadata$devices$model <- c("WristOne", "ChestOne")
  metadata$instruments <- glc_preview_empty_table(
    glc_collection_plan_metadata_tables()$instruments,
    2L
  )
  metadata$instruments$dataset_id <- c("dataset-a", "dataset-b")
  metadata$instruments$file_group_id <- c("dataset-a:1", "dataset-b:1")
  metadata$instruments$instrument_type <- "device"
  metadata$instruments$instrument_name <- c(
    "Fixture WristOne",
    "Fixture ChestOne"
  )
  metadata$instruments$collection_method <- "wearable measurement"
  metadata$instruments$recorded_by <- "device"
  metadata$instruments$software_name <- NA_character_
  metadata$studies <- glc_preview_empty_table(
    glc_collection_plan_metadata_tables()$studies,
    1L
  )
  metadata$studies$study_id <- "study-a"
  metadata$studies$schema_version <- "3.0.2"
  metadata$studies$title <- "Fixture study"
  metadata$studies$geographical_location <- "Munich"
  metadata$participant_characteristics <- glc_preview_empty_table(
    glc_collection_plan_metadata_tables()$participant_characteristics,
    4L
  )
  metadata$participant_characteristics$participant_id <- rep(
    c("participant-a", "participant-b"),
    2L
  )
  metadata$participant_characteristics$participant_link_status <- "resolved"
  metadata$participant_characteristics$characteristic_position <- rep(
    1:2,
    each = 2L
  )
  metadata$participant_characteristics$value_position <- 1L
  metadata$participant_characteristics$name <- rep(
    c("chronotype", "response score"),
    each = 2L
  )
  metadata$participant_characteristics$value <- list("early", "late", 3, 8)
  metadata$participant_characteristics$value_type <- rep(
    c("character", "double"),
    each = 2L
  )
  metadata$participant_characteristics$unit <- c("", "", "points", "points")
  metadata$file_group_variables <- glc_preview_empty_table(
    glc_collection_plan_metadata_tables()$file_group_variables,
    6L
  )
  metadata$file_group_variables$dataset_id <- rep(
    c("dataset-a", "dataset-b", "dataset-a"),
    each = 2L
  )
  metadata$file_group_variables$file_group_id <- rep(
    c("dataset-a:1", "dataset-b:1", "dataset-a:2"),
    each = 2L
  )
  metadata$file_group_variables$declaration_position <- rep(1:2, 3L)
  metadata$file_group_variables$selected_by_request <- rep(c(TRUE, FALSE), 3L)
  metadata$file_group_variables$selected_for_output <- TRUE
  metadata$file_group_variables$selection_origin <- rep(
    c("matched", "required"),
    3L
  )
  metadata$file_group_variables$name <- rep(c("MEDI", "Datetime"), 3L)
  metadata$file_group_variables$label <- rep(
    c("Melanopic EDI", "Date and time"),
    3L
  )
  metadata$file_group_variables$description <- rep(
    c("Melanopic equivalent daylight illuminance.", "Measurement timestamp."),
    3L
  )
  metadata$file_group_variables$unit <- rep(c("lx", NA_character_), 3L)
  metadata$file_group_variables$type <- rep(c("number", "datetime"), 3L)
  metadata$file_group_variables$term <- rep(c(term, "datetime"), 3L)
  metadata$file_group_variables$term_name <- rep(
    c("Melanopic EDI", "Date and time"),
    3L
  )
  metadata$file_group_variables$primary <- rep(c(TRUE, FALSE), 3L)
  metadata$file_group_variables$factor_level_count <- 0L

  structure(
    list(
      plan_schema = "glc-collection-plan",
      plan_version = "1.2.0",
      provenance = list(
        repository = inspection$source$repository,
        source_revision = inspection$source$resolved_revision,
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
        measurement_contents_transferred = FALSE,
        measurement_contents_inspected = FALSE,
        interactive_filtering_network_access = FALSE,
        refinement_network_access = FALSE
      ),
      preferred_unit_id = unit_ids[[1L]],
      units = units,
      groups = groups,
      variables = variables,
      read_columns = read_columns,
      output_columns = output_columns,
      files = files,
      compatibility = compatibility,
      extensions = extensions,
      compatibility_sets = sets,
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

glc_preview_test_factor_diagnostic <- function(plan, applies = TRUE) {
  contracts <- glc_collection_plan_required_tables()
  diagnostic_id <- paste0("glcd_", strrep(if (applies) "c" else "d", 64L))
  compatibility_id <- plan$compatibility_sets$compatibility_id[[1L]]
  unit_id <- plan$compatibility_sets$final_unit_ids[[1L]][[1L]]
  group_ids <- plan$compatibility_sets$file_group_ids[[1L]]

  diagnostics <- glc_preview_empty_table(
    contracts$compatibility_diagnostics,
    1L
  )
  diagnostics$diagnostic_id <- diagnostic_id
  diagnostics$selection_scope <- if (applies) "selected" else "not_selected"
  diagnostics$classification <- "safely_harmonizable"
  diagnostics$code <- "factor_level_union"
  diagnostics$variable_name <- "EVENT"
  diagnostics$applies_to_current_plan <- applies
  diagnostics$prospective_scopes <- list(if (applies) character() else "all")
  diagnostics$message <- if (applies) {
    paste(
      "EVENT has compatible factor declarations. Levels will be harmonized",
      "to: 0, 1, 2, 3, 4."
    )
  } else {
    paste(
      "EVENT is not selected. Its compatible factor declarations would be",
      "harmonized if all variables are included later."
    )
  }
  diagnostics$affected_group_count <- length(group_ids)
  diagnostics$affected_structure_count <- 1L
  diagnostics$affected_unit_count <- 1L
  diagnostics$file_group_ids <- list(group_ids)
  diagnostics$compatibility_ids <- list(compatibility_id)
  diagnostics$unit_ids <- list(unit_id)
  diagnostics$union_values <- list(c("0", "1", "2", "3", "4"))
  diagnostics$union_labels <- list(character())
  diagnostics$union_descriptions <- list(character())

  diagnostic_groups <- glc_preview_empty_table(
    contracts$compatibility_diagnostic_groups,
    length(group_ids)
  )
  group_rows <- plan$groups[match(group_ids, plan$groups$file_group_id), ]
  diagnostic_groups$diagnostic_id <- diagnostic_id
  diagnostic_groups$dataset_id <- group_rows$dataset_id
  diagnostic_groups$file_group_id <- group_ids
  diagnostic_groups$current_status <- group_rows$status
  diagnostic_groups$compatibility_id <- compatibility_id
  diagnostic_groups$unit_id <- unit_id
  diagnostic_groups$variable_present <- TRUE
  diagnostic_groups$selected_by_request <- applies
  diagnostic_groups$declaration_position <- 2L
  diagnostic_groups$declared_type <- "factor"
  diagnostic_groups$factor_values <- list(
    c("0", "1", "2", "3"),
    c("0", "1", "2", "3", "4")
  )
  diagnostic_groups$factor_labels <- rep(list(character()), length(group_ids))
  diagnostic_groups$factor_descriptions <- rep(
    list(character()),
    length(group_ids)
  )

  plan$compatibility_diagnostics <- diagnostics
  plan$compatibility_diagnostic_groups <- diagnostic_groups
  plan$compatibility_sets$diagnostic_ids[[1L]] <- diagnostic_id
  plan$units$diagnostic_ids[[1L]] <- diagnostic_id
  plan$compatibility$diagnostic_ids[[1L]] <- diagnostic_id
  if (applies) {
    plan$compatibility_sets$harmonization_required[[1L]] <- TRUE
    plan$compatibility_sets$harmonized_variables[[1L]] <- "EVENT"
    plan$units$harmonization_required[[1L]] <- TRUE
    plan$units$harmonized_variables[[1L]] <- "EVENT"
    plan$compatibility$harmonization_required[[1L]] <- TRUE
    plan$compatibility$harmonized_variables[[1L]] <- "EVENT"
  }
  plan
}

glc_preview_test_refinement <- function(
  plan,
  compatibility_id,
  file_group_ids
) {
  groups <- plan$groups[
    plan$groups$file_group_id %in% file_group_ids,
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
      constraints = glc_preview_empty_table(
        glc_collection_refinement_required_tables()$constraints
      )
    ),
    class = c("glc_collection_refinement", "list")
  )
}

glc_preview_test_runtime <- function() {
  new_session_runtime(
    resolve_runtime_profile(
      "hosted",
      workers = 0L,
      is_interactive = FALSE,
      available_cores = 2L
    ),
    session = NULL
  )
}

test_that("Slice 2A UI is a metadata-only five-step wizard", {
  html <- htmltools::renderTags(glcPreviewUI("glc"))$html

  expect_match(html, "Choose source", fixed = TRUE)
  expect_match(html, "Inspect package", fixed = TRUE)
  expect_match(html, "Inventory &amp; discovery")
  expect_match(html, "Choose structure", fixed = TRUE)
  expect_match(html, "Narrow with metadata", fixed = TRUE)
  expect_match(html, "core-metadata transfer size", fixed = TRUE)
  expect_match(html, "Metadata transfer size unavailable", fixed = TRUE)
  expect_match(html, ">Inspect package<", fixed = TRUE)
  expect_match(html, "Collect compatible groups", fixed = TRUE)
  expect_match(html, "modality and/or term", fixed = TRUE)
  expect_match(html, "Metadata-only through Slice 2A", fixed = TRUE)
  expect_false(grepl("confirm_inspection", html, fixed = TRUE))
  expect_false(grepl("Exact inspection request", html, fixed = TRUE))
  expect_false(grepl("inspection_status", html, fixed = TRUE))
  expect_false(grepl("plan_status", html, fixed = TRUE))
  expect_false(grepl("Preview bounded rows", html, fixed = TRUE))
  expect_false(grepl("Read and collect", html, fixed = TRUE))
  expect_false(grepl("declaration", html, ignore.case = TRUE))
  expect_false(grepl(
    ".llw-glc-source > .card:last-child",
    html,
    fixed = TRUE
  ))
  expect_equal(length(glc_wizard_steps()$value), 5L)
  expect_lt(
    regexpr("glc-plan_summary", html, fixed = TRUE)[[1L]],
    regexpr("glc-continue_structure_button", html, fixed = TRUE)[[1L]]
  )
})

test_that("inventory overview is explained and delegates full exploration", {
  inspection <- glc_preview_test_inspection()
  html <- htmltools::renderTags(glc_inventory_detail_ui(inspection))$html
  metrics <- htmltools::renderTags(glc_inventory_metrics_ui(inspection))$html

  expect_match(html, "Explore the complete package separately", fixed = TRUE)
  expect_match(html, "compatible package parts", fixed = TRUE)
  expect_match(
    html,
    "https://tscnlab-glc-dp-r.share.connect.posit.cloud",
    fixed = TRUE
  )
  expect_false(grepl("table-responsive", html, fixed = TRUE))
  expect_match(metrics, "Participants", fixed = TRUE)
  expect_match(metrics, "Modalities", fixed = TRUE)
  expect_match(metrics, "title=", fixed = TRUE)
  expect_equal(
    stringr::str_count(metrics, 'class="llw-wizard-metric"'),
    8L
  )
  wizard_html <- htmltools::renderTags(glcPreviewUI("glc"))$html
  expect_match(
    wizard_html,
    "repeat(2, minmax(0, 1fr)) !important",
    fixed = TRUE
  )
  expect_match(wizard_html, "word-break: normal", fixed = TRUE)
  expect_match(
    wizard_html,
    ".llw-glc-unit-selector .radio {position: relative; display: flex;",
    fixed = TRUE
  )
  expect_match(
    wizard_html,
    ".llw-glc-unit-selector .radio > label > span {display: block;",
    fixed = TRUE
  )
  expect_match(
    wizard_html,
    "box-sizing: border-box; width: 100%;",
    fixed = TRUE
  )
})

test_that("term search uses human labels, identifiers, and descriptions", {
  inspection <- glc_preview_test_inspection()
  terms <- inspection$terms

  expect_identical(terms$term, c("datetime", "melanopic_edi"))
  expect_identical(
    terms$label[terms$term == "melanopic_edi"],
    "Melanopic EDI"
  )
  expect_identical(
    glc_filter_terms(terms, "daylight")$term,
    "melanopic_edi"
  )
  expect_identical(
    glc_filter_terms(terms, "Datetime")$term,
    "datetime"
  )
  expect_identical(
    glc_filter_terms(
      glc_preview_test_modality_inspection()$terms,
      "activity context"
    )$term,
    "activity_context"
  )
  labels <- names(glc_term_choice_labels(terms))
  expect_true(any(grepl("Melanopic EDI — melanopic_edi", labels, fixed = TRUE)))
  expect_identical(
    glc_term_choice_labels(terms[0, , drop = FALSE]),
    stats::setNames(character(), character())
  )

  modalities <- glc_modality_catalogue(inspection$files)
  expect_identical(modalities$modality, c("activity", "light"))
  expect_identical(
    glc_modality_file_groups(inspection$files, "light"),
    c("dataset-a:1", "dataset-b:1")
  )

  scoped <- glc_preview_test_modality_inspection()
  expect_true(
    "activity_context" %in% glc_terms_for_modality(scoped, "activity")$term
  )
  expect_false(
    "activity_context" %in% glc_terms_for_modality(scoped, "light")$term
  )
})

test_that("progress UI exposes real phase completion accessibly", {
  progress <- data.frame(
    phase = c("open", "inventory"),
    label = c("Opening exact revision", "Loading public inventories"),
    status = c("complete", "running"),
    elapsed_seconds = c(0.2, NA_real_),
    detail = c("Opened.", "Loading."),
    stringsAsFactors = FALSE
  )
  html <- htmltools::renderTags(glc_task_progress_ui(
    progress,
    total_phases = 6L,
    heading = "Exact-package inspection",
    idle_message = "Idle."
  ))$html

  expect_match(html, "role=\"progressbar\"", fixed = TRUE)
  expect_match(html, "aria-valuenow=\"25\"", fixed = TRUE)
  expect_match(html, "Opening exact revision", fixed = TRUE)
  expect_match(html, "Loading public inventories", fixed = TRUE)
  expect_match(html, "Running", fixed = TRUE)

  active_html <- htmltools::renderTags(glc_task_progress_ui(
    NULL,
    total_phases = 6L,
    heading = "Exact-package inspection",
    idle_message = "Idle.",
    active = TRUE,
    active_label = "Preparing exact-revision inspection",
    active_detail = "Starting the background worker."
  ))$html
  expect_match(active_html, "role=\"progressbar\"", fixed = TRUE)
  expect_match(
    active_html,
    "Preparing exact-revision inspection",
    fixed = TRUE
  )
  expect_false(grepl("Idle.", active_html, fixed = TRUE))

  cancelled_html <- htmltools::renderTags(glc_task_progress_ui(
    progress,
    total_phases = 6L,
    heading = "Exact-package inspection",
    idle_message = "Idle.",
    terminal_state = "cancelled",
    terminal_message = "Cancellation was requested."
  ))$html
  expect_match(cancelled_html, "Cancelled after 1 of 6 phases", fixed = TRUE)
  expect_match(cancelled_html, ">Cancelled<", fixed = TRUE)
  expect_false(grepl(">Running<", cancelled_html, fixed = TRUE))
  expect_false(grepl("progress-bar-animated", cancelled_html, fixed = TRUE))

  cancelled_before_phase_html <- htmltools::renderTags(glc_task_progress_ui(
    NULL,
    total_phases = 3L,
    heading = "Structural compatibility planning",
    idle_message = "Idle.",
    terminal_state = "cancelled",
    terminal_message = "Cancellation was requested."
  ))$html
  expect_match(
    cancelled_before_phase_html,
    "Structural compatibility planning cancelled",
    fixed = TRUE
  )
  expect_false(grepl(">Running<", cancelled_before_phase_html, fixed = TRUE))

  completed_progress <- data.frame(
    phase = paste0("phase-", seq_len(3L)),
    label = paste("Phase", seq_len(3L)),
    status = "complete",
    elapsed_seconds = 0.1,
    detail = "Complete.",
    stringsAsFactors = FALSE
  )
  completed_html <- htmltools::renderTags(glc_task_progress_ui(
    completed_progress,
    total_phases = 3L,
    heading = "Structural compatibility planning",
    idle_message = "Idle."
  ))$html
  expect_match(
    completed_html,
    "Structural compatibility planning ready",
    fixed = TRUE
  )
  expect_match(
    completed_html,
    "All 3 phases completed successfully",
    fixed = TRUE
  )
  expect_false(grepl("role=\"progressbar\"", completed_html, fixed = TRUE))
  expect_false(grepl("list-group-numbered", completed_html, fixed = TRUE))

  enabled_cancel <- htmltools::renderTags(
    glc_cancel_task_button(identity, "cancel", TRUE, "Cancel task")
  )$html
  disabled_cancel <- htmltools::renderTags(
    glc_cancel_task_button(identity, "cancel", FALSE, "Cancel task")
  )$html
  expect_false(grepl("disabled=", enabled_cancel, fixed = TRUE))
  expect_match(disabled_cancel, "disabled=\"disabled\"", fixed = TRUE)
})

test_that("inspection retries wait for complete controller release", {
  expect_false(glc_inspection_controller_released(
    "cancelled",
    "running",
    1L
  ))
  expect_false(glc_inspection_controller_released(
    "cancelled",
    "error",
    1L
  ))
  expect_true(glc_inspection_controller_released(
    "cancelled",
    "error",
    0L
  ))
  expect_true(glc_inspection_controller_released(
    "complete",
    "success",
    0L
  ))
  expect_true(glc_inspection_controller_released(
    "idle",
    "initial",
    0L
  ))

  attempt <- list(
    context_revision = 4L,
    source_key = paste0("fixture/validated@", strrep("a", 40L)),
    requested_at = as.POSIXct("2026-08-14 12:00:00", tz = "UTC")
  )
  expect_true(glc_inspection_attempt_is_current(
    attempt,
    4L,
    attempt$source_key
  ))
  expect_false(glc_inspection_attempt_is_current(
    attempt,
    5L,
    attempt$source_key
  ))
  expect_false(glc_inspection_attempt_is_current(
    attempt,
    4L,
    paste0("fixture/other@", strrep("b", 40L))
  ))
  expect_false(glc_inspection_attempt_expired(
    attempt,
    now = as.POSIXct("2026-08-14 12:00:14", tz = "UTC")
  ))
  expect_true(glc_inspection_attempt_expired(
    attempt,
    now = as.POSIXct("2026-08-14 12:00:15", tz = "UTC")
  ))

  planning_attempt <- c(
    attempt,
    list(
      request = new_glc_planning_request(
        term = "melanopic_edi",
        modality = "light",
        file_group_ids = "dataset-a:1"
      )
    )
  )
  expect_true(glc_planning_attempt_is_current(
    planning_attempt,
    4L,
    attempt$source_key,
    "melanopic_edi",
    "light"
  ))
  expect_false(glc_planning_attempt_is_current(
    planning_attempt,
    5L,
    attempt$source_key,
    "melanopic_edi",
    "light"
  ))
  expect_false(glc_planning_attempt_is_current(
    planning_attempt,
    4L,
    attempt$source_key,
    NULL,
    "light"
  ))
})

test_that("reader copy pluralizes counts and exposes complete variable lists", {
  expect_identical(
    glc_pluralized_count(0L, "linked participant"),
    "0 linked participants"
  )
  expect_identical(
    glc_pluralized_count(1L, "linked participant"),
    "1 linked participant"
  )
  expect_identical(
    glc_pluralized_count(17L, "linked participant"),
    "17 linked participants"
  )

  variables <- paste0("Variable ", seq_len(25L))
  html <- htmltools::renderTags(glc_full_variable_list_tag(variables))$html
  expect_match(html, "Show all variables", fixed = TRUE)
  expect_match(html, "Variable 1", fixed = TRUE)
  expect_match(html, "Variable 25", fixed = TRUE)
  expect_equal(stringr::str_count(html, "<li>"), 25L)
})

test_that("source continuation and deep-link arrival start package inspection", {
  make_worker <- function(calls) {
    function(payload, spec) {
      calls$count <- calls$count + 1L
      glc_preview_test_inspection(
        glc_preview_test_selection(
          repository = payload$request$repository,
          revision = payload$request$resolved_revision
        ),
        cache_dir = payload$request$cache_dir
      )
    }
  }

  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = make_worker(calls)
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      expect_equal(calls$count, 0L)
      session$setInputs(continue_inspect = 1L)
      flush_m1_promises(session)
      expect_equal(calls$count, 1L)
      expect_s3_class(returned$inspection(), "llw_glc_inspection")
      expect_equal(returned$inspection_controller_revision(), 0L)
    }
  )

  deep_runtime <- glc_preview_test_runtime()
  on.exit(deep_runtime$cleanup(), add = TRUE)
  deep_source <- shiny::reactiveVal(glc_preview_test_selection())
  auto <- shiny::reactiveVal(TRUE)
  deep_calls <- new.env(parent = emptyenv())
  deep_calls$count <- 0L
  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = deep_source,
      runtime = deep_runtime,
      cache_dir = deep_runtime$paths$cache,
      auto_inspect = auto,
      inspection_worker = make_worker(deep_calls)
    ),
    {
      returned <- session$getReturned()
      flush_m1_promises(session)
      expect_equal(deep_calls$count, 1L)
      expect_s3_class(returned$inspection(), "llw_glc_inspection")

      auto(FALSE)
      session$flushReact()
      expect_null(returned$inspection())
      auto(TRUE)
      flush_m1_promises(session)
      expect_equal(deep_calls$count, 2L)
      expect_s3_class(returned$inspection(), "llw_glc_inspection")
      expect_equal(returned$inspection_controller_revision(), 1L)
    }
  )
})

test_that("cancelled inspection attempts use fresh controllers on retry", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$count <- 0L
  inspection_worker <- function(payload, spec) {
    calls$count <- calls$count + 1L
    glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker
    ),
    {
      returned <- session$getReturned()
      session$flushReact()

      session$setInputs(inspect = 1L)
      expect_equal(calls$count, 1L)
      session$setInputs(cancel_inspection = 1L)
      expect_identical(returned$inspection_status()$state, "cancelled")
      flush_m1_promises(session)
      expect_equal(runtime$active_count(), 0L)

      session$setInputs(inspect = 2L)
      flush_m1_promises(session)
      expect_equal(calls$count, 2L)
      expect_equal(returned$inspection_controller_revision(), 1L)
      expect_false(returned$inspection_pending())
      expect_identical(returned$inspection_status()$state, "complete")

      session$setInputs(inspect = 3L)
      flush_m1_promises(session)
      expect_equal(calls$count, 3L)
      expect_equal(returned$inspection_controller_revision(), 2L)
      expect_identical(returned$inspection_status()$state, "complete")

      source(glc_preview_test_selection(
        repository = "fixture/second",
        revision = strrep("b", 40L)
      ))
      session$flushReact()
      expect_null(returned$inspection())
      expect_identical(returned$inspection_status()$state, "idle")

      session$setInputs(inspect = 4L)
      flush_m1_promises(session)
      expect_equal(calls$count, 4L)
      expect_equal(returned$inspection_controller_revision(), 3L)
      expect_identical(returned$inspection_status()$state, "complete")
      expect_identical(
        returned$inspection()$source$repository,
        "fixture/second"
      )
    }
  )
})

test_that("cancelled planning attempts use fresh controllers on retry", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$planning <- 0L

  inspection_worker <- function(payload, spec) {
    glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }
  planning_worker <- function(payload, spec) {
    calls$planning <- calls$planning + 1L
    inspection <- glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$source$repository,
        revision = payload$source$resolved_revision
      ),
      cache_dir = payload$source$cache_dir
    )
    glc_preview_test_plan(inspection, payload$request)
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1L)
      flush_m1_promises(session)

      session$setInputs(modality_id = "light")
      session$setInputs(term_id = "melanopic_edi")
      session$flushReact()
      expect_identical(returned$selected_modality(), "light")
      expect_identical(returned$selected_term(), "melanopic_edi")

      session$setInputs(plan = 1L)
      expect_equal(calls$planning, 1L)
      session$setInputs(cancel_plan = 1L)
      expect_identical(returned$plan_status()$state, "cancelled")
      expect_identical(returned$selected_term(), "melanopic_edi")
      expect_identical(returned$selected_modality(), "light")
      flush_m1_promises(session)
      expect_equal(runtime$active_count(), 0L)

      # A renderUI remount transiently removes the browser input. That is not
      # an intentional user clear and must not cancel the fresh retry.
      session$setInputs(term_id = NULL, modality_id = NULL)
      session$flushReact()
      expect_identical(returned$selected_term(), "melanopic_edi")
      expect_identical(returned$selected_modality(), "light")

      session$setInputs(plan = 2L)
      flush_m1_promises(session)
      expect_equal(calls$planning, 2L)
      expect_equal(returned$planning_controller_revision(), 1L)
      expect_false(returned$planning_pending())
      expect_identical(returned$plan_status()$state, "complete")
      expect_s3_class(returned$plan(), "glc_collection_plan")

      session$setInputs(term_id = "")
      session$flushReact()
      expect_null(returned$selected_term())
      expect_identical(returned$selected_modality(), "light")
      session$setInputs(plan = 3L)
      expect_equal(calls$planning, 3L)
      session$setInputs(cancel_plan = 2L)
      expect_identical(returned$plan_status()$state, "cancelled")
      flush_m1_promises(session)

      session$setInputs(plan = 4L)
      flush_m1_promises(session)
      expect_equal(calls$planning, 4L)
      expect_equal(returned$planning_controller_revision(), 3L)
      expect_null(returned$planning_request()$term)
      expect_identical(returned$planning_request()$modality, "light")
      expect_identical(returned$plan_status()$state, "complete")

      session$setInputs(modality_id = "")
      session$setInputs(term_id = "melanopic_edi")
      session$flushReact()
      session$setInputs(plan = 5L)
      flush_m1_promises(session)
      expect_equal(calls$planning, 5L)
      expect_identical(returned$planning_request()$term, "melanopic_edi")
      expect_null(returned$planning_request()$modality)
      expect_identical(returned$plan_status()$state, "complete")
    }
  )
})

test_that("planning cancellation is isolated across Shiny sessions", {
  runtime_a <- glc_preview_test_runtime()
  runtime_b <- glc_preview_test_runtime()
  on.exit(runtime_a$cleanup(), add = TRUE)
  on.exit(runtime_b$cleanup(), add = TRUE)
  source_a <- shiny::reactiveVal(glc_preview_test_selection())
  source_b <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$a <- 0L
  calls$b <- 0L

  inspection_worker <- function(payload, spec) {
    glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }
  planning_worker <- function(counter) {
    force(counter)
    function(payload, spec) {
      calls[[counter]] <- calls[[counter]] + 1L
      inspection <- glc_preview_test_inspection(
        glc_preview_test_selection(
          repository = payload$source$repository,
          revision = payload$source$resolved_revision
        ),
        cache_dir = payload$source$cache_dir
      )
      glc_preview_test_plan(inspection, payload$request)
    }
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source_a,
      runtime = runtime_a,
      cache_dir = runtime_a$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker("a")
    ),
    {
      returned_a <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1L)
      flush_m1_promises(session)
      session$setInputs(term_id = "melanopic_edi")
      session$setInputs(plan = 1L)
      session$setInputs(cancel_plan = 1L)
      expect_identical(returned_a$plan_status()$state, "cancelled")
      expect_equal(calls$a, 1L)
      expect_identical(returned_a$plan_status()$state, "cancelled")
      expect_identical(returned_a$selected_term(), "melanopic_edi")
    }
  )

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source_b,
      runtime = runtime_b,
      cache_dir = runtime_b$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker("b")
    ),
    {
      returned_b <- session$getReturned()
      session$flushReact()
      expect_identical(returned_b$plan_status()$state, "idle")
      session$setInputs(inspect = 1L)
      flush_m1_promises(session)
      session$setInputs(term_id = "melanopic_edi")
      session$setInputs(plan = 1L)
      flush_m1_promises(session)
      expect_equal(calls$b, 1L)
      expect_identical(returned_b$plan_status()$state, "complete")
      expect_s3_class(returned_b$plan(), "glc_collection_plan")
    }
  )
})

test_that("queued planning retry waits for session capacity release", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$planning <- 0L
  inspection_worker <- function(payload, spec) {
    glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }
  planning_worker <- function(payload, spec) {
    calls$planning <- calls$planning + 1L
    inspection <- glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$source$repository,
        revision = payload$source$resolved_revision
      ),
      cache_dir = payload$source$cache_dir
    )
    glc_preview_test_plan(inspection, payload$request)
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1L)
      flush_m1_promises(session)
      session$setInputs(term_id = "melanopic_edi")
      session$flushReact()

      expect_identical(
        runtime$submit("test-capacity-blocker", function() invisible(NULL)),
        "running"
      )
      session$setInputs(plan = 1L)
      expect_equal(calls$planning, 0L)
      expect_identical(returned$plan_status()$state, "queued")
      session$setInputs(term_id = NULL)
      session$flushReact()
      expect_identical(returned$selected_term(), "melanopic_edi")
      expect_true(returned$planning_pending())
      session$setInputs(cancel_plan = 1L)
      expect_identical(returned$plan_status()$state, "cancelled")

      session$setInputs(plan = 2L)
      expect_true(returned$planning_pending())
      expect_equal(calls$planning, 0L)
      runtime$release("test-capacity-blocker")
      session$elapse(150)
      flush_m1_promises(session)
      expect_false(returned$planning_pending())
      expect_equal(calls$planning, 1L)
      expect_identical(returned$plan_status()$state, "complete")
      expect_equal(returned$planning_controller_revision(), 1L)
    }
  )
})

test_that("inspection and planning require separate explicit actions", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$inspection <- 0L
  calls$planning <- 0L
  calls$refinement <- 0L
  calls$fail_inspection <- FALSE

  inspection_worker <- function(payload, spec) {
    calls$inspection <- calls$inspection + 1L
    calls$inspection_payload <- payload
    if (calls$fail_inspection) {
      stop("private fixture inspection failure", call. = FALSE)
    }
    glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }
  planning_worker <- function(payload, spec) {
    calls$planning <- calls$planning + 1L
    calls$planning_payload <- payload
    inspection <- glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$source$repository,
        revision = payload$source$resolved_revision
      ),
      cache_dir = payload$source$cache_dir
    )
    glc_preview_test_plan(inspection, payload$request)
  }
  refine_collection <- function(plan, compatibility_id, file_group_ids) {
    calls$refinement <- calls$refinement + 1L
    calls$refinement_compatibility <- compatibility_id
    calls$refinement_groups <- file_group_ids
    glc_preview_test_refinement(plan, compatibility_id, file_group_ids)
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker,
      refine_collection = refine_collection
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      expect_equal(calls$inspection, 0L)
      expect_equal(calls$planning, 0L)
      expect_null(returned$inspection())
      expect_null(returned$plan())

      session$setInputs(inspect = 1)
      flush_m1_promises(session)
      expect_equal(calls$inspection, 1L)
      expect_s3_class(returned$inspection(), "llw_glc_inspection")
      expect_equal(calls$planning, 0L)
      expect_null(returned$plan())

      session$setInputs(term_id = "melanopic_edi")
      session$flushReact()
      expect_identical(returned$selected_term(), "melanopic_edi")
      expect_null(returned$selected_modality())
      expect_equal(calls$planning, 0L)

      session$setInputs(plan = 1)
      flush_m1_promises(session)
      expect_equal(calls$planning, 1L)
      expect_s3_class(returned$plan(), "glc_collection_plan")
      expected_compatibility <- paste0("glcc_", strrep("a", 64L))
      second_compatibility <- paste0("glcc_", strrep("b", 64L))
      expect_identical(
        returned$selected_compatibility_id(),
        expected_compatibility
      )
      expect_identical(
        calls$planning_payload$source$resolved_revision,
        strrep("a", 40L)
      )
      expect_identical(calls$planning_payload$request$variable_scope, "matched")

      filter_revision <- returned$metadata_filter_revision()
      position_input <- glc_metadata_filter_input_id(
        "filter_position",
        filter_revision
      )
      do.call(
        session$setInputs,
        stats::setNames(list("chest"), position_input)
      )
      session$flushReact()
      expect_identical(returned$filtered_file_group_ids(), "dataset-b:1")
      expect_equal(returned$active_metadata_filter_count(), 1L)

      selector_html <- htmltools::renderTags(output$compatibility_selector)$html
      expect_match(selector_html, "type=\"radio\"", fixed = TRUE)
      expect_match(selector_html, "Fixture WristOne", fixed = TRUE)
      session$setInputs(compatibility_id = second_compatibility)
      session$flushReact()
      expect_identical(
        returned$selected_compatibility_id(),
        second_compatibility
      )
      expect_gt(returned$metadata_filter_revision(), filter_revision)
      expect_equal(returned$active_metadata_filter_count(), 0L)
      selected_status_html <- htmltools::renderTags(
        output$selected_structure_status
      )$html
      expect_match(selected_status_html, "Structure 2 selected", fixed = TRUE)
      expect_identical(returned$filtered_file_group_ids(), "dataset-a:2")
      filter_feedback_html <- htmltools::renderTags(output$filter_feedback)$html
      expect_match(
        filter_feedback_html,
        "No metadata filter is active",
        fixed = TRUE
      )
      expect_equal(calls$refinement, 0L)
      session$setInputs(refine = 1)
      session$flushReact()
      expect_equal(calls$refinement, 1L)
      expect_identical(calls$refinement_compatibility, second_compatibility)
      expect_identical(calls$refinement_groups, "dataset-a:2")
      expect_s3_class(returned$refinement(), "glc_collection_refinement")

      second_revision <- returned$metadata_filter_revision()
      session$setInputs(compatibility_id = expected_compatibility)
      session$flushReact()
      expect_gt(returned$metadata_filter_revision(), second_revision)
      expect_equal(returned$active_metadata_filter_count(), 0L)
      expect_identical(
        sort(returned$filtered_file_group_ids()),
        c("dataset-a:1", "dataset-b:1")
      )
      expect_null(returned$refinement())

      calls$fail_inspection <- TRUE
      session$setInputs(inspect = 2)
      expect_warning(
        flush_m1_promises(session),
        "private fixture inspection failure"
      )
      expect_equal(calls$inspection, 2L)
      expect_identical(returned$inspection_status()$state, "error")
      expect_s3_class(returned$inspection(), "llw_glc_inspection")
    }
  )
})

test_that("source, term, and modality changes reset dependent plans", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$planning <- 0L

  inspection_worker <- function(payload, spec) {
    glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }
  planning_worker <- function(payload, spec) {
    calls$planning <- calls$planning + 1L
    inspection <- glc_preview_test_inspection(
      glc_preview_test_selection(
        repository = payload$source$repository,
        revision = payload$source$resolved_revision
      ),
      cache_dir = payload$source$cache_dir
    )
    glc_preview_test_plan(inspection, payload$request)
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1)
      flush_m1_promises(session)
      session$setInputs(term_id = "melanopic_edi")
      session$flushReact()
      session$setInputs(plan = 1)
      flush_m1_promises(session)
      expect_s3_class(returned$plan(), "glc_collection_plan")
      planned_filter_revision <- returned$metadata_filter_revision()

      session$setInputs(term_id = "datetime")
      session$flushReact()
      expect_null(returned$plan())
      expect_identical(returned$selected_term(), "datetime")
      expect_equal(calls$planning, 1L)
      expect_gt(returned$metadata_filter_revision(), planned_filter_revision)

      term_filter_revision <- returned$metadata_filter_revision()
      session$setInputs(modality_id = "light")
      session$flushReact()
      expect_null(returned$plan())
      expect_identical(returned$selected_modality(), "light")
      expect_gt(returned$metadata_filter_revision(), term_filter_revision)
      session$setInputs(plan = 2)
      flush_m1_promises(session)
      expect_identical(
        returned$planning_request()$file_group_ids,
        c("dataset-a:1", "dataset-b:1")
      )

      replanned_filter_revision <- returned$metadata_filter_revision()
      source(glc_preview_test_selection(
        repository = "fixture/second",
        revision = strrep("b", 40L)
      ))
      session$flushReact()
      expect_null(returned$inspection())
      expect_null(returned$plan())
      expect_null(returned$selected_term())
      expect_identical(returned$inspection_status()$state, "idle")
      expect_identical(returned$plan_status()$state, "idle")
      expect_gt(
        returned$metadata_filter_revision(),
        replanned_filter_revision
      )

      session$setInputs(inspect = 2)
      flush_m1_promises(session)
      expect_s3_class(returned$inspection(), "llw_glc_inspection")
    }
  )
})

test_that("a modality scopes canonical terms and clears an incompatible term", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  inspection_worker <- function(payload, spec) {
    glc_preview_test_modality_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1L)
      flush_m1_promises(session)

      session$setInputs(term_id = "activity_context")
      session$flushReact()
      expect_identical(returned$selected_term(), "activity_context")

      session$setInputs(modality_id = "light")
      session$flushReact()
      expect_identical(returned$selected_modality(), "light")
      expect_null(returned$selected_term())
      light_html <- htmltools::renderTags(output$term_selector)$html
      expect_false(grepl("activity_context", light_html, fixed = TRUE))

      session$setInputs(modality_id = "activity")
      session$flushReact()
      activity_html <- htmltools::renderTags(output$term_selector)$html
      expect_match(activity_html, "activity_context", fixed = TRUE)
      feedback_html <- htmltools::renderTags(output$discovery_feedback)$html
      expect_match(feedback_html, "within modality activity", fixed = TRUE)

      session$setInputs(term_search = "no matching term")
      session$flushReact()
      empty_html <- htmltools::renderTags(output$term_selector)$html
      expect_match(empty_html, "No term filter", fixed = TRUE)
      expect_false(grepl("Error:", empty_html, fixed = TRUE))
      empty_feedback <- htmltools::renderTags(output$discovery_feedback)$html
      expect_match(empty_feedback, "0 of", fixed = TRUE)
    }
  )
})

test_that("structural sets stay stable while typed metadata narrows membership", {
  inspection <- glc_preview_test_inspection()
  plan <- glc_preview_test_plan(inspection)
  source <- glc_inspection_source_spec(inspection, tempfile("glc-cache-"))
  request <- new_glc_planning_request(term = "melanopic_edi")
  first_id <- paste0("glcc_", strrep("a", 64L))
  second_id <- paste0("glcc_", strrep("b", 64L))

  expect_no_error(validate_glc_collection_plan(plan, source, request))
  expect_identical(glc_plan_compatibility_default(plan), first_id)
  expect_identical(
    glc_compatibility_groups(plan, first_id)$file_group_id,
    c("dataset-a:1", "dataset-b:1")
  )

  context <- glc_compatibility_context(plan, first_id)
  expect_identical(context$positions, c("chest", "wrist"))
  expect_identical(
    context$descriptions,
    c(
      "Chest light recording",
      "Wrist light recording"
    )
  )
  expect_identical(
    context$instrument_names,
    c("Fixture ChestOne", "Fixture WristOne")
  )
  card_html <- htmltools::renderTags(
    glc_compatibility_choice_tag(context, 1L, broadest = TRUE)
  )$html
  expect_match(card_html, "Instrument", fixed = TRUE)
  expect_match(card_html, "Fixture WristOne", fixed = TRUE)
  expect_match(card_html, "1 other variable available", fixed = TRUE)
  expect_match(card_html, "Broadest coverage", fixed = TRUE)
  expect_match(card_html, ">Modalities<", fixed = TRUE)
  expect_match(card_html, ">light<", fixed = TRUE)

  active_factor_plan <- glc_preview_test_factor_diagnostic(plan, applies = TRUE)
  expect_no_error(validate_glc_collection_plan(
    active_factor_plan,
    source,
    request
  ))
  active_summary <- htmltools::renderTags(
    glc_compatibility_summary_ui(active_factor_plan, modality = "light")
  )$html
  active_card <- htmltools::renderTags(glc_compatibility_choice_tag(
    glc_compatibility_context(active_factor_plan, first_id),
    1L,
    broadest = TRUE
  ))$html
  expect_match(active_summary, "Current compatibility handling", fixed = TRUE)
  expect_match(active_summary, "EVENT", fixed = TRUE)
  expect_match(active_summary, "0, 1, 2, 3, 4", fixed = TRUE)
  expect_match(
    active_card,
    "Compatibility and harmonization details",
    fixed = TRUE
  )
  expect_match(active_card, "EVENT", fixed = TRUE)

  prospective_factor_plan <- glc_preview_test_factor_diagnostic(
    plan,
    applies = FALSE
  )
  expect_no_error(validate_glc_collection_plan(
    prospective_factor_plan,
    source,
    request
  ))
  prospective_summary <- htmltools::renderTags(
    glc_compatibility_summary_ui(prospective_factor_plan, modality = "light")
  )$html
  expect_match(
    prospective_summary,
    "If more variables are included later",
    fixed = TRUE
  )
  expect_match(prospective_summary, "EVENT is not selected", fixed = TRUE)

  detail_html <- htmltools::renderTags(
    glc_aggregated_structure_detail_ui(context)
  )$html
  expect_match(detail_html, "2 participants", fixed = TRUE)
  expect_match(detail_html, "2 devices", fixed = TRUE)
  expect_match(detail_html, "2 files", fixed = TRUE)
  expect_match(detail_html, "Date and time", fixed = TRUE)
  expect_equal(stringr::str_count(detail_html, "<li>"), 2L)

  expect_identical(
    glc_filter_compatibility_groups(
      plan,
      first_id,
      list(positions = "wrist")
    ),
    "dataset-a:1"
  )
  expect_identical(
    glc_filter_compatibility_groups(
      plan,
      first_id,
      list(sexes = "male", positions = "chest")
    ),
    "dataset-b:1"
  )
  expect_identical(
    glc_filter_compatibility_groups(
      plan,
      first_id,
      list(age_range = c(30, 35))
    ),
    "dataset-a:1"
  )
  expect_identical(
    glc_filter_compatibility_groups(
      plan,
      first_id,
      list(device_models = paste("Fixture", "WristOne", sep = "\r"))
    ),
    "dataset-a:1"
  )
  expect_identical(
    glc_filter_compatibility_groups(
      plan,
      first_id,
      list(
        characteristic_name = "response score",
        characteristic_range = c(0, 5)
      )
    ),
    "dataset-a:1"
  )
  expect_identical(
    glc_filter_compatibility_groups(
      plan,
      first_id,
      list(
        characteristic_name = "chronotype",
        characteristic_values = "late"
      )
    ),
    "dataset-b:1"
  )
  expect_identical(
    glc_filter_compatibility_groups(
      plan,
      second_id,
      list(device_relation = "without")
    ),
    "dataset-a:2"
  )
  no_device_filters <- htmltools::renderTags(glc_metadata_filter_inputs(
    identity,
    glc_metadata_filter_catalogue(plan, second_id)
  ))$html
  expect_false(grepl("Device relationship", no_device_filters, fixed = TRUE))
  expect_false(grepl("Device filters", no_device_filters, fixed = TRUE))
  expect_false(grepl(
    "Wearing or device location",
    no_device_filters,
    fixed = TRUE
  ))
  expect_false(grepl("Device model", no_device_filters, fixed = TRUE))

  device_filters <- htmltools::renderTags(glc_metadata_filter_inputs(
    identity,
    glc_metadata_filter_catalogue(plan, first_id)
  ))$html
  expect_match(device_filters, "Participant age range", fixed = TRUE)
  expect_false(grepl("llw-glc-histogram", device_filters, fixed = TRUE))
  expect_match(device_filters, "Characteristic", fixed = TRUE)
  expect_false(grepl("Study location", device_filters, fixed = TRUE))
  expect_match(device_filters, "Device filters", fixed = TRUE)
  expect_match(device_filters, "Device model", fixed = TRUE)

  catalogue <- glc_metadata_filter_catalogue(plan, first_id)
  numeric_spec <- glc_characteristic_filter_spec(
    catalogue$characteristics,
    "response score"
  )
  expect_identical(numeric_spec$type, "numeric")
  expect_equal(
    unname(c(numeric_spec$slider$min, numeric_spec$slider$max)),
    c(3, 8)
  )
  full_numeric_range <- glc_metadata_filters_from_input(
    list(
      filter_characteristic_name = "response score",
      filter_characteristic_values = c(3, 8)
    ),
    catalogue
  )
  expect_length(full_numeric_range$characteristic_range, 0L)
  expect_equal(glc_active_metadata_filter_count(full_numeric_range), 0L)
  narrowed_numeric_range <- glc_metadata_filters_from_input(
    list(
      filter_characteristic_name = "response score",
      filter_characteristic_values = c(3, 5)
    ),
    catalogue
  )
  expect_equal(narrowed_numeric_range$characteristic_range, c(3, 5))
  expect_equal(glc_active_metadata_filter_count(narrowed_numeric_range), 1L)
  expect_identical(
    glc_filter_compatibility_groups(plan, first_id, narrowed_numeric_range),
    "dataset-a:1"
  )
  numeric_characteristic <- htmltools::renderTags(
    glc_characteristic_value_input(
      identity,
      catalogue,
      "response score"
    )
  )$html
  categorical_characteristic <- htmltools::renderTags(
    glc_characteristic_value_input(
      identity,
      catalogue,
      "chronotype"
    )
  )$html
  expect_match(
    numeric_characteristic,
    "response score (points) range",
    fixed = TRUE
  )
  expect_match(
    numeric_characteristic,
    "class=\"js-range-slider\"",
    fixed = TRUE
  )
  expect_match(numeric_characteristic, "data-type=\"double\"", fixed = TRUE)
  expect_match(categorical_characteristic, "chronotype values", fixed = TRUE)
  expect_match(categorical_characteristic, "early", fixed = TRUE)
  expect_match(categorical_characteristic, "late", fixed = TRUE)

  refinement <- glc_preview_test_refinement(
    plan,
    first_id,
    "dataset-a:1"
  )
  expect_no_error(validate_glc_collection_refinement(
    refinement,
    plan,
    first_id,
    "dataset-a:1"
  ))
  expect_false(refinement$assurance$network_access)
  expect_equal(glc_refinement_file_count(refinement, plan), 1L)
  full_refinement <- glc_preview_test_refinement(
    plan,
    first_id,
    c("dataset-a:1", "dataset-b:1")
  )
  no_device_refinement <- glc_preview_test_refinement(
    plan,
    second_id,
    "dataset-a:2"
  )
  expect_equal(glc_refinement_file_count(full_refinement, plan), 2L)
  expect_equal(glc_refinement_file_count(no_device_refinement, plan), 1L)
  expect_equal(glc_refinement_file_count(full_refinement, plan), 2L)
  refinement_html <- htmltools::renderTags(
    glc_refinement_summary_ui(refinement, plan)
  )$html
  expect_match(refinement_html, "File groups", fixed = TRUE)
  expect_match(refinement_html, "Datasets", fixed = TRUE)
  expect_match(refinement_html, "Files", fixed = TRUE)
  expect_match(refinement_html, "Participants", fixed = TRUE)
  expect_false(grepl("Final units", refinement_html, fixed = TRUE))
  expect_null(nonserializable_path(refinement))
})

test_that("hidden metadata controls cannot retain an active filter", {
  plan <- glc_preview_test_plan()
  first_id <- plan$compatibility_sets$compatibility_id[[1L]]
  second_id <- plan$compatibility_sets$compatibility_id[[2L]]
  device_catalogue <- glc_metadata_filter_catalogue(plan, first_id)
  no_device_catalogue <- glc_metadata_filter_catalogue(plan, second_id)
  stale_input <- list(filter_position = "chest")

  expect_equal(
    glc_active_metadata_filter_count(
      glc_metadata_filters_from_input(stale_input, device_catalogue)
    ),
    1L
  )
  no_device_filters <- glc_metadata_filters_from_input(
    stale_input,
    no_device_catalogue
  )
  expect_equal(glc_active_metadata_filter_count(no_device_filters), 0L)
  expect_length(no_device_filters$positions, 0L)
  expect_identical(
    glc_filter_compatibility_groups(plan, second_id, no_device_filters),
    "dataset-a:2"
  )
})
