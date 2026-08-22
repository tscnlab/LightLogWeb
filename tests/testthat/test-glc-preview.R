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

glc_preview_test_scope_inspection <- function(
  selection = glc_preview_test_selection(),
  cache_dir = tempfile("glc-cache-")
) {
  inspection <- glc_preview_test_inspection(selection, cache_dir)
  variables <- inspection$variables
  additional <- data.frame(
    dataset_id = c("dataset-a", "dataset-a", "dataset-b", "dataset-a"),
    file_group_id = c(
      "dataset-a:1",
      "dataset-a:2",
      "dataset-b:1",
      "dataset-a:2"
    ),
    name = c("Id", "Id", "Id", "ability"),
    label = c(rep("Participant ID", 3L), "Participant ability"),
    description = c(
      rep("Stable participant identity.", 3L),
      "Participant ability score."
    ),
    unit = NA_character_,
    type = c(rep("string", 3L), "number"),
    term = c(rep("participant_id", 3L), "ability"),
    term_name = c(rep("Participant ID", 3L), "Participant ability"),
    primary = c(rep(FALSE, 3L), TRUE),
    stringsAsFactors = FALSE
  )
  additional$factor_values <- rep(list(character()), nrow(additional))
  additional$factor_labels <- rep(list(character()), nrow(additional))
  additional$factor_descriptions <- rep(list(character()), nrow(additional))
  variables <- rbind(variables, additional)
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

glc_preview_test_rekey_plan <- function(
  plan,
  compatibility_digit = "9",
  unit_digit = "8"
) {
  stopifnot(
    nrow(plan$compatibility_sets) == 1L,
    nrow(plan$units) == 1L,
    grepl("^[0-9a-f]$", compatibility_digit),
    grepl("^[0-9a-f]$", unit_digit)
  )
  old_compatibility <- plan$compatibility_sets$compatibility_id[[1L]]
  old_unit <- plan$units$unit_id[[1L]]
  new_compatibility <- paste0("glcc_", strrep(compatibility_digit, 64L))
  new_unit <- paste0("glcu_", strrep(unit_digit, 64L))
  replace_compatibility <- function(value) {
    replace <- !is.na(value) & value == old_compatibility
    value[replace] <- new_compatibility
    value
  }
  replace_unit <- function(value) {
    replace <- !is.na(value) & value == old_unit
    value[replace] <- new_unit
    value
  }

  plan$preferred_unit_id <- new_unit
  plan$units$unit_id <- replace_unit(plan$units$unit_id)
  plan$units$compatibility_id <- replace_compatibility(
    plan$units$compatibility_id
  )
  for (table in c("groups", "variables", "read_columns", "files")) {
    plan[[table]]$unit_id <- replace_unit(plan[[table]]$unit_id)
  }
  plan$output_columns$unit_id <- replace_unit(plan$output_columns$unit_id)
  plan$compatibility$unit_id <- replace_unit(plan$compatibility$unit_id)
  plan$compatibility_sets$compatibility_id <- replace_compatibility(
    plan$compatibility_sets$compatibility_id
  )
  plan$groups$compatibility_id <- replace_compatibility(
    plan$groups$compatibility_id
  )
  plan$compatibility_sets$final_unit_ids <- lapply(
    plan$compatibility_sets$final_unit_ids,
    replace_unit
  )
  if (nrow(plan$compatibility_diagnostics) > 0L) {
    plan$compatibility_diagnostics$compatibility_ids <- lapply(
      plan$compatibility_diagnostics$compatibility_ids,
      replace_compatibility
    )
    plan$compatibility_diagnostics$unit_ids <- lapply(
      plan$compatibility_diagnostics$unit_ids,
      replace_unit
    )
  }
  if (nrow(plan$compatibility_diagnostic_groups) > 0L) {
    plan$compatibility_diagnostic_groups$compatibility_id <-
      replace_compatibility(
        plan$compatibility_diagnostic_groups$compatibility_id
      )
    plan$compatibility_diagnostic_groups$unit_id <- replace_unit(
      plan$compatibility_diagnostic_groups$unit_id
    )
  }
  plan$refinement_input$fingerprint <- strrep(compatibility_digit, 64L)
  plan
}

glc_preview_test_one_unit_membership <- function(
  plan,
  included_file_group_ids,
  selected_name = NULL
) {
  stopifnot(
    nrow(plan$compatibility_sets) == 1L,
    nrow(plan$units) == 1L
  )
  included_file_group_ids <- sort(
    unique(as.character(included_file_group_ids)),
    method = "radix"
  )
  known_ids <- as.character(plan$groups$file_group_id)
  stopifnot(
    length(included_file_group_ids) > 0L,
    all(included_file_group_ids %in% known_ids)
  )
  included <- known_ids %in% included_file_group_ids
  compatibility_id <- plan$compatibility_sets$compatibility_id[[1L]]
  unit_id <- plan$units$unit_id[[1L]]

  plan$groups$status <- ifelse(included, "included", "excluded")
  plan$groups$unit_id <- ifelse(included, unit_id, NA_character_)
  plan$groups$compatibility_id <- ifelse(
    included,
    compatibility_id,
    NA_character_
  )
  plan$groups$selected_variables <- lapply(
    included,
    function(value)
      if (value && !is.null(selected_name)) {
        selected_name
      } else {
        character()
      }
  )
  plan$groups$reason_codes <- lapply(
    included,
    function(value) if (value) "included" else "variable_missing"
  )
  plan$groups$messages <- lapply(
    included,
    function(value)
      if (value) {
        "Included."
      } else {
        "The selected source variable is absent."
      }
  )
  plan$files$status <- plan$groups$status
  plan$files$unit_id <- plan$groups$unit_id

  included_files <- plan$files[included, , drop = FALSE]
  known <- included_files$bytes_known %in% TRUE
  listed_bytes <- as.numeric(included_files$declared_bytes[known])
  listed_bytes <- listed_bytes[is.finite(listed_bytes)]
  dataset_count <- length(unique(plan$groups$dataset_id[included]))
  file_count <- nrow(included_files)
  known_count <- sum(known)
  unknown_count <- file_count - known_count
  bytes_complete <- file_count > 0L && unknown_count == 0L
  declared_bytes <- sum(listed_bytes)

  plan$units$dataset_count <- dataset_count
  plan$units$file_group_count <- length(included_file_group_ids)
  plan$units$file_count <- file_count
  plan$units$declared_bytes <- declared_bytes
  plan$units$known_file_count <- known_count
  plan$units$unknown_file_count <- unknown_count
  plan$units$declared_bytes_complete <- bytes_complete
  plan$units$file_group_ids <- list(included_file_group_ids)
  plan$compatibility_sets$dataset_count <- dataset_count
  plan$compatibility_sets$file_group_count <- length(included_file_group_ids)
  plan$compatibility_sets$file_count <- file_count
  plan$compatibility_sets$declared_bytes <- declared_bytes
  plan$compatibility_sets$known_file_count <- known_count
  plan$compatibility_sets$unknown_file_count <- unknown_count
  plan$compatibility_sets$declared_bytes_complete <- bytes_complete
  plan$compatibility_sets$file_group_ids <- list(included_file_group_ids)

  if (!is.null(selected_name)) {
    plan$variables$name <- selected_name
    plan$variables$label <- selected_name
    plan$variables$selection_origin <- if (
      identical(plan$request$variable_scope, "selected")
    ) {
      "selected"
    } else {
      "matched"
    }
    plan$read_columns$name <- selected_name
    plan$output_columns$name <- selected_name
    plan$compatibility$selected_names <- list(selected_name)
    plan$compatibility_sets$selected_names <- list(selected_name)
  }
  plan
}

glc_preview_test_add_automatic_read_column <- function(
  plan,
  name = "Datetime",
  declared_type = "datetime"
) {
  rows <- plan$read_columns
  rows$position <- rows$position + 1L
  rows$name <- name
  rows$declared_type <- declared_type
  rows$origin <- "required"
  rows$selected_for_output <- FALSE
  rows$automatic <- TRUE
  rows$message <- paste(name, "is required to assemble the selected output.")
  plan$read_columns <- rbind(plan$read_columns, rows)
  plan
}

glc_preview_test_zero_structure_plan <- function(plan) {
  plan$preferred_unit_id <- NA_character_
  for (table in c(
    "units",
    "variables",
    "read_columns",
    "output_columns",
    "compatibility",
    "compatibility_sets",
    "compatibility_diagnostics",
    "compatibility_diagnostic_groups"
  )) {
    plan[[table]] <- plan[[table]][0, , drop = FALSE]
  }
  plan$groups$status <- "excluded"
  plan$groups$unit_id <- NA_character_
  plan$groups$compatibility_id <- NA_character_
  plan$groups$selected_variables <- rep(list(character()), nrow(plan$groups))
  plan$groups$reason_codes <- rep(list("variable_missing"), nrow(plan$groups))
  plan$groups$messages <- rep(
    list("The selected source variable is absent."),
    nrow(plan$groups)
  )
  plan$files$status <- "excluded"
  plan$files$unit_id <- NA_character_
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

glc_preview_async_test_runtime <- function() {
  new_session_runtime(
    resolve_runtime_profile(
      "hosted",
      workers = 1L,
      is_interactive = FALSE,
      available_cores = 2L
    ),
    session = NULL
  )
}

glc_preview_wait_for <- function(session, predicate, timeout_seconds = 6) {
  deadline <- Sys.time() + timeout_seconds
  repeat {
    session$elapse(100)
    flush_m1_promises(session, times = 2L)
    if (isTRUE(predicate())) return(invisible(TRUE))
    if (Sys.time() >= deadline) {
      stop("Timed out while waiting for the module lifecycle state.")
    }
    Sys.sleep(0.01)
  }
}

test_that("Slice 2C-B UI separates optional preview from fresh full Apply", {
  html <- htmltools::renderTags(glcPreviewUI("glc"))$html
  step_labels <- c(
    "Source",
    "Inspect",
    "Discovery",
    "Selection",
    "Filtering",
    "Variables",
    "Import"
  )

  expect_identical(glc_wizard_steps()$label, step_labels)
  expect_match(html, "core metadata when needed", fixed = TRUE)
  expect_false(grepl("Metadata transfer size unavailable", html, fixed = TRUE))
  expect_match(html, "inspect_button", fixed = TRUE)
  expect_match(html, "Collect compatible groups", fixed = TRUE)
  expect_match(html, "modality and/or term", fixed = TRUE)
  expect_match(html, "validates their complete file schemas", fixed = TRUE)
  expect_false(grepl("compares the selected columns", html, fixed = TRUE))
  expect_match(html, "No transfer until Preview or Apply", fixed = TRUE)
  expect_match(html, "Project the output columns", fixed = TRUE)
  expect_match(html, "scope_mode_selector", fixed = TRUE)
  expect_match(html, "llw-glc-scope-feedback-output", fixed = TRUE)
  expect_match(
    html,
    ".llw-glc-scope-feedback-output:not(:empty)",
    fixed = TRUE
  )
  expect_match(html, "padding-top: .75rem", fixed = TRUE)
  expect_match(html, "scope_review", fixed = TRUE)
  expect_match(html, "continue_preview_button", fixed = TRUE)
  expect_match(html, "llw-glc-scope-review-output", fixed = TRUE)
  expect_false(grepl("llw-glc-scope-review-result", html, fixed = TRUE))
  expect_false(grepl("confirm_inspection", html, fixed = TRUE))
  expect_false(grepl("Exact inspection request", html, fixed = TRUE))
  expect_false(grepl("inspection_status", html, fixed = TRUE))
  expect_false(grepl("plan_status", html, fixed = TRUE))
  expect_match(html, "Import the selected data", fixed = TRUE)
  expect_match(html, "Import the complete selected data", fixed = TRUE)
  expect_match(html, "Apply starts a fresh read of every", fixed = TRUE)
  expect_match(html, "llw-glc-import-card", fixed = TRUE)
  expect_match(
    html,
    ".llw-glc-import-card__body {padding: 1rem; overflow: visible;}",
    fixed = TRUE
  )
  expect_match(html, "height: auto !important", fixed = TRUE)
  expect_match(html, "llw-glc-preview-disclosure", fixed = TRUE)
  expect_match(html, ">Preview<", fixed = TRUE)
  expect_false(grepl(
    'class="llw-glc-preview-disclosure" open',
    html,
    fixed = TRUE
  ))
  expect_lt(
    regexpr("transfer_row_budget", html, fixed = TRUE)[[1L]],
    regexpr("apply_full_import_button", html, fixed = TRUE)[[1L]]
  )
  expect_false(grepl("Confirm filtered groups", html, fixed = TRUE))
  expect_match(html, "continue_variables_button", fixed = TRUE)
  expect_lt(
    regexpr("back_scope", html, fixed = TRUE)[[1L]],
    regexpr("continue_preview_button", html, fixed = TRUE)[[1L]]
  )
  expect_match(html, "import_review", fixed = TRUE)
  expect_match(html, "transfer_row_budget", fixed = TRUE)
  expect_match(html, "transfer_problems", fixed = TRUE)
  expect_match(html, "whole number from 1 through 10,000", fixed = TRUE)
  expect_match(html, 'max="10000"', fixed = TRUE)
  expect_match(
    html,
    "grid-template-columns: repeat(7, minmax(0, 1fr))",
    fixed = TRUE
  )
  expect_match(html, "text-overflow: clip", fixed = TRUE)
  expect_match(
    html,
    "grid-template-columns: repeat(2, minmax(0, 1fr))",
    fixed = TRUE
  )
  expect_match(html, "inspection_cancellation_drain", fixed = TRUE)
  expect_match(html, "planning_cancellation_drain", fixed = TRUE)
  expect_match(html, "transfer_cancellation_drain", fixed = TRUE)
  expect_match(html, "apply_full_import_button", fixed = TRUE)
  expect_match(html, "cancel_apply_button", fixed = TRUE)
  expect_match(html, "apply_cancellation_drain", fixed = TRUE)
  expect_match(html, "full_import_result", fixed = TRUE)
  expect_match(
    html,
    ".llw-glc-preview-table {display: block; width: 100%; max-width: 100%;",
    fixed = TRUE
  )
  expect_match(
    html,
    ".llw-glc-preview-table table {width: max-content; min-width: 100%;",
    fixed = TRUE
  )
  expect_match(
    html,
    ".llw-glc-full-import-result > *",
    fixed = TRUE
  )
  expect_match(html, "llw-glc-full-import-output", fixed = TRUE)
  expect_match(
    html,
    ".llw-glc-full-import-output:not(:empty)",
    fixed = TRUE
  )
  expect_match(
    html,
    paste(
      ".llw-glc-import-stack",
      ".llw-glc-full-import-result.llw-glc-result-surface"
    ),
    fixed = TRUE
  )
  expect_match(
    html,
    ".llw-glc-scope-feedback-result {margin-top: .75rem !important;}",
    fixed = TRUE
  )
  expect_match(html, "llw-glc-import-elapsed", fixed = TRUE)
  expect_match(
    html,
    'id="glc-transfer_preview_button"',
    fixed = TRUE
  )
  expect_false(grepl("Import preview", html, fixed = TRUE))
  expect_false(grepl("Create dataset", html, fixed = TRUE))
  expect_false(grepl(
    'class="llw-glc-transfer-preview-result',
    html,
    fixed = TRUE
  ))
  expect_false(grepl(
    'class="llw-glc-full-import-result',
    html,
    fixed = TRUE
  ))
  expect_false(grepl("declaration", html, ignore.case = TRUE))
  expect_false(grepl(
    ".llw-glc-source > .card:last-child",
    html,
    fixed = TRUE
  ))
  expect_equal(length(glc_wizard_steps()$value), 7L)
  expect_lt(
    regexpr("glc-plan_summary", html, fixed = TRUE)[[1L]],
    regexpr("glc-continue_structure_button", html, fixed = TRUE)[[1L]]
  )
})

test_that("scope review substitutes useful evidence when every size is unknown", {
  review <- structure(
    list(
      selected_groups = data.frame(
        dataset_id = c("dataset-a", "dataset-b"),
        stringsAsFactors = FALSE
      ),
      selected_source_variables = c("MEDI", "Datetime"),
      read_columns = data.frame(
        name = c("MEDI", "Datetime"),
        automatic = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      ),
      output_columns = data.frame(
        name = c("MEDI", "Datetime"),
        automatic = c(FALSE, TRUE),
        stringsAsFactors = FALSE
      ),
      files = data.frame(path = c("a.csv", "b.csv")),
      byte_evidence = list(
        file_count = 2L,
        known_file_count = 0L,
        unknown_file_count = 2L,
        known_declared_bytes = 0,
        declared_bytes_complete = FALSE,
        declared_total_bytes = NA_real_
      ),
      diagnostics = data.frame(),
      source = list(
        repository = "fixture/package",
        resolved_revision = strrep("a", 40L)
      ),
      selected_compatibility_id = paste0("glcc_", strrep("b", 64L)),
      selected_unit_ids = paste0("glcu_", strrep("c", 64L))
    ),
    class = c("llw_glc_projection_review", "list")
  )

  unknown_html <- htmltools::renderTags(glc_scope_review_ui(review))$html
  expect_match(unknown_html, "Included variables", fixed = TRUE)
  expect_match(unknown_html, "Selected for the output", fixed = TRUE)
  expect_false(grepl("Listed size", unknown_html, fixed = TRUE))
  expect_false(grepl("0 B known", unknown_html, fixed = TRUE))

  result_html <- htmltools::renderTags(
    glc_scope_review_result_ui(glc_scope_review_ui(review))
  )$html
  expect_match(
    result_html,
    'class="llw-glc-scope-review-result mt-3"',
    fixed = TRUE
  )
  expect_match(
    result_html,
    "Output variables are ready for review",
    fixed = TRUE
  )

  review$byte_evidence$known_file_count <- 1L
  review$byte_evidence$unknown_file_count <- 1L
  review$byte_evidence$known_declared_bytes <- 2048
  partial_html <- htmltools::renderTags(glc_scope_review_ui(review))$html
  expect_match(partial_html, "Listed size", fixed = TRUE)
  expect_match(partial_html, "2.00 KiB known; 1 unknown", fixed = TRUE)
  expect_false(grepl("Included variables", partial_html, fixed = TRUE))
})

test_that("import display is compact and uses neutral missing-value copy", {
  expect_identical(glc_compact_display_value(NA_character_), "---")
  expect_identical(glc_compact_display_value("Unknown"), "---")
  expect_identical(glc_compact_display_value("unknown"), "---")
  expect_identical(glc_typed_facet_value(NA_character_), "---")
  expect_identical(glc_format_bytes(NA_real_), "---")
  expect_identical(glc_transfer_display_row_limit(), 6L)

  sample <- data.frame(
    row = seq_len(8L),
    value = c("Unknown", rep(NA_character_, 7L)),
    stringsAsFactors = FALSE
  )
  table_html <- htmltools::renderTags(glc_transfer_preview_table(sample))$html
  row_tags <- gregexpr("<tr>", table_html, fixed = TRUE)[[1L]]
  expect_identical(sum(row_tags > 0L), 7L)
  expect_false(grepl("Unknown", table_html, fixed = TRUE))
  expect_match(table_html, "---", fixed = TRUE)
})

test_that("long full reads show a dedicated elapsed timer and phase timing", {
  progress_path <- tempfile("full-import-progress-", fileext = ".rds")
  on.exit(unlink(progress_path, force = TRUE), add = TRUE)
  saveRDS(list(), progress_path)
  now <- as.POSIXct("2026-08-21 19:00:00", tz = "UTC")
  Sys.setFileTime(progress_path, now - 65)
  progress <- data.frame(
    phase = c("validate", "read"),
    label = c("Validating contract", "Transferring selected files"),
    status = c("complete", "running"),
    elapsed_seconds = c(0.01, NA_real_),
    detail = c("Valid.", "Reading."),
    stringsAsFactors = FALSE
  )
  review <- structure(
    list(files = data.frame(path = sprintf("file-%02d", seq_len(18L)))),
    class = c("llw_glc_transfer_review", "list")
  )

  displayed <- glc_full_import_progress_for_display(
    progress,
    review,
    progress_path,
    now = now
  )
  expect_match(displayed$detail[[2L]], "18 selected files", fixed = TRUE)
  expect_false(grepl("elapsed", displayed$detail[[2L]], fixed = TRUE))
  expect_match(
    displayed$detail[[2L]],
    "rather than exposing a reliable per-file count",
    fixed = TRUE
  )
  expect_identical(
    attr(displayed, "llw_elapsed_label", exact = TRUE),
    "1 min 5 s"
  )
  elapsed_html <- htmltools::renderTags(
    glc_full_import_elapsed_ui(
      attr(displayed, "llw_elapsed_label", exact = TRUE)
    )
  )$html
  expect_match(elapsed_html, "Elapsed transfer time", fixed = TRUE)
  expect_match(elapsed_html, "1 min 5 s", fixed = TRUE)
  expect_match(elapsed_html, 'role="timer"', fixed = TRUE)

  timing_html <- htmltools::renderTags(glc_task_timing_ui(progress))$html
  expect_match(timing_html, "Process timing", fixed = TRUE)
  expect_match(timing_html, "Validating contract", fixed = TRUE)
  expect_match(timing_html, "0.01 s", fixed = TRUE)
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

  draining_html <- htmltools::renderTags(glc_cancellation_drain_ui(
    new_task_status(
      "glc_discovery",
      state = "cancelled",
      message = "Cancellation was requested."
    ),
    1L
  ))$html
  expect_match(
    draining_html,
    "Finishing cancellation before retry...",
    fixed = TRUE
  )
  expect_null(glc_cancellation_drain_ui(
    new_task_status("glc_discovery", state = "cancelled"),
    0L
  ))
  expect_null(glc_cancellation_drain_ui(
    new_task_status("glc_discovery", state = "complete"),
    1L
  ))

  planning_progress <- data.frame(
    phase = "plan",
    label = "Building collectable units",
    status = "running",
    elapsed_seconds = NA_real_,
    detail = "Applying the engine to a selected canonical term.",
    stringsAsFactors = FALSE
  )
  displayed_progress <- glc_planning_progress_for_display(planning_progress)
  expect_match(
    displayed_progress$detail,
    "identify candidate file groups",
    fixed = TRUE
  )
  expect_match(
    displayed_progress$detail,
    "complete file schema",
    fixed = TRUE
  )
  expect_false(grepl(
    "engine to a selected canonical term",
    displayed_progress$detail,
    fixed = TRUE
  ))
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

test_that("terminal cancellation explains natural runtime drain", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  expect_identical(
    runtime$submit("inspection-drain-blocker", function() invisible(NULL)),
    "running"
  )
  source <- shiny::reactiveVal(glc_preview_test_selection())

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = function(payload, spec) {
        stop("The blocked inspection must not run.")
      },
      task_start_timeout_seconds = 5
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1L)
      session$flushReact()
      expect_true(returned$inspection_pending())

      session$setInputs(cancel_inspection = 1L)
      session$flushReact()
      expect_identical(returned$inspection_status()$state, "cancelled")
      drain_html <- htmltools::renderTags(
        output$inspection_cancellation_drain
      )$html
      expect_match(
        drain_html,
        "Finishing cancellation before retry...",
        fixed = TRUE
      )

      runtime$release("inspection-drain-blocker")
      session$elapse(250)
      session$flushReact()
      released_html <- htmltools::renderTags(
        output$inspection_cancellation_drain
      )$html
      expect_false(grepl(
        "Finishing cancellation before retry...",
        released_html,
        fixed = TRUE
      ))
    }
  )
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
      # Retry immediately. A synchronous test worker may already have released
      # its runtime slot, while the separate bounded-deadline test exercises
      # the queued path. In both cases the retry must use a fresh controller.
      session$setInputs(inspect = 2L)
      flush_m1_promises(session)
      expect_equal(calls$count, 2L)
      expect_equal(runtime$active_count(), 0L)
      expect_equal(returned$inspection_controller_revision(), 1L)
      expect_false(returned$inspection_pending())
      expect_identical(returned$inspection_status()$state, "complete")

      session$setInputs(inspect = 3L)
      expect_equal(calls$count, 3L)
      session$setInputs(cancel_inspection = 2L)
      expect_identical(returned$inspection_status()$state, "cancelled")
      session$setInputs(inspect = 4L)
      flush_m1_promises(session)
      expect_equal(calls$count, 4L)
      expect_equal(runtime$active_count(), 0L)
      expect_equal(returned$inspection_controller_revision(), 3L)
      expect_identical(returned$inspection_status()$state, "complete")

      source(glc_preview_test_selection(
        repository = "fixture/second",
        revision = strrep("b", 40L)
      ))
      session$flushReact()
      expect_null(returned$inspection())
      expect_identical(returned$inspection_status()$state, "idle")

      session$setInputs(inspect = 5L)
      flush_m1_promises(session)
      expect_equal(calls$count, 5L)
      expect_equal(returned$inspection_controller_revision(), 4L)
      expect_identical(returned$inspection_status()$state, "complete")
      expect_identical(
        returned$inspection()$source$repository,
        "fixture/second"
      )
    }
  )
})

test_that("pending inspection startup settles at its bounded deadline", {
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
      inspection_worker = inspection_worker,
      task_start_timeout_seconds = 1
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      expect_identical(
        runtime$submit(
          "inspection-capacity-blocker",
          function() invisible(NULL)
        ),
        "running"
      )

      session$setInputs(inspect = 1L)
      expect_true(returned$inspection_pending())
      expect_equal(calls$count, 0L)
      Sys.sleep(1.05)
      session$elapse(1050)
      session$flushReact()
      expect_false(returned$inspection_pending())
      expect_identical(returned$inspection_status()$state, "error")
      expect_match(
        returned$inspection_status()$message,
        "still stopping",
        fixed = TRUE
      )

      runtime$release("inspection-capacity-blocker")
      session$setInputs(inspect = 2L)
      flush_m1_promises(session)
      expect_equal(calls$count, 1L)
      expect_identical(returned$inspection_status()$state, "complete")
    }
  )
})

test_that("long-lived inspection controllers release across Deep and Fresh", {
  skip_if_not_installed("mirai")
  runtime <- glc_preview_async_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error")) {
    skip("mirai daemons are unavailable in this test runtime")
  }

  selection <- glc_preview_test_selection()
  fixture <- glc_preview_test_inspection(
    selection,
    cache_dir = runtime$paths$cache
  )
  request <- new_glc_planning_request(
    term = "melanopic_edi",
    variable_scope = "all"
  )
  plan <- glc_preview_test_plan(fixture, request)
  source <- shiny::reactiveVal(selection)
  auto <- shiny::reactiveVal(FALSE)
  inspection_worker <- local({
    value <- fixture
    function(payload, spec) {
      Sys.sleep(0.25)
      value
    }
  })
  planning_worker <- local({
    value <- plan
    function(payload, spec) {
      Sys.sleep(0.2)
      value
    }
  })

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      auto_inspect = auto,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker,
      task_start_timeout_seconds = 2
    ),
    {
      returned <- session$getReturned()
      session$flushReact()

      session$setInputs(inspect = 1L)
      glc_preview_wait_for(session, function() {
        identical(returned$inspection_status()$state, "complete")
      })
      session$setInputs(term_id = "melanopic_edi")
      session$setInputs(plan = 1L)
      glc_preview_wait_for(session, function() {
        identical(returned$plan_status()$state, "complete")
      })

      auto(TRUE)
      glc_preview_wait_for(session, returned$inspection_active)
      auto(FALSE)
      session$flushReact()
      expect_null(returned$inspection())
      expect_identical(returned$inspection_status()$state, "idle")
      glc_preview_wait_for(session, function() runtime$active_count() == 0L)

      auto(TRUE)
      glc_preview_wait_for(session, function() {
        identical(returned$inspection_status()$state, "complete")
      })

      for (attempt in seq_len(2L)) {
        session$setInputs(inspect = attempt + 1L)
        glc_preview_wait_for(session, returned$inspection_active)
        session$setInputs(cancel_inspection = attempt)
        expect_identical(returned$inspection_status()$state, "cancelled")
        expect_false(returned$inspection_active())

        session$setInputs(inspect = attempt + 10L)
        expect_true(
          returned$inspection_pending() || returned$inspection_active()
        )
        glc_preview_wait_for(session, function() {
          identical(returned$inspection_status()$state, "complete")
        })
      }

      expect_equal(runtime$active_count(), 0L)
      expect_false(returned$inspection_pending())
      expect_false(returned$inspection_active())
    }
  )
})

test_that("invoked inspection startup has a bounded recoverable deadline", {
  skip_if_not_installed("mirai")
  runtime <- glc_preview_async_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error")) {
    skip("mirai daemons are unavailable in this test runtime")
  }

  selection <- glc_preview_test_selection()
  fixture <- glc_preview_test_inspection(
    selection,
    cache_dir = runtime$paths$cache
  )
  marker <- file.path(runtime$paths$tasks, "inspection-startup-marker")
  source <- shiny::reactiveVal(selection)
  inspection_worker <- local({
    value <- fixture
    marker_path <- marker
    function(payload, spec) {
      first <- !file.exists(marker_path)
      if (first) {
        writeLines("started", marker_path, useBytes = TRUE)
        Sys.sleep(1.5)
      } else {
        saveRDS(
          data.frame(
            phase = "open",
            label = "Opening exact revision",
            status = "running",
            elapsed_seconds = NA_real_,
            detail = "The retry worker started successfully.",
            stringsAsFactors = FALSE
          ),
          payload$progress_path,
          version = 2
        )
      }
      value
    }
  })

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      task_start_timeout_seconds = 0.5
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1L)
      glc_preview_wait_for(session, function() {
        identical(returned$inspection_status()$state, "error")
      })
      expect_match(
        returned$inspection_status()$message,
        "could not start in time",
        fixed = TRUE
      )
      expect_false(returned$inspection_active())

      glc_preview_wait_for(session, function() runtime$active_count() == 0L)
      session$setInputs(inspect = 2L)
      glc_preview_wait_for(session, function() {
        identical(returned$inspection_status()$state, "complete")
      })
      expect_s3_class(returned$inspection(), "llw_glc_inspection")
      expect_equal(runtime$active_count(), 0L)
    }
  )
})

test_that("cancelled planning workers release for immediate repeated retries", {
  skip_if_not_installed("mirai")
  runtime <- glc_preview_async_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error")) {
    skip("mirai daemons are unavailable in this test runtime")
  }

  selection <- glc_preview_test_selection()
  fixture <- glc_preview_test_inspection(
    selection,
    cache_dir = runtime$paths$cache
  )
  request <- new_glc_planning_request(
    term = "melanopic_edi",
    modality = "light",
    file_group_ids = glc_modality_file_groups(fixture$files, "light"),
    variable_scope = "all"
  )
  plan <- glc_preview_test_plan(fixture, request)
  source <- shiny::reactiveVal(selection)
  inspection_worker <- local({
    value <- fixture
    function(payload, spec) value
  })
  planning_worker <- local({
    value <- plan
    function(payload, spec) {
      saveRDS(
        data.frame(
          phase = "plan",
          label = "Building collectable units",
          status = "running",
          elapsed_seconds = NA_real_,
          detail = "The planning worker started successfully.",
          stringsAsFactors = FALSE
        ),
        payload$progress_path,
        version = 2
      )
      Sys.sleep(0.25)
      value
    }
  })

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker,
      task_start_timeout_seconds = 2
    ),
    {
      returned <- session$getReturned()
      session$flushReact()
      session$setInputs(inspect = 1L)
      glc_preview_wait_for(session, function() {
        identical(returned$inspection_status()$state, "complete")
      })
      session$setInputs(modality_id = "light")
      session$setInputs(term_id = "melanopic_edi")
      session$flushReact()

      for (attempt in seq_len(2L)) {
        if (attempt == 1L) {
          session$setInputs(continue_structure = attempt)
          expect_true(returned$continue_after_planning())
        } else {
          session$setInputs(plan = attempt)
        }
        glc_preview_wait_for(session, returned$planning_active)
        session$setInputs(cancel_plan = attempt)
        expect_identical(returned$plan_status()$state, "cancelled")
        expect_false(returned$continue_after_planning())
        expect_identical(returned$selected_term(), "melanopic_edi")
        expect_identical(returned$selected_modality(), "light")
        expect_false(returned$planning_active())

        session$setInputs(plan = attempt + 10L)
        expect_true(returned$planning_pending() || returned$planning_active())
        glc_preview_wait_for(session, function() {
          identical(returned$plan_status()$state, "complete")
        })
      }

      expect_equal(runtime$active_count(), 0L)
      expect_false(returned$planning_pending())
      expect_false(returned$planning_active())
      expect_s3_class(returned$plan(), "glc_collection_plan")
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

      session$setInputs(continue_structure = 1L)
      expect_equal(calls$planning, 1L)
      expect_true(returned$continue_after_planning())
      session$setInputs(cancel_plan = 1L)
      expect_identical(returned$plan_status()$state, "cancelled")
      expect_false(returned$continue_after_planning())
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
      inspected_button_html <- htmltools::renderTags(
        output$inspect_button
      )$html
      expect_match(
        inspected_button_html,
        'disabled="disabled"',
        fixed = TRUE
      )
      expect_match(
        inspected_button_html,
        "already inspected",
        fixed = TRUE
      )
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
      expect_identical(calls$planning_payload$request$variable_scope, "all")

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
      expect_match(
        feedback_html,
        "identifies candidate file groups",
        fixed = TRUE
      )
      expect_match(feedback_html, "complete file schema", fixed = TRUE)
      expect_match(feedback_html, "Step 6", fixed = TRUE)

      session$setInputs(term_id = "activity_context")
      session$flushReact()
      combined_feedback <- htmltools::renderTags(
        output$discovery_feedback
      )$html
      expect_match(
        combined_feedback,
        "term and modality identify candidate file",
        fixed = TRUE
      )

      session$setInputs(modality_id = "")
      session$flushReact()
      term_feedback <- htmltools::renderTags(output$discovery_feedback)$html
      expect_match(
        term_feedback,
        "canonical term identifies candidate file groups",
        fixed = TRUE
      )
      expect_match(term_feedback, "complete file schema", fixed = TRUE)

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
  expect_match(
    card_html,
    "Complete file schema validated for compatibility",
    fixed = TRUE
  )
  expect_false(grepl(
    "Variables selected by this discovery request",
    card_html,
    fixed = TRUE
  ))

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

glc_preview_test_projection_inspection <- function(
  selection = glc_preview_test_selection(),
  cache_dir = tempfile("glc-cache-")
) {
  inspection <- glc_preview_test_inspection(selection, cache_dir)
  variables <- inspection$variables
  variables$name[variables$name == "melEDI"] <- "MEDI"
  datetime_row <- variables[
    variables$file_group_id == "dataset-a:2" &
      variables$name == "Datetime",
    ,
    drop = FALSE
  ]
  datetime_row$dataset_id <- "dataset-a"
  datetime_row$file_group_id <- "dataset-a:1"
  variables <- rbind(variables, datetime_row)
  variables <- variables[
    order(
      variables$dataset_id,
      variables$file_group_id,
      variables$name,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
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

glc_preview_test_projection_plan <- function(
  inspection = glc_preview_test_projection_inspection(),
  request = new_glc_planning_request(
    term = "melanopic_edi",
    variable_scope = "all"
  )
) {
  plan <- glc_preview_test_plan(
    inspection,
    request,
    two_units = FALSE
  )
  plan$units$variable_count <- 2L
  plan$compatibility_sets$variable_count <- 2L
  plan$groups$selected_variables <- rep(
    list(c("Datetime", "MEDI")),
    nrow(plan$groups)
  )

  variables <- plan$variables
  variables$name[variables$name == "melEDI"] <- "MEDI"
  variables$selection_origin <- "all"
  datetime_rows <- variables
  datetime_rows$position <- 2L
  datetime_rows$name <- "Datetime"
  datetime_rows$label <- "Date and time"
  datetime_rows$description <- "Measurement timestamp."
  datetime_rows$unit <- NA_character_
  datetime_rows$type <- "datetime"
  datetime_rows$term <- "datetime"
  datetime_rows$term_name <- "Date and time"
  datetime_rows$primary <- FALSE
  datetime_rows$selection_origin <- "all"
  plan$variables <- rbind(variables, datetime_rows)
  plan$variables <- plan$variables[
    order(
      plan$variables$dataset_id,
      plan$variables$file_group_id,
      plan$variables$position,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]

  read_columns <- plan$read_columns
  read_columns$name[read_columns$name == "melEDI"] <- "MEDI"
  read_columns$origin <- "selected"
  read_columns$selected_for_output <- TRUE
  read_columns$automatic <- FALSE
  read_columns$message <- "Selected from the complete file schema."
  automatic_rows <- read_columns
  automatic_rows$position <- 2L
  automatic_rows$name <- "Datetime"
  automatic_rows$declared_type <- "datetime"
  automatic_rows$origin <- "required"
  automatic_rows$selected_for_output <- FALSE
  automatic_rows$automatic <- TRUE
  automatic_rows$message <- paste(
    "Datetime is required to validate and assemble the selected output."
  )
  plan$read_columns <- rbind(read_columns, automatic_rows)
  plan$read_columns <- plan$read_columns[
    order(
      plan$read_columns$dataset_id,
      plan$read_columns$file_group_id,
      plan$read_columns$position,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]

  output_datetime <- plan$output_columns
  output_datetime$position <- 2L
  output_datetime$name <- "Datetime"
  output_datetime$source_declared_type <- "datetime"
  output_datetime$expected_type <- "POSIXct"
  output_datetime$origin <- "required"
  output_datetime$automatic <- TRUE
  output_datetime$message <- "Standardized time column retained by glcdp."
  plan$output_columns <- rbind(plan$output_columns, output_datetime)

  selected_names <- c("Datetime", "MEDI")
  plan$compatibility$selected_names <- list(selected_names)
  plan$compatibility$declared_types <- list(c("datetime", "number"))
  plan$compatibility_sets$selected_names <- list(selected_names)
  plan$compatibility_sets$declared_types <- list(c("datetime", "number"))
  plan$metadata$file_group_variables$selected_by_request <- TRUE
  plan$metadata$file_group_variables$selection_origin <- "all"
  plan
}

glc_preview_test_preview_worker <- function(
  calls = NULL,
  delay_seconds = 0
) {
  force(calls)
  force(delay_seconds)
  function(payload, spec) {
    if (!is.null(calls)) {
      calls$preview <- (calls$preview %||% 0L) + 1L
    }
    if (delay_seconds > 0) {
      saveRDS(
        data.frame(
          phase = "fixture",
          label = "Preparing deterministic fixture records",
          status = "running",
          elapsed_seconds = NA_real_,
          detail = "The injected preview worker started successfully.",
          stringsAsFactors = FALSE
        ),
        payload$progress_path,
        version = 2
      )
      Sys.sleep(delay_seconds)
    }

    review <- payload$transfer_review
    validate_glc_transfer_review(review)
    allocated <- review$allocated_files
    rows_per_file <- as.integer(review$allocation$per_file_n_max)
    output_names <- unique(as.character(review$projection$output_columns$name))

    fixture_column <- function(name, count, file_index) {
      lower <- tolower(name)
      if (grepl("date|time", lower)) {
        return(
          as.POSIXct(
            "2026-08-01 12:00:00",
            tz = "UTC"
          ) +
            seq_len(count) +
            (file_index - 1L) * 60L
        )
      }
      if (grepl("(^|_)id$|participant|file_group|file\\.name", lower)) {
        return(paste0("fixture-", file_index, "-", seq_len(count)))
      }
      as.numeric(seq_len(count) + (file_index - 1L) * count)
    }

    file_data <- lapply(seq_len(nrow(allocated)), function(index) {
      values <- stats::setNames(
        lapply(
          output_names,
          fixture_column,
          count = rows_per_file,
          file_index = index
        ),
        output_names
      )
      as.data.frame(values, check.names = FALSE, stringsAsFactors = FALSE)
    })
    collected <- do.call(rbind, file_data)
    rownames(collected) <- NULL
    collection <- data.frame(
      dataset_id = as.character(allocated$dataset_id),
      file_group_id = as.character(allocated$file_group_id),
      files = I(lapply(
        allocated$declared_path,
        function(path) file.path("deterministic-fixture", path)
      )),
      runtime_input_ids = I(lapply(
        seq_len(nrow(allocated)),
        function(index) sprintf("fixture://preview-input/%03d", index)
      )),
      data = I(file_data),
      stringsAsFactors = FALSE
    )

    exports <- glcdp_export_contract()
    api <- stats::setNames(
      lapply(exports, function(name) function(...) NULL),
      exports
    )
    api$glc_open <- function(...) list(fixture = TRUE)
    api$glc_read <- function(...) collection
    api$glc_collect <- function(x, standardize) collected
    glcdp_transfer_preview_execute(
      payload,
      api = api,
      execution_mode = "fixture"
    )
  }
}

glc_preview_test_full_import_worker <- function(
  calls = NULL,
  delay_seconds = 0
) {
  force(calls)
  force(delay_seconds)
  function(payload, spec) {
    if (!is.null(calls)) {
      calls$apply <- (calls$apply %||% 0L) + 1L
      calls$apply_payload_names <- names(payload)
    }
    if (delay_seconds > 0) {
      saveRDS(
        data.frame(
          phase = "fixture",
          label = "Preparing a fresh deterministic full read",
          status = "running",
          elapsed_seconds = NA_real_,
          detail = "The injected full-import worker started successfully.",
          stringsAsFactors = FALSE
        ),
        payload$progress_path,
        version = 2
      )
      Sys.sleep(delay_seconds)
    }

    review <- payload$transfer_review
    validate_glc_transfer_review(review)
    selected <- review$files
    output_names <- unique(as.character(review$projection$output_columns$name))

    fixture_column <- function(name, count, file_index) {
      lower <- tolower(name)
      if (grepl("date|time", lower)) {
        return(
          as.POSIXct(
            "2026-08-01 12:00:00",
            tz = "UTC"
          ) +
            seq_len(count) +
            (file_index - 1L) * 60L
        )
      }
      if (grepl("(^|_)id$|participant|file_group|file\\.name", lower)) {
        return(paste0("full-fixture-", file_index, "-", seq_len(count)))
      }
      as.numeric(seq_len(count) + (file_index - 1L) * count)
    }

    rows_per_file <- 2L
    file_data <- lapply(seq_len(nrow(selected)), function(index) {
      values <- stats::setNames(
        lapply(
          output_names,
          fixture_column,
          count = rows_per_file,
          file_index = index
        ),
        output_names
      )
      as.data.frame(values, check.names = FALSE, stringsAsFactors = FALSE)
    })
    collected <- do.call(rbind, file_data)
    rownames(collected) <- NULL
    collection <- data.frame(
      dataset_id = as.character(selected$dataset_id),
      file_group_id = as.character(selected$file_group_id),
      files = I(lapply(
        selected$declared_path,
        function(path) file.path("deterministic-full-fixture", path)
      )),
      runtime_input_ids = I(lapply(
        seq_len(nrow(selected)),
        function(index) sprintf("fixture://full-import-input/%03d", index)
      )),
      data = I(file_data),
      stringsAsFactors = FALSE
    )

    exports <- glcdp_export_contract()
    api <- stats::setNames(
      lapply(exports, function(name) function(...) NULL),
      exports
    )
    api$glc_open <- function(...) list(fixture = TRUE)
    api$glc_read <- function(...) collection
    api$glc_collect <- function(x, standardize) collected
    glcdp_full_import_execute(
      payload,
      api = api,
      execution_mode = "fixture"
    )
  }
}

test_that("numeric-looking scalar characteristics use a lossless range rule", {
  plan <- glc_preview_test_plan()
  first_id <- plan$compatibility_sets$compatibility_id[[1L]]
  characteristics <- glc_preview_empty_table(
    glc_collection_plan_metadata_tables()$participant_characteristics,
    4L
  )
  characteristics$participant_id <- rep(
    c("participant-a", "participant-b"),
    2L
  )
  characteristics$participant_link_status <- "resolved"
  characteristics$characteristic_position <- rep(1:2, each = 2L)
  characteristics$value_position <- 1L
  characteristics$name <- rep(c("VLSQ8", "Outdoor minutes"), each = 2L)
  characteristics$description <- rep(
    c("Validated questionnaire score.", "Time spent outdoors."),
    each = 2L
  )
  characteristics$value <- list("14", "22", "30", "120")
  characteristics$value_type <- "character"
  characteristics$unit <- c("", "", "minutes", "minutes")
  plan$metadata$participant_characteristics <- characteristics

  catalogue <- glc_metadata_filter_catalogue(plan, first_id)
  vlsq8 <- glc_characteristic_filter_spec(
    catalogue$characteristics,
    "VLSQ8"
  )
  outdoor <- glc_characteristic_filter_spec(
    catalogue$characteristics,
    "Outdoor minutes"
  )
  expect_identical(vlsq8$type, "numeric")
  expect_identical(outdoor$type, "numeric")
  expect_equal(c(vlsq8$slider$min, vlsq8$slider$max), c(14, 22))
  expect_equal(c(outdoor$slider$min, outdoor$slider$max), c(30, 120))
  expect_identical(
    vlsq8$numeric_projection$original_token,
    c("14", "22")
  )

  filters <- glc_metadata_filters_from_input(
    list(
      filter_characteristic_name = "VLSQ8",
      filter_characteristic_values = c(14, 14)
    ),
    catalogue
  )
  expect_equal(filters$characteristic_range, c(14, 14))
  expect_identical(
    filters$characteristic_numeric_tokens$original_token,
    c("14", "22")
  )
  expect_identical(
    glc_filter_compatibility_groups(plan, first_id, filters),
    "dataset-a:1"
  )
  expect_identical(
    glc_characteristic_value_input_id("VLSQ8"),
    "filter_characteristic_values"
  )
  vlsq8_input_id <- glc_characteristic_value_input_id("VLSQ8", 4L)
  outdoor_input_id <- glc_characteristic_value_input_id(
    "Outdoor minutes",
    4L
  )
  expect_false(identical(vlsq8_input_id, outdoor_input_id))
  switched_inputs <- list(filter_characteristic_name_r4 = "Outdoor minutes")
  switched_inputs[[vlsq8_input_id]] <- c(14, 14)
  switched_filters <- glc_metadata_filters_from_input(
    switched_inputs,
    catalogue,
    revision = 4L
  )
  expect_length(switched_filters$characteristic_range, 0L)
  expect_equal(glc_active_metadata_filter_count(switched_filters), 0L)
  switched_inputs[[outdoor_input_id]] <- c(30, 120)
  switched_filters <- glc_metadata_filters_from_input(
    switched_inputs,
    catalogue,
    revision = 4L
  )
  expect_length(switched_filters$characteristic_range, 0L)
  expect_equal(glc_active_metadata_filter_count(switched_filters), 0L)

  decimal_outdoor <- characteristics[
    characteristics$name == "Outdoor minutes",
  ]
  decimal_outdoor$value <- list("60", "565.714285714286")
  plan$metadata$participant_characteristics <- rbind(
    characteristics[characteristics$name != "Outdoor minutes", ],
    decimal_outdoor
  )
  decimal_catalogue <- glc_metadata_filter_catalogue(plan, first_id)
  decimal_inputs <- list(filter_characteristic_name_r4 = "Outdoor minutes")
  decimal_inputs[[outdoor_input_id]] <- c(60, 565.7)
  decimal_filters <- glc_metadata_filters_from_input(
    decimal_inputs,
    decimal_catalogue,
    revision = 4L
  )
  expect_length(decimal_filters$characteristic_range, 0L)
  expect_equal(glc_active_metadata_filter_count(decimal_filters), 0L)
  decimal_inputs[[outdoor_input_id]] <- c(60, 560.6)
  decimal_filters <- glc_metadata_filters_from_input(
    decimal_inputs,
    decimal_catalogue,
    revision = 4L
  )
  expect_equal(decimal_filters$characteristic_range, c(60, 560.6))
  expect_equal(glc_active_metadata_filter_count(decimal_filters), 1L)
  slider_html <- htmltools::renderTags(glc_characteristic_value_input(
    identity,
    catalogue,
    "VLSQ8"
  ))$html
  expect_match(slider_html, "VLSQ8 range", fixed = TRUE)
  expect_match(slider_html, "class=\"js-range-slider\"", fixed = TRUE)

  unsafe <- function(tokens, units = rep("", length(tokens)), values = NULL) {
    data.frame(
      value_type = rep("character", length(tokens)),
      unit = units,
      value_key = tokens,
      numeric_value = suppressWarnings(as.numeric(tokens)),
      scalar_value = rep(TRUE, length(tokens)),
      stringsAsFactors = FALSE
    )
  }
  expect_null(glc_numeric_characteristic_projection(
    unsafe(c("01", "1"))
  ))
  expect_null(glc_numeric_characteristic_projection(
    unsafe(c("1", "2"), c("points", "percent"))
  ))
  non_scalar <- unsafe(c("1", "2"))
  non_scalar$scalar_value[[2L]] <- FALSE
  expect_null(glc_numeric_characteristic_projection(non_scalar))
  expect_null(glc_numeric_characteristic_projection(
    unsafe(c("1", "not numeric"))
  ))
  expect_null(glc_numeric_characteristic_projection(
    unsafe(c("1", "Inf"))
  ))
})

test_that("variable projection preserves the fixed Slice 2A structure", {
  runtime <- glc_preview_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  source <- shiny::reactiveVal(glc_preview_test_selection())
  calls <- new.env(parent = emptyenv())
  calls$planning <- 0L
  calls$preview <- 0L
  calls$apply <- 0L

  inspection_worker <- function(payload, spec) {
    glc_preview_test_projection_inspection(
      glc_preview_test_selection(
        repository = payload$request$repository,
        revision = payload$request$resolved_revision
      ),
      cache_dir = payload$request$cache_dir
    )
  }
  planning_worker <- function(payload, spec) {
    calls$planning <- calls$planning + 1L
    inspection <- glc_preview_test_projection_inspection(
      glc_preview_test_selection(
        repository = payload$source$repository,
        revision = payload$source$resolved_revision
      ),
      cache_dir = payload$source$cache_dir
    )
    glc_preview_test_projection_plan(inspection, payload$request)
  }

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker,
      refine_collection = glc_preview_test_refinement,
      preview_worker = glc_preview_test_preview_worker(calls),
      apply_worker = glc_preview_test_full_import_worker(calls),
      task_start_timeout_seconds = 1
    ),
    {
      returned <- session$getReturned()
      set_scope_input <- function(id, value) {
        input_id <- glc_scope_input_id(
          id,
          returned$scope_input_revision()
        )
        do.call(
          session$setInputs,
          stats::setNames(list(value), input_id)
        )
        session$flushReact()
        invisible(input_id)
      }
      session$flushReact()
      session$setInputs(inspect = 1L)
      flush_m1_promises(session)
      session$setInputs(term_id = "melanopic_edi")
      session$flushReact()
      continue_structure_html <- htmltools::renderTags(
        output$continue_structure_button
      )$html
      expect_false(grepl(
        'disabled="disabled"',
        continue_structure_html,
        fixed = TRUE
      ))
      expect_match(
        continue_structure_html,
        "Collect compatible groups for this request",
        fixed = TRUE
      )
      session$setInputs(continue_structure = 1L)
      flush_m1_promises(session)

      expect_equal(calls$planning, 1L)
      expect_identical(returned$wizard_step(), "structure")
      expect_false(returned$continue_after_planning())
      expect_identical(returned$planning_request()$variable_scope, "all")
      parent_id <- returned$selected_compatibility_id()
      parent_unit_ids <- returned$plan()$compatibility_sets$final_unit_ids[[1L]]

      continue_variables_html <- htmltools::renderTags(
        output$continue_variables_button
      )$html
      expect_match(
        continue_variables_html,
        "Confirm and continue to variable selection",
        fixed = TRUE
      )
      expect_false(grepl(
        'disabled="disabled"',
        continue_variables_html,
        fixed = TRUE
      ))
      session$setInputs(continue_variables = 1L)
      session$flushReact()
      refined_ids <- returned$refinement()$request$file_group
      expect_setequal(refined_ids, c("dataset-a:1", "dataset-b:1"))

      set_scope_input("scope_mode", "discovery")
      scope_feedback_html <- htmltools::renderTags(
        output$scope_feedback
      )$html
      expect_match(
        scope_feedback_html,
        'class="llw-glc-scope-feedback-result"',
        fixed = TRUE
      )
      session$setInputs(scope_apply = 1L)
      session$flushReact()
      discovery_review <- returned$scope_review()
      expect_null(
        returned$scope_review_error(),
        info = if (!is.null(returned$scope_review_error())) {
          conditionMessage(returned$scope_review_error())
        }
      )
      expect_equal(calls$planning, 1L)
      expect_s3_class(discovery_review, "llw_glc_projection_review")
      expect_identical(discovery_review$selected_source_variables, "MEDI")
      expect_identical(
        discovery_review$selected_compatibility_id,
        parent_id
      )
      expect_identical(discovery_review$selected_unit_ids, parent_unit_ids)
      expect_identical(
        discovery_review$refined_file_group_ids,
        refined_ids
      )
      expect_false(discovery_review$assurance$compatibility_replanned)
      expect_true(any(
        discovery_review$read_columns$name == "Datetime" &
          discovery_review$read_columns$automatic %in% TRUE
      ))

      set_scope_input("scope_mode", "all")
      session$setInputs(scope_apply = 2L)
      session$flushReact()
      all_review <- returned$scope_review()
      expect_null(
        returned$scope_review_error(),
        info = if (!is.null(returned$scope_review_error())) {
          conditionMessage(returned$scope_review_error())
        }
      )
      expect_equal(calls$planning, 1L)
      expect_identical(
        all_review$selected_source_variables,
        c("Datetime", "MEDI")
      )
      expect_identical(all_review$selected_compatibility_id, parent_id)
      expect_identical(all_review$selected_unit_ids, parent_unit_ids)

      set_scope_input("scope_mode", "custom")
      set_scope_input("scope_custom_variables", "MEDI")
      expect_null(returned$scope_review())
      continue_html <- htmltools::renderTags(
        output$continue_preview_button
      )$html
      expect_match(continue_html, "Continue to Import", fixed = TRUE)
      session$setInputs(continue_preview = 1L)
      session$flushReact()
      custom_review <- returned$scope_review()
      expect_equal(calls$planning, 1L)
      expect_identical(custom_review$selected_source_variables, "MEDI")
      expect_identical(custom_review$selected_compatibility_id, parent_id)
      expect_identical(custom_review$selected_unit_ids, parent_unit_ids)
      expect_null(nonserializable_path(custom_review))
      expect_type(serialize(custom_review, NULL), "raw")

      session$setInputs(
        transfer_row_budget = 4L,
        transfer_problems = "error"
      )
      session$flushReact()
      transfer_review <- returned$transfer_review()
      expect_s3_class(transfer_review, "llw_glc_transfer_review")
      expect_null(returned$transfer_review_error())
      expect_equal(calls$preview, 0L)
      expect_null(returned$transfer_preview())
      expect_identical(transfer_review$source, custom_review$source)
      expect_identical(
        transfer_review$fixed$compatibility_id,
        custom_review$selected_compatibility_id
      )
      expect_identical(
        transfer_review$fixed$unit_id,
        custom_review$selected_unit_ids[[1L]]
      )
      expect_identical(
        transfer_review$fixed$file_group_ids,
        custom_review$refined_file_group_ids
      )
      expect_identical(transfer_review$problems_policy, "error")
      expect_identical(transfer_review$allocation$total_budget, 4L)
      expect_identical(transfer_review$allocation$preview_file_count, 2L)
      expect_identical(transfer_review$allocation$per_file_n_max, 2)
      expect_identical(transfer_review$allocation$maximum_rows, 4)
      expect_true(transfer_review$assurance$explicit_action_required)
      expect_false(transfer_review$assurance$package_reopened)
      expect_false(transfer_review$assurance$network_accessed)
      expect_false(
        transfer_review$assurance$measurement_contents_transferred
      )

      import_html <- htmltools::renderTags(output$import_review)$html
      expect_match(import_html, "Ready to import", fixed = TRUE)
      expect_match(import_html, "2 selected files", fixed = TRUE)
      expect_match(import_html, "Package-listed size", fixed = TRUE)
      expect_match(import_html, "100 B known; 1 unknown", fixed = TRUE)
      expect_false(grepl(
        "Import size evidence is incomplete",
        import_html,
        fixed = TRUE
      ))
      expect_false(grepl("Preview-row budget", import_html, fixed = TRUE))

      preview_review_html <- htmltools::renderTags(
        output$transfer_review
      )$html
      expect_match(preview_review_html, "Preview-row budget", fixed = TRUE)
      expect_match(preview_review_html, "Rows per file", fixed = TRUE)
      expect_match(preview_review_html, "Maximum rows", fixed = TRUE)
      expect_match(preview_review_html, "Git-LFS", fixed = TRUE)
      expect_match(preview_review_html, "Problems: error", fixed = TRUE)

      apply_button_html <- htmltools::renderTags(
        output$apply_full_import_button
      )$html
      expect_false(grepl(
        'disabled="disabled"',
        apply_button_html,
        fixed = TRUE
      ))
      expect_match(apply_button_html, "btn-primary", fixed = TRUE)
      expect_false(grepl("btn-danger", apply_button_html, fixed = TRUE))
      session$setInputs(apply_full_import = 1L, .priority = "event")
      flush_m1_promises(session)
      first_full_import <- returned$full_import()
      expect_equal(calls$apply, 1L)
      expect_identical(
        calls$apply_payload_names,
        c("operation", "source", "transfer_review", "progress_path")
      )
      expect_false("preview_result" %in% calls$apply_payload_names)
      expect_s3_class(first_full_import, "llw_glc_full_import_result")
      expect_identical(
        first_full_import$transfer_review_fingerprint,
        glc_full_import_contract_review(transfer_review)$fingerprint
      )
      expect_identical(first_full_import$actual$input_count, 2L)
      expect_identical(first_full_import$actual$file_group_count, 2L)
      expect_identical(first_full_import$actual$row_count, 4L)
      expect_true(first_full_import$assurance$fresh_full_read)
      expect_false(first_full_import$assurance$preview_result_used)
      expect_true(first_full_import$assurance$complete_collected_handoff)
      expect_false(first_full_import$assurance$persisted)
      expect_null(nonserializable_path(first_full_import))
      expect_type(serialize(first_full_import, NULL), "raw")
      full_import_html <- htmltools::renderTags(
        output$full_import_result
      )$html
      expect_match(
        full_import_html,
        paste(
          'class="llw-glc-full-import-result',
          'llw-glc-result-surface"'
        ),
        fixed = TRUE
      )
      expect_match(
        full_import_html,
        "Development fixture full import ready",
        fixed = TRUE
      )
      expect_match(
        full_import_html,
        "Complete collected handoff, not yet a dataset",
        fixed = TRUE
      )
      expect_match(
        full_import_html,
        "llw-glc-full-import-panel",
        fixed = TRUE
      )
      expect_match(
        full_import_html,
        "fixture://full-import-input/001",
        fixed = TRUE
      )
      expect_match(full_import_html, "Preview rows used", fixed = TRUE)
      expect_match(
        full_import_html,
        "No. Apply performed a fresh full read.",
        fixed = TRUE
      )
      completed_apply_button <- htmltools::renderTags(
        output$apply_full_import_button
      )$html
      expect_match(
        completed_apply_button,
        'disabled="disabled"',
        fixed = TRUE
      )
      expect_match(
        completed_apply_button,
        "already imported",
        fixed = TRUE
      )

      valid_review_fingerprint <- transfer_review$fingerprint
      for (invalid_budget in c(0, -1, 1.5, 10001, 1e6)) {
        session$setInputs(transfer_row_budget = invalid_budget)
        session$flushReact()
        expect_s3_class(
          returned$transfer_review_error(),
          "llw_validation_error"
        )
        expect_identical(
          returned$transfer_review()$fingerprint,
          valid_review_fingerprint
        )
        invalid_html <- htmltools::renderTags(output$transfer_review)$html
        expect_match(
          invalid_html,
          "Correct the preview-row budget",
          fixed = TRUE
        )
        expect_match(invalid_html, "Last valid preview retained", fixed = TRUE)
        expect_false(grepl("Unknown task state", invalid_html, fixed = TRUE))
        invalid_button <- htmltools::renderTags(
          output$transfer_preview_button
        )$html
        expect_match(invalid_button, 'disabled="disabled"', fixed = TRUE)
        apply_after_invalid <- htmltools::renderTags(
          output$apply_full_import_button
        )$html
        expect_match(
          apply_after_invalid,
          'disabled="disabled"',
          fixed = TRUE
        )
        expect_identical(returned$full_import(), first_full_import)
      }

      session$setInputs(transfer_row_budget = 10000L)
      session$flushReact()
      at_limit_review <- returned$transfer_review()
      expect_null(returned$transfer_review_error())
      expect_identical(at_limit_review$allocation$total_budget, 10000L)
      expect_identical(at_limit_review$allocation$maximum_rows, 10000)
      at_limit_html <- htmltools::renderTags(output$transfer_review)$html
      expect_match(at_limit_html, "10,000", fixed = TRUE)
      expect_match(at_limit_html, "does not cap network bytes", fixed = TRUE)

      session$setInputs(transfer_row_budget = 10001L)
      session$flushReact()
      expect_match(
        llw_public_message(returned$transfer_review_error()),
        "10,000",
        fixed = TRUE
      )
      expect_identical(
        returned$transfer_review()$fingerprint,
        at_limit_review$fingerprint
      )

      session$setInputs(transfer_row_budget = 4L)
      session$flushReact()
      transfer_review <- returned$transfer_review()
      expect_null(returned$transfer_review_error())
      expect_identical(transfer_review$allocation$total_budget, 4L)

      transfer_button_html <- htmltools::renderTags(
        output$transfer_preview_button
      )$html
      expect_false(grepl(
        'disabled="disabled"',
        transfer_button_html,
        fixed = TRUE
      ))

      expect_identical(
        runtime$submit(
          "transfer-capacity-blocker",
          function() invisible(NULL)
        ),
        "running"
      )
      session$setInputs(transfer_preview = 1L, .priority = "event")
      expect_true(returned$transfer_pending())
      expect_equal(calls$preview, 0L)
      Sys.sleep(1.05)
      session$elapse(1050)
      session$flushReact()
      expect_false(returned$transfer_pending())
      expect_identical(returned$transfer_status()$state, "error")
      expect_match(
        returned$transfer_status()$message,
        "still stopping",
        fixed = TRUE
      )
      expect_null(returned$transfer_preview())

      runtime$release("transfer-capacity-blocker")
      session$setInputs(transfer_preview = 2L, .priority = "event")
      flush_m1_promises(session)
      preview <- returned$transfer_preview()
      expect_equal(calls$preview, 1L)
      expect_s3_class(preview, "llw_glc_transfer_preview_result")
      expect_identical(
        preview$transfer_review_fingerprint,
        transfer_review$fingerprint
      )
      expect_identical(preview$reviewed$file_count, 2L)
      expect_identical(preview$actual$input_count, 2L)
      expect_identical(preview$actual$row_count, 4L)
      expect_lte(preview$actual$row_count, 4L)
      expect_true(preview$assurance$deterministic_fixture)
      expect_false(preview$assurance$measurement_contents_accessed)
      expect_false(preview$assurance$imported)
      expect_false(preview$assurance$persisted)
      expect_null(nonserializable_path(preview))
      expect_type(serialize(preview, NULL), "raw")
      preview_html <- htmltools::renderTags(
        output$transfer_preview_result
      )$html
      expect_match(
        preview_html,
        'class="llw-glc-transfer-preview-result mt-3"',
        fixed = TRUE
      )
      expect_match(
        preview_html,
        "Development fixture preview ready",
        fixed = TRUE
      )
      expect_match(preview_html, "not a dataset", ignore.case = TRUE)
      expect_match(
        preview_html,
        "No package or measurement file was opened",
        fixed = TRUE
      )
      expect_match(
        preview_html,
        "Reviewed live-package paths (not opened)",
        fixed = TRUE
      )
      expect_match(preview_html, "Actual fixture inputs", fixed = TRUE)
      expect_match(preview_html, "fixture://preview-input/001", fixed = TRUE)
      expect_match(preview_html, "Live measurement access", fixed = TRUE)
      expect_match(
        preview_html,
        "None. The reviewed package paths were not opened.",
        fixed = TRUE
      )
      expect_false(grepl("Actual files", preview_html, fixed = TRUE))

      session$setInputs(apply_full_import = 2L, .priority = "event")
      flush_m1_promises(session)
      second_full_import <- returned$full_import()
      expect_equal(calls$apply, 2L)
      expect_false("preview_result" %in% calls$apply_payload_names)
      expect_s3_class(second_full_import, "llw_glc_full_import_result")
      expect_identical(second_full_import$data, first_full_import$data)
      expect_identical(
        second_full_import$transfer_review_fingerprint,
        glc_full_import_contract_review(transfer_review)$fingerprint
      )
      expect_false(second_full_import$assurance$preview_result_used)

      session$setInputs(transfer_row_budget = 0)
      session$flushReact()
      expect_s3_class(
        returned$transfer_review_error(),
        "llw_validation_error"
      )
      expect_identical(
        returned$transfer_review()$fingerprint,
        transfer_review$fingerprint
      )
      expect_null(returned$transfer_preview())
      expect_identical(returned$full_import(), second_full_import)
      invalid_after_result_button <- htmltools::renderTags(
        output$transfer_preview_button
      )$html
      expect_match(
        invalid_after_result_button,
        'disabled="disabled"',
        fixed = TRUE
      )
      apply_after_result_invalid <- htmltools::renderTags(
        output$apply_full_import_button
      )$html
      expect_match(
        apply_after_result_invalid,
        'disabled="disabled"',
        fixed = TRUE
      )

      session$setInputs(transfer_row_budget = 4L)
      session$flushReact()
      expect_null(returned$transfer_review_error())
      expect_null(returned$transfer_preview())
      expect_identical(returned$full_import(), second_full_import)
      expect_identical(
        returned$transfer_review()$allocation$total_budget,
        4L
      )
      expect_identical(returned$transfer_review()$problems_policy, "error")
      expect_identical(
        returned$transfer_review()$fixed,
        transfer_review$fixed
      )

      prior_transfer_fingerprint <- transfer_review$fingerprint
      session$setInputs(
        transfer_row_budget = 2L,
        transfer_problems = "warn"
      )
      session$flushReact()
      changed_review <- returned$transfer_review()
      expect_s3_class(changed_review, "llw_glc_transfer_review")
      expect_false(identical(
        changed_review$fingerprint,
        prior_transfer_fingerprint
      ))
      expect_identical(changed_review$problems_policy, "warn")
      expect_identical(changed_review$allocation$total_budget, 2L)
      expect_identical(
        changed_review$projection,
        transfer_review$projection
      )
      changed_apply_review <- glc_full_import_contract_review(changed_review)
      expect_identical(
        glc_full_import_contract_fingerprint(changed_review),
        glc_full_import_contract_fingerprint(transfer_review)
      )
      expect_identical(
        changed_apply_review$problems_policy,
        "error"
      )
      expect_identical(
        changed_apply_review$allocation$total_budget,
        1L
      )
      expect_null(returned$transfer_preview())
      expect_identical(returned$full_import(), second_full_import)
      expect_equal(calls$preview, 1L)

      session$setInputs(transfer_preview = 3L)
      flush_m1_promises(session)
      expect_equal(calls$preview, 2L)
      expect_identical(
        returned$transfer_preview()$transfer_review_fingerprint,
        changed_review$fingerprint
      )
      changed_preview_html <- htmltools::renderTags(
        output$transfer_preview_result
      )$html
      expect_match(
        changed_preview_html,
        "1 parsed row per file",
        fixed = TRUE
      )
      expect_false(grepl(
        "1 parsed rows per file",
        changed_preview_html,
        fixed = TRUE
      ))
      expect_false(grepl("preview .", changed_preview_html, fixed = TRUE))
      expect_identical(returned$full_import(), second_full_import)

      prior_scope_revision <- returned$scope_input_revision()
      session$setInputs(term_id = "datetime")
      session$flushReact()
      expect_gt(returned$scope_input_revision(), prior_scope_revision)
      expect_null(returned$scope_review())
      expect_null(returned$transfer_review())
      expect_null(returned$transfer_preview())
      expect_null(returned$full_import())
      expect_null(returned$plan())

      session$setInputs(plan = 2L)
      flush_m1_promises(session)
      session$setInputs(refine = 2L)
      session$flushReact()
      expect_identical(returned$scope_mode(), "discovery")
      expect_identical(returned$scope_projection_variables(), "Datetime")
      expect_false("MEDI" %in% returned$scope_projection_variables())

      set_scope_input("scope_mode", "custom")
      set_scope_input("scope_custom_variables", character())
      expect_null(returned$scope_review())
      button_html <- htmltools::renderTags(output$scope_apply_button)$html
      expect_match(button_html, "disabled=\"disabled\"", fixed = TRUE)

      source(glc_preview_test_selection(
        repository = "tscnlab/melidos-knust-glc-dataset",
        revision = strrep("b", 40L)
      ))
      session$flushReact()
      expect_null(returned$scope_review())
      expect_null(returned$transfer_review())
      expect_null(returned$transfer_preview())
      expect_null(returned$full_import())
      expect_equal(calls$planning, 2L)
      reset_inspect_button <- htmltools::renderTags(
        output$inspect_button
      )$html
      expect_false(grepl(
        'disabled="disabled"',
        reset_inspect_button,
        fixed = TRUE
      ))
    }
  )
})

test_that("bounded preview workers release for immediate repeated retries", {
  skip_if_not_installed("mirai")
  runtime <- glc_preview_async_test_runtime()
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error")) {
    skip("mirai daemons are unavailable in this test runtime")
  }

  selection <- glc_preview_test_selection()
  inspection <- glc_preview_test_projection_inspection(
    selection,
    cache_dir = runtime$paths$cache
  )
  request <- new_glc_planning_request(
    term = "melanopic_edi",
    variable_scope = "all"
  )
  plan <- glc_preview_test_projection_plan(inspection, request)
  source <- shiny::reactiveVal(selection)
  preview_box <- new.env(parent = emptyenv())
  preview_box$value <- NULL
  apply_box <- new.env(parent = emptyenv())
  apply_box$value <- NULL

  inspection_worker <- local({
    value <- inspection
    function(payload, spec) value
  })
  planning_worker <- local({
    value <- plan
    function(payload, spec) value
  })
  preview_worker <- local({
    box <- preview_box
    function(payload, spec) {
      saveRDS(
        data.frame(
          phase = "read",
          label = "Reading the bounded fixture subset",
          status = "running",
          elapsed_seconds = NA_real_,
          detail = "The injected preview worker started successfully.",
          stringsAsFactors = FALSE
        ),
        payload$progress_path,
        version = 2
      )
      Sys.sleep(0.25)
      box$value
    }
  })
  apply_worker <- local({
    box <- apply_box
    function(payload, spec) {
      saveRDS(
        data.frame(
          phase = "read",
          label = "Reading every selected fixture input",
          status = "running",
          elapsed_seconds = NA_real_,
          detail = "The injected full-import worker started successfully.",
          stringsAsFactors = FALSE
        ),
        payload$progress_path,
        version = 2
      )
      Sys.sleep(0.25)
      box$value
    }
  })

  shiny::testServer(
    glcPreviewServer,
    args = list(
      source_selection = source,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      inspection_worker = inspection_worker,
      planning_worker = planning_worker,
      refine_collection = glc_preview_test_refinement,
      preview_worker = preview_worker,
      apply_worker = apply_worker,
      task_start_timeout_seconds = 2
    ),
    {
      returned <- session$getReturned()
      set_scope_input <- function(id, value) {
        input_id <- glc_scope_input_id(
          id,
          returned$scope_input_revision()
        )
        do.call(
          session$setInputs,
          stats::setNames(list(value), input_id)
        )
        session$flushReact()
        invisible(input_id)
      }

      session$flushReact()
      session$setInputs(inspect = 1L)
      glc_preview_wait_for(session, function() {
        identical(returned$inspection_status()$state, "complete")
      })
      session$setInputs(term_id = "melanopic_edi")
      session$setInputs(plan = 1L)
      glc_preview_wait_for(session, function() {
        identical(returned$plan_status()$state, "complete")
      })
      session$setInputs(refine = 1L)
      session$flushReact()
      set_scope_input("scope_mode", "custom")
      set_scope_input("scope_custom_variables", "MEDI")
      session$setInputs(scope_apply = 1L)
      session$setInputs(
        transfer_row_budget = 4L,
        transfer_problems = "error",
        continue_preview = 1L
      )
      session$flushReact()

      review <- returned$transfer_review()
      expect_s3_class(review, "llw_glc_transfer_review")
      source_spec <- glc_inspection_source_spec(
        returned$inspection(),
        runtime$paths$cache
      )
      payload <- new_glc_transfer_task_payload(source_spec, review)
      preview_box$value <- glc_preview_test_preview_worker()(payload, list())
      apply_payload <- new_glc_full_import_task_payload(source_spec, review)
      apply_box$value <- glc_preview_test_full_import_worker()(
        apply_payload,
        list()
      )
      htmltools::renderTags(output$transfer_preview_button)

      for (attempt in seq_len(2L)) {
        session$setInputs(
          transfer_preview = attempt * 10L + 1L,
          .priority = "event"
        )
        glc_preview_wait_for(session, returned$transfer_active)
        session$setInputs(cancel_transfer = attempt)
        expect_identical(returned$transfer_status()$state, "cancelled")
        expect_false(returned$transfer_active())
        expect_null(returned$transfer_preview())

        session$setInputs(
          transfer_preview = attempt * 10L + 2L,
          .priority = "event"
        )
        expect_true(returned$transfer_pending() || returned$transfer_active())
        glc_preview_wait_for(session, function() {
          identical(returned$transfer_status()$state, "complete")
        })
        expect_s3_class(
          returned$transfer_preview(),
          "llw_glc_transfer_preview_result"
        )
        expect_identical(
          returned$transfer_preview()$transfer_review_fingerprint,
          review$fingerprint
        )
      }

      expect_equal(runtime$active_count(), 0L)
      expect_false(returned$transfer_pending())
      expect_false(returned$transfer_active())

      htmltools::renderTags(output$apply_full_import_button)
      for (attempt in seq_len(2L)) {
        session$setInputs(
          apply_full_import = attempt * 10L + 1L,
          .priority = "event"
        )
        glc_preview_wait_for(session, returned$apply_active)
        session$setInputs(cancel_apply = attempt)
        expect_identical(returned$apply_status()$state, "cancelled")
        expect_false(returned$apply_active())
        if (attempt == 1L) {
          expect_null(returned$full_import())
        } else {
          expect_s3_class(
            returned$full_import(),
            "llw_glc_full_import_result"
          )
          retained_html <- htmltools::renderTags(
            output$full_import_result
          )$html
          expect_match(
            retained_html,
            "Last successful full import retained",
            fixed = TRUE
          )
        }

        session$setInputs(
          apply_full_import = attempt * 10L + 2L,
          .priority = "event"
        )
        expect_true(returned$apply_pending() || returned$apply_active())
        glc_preview_wait_for(session, function() {
          identical(returned$apply_status()$state, "complete")
        })
        expect_s3_class(
          returned$full_import(),
          "llw_glc_full_import_result"
        )
        expect_identical(
          returned$full_import()$transfer_review_fingerprint,
          review$fingerprint
        )
        expect_false(returned$full_import()$assurance$preview_result_used)
      }

      expect_equal(runtime$active_count(), 0L)
      expect_false(returned$apply_pending())
      expect_false(returned$apply_active())
    }
  )
})

test_that("variable projection choices never invent a matched modality scope", {
  term_choices <- glc_scope_mode_choices(new_glc_planning_request(
    term = "melanopic_edi",
    variable_scope = "all"
  ))
  modality_choices <- glc_scope_mode_choices(new_glc_planning_request(
    modality = "light",
    file_group_ids = c("dataset-a:1", "dataset-b:1"),
    variable_scope = "all"
  ))

  expect_identical(
    term_choices$values,
    c("discovery", "all", "custom")
  )
  expect_identical(modality_choices$values, c("all", "custom"))
  expect_false("matched" %in% c(term_choices$values, modality_choices$values))
})
