test_that("immutable metadata indexes nested and unknown source fields", {
  source <- new_metadata_source_namespace(
    namespace_id = "source-a",
    source_dataset_id = "dataset-source-a",
    label = "Source A",
    source_type = "test",
    records = list(
      study = data.frame(
        study_id = "study-a",
        title = "Nested source study",
        stringsAsFactors = FALSE
      ),
      extension_resource = list(
        unknown_vendor_block = list(
          relationship = "device-a",
          custom_code = "retained-unknown-value"
        )
      )
    ),
    state = "complete",
    source_schema_version = "3.0.2"
  )
  bundle <- new_metadata_bundle(stats::setNames(list(source), "source-a"))
  result <- metadata_index_query(bundle$index, "retained-unknown-value")

  expect_identical(result$total, 1L)
  expect_identical(result$rows$field, "custom_code")
  expect_match(result$rows$path, "unknown_vendor_block", fixed = TRUE)
  expect_identical(result$rows$layer, "Immutable source")
  expect_identical(result$rows$source_dataset_id, "dataset-source-a")

  tampered <- source
  tampered$records$study$title <- "Changed behind the fingerprint"
  expect_error(
    validate_metadata_source_namespace(tampered),
    class = "llw_validation_error",
    regexp = "fingerprint"
  )
})

test_that("search is limited to field names and retained values", {
  source <- new_metadata_source_namespace(
    namespace_id = "source-context",
    source_dataset_id = "dataset-context",
    label = "MEDI contextual source",
    source_type = "test",
    records = list(
      study = list(
        title = "Unrelated title",
        primary_variables = "MEDI"
      )
    ),
    state = "complete"
  )
  bundle <- new_metadata_bundle(stats::setNames(
    list(source),
    source$namespace_id
  ))
  result <- metadata_index_query_groups(bundle$index, "medi")

  expect_identical(result$total, 1L)
  expect_identical(result$rows$field, "primary_variables")
  expect_false(any(grepl(
    "contextual source",
    bundle$index$entries$search_text,
    fixed = TRUE
  )))
  expect_identical(
    metadata_search_text("participant_age", "31"),
    "participant_age participant age 31"
  )
})

test_that("typed overlays remain separate, searchable, and source-bound", {
  source <- new_metadata_source_namespace(
    namespace_id = "source-a",
    label = "Immutable package",
    source_type = "test",
    records = list(study = list(title = "Immutable title")),
    state = "complete"
  )
  base <- new_metadata_bundle(stats::setNames(list(source), "source-a"))
  source_fingerprint <- base$sources[[1L]]$fingerprint
  tables <- metadata_empty_overlay_tables()
  tables$datasets <- data.frame(
    dataset_id = "dataset-a",
    title = "Overlay display title",
    stringsAsFactors = FALSE
  )
  tables$sites <- data.frame(
    dataset_id = "dataset-a",
    site = "Munich",
    country = "DE",
    source_timezone = "Europe/Berlin",
    latitude = 48.137,
    longitude = 11.575,
    stringsAsFactors = FALSE
  )
  tables$variables <- data.frame(
    dataset_id = "dataset-a",
    file_group_id = "dataset-a:1",
    name = "MEDI",
    label = "Melanopic EDI",
    unit = "lx",
    stringsAsFactors = FALSE
  )
  overlay <- new_metadata_overlay(
    source_fingerprint = base$source_fingerprint,
    tables = tables,
    revision = 2L,
    unknown = list(custom = list(relation = "device-a"))
  )
  bundle <- new_metadata_bundle(base$sources, overlay = overlay)
  source_result <- metadata_index_query(bundle$index, "immutable title")
  overlay_result <- metadata_index_query(bundle$index, "melanopic edi")

  expect_identical(bundle$sources[[1L]]$fingerprint, source_fingerprint)
  expect_identical(source_result$rows$layer, "Immutable source")
  expect_identical(overlay_result$total, 1L)
  expect_identical(overlay_result$rows$layer, "LightLogWeb overlay")
  expect_identical(
    overlay_result$rows$namespace_id,
    metadata_overlay_namespace_id()
  )
  expect_identical(
    metadata_index_query(
      bundle$index,
      layer = "LightLogWeb overlay"
    )$total,
    sum(bundle$index$entries$layer == "LightLogWeb overlay")
  )

  mismatched <- overlay
  mismatched$source_fingerprint <- paste0("sha256:", strrep("a", 64L))
  expect_error(
    new_metadata_bundle(base$sources, overlay = mismatched),
    class = "llw_validation_error",
    regexp = "does not match"
  )
})

test_that("overlay tables reject invalid types, identities, zones, and coordinates", {
  fingerprint <- paste0("sha256:", strrep("b", 64L))
  tables <- metadata_empty_overlay_tables()
  tables$sites <- data.frame(
    dataset_id = "dataset-a",
    site = "Site",
    country = "DE",
    source_timezone = "Not/A_Timezone",
    latitude = 48,
    longitude = 11,
    stringsAsFactors = FALSE
  )
  expect_error(
    new_metadata_overlay(fingerprint, tables),
    class = "llw_validation_error",
    regexp = "IANA timezone"
  )

  tables$sites$source_timezone <- "UTC"
  tables$sites$latitude <- 91
  expect_error(
    new_metadata_overlay(fingerprint, tables),
    class = "llw_validation_error",
    regexp = "coordinates"
  )

  tables <- metadata_empty_overlay_tables()
  tables$datasets <- data.frame(
    dataset_id = c("dataset-a", "dataset-a"),
    title = c("First", "Second"),
    stringsAsFactors = FALSE
  )
  expect_error(
    new_metadata_overlay(fingerprint, tables),
    class = "llw_validation_error",
    regexp = "duplicate identities"
  )

  tables <- metadata_empty_overlay_tables()
  tables$devices <- data.frame(
    device_id = factor("device-a"),
    manufacturer = "Example",
    model = "One"
  )
  expect_error(
    new_metadata_overlay(fingerprint, tables),
    class = "llw_validation_error",
    regexp = "must be character"
  )
})

test_that("repeated metadata records collapse into one stable field group", {
  source <- new_metadata_source_namespace(
    namespace_id = "source-participants",
    source_dataset_id = "dataset-participants",
    label = "Participant source",
    source_type = "test",
    records = list(
      participants = data.frame(
        participant_internal_id = c("P01", "P02", "P03"),
        participant_age = c(22, 35, 35),
        stringsAsFactors = FALSE
      )
    ),
    state = "complete"
  )
  bundle <- new_metadata_bundle(stats::setNames(
    list(source),
    source$namespace_id
  ))
  result <- metadata_index_query_groups(bundle$index, "participant_age")
  selected <- metadata_index_group(bundle$index, result$rows$group_id)

  expect_identical(result$total, 1L)
  expect_identical(result$rows$entry_count, 3L)
  expect_identical(result$rows$record_count, 3L)
  expect_identical(result$rows$distinct_value_count, 2L)
  expect_identical(selected$entries$value, c("22", "35", "35"))
  expect_match(selected$entries$path, "participants\\[[123]\\]/participant_age")
  expect_identical(
    unique(selected$entries$group_path),
    result$rows$group_path
  )
  expect_identical(
    unique(selected$entries$group_id),
    result$rows$group_id
  )
})

test_that("relationship contexts are versioned, scoped, and tamper evident", {
  context <- new_metadata_relationship_context(
    file_groups = data.frame(
      file_group_id = c("D1:1", "D2:1"),
      dataset_internal_id = c("D1", "D2"),
      participant_internal_id = c("P1", "P2"),
      stringsAsFactors = FALSE
    ),
    datasets = data.frame(
      Id = c("D1", "D2"),
      study_title = c("Study", "Study"),
      stringsAsFactors = FALSE
    ),
    requested_fields = c("participant_internal_id", "study_title"),
    provenance = list(
      extractor = "glcdp::extract_metadata",
      measurement_contents_read = FALSE
    )
  )
  scoped <- metadata_relationship_subset(context, "D2:1", "D2")

  expect_s3_class(context, "llw_metadata_relationships")
  expect_identical(context$relationship_version, "1.0.0")
  expect_identical(scoped$file_groups$file_group_id, "D2:1")
  expect_identical(scoped$datasets$Id, "D2")
  expect_false(scoped$provenance$measurement_contents_read)
  expect_null(nonserializable_path(scoped))

  source <- new_metadata_source_namespace(
    namespace_id = "source-relationships",
    label = "Relationship source",
    source_type = "test",
    records = list(
      core_metadata = list(study = list(study_title = "Exact source title")),
      relationship_context = context
    )
  )
  bundle <- new_metadata_bundle(list(`source-relationships` = source))
  expect_false(any(
    bundle$index$entries$resource == "relationship_context"
  ))
  expect_identical(
    metadata_index_query_groups(bundle$index, "Exact source title")$total,
    1L
  )
  outer <- new_metadata_source_namespace(
    namespace_id = "source-outer",
    label = "Outer appended source",
    source_type = "append_source",
    records = list(metadata_bundle = bundle)
  )
  nested_record <- new_dataset_record(
    data.frame(
      Id = "D1",
      Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
      MEDI = 1,
      stringsAsFactors = FALSE
    ),
    "Nested relationships",
    new_source_manifest("test", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI"),
    metadata_bundle = new_metadata_bundle(list(`source-outer` = outer))
  )
  nested_contexts <- metadata_relationships_for_record(nested_record)
  expect_named(nested_contexts, "source-outer")
  expect_identical(nested_contexts[[1L]]$fingerprint, context$fingerprint)

  tampered <- context
  tampered$file_groups$participant_internal_id[[1L]] <- "changed"
  expect_error(
    validate_metadata_relationship_context(tampered),
    class = "llw_validation_error",
    regexp = "fingerprint"
  )
})

test_that("metadata focus requests resolve exact groups and record context", {
  source <- new_metadata_source_namespace(
    namespace_id = "source-focus",
    label = "Focus source",
    source_type = "test",
    records = list(
      participants = data.frame(
        participant_internal_id = c("P01", "P02"),
        participant_age = c(31, 44),
        stringsAsFactors = FALSE
      ),
      variable_inventory = data.frame(
        dataset_id = c("D1", "D1"),
        file_group_id = c("D1:1", "D1:1"),
        name = c("LIGHT", "MEDI"),
        label = c("Photopic illuminance", "Melanopic EDI"),
        stringsAsFactors = FALSE
      )
    )
  )
  bundle <- new_metadata_bundle(list(`source-focus` = source))
  request <- new_metadata_focus_request(
    dataset_id = "dataset-focus",
    namespace_id = "source-focus",
    field = "participant_internal_id",
    path_contains = "/participants",
    record_identity = "P02",
    label = "Participant P02"
  )
  resolved <- metadata_resolve_focus_request(bundle, request)

  expect_identical(resolved$state, "resolved")
  expect_match(resolved$group_id, "^metadata_group_")
  expect_match(resolved$node_id, "^metadata_node_")
  expect_null(nonserializable_path(request))

  variable_request <- new_metadata_focus_request(
    dataset_id = "dataset-focus",
    namespace_id = "source-focus",
    field = "name",
    path_contains = "/variable_inventory",
    record_identity = "MEDI",
    label = "Variable MEDI"
  )
  variable_resolution <- metadata_resolve_focus_request(
    bundle,
    variable_request
  )
  expect_identical(variable_resolution$state, "resolved")
  expect_match(variable_resolution$node_id, "^metadata_node_")

  missing_identity <- variable_request
  missing_identity$record_identity <- "NOT_PRESENT"
  missing_identity <- do.call(
    new_metadata_focus_request,
    missing_identity[c(
      "dataset_id",
      "namespace_id",
      "field",
      "path_contains",
      "record_identity",
      "group_id",
      "label",
      "request_id"
    )]
  )
  expect_identical(
    metadata_resolve_focus_request(bundle, missing_identity)$state,
    "unavailable"
  )

  unavailable <- request
  unavailable$field <- "not_a_field"
  unavailable <- do.call(
    new_metadata_focus_request,
    unavailable[c(
      "dataset_id",
      "namespace_id",
      "field",
      "path_contains",
      "record_identity",
      "group_id",
      "label",
      "request_id"
    )]
  )
  expect_identical(
    metadata_resolve_focus_request(bundle, unavailable)$state,
    "unavailable"
  )
})

test_that("variable resolution preserves raw identity and overlay provenance", {
  source <- new_metadata_source_namespace(
    namespace_id = "source-variable",
    label = "Variable source",
    source_type = "test",
    records = list(
      variable_inventory = data.frame(
        dataset_id = "D1",
        file_group_id = "D1:1",
        name = "MEDI",
        label = "Source melanopic label",
        unit = "lx",
        calibration = "Source calibration",
        stringsAsFactors = FALSE
      )
    )
  )
  base <- new_metadata_bundle(list(`source-variable` = source))
  tables <- metadata_empty_overlay_tables()
  tables$variables <- data.frame(
    dataset_id = "D1",
    file_group_id = "D1:1",
    name = "MEDI",
    label = "Overlay melanopic label",
    unit = "melanopic lux",
    stringsAsFactors = FALSE
  )
  bundle <- new_metadata_bundle(
    base$sources,
    new_metadata_overlay(base$source_fingerprint, tables, revision = 1L)
  )
  data <- data.frame(
    Id = "D1",
    Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
    MEDI = 10,
    stringsAsFactors = FALSE
  )
  record <- new_dataset_record(
    data,
    "Resolver fixture",
    new_source_manifest("test", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI"),
    metadata_bundle = bundle,
    id = "dataset_resolver"
  )
  resolved <- metadata_resolve_variable(record, "MEDI")

  expect_identical(resolved$raw_name, "MEDI")
  expect_identical(resolved$display_label, "Overlay melanopic label")
  expect_identical(resolved$unit, "melanopic lux")
  expect_identical(resolved$provenance, "LightLogWeb overlay")
  expect_identical(resolved$calibration, "Source calibration")
  expect_identical(resolved$calibration_provenance, "Immutable source")
  expect_s3_class(resolved$target, "llw_metadata_focus_request")

  absent <- metadata_resolve_variable(record, "unknown")
  expect_identical(absent$display_label, "unknown")
  expect_identical(absent$unit, "unit not specified")
  expect_identical(absent$provenance, "Not specified")
})

test_that("appended overlay resolution stays inside its exact source mapping", {
  variable_source <- function(namespace_id, dataset_id, group_id, unit) {
    new_metadata_source_namespace(
      namespace_id = namespace_id,
      label = namespace_id,
      source_type = "test",
      records = list(
        variable_inventory = data.frame(
          dataset_id = dataset_id,
          file_group_id = group_id,
          name = "MEDI",
          label = paste(namespace_id, "source label"),
          unit = unit,
          stringsAsFactors = FALSE
        )
      )
    )
  }
  sources <- list(
    source_a = variable_source("source_a", "D1", "D1:1", "lx"),
    source_b = variable_source("source_b", "D2", "D2:1", "melanopic lux")
  )
  base <- new_metadata_bundle(sources)
  tables <- metadata_empty_overlay_tables()
  tables$variables <- data.frame(
    dataset_id = c("D1", "D2"),
    file_group_id = c("D1:1", "D2:1"),
    name = c("MEDI", "MEDI"),
    label = c("Overlay A", "Overlay B"),
    unit = c("unit A", "unit B"),
    stringsAsFactors = FALSE
  )
  bundle <- new_metadata_bundle(
    sources,
    new_metadata_overlay(base$source_fingerprint, tables, revision = 1L)
  )
  record <- new_dataset_record(
    data.frame(
      Id = "P01",
      Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
      a_MEDI = 1,
      b_MEDI = 2,
      stringsAsFactors = FALSE
    ),
    "Mapped overlays",
    new_source_manifest("test", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "a_MEDI"),
    metadata_bundle = bundle,
    provenance = list(
      append_merge = list(
        source_records = list(
          list(
            dataset_id = "source_a",
            mapping = list(
              measurement_target = "a_MEDI",
              measurement_column = "MEDI"
            )
          ),
          list(
            dataset_id = "source_b",
            mapping = list(
              measurement_target = "b_MEDI",
              measurement_column = "MEDI"
            )
          )
        )
      )
    )
  )

  resolved_a <- metadata_resolve_variable(record, "a_MEDI")
  resolved_b <- metadata_resolve_variable(record, "b_MEDI")
  expect_identical(resolved_a$display_label, "Overlay A")
  expect_identical(resolved_a$unit, "unit A")
  expect_false(resolved_a$conflict)
  expect_identical(resolved_b$display_label, "Overlay B")
  expect_identical(resolved_b$unit, "unit B")
  expect_false(resolved_b$conflict)
})

test_that("semantic metadata rows group peers without collecting siblings", {
  variable_rows <- function(dataset_id, descriptions) {
    data.frame(
      dataset_id = dataset_id,
      file_group_id = paste0(dataset_id, ":1"),
      name = c("LIGHT", "MEDI"),
      label = c("Photopic illuminance", "Melanopic EDI"),
      description = descriptions,
      stringsAsFactors = FALSE
    )
  }
  source <- new_metadata_source_namespace(
    namespace_id = "source-variables",
    source_dataset_id = "dataset-variables",
    label = "Variable source",
    source_type = "test",
    records = list(
      variable_inventory = rbind(
        variable_rows("dataset-a", c("Visible light", "Melanopic EDI")),
        variable_rows("dataset-b", c("Visible light", "Melanopic EDI"))
      )
    ),
    state = "complete"
  )
  bundle <- new_metadata_bundle(stats::setNames(
    list(source),
    source$namespace_id
  ))
  result <- metadata_index_query_groups(bundle$index, "melanopic edi")
  description <- result$rows[result$rows$field == "description", , drop = FALSE]
  selected <- metadata_index_group(bundle$index, description$group_id[[1L]])

  expect_identical(nrow(description), 1L)
  expect_identical(description$entry_count, 2L)
  expect_identical(description$record_count, 2L)
  expect_identical(description$group_context, "MEDI")
  expect_identical(
    selected$entries$record,
    c("dataset-a:1", "dataset-b:1")
  )
  expect_identical(selected$entries$value, c("Melanopic EDI", "Melanopic EDI"))
  expect_false(any(selected$entries$value == "Visible light"))
})

test_that("metadata neighborhoods expose exact sibling properties", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(
    bundle$index,
    "VLSQ8",
    limit = 100L
  )
  name_row <- result$rows[
    result$rows$field == "participant_characteristic_name" &
      result$rows$group_context == "VLSQ8",
    ,
    drop = FALSE
  ]
  group <- metadata_index_group(bundle$index, name_row$group_id[[1L]])
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)

  expect_s3_class(neighborhood, "llw_metadata_neighborhood")
  expect_identical(nrow(neighborhood$item_nodes), 5L)
  expect_identical(
    neighborhood$item_nodes$instance_record,
    paste0("TUM_S00", 1:5)
  )
  expect_setequal(
    neighborhood$property_groups$field,
    c(
      "participant_internal_id",
      "participant_characteristic_name",
      "participant_characteristic_value",
      "participant_characteristic_unit",
      "participant_characteristic_description"
    )
  )
  value_group_id <- neighborhood$property_groups$group_id[
    neighborhood$property_groups$field == "participant_characteristic_value"
  ][[1L]]
  values <- metadata_index_group(bundle$index, value_group_id)$entries$value
  expect_identical(values, c("23", "18", "20", "30", "21"))
  expect_true(all(neighborhood$item_nodes$kind == "record"))
})

test_that("hierarchy keeps terms separate from concrete variable units", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  term_result <- metadata_index_query_groups(
    bundle$index,
    "melanopic EDI",
    limit = 100L
  )
  term_row <- term_result$rows[
    term_result$rows$field == "label" &
      term_result$rows$group_context == "melanopic_edi",
    ,
    drop = FALSE
  ]
  term_group <- metadata_index_group(
    bundle$index,
    term_row$group_id[[1L]]
  )
  term_neighborhood <- metadata_index_group_neighborhood(
    bundle$index,
    term_group
  )
  term_context <- metadata_index_node_context(
    bundle$index,
    term_neighborhood$representative_node_id
  )

  expect_setequal(term_neighborhood$property_groups$field, c("term", "label"))
  expect_false(any(grepl("unit", term_neighborhood$property_groups$field)))
  expect_true(any(
    term_context$nearby$kind == "table" &
      term_context$nearby$label == "dataset_file"
  ))

  variable_result <- metadata_index_query_groups(
    bundle$index,
    "dataset_file_variables_name",
    limit = 100L
  )
  medi_row <- variable_result$rows[
    variable_result$rows$group_context == "MEDI",
    ,
    drop = FALSE
  ]
  medi_group <- metadata_index_group(
    bundle$index,
    medi_row$group_id[[1L]]
  )
  medi_neighborhood <- metadata_index_group_neighborhood(
    bundle$index,
    medi_group
  )
  unit_group_id <- medi_neighborhood$property_groups$group_id[
    medi_neighborhood$property_groups$field == "dataset_file_variables_units"
  ][[1L]]
  units <- metadata_index_group(bundle$index, unit_group_id)$entries$value

  expect_true(
    "dataset_file_variables_units" %in%
      medi_neighborhood$property_groups$field
  )
  expect_identical(unique(units), "lx")
  expect_false(unit_group_id %in% term_neighborhood$property_groups$group_id)
})

test_that("hierarchy focus traverses retained parents and children", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  term_result <- metadata_index_query_groups(
    bundle$index,
    "melanopic EDI",
    limit = 100L
  )
  term_row <- term_result$rows[
    term_result$rows$field == "label" &
      term_result$rows$group_context == "melanopic_edi",
    ,
    drop = FALSE
  ]
  term_group <- metadata_index_group(
    bundle$index,
    term_row$group_id[[1L]]
  )
  neighborhood <- metadata_index_group_neighborhood(bundle$index, term_group)
  term_context <- metadata_index_node_context(
    bundle$index,
    neighborhood$representative_node_id
  )
  dataset_file_id <- term_context$nearby$node_id[
    term_context$nearby$kind == "table" &
      term_context$nearby$label == "dataset_file"
  ][[1L]]
  dataset_file <- metadata_index_node_context(bundle$index, dataset_file_id)
  file_record <- metadata_index_node_context(
    bundle$index,
    dataset_file$children$node_id[[1L]]
  )
  variable_table_id <- file_record$children$node_id[
    file_record$children$label == "dataset_file_variables"
  ][[1L]]
  variable_table <- metadata_index_node_context(
    bundle$index,
    variable_table_id
  )
  medi_node_id <- variable_table$children$node_id[
    variable_table$children$label == "MEDI"
  ][[1L]]
  medi_context <- metadata_index_node_context(bundle$index, medi_node_id)

  expect_identical(
    utils::tail(
      medi_context$ancestors$node_id,
      1L
    ),
    medi_node_id
  )
  expect_true("dataset_file_variables_units" %in% medi_context$groups$field)
  expect_true(any(
    medi_context$entries$field == "dataset_file_variables_units" &
      medi_context$entries$value == "lx"
  ))
})

test_that("appended sources retain exact nested file-group identities", {
  records <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )
  bundle <- records[["dataset_m7appendedreview"]]$metadata_bundle
  expected_records <- c(
    "MELIDOS_IZTECH_S001:18",
    "MELIDOS_IZTECH_S002:18",
    "MELIDOS_IZTECH_S003:18",
    "MELIDOS_IZTECH_S004:19",
    "MELIDOS_IZTECH_S004:20",
    "MELIDOS_IZTECH_S005:18"
  )
  device_rows <- metadata_index_query_groups(
    bundle$index,
    "dataset_file_crossref_device_id",
    limit = 100L
  )$rows
  device_row <- device_rows[
    grepl("IZTECH", device_rows$source_label, fixed = TRUE),
    ,
    drop = FALSE
  ]
  device_group <- metadata_index_group(
    bundle$index,
    device_row$group_id[[1L]]
  )
  device_neighborhood <- metadata_index_group_neighborhood(
    bundle$index,
    device_group
  )

  expect_identical(device_group$entries$record, expected_records)
  expect_identical(
    device_group$entries$value,
    c(
      "IZTECH_AL03",
      "IZTECH_AL03",
      "IZTECH_AL05_FW1_7",
      "IZTECH_AL03",
      "IZTECH_AL04",
      "IZTECH_AL06_FW1_7"
    )
  )
  expect_identical(
    device_neighborhood$item_nodes$instance_record,
    expected_records
  )
  expect_identical(
    device_neighborhood$item_nodes$record_identity,
    expected_records
  )

  variable_rows <- metadata_index_query_groups(
    bundle$index,
    "dataset_file_variables_name",
    limit = 100L
  )$rows
  medi_row <- variable_rows[
    grepl("IZTECH", variable_rows$source_label, fixed = TRUE) &
      variable_rows$group_context == "MEDI",
    ,
    drop = FALSE
  ]
  medi_group <- metadata_index_group(
    bundle$index,
    medi_row$group_id[[1L]]
  )
  medi_neighborhood <- metadata_index_group_neighborhood(
    bundle$index,
    medi_group
  )
  s004_19 <- which(
    medi_neighborhood$item_nodes$record_identity == "MELIDOS_IZTECH_S004:19"
  )[[1L]]
  context <- metadata_index_node_context(
    bundle$index,
    medi_neighborhood$item_nodes$node_id[[s004_19]]
  )

  expect_identical(medi_group$entries$record, expected_records)
  expect_identical(
    medi_neighborhood$item_nodes$instance_record,
    expected_records
  )
  expect_true("MELIDOS_IZTECH_S004:19" %in% context$ancestors$label)
  expect_true(
    "MELIDOS_IZTECH_S004:19" %in% context$ancestors$record_identity
  )

  single <- m7_fixture("iztech-light-subset")$metadata_snapshot$bundle
  single_rows <- metadata_index_query_groups(
    single$index,
    "dataset_file_crossref_device_id",
    limit = 100L
  )$rows
  single_group <- metadata_index_group(
    single$index,
    single_rows$group_id[[1L]]
  )
  expect_identical(single_group$entries$record, expected_records)
})

test_that("search, filtering, sorting, and selection use stable in-memory groups", {
  bundle <- m7_fixture("ucr-all-light-variables")$metadata_snapshot$bundle
  all_rows <- metadata_index_query_groups(bundle$index, limit = 10000L)
  search <- metadata_index_query_groups(
    bundle$index,
    query = "medi",
    limit = 10000L
  )
  resource <- search$rows$resource[[1L]]
  filtered <- metadata_index_query_groups(
    bundle$index,
    query = "medi",
    resource = resource,
    sort_by = "field",
    limit = 10000L
  )
  reversed <- metadata_index_query_groups(
    bundle$index,
    query = "medi",
    resource = resource,
    sort_by = "field",
    descending = TRUE,
    limit = 10000L
  )
  selected <- metadata_index_group(
    bundle$index,
    filtered$rows$group_id[[1L]]
  )

  expect_gt(all_rows$total, search$total)
  expect_gt(search$total, 0L)
  expect_true(all(filtered$rows$resource == resource))
  expect_identical(reversed$rows$group_id, rev(filtered$rows$group_id))
  expect_identical(
    selected$group$group_id,
    filtered$rows$group_id[[1L]]
  )
  expect_true(all(selected$entries$group_id == selected$group$group_id))
})

test_that("metadata-free datasets retain raw-name and unit fallbacks", {
  record <- m1_record("No optional metadata")
  bundle <- metadata_bundle_for_record(record)

  expect_identical(bundle$sources[[1L]]$state, "absent")
  expect_identical(metadata_display_label("MEDI"), "MEDI")
  expect_identical(metadata_display_unit(), "unit not specified")
  expect_identical(
    metadata_display_label("MEDI", "Melanopic EDI"),
    "Melanopic EDI"
  )
  expect_identical(metadata_display_unit("lx"), "lx")
  expect_identical(dataset_raw_data(record), m1_fixture_data())
})

test_that("Milestone 6 record recovery carries the exact metadata bundle", {
  record <- m7_fixture_record("tum-medi-only")
  fingerprint <- record$metadata_bundle$source_fingerprint
  transported <- unserialize(serialize(record, NULL, version = 3))
  model <- session_add_dataset(new_session_model("hosted"), transported)
  recovered <- session_dataset(
    unserialize(serialize(model, NULL, version = 3)),
    record$id
  )

  expect_identical(recovered$metadata_bundle$source_fingerprint, fingerprint)
  expect_identical(recovered$metadata_bundle, record$metadata_bundle)

  draft <- stage_dataset_draft(
    recovered,
    list(analysis_settings = list(display_scale = "linear"))
  )
  preview <- preview_dataset_draft(
    draft,
    function(raw_data, recipe, analysis_settings, factual_metadata) raw_data
  )
  applied <- apply_dataset_draft(preview)
  reset <- reset_dataset_record(applied)
  undone <- undo_dataset_record(reset)

  expect_identical(applied$metadata_bundle, record$metadata_bundle)
  expect_identical(reset$metadata_bundle, record$metadata_bundle)
  expect_identical(undone$metadata_bundle, record$metadata_bundle)
})

test_that("append metadata keeps exact source namespaces and lineage", {
  first <- m7_fixture_record("tum-medi-only", "TUM fixture")
  second <- m7_fixture_record("iztech-light-subset", "IZTECH fixture")
  third <- m1_record("Metadata-free fixture")
  provenance <- lapply(list(first, second, third), function(record) {
    list(
      dataset_id = record$id,
      display_name = record$display_name,
      revision = record$revision,
      raw_checksum = record$raw_checksum,
      source_manifest = record$source_manifest,
      factual_metadata = record$factual_metadata,
      metadata_bundle = record$metadata_bundle
    )
  })
  bundle <- new_appended_metadata_bundle(provenance)

  expect_identical(names(bundle$sources), c(first$id, second$id, third$id))
  expect_identical(
    unname(vapply(
      bundle$sources,
      `[[`,
      character(1),
      "source_dataset_id"
    )),
    c(first$id, second$id, third$id)
  )
  expect_identical(
    bundle$sources[[first$id]]$records$metadata_bundle$source_fingerprint,
    first$metadata_bundle$source_fingerprint
  )
  expect_identical(
    bundle$sources[[second$id]]$records$metadata_bundle$source_fingerprint,
    second$metadata_bundle$source_fingerprint
  )
  expect_identical(
    vapply(bundle$sources, `[[`, character(1), "state"),
    c(
      setNames("complete", first$id),
      setNames("partial", second$id),
      setNames("absent", third$id)
    )
  )
})
