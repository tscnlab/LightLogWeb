metadata_test_hierarchy_panel <- function(
  group,
  bundle,
  focus_node_id = NULL,
  input_ids = NULL
) {
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)
  if (is.null(focus_node_id)) {
    focus_node_id <- neighborhood$representative_node_id
  }
  context <- metadata_index_node_context(bundle$index, focus_node_id)
  metadata_hierarchy_panel_ui(
    group,
    context,
    neighborhood,
    input_ids = input_ids
  )
}

test_that("metadata explorer presents discoverable source and overlay context", {
  ui <- htmltools::renderTags(metadata_explorer_ui("metadata"))$html
  complete <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  complete_html <- htmltools::renderTags(metadata_state_callout(complete))$html
  layer_html <- htmltools::renderTags(metadata_source_overlay_ui(complete))$html

  expect_match(ui, "Explore metadata", fixed = TRUE)
  expect_match(ui, "Search field names and values", fixed = TRUE)
  expect_match(ui, "Clear search", fixed = TRUE)
  expect_match(ui, "explore its properties", fixed = TRUE)
  expect_match(ui, "metadata-dataset_identity", fixed = TRUE)
  expect_match(ui, "metadata-filter_grid", fixed = TRUE)
  expect_match(ui, "metadata-hierarchy_shell", fixed = TRUE)
  expect_lt(
    regexpr("metadata-hierarchy_shell", ui, fixed = TRUE)[[1L]],
    regexpr("metadata-entry_detail", ui, fixed = TRUE)[[1L]]
  )
  expect_match(complete_html, "does not revalidate", fixed = TRUE)
  expect_match(layer_html, "Immutable source", fixed = TRUE)
  expect_match(layer_html, "LightLogWeb overlay", fixed = TRUE)
  expect_match(layer_html, "never.*silently reconciled")
  ui_source <- paste(deparse(body(metadata_explorer_ui)), collapse = "\n")
  expect_false(grepl("tags\\$script|<script", ui_source))
})

test_that("metadata hierarchy starts folded above the selected field", {
  shell <- htmltools::renderTags(
    metadata_hierarchy_shell_ui(NS("metadata"))
  )$html

  expect_match(shell, "Explore metadata structure", fixed = TRUE)
  expect_match(shell, "metadata-hierarchy_summary", fixed = TRUE)
  expect_match(shell, "metadata-hierarchy_content", fixed = TRUE)
  expect_match(shell, "aria-expanded=\"false\"", fixed = TRUE)
  expect_false(grepl("accordion-collapse collapse show", shell, fixed = TRUE))
})

test_that("filter grid contains only controls relevant to the record", {
  single <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  appended <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )[["dataset_m7appendedreview"]]$metadata_bundle
  single_html <- htmltools::renderTags(metadata_filter_grid_ui(
    single,
    NS("metadata")
  ))$html
  appended_html <- htmltools::renderTags(metadata_filter_grid_ui(
    appended,
    NS("metadata")
  ))$html
  grid_item_count <- function(html) {
    length(regmatches(
      html,
      gregexpr("bslib-grid-item", html, fixed = TRUE)
    )[[1L]])
  }
  stylesheet <- system.file(
    "app",
    "scss",
    "lightlogweb.scss",
    package = "LightLogWeb",
    mustWork = TRUE
  )
  scss <- paste(readLines(stylesheet, warn = FALSE), collapse = "\n")

  expect_match(single_html, "Section", fixed = TRUE)
  expect_match(single_html, "Sort results by", fixed = TRUE)
  expect_false(grepl("Source dataset", single_html, fixed = TRUE))
  expect_false(grepl("Metadata layer", single_html, fixed = TRUE))
  expect_identical(grid_item_count(single_html), 2L)
  expect_match(appended_html, "Source dataset", fixed = TRUE)
  expect_match(appended_html, "Metadata layer", fixed = TRUE)
  expect_match(appended_html, "Section", fixed = TRUE)
  expect_match(appended_html, "Sort results by", fixed = TRUE)
  expect_identical(grid_item_count(appended_html), 4L)
  expect_false(grepl("llw-metadata-optional-filter", scss, fixed = TRUE))
  expect_false(grepl("llw-metadata-sort-filter", scss, fixed = TRUE))
  expect_false(grepl("var(--llw-space-5)", scss, fixed = TRUE))
  expect_match(
    scss,
    "grid-template-columns: 1.1rem minmax(0, 1fr);",
    fixed = TRUE
  )
})

test_that("partial and absent metadata states remain truthful and usable", {
  partial <- m7_fixture("iztech-light-subset")$metadata_snapshot$bundle
  absent <- m1_record("Absent metadata")$metadata_bundle
  partial_html <- htmltools::renderTags(metadata_state_callout(partial))$html
  absent_html <- htmltools::renderTags(metadata_state_callout(absent))$html

  expect_match(partial_html, "Partial metadata", fixed = TRUE)
  expect_match(partial_html, "missing values are not inferred", fixed = TRUE)
  expect_match(absent_html, "Optional metadata absent", fixed = TRUE)
  expect_match(absent_html, "fully usable", fixed = TRUE)
  expect_match(absent_html, "unit not specified", fixed = TRUE)

  absent_result <- metadata_index_query(absent$index)
  absent_message <- metadata_result_summary_message(absent, absent_result)
  expect_match(
    absent_message,
    "contains no retained optional metadata",
    fixed = TRUE
  )
  expect_match(absent_message, "raw variable names", fixed = TRUE)
  expect_false(grepl("Clear or broaden", absent_message, fixed = TRUE))
})

test_that("visible review route uses all three exact offline fixtures", {
  records <- metadata_explorer_fixture_records(m7_fixture_directory())
  derived <- records[seq_len(3L)]
  appended <- records[[4L]]
  absent <- records[[5L]]
  expected_repositories <- c(
    "tscnlab/melidos-tum-glc-dataset",
    "tscnlab/melidos-iztech-glc-dataset",
    "tscnlab/melidos-ucr-glc-dataset"
  )
  expected_revisions <- c(
    "1187d19b7f6145b73b8d4dfee13af711c1cb6180",
    "abc456bdb418e6250b9c973813a190dc062c6e76",
    "1d1309c969b9ca754a5c9696382c39d4b58ccdfd"
  )
  expected_contracts <- c(
    "MEDI only (exact source variable: MEDI)",
    "Explicit subset: LIGHT, MEDI",
    "All variables from the selected compatible unit (29 source variables)"
  )

  expect_length(records, 5L)
  expect_identical(
    names(derived),
    c(
      "dataset_m7tummedionly",
      "dataset_m7iztechlightsubset",
      "dataset_m7ucrallvariables"
    )
  )
  expect_identical(
    unname(vapply(
      derived,
      function(record) {
        record$source_manifest$details$source$repository
      },
      character(1)
    )),
    expected_repositories
  )
  expect_identical(
    unname(vapply(
      derived,
      function(record) {
        record$source_manifest$details$source$revision
      },
      character(1)
    )),
    expected_revisions
  )
  expect_identical(
    unname(vapply(
      derived,
      function(record) {
        record$source_manifest$details$fixture_contract
      },
      character(1)
    )),
    expected_contracts
  )
  expect_true(all(vapply(
    derived,
    function(record) {
      isTRUE(record$source_manifest$details$derived_fixture)
    },
    logical(1)
  )))
  expect_identical(
    unname(vapply(
      derived,
      function(record) {
        record$metadata_bundle$sources[[1L]]$state
      },
      character(1)
    )),
    c("complete", "partial", "complete")
  )
  expect_identical(
    absent$metadata_bundle$sources[[1L]]$state,
    "absent"
  )

  appended_bundle <- appended$metadata_bundle
  appended_data <- dataset_raw_data(appended)
  expected_lineage <- c(
    "dataset_m7tummedionly",
    "dataset_m7iztechlightsubset"
  )
  expect_identical(appended$id, "dataset_m7appendedreview")
  expect_identical(names(appended_bundle$sources), expected_lineage)
  expect_identical(
    unname(vapply(
      appended_bundle$sources,
      `[[`,
      character(1),
      "label"
    )),
    c(
      "TUM MEDI-only derived fixture",
      "IZTECH photopic and melanopic subset derived fixture"
    )
  )
  expect_identical(appended_bundle$overlay$revision, 1L)
  expect_identical(
    sort(unique(appended_data$llw_source_dataset_id)),
    sort(expected_lineage)
  )
  expect_identical(nrow(appended_data), 352L)
  expect_true(all(
    c(
      "tum_MEDI",
      "iztech_MEDI",
      "iztech_LIGHT",
      "llw_source_dataset_id"
    ) %in%
      names(appended_data)
  ))
  expect_false("MEDI" %in% names(appended_data))
  expect_length(metadata_source_choices(appended_bundle), 3L)
  expect_length(metadata_layer_choices(appended_bundle), 3L)
  appended_sections <- metadata_resource_choices(appended_bundle$index)
  expect_identical(
    unname(appended_sections[["Retained Source Metadata"]]),
    "metadata_bundle"
  )
  expect_identical(
    unname(appended_sections[["Record Summary"]]),
    "factual_metadata"
  )
  appended_sex <- metadata_index_query_groups(
    appended_bundle$index,
    query = "participant_sex",
    namespace_id = expected_lineage[[1L]],
    limit = 10L
  )$rows[1L, , drop = FALSE]
  expect_identical(
    metadata_group_path_segments(appended_sex),
    c("core_metadata", "participants", "participant_sex")
  )
  expect_identical(
    metadata_group_location_text(appended_sex),
    "Core Metadata / Participants / Participant Sex"
  )
  appended_sex_detail <- htmltools::renderTags(metadata_group_detail_ui(
    metadata_index_group(appended_bundle$index, appended_sex$group_id),
    appended_bundle,
    show_source = TRUE
  ))$html
  expect_match(
    appended_sex_detail,
    "metadata_bundle/sources/",
    fixed = TRUE
  )

  identity_html <- htmltools::renderTags(
    metadata_record_identity_ui(derived[[3L]])
  )$html
  expect_match(identity_html, "Derived offline fixture", fixed = TRUE)
  expect_match(identity_html, expected_repositories[[3L]], fixed = TRUE)
  expect_match(identity_html, expected_revisions[[3L]], fixed = TRUE)
  expect_match(identity_html, expected_contracts[[3L]], fixed = TRUE)
  expect_match(
    identity_html,
    "not the complete published dataset",
    fixed = TRUE
  )

  appended_identity_html <- htmltools::renderTags(
    metadata_record_identity_ui(appended)
  )$html
  expect_match(
    appended_identity_html,
    "Derived offline append",
    fixed = TRUE
  )
  expect_match(
    appended_identity_html,
    "dataset_m7tummedionly",
    fixed = TRUE
  )
  expect_match(
    appended_identity_html,
    "dataset_m7iztechlightsubset",
    fixed = TRUE
  )
  expect_match(
    appended_identity_html,
    "Public package",
    fixed = TRUE
  )
  expect_match(
    appended_identity_html,
    "Exact revision",
    fixed = TRUE
  )
  expect_match(
    appended_identity_html,
    "llw_source_dataset_id",
    fixed = TRUE
  )
  expect_match(
    appended_identity_html,
    "not flattened or semantically reconciled",
    fixed = TRUE
  )
  expect_match(
    appended_identity_html,
    "bound to the combined source fingerprint",
    fixed = TRUE
  )
  expect_s3_class(metadata_explorer_app(records), "shiny.appobj")
})

test_that("appended variable searches do not inherit source-label context", {
  appended <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )[["dataset_m7appendedreview"]]$metadata_bundle
  source_ids <- c(
    "dataset_m7tummedionly",
    "dataset_m7iztechlightsubset"
  )
  expected_direct_sources <- list(
    medi = source_ids,
    light = source_ids[[2L]]
  )

  for (query in names(expected_direct_sources)) {
    direct_entries <- appended$index$entries[
      appended$index$entries$field %in%
        c("primary_variables", "dataset_file_variables_name") &
        tolower(appended$index$entries$value) == query,
      ,
      drop = FALSE
    ]
    direct_group_ids <- unique(direct_entries$group_id)
    expect_setequal(
      unique(direct_entries$namespace_id),
      expected_direct_sources[[query]]
    )

    all_sources <- metadata_index_query_groups(
      appended$index,
      query = query,
      layer = "Immutable source",
      limit = 100L
    )
    expect_true(all_sources$total <= 100L)
    expect_false(all_sources$limited)
    expect_true(all(direct_group_ids %in% all_sources$rows$group_id))

    for (source_id in source_ids) {
      one_source <- metadata_index_query_groups(
        appended$index,
        query = query,
        namespace_id = source_id,
        layer = "Immutable source",
        limit = 100L
      )
      expect_true(one_source$total <= 40L)
      expect_false(one_source$limited)
      source_direct_ids <- unique(
        direct_entries$group_id[direct_entries$namespace_id == source_id]
      )
      expect_true(all(source_direct_ids %in% one_source$rows$group_id))
    }
  }
})

test_that("melanopic searches collect the same item without sibling variables", {
  bundle <- m7_fixture("iztech-light-subset")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(
    bundle$index,
    query = "melanopic EDI",
    limit = 100L
  )
  selected <- lapply(
    result$rows$group_id,
    function(group_id) metadata_index_group(bundle$index, group_id)$entries
  )
  retained_values <- unlist(lapply(selected, `[[`, "value"), use.names = FALSE)

  expect_gt(result$total, 0L)
  expect_true(all(nzchar(result$rows$group_context)))
  expect_false(any(grepl("Photopic", retained_values, ignore.case = TRUE)))
  expect_false(any(retained_values == "LIGHT"))
})

test_that("offline review records search the same raw and visible field names", {
  records <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )
  cases <- list(
    list(
      record_id = "dataset_m7ucrallvariables",
      raw = "participant_age",
      visible = "participant age"
    ),
    list(
      record_id = "dataset_m7tummedionly",
      raw = "primary_variables",
      visible = "primary variables"
    ),
    list(
      record_id = "dataset_m7iztechlightsubset",
      raw = "dataset_file_variables_name",
      visible = "dataset file variables name"
    )
  )

  for (case in cases) {
    index <- records[[case$record_id]]$metadata_bundle$index
    raw_result <- metadata_index_query_groups(
      index,
      query = case$raw,
      limit = 40L
    )
    visible_result <- metadata_index_query_groups(
      index,
      query = case$visible,
      limit = 40L
    )

    expect_gt(raw_result$total, 0L)
    expect_identical(visible_result$total, raw_result$total)
    expect_setequal(
      visible_result$rows$group_id,
      raw_result$rows$group_id
    )
    expect_true(all(visible_result$rows$field == case$raw))
  }
})

test_that("explorer interactions need only the retained in-memory bundle", {
  fixture_bundle <- m7_fixture(
    "ucr-all-light-variables"
  )$metadata_snapshot$bundle
  bare_record <- list(metadata_bundle = fixture_bundle)

  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(bare_record)),
    {
      session$flushReact()
      expect_true(status()$ready)
      expect_identical(status()$source_count, 1L)
      initial_total <- status()$match_count
      expect_gt(initial_total, 50L)

      session$setInputs(query = "MEDI")
      session$flushReact()
      expect_gt(status()$match_count, 0L)
      expect_lt(status()$match_count, initial_total)
      first_id <- query_result()$rows$group_id[[1L]]

      session$setInputs(sort_by = "field", descending = TRUE)
      session$flushReact()
      expect_identical(
        metadata_index_group(fixture_bundle$index, first_id)$group$group_id,
        first_id
      )

      selected_id <- query_result()$rows$group_id[[1L]]
      session$setInputs(group_id = selected_id)
      session$flushReact()
      expect_identical(selected_group()$group$group_id, selected_id)
      expect_true(all(selected_group()$entries$group_id == selected_id))
      expect_identical(status()$selected_group_id, selected_id)

      session$setInputs(query = "zzzz-no-match")
      session$flushReact()
      expect_identical(status()$match_count, 0L)
      expect_null(selected_group())
      expect_null(selected_entry())
      expect_null(status()$selected_group_id)
      expect_null(status()$selected_entry_id)

      session$setInputs(query = "MEDI")
      session$flushReact()
      expect_gt(status()$match_count, 0L)
    }
  )
})

test_that("explorer pivots among item properties and hierarchy nodes", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  bare_record <- list(metadata_bundle = bundle)

  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(bare_record)),
    {
      session$flushReact()
      session$setInputs(query = "VLSQ8")
      session$flushReact()
      name_id <- query_result()$rows$group_id[
        query_result()$rows$field == "participant_characteristic_name"
      ][[1L]]
      session$setInputs(group_id = name_id)
      session$flushReact()

      expect_identical(
        selected_group()$group$field,
        "participant_characteristic_name"
      )
      expect_identical(nrow(selected_neighborhood()$item_nodes), 5L)
      value_id <- selected_neighborhood()$property_groups$group_id[
        selected_neighborhood()$property_groups$field ==
          "participant_characteristic_value"
      ][[1L]]
      session$setInputs(neighborhood_group_id = value_id)
      session$flushReact()
      expect_identical(
        selected_group()$group$field,
        "participant_characteristic_value"
      )
      expect_identical(
        selected_group()$entries$value,
        c("23", "18", "20", "30", "21")
      )
      second_instance <- selected_neighborhood()$item_nodes$node_id[[2L]]
      session$setInputs(hierarchy_instance_node_id = second_instance)
      session$flushReact()
      expect_identical(hierarchy_context()$node$node_id, second_instance)

      session$setInputs(query = "melanopic EDI")
      session$flushReact()
      label_id <- query_result()$rows$group_id[
        query_result()$rows$field == "label" &
          query_result()$rows$group_context == "melanopic_edi"
      ][[1L]]
      session$setInputs(group_id = label_id)
      session$flushReact()
      dataset_file_id <- hierarchy_context()$nearby$node_id[
        hierarchy_context()$nearby$kind == "table" &
          hierarchy_context()$nearby$label == "dataset_file"
      ][[1L]]
      session$setInputs(hierarchy_nearby_node_id = dataset_file_id)
      session$flushReact()
      expect_identical(
        hierarchy_context()$node$node_id,
        dataset_file_id
      )
      expect_identical(hierarchy_context()$node$label, "dataset_file")
    }
  )
})

test_that("hierarchy controls sit above a display-only selected-field card", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(
    bundle$index,
    "participant_age"
  )
  group <- metadata_index_group(bundle$index, result$rows$group_id[[1L]])
  location <- htmltools::renderTags(
    metadata_group_location_ui(group$group)
  )$html
  detail <- htmltools::renderTags(
    metadata_group_detail_ui(group, bundle)
  )$html
  panel <- htmltools::renderTags(
    metadata_test_hierarchy_panel(group, bundle)
  )$html
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)
  context <- metadata_index_node_context(
    bundle$index,
    neighborhood$representative_node_id
  )
  summary <- htmltools::renderTags(
    metadata_hierarchy_summary_ui(group, context)
  )$html

  expect_match(location, "Where this field lives", fixed = TRUE)
  expect_match(location, "Section", fixed = TRUE)
  expect_match(location, "Field", fixed = TRUE)
  expect_match(location, "Participants", fixed = TRUE)
  expect_match(detail, "Repeated records are kept together", fixed = TRUE)
  expect_match(detail, "Exact grouped location", fixed = TRUE)
  expect_match(detail, "Exact retained paths", fixed = TRUE)
  expect_false(grepl("Explore this metadata item", detail, fixed = TRUE))
  expect_false(grepl("Explore the metadata hierarchy", detail, fixed = TRUE))
  expect_false(grepl("type=\"radio\"", detail, fixed = TRUE))
  expect_match(
    panel,
    "This panel controls the selected metadata field below.",
    fixed = TRUE
  )
  expect_match(
    panel,
    "Search and result order above stay unchanged.",
    fixed = TRUE
  )
  expect_match(panel, "Explore this metadata item", fixed = TRUE)
  expect_match(panel, "Explore the metadata hierarchy", fixed = TRUE)
  expect_match(panel, "Selected field below", fixed = TRUE)
  expect_match(panel, "Current hierarchy focus", fixed = TRUE)
  expect_match(summary, "Selected field: Participant Age", fixed = TRUE)
  expect_match(summary, "Hierarchy focus:", fixed = TRUE)
  for (path in group$entries$path) {
    expect_match(detail, path, fixed = TRUE)
  }
})

test_that("item properties make VLSQ8 values directly selectable", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(bundle$index, "VLSQ8", limit = 100L)
  row <- result$rows[
    result$rows$field == "participant_characteristic_name" &
      result$rows$group_context == "VLSQ8",
    ,
    drop = FALSE
  ]
  group <- metadata_index_group(bundle$index, row$group_id[[1L]])
  html <- htmltools::renderTags(
    metadata_test_hierarchy_panel(group, bundle)
  )$html

  expect_match(html, "Explore this metadata item", fixed = TRUE)
  expect_match(html, "participant_characteristic_value", fixed = TRUE)
  expect_match(html, "participant_characteristic_unit", fixed = TRUE)
  expect_match(html, "same exact source item", fixed = TRUE)
  expect_match(html, "Choose an exact item instance", fixed = TRUE)
  expect_match(html, "Retained record TUM_S001", fixed = TRUE)
  expect_match(html, "type=\"radio\"", fixed = TRUE)
})

test_that("term hierarchy exposes containment without borrowing a unit", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(
    bundle$index,
    "melanopic EDI",
    limit = 100L
  )
  row <- result$rows[
    result$rows$field == "label" &
      result$rows$group_context == "melanopic_edi",
    ,
    drop = FALSE
  ]
  group <- metadata_index_group(bundle$index, row$group_id[[1L]])
  html <- htmltools::renderTags(
    metadata_test_hierarchy_panel(group, bundle)
  )$html

  expect_match(html, "Nearby structural branches", fixed = TRUE)
  expect_match(html, "Dataset File", fixed = TRUE)
  expect_match(html, "Containment is not semantic equivalence", fixed = TRUE)
  expect_false(grepl("dataset_file_variables_units", html, fixed = TRUE))
})

test_that("group detail keeps file groups separate from semantic items", {
  bundle <- m7_fixture("iztech-light-subset")$metadata_snapshot$bundle
  device_result <- metadata_index_query_groups(
    bundle$index,
    "dataset_file_crossref_device_id",
    limit = 100L
  )
  device_group <- metadata_index_group(
    bundle$index,
    device_result$rows$group_id[[1L]]
  )
  device_html <- htmltools::renderTags(
    metadata_group_values_ui(device_group)
  )$html
  variable_result <- metadata_index_query_groups(
    bundle$index,
    "dataset_file_variables_name",
    limit = 100L
  )
  medi_id <- variable_result$rows$group_id[
    variable_result$rows$group_context == "MEDI"
  ][[1L]]
  medi_group <- metadata_index_group(bundle$index, medi_id)
  medi_html <- htmltools::renderTags(
    metadata_group_values_ui(medi_group)
  )$html

  expect_match(device_html, "Dataset / file group", fixed = TRUE)
  expect_match(device_html, "MELIDOS_IZTECH_S004:19", fixed = TRUE)
  expect_match(device_html, "IZTECH_AL03", fixed = TRUE)
  expect_match(device_html, "MELIDOS_IZTECH_S004:20", fixed = TRUE)
  expect_match(device_html, "IZTECH_AL04", fixed = TRUE)
  expect_false(grepl("Semantic item", device_html, fixed = TRUE))
  expect_match(medi_html, "Dataset / file group", fixed = TRUE)
  expect_match(medi_html, "Semantic item", fixed = TRUE)
  expect_match(medi_html, "MELIDOS_IZTECH_S004:19", fixed = TRUE)
  expect_match(medi_html, ">MEDI<", fixed = TRUE)
})

test_that("appended hierarchy exposes exact IZTECH file-group instances", {
  records <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )
  bundle <- records[["dataset_m7appendedreview"]]$metadata_bundle
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
  focus_19 <- device_neighborhood$item_nodes$node_id[
    device_neighborhood$item_nodes$record_identity == "MELIDOS_IZTECH_S004:19"
  ][[1L]]
  device_html <- htmltools::renderTags(metadata_test_hierarchy_panel(
    device_group,
    bundle,
    focus_node_id = focus_19
  ))$html

  expect_match(device_html, "MELIDOS_IZTECH_S004:19", fixed = TRUE)
  expect_match(device_html, "IZTECH_AL03", fixed = TRUE)
  expect_match(device_html, "MELIDOS_IZTECH_S004:20", fixed = TRUE)
  expect_match(device_html, "IZTECH_AL04", fixed = TRUE)
  expect_match(device_html, "Retained record identity", fixed = TRUE)
  expect_match(device_html, "Exact source path", fixed = TRUE)

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
  focus_20 <- medi_neighborhood$item_nodes$node_id[
    medi_neighborhood$item_nodes$record_identity == "MELIDOS_IZTECH_S004:20"
  ][[1L]]
  medi_html <- htmltools::renderTags(metadata_test_hierarchy_panel(
    medi_group,
    bundle,
    focus_node_id = focus_20
  ))$html

  expect_match(
    medi_html,
    "Retained record MELIDOS_IZTECH_S004:19",
    fixed = TRUE
  )
  expect_match(
    medi_html,
    "Retained record MELIDOS_IZTECH_S004:20",
    fixed = TRUE
  )
  expect_match(medi_html, "Retained record identity", fixed = TRUE)
  expect_match(medi_html, "MELIDOS_IZTECH_S004:20", fixed = TRUE)
})

test_that("matching fields are directly selectable result rows", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  rows <- metadata_index_query_groups(bundle$index, "participant_age")$rows
  html <- htmltools::renderTags(metadata_result_list_ui(
    rows,
    "field_selection"
  ))$html

  expect_match(html, "type=\"radio\"", fixed = TRUE)
  expect_match(html, "llw-metadata-result-option", fixed = TRUE)
  expect_match(html, "participant_age", fixed = TRUE)
  expect_match(html, "Participants / Participant Age", fixed = TRUE)
  expect_false(grepl("<select", html, fixed = TRUE))
})

test_that("source controls and result provenance appear only for appends", {
  single <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  first <- m7_fixture_record("tum-medi-only", "TUM fixture")
  second <- m7_fixture_record("iztech-light-subset", "IZTECH fixture")
  provenance <- lapply(list(first, second), function(record) {
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
  appended <- new_appended_metadata_bundle(provenance)
  single_rows <- metadata_index_query_groups(single$index, limit = 1L)$rows
  appended_rows <- metadata_index_query_groups(appended$index, limit = 1L)$rows
  single_html <- htmltools::renderTags(metadata_result_list_ui(
    single_rows,
    "single_selection",
    show_source = FALSE
  ))$html
  appended_html <- htmltools::renderTags(metadata_result_list_ui(
    appended_rows,
    "appended_selection",
    show_source = TRUE
  ))$html

  expect_identical(unname(metadata_source_choices(single)[[1L]]), "all")
  expect_length(metadata_source_choices(appended), 3L)
  expect_false(grepl(single$sources[[1L]]$label, single_html, fixed = TRUE))
  expect_match(
    appended_html,
    appended_rows$source_label[[1L]],
    fixed = TRUE
  )
})

test_that("overlay result details never imply source mutation or validation", {
  source <- new_metadata_source_namespace(
    "source-a",
    "Package source",
    "test",
    records = list(study = list(title = "Source title")),
    state = "complete"
  )
  base <- new_metadata_bundle(stats::setNames(list(source), "source-a"))
  tables <- metadata_empty_overlay_tables()
  tables$datasets <- data.frame(
    dataset_id = "dataset-a",
    title = "Overlay title",
    stringsAsFactors = FALSE
  )
  bundle <- new_metadata_bundle(
    base$sources,
    overlay = new_metadata_overlay(
      base$source_fingerprint,
      tables = tables,
      revision = 1L
    )
  )
  result <- metadata_index_query_groups(bundle$index, "overlay title")
  group <- metadata_index_group(bundle$index, result$rows$group_id[[1L]])
  html <- htmltools::renderTags(
    metadata_group_detail_ui(group, bundle)
  )$html

  expect_match(html, "LightLogWeb overlay revision 1", fixed = TRUE)
  expect_match(html, "Bound source fingerprint", fixed = TRUE)
  expect_match(html, "does not change or revalidate", fixed = TRUE)
  expect_false(grepl("Immutable source</span>", html, fixed = TRUE))
})
