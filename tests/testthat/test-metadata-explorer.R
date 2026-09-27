metadata_test_go_to_areas <- function(session) {
  returned <- session$getReturned()
  for (step in seq_len(100L)) {
    if (is.null(returned$hierarchy_context())) return(invisible(NULL))
    count <- isolate(session$input$hierarchy_up %||% 0)
    session$setInputs(hierarchy_up = count + 1)
    session$flushReact()
  }
  stop("One level up did not reach the metadata-area overview.")
}

metadata_test_open_node <- function(
  session,
  node_id,
  areas = FALSE,
  count = 1
) {
  if (isTRUE(areas)) metadata_test_go_to_areas(session)
  session$flushReact()
  do.call(
    session$setInputs,
    stats::setNames(
      list(count),
      metadata_hierarchy_action_id(node_id)
    )
  )
  session$flushReact()
}

metadata_test_hierarchy_panel <- function(
  group,
  bundle,
  focus_node_id = NULL,
  input_ids = NULL,
  dialog = FALSE
) {
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)
  if (is.null(focus_node_id)) {
    focus_node_id <- neighborhood$representative_node_id
  }
  context <- metadata_hierarchy_node_context(bundle$index, focus_node_id)
  roots <- metadata_hierarchy_logical_roots(bundle$index, group)
  if (isTRUE(dialog)) {
    return(metadata_hierarchy_context_ui(
      context,
      roots,
      list(root = "root", path = "path", child = "child"),
      group$group$group_id[[1L]]
    ))
  }
  metadata_hierarchy_panel_ui(
    group,
    context,
    roots,
    input_ids = input_ids
  )
}

test_that("metadata explorer presents discoverable source and overlay context", {
  ui <- htmltools::renderTags(metadata_explorer_ui("metadata"))$html
  complete <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  complete_html <- htmltools::renderTags(metadata_state_callout(complete))$html
  layer_html <- htmltools::renderTags(metadata_source_overlay_ui(complete))$html

  expect_match(ui, "Dataset and metadata context", fixed = TRUE)
  expect_match(ui, "Search field names and values", fixed = TRUE)
  expect_match(ui, "Clear search", fixed = TRUE)
  expect_match(ui, "metadata-dataset_identity", fixed = TRUE)
  expect_match(ui, "metadata-filter_grid", fixed = TRUE)
  expect_match(ui, "llw-metadata-search-results", fixed = TRUE)
  expect_match(ui, "metadata-result_summary", fixed = TRUE)
  expect_match(ui, "metadata-result_list", fixed = TRUE)
  expect_match(ui, "metadata-hierarchy_shell", fixed = TRUE)
  expect_false(grepl("metadata-item_properties", ui, fixed = TRUE))
  expect_lt(
    regexpr("metadata-result_list", ui, fixed = TRUE)[[1L]],
    regexpr("metadata-hierarchy_shell", ui, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("metadata-hierarchy_shell", ui, fixed = TRUE)[[1L]],
    regexpr("metadata-entry_detail", ui, fixed = TRUE)[[1L]]
  )
  for (context in c(
    "dataset_state",
    "dataset_identity",
    "layer_summary",
    "focus_status"
  )) {
    expect_lt(
      regexpr("metadata-entry_detail", ui, fixed = TRUE)[[1L]],
      regexpr(paste0("metadata-", context), ui, fixed = TRUE)[[1L]]
    )
  }
  expect_match(complete_html, "does not revalidate", fixed = TRUE)
  expect_match(layer_html, "Immutable source", fixed = TRUE)
  expect_match(layer_html, "LightLogWeb overlay", fixed = TRUE)
  expect_match(layer_html, "never.*silently reconciled")
  ui_source <- paste(deparse(body(metadata_explorer_ui)), collapse = "\n")
  expect_false(grepl("tags\\$script|<script", ui_source))
})

test_that("dataset details labels retain exact factual metadata identities", {
  bundle <- new_metadata_bundle_from_factual(
    factual_metadata = list(variables = list(MEDI = list(unit = "lux"))),
    namespace_id = "dataset_m7wording",
    label = "Imported dataset details",
    source_type = "package_sample"
  )
  original <- serialize(bundle, NULL)
  result <- metadata_index_query_groups(bundle$index, "unit")
  group <- metadata_index_group(bundle$index, result$rows$group_id[[1L]])
  roots <- metadata_hierarchy_logical_roots(bundle$index, group)
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)
  context <- metadata_hierarchy_node_context(
    bundle$index,
    neighborhood$representative_node_id
  )
  path <- metadata_hierarchy_full_path(context, roots)

  expect_identical(
    metadata_section_label("factual_metadata"),
    "Dataset details"
  )
  expect_identical(
    unique(metadata_section_index(bundle$index)$section_label),
    "Dataset details"
  )
  expect_identical(
    metadata_group_location_text(group$group),
    "Dataset details / Variables / MEDI / Unit"
  )
  rendered <- lapply(
    list(
      metadata_group_choice_ui(group$group),
      metadata_hierarchy_node_choice_ui(roots[1L, , drop = FALSE]),
      metadata_hierarchy_path_ui(path, "compact"),
      metadata_hierarchy_path_ui(path, "full", compact = FALSE)
    ),
    function(ui) htmltools::renderTags(ui)$html
  )
  for (html in rendered) {
    expect_match(html, "Dataset details", fixed = TRUE)
    expect_false(grepl("Factual Metadata|Record Summary", html))
  }
  expect_match(rendered[[3L]], "factual_metadata", fixed = TRUE)
  expect_true("factual_metadata" %in% roots$label)
  expect_match(group$group$group_path[[1L]], "/factual_metadata/", fixed = TRUE)
  expect_identical(serialize(bundle, NULL), original)
})

test_that("one metadata explorer contains path, parent action, help, and contents", {
  shell <- htmltools::renderTags(
    metadata_hierarchy_shell_ui(NS("metadata"))
  )$html

  expect_match(shell, "Metadata explorer", fixed = TRUE)
  expect_match(shell, "metadata-hierarchy_focus", fixed = TRUE)
  expect_match(shell, "metadata-hierarchy_content", fixed = TRUE)
  expect_match(shell, "metadata-item_properties", fixed = TRUE)
  expect_match(shell, "metadata-hierarchy_up", fixed = TRUE)
  expect_match(shell, "metadata-item_count", fixed = TRUE)
  expect_match(shell, "One level up", fixed = TRUE)
  expect_match(shell, "disabled", fixed = TRUE)
  expect_match(shell, "metadata-explorer_help_button", fixed = TRUE)
  help_button <- regmatches(
    shell,
    regexpr('<button[^>]*id="metadata-explorer_help_button"[^>]*>', shell)
  )
  expect_match(
    help_button,
    'class="btn btn-link llw-icon-button"',
    fixed = TRUE
  )
  expect_match(shell, "How to use the metadata explorer", fixed = TRUE)
  expect_match(shell, "bslib-popover", fixed = TRUE)
  expect_match(shell, "llw-metadata-help-popup", fixed = TRUE)
  expect_match(shell, "one exact source namespace", fixed = TRUE)
  expect_lt(
    regexpr("metadata-hierarchy_content", shell, fixed = TRUE)[[1L]],
    regexpr("metadata-hierarchy_up", shell, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("metadata-hierarchy_up", shell, fixed = TRUE)[[1L]],
    regexpr("metadata-item_count", shell, fixed = TRUE)[[1L]]
  )
  expect_lt(
    regexpr("metadata-item_count", shell, fixed = TRUE)[[1L]],
    regexpr("metadata-item_properties", shell, fixed = TRUE)[[1L]]
  )
  expect_false(grepl("Path to this focus|Explore this metadata item", shell))
  expect_match(shell, "tabindex=\"-1\"", fixed = TRUE)
  expect_false(grepl("accordion|aria-expanded", shell))
})

test_that("branch-state text colors remain scoped and retain the focus outline", {
  scss <- paste(
    readLines(
      system.file(
        "app",
        "scss",
        "lightlogweb.scss",
        package = "LightLogWeb",
        mustWork = TRUE
      ),
      warn = FALSE
    ),
    collapse = "\n"
  )
  branch <- regmatches(
    scss,
    regexpr(
      "\\.llw-app \\.llw-metadata-branch-action \\{[^}]+\\}",
      scss
    )
  )
  for (state in c("", "hover-", "active-")) {
    expect_match(
      branch,
      paste0("--bs-btn-", state, "color: var(--llw-text)"),
      fixed = TRUE
    )
  }
  expect_match(scss, ".llw-metadata-branch-action:focus-visible", fixed = TRUE)
  expect_match(scss, "outline: 0.2rem solid var(--llw-focus)", fixed = TRUE)
})

test_that("compact paths keep four levels while dialogs retain every ancestor", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(
    bundle$index,
    "dataset_file_variables_name",
    limit = 100L
  )
  row <- result$rows[
    result$rows$field == "dataset_file_variables_name" &
      result$rows$group_context == "MEDI",
    ,
    drop = FALSE
  ]
  group <- metadata_index_group(bundle$index, row$group_id[[1L]])
  context <- metadata_hierarchy_node_context(
    bundle$index,
    metadata_index_group_neighborhood(
      bundle$index,
      group
    )$representative_node_id
  )
  roots <- metadata_hierarchy_logical_roots(bundle$index, group)
  path <- metadata_hierarchy_full_path(context, roots)
  expect_gt(nrow(path), 4L)
  expect_identical(path$label[[1L]], "core_metadata")
  for (depth in seq_len(nrow(path))) {
    shortened <- utils::head(path, depth)
    compact <- htmltools::renderTags(metadata_hierarchy_path_ui(
      shortened,
      "path"
    ))$html
    full <- htmltools::renderTags(metadata_hierarchy_path_ui(
      shortened,
      "path",
      FALSE
    ))$html
    expect_length(
      regmatches(compact, gregexpr('type="radio"', compact))[[1L]],
      min(depth, 4L)
    )
    expect_length(regmatches(full, gregexpr('type="radio"', full))[[1L]], depth)
    expect_match(compact, shortened$node_id[[depth]], fixed = TRUE)
    expect_match(compact, "--llw-path-small-levels: 1", fixed = TRUE)
    for (id in shortened$node_id) expect_match(full, id, fixed = TRUE)
  }
  panel <- htmltools::renderTags(metadata_hierarchy_panel_ui(
    group,
    context,
    roots
  ))$html
  expect_match(panel, "has-overflow", fixed = TRUE)
  expect_false(grepl("Fields at this level", panel, fixed = TRUE))
  expect_false(grepl("Browse", panel, fixed = TRUE))
  expect_lt(
    regexpr("llw-metadata-path-overflow", panel, fixed = TRUE)[[1L]],
    regexpr("llw-metadata-breadcrumbs", panel, fixed = TRUE)[[1L]]
  )
  dialog <- htmltools::renderTags(metadata_hierarchy_context_ui(
    context,
    roots,
    list(path = "full_path")
  ))$html
  expect_length(
    regmatches(dialog, gregexpr('type="radio"', dialog))[[1L]],
    nrow(path)
  )
  expect_false(grepl(
    "Browse|Metadata area|Contained metadata|Exact retained source nodes",
    dialog
  ))
})

test_that("parent navigation follows the full meaningful path, not its clipped tail", {
  expect_null(metadata_hierarchy_parent_id(NULL, NULL))
  for (fixture in c(
    "tum-medi-only",
    "iztech-light-subset",
    "ucr-all-light-variables"
  )) {
    bundle <- m7_fixture(fixture)$metadata_snapshot$bundle
    result <- metadata_index_query_groups(
      bundle$index,
      "dataset_file_variables_name",
      limit = 100L
    )
    group <- metadata_index_group(bundle$index, result$rows$group_id[[1L]])
    focus <- metadata_index_group_neighborhood(bundle$index, group)
    context <- metadata_hierarchy_node_context(
      bundle$index,
      focus$representative_node_id
    )
    roots <- metadata_hierarchy_logical_roots(bundle$index, group)
    path <- metadata_hierarchy_full_path(context, roots)
    expect_gt(nrow(path), 4L)
    for (depth in seq_len(nrow(path))) {
      current <- metadata_hierarchy_node_context(
        bundle$index,
        path$node_id[[depth]]
      )
      expect_identical(
        metadata_hierarchy_parent_id(current, roots),
        if (depth == 1L) "metadata_areas" else path$node_id[[depth - 1L]]
      )
    }
  }
})

test_that("one level up preserves field and search context and stops at the overview", {
  record <- m7_fixture_record("iztech-light-subset")
  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(record)),
    {
      session$setInputs(
        query = "dataset_file_variables_name",
        sort_by = "value",
        descending = TRUE,
        resource = "core_metadata",
        layer = "all"
      )
      row <- query_result()$rows
      selected <- row$group_id[row$group_context == "MEDI"][[1L]]
      session$setInputs(group_id = selected)
      session$flushReact()
      path <- metadata_hierarchy_full_path(
        hierarchy_context(),
        hierarchy_roots()
      )
      expect_gt(nrow(path), 4L)
      source_before <- bundle()$source_fingerprint
      session$setInputs(hierarchy_up = 0)
      session$setInputs(hierarchy_up = "invalid")
      expect_identical(
        hierarchy_context()$node$node_id,
        utils::tail(path$node_id, 1L)
      )
      count <- 0
      for (depth in rev(seq_len(nrow(path) - 1L))) {
        count <- count + 1
        session$setInputs(hierarchy_up = count)
        session$flushReact()
        expect_identical(
          hierarchy_context()$node$node_id,
          path$node_id[[depth]]
        )
        expect_identical(active_group_id(), selected)
        expect_identical(input$query, "dataset_file_variables_name")
        expect_identical(input$sort_by, "value")
        expect_true(input$descending)
        expect_identical(input$resource, "core_metadata")
        expect_identical(selected_section(), "core_metadata")
        expect_identical(input$layer, "all")
      }
      session$setInputs(hierarchy_up = count + 1)
      session$flushReact()
      expect_null(hierarchy_context())
      expect_null(hierarchy_parent_id())
      expect_identical(
        hierarchy_navigation_nodes()$node_id,
        hierarchy_roots()$node_id
      )
      session$setInputs(hierarchy_up = count + 2)
      expect_null(hierarchy_context())
      expect_identical(active_group_id(), selected)
      metadata_test_open_node(session, path$node_id[[1L]])
      expect_identical(hierarchy_context()$node$node_id, path$node_id[[1L]])
      expect_identical(hierarchy_parent_id(), "metadata_areas")
      expect_identical(bundle()$source_fingerprint, source_before)
    }
  )
})

test_that("visible exploration separates entering branches from choosing fields", {
  record <- m7_fixture_record("iztech-light-subset")
  original_fingerprint <- record$metadata_bundle$source_fingerprint
  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(record)),
    {
      session$setInputs(query = "participant_age", sort_by = "value")
      session$setInputs(group_id = query_result()$rows$group_id[[1L]])
      session$flushReact()
      first_group <- active_group_id()
      roots <- hierarchy_roots()
      core_id <- roots$node_id[roots$label == "core_metadata"][[1L]]
      stale_root <- roots$node_id[roots$label == "dataset_inventory"][[1L]]
      metadata_test_go_to_areas(session)
      expect_null(hierarchy_context())
      expect_identical(hierarchy_navigation_nodes()$node_id, roots$node_id)
      expect_identical(active_group_id(), first_group)
      overview <- htmltools::renderTags(metadata_item_properties_ui(
        NULL,
        "properties",
        first_group,
        children = hierarchy_navigation_nodes(),
        show_areas = TRUE
      ))$html
      expect_match(overview, 'aria-label="Open Core Metadata"', fixed = TRUE)
      expect_match(
        overview,
        'aria-label="Open Dataset Inventory"',
        fixed = TRUE
      )
      expect_match(
        overview,
        'aria-label="Open Variable Inventory"',
        fixed = TRUE
      )
      expect_false(grepl('type="radio"|Browse', overview))

      metadata_test_open_node(session, core_id)
      datasets_id <- hierarchy_context()$children$node_id[
        hierarchy_context()$children$label == "datasets"
      ][[1L]]
      metadata_test_open_node(session, datasets_id)
      s004_id <- hierarchy_context()$children$node_id[
        hierarchy_context()$children$label == "MELIDOS_IZTECH_S004"
      ][[1L]]
      metadata_test_open_node(session, s004_id)
      context_before <- hierarchy_context()
      groups_before <- hierarchy_neighborhood()$property_groups
      expect_gt(nrow(context_before$children), 0L)
      expect_gt(nrow(groups_before), 0L)
      contents <- htmltools::renderTags(metadata_item_properties_ui(
        hierarchy_neighborhood(),
        "properties",
        first_group,
        children = context_before$children
      ))$html
      expect_match(contents, "llw-metadata-focus-contents", fixed = TRUE)
      expect_match(contents, "llw-metadata-branch-action", fixed = TRUE)
      expect_match(contents, "chevron-right", fixed = TRUE)
      expect_match(contents, 'type="radio"', fixed = TRUE)
      expect_match(contents, "dataset_internal_id", fixed = TRUE)
      expect_false(grepl("Browse|Fields at this level", contents))

      selected_field <- groups_before$group_id[
        groups_before$field == "dataset_internal_id"
      ][[1L]]
      session$setInputs(neighborhood_group_id = selected_field)
      session$flushReact()
      expect_identical(active_group_id(), selected_field)
      expect_identical(hierarchy_context(), context_before)
      expect_identical(hierarchy_neighborhood()$property_groups, groups_before)
      expect_identical(input$query, "participant_age")
      expect_identical(input$sort_by, "value")

      file_id <- context_before$children$node_id[
        context_before$children$label == "dataset_file"
      ][[1L]]
      metadata_test_open_node(session, file_id)
      expect_identical(hierarchy_context()$node$node_id, file_id)
      expect_gt(
        nrow(metadata_hierarchy_full_path(hierarchy_context(), roots)),
        nrow(metadata_hierarchy_full_path(context_before, roots))
      )
      expect_identical(active_group_id(), selected_field)
      session$setInputs(hierarchy_path_node_id = s004_id)
      session$flushReact()
      expect_identical(hierarchy_context()$node$node_id, s004_id)

      # A newly mounted button resets to zero. A subsequent real activation
      # must still work, but a removed branch event must not open another area.
      metadata_test_open_node(session, file_id, count = 0)
      expect_identical(hierarchy_context()$node$node_id, s004_id)
      metadata_test_open_node(session, stale_root, count = 5)
      expect_identical(hierarchy_context()$node$node_id, s004_id)
      metadata_test_open_node(session, file_id, count = "malformed")
      expect_identical(hierarchy_context()$node$node_id, s004_id)
      metadata_test_open_node(session, file_id, count = 1)
      expect_identical(hierarchy_context()$node$node_id, file_id)
      expect_identical(active_group_id(), selected_field)
      expect_identical(bundle()$source_fingerprint, original_fingerprint)
    }
  )
})

test_that("filter controls stay mounted with only relevant controls visible", {
  single <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  one_section <- new_metadata_bundle_from_factual(
    factual_metadata = list(study_title = "One retained summary"),
    namespace_id = "one-section",
    label = "One section",
    source_type = "test"
  )
  appended <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )[["dataset_m7appendedreview"]]$metadata_bundle
  grid_html <- htmltools::renderTags(metadata_filter_grid_ui(NS(
    "metadata"
  )))$html
  single_visible <- metadata_filter_visibility(metadata_filter_choices(single))
  appended_visible <- metadata_filter_visibility(metadata_filter_choices(
    appended
  ))
  one_section_visible <- metadata_filter_visibility(metadata_filter_choices(
    one_section
  ))
  stylesheet <- system.file(
    "app",
    "scss",
    "lightlogweb.scss",
    package = "LightLogWeb",
    mustWork = TRUE
  )
  scss <- paste(readLines(stylesheet, warn = FALSE), collapse = "\n")

  expect_identical(
    single_visible,
    list(namespace = FALSE, layer = FALSE, resource = TRUE)
  )
  expect_identical(
    appended_visible,
    list(namespace = TRUE, layer = TRUE, resource = TRUE)
  )
  expect_identical(
    one_section_visible,
    list(namespace = FALSE, layer = FALSE, resource = FALSE)
  )
  for (id in c("namespace", "layer", "resource", "sort_by")) {
    expect_match(grid_html, paste0('id="metadata-', id, '"'), fixed = TRUE)
  }
  expect_match(grid_html, 'data-ns-prefix="metadata-"', fixed = TRUE)
  expect_match(
    grid_html,
    'data-section-request="metadata-section_request"',
    fixed = TRUE
  )
  expect_match(grid_html, "Updating sections...", fixed = TRUE)
  expect_match(grid_html, "disabled=", fixed = TRUE)
  expect_false(grepl("shiny-html-output", grid_html, fixed = TRUE))
  # Shiny controls conditional visibility itself; an inline display:none would
  # override its shown class and leave valid filters inaccessible.
  expect_false(grepl("display: none", grid_html, fixed = TRUE))
  expect_match(grid_html, "llw-metadata-filter-grid", fixed = TRUE)
  expect_match(
    scss,
    "grid-template-columns: repeat(auto-fit, minmax(12rem, 1fr));",
    fixed = TRUE
  )
  expect_false(grepl("llw-metadata-optional-filter", scss, fixed = TRUE))
  expect_false(grepl("llw-metadata-sort-filter", scss, fixed = TRUE))
  expect_false(grepl("var(--llw-space-5)", scss, fixed = TRUE))
  expect_match(
    scss,
    "grid-template-columns: 1.1rem minmax(0, 1fr);",
    fixed = TRUE
  )
})

test_that("filter selection normalization rejects invalid browser values", {
  choices <- c("All metadata" = "all", "Source metadata" = "Immutable source")
  for (invalid in list(
    NULL,
    character(),
    NA_character_,
    c("all", "x"),
    1,
    "stale"
  )) {
    expect_identical(metadata_filter_selection(invalid, choices), "all")
  }
  expect_identical(
    metadata_filter_selection("Immutable source", choices),
    "Immutable source"
  )
  expect_identical(metadata_filter_choices(NULL)$sort_by[[1L]], "path")
})

test_that("section replies validate requests and retain only valid current choices", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  index <- metadata_section_index(bundle$index)
  inventory <- metadata_filter_inventory(index)
  expect_lt(nrow(inventory), nrow(index))
  expect_identical(
    metadata_filter_choices(bundle, inventory),
    metadata_filter_choices(bundle, index)
  )
  request <- list(
    epoch = 1,
    generation = 3,
    namespace = "all",
    layer = "all",
    resource = "core_metadata"
  )
  expect_true(metadata_section_request_valid(request))
  reply <- metadata_section_reply(request, bundle, index, 1)
  expect_identical(reply$selected, "core_metadata")
  expect_true(reply$visible)
  expect_identical(reply$generation, 3)
  expect_identical(reply$epoch, 1)
  expect_identical(reply, metadata_section_reply(request, bundle, inventory, 1))
  expect_identical(
    reply$choices[[1L]],
    list(value = "all", label = "All sections")
  )
  expect_null(metadata_section_reply(request, bundle, index, 2))
  for (key in c("epoch", "generation")) {
    for (value in list(NULL, NA_real_, Inf, -1, 0.5, "1", c(1, 2), 2^53)) {
      invalid <- request
      invalid[[key]] <- value
      expect_false(metadata_section_request_valid(invalid))
      expect_null(metadata_section_reply(invalid, bundle, index, 1))
    }
  }
  for (key in c("namespace", "layer", "resource")) {
    for (value in list(
      NULL,
      NA_character_,
      c("all", "all"),
      1,
      strrep("x", 4097)
    )) {
      invalid <- request
      invalid[[key]] <- value
      expect_false(metadata_section_request_valid(invalid))
    }
  }
  request$resource <- "missing_section"
  expect_identical(
    metadata_section_reply(request, bundle, index, 1)$selected,
    "all"
  )
  request$namespace <- "unknown_source"
  expect_null(metadata_section_reply(request, bundle, index, 1))
  request$namespace <- "all"
  request$layer <- "unknown_layer"
  expect_null(metadata_section_reply(request, bundle, index, 1))
  request$layer <- "all"
  empty <- metadata_section_reply(request, NULL, NULL, 1)
  expect_false(empty$visible)
  expect_identical(empty$selected, "all")
})

test_that("guarded section requests reject stale generations and reset epochs once", {
  records <- metadata_explorer_fixture_records(m7_fixture_directory())
  current <- reactiveVal(records[["dataset_m7appendedreview"]])
  messages <- list()
  native <- list()
  mock <- shiny::MockShinySession$new()
  mock$sendCustomMessage <- function(type, message) {
    if (identical(type, "llw-metadata-section")) {
      messages[[length(messages) + 1L]] <<- message
    }
  }
  mock$sendInputMessage <- function(inputId, message) {
    native[[length(native) + 1L]] <<- list(
      id = sub("^.*-", "", inputId),
      message = message
    )
  }
  testServer(
    metadata_explorer_server,
    args = list(dataset = current),
    session = mock,
    {
      session$setInputs(
        namespace = "all",
        layer = "all",
        resource = "all",
        query = "",
        sort_by = "value",
        descending = TRUE
      )
      expect_length(messages, 1L)
      expect_identical(messages[[1L]]$kind, "context")
      expect_true(messages[[1L]]$reset)
      epoch <- messages[[1L]]$epoch
      request <- list(
        epoch = epoch,
        generation = 1,
        namespace = "all",
        layer = "all",
        resource = "core_metadata"
      )
      session$setInputs(section_request = request)
      expect_identical(tail(messages, 1L)[[1L]]$selected, "core_metadata")
      iztech <- names(bundle()$sources)[grepl(
        "iztech",
        names(bundle()$sources)
      )]
      for (source in c(iztech, "all", iztech)) {
        request$generation <- request$generation + 1
        request$namespace <- source
        session$setInputs(namespace = source, section_request = request)
        reply <- tail(messages, 1L)[[1L]]
        expect_identical(reply$generation, request$generation)
        expect_identical(reply$namespace, source)
        expect_identical(reply$selected, "core_metadata")
        expect_identical(reply$epoch, epoch)
      }
      before <- length(messages)
      session$setInputs(section_request = request)
      request$generation <- request$generation - 1
      session$setInputs(section_request = request)
      session$setInputs(resource = "core_metadata")
      for (step in seq_len(3L)) session$flushReact()
      expect_length(messages, before)
      expect_identical(query_result()$total, 253L)
      expect_false(any(vapply(
        native,
        function(x) identical(x$id, "resource"),
        TRUE
      )))
      request$generation <- request$generation + 2
      request$namespace <- "invalid_source"
      session$setInputs(section_request = request)
      expect_identical(tail(messages, 1L)[[1L]]$kind, "error")
      request$generation <- request$generation + 1
      request$namespace <- "all"
      request$layer <- "LightLogWeb overlay"
      session$setInputs(
        namespace = "all",
        layer = request$layer,
        section_request = request
      )
      expect_false(tail(messages, 1L)[[1L]]$visible)
      expect_identical(tail(messages, 1L)[[1L]]$selected, "all")
      request$epoch <- 0
      session$setInputs(section_request = request)
      expect_identical(tail(messages, 1L)[[1L]]$kind, "context")
      expect_identical(tail(messages, 1L)[[1L]]$epoch, epoch)
      messages <<- list()
      current(records[["dataset_m7absent"]])
      session$flushReact()
      expect_length(messages, 1L)
      expect_true(messages[[1L]]$reset)
      expect_gt(messages[[1L]]$epoch, epoch)
      session$setInputs(namespace = "all", layer = "all", resource = "all")
      session$flushReact()
      expect_length(messages, 1L)
      expect_identical(input$sort_by, "value")
      expect_true(input$descending)
      expect_false(any(vapply(
        native,
        function(x) identical(x$id, "resource"),
        TRUE
      )))
    }
  )
})

test_that("the browser section guard ignores delayed replies and protects ready choices", {
  node <- Sys.getenv("LLW_NODE_BINARY", unset = Sys.which("node"))
  skip_if(
    !nzchar(node),
    "Node is unavailable for the offline browser-protocol unit test"
  )
  script <- system.file(
    "app",
    "www",
    "js",
    "metadata-navigation.js",
    package = "LightLogWeb",
    mustWork = TRUE
  )
  harness <- tempfile(fileext = ".cjs")
  on.exit(unlink(harness), add = TRUE)
  # A minimal DOM/message harness executes the actual shipped handler. It is
  # deterministic delivery-order coverage, not a substitute for real UI tests.
  writeLines(
    r"---(
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const handlers = {}, events = {}, requests = [], nodes = new Map(), grids = [];
let assertions = 0;
const equal = (a, b) => { assertions++; assert.deepEqual(a, b); };
const element = () => ({ value: 'all', disabled: false, hidden: false,
  setAttribute(k, v) { this[k] = v; },
  focus() { document.activeElement = this; }, getClientRects() { return [{}]; },
  replaceChildren(fragment) { this.options = fragment.children; },
  dispatchEvent() { events['change.llwMetadataSections'].call(this); } });
const makeGrid = id => {
  const el = Object.fromEntries(['source', 'layer', 'select', 'slot', 'status', 'feedback', 'retry']
    .map(k => [k, element()]));
  const grid = { id, dataset: { sectionSource: id + '-source', sectionLayer: id + '-layer',
    sectionInput: id + '-section', sectionRequest: id + '-request' },
    matches: () => true, querySelectorAll: () => [el.source, el.layer, el.select],
    querySelector: s => el[{'.llw-metadata-section-slot':'slot',
      '.llw-metadata-section-status':'status', '.llw-metadata-section-feedback':'feedback',
      '.llw-metadata-section-retry':'retry'}[s]] };
  Object.values(el).forEach(x => { x.closest = () => grid; });
  nodes.set(id, grid);
  nodes.set(grid.dataset.sectionSource, el.source);
  nodes.set(grid.dataset.sectionLayer, el.layer);
  nodes.set(grid.dataset.sectionInput, el.select);
  grids.push(grid);
  return { grid, el };
};
const a = makeGrid('a'), b = makeGrid('b');
const document = { getElementById: id => nodes.get(id),
  querySelectorAll: () => grids, addEventListener: () => {},
  createElement: () => ({}),
  createDocumentFragment: () => ({children: [], appendChild(x) { this.children.push(x); }}) };
const Shiny = { addCustomMessageHandler: (name, f) => { handlers[name] = f; },
  setInputValue: (id, value) => requests.push({id, ...value}) };
const window = {Shiny, jQuery: () => ({on: (name, ...args) => {events[name] = args.at(-1);}})};
vm.runInNewContext(fs.readFileSync(process.argv[2], 'utf8'),
  {window, document, Shiny, Event: class {constructor(type) {this.type = type;}}});
const message = handlers['llw-metadata-section'];
const last = () => requests.at(-1);
const change = (node, value) => {node.value = value; events['change.llwMetadataSections'].call(node);};
const context = (target, epoch, reset = false, selected = null) =>
  message({target, kind:'context', epoch, reset, selected});
const reply = (r, selected = r.resource, visible = true) => ({
  target: r.id.replace('-request', ''), kind:'choices', epoch:r.epoch,
  generation:r.generation, namespace:r.namespace, layer:r.layer, selected, visible,
  choices:[{value:'all',label:'All sections'}, {value:'core_metadata',label:'Core Metadata'},
    {value:'dataset_inventory',label:'Dataset Inventory'}]});
context('a', 1, true);
equal(a.el.select.disabled, true);
message(reply(last()));
equal(a.el.select.disabled, false);
change(a.el.select, 'core_metadata');
change(a.el.source, 'iztech'); const oldB = {...last()};
equal(a.el.select.disabled, true);
equal(a.el.status.textContent, 'Updating sections...');
change(a.el.source, 'all'); const newA = {...last()};
equal(newA.generation > oldB.generation, true);
message(reply(oldB, 'all'));
equal(a.el.select.disabled, true);
message(reply(newA));
equal(a.el.select.value, 'core_metadata');
equal(a.el.select.disabled, false);
change(a.el.select, 'dataset_inventory');
message(reply(newA, 'all'));
equal(a.el.select.value, 'dataset_inventory');
change(a.el.source, 'iztech'); const obsolete = {...last()};
context('a', 2, true); const reset = {...last()};
message(reply(obsolete));
equal(a.el.select.disabled, true);
message(reply(reset));
equal(a.el.select.value, 'all');
change(a.el.select, 'core_metadata');
context('a', 2, true);
equal(a.el.select.value, 'core_metadata');
equal(last().generation, reset.generation);
change(a.el.layer, 'overlay');
message({...reply(last()), kind:'error'});
equal(a.el.select.disabled, true);
equal(a.el.status.textContent.includes('Retry'), true);
events['click.llwMetadataSections'].call(a.el.retry);
document.activeElement = a.el.retry;
message(reply(last(), 'all', false));
equal(a.el.slot.hidden, true);
equal(a.el.select.disabled, true);
equal(document.activeElement, a.el.source);
change(a.el.layer, 'all'); message(reply(last()));
change(a.el.select, 'core_metadata');
context('a', 3); message(reply(last()));
equal(a.el.select.value, 'core_metadata');
context('b', 1, true); message(reply(last()));
change(b.el.select, 'dataset_inventory');
context('a', 4, false, 'core_metadata'); message(reply(last()));
equal(b.el.select.value, 'dataset_inventory');
equal(a.el.select.value, 'core_metadata');
events['shiny:disconnected.llwMetadataSections']();
equal(a.el.select.disabled, true);
equal(a.el.retry.disabled, true);
events['shiny:connected.llwMetadataSections']();
equal(a.el.retry.disabled, false);
context('a', 4); const reconnect = {...last()};
message(reply(reconnect));
equal(a.el.select.value, 'core_metadata');
equal(a.el.select.disabled, false);
change(a.el.source, 'all');
const escaped = reply(last()); escaped.choices[1].label = '<img onerror=alert(1)>';
message(escaped);
equal(a.el.select.options[1].textContent, '<img onerror=alert(1)>');
equal(a.el.select.options[1].innerHTML, undefined);
equal(a.el.feedback.hidden, true);
console.log('Section protocol assertions passed: ' + assertions);
)---",
    harness
  )
  result <- system2(
    node,
    c(shQuote(harness), shQuote(script)),
    stdout = TRUE,
    stderr = TRUE
  )
  expect_null(attr(result, "status"), info = paste(result, collapse = "\n"))
  expect_match(
    paste(result, collapse = "\n"),
    "Section protocol assertions passed:",
    fixed = TRUE
  )
})

test_that("rapid filters and delayed input echoes do not rewrite their own controls", {
  fixture_record <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )[["dataset_m7appendedreview"]]
  messages <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages) + 1L]] <<- list(
      id = sub("^.*-", "", inputId),
      value = message
    )
  }
  input_messages <- function(ids) Filter(function(x) x$id %in% ids, messages)
  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(fixture_record)),
    session = mock_session,
    {
      session$setInputs(
        namespace = "all",
        layer = "all",
        resource = "core_metadata",
        query = "MEDI",
        sort_by = "value",
        descending = TRUE,
        clear_query = 0L
      )
      initial_total <- query_result()$total
      messages <<- list()
      for (layer in c(
        "LightLogWeb overlay",
        "all",
        "LightLogWeb overlay",
        "all",
        "LightLogWeb overlay"
      )) {
        session$setInputs(layer = layer)
        expect_identical(selected_layer(), layer)
        expect_length(input_messages(c("namespace", "layer", "sort_by")), 0L)
        section_messages <- input_messages("resource")
        if (length(section_messages)) {
          session$setInputs(
            resource = tail(section_messages, 1L)[[1L]]$value$value
          )
        }
        before_echo <- length(messages)
        session$setInputs(layer = layer)
        session$flushReact()
        expect_length(messages, before_echo)
      }
      expect_identical(selected_section(), "all")
      expect_identical(input$sort_by, "value")
      expect_true(input$descending)
      expect_lt(query_result()$total, initial_total)

      messages <<- list()
      session$setInputs(clear_query = 1L)
      expect_length(input_messages("query"), 1L)
      expect_identical(input_messages("query")[[1L]]$value$value, "")
      expect_length(
        input_messages(c("namespace", "layer", "resource", "sort_by")),
        0L
      )
      session$setInputs(query = "")
      expect_identical(selected_layer(), "LightLogWeb overlay")
      expect_identical(query_result()$total, 2L)
      stable_messages <- length(messages)
      for (step in seq_len(8L)) session$flushReact()
      expect_length(messages, stable_messages)

      session$setInputs(
        layer = "all",
        resource = "core_metadata",
        query = "MEDI"
      )
      messages <<- list()
      session$setInputs(clear_query = 2L)
      session$setInputs(query = "")
      expect_identical(selected_section(), "core_metadata")
      expect_length(
        input_messages(c("namespace", "layer", "resource", "sort_by")),
        0L
      )
    }
  )
})

test_that("filter updates retain Apply Undo context and reset once for a new dataset", {
  records <- metadata_explorer_fixture_records(m7_fixture_directory())
  fixture_record <- records[["dataset_m7appendedreview"]]
  context <- metadata_overlay_context(fixture_record)
  candidates <- context$catalog
  target <- candidates$target_id[
    candidates$field == "unit" &
      candidates$name == "MEDI" &
      candidates$file_group_id == "MELIDOS_IZTECH_S004:19"
  ]
  expect_length(target, 1L)
  applied <- apply_metadata_overlay(
    fixture_record,
    context,
    metadata_overlay_preview(
      fixture_record,
      context,
      target,
      "klx",
      scope = "single"
    )
  )
  undone <- undo_metadata_overlay(applied, context)
  current <- reactiveVal(fixture_record)
  messages <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages) + 1L]] <<- list(
      id = sub("^.*-", "", inputId),
      value = message
    )
  }
  input_messages <- function(ids) Filter(function(x) x$id %in% ids, messages)
  testServer(
    metadata_explorer_server,
    args = list(dataset = current),
    session = mock_session,
    {
      iztech <- names(bundle()$sources)[grepl(
        "iztech",
        names(bundle()$sources)
      )]
      tum <- names(bundle()$sources)[grepl("tum", names(bundle()$sources))]
      session$setInputs(
        namespace = iztech,
        layer = "all",
        resource = "core_metadata",
        query = "dataset_file_variables_units",
        sort_by = "value"
      )
      medi <- query_result()$rows$group_id[
        query_result()$rows$group_context == "MEDI"
      ]
      expect_length(medi, 1L)
      session$setInputs(group_id = medi)
      messages <<- list()
      current(applied)
      session$flushReact()
      expect_identical(selected_namespace(), iztech)
      expect_identical(selected_group()$group$group_id, medi)
      expect_length(
        input_messages(c("namespace", "layer", "resource", "sort_by", "query")),
        0L
      )
      session$setInputs(namespace = tum)
      messages <<- list()
      current(undone)
      session$flushReact()
      expect_identical(selected_namespace(), tum)
      expect_length(
        input_messages(c("namespace", "layer", "resource", "sort_by", "query")),
        0L
      )
      session$setInputs(namespace = iztech)
      session$setInputs(
        namespace = "all",
        layer = "LightLogWeb overlay",
        query = ""
      )
      expect_identical(selected_namespace(), "all")
      expect_identical(selected_layer(), "LightLogWeb overlay")
      expect_identical(query_result()$total, 3L)
      messages <<- list()
      session$setInputs(resource = "all")
      for (step in seq_len(8L)) session$flushReact()
      expect_length(
        input_messages(c("namespace", "layer", "resource", "sort_by", "query")),
        0L
      )

      current(records[["dataset_m7absent"]])
      session$flushReact()
      resets <- input_messages(c("namespace", "layer", "resource", "query"))
      reset_counts <- table(vapply(resets, `[[`, "", "id"))
      expect_identical(
        names(reset_counts),
        c("layer", "namespace", "query")
      )
      expect_true(all(reset_counts == 1L))
      expect_identical(selected_namespace(), "all")
      expect_identical(selected_layer(), "all")
      expect_null(selected_group())
      messages <<- list()
      session$setInputs(
        namespace = "all",
        layer = "all",
        resource = "all",
        query = ""
      )
      for (step in seq_len(8L)) session$flushReact()
      expect_length(
        input_messages(c("namespace", "layer", "resource", "sort_by", "query")),
        0L
      )
      expect_identical(
        current()$metadata_bundle$sources,
        records[["dataset_m7absent"]]$metadata_bundle$sources
      )
    }
  )
  expect_identical(
    undone$metadata_bundle$sources,
    fixture_record$metadata_bundle$sources
  )
  expect_identical(undone$raw_payload, fixture_record$raw_payload)
})

test_that("section filters use meaningful logical areas before the result cap", {
  expected_sections <- c(
    "All sections" = "all",
    "Core Metadata" = "core_metadata",
    "Dataset Inventory" = "dataset_inventory",
    "Variable Inventory" = "variable_inventory"
  )
  expected_totals <- c(
    `tum-medi-only` = 354L,
    `iztech-light-subset` = 326L,
    `ucr-all-light-variables` = 723L
  )

  for (fixture_id in names(expected_totals)) {
    index <- m7_fixture(fixture_id)$metadata_snapshot$bundle$index
    section_index <- metadata_section_index(index)
    choices <- metadata_section_choices(section_index)
    expect_identical(choices[seq_along(expected_sections)], expected_sections)

    unfiltered <- metadata_index_query_groups_by_section(
      index,
      section_index = section_index,
      limit = 40L
    )
    expect_identical(unfiltered$total, expected_totals[[fixture_id]])
    expect_identical(nrow(unfiltered$rows), 40L)
    expect_true(unfiltered$limited)

    datasets <- metadata_index_query_groups_by_section(
      index,
      section_index = section_index,
      section_id = "dataset_inventory",
      limit = 40L
    )
    expect_identical(datasets$total, 13L)
    expect_identical(nrow(datasets$rows), 13L)
    expect_false(datasets$limited)
    expect_true(all(
      section_index$section_id[
        match(datasets$rows$group_id, section_index$group_id)
      ] ==
        "dataset_inventory"
    ))
  }
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
  appended_section_index <- metadata_section_index(appended_bundle$index)
  appended_sections <- metadata_section_choices(appended_section_index)
  expect_true(all(
    c("core_metadata", "dataset_inventory", "variable_inventory") %in%
      unname(appended_sections)
  ))
  for (namespace_id in expected_lineage) {
    scoped_sections <- metadata_section_choices(
      appended_section_index,
      namespace_id = namespace_id,
      layer = "Immutable source"
    )
    expect_true(all(
      c("core_metadata", "dataset_inventory", "variable_inventory") %in%
        unname(scoped_sections)
    ))
  }
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

      session$setInputs(resource = "dataset_inventory")
      session$flushReact()
      expect_identical(status()$match_count, 13L)
      expect_identical(nrow(query_result()$rows), 13L)
      expect_true(all(
        vapply(
          seq_len(nrow(query_result()$rows)),
          function(row) {
            metadata_group_section_id(
              query_result()$rows[row, , drop = FALSE]
            ) ==
              "dataset_inventory"
          },
          logical(1)
        )
      ))
      session$setInputs(resource = "all")
      session$flushReact()
      expect_identical(status()$match_count, initial_total)

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

test_that("navigation requests focus but filters and in-place field choices do not", {
  record <- m7_fixture_record("iztech-light-subset")
  messages <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendCustomMessage <- function(type, message) {
    if (identical(type, "llw-metadata-focus")) {
      messages[[length(messages) + 1L]] <<- message
    }
  }
  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(record)),
    session = mock_session,
    {
      session$setInputs(query = "MEDI")
      session$flushReact()
      expect_length(messages, 0L)
      id <- query_result()$rows$group_id[[1L]]
      session$setInputs(group_id = id)
      session$flushReact()
      expect_identical(tail(messages, 1L)[[1L]]$group_id, id)
      expect_identical(
        tail(messages, 1L)[[1L]]$target,
        session$ns("hierarchy_focus")
      )
      count <- length(messages)
      session$setInputs(sort_by = "field", descending = TRUE)
      session$flushReact()
      expect_length(messages, count)
      sibling <- hierarchy_neighborhood()$property_groups$group_id[[1L]]
      session$setInputs(neighborhood_group_id = sibling)
      session$flushReact()
      expect_length(messages, count)
      expect_identical(status()$selected_group_id, sibling)
      count <- length(messages)
      session$setInputs(hierarchy_path_node_id = "stale-node")
      session$flushReact()
      expect_length(messages, count)
      root <- hierarchy_roots()$node_id[[1L]]
      metadata_test_open_node(session, root, areas = TRUE)
      session$flushReact()
      expect_gt(length(messages), count)
      expect_identical(status()$selected_group_id, sibling)
    }
  )
})

test_that("full-path dialogs keep native dismissal separate from validated navigation", {
  fixture_record <- m7_fixture_record("ucr-all-light-variables")
  messages <- list()
  modals <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendCustomMessage <- function(type, message) {
    if (identical(type, "llw-metadata-focus")) {
      messages[[length(messages) + 1L]] <<- message
    }
  }
  mock_session$sendModal <- function(type, message) {
    modals[[length(modals) + 1L]] <<- list(type = type, message = message)
  }
  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(fixture_record)),
    session = mock_session,
    {
      session$setInputs(query = "Melanopic equivalent daylight illuminance")
      rows <- query_result()$rows
      id <- rows$group_id[
        rows$field == "dataset_file_variables_labels" &
          rows$group_context == "MEDI"
      ][[1L]]
      session$setInputs(group_id = id)
      selected_focus <- focused_node_id()
      count <- length(messages)
      session$setInputs(hierarchy_full_path = 1L)
      expect_length(messages, count)
      expect_identical(focused_node_id(), selected_focus)
      expect_identical(tail(modals, 1L)[[1L]]$type, "show")
      html <- tail(modals, 1L)[[1L]]$message$html
      expect_match(html, "llw-metadata-navigation-dialog", fixed = TRUE)
      expect_match(
        html,
        session$ns("hierarchy_dialog_path_node_id"),
        fixed = TRUE
      )
      expect_match(html, 'data-bs-dismiss="modal">Close</button>', fixed = TRUE)
      expect_match(html, 'type="button"', fixed = TRUE)
      session$setInputs(hierarchy_dialog_path_node_id = "stale-node")
      expect_length(messages, count)
      expect_identical(focused_node_id(), selected_focus)
      expect_length(modals, 1L)
      root <- hierarchy_roots()$node_id[[1L]]
      session$setInputs(hierarchy_dialog_path_node_id = root)
      session$flushReact()
      expect_identical(tail(modals, 1L)[[1L]]$type, "remove")
      expect_identical(focused_node_id(), root)
      expect_identical(active_group_id(), id)
      expect_length(messages, count + 1L)
      expect_identical(
        tail(messages, 1L)[[1L]]$target,
        session$ns("hierarchy_focus")
      )
      expect_identical(tail(messages, 1L)[[1L]]$group_id, id)
      session$setInputs(hierarchy_full_path = 2L)
      expect_identical(tail(modals, 1L)[[1L]]$type, "show")
      expect_identical(focused_node_id(), root)
      expect_length(messages, count + 1L)
      expect_identical(
        fixture_record$metadata_bundle$sources,
        dataset()$metadata_bundle$sources
      )
    }
  )
})

test_that("field selection updates values without rebuilding the scrolled list", {
  for (fixture in c(
    "tum-medi-only",
    "iztech-light-subset",
    "ucr-all-light-variables"
  )) {
    fixture_record <- m7_fixture_record(fixture)
    updates <- list()
    mock_session <- shiny::MockShinySession$new()
    mock_session$sendInputMessage <- function(inputId, message) {
      if (grepl("neighborhood_group_id$", inputId)) {
        updates[[length(updates) + 1L]] <<- message
      }
    }
    testServer(
      metadata_explorer_server,
      args = list(dataset = reactive(fixture_record)),
      session = mock_session,
      {
        session$setInputs(query = "participant_age", sort_by = "value")
        id <- query_result()$rows$group_id[[1L]]
        session$setInputs(group_id = id)
        session$flushReact()
        contents <- output$item_properties
        context <- hierarchy_context()
        choices <- hierarchy_neighborhood()$property_groups$group_id
        selected <- setdiff(choices, id)[[1L]]
        session$setInputs(neighborhood_group_id = selected)
        session$flushReact()
        expect_identical(output$item_properties, contents)
        expect_identical(hierarchy_context(), context)
        expect_identical(status()$selected_group_id, selected)
        expect_match(
          output$entry_detail$html,
          selected_group()$group$field[[1L]],
          fixed = TRUE
        )
        expect_identical(tail(updates, 1L)[[1L]]$value, selected)
        expect_null(tail(updates, 1L)[[1L]]$options)
        expect_identical(input$query, "participant_age")
        expect_identical(input$sort_by, "value")

        # Search/crosslink selections at the same focus still update the radio.
        activate_group(id, reset_focus = FALSE)
        session$flushReact()
        expect_identical(output$item_properties, contents)
        expect_identical(tail(updates, 1L)[[1L]]$value, id)
        expect_identical(
          bundle()$source_fingerprint,
          fixture_record$metadata_bundle$source_fingerprint
        )

        session$setInputs(hierarchy_up = 1)
        session$flushReact()
        expect_false(identical(output$item_properties, contents))
        metadata_test_go_to_areas(session)
        expect_null(output$item_count)
      }
    )
  }
})

test_that("overlay Apply and Undo retain source selection, search and hierarchy focus", {
  record <- m7_fixture_record("iztech-light-subset")
  current <- shiny::reactiveVal(record)
  context <- metadata_overlay_context(record)
  shiny::testServer(metadata_explorer_server, args = list(dataset = current), {
    session$flushReact()
    session$setInputs(query = "MEDI", sort_by = "path")
    id <- query_result()$rows$group_id[[1L]]
    session$setInputs(group_id = id)
    # Select a sibling in the hierarchy so preserving just the search result
    # would wrongly replace the user's current field after a commit.
    properties <- hierarchy_neighborhood()$property_groups
    other <- properties$group_id[properties$group_id != id][[1L]]
    session$setInputs(neighborhood_group_id = other)
    before <- session$getReturned()$status()
    target <- context$catalog$target_id[context$catalog$field == "sex"][[1L]]
    changed <- apply_metadata_overlay(
      current(),
      context,
      metadata_overlay_preview(current(), context, target, "Test assertion")
    )
    current(changed)
    session$flushReact()
    expect_identical(input$query, "MEDI")
    expect_identical(
      session$getReturned()$status()$selected_group_id,
      before$selected_group_id
    )
    expect_identical(
      session$getReturned()$status()$focused_node_id,
      before$focused_node_id
    )
    current(undo_metadata_overlay(changed, context))
    session$flushReact()
    expect_identical(
      session$getReturned()$status()$selected_group_id,
      before$selected_group_id
    )
    expect_identical(
      session$getReturned()$status()$focused_node_id,
      before$focused_node_id
    )
  })
})

test_that("dataset changes clear selected metadata and hierarchy context", {
  selected <- shiny::reactiveVal(m7_fixture_record(
    "ucr-all-light-variables"
  ))

  testServer(
    metadata_explorer_server,
    args = list(dataset = selected),
    {
      session$setInputs(query = "MEDI")
      session$flushReact()
      first_group_id <- query_result()$rows$group_id[[1L]]
      session$setInputs(group_id = first_group_id)
      session$flushReact()
      expect_false(is.null(selected_group()))
      expect_false(is.null(hierarchy_context()))

      selected(m7_fixture_record("tum-medi-only"))
      session$flushReact()
      expect_null(selected_group())
      expect_null(hierarchy_context())
      expect_null(status()$selected_group_id)
      expect_null(status()$focused_node_id)
    }
  )
})

test_that("a refreshed record never displays a stale checked metadata field", {
  original <- m7_fixture_record("tum-medi-only")
  selected <- shiny::reactiveVal(original)

  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(selected())),
    {
      session$setInputs(query = "MEDI")
      session$flushReact()
      first_group_id <- query_result()$rows$group_id[[1L]]
      session$setInputs(group_id = first_group_id)
      session$flushReact()
      expect_identical(status()$selected_group_id, first_group_id)

      refreshed <- original
      refreshed$name <- paste(original$name, "refreshed")
      selected(refreshed)
      session$flushReact()

      expect_null(status()$selected_group_id)
      result_html <- output$result_list
      result_html <- if (is.list(result_html) && !is.null(result_html$html)) {
        result_html$html
      } else {
        paste(as.character(result_html), collapse = "")
      }
      expect_false(grepl('checked="checked"', result_html, fixed = TRUE))
    }
  )
})

test_that("external crosslinks select one exact in-memory metadata context", {
  fixture_record <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )[["dataset_m7iztechlightsubset"]]
  namespace_id <- names(fixture_record$metadata_bundle$sources)[[1L]]
  selected <- shiny::reactiveVal(fixture_record)
  focus <- shiny::reactiveVal(NULL)

  testServer(
    metadata_explorer_server,
    args = list(
      dataset = reactive(selected()),
      focus_request = reactive(focus())
    ),
    {
      session$flushReact()
      focus(new_metadata_focus_request(
        dataset_id = selected()$id,
        namespace_id = namespace_id,
        field = "participant_internal_id",
        path_contains = "/core_metadata/participants",
        record_identity = "IZTECH_S004",
        label = "Participant IZTECH_S004"
      ))
      session$flushReact()

      expect_identical(status()$external_focus$state, "resolved")
      expect_identical(
        selected_group()$group$field,
        "participant_internal_id"
      )
      expect_identical(
        hierarchy_context()$node$record_identity,
        "IZTECH_S004"
      )

      stale_group <- query_result()$rows$group_id[
        query_result()$rows$group_id != selected_group()$group$group_id[[1L]]
      ][[1L]]
      session$setInputs(group_id = stale_group)
      session$flushReact()
      expect_identical(
        hierarchy_context()$node$record_identity,
        "IZTECH_S004"
      )
      expect_identical(status()$external_focus$state, "resolved")
      session$setInputs(group_id = selected_group()$group$group_id[[1L]])
      session$flushReact()
      session$setInputs(
        neighborhood_group_id = selected_group()$group$group_id[[1L]]
      )
      session$flushReact()
      expect_identical(
        hierarchy_context()$node$record_identity,
        "IZTECH_S004"
      )

      participant_age <- selected_neighborhood()$property_groups$group_id[
        selected_neighborhood()$property_groups$field == "participant_age"
      ][[1L]]
      session$setInputs(neighborhood_group_id = participant_age)
      session$flushReact()
      expect_identical(selected_group()$group$field, "participant_age")
      expect_identical(
        hierarchy_context()$node$record_identity,
        "IZTECH_S004"
      )

      device_request <- new_metadata_focus_request(
        dataset_id = selected()$id,
        namespace_id = namespace_id,
        field = "device_internal_id",
        path_contains = "/core_metadata/devices",
        record_identity = "IZTECH_AL04",
        label = "Device IZTECH_AL04"
      )
      device_resolution <- metadata_resolve_focus_request(
        selected()$metadata_bundle,
        device_request
      )
      focus(device_request)
      session$flushReact()
      session$setInputs(query = "IZTECH_AL04")
      session$flushReact()

      expect_identical(status()$external_focus$state, "resolved")
      expect_identical(selected_group()$group$field, "device_internal_id")
      expect_identical(
        hierarchy_context()$node$record_identity,
        "IZTECH_AL04"
      )
      result_html <- output$result_list
      result_html <- if (is.list(result_html) && !is.null(result_html$html)) {
        result_html$html
      } else {
        paste(as.character(result_html), collapse = "")
      }
      expect_match(
        result_html,
        paste0(
          'value="',
          device_resolution$group_id,
          '" checked="checked"'
        ),
        fixed = TRUE
      )
    }
  )
})

test_that("removing a crosslink target clears obsolete success without replaying it", {
  original <- metadata_explorer_showcase_records()[[3L]]
  context <- metadata_overlay_context(original)
  annotated <- apply_metadata_overlay(
    original,
    context,
    metadata_overlay_focus_preview(
      original,
      context,
      "MEDI",
      "Temporary label",
      ""
    )
  )
  for (action in c("undo", "clear")) {
    selected <- shiny::reactiveVal(annotated)
    focus <- shiny::reactiveVal(NULL)
    testServer(
      metadata_explorer_server,
      args = list(dataset = selected, focus_request = focus),
      {
        session$flushReact()
        request <- metadata_resolve_variable(selected(), "MEDI")$target
        focus(request)
        session$flushReact()
        target <- selected_group()$group$group_id[[1L]]
        session$setInputs(query = "MEDI", group_id = target, sort_by = "field")
        expect_identical(status()$external_focus$state, "resolved")
        expect_match(
          output$focus_status$html,
          "exact retained field",
          fixed = TRUE
        )

        removed <- if (action == "undo") {
          undo_metadata_overlay(selected(), context)
        } else {
          apply_metadata_overlay(
            selected(),
            context,
            metadata_overlay_focus_preview(selected(), context, "MEDI", "", "")
          )
        }
        selected(removed)
        session$flushReact()
        expect_null(status()$external_focus)
        expect_null(output$focus_status)
        expect_null(status()$selected_group_id)
        expect_null(status()$focused_node_id)
        expect_null(metadata_resolve_variable(selected(), "MEDI")$target)
        expect_identical(input$query, "MEDI")
        expect_identical(input$sort_by, "field")
        expect_identical(selected()$raw_payload, original$raw_payload)
        expect_identical(
          selected()$metadata_bundle$sources,
          original$metadata_bundle$sources
        )
        expect_match(
          output$hierarchy_content$html,
          "No retained metadata fields",
          fixed = TRUE
        )

        # Restoring the field does not replay an old crosslink. A fresh explicit
        # request can select it again and publish a current success notice.
        selected(apply_metadata_overlay(
          selected(),
          context,
          metadata_overlay_focus_preview(
            selected(),
            context,
            "MEDI",
            "Temporary label",
            ""
          )
        ))
        session$flushReact()
        expect_null(status()$external_focus)
        expect_null(output$focus_status)
        request$request_id <- paste0(request$request_id, "_again")
        focus(request)
        session$flushReact()
        expect_identical(status()$external_focus$state, "resolved")
        expect_identical(status()$selected_group_id, target)
      }
    )
  }
})

test_that("removed overlay crosslinks clear while unrelated source fields remain usable", {
  original <- new_dataset_record(
    data.frame(
      Id = "P01",
      Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
      MEDI = 125
    ),
    "Unrelated retained facts",
    new_source_manifest("test", source_timezone = "UTC"),
    factual_metadata = list(study = list(title = "Retained study")),
    analysis_settings = list(primary_variable = "MEDI"),
    id = "dataset_r61partial"
  )
  context <- metadata_overlay_context(original)
  annotated <- apply_metadata_overlay(
    original,
    context,
    metadata_overlay_focus_preview(
      original,
      context,
      "MEDI",
      "Temporary label",
      ""
    )
  )
  selected <- shiny::reactiveVal(annotated)
  focus <- shiny::reactiveVal(NULL)
  testServer(
    metadata_explorer_server,
    args = list(dataset = selected, focus_request = focus),
    {
      session$flushReact()
      focus(metadata_resolve_variable(selected(), "MEDI")$target)
      session$flushReact()
      session$setInputs(query = "MEDI", namespace = "all", layer = "all")
      selected(apply_metadata_overlay(
        selected(),
        context,
        metadata_overlay_focus_preview(selected(), context, "MEDI", "", "")
      ))
      session$flushReact()
      expect_null(status()$external_focus)
      expect_null(output$focus_status)
      expect_null(status()$selected_group_id)
      expect_identical(input$query, "MEDI")
      expect_identical(input$namespace, "all")
      expect_identical(input$layer, "all")
      expect_identical(
        selected()$metadata_bundle$sources,
        original$metadata_bundle$sources
      )
      groups <- selected()$metadata_bundle$index$groups
      title <- groups$group_id[groups$field == "title"][[1L]]
      session$setInputs(query = "", group_id = title)
      expect_identical(selected_group()$entries$value, "Retained study")
      expect_null(status()$external_focus)
    }
  )
})

test_that("retained factual and inventory crosslinks survive overlay updates", {
  records <- list(
    metadata_explorer_showcase_records()[[1L]],
    m7_fixture_record()
  )
  for (original in records) {
    context <- metadata_overlay_context(original)
    selected <- shiny::reactiveVal(original)
    focus <- shiny::reactiveVal(NULL)
    testServer(
      metadata_explorer_server,
      args = list(dataset = selected, focus_request = focus),
      {
        session$flushReact()
        focus(metadata_resolve_variable(selected(), "MEDI")$target)
        session$flushReact()
        before <- status()
        state <- metadata_overlay_focus_state(selected(), context, "MEDI")
        selected(apply_metadata_overlay(
          selected(),
          context,
          metadata_overlay_focus_preview(
            selected(),
            context,
            "MEDI",
            "Retained target",
            state$fields$unit$value
          )
        ))
        session$flushReact()
        expect_identical(status()$external_focus, before$external_focus)
        expect_identical(status()$selected_group_id, before$selected_group_id)
        expect_identical(status()$focused_node_id, before$focused_node_id)
        selected(undo_metadata_overlay(selected(), context))
        session$flushReact()
        expect_identical(status()$external_focus, before$external_focus)
        expect_identical(status()$selected_group_id, before$selected_group_id)
        expect_identical(status()$focused_node_id, before$focused_node_id)

        selected(metadata_explorer_showcase_records()[[3L]])
        session$flushReact()
        expect_null(status()$external_focus)
        expect_null(output$focus_status)
        expect_null(status()$selected_group_id)
      }
    )
  }
})

test_that("explorer pivots among item properties and logical hierarchy roots", {
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
      roots <- hierarchy_roots()
      expect_identical(
        roots$label,
        c("core_metadata", "dataset_inventory", "variable_inventory")
      )
      expect_identical(roots$kind, c("group", "table", "table"))
      dataset_inventory_id <- roots$node_id[
        roots$label == "dataset_inventory"
      ][[1L]]
      metadata_test_open_node(session, dataset_inventory_id, areas = TRUE)
      session$flushReact()
      expect_identical(
        hierarchy_context()$node$node_id,
        dataset_inventory_id
      )
      expect_identical(
        hierarchy_context()$node$label,
        "dataset_inventory"
      )
      first_dataset <- hierarchy_context()$children$node_id[[1L]]
      metadata_test_open_node(session, first_dataset)
      session$flushReact()
      expect_identical(hierarchy_context()$node$node_id, first_dataset)
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
  detail <- htmltools::renderTags(
    metadata_group_detail_ui(group, bundle)
  )$html
  panel <- htmltools::renderTags(
    metadata_test_hierarchy_panel(group, bundle)
  )$html
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)
  properties <- htmltools::renderTags(metadata_item_properties_ui(
    neighborhood,
    "metadata_properties",
    group$group$group_id[[1L]]
  ))$html
  context <- metadata_hierarchy_node_context(
    bundle$index,
    neighborhood$representative_node_id
  )
  summary <- htmltools::renderTags(
    metadata_hierarchy_summary_ui(group, context)
  )$html

  expect_false(grepl("Where this field lives", detail, fixed = TRUE))
  expect_match(panel, "llw-metadata-location", fixed = TRUE)
  expect_match(panel, "Participants", fixed = TRUE)
  expect_match(panel, "<code>participants</code>", fixed = TRUE)
  expect_lt(
    regexpr("Values in this field", detail)[[1L]],
    regexpr("About this field", detail)[[1L]]
  )
  expect_match(detail, "Repeated records are kept together", fixed = TRUE)
  expect_match(detail, "Exact grouped location", fixed = TRUE)
  expect_match(detail, "Exact retained paths", fixed = TRUE)
  expect_false(grepl("Explore this metadata item", detail, fixed = TRUE))
  expect_false(grepl("Navigate retained metadata", detail, fixed = TRUE))
  expect_false(grepl("type=\"radio\"", detail, fixed = TRUE))
  expect_match(panel, "llw-metadata-breadcrumbs", fixed = TRUE)
  expect_false(grepl("Browse", panel, fixed = TRUE))
  expect_false(grepl(
    "Fields at this level|Navigate retained metadata|Nearby structural branches|Choose an exact item instance",
    panel
  ))
  expect_match(properties, "Fields and contained metadata", fixed = TRUE)
  expect_false(grepl("Explore this metadata item|card-header", properties))
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
  panel <- htmltools::renderTags(
    metadata_test_hierarchy_panel(group, bundle)
  )$html
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)
  properties <- htmltools::renderTags(metadata_item_properties_ui(
    neighborhood,
    "metadata_properties",
    group$group$group_id[[1L]]
  ))$html

  expect_match(properties, "Fields and contained metadata", fixed = TRUE)
  expect_match(properties, "participant_characteristic_value", fixed = TRUE)
  expect_match(properties, "participant_characteristic_unit", fixed = TRUE)
  expect_false(grepl("Use the arrow|Arrows open branches", properties))
  expect_false(grepl("retained items", properties, fixed = TRUE))
  expect_match(
    htmltools::renderTags(metadata_item_count_ui(neighborhood))$html,
    "5 retained items",
    fixed = TRUE
  )
  expect_match(properties, "type=\"radio\"", fixed = TRUE)
  expect_false(grepl("Choose an exact item instance", panel, fixed = TRUE))
  expect_false(grepl("Retained record TUM_S001", panel, fixed = TRUE))
})

test_that("term hierarchy uses logical roots without nearby branch shortcuts", {
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
    metadata_test_hierarchy_panel(group, bundle, dialog = TRUE)
  )$html

  expect_false(grepl("Metadata area|Contained metadata|Browse", html))
  expect_match(html, "Core Metadata", fixed = TRUE)
  expect_false(grepl("Dataset Inventory|Variable Inventory", html))
  expect_false(grepl("Nearby structural branches", html, fixed = TRUE))
  expect_false(grepl("Source dataset", html, fixed = TRUE))
  expect_false(grepl("Core Metadata Section", html, fixed = TRUE))
  expect_false(grepl("dataset_file_variables_units", html, fixed = TRUE))
})

test_that("logical paths collapse wrappers and retain unknown top-level areas", {
  bundle <- m7_fixture("tum-medi-only")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(bundle$index, "participant_age")
  group <- metadata_index_group(bundle$index, result$rows$group_id[[1L]])
  roots <- metadata_hierarchy_logical_roots(bundle$index, group)
  neighborhood <- metadata_index_group_neighborhood(bundle$index, group)
  context <- metadata_hierarchy_node_context(
    bundle$index,
    neighborhood$representative_node_id
  )
  visible_path <- metadata_hierarchy_visible_path(context, roots)

  expect_false("nearby" %in% names(context))
  expect_identical(
    roots$label,
    c("core_metadata", "dataset_inventory", "variable_inventory")
  )
  expect_identical(roots$kind, c("group", "table", "table"))
  expect_false(any(roots$kind == "resource"))
  expect_identical(visible_path$label, c("participants", "TUM_S001"))

  variable_root_id <- roots$node_id[roots$label == "variable_inventory"][[1L]]
  variable_root <- metadata_hierarchy_node_context(
    bundle$index,
    variable_root_id
  )
  expect_identical(
    sort(variable_root$children$label, method = "radix"),
    c("datetime", "melanopic_edi", "participant_id", "source_file")
  )
  expect_identical(
    variable_root$children$logical_item_count,
    rep(5L, 4L)
  )
  medi_node_id <- variable_root$children$node_id[
    variable_root$children$label == "melanopic_edi"
  ][[1L]]
  medi_context <- metadata_hierarchy_node_context(bundle$index, medi_node_id)
  expect_identical(medi_context$node$logical_item_count, 5L)
  expect_identical(nrow(medi_context$exact_nodes), 5L)
  expect_identical(nrow(medi_context$entries), 60L)
  unit_group <- medi_context$groups[
    medi_context$groups$field == "unit",
    ,
    drop = FALSE
  ]
  expect_identical(unit_group$record_count, 5L)
  expect_identical(unit_group$value_preview, "lx")
  medi_panel <- htmltools::renderTags(metadata_hierarchy_context_ui(
    medi_context,
    roots,
    list(root = "root", path = "path", child = "child"),
    group$group$group_id[[1L]]
  ))$html
  expect_match(medi_panel, "melanopic_edi", fixed = TRUE)
  expect_false(grepl(
    "Exact retained source nodes|Contained metadata",
    medi_panel
  ))

  appended <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )[["dataset_m7appendedreview"]]$metadata_bundle
  appended_result <- metadata_index_query_groups(
    appended$index,
    "participant_age"
  )
  appended_group <- metadata_index_group(
    appended$index,
    appended_result$rows$group_id[[1L]]
  )
  appended_roots <- metadata_hierarchy_logical_roots(
    appended$index,
    appended_group
  )
  expect_true(all(
    c(
      "core_metadata",
      "dataset_inventory",
      "variable_inventory",
      "factual_metadata"
    ) %in%
      appended_roots$label
  ))
  expect_false("metadata_bundle" %in% appended_roots$label)
  expect_false("records" %in% appended_roots$label)
})

test_that("metadata-area controls do not override an exact descendant focus", {
  bundle <- m7_fixture("iztech-light-subset")$metadata_snapshot$bundle
  request <- new_metadata_focus_request(
    dataset_id = "dataset_m7iztechlightsubset",
    namespace_id = names(bundle$sources)[[1L]],
    field = "participant_internal_id",
    path_contains = "/core_metadata/participants",
    record_identity = "IZTECH_S004",
    label = "Participant IZTECH_S004"
  )
  resolution <- metadata_resolve_focus_request(bundle, request)
  group <- metadata_index_group(bundle$index, resolution$group_id)
  context <- metadata_hierarchy_node_context(bundle$index, resolution$node_id)
  roots <- metadata_hierarchy_logical_roots(bundle$index, group)
  active_root <- metadata_hierarchy_active_root_id(context, roots)
  root_html <- htmltools::renderTags(metadata_hierarchy_node_list_ui(
    roots,
    "hierarchy_root",
    "Metadata areas",
    selected = if (identical(context$node$node_id[[1L]], active_root)) {
      active_root
    } else {
      character()
    }
  ))$html

  expect_identical(context$node$record_identity, "IZTECH_S004")
  expect_false(grepl("checked=\"checked\"", root_html, fixed = TRUE))

  root_context <- metadata_hierarchy_node_context(bundle$index, active_root)
  root_html <- htmltools::renderTags(metadata_hierarchy_node_list_ui(
    roots,
    "hierarchy_root",
    "Metadata areas",
    selected = if (identical(root_context$node$node_id[[1L]], active_root)) {
      active_root
    } else {
      character()
    }
  ))$html
  expect_match(
    root_html,
    paste0('value="', active_root, '" checked="checked"'),
    fixed = TRUE
  )
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

test_that("appended hierarchy retains exact IZTECH instances without a selector", {
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
  expect_setequal(
    device_neighborhood$item_nodes$record_identity[
      grepl(
        "MELIDOS_IZTECH_S004",
        device_neighborhood$item_nodes$record_identity
      )
    ],
    c("MELIDOS_IZTECH_S004:19", "MELIDOS_IZTECH_S004:20")
  )
  device_html <- htmltools::renderTags(metadata_test_hierarchy_panel(
    device_group,
    bundle,
    focus_node_id = focus_19,
    dialog = TRUE
  ))$html

  expect_match(device_html, "MELIDOS_IZTECH_S004:19", fixed = TRUE)
  expect_false(grepl("MELIDOS_IZTECH_S004:20", device_html, fixed = TRUE))
  expect_false(grepl(
    "Choose an exact item instance",
    device_html,
    fixed = TRUE
  ))
  expect_false(grepl("Exact retained source nodes", device_html, fixed = TRUE))
  expect_match(
    device_neighborhood$item_nodes$path[
      device_neighborhood$item_nodes$node_id == focus_19
    ],
    "/dataset_file[1]",
    fixed = TRUE
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
  focus_20 <- medi_neighborhood$item_nodes$node_id[
    medi_neighborhood$item_nodes$record_identity == "MELIDOS_IZTECH_S004:20"
  ][[1L]]
  expect_true(focus_20 %in% medi_neighborhood$item_nodes$node_id)
  medi_html <- htmltools::renderTags(metadata_test_hierarchy_panel(
    medi_group,
    bundle,
    focus_node_id = focus_20,
    dialog = TRUE
  ))$html

  expect_false(grepl("MELIDOS_IZTECH_S004:19", medi_html, fixed = TRUE))
  expect_false(grepl("Choose an exact item instance", medi_html, fixed = TRUE))
  expect_false(grepl("Exact retained source nodes", medi_html, fixed = TRUE))
  expect_match(medi_html, "MELIDOS_IZTECH_S004:20", fixed = TRUE)
  expect_match(
    medi_neighborhood$item_nodes$path[
      medi_neighborhood$item_nodes$node_id == focus_20
    ],
    "/dataset_file[2]",
    fixed = TRUE
  )
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

test_that("appended indexes exclude LightLogWeb relationship contracts", {
  records <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )
  bundle <- records[["dataset_m7appendedreview"]]$metadata_bundle
  paths <- bundle$index$entries$path
  result <- metadata_index_query_groups(
    bundle$index,
    "glcdp_revision",
    limit = 100L
  )

  expect_false(any(grepl("/relationship_context", paths, fixed = TRUE)))
  expect_identical(result$total, 0L)
  expect_identical(nrow(result$rows), 0L)
})

test_that("appended details show the stable retained source fingerprint", {
  records <- metadata_explorer_fixture_records(
    m7_fixture_directory(),
    include_absent = FALSE
  )
  single <- records[["dataset_m7iztechlightsubset"]]$metadata_bundle
  appended <- records[["dataset_m7appendedreview"]]$metadata_bundle
  wrapper_id <- "dataset_m7iztechlightsubset"
  result <- metadata_index_query_groups(
    appended$index,
    "dataset_crossref_study_id",
    namespace_id = wrapper_id,
    limit = 100L
  )
  expect_gt(result$total, 0L)
  group <- metadata_index_group(
    appended$index,
    result$rows$group_id[[1L]]
  )
  retained <- metadata_display_source_fingerprint(
    appended,
    wrapper_id,
    group$group$group_path[[1L]]
  )

  expect_identical(
    retained$fingerprint,
    single$sources[[1L]]$fingerprint
  )
  expect_identical(retained$label, "Retained source fingerprint")
  expect_false(identical(
    retained$fingerprint,
    appended$sources[[wrapper_id]]$fingerprint
  ))
  html <- htmltools::renderTags(
    metadata_group_detail_ui(group, appended, show_source = TRUE)
  )$html
  expect_match(html, "Retained source fingerprint", fixed = TRUE)
  expect_match(
    html,
    substr(single$sources[[1L]]$fingerprint, 1L, 23L),
    fixed = TRUE
  )
  expect_false(grepl(
    substr(appended$sources[[wrapper_id]]$fingerprint, 1L, 23L),
    html,
    fixed = TRUE
  ))
})

test_that("hierarchy branch navigation does not auto-select another field", {
  bundle <- m7_fixture("iztech-light-subset")$metadata_snapshot$bundle
  result <- metadata_index_query_groups(
    bundle$index,
    "dataset_internal_id",
    limit = 100L
  )
  row <- result$rows[result$rows$field == "dataset_internal_id", , drop = FALSE]
  group <- metadata_index_group(bundle$index, row$group_id[[1L]])
  s004_20 <- bundle$index$nodes$node_id[
    bundle$index$nodes$record_identity == "MELIDOS_IZTECH_S004:20"
  ][[1L]]
  s004_context <- metadata_hierarchy_node_context(bundle$index, s004_20)
  html <- htmltools::renderTags(metadata_property_group_list_ui(
    s004_context$groups,
    "branch_fields",
    group$group$group_id[[1L]]
  ))$html
  panel_html <- htmltools::renderTags(metadata_hierarchy_panel_ui(
    group,
    s004_context,
    metadata_hierarchy_logical_roots(bundle$index, group),
    input_ids = list(field = "branch_fields")
  ))$html

  expect_false(grepl("checked", html, fixed = TRUE))
  expect_false(grepl("Fields at this level|branch_fields", panel_html))
})

test_that("hierarchy focus immediately replaces the item comparison context", {
  iztech <- m7_fixture_record("iztech-light-subset")
  iztech_namespace <- names(iztech$metadata_bundle$sources)[[1L]]
  iztech_focus <- shiny::reactiveVal(NULL)

  testServer(
    metadata_explorer_server,
    args = list(
      dataset = reactive(iztech),
      focus_request = reactive(iztech_focus())
    ),
    {
      iztech_focus(new_metadata_focus_request(
        dataset_id = iztech$id,
        namespace_id = iztech_namespace,
        field = "participant_internal_id",
        path_contains = "/core_metadata/participants",
        record_identity = "IZTECH_S004",
        label = "Participant IZTECH_S004"
      ))
      session$flushReact()
      selected_before <- selected_group()$group$group_id[[1L]]
      expect_identical(
        hierarchy_context()$node$record_identity,
        "IZTECH_S004"
      )

      roots <- hierarchy_roots()
      dataset_root <- roots$node_id[
        roots$label == "dataset_inventory"
      ][[1L]]
      metadata_test_open_node(session, dataset_root, areas = TRUE)
      session$flushReact()
      expect_identical(
        selected_group()$group$group_id[[1L]],
        selected_before
      )
      expect_identical(nrow(hierarchy_neighborhood()$property_groups), 0L)
      empty_html <- htmltools::renderTags(metadata_item_properties_ui(
        hierarchy_neighborhood(),
        "comparison",
        selected_before,
        children = hierarchy_context()$children
      ))$html
      expect_match(
        empty_html,
        "No fields at this hierarchy focus",
        fixed = TRUE
      )
      expect_match(empty_html, "Open a contained item below", fixed = TRUE)
      expect_match(
        empty_html,
        'aria-label="Open MELIDOS IZTECH S004"',
        fixed = TRUE
      )

      s004 <- hierarchy_context()$children$node_id[
        hierarchy_context()$children$label == "MELIDOS_IZTECH_S004"
      ][[1L]]
      metadata_test_open_node(session, s004)
      session$flushReact()
      fields <- hierarchy_neighborhood()$property_groups$field
      expect_true(all(c("dataset_id", "device_ids") %in% fields))
      expect_false(any(
        c("participant_age", "participant_sex", "participant_gender") %in%
          fields
      ))
      expect_identical(
        selected_group()$group$group_id[[1L]],
        selected_before
      )
      comparison_html <- htmltools::renderTags(metadata_item_properties_ui(
        hierarchy_neighborhood(),
        "comparison",
        selected_before
      ))$html
      expect_match(comparison_html, "device_ids", fixed = TRUE)
      expect_false(grepl("participant_age", comparison_html, fixed = TRUE))

      device_ids <- hierarchy_neighborhood()$property_groups$group_id[
        hierarchy_neighborhood()$property_groups$field == "device_ids"
      ][[1L]]
      session$setInputs(neighborhood_group_id = device_ids)
      session$flushReact()
      expect_identical(selected_group()$group$field, "device_ids")
      expect_identical(hierarchy_context()$node$label, "MELIDOS_IZTECH_S004")
    }
  )

  ucr <- m7_fixture_record("ucr-all-light-variables")
  testServer(
    metadata_explorer_server,
    args = list(dataset = reactive(ucr)),
    {
      session$setInputs(query = "Medication")
      session$flushReact()
      medication <- query_result()$rows$group_id[
        query_result()$rows$field == "participant_characteristic_name" &
          query_result()$rows$group_context == "medication"
      ][[1L]]
      session$setInputs(group_id = medication)
      session$flushReact()
      expect_identical(
        selected_group()$group$field,
        "participant_characteristic_name"
      )

      roots <- hierarchy_roots()
      variable_root <- roots$node_id[
        roots$label == "variable_inventory"
      ][[1L]]
      metadata_test_open_node(session, variable_root, areas = TRUE)
      session$flushReact()
      medi <- hierarchy_context()$children$node_id[
        hierarchy_context()$children$label == "melanopic_edi"
      ][[1L]]
      metadata_test_open_node(session, medi)
      session$flushReact()

      fields <- hierarchy_neighborhood()$property_groups$field
      expect_true(all(c("name", "label", "unit") %in% fields))
      expect_false(any(grepl(
        "participant_characteristic",
        fields,
        fixed = TRUE
      )))
      expect_identical(
        selected_group()$group$field,
        "participant_characteristic_name"
      )
      unit <- hierarchy_neighborhood()$property_groups$group_id[
        hierarchy_neighborhood()$property_groups$field == "unit"
      ][[1L]]
      session$setInputs(neighborhood_group_id = unit)
      session$flushReact()
      expect_identical(selected_group()$group$field, "unit")
      expect_identical(hierarchy_context()$node$label, "melanopic_edi")
    }
  )
})

test_that("current overlay values refresh visible details without changing source selection", {
  original <- m7_fixture_record("tum-medi-only")
  context <- metadata_overlay_context(original)
  entries <- context$source_index$entries
  group_id <- entries$group_id[
    entries$field == "dataset_file_variables_labels" &
      entries$group_context == "MEDI"
  ][[1L]]
  current <- shiny::reactiveVal(original)
  view <- shiny::reactive(metadata_overlay_presentation(
    context,
    current()$metadata_bundle$overlay
  ))
  shiny::testServer(
    metadata_explorer_server,
    args = list(dataset = current, editable = TRUE, presentation = view),
    {
      session$flushReact()
      session$setInputs(
        query = "dataset_file_variables_labels",
        group_id = group_id
      )
      before <- selected_group()
      focus <- hierarchy_context()$node$node_id
      targets <- context$catalog[
        context$catalog$table == "variables" &
          context$catalog$field == "label" &
          context$catalog$name == "MEDI",
      ]
      changed <- apply_metadata_overlay(
        current(),
        context,
        metadata_overlay_preview(
          current(),
          context,
          targets$target_id[[1L]],
          "Current owner label",
          scope = "single"
        )
      )
      current(changed)
      session$flushReact()
      expect_identical(selected_group()$entries$value, before$entries$value)
      expect_identical(selected_group()$group$group_id, before$group$group_id)
      expect_identical(hierarchy_context()$node$node_id, focus)
      expect_identical(input$query, "dataset_file_variables_labels")
      expect_match(
        output$entry_detail$html,
        "Current owner label",
        fixed = TRUE
      )
      expect_match(
        output$entry_detail$html,
        "Original source value",
        fixed = TRUE
      )
      expect_match(
        output$item_properties$html,
        "Includes overlay",
        fixed = TRUE
      )
      expect_match(output$result_list$html, "Includes overlay", fixed = TRUE)
      current(undo_metadata_overlay(current(), context))
      session$flushReact()
      expect_false(grepl(
        "Current owner label",
        output$entry_detail$html,
        fixed = TRUE
      ))
      expect_false(grepl(
        "Includes overlay",
        output$item_properties$html,
        fixed = TRUE
      ))
      expect_identical(selected_group()$group$group_id, before$group$group_id)
    }
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
