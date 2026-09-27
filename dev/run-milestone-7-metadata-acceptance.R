# Run the offline Milestone 7 metadata performance and memory gates.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required for metadata acceptance.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The `jsonlite` package is required for metadata acceptance output.")
}

pkgload::load_all(".", export_all = TRUE, quiet = TRUE)

benchmarked_at <- Sys.getenv("LLW_M7_BENCHMARKED_AT", unset = NA_character_)
if (
  is.na(benchmarked_at) ||
    !grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      benchmarked_at
    )
) {
  stop("Set LLW_M7_BENCHMARKED_AT to one explicit UTC timestamp.")
}

budgets <- list(
  interaction_design_target_p95_ms = 50,
  interaction_hard_gate_p95_ms = 100,
  overlay_large_commit_p95_ms = 500,
  overlay_large_single_remove_p95_ms = 150,
  retained_bundle_max_bytes = 32 * 1024^2,
  three_fixture_bundles_max_bytes = 64 * 1024^2,
  scaled_index_max_bytes = 64 * 1024^2,
  section_index_max_bytes = 16 * 1024^2,
  hierarchy_navigation_max_alloc_bytes = 10 * 1024^2,
  hierarchy_panel_render_max_alloc_bytes = 10 * 1024^2,
  hierarchy_dialog_render_max_alloc_bytes = 10 * 1024^2,
  focus_contents_render_max_alloc_bytes = 10 * 1024^2,
  table_view_restore_max_alloc_bytes = 10 * 1024^2,
  filter_choices_max_alloc_bytes = 10 * 1024^2,
  relationship_add_max_alloc_bytes = 64 * 1024^2,
  relationship_add_min_rows = 50000L,
  scaled_inventory_min_entries = 50000L
)

fixture_directory <- file.path("tests", "testthat", "fixtures", "milestone-7")
manifest <- jsonlite::read_json(
  file.path(fixture_directory, "manifest.json"),
  simplifyVector = FALSE
)
fixture_ids <- vapply(manifest$fixtures, `[[`, character(1), "fixture_id")
fixtures <- lapply(manifest$fixtures, function(entry) {
  readRDS(file.path(fixture_directory, entry$file))
})
names(fixtures) <- fixture_ids
snapshots <- lapply(fixtures, function(fixture) {
  validate_glc_source_metadata_snapshot(fixture$metadata_snapshot)
})
bundles <- lapply(snapshots, `[[`, "bundle")
review_records <- metadata_explorer_fixture_records(
  fixture_directory,
  include_absent = TRUE
)
appended_review <- review_records[["dataset_m7appendedreview"]]
appended_review_bundle <- appended_review$metadata_bundle

scaled_metadata_index <- function(index, minimum_entries) {
  repeats <- ceiling(minimum_entries / nrow(index$entries))
  repeated_nodes <- lapply(seq_len(repeats), function(repetition) {
    rows <- index$nodes
    suffix <- paste0("_scale_", repetition)
    rows$node_id <- paste0(rows$node_id, suffix)
    has_parent <- !is.na(rows$parent_node_id) & nzchar(rows$parent_node_id)
    rows$parent_node_id[has_parent] <- paste0(
      rows$parent_node_id[has_parent],
      suffix
    )
    rows$path <- paste0(rows$path, "/scale[", repetition, "]")
    rows
  })
  entries <- do.call(
    rbind,
    lapply(seq_len(repeats), function(repetition) {
      rows <- index$entries
      suffix <- paste0("_scale_", repetition)
      rows$entry_id <- paste0(rows$entry_id, "_scale_", repetition)
      rows$node_id <- paste0(rows$node_id, suffix)
      rows$parent_node_id <- paste0(rows$parent_node_id, suffix)
      rows$path <- paste0(rows$path, "/scale[", repetition, "]")
      rows$group_path <- paste0(
        rows$group_path,
        "/scale[",
        repetition,
        "]"
      )
      rows$group_id <- vapply(
        seq_len(nrow(rows)),
        function(row) {
          metadata_group_id(
            rows$namespace_id[[row]],
            rows$layer[[row]],
            rows$resource[[row]],
            rows$group_path[[row]],
            rows$field[[row]],
            rows$group_discriminator[[row]]
          )
        },
        character(1)
      )
      rows$search_text <- paste(rows$search_text, "scale", repetition)
      rows
    })
  )
  entries <- utils::head(entries, minimum_entries)
  rownames(entries) <- NULL
  entries <- metadata_index_add_sort_ranks(entries)
  result <- index
  result$nodes <- do.call(rbind, repeated_nodes)
  rownames(result$nodes) <- NULL
  result$nodes$direct_value_count <- metadata_index_node_counts(
    entries$node_id,
    result$nodes$node_id
  )
  result$nodes$child_node_count <- metadata_index_node_counts(
    result$nodes$parent_node_id,
    result$nodes$node_id
  )
  result$nodes$parent_row <- match(
    result$nodes$parent_node_id,
    result$nodes$node_id,
    nomatch = 0L
  )
  result$entries <- entries
  result$groups <- metadata_index_groups(entries)
  validate_metadata_index(result, result$source_fingerprint)
}

timing_samples_ms <- function(operation, iterations = 200L, warmup = 20L) {
  for (index in seq_len(warmup)) operation()
  samples <- numeric(iterations)
  for (index in seq_len(iterations)) {
    start <- proc.time()[["elapsed"]]
    operation()
    samples[[index]] <- (proc.time()[["elapsed"]] - start) * 1000
  }
  samples
}

timing_evidence <- function(operation, iterations = 200L, warmup = 20L) {
  samples <- timing_samples_ms(operation, iterations, warmup)
  p95 <- unname(stats::quantile(samples, 0.95, type = 8))
  list(
    iterations = iterations,
    p50_ms = unname(stats::quantile(samples, 0.5, type = 8)),
    p95_ms = p95,
    max_ms = max(samples),
    design_target_pass = p95 <= budgets$interaction_design_target_p95_ms,
    hard_gate_pass = p95 <= budgets$interaction_hard_gate_p95_ms
  )
}

allocated_bytes <- function(operation, warmup = 5L) {
  for (index in seq_len(warmup)) operation()
  profile <- tempfile("llw-metadata-allocation-", fileext = ".out")
  on.exit(unlink(profile), add = TRUE)
  utils::Rprofmem(profile)
  on.exit(utils::Rprofmem(NULL), add = TRUE)
  operation()
  utils::Rprofmem(NULL)
  lines <- readLines(profile, warn = FALSE)
  bytes <- suppressWarnings(as.numeric(sub(" .*", "", lines)))
  sum(bytes, na.rm = TRUE)
}

# The production store transaction followed by the production presentation
# consumers. Numeric caches are initialized once. Preparation and cleanup are
# outside each sample, so monotonically increasing revisions do not accumulate
# history or repeatedly time cold measurement work. Browser timing is separate.
benchmark_final_integration <- function(records) {
  source <- new_metadata_source_namespace(
    "integration-wide",
    "Synthetic 5,000 variable occurrences",
    "test",
    records = list(
      variable_inventory = data.frame(
        dataset_id = paste0("D", seq_len(5000L)),
        file_group_id = paste0("D", seq_len(5000L), ":1"),
        name = "MEDI",
        label = "Melanopic EDI",
        unit = "lx",
        type = "numeric"
      )
    )
  )
  wide <- new_dataset_record(
    data.frame(
      Id = "D1",
      Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
      MEDI = 1
    ),
    "Synthetic wide integration inventory",
    new_source_manifest("test", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI"),
    metadata_bundle = new_metadata_bundle(list(`integration-wide` = source)),
    id = "dataset_integrationwide"
  )
  large <- records[[1L]]
  original <- unserialize(large$raw_payload)
  rows <- rep(seq_len(nrow(original)), length.out = 250000L)
  data <- original[rows, , drop = FALSE]
  # Deliberately synthetic time series with the frozen metadata identities.
  # It is a row-scaling probe, not another public-data import or scientific result.
  for (id in unique(data$Id)) {
    positions <- which(data$Id == id)
    data$Datetime[positions] <- original$Datetime[[1L]] +
      seq.int(0, length(positions) - 1L) * 60
  }
  large$raw_payload <- serialize_dataset_data(data)
  large$raw_checksum <- sha256_raw(large$raw_payload)
  large$prepared_payload <- large$raw_payload
  large$display_name <- "Synthetic 250,000-row integration probe"
  large$id <- "dataset_integrationlargerows"
  large <- validate_dataset_record(large)
  cases <- c(
    records,
    list(
      small_example = sample_dataset_record(),
      wide = wide,
      synthetic_250000_rows = large
    )
  )

  one_case <- function(record) {
    context <- metadata_overlay_context(record)
    targets <- context$catalog
    eligible <- which(
      targets$table == "variables" &
        targets$field == "unit" &
        targets$name == "MEDI"
    )
    stopifnot(length(eligible) > 0L)
    target <- targets$target_id[[eligible[[1L]]]]
    target_count <- nrow(metadata_overlay_target_scope(context, target, "all"))
    domain <- shiny::MockShinySession$new()
    on.exit(domain$close(), add = TRUE)
    store <- shiny::withReactiveDomain(domain, new_session_store("local"))
    store$dispatch(new_session_event("add", value = record))
    cache <- new_dashboard_snapshot_cache(
      max_bytes = 384 * 1024^2,
      max_entries = 3L,
      record_bytes = store$record_bytes
    )
    initial <- cache$get(record)
    focus <- dashboard_focus_view(initial$value)
    table_views <- lapply(c("raw_data", "prepared_data"), function(stage) {
      data <- initial$value[[stage]]
      list(
        restore = new_dashboard_table_view_state(),
        columns = names(data),
        rows = nrow(data),
        state = list(
          time = 1000,
          columns = vector("list", ncol(data)),
          search = list(search = "S003"),
          order = list(list(0L, "desc")),
          length = 10L,
          start = 10L
        )
      )
    })
    for (view in table_views) {
      view$restore(NULL, initial$signature, view$columns, view$rows)
    }
    table_tick <- 1000
    restore_table_views <- function() {
      table_tick <<- table_tick + 1
      for (view in table_views) {
        state <- view$state
        state$time <- table_tick
        options <- view$restore(
          state,
          initial$signature,
          view$columns,
          view$rows,
          min(32L, view$rows)
        )
        stopifnot(identical(options$search$search, "S003"))
      }
      invisible(NULL)
    }
    table_view_timing <- timing_evidence(restore_table_views)
    table_view_bytes <- allocated_bytes(restore_table_views)
    inventory_cache <- new_append_ui_cache(
      max_entries = 12L,
      max_bytes = 1024^2
    )
    contract_cache <- new_append_ui_cache(
      max_entries = 24L,
      max_bytes = 2 * 1024^2
    )
    inventory_cache$get(
      dataset_inventory_signature(record),
      function() dataset_record_inventory(record)
    )
    contract_cache$get(
      append_ui_record_signature(record),
      function() append_ui_record_contract(record)
    )
    refresh <- function(updated) {
      snapshot <- cache$get(updated)
      current_focus <- dashboard_refresh_focus_view(focus, snapshot$value)
      restore_table_views()
      inventory <- inventory_cache$get(
        dataset_inventory_signature(updated),
        function() {
          stop("A metadata-only update rebuilt the library inventory")
        }
      )
      inventory <- dataset_inventory_presentation(inventory, updated)
      contract_cache$get(append_ui_record_signature(updated), function() {
        stop("A metadata-only update rebuilt the append source contract")
      })
      presentation <- metadata_overlay_presentation(
        context,
        updated$metadata_bundle$overlay
      )
      stopifnot(
        snapshot$cache_hit,
        identical(snapshot$value$quality, initial$value$quality),
        identical(snapshot$value$coverage, initial$value$coverage),
        identical(current_focus$data, focus$data),
        identical(updated$raw_payload, record$raw_payload),
        identical(updated$prepared_payload, record$prepared_payload),
        identical(updated$analysis_settings, record$analysis_settings),
        identical(
          updated$metadata_bundle$sources,
          record$metadata_bundle$sources
        )
      )
      invisible(updated)
    }
    current <- function() shiny::isolate(store$selected_dataset())
    apply <- function(preview) refresh(store$apply_overlay(context, preview))
    undo <- function() refresh(store$apply_overlay(context, undo = TRUE))
    draft <- function(action = "set", scope = "all") {
      metadata_overlay_preview(
        current(),
        context,
        target,
        if (action == "set") "integration-test-unit" else NULL,
        scope = scope,
        action = action
      )
    }
    measure <- function(name, prepare, operation, cleanup, gate) {
      for (i in seq_len(5L)) {
        prepared <- prepare()
        operation(prepared)
        cleanup()
      }
      samples <- numeric(40L)
      for (i in seq_along(samples)) {
        prepared <- prepare()
        started <- proc.time()[["elapsed"]]
        operation(prepared)
        samples[[i]] <- (proc.time()[["elapsed"]] - started) * 1000
        cleanup()
      }
      prepared <- prepare()
      bytes <- allocated_bytes(function() operation(prepared), warmup = 0L)
      cleanup()
      p95 <- unname(stats::quantile(samples, 0.95, type = 8))
      list(
        operation = name,
        iterations = length(samples),
        p50_ms = unname(stats::quantile(samples, .5, type = 8)),
        p95_ms = p95,
        max_ms = max(samples),
        hard_gate_p95_ms = gate,
        hard_gate_pass = p95 <= gate,
        design_target_pass = p95 <= 50,
        allocated_bytes = bytes,
        allocation_limit = 64 * 1024^2,
        allocation_pass = bytes <= 64 * 1024^2
      )
    }
    batch_gate <- if (target_count >= 5000L) 500 else 100
    operations <- list(measure(
      "apply_batch_and_refresh",
      draft,
      apply,
      undo,
      batch_gate
    ))
    apply(draft())
    operations[[2L]] <- measure(
      "remove_single_and_refresh",
      function() draft("remove", "single"),
      apply,
      undo,
      if (target_count >= 5000L) 150 else 100
    )
    undo()
    operations[[3L]] <- measure(
      "undo_batch_and_refresh",
      function() {
        apply(draft())
        NULL
      },
      function(unused) undo(),
      function() NULL,
      batch_gate
    )
    quick_draft <- function()
      metadata_overlay_focus_preview(
        current(),
        context,
        focus$focus_variable,
        "Quick integration label",
        "quick-test-unit"
      )
    operations[[4L]] <- measure(
      "quick_two_field_save_and_refresh",
      quick_draft,
      apply,
      undo,
      batch_gate
    )
    operations[[5L]] <- measure(
      "quick_two_field_undo_and_refresh",
      function() {
        apply(quick_draft())
        NULL
      },
      function(unused) undo(),
      function() NULL,
      batch_gate
    )
    quick_open <- function()
      metadata_overlay_focus_state(current(), context, focus$focus_variable)
    quick_open_timing <- timing_evidence(quick_open)
    quick_open_bytes <- allocated_bytes(quick_open)
    stopifnot(quick_open_bytes <= 10 * 1024^2)

    # Instrumentation is separate from timing and allocation samples.
    .GlobalEnv$.llw_integration_payloads <- list(
      record$raw_payload,
      record$prepared_payload
    )
    .GlobalEnv$.llw_integration_decodes <- 0L
    trace(
      "unserialize",
      where = baseenv(),
      print = FALSE,
      tracer = quote({
        if (
          is.raw(connection) &&
            any(vapply(
              .GlobalEnv$.llw_integration_payloads,
              function(value) identical(value, connection),
              logical(1)
            ))
        ) {
          .GlobalEnv$.llw_integration_decodes <- .GlobalEnv$.llw_integration_decodes +
            1L
        }
      })
    )
    cleanup_trace <- function() {
      untrace("unserialize", where = baseenv())
      rm(
        list = c(".llw_integration_payloads", ".llw_integration_decodes"),
        envir = .GlobalEnv
      )
    }
    on.exit(cleanup_trace(), add = TRUE)
    bound_checks <- list()
    check_bound <- function(updated) {
      actual <- as.numeric(object.size(updated$metadata_bundle))
      bound <- metadata_overlay_bundle_bytes(context, updated$metadata_bundle)
      bound_checks[[length(bound_checks) + 1L]] <<- list(
        revision = updated$revision,
        bundle_bytes = actual,
        bundle_upper_bound = bound,
        pass = bound >= actual &&
          bound <= 32 * 1024^2 &&
          store$record_bytes(updated) >= as.numeric(object.size(updated))
      )
      invisible(updated)
    }
    check_bound(apply(draft()))
    check_bound(apply(draft("remove", "single")))
    check_bound(undo())
    check_bound(undo())
    quick_open()
    check_bound(apply(quick_draft()))
    check_bound(undo())
    decodes <- .GlobalEnv$.llw_integration_decodes
    stats <- cache$stats()
    actual_cache_bytes <- as.numeric(object.size(cache$get(current())$value))
    list(
      case_id = record$id,
      label = record$display_name,
      measurement_rows = nrow(initial$value$raw_data),
      source_entries = nrow(context$source_index$entries),
      target_count = target_count,
      quick_variable = focus$focus_variable,
      quick_target_count = sum(quick_open()$targets$field == "unit"),
      table_view_timing = table_view_timing,
      table_view_allocated_bytes = table_view_bytes,
      quick_open_timing = quick_open_timing,
      quick_open_allocated_bytes = quick_open_bytes,
      operations = operations,
      measurement_payload_decodes = decodes,
      invariants_pass = decodes == 0L,
      size_bounds = bound_checks,
      size_bounds_pass = all(vapply(bound_checks, `[[`, logical(1), "pass")),
      context_size_cache_bytes = as.numeric(object.size(as.list(
        context$size_cache
      ))),
      cache = stats,
      cache_bounds_pass = stats$bytes <= stats$max_bytes &&
        stats$bytes >= actual_cache_bytes &&
        stats$entries == 1L &&
        stats$builds == 1L,
      actual_cache_bytes = actual_cache_bytes,
      inventory_builds = inventory_cache$stats()$builds,
      append_contract_builds = contract_cache$stats()$builds
    )
  }
  results <- lapply(cases, one_case)
  operations <- unlist(lapply(results, `[[`, "operations"), recursive = FALSE)
  list(
    baseline = "9568feaf9ee646526d24ae7b5714de92940d6a45",
    benchmarked_at = benchmarked_at,
    scope = "Production store Apply/Remove/Undo and atomic Dashboard Label/Unit Save/Undo, plus explorer, library, dashboard, both native table-view restorations and append-contract presentation consumers; R-side timing, not a browser latency claim.",
    cases = results,
    gates = list(
      quick_open_timing_pass = all(vapply(
        results,
        function(x) x$quick_open_timing$p95_ms <= 100,
        logical(1)
      )),
      quick_open_allocation_pass = all(vapply(
        results,
        function(x) x$quick_open_allocated_bytes <= 10 * 1024^2,
        logical(1)
      )),
      hard_timing_pass = all(vapply(
        operations,
        `[[`,
        logical(1),
        "hard_gate_pass"
      )),
      allocation_pass = all(vapply(
        operations,
        `[[`,
        logical(1),
        "allocation_pass"
      )),
      invariants_pass = all(vapply(
        results,
        `[[`,
        logical(1),
        "invariants_pass"
      )),
      size_bounds_pass = all(vapply(
        results,
        `[[`,
        logical(1),
        "size_bounds_pass"
      )),
      cache_bounds_pass = all(vapply(
        results,
        `[[`,
        logical(1),
        "cache_bounds_pass"
      ))
    )
  )
}

if (identical(Sys.getenv("LLW_M7_INTEGRATION_ONLY"), "true")) {
  integration <- benchmark_final_integration(review_records)
  path <- Sys.getenv("LLW_M7_INTEGRATION_OUTPUT")
  stopifnot(nzchar(path))
  jsonlite::write_json(
    integration,
    path,
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = 8
  )
  print(integration)
  quit(status = if (all(unlist(integration$gates))) 0L else 1L)
}

benchmark_overlay_authoring <- function(records, scaled_index) {
  measure <- function(
    operation,
    mutation = FALSE,
    large_commit = FALSE,
    large_single_remove = FALSE,
    iterations = 60L
  ) {
    timing <- timing_evidence(operation, iterations = iterations, warmup = 10L)
    bytes <- allocated_bytes(operation)
    limit <- if (mutation) 64 * 1024^2 else 10 * 1024^2
    timing_limit <- if (large_commit)
      budgets$overlay_large_commit_p95_ms else if (large_single_remove)
      budgets$overlay_large_single_remove_p95_ms else
      budgets$interaction_hard_gate_p95_ms
    timing$hard_gate_pass <- timing$p95_ms <= timing_limit
    c(
      timing,
      list(
        gate_class = if (large_commit)
          "owner-approved large-inventory commit" else if (large_single_remove)
          "owner-approved large-inventory single removal" else
          "ordinary interaction",
        hard_gate_p95_ms = timing_limit,
        allocated_bytes = bytes,
        allocation_limit = limit,
        allocation_pass = bytes <= limit
      )
    )
  }
  one_case <- function(record, field = "sex") {
    context <- metadata_overlay_context(record)
    targets <- context$catalog[context$catalog$field == field, ]
    if (nrow(targets) == 0L)
      targets <- context$catalog[context$catalog$field == "unit", ]
    if (identical(field, "unit")) {
      targets <- targets[
        !targets$variable_type %in% c("string", "boolean", "factor"),
      ]
      melanopic <- targets[
        grepl("MEDI|melanopic", targets$name, ignore.case = TRUE),
      ]
      if (nrow(melanopic) > 0L) targets <- melanopic
    }
    id <- targets$target_id[[1L]]
    batch <- metadata_overlay_preview(
      record,
      context,
      id,
      "Explicit benchmark assertion"
    )
    single <- metadata_overlay_preview(
      record,
      context,
      id,
      "Explicit benchmark assertion",
      "single"
    )
    populated <- apply_metadata_overlay(record, context, batch)
    update <- metadata_overlay_preview(
      populated,
      context,
      id,
      "Another assertion",
      "single"
    )
    remove <- metadata_overlay_preview(
      populated,
      context,
      id,
      action = "remove",
      scope = "single"
    )
    update_batch <- metadata_overlay_preview(
      populated,
      context,
      id,
      "Another assertion"
    )
    remove_batch <- metadata_overlay_preview(
      populated,
      context,
      id,
      action = "remove"
    )
    no_op <- metadata_overlay_preview(
      populated,
      context,
      id,
      "Explicit benchmark assertion"
    )
    updated_single <- apply_metadata_overlay(populated, context, update)
    removed_single <- apply_metadata_overlay(populated, context, remove)
    source_entries <- context$source_index$entries
    source_row <- match(targets$source_path[[1L]], source_entries$path)
    source_selection <- list(
      group = if (!is.na(source_row))
        metadata_index_group(
          context$source_index,
          source_entries$group_id[[source_row]]
        ) else NULL
    )
    overlay_groups <- populated$metadata_bundle$index$groups
    overlay_group <- overlay_groups$group_id[
      overlay_groups$namespace_id == metadata_overlay_namespace_id() &
        overlay_groups$resource == targets$table[[1L]] &
        overlay_groups$field == targets$field[[1L]]
    ][[1L]]
    overlay_selection <- list(
      group = metadata_index_group(
        populated$metadata_bundle$index,
        overlay_group
      )
    )
    functions <- list(
      contextual_effective_presentation = function()
        metadata_overlay_presentation(
          context,
          populated$metadata_bundle$overlay
        ),
      contextual_missing_targets = function()
        metadata_overlay_missing_targets(context),
      contextual_source_selection = function()
        metadata_overlay_explorer_selection(
          context,
          source_selection,
          record$metadata_bundle$overlay
        ),
      contextual_overlay_selection = function()
        metadata_overlay_explorer_selection(
          context,
          overlay_selection,
          populated$metadata_bundle$overlay
        ),
      preview_single = function()
        metadata_overlay_preview(record, context, id, "Draft", "single"),
      preview_batch = function()
        metadata_overlay_preview(record, context, id, "Draft"),
      apply_single = function() apply_metadata_overlay(record, context, single),
      apply_batch = function() apply_metadata_overlay(record, context, batch),
      update_single_populated = function()
        apply_metadata_overlay(populated, context, update),
      update_batch_populated = function()
        apply_metadata_overlay(populated, context, update_batch),
      remove_single_populated = function()
        apply_metadata_overlay(populated, context, remove),
      remove_batch_populated = function()
        apply_metadata_overlay(populated, context, remove_batch),
      no_op_apply = function()
        apply_metadata_overlay(populated, context, no_op),
      undo_batch = function() undo_metadata_overlay(populated, context),
      undo_single_update = function()
        undo_metadata_overlay(updated_single, context),
      undo_single_remove = function()
        undo_metadata_overlay(removed_single, context)
    )
    operations <- lapply(names(functions), function(name) {
      c(
        list(operation = name),
        measure(
          functions[[name]],
          mutation = !startsWith(name, "preview") &&
            !startsWith(name, "contextual"),
          large_commit = nrow(batch$targets) >= 5000L &&
            name %in%
              c(
                "apply_batch",
                "update_batch_populated",
                "remove_batch_populated",
                "undo_batch"
              ),
          large_single_remove = nrow(batch$targets) >= 5000L &&
            identical(targets$table[[1L]], "variables") &&
            identical(name, "remove_single_populated")
        )
      )
    })
    list(
      dataset_id = record$id,
      label = record$display_name,
      edited_field = targets$field[[1L]],
      exact_variable_name = if (identical(targets$table[[1L]], "variables"))
        targets$name[[1L]] else NULL,
      source_entries = nrow(context$source_index$entries),
      target_count = nrow(batch$targets),
      context_bytes = as.numeric(object.size(context)) +
        as.numeric(object.size(as.list(context$size_cache))),
      updated_bundle_bytes = as.numeric(object.size(populated$metadata_bundle)),
      operations = operations
    )
  }
  n <- 5000L
  source <- new_metadata_source_namespace(
    "overlay-stress",
    "Deterministic synthetic overlay stress",
    "test",
    records = list(
      core_metadata = list(
        participants = data.frame(
          participant_internal_id = paste0("P", seq_len(n)),
          participant_sex = "Retained source value"
        )
      )
    )
  )
  stress <- new_dataset_record(
    data.frame(
      Id = "P1",
      Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
      MEDI = 1
    ),
    "5,000-field synthetic overlay inventory",
    new_source_manifest("test", source_timezone = "UTC"),
    metadata_bundle = new_metadata_bundle(list(`overlay-stress` = source)),
    id = "dataset_overlaystress"
  )
  extra_records <- metadata_explorer_showcase_records()[1:2]
  nested_source <- new_metadata_source_namespace(
    "nested-overlay-review",
    "Nested retained source",
    "append_source",
    records = list(metadata_bundle = records[[1L]]$metadata_bundle)
  )
  nested <- new_dataset_record(
    dataset_raw_data(records[[1L]]),
    "Nested source and unknown overlay benchmark",
    new_source_manifest("test", source_timezone = "UTC"),
    metadata_bundle = new_metadata_bundle(list(
      `nested-overlay-review` = nested_source
    )),
    id = "dataset_overlaynested"
  )
  nested_overlay <- new_metadata_overlay(
    nested$metadata_bundle$source_fingerprint,
    unknown = list(
      extension = list(
        label = "Unknown retained field",
        values = c("", NA_character_, "non-ASCII: ä")
      )
    )
  )
  nested$metadata_bundle <- new_metadata_bundle(
    nested$metadata_bundle$sources,
    nested_overlay
  )
  variable_source <- new_metadata_source_namespace(
    "variable-stress",
    "Deterministic synthetic variable stress",
    "test",
    records = list(
      variable_inventory = data.frame(
        dataset_id = paste0("D", seq_len(n)),
        file_group_id = paste0("D", seq_len(n), ":1"),
        name = "MEDI",
        label = "Melanopic EDI",
        unit = "lx",
        type = "numeric"
      )
    )
  )
  variable_stress <- new_dataset_record(
    data.frame(
      Id = "D1",
      Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
      MEDI = 1
    ),
    "5,000-variable-occurrence synthetic overlay inventory",
    new_source_manifest("test", source_timezone = "UTC"),
    metadata_bundle = new_metadata_bundle(list(
      `variable-stress` = variable_source
    )),
    id = "dataset_overlayvariablestress"
  )
  cases <- c(
    lapply(c(records, extra_records, list(nested, stress)), one_case),
    lapply(
      c(records[seq_len(4L)], list(variable_stress)),
      one_case,
      field = "unit"
    )
  )
  context <- metadata_overlay_context(stress)
  target <- context$catalog$target_id[context$catalog$field == "sex"][[1L]]
  patch <- metadata_overlay_preview(
    stress,
    context,
    target,
    "Explicit stress assertion"
  )
  populated <- apply_metadata_overlay(stress, context, patch)
  wide <- context
  wide$source_index <- scaled_index
  wide$source_index$source_fingerprint <- wide$source_fingerprint
  indexed <- measure(
    function()
      metadata_overlay_patch_index(wide, populated$metadata_bundle$overlay),
    mutation = TRUE,
    large_commit = TRUE
  )
  single_patch <- metadata_overlay_preview(
    stress,
    context,
    target,
    "One explicit stress assertion",
    scope = "single"
  )
  single_populated <- apply_metadata_overlay(stress, context, single_patch)
  indexed_single <- measure(
    function()
      metadata_overlay_patch_index(
        wide,
        single_populated$metadata_bundle$overlay
      ),
    mutation = TRUE,
    large_commit = TRUE
  )
  previous <- list(
    overlay = populated$metadata_bundle$overlay,
    index = metadata_overlay_patch_index(
      wide,
      populated$metadata_bundle$overlay
    )
  )
  next_overlay <- previous$overlay
  next_overlay$revision <- next_overlay$revision + 1L
  next_overlay$tables$participants$sex[[1L]] <- "One updated assertion"
  indexed_update <- measure(
    function() metadata_overlay_patch_index(wide, next_overlay, previous),
    mutation = TRUE
  )
  all_operations <- c(
    unlist(lapply(cases, `[[`, "operations"), recursive = FALSE),
    list(indexed, indexed_single, indexed_update)
  )
  list(
    benchmarked_at = benchmarked_at,
    budgets = list(
      ordinary_p95_ms = budgets$interaction_hard_gate_p95_ms,
      large_commit_p95_ms = budgets$overlay_large_commit_p95_ms,
      large_single_remove_p95_ms = budgets$overlay_large_single_remove_p95_ms,
      large_commit_owner_approval = "APPROVE M7 SLICE 3 LARGE-INVENTORY COMMIT BUDGET",
      large_single_remove_owner_approval = "Owner approval, 2026-09-22: 150 ms for warm single removal in the 5,000-variable-occurrence inventory only",
      allocation_bytes = 64 * 1024^2
    ),
    fixture_manifest_sha256 = digest::digest(
      file = file.path(fixture_directory, "manifest.json"),
      algo = "sha256"
    ),
    note = "Offline deterministic fixtures. Timings are local, not a concurrent-capacity guarantee. Wide index patch is an index-only test, not a dataset exceeding the 32 MiB bundle limit.",
    cases = cases,
    source_50000_overlay_5000_index_patch = indexed,
    source_50000_overlay_1_index_patch = indexed_single,
    source_50000_overlay_5000_single_update = indexed_update,
    gates = list(
      hard_timing_pass = all(vapply(
        all_operations,
        `[[`,
        logical(1),
        "hard_gate_pass"
      )),
      allocation_pass = all(vapply(
        all_operations,
        `[[`,
        logical(1),
        "allocation_pass"
      ))
    )
  )
}

# Keep overlay probes independently runnable while owner feedback on the
# isolated module is pending. Prior-slice evidence retains its own timestamp.
if (identical(Sys.getenv("LLW_M7_OVERLAY_ONLY"), "true")) {
  prior <- jsonlite::read_json(
    "dev/milestone-7-metadata-benchmark.json",
    simplifyVector = FALSE
  )
  prior$overlay_authoring <- benchmark_overlay_authoring(
    review_records,
    scaled_metadata_index(bundles[["ucr-all-light-variables"]]$index, 50000L)
  )
  jsonlite::write_json(
    prior,
    "dev/milestone-7-metadata-benchmark.json",
    pretty = TRUE,
    auto_unbox = TRUE,
    digits = 8
  )
  print(prior$overlay_authoring)
  quit(status = if (all(unlist(prior$overlay_authoring$gates))) 0L else 1L)
}

widest_metadata_node_id <- function(index) {
  child_counts <- sort(table(index$nodes$parent_node_id), decreasing = TRUE)
  names(child_counts)[[1L]]
}

render_focus_contents <- function(index, node_id, selected_group_id) {
  context <- metadata_hierarchy_node_context(index, node_id)
  htmltools::renderTags(metadata_item_properties_ui(
    metadata_hierarchy_context_neighborhood(context),
    "properties",
    selected_group_id,
    children = context$children
  ))$html
}

filter_choice_burst <- function(bundle, inventory) {
  first_source <- names(bundle$sources)[[1L]]
  generation <- 0
  for (source in c(first_source, "all")) {
    for (layer in c("all", "LightLogWeb overlay", "Immutable source", "all")) {
      generation <- generation + 1
      choices <- metadata_filter_choices(bundle, inventory, source, layer)
      metadata_filter_visibility(choices)
      reply <- metadata_section_reply(
        list(
          epoch = 1,
          generation = generation,
          namespace = metadata_filter_selection(source, choices$namespace),
          layer = metadata_filter_selection(layer, choices$layer),
          resource = "core_metadata"
        ),
        bundle,
        inventory,
        1
      )
      jsonlite::toJSON(reply, auto_unbox = TRUE)
    }
  }
  invisible(choices)
}

benchmark_index <- function(index, filter_bundle, iterations = 200L) {
  first_namespace <- index$entries$namespace_id[[1L]]
  section_index <- metadata_section_index(index)
  inventory <- metadata_filter_inventory(section_index)
  section_choices <- metadata_section_choices(
    section_index,
    namespace_id = first_namespace
  )
  specific_sections <- setdiff(unname(section_choices), "all")
  selected_section <- if ("dataset_inventory" %in% specific_sections) {
    "dataset_inventory"
  } else {
    specific_sections[[1L]]
  }
  selected_group_id <- index$groups$group_id[[floor(nrow(index$groups) / 2L)]]
  widest_node_id <- widest_metadata_node_id(index)
  operations <- list(
    filter_choice_update = function() {
      metadata_filter_choices(filter_bundle, inventory, first_namespace)
    },
    filter_choice_burst = function() {
      filter_choice_burst(filter_bundle, inventory)
    },
    section_guard_reply = function() {
      metadata_section_reply(
        list(
          epoch = 1,
          generation = 1,
          namespace = "all",
          layer = "all",
          resource = "core_metadata"
        ),
        filter_bundle,
        inventory,
        1
      )
    },
    navigation = function() {
      metadata_section_choices(section_index, first_namespace)
    },
    search = function() {
      metadata_index_query_groups_by_section(
        index,
        section_index = section_index,
        query = "medi",
        limit = 200L
      )
    },
    filtering = function() {
      metadata_index_query_groups_by_section(
        index,
        section_index = section_index,
        namespace_id = first_namespace,
        section_id = selected_section,
        limit = 200L
      )
    },
    sorting = function() {
      metadata_index_query_groups_by_section(
        index,
        section_index = section_index,
        sort_by = "value",
        descending = TRUE,
        limit = 200L
      )
    },
    selection = function() {
      metadata_index_group(index, selected_group_id)
    },
    item_neighborhood = function() {
      metadata_index_group_neighborhood(index, selected_group_id)
    },
    logical_roots = function() {
      group <- metadata_index_group(index, selected_group_id)
      metadata_hierarchy_logical_roots(index, group)
    },
    hierarchy_navigation = function() {
      group <- metadata_index_group(index, selected_group_id)
      neighborhood <- metadata_index_group_neighborhood(
        index,
        selected_group_id
      )
      context <- metadata_hierarchy_node_context(
        index,
        neighborhood$representative_node_id
      )
      roots <- metadata_hierarchy_logical_roots(index, group)
      active_root <- metadata_hierarchy_active_root_id(context, roots)
      destination <- roots$node_id[roots$node_id != active_root]
      if (length(destination) > 0L) {
        metadata_hierarchy_node_context(index, destination[[1L]])
      } else if (nrow(context$children) > 0L) {
        metadata_hierarchy_node_context(index, context$children$node_id[[1L]])
      } else {
        context
      }
    },
    hierarchy_visible_path = function() {
      group <- metadata_index_group(index, selected_group_id)
      neighborhood <- metadata_index_group_neighborhood(
        index,
        selected_group_id
      )
      context <- metadata_hierarchy_node_context(
        index,
        neighborhood$representative_node_id
      )
      roots <- metadata_hierarchy_logical_roots(index, group)
      metadata_hierarchy_visible_path(context, roots)
    },
    hierarchy_parent_navigation = function() {
      group <- metadata_index_group(index, selected_group_id)
      neighborhood <- metadata_index_group_neighborhood(index, group)
      context <- metadata_hierarchy_node_context(
        index,
        neighborhood$representative_node_id
      )
      roots <- metadata_hierarchy_logical_roots(index, group)
      metadata_hierarchy_parent_id(context, roots)
    },
    compact_hierarchy_shell = function() {
      htmltools::renderTags(
        metadata_hierarchy_shell_ui(shiny::NS("metadata"))
      )$html
    },
    hierarchy_panel_render = function() {
      group <- metadata_index_group(index, selected_group_id)
      neighborhood <- metadata_index_group_neighborhood(index, group)
      context <- metadata_hierarchy_node_context(
        index,
        neighborhood$representative_node_id
      )
      roots <- metadata_hierarchy_logical_roots(index, group)
      htmltools::renderTags(metadata_hierarchy_panel_ui(
        group,
        context,
        roots
      ))$html
    },
    hierarchy_dialog_render = function() {
      group <- metadata_index_group(index, selected_group_id)
      neighborhood <- metadata_index_group_neighborhood(index, group)
      context <- metadata_hierarchy_node_context(
        index,
        neighborhood$representative_node_id
      )
      roots <- metadata_hierarchy_logical_roots(index, group)
      htmltools::renderTags(metadata_hierarchy_context_ui(
        context,
        roots,
        list(root = "root", path = "path", child = "child"),
        group$group$group_id[[1L]]
      ))$html
    },
    focus_comparison = function() {
      neighborhood <- metadata_index_group_neighborhood(
        index,
        selected_group_id
      )
      context <- metadata_hierarchy_node_context(
        index,
        neighborhood$representative_node_id
      )
      metadata_hierarchy_context_neighborhood(context)
    },
    focus_contents_render = function() {
      neighborhood <- metadata_index_group_neighborhood(
        index,
        selected_group_id
      )
      render_focus_contents(
        index,
        neighborhood$representative_node_id,
        selected_group_id
      )
    },
    widest_contents_render = function() {
      render_focus_contents(index, widest_node_id, selected_group_id)
    }
  )
  lapply(names(operations), function(name) {
    samples <- timing_samples_ms(operations[[name]], iterations = iterations)
    list(
      operation = name,
      iterations = iterations,
      p50_ms = unname(stats::quantile(samples, 0.5, type = 8)),
      p95_ms = unname(stats::quantile(samples, 0.95, type = 8)),
      max_ms = max(samples),
      design_target_pass = unname(stats::quantile(samples, 0.95, type = 8)) <=
        budgets$interaction_design_target_p95_ms,
      hard_gate_pass = unname(stats::quantile(samples, 0.95, type = 8)) <=
        budgets$interaction_hard_gate_p95_ms
    )
  })
}

bundle_memory <- lapply(names(bundles), function(fixture_id) {
  bundle <- bundles[[fixture_id]]
  list(
    fixture_id = fixture_id,
    source_count = length(bundle$sources),
    node_count = nrow(bundle$index$nodes),
    entry_count = nrow(bundle$index$entries),
    group_count = nrow(bundle$index$groups),
    bundle_bytes = as.numeric(object.size(bundle)),
    pass = as.numeric(object.size(bundle)) <= budgets$retained_bundle_max_bytes
  )
})

scaled_index <- scaled_metadata_index(
  bundles[["ucr-all-light-variables"]]$index,
  budgets$scaled_inventory_min_entries
)
cases <- c(
  lapply(bundles, `[[`, "index"),
  list(`tum-iztech-appended` = appended_review_bundle$index),
  list(`ucr-scaled-50000` = scaled_index)
)
scaled_filter_bundle <- bundles[["ucr-all-light-variables"]]
scaled_filter_bundle$index <- scaled_index
filter_bundles <- c(
  bundles,
  list(`tum-iztech-appended` = appended_review_bundle),
  list(`ucr-scaled-50000` = scaled_filter_bundle)
)
iterations <- c(
  stats::setNames(rep(200L, length(bundles)), names(bundles)),
  `tum-iztech-appended` = 200L,
  `ucr-scaled-50000` = 120L
)
timings <- lapply(names(cases), function(case_id) {
  section_index <- metadata_section_index(cases[[case_id]])
  selected_group_id <- cases[[case_id]]$groups$group_id[[
    floor(nrow(cases[[case_id]]$groups) / 2L)
  ]]
  hierarchy_operation <- function() {
    group <- metadata_index_group(cases[[case_id]], selected_group_id)
    neighborhood <- metadata_index_group_neighborhood(
      cases[[case_id]],
      selected_group_id
    )
    context <- metadata_hierarchy_node_context(
      cases[[case_id]],
      neighborhood$representative_node_id
    )
    roots <- metadata_hierarchy_logical_roots(cases[[case_id]], group)
    metadata_hierarchy_visible_path(context, roots)
  }
  hierarchy_panel_operation <- function() {
    group <- metadata_index_group(cases[[case_id]], selected_group_id)
    neighborhood <- metadata_index_group_neighborhood(
      cases[[case_id]],
      group
    )
    context <- metadata_hierarchy_node_context(
      cases[[case_id]],
      neighborhood$representative_node_id
    )
    roots <- metadata_hierarchy_logical_roots(cases[[case_id]], group)
    htmltools::renderTags(metadata_hierarchy_panel_ui(
      group,
      context,
      roots
    ))$html
  }
  hierarchy_allocated_bytes <- allocated_bytes(hierarchy_operation)
  hierarchy_panel_allocated_bytes <- allocated_bytes(
    hierarchy_panel_operation
  )
  hierarchy_dialog_allocated_bytes <- allocated_bytes(function() {
    group <- metadata_index_group(cases[[case_id]], selected_group_id)
    neighborhood <- metadata_index_group_neighborhood(cases[[case_id]], group)
    context <- metadata_hierarchy_node_context(
      cases[[case_id]],
      neighborhood$representative_node_id
    )
    htmltools::renderTags(metadata_hierarchy_context_ui(
      context,
      metadata_hierarchy_logical_roots(cases[[case_id]], group),
      list(root = "root", path = "path", child = "child"),
      group$group$group_id[[1L]]
    ))$html
  })
  focus_contents_allocated_bytes <- allocated_bytes(function() {
    neighborhood <- metadata_index_group_neighborhood(
      cases[[case_id]],
      selected_group_id
    )
    render_focus_contents(
      cases[[case_id]],
      neighborhood$representative_node_id,
      selected_group_id
    )
  })
  widest_node_id <- widest_metadata_node_id(cases[[case_id]])
  widest_context <- metadata_hierarchy_node_context(
    cases[[case_id]],
    widest_node_id
  )
  widest_contents_allocated_bytes <- allocated_bytes(function() {
    render_focus_contents(cases[[case_id]], widest_node_id, selected_group_id)
  })
  filter_inventory <- metadata_filter_inventory(section_index)
  filter_choices_allocated_bytes <- allocated_bytes(function() {
    filter_choice_burst(
      filter_bundles[[case_id]],
      metadata_filter_inventory(section_index)
    )
  })
  list(
    case_id = case_id,
    entry_count = nrow(cases[[case_id]]$entries),
    group_count = nrow(cases[[case_id]]$groups),
    node_count = nrow(cases[[case_id]]$nodes),
    index_bytes = as.numeric(object.size(cases[[case_id]])),
    section_index_bytes = as.numeric(object.size(section_index)),
    filter_inventory_bytes = as.numeric(object.size(filter_inventory)),
    section_index_memory_pass = as.numeric(object.size(section_index)) +
      as.numeric(object.size(filter_inventory)) <=
      budgets$section_index_max_bytes,
    filter_choices_allocated_bytes = filter_choices_allocated_bytes,
    filter_choices_allocation_pass = filter_choices_allocated_bytes <=
      budgets$filter_choices_max_alloc_bytes,
    hierarchy_allocated_bytes = hierarchy_allocated_bytes,
    hierarchy_allocation_pass = hierarchy_allocated_bytes <=
      budgets$hierarchy_navigation_max_alloc_bytes,
    hierarchy_panel_allocated_bytes = hierarchy_panel_allocated_bytes,
    hierarchy_panel_allocation_pass = hierarchy_panel_allocated_bytes <=
      budgets$hierarchy_panel_render_max_alloc_bytes,
    hierarchy_dialog_allocated_bytes = hierarchy_dialog_allocated_bytes,
    hierarchy_dialog_allocation_pass = hierarchy_dialog_allocated_bytes <=
      budgets$hierarchy_dialog_render_max_alloc_bytes,
    focus_contents_allocated_bytes = focus_contents_allocated_bytes,
    focus_contents_allocation_pass = focus_contents_allocated_bytes <=
      budgets$focus_contents_render_max_alloc_bytes,
    widest_contents = list(
      node_id = widest_node_id,
      label = widest_context$node$label[[1L]],
      exact_child_count = widest_context$node$child_node_count[[1L]],
      visible_child_count = nrow(widest_context$children),
      field_count = nrow(widest_context$groups)
    ),
    widest_contents_allocated_bytes = widest_contents_allocated_bytes,
    widest_contents_allocation_pass = widest_contents_allocated_bytes <=
      budgets$focus_contents_render_max_alloc_bytes,
    operations = benchmark_index(
      cases[[case_id]],
      filter_bundles[[case_id]],
      iterations[[case_id]]
    )
  )
})

all_operations <- unlist(
  lapply(timings, function(case) case$operations),
  recursive = FALSE
)
memory <- list(
  bundles = bundle_memory,
  combined_bundle_bytes = sum(vapply(
    bundles,
    function(bundle) as.numeric(object.size(bundle)),
    numeric(1)
  )),
  combined_bundle_pass = sum(vapply(
    bundles,
    function(bundle) as.numeric(object.size(bundle)),
    numeric(1)
  )) <=
    budgets$three_fixture_bundles_max_bytes,
  appended_review_bundle_bytes = as.numeric(object.size(
    appended_review_bundle
  )),
  appended_review_bundle_pass = as.numeric(object.size(
    appended_review_bundle
  )) <=
    budgets$retained_bundle_max_bytes,
  scaled_index_bytes = as.numeric(object.size(scaled_index)),
  scaled_index_pass = as.numeric(object.size(scaled_index)) <=
    budgets$scaled_index_max_bytes
)

review_construction_samples <- timing_samples_ms(
  function() {
    metadata_explorer_fixture_records(
      fixture_directory,
      include_absent = TRUE
    )
  },
  iterations = 20L,
  warmup = 2L
)
review_selection_samples <- timing_samples_ms(
  function() review_records[["dataset_m7appendedreview"]]$metadata_bundle,
  iterations = 200L,
  warmup = 20L
)
review_data <- dataset_raw_data(appended_review)
expected_iztech_file_groups <- c(
  "MELIDOS_IZTECH_S001:18",
  "MELIDOS_IZTECH_S002:18",
  "MELIDOS_IZTECH_S003:18",
  "MELIDOS_IZTECH_S004:19",
  "MELIDOS_IZTECH_S004:20",
  "MELIDOS_IZTECH_S005:18"
)
device_rows <- metadata_index_query_groups(
  appended_review_bundle$index,
  "dataset_file_crossref_device_id",
  limit = 100L
)$rows
device_row <- device_rows[
  grepl("IZTECH", device_rows$source_label, fixed = TRUE),
  ,
  drop = FALSE
]
device_entries <- metadata_index_group(
  appended_review_bundle$index,
  device_row$group_id[[1L]]
)$entries
variable_rows <- metadata_index_query_groups(
  appended_review_bundle$index,
  "dataset_file_variables_name",
  limit = 100L
)$rows
medi_row <- variable_rows[
  grepl("IZTECH", variable_rows$source_label, fixed = TRUE) &
    variable_rows$group_context == "MEDI",
  ,
  drop = FALSE
]
medi_entries <- metadata_index_group(
  appended_review_bundle$index,
  medi_row$group_id[[1L]]
)$entries
file_group_identity <- list(
  expected_records = expected_iztech_file_groups,
  device_records = as.character(device_entries$record),
  device_ids = as.character(device_entries$value),
  medi_records = as.character(medi_entries$record),
  pass = identical(
    as.character(device_entries$record),
    expected_iztech_file_groups
  ) &&
    identical(
      as.character(device_entries$value[4:5]),
      c("IZTECH_AL03", "IZTECH_AL04")
    ) &&
    identical(
      as.character(medi_entries$record),
      expected_iztech_file_groups
    )
)
review_route <- list(
  record_count = length(review_records),
  appended_source_count = length(appended_review_bundle$sources),
  appended_overlay_revision = appended_review_bundle$overlay$revision,
  appended_lineage_rows = nrow(review_data),
  appended_lineage_ids = sort(unique(
    as.character(review_data$llw_source_dataset_id)
  )),
  appended_file_group_identity = file_group_identity,
  appended_bundle_bytes = as.numeric(object.size(appended_review_bundle)),
  construction = list(
    iterations = 20L,
    p50_ms = unname(stats::quantile(
      review_construction_samples,
      0.5,
      type = 8
    )),
    p95_ms = unname(stats::quantile(
      review_construction_samples,
      0.95,
      type = 8
    )),
    max_ms = max(review_construction_samples),
    interaction_gate_not_applicable = TRUE,
    reason = "Development showcase startup, not a user-local interaction"
  ),
  selection = list(
    iterations = 200L,
    p50_ms = unname(stats::quantile(
      review_selection_samples,
      0.5,
      type = 8
    )),
    p95_ms = unname(stats::quantile(
      review_selection_samples,
      0.95,
      type = 8
    )),
    max_ms = max(review_selection_samples),
    design_target_pass = unname(stats::quantile(
      review_selection_samples,
      0.95,
      type = 8
    )) <=
      budgets$interaction_design_target_p95_ms,
    hard_gate_pass = unname(stats::quantile(
      review_selection_samples,
      0.95,
      type = 8
    )) <=
      budgets$interaction_hard_gate_p95_ms
  ),
  package_reopen_count = 0L,
  record_deserialization_count_after_startup = 0L
)

required_section_choices <- c(
  "All sections" = "all",
  "Core Metadata" = "core_metadata",
  "Dataset Inventory" = "dataset_inventory",
  "Variable Inventory" = "variable_inventory"
)
fixture_section_choices <- lapply(bundles, function(bundle) {
  metadata_section_choices(metadata_section_index(bundle$index))
})
ucr_section_index <- metadata_section_index(
  bundles[["ucr-all-light-variables"]]$index
)
ucr_dataset_filter <- metadata_index_query_groups_by_section(
  bundles[["ucr-all-light-variables"]]$index,
  section_index = ucr_section_index,
  section_id = "dataset_inventory",
  limit = 40L
)

iztech_index <- bundles[["iztech-light-subset"]]$index
iztech_participant_rows <- metadata_index_query_groups(
  iztech_index,
  "IZTECH_S004",
  limit = nrow(iztech_index$groups)
)$rows
iztech_participant_row <- iztech_participant_rows[
  iztech_participant_rows$field == "participant_internal_id",
  ,
  drop = FALSE
][1L, , drop = FALSE]
iztech_participant_group <- metadata_index_group(
  iztech_index,
  iztech_participant_row$group_id[[1L]]
)
iztech_roots <- metadata_hierarchy_logical_roots(
  iztech_index,
  iztech_participant_group
)
iztech_dataset_root <- metadata_hierarchy_node_context(
  iztech_index,
  iztech_roots$node_id[iztech_roots$label == "dataset_inventory"][[1L]]
)
iztech_s004 <- metadata_hierarchy_node_context(
  iztech_index,
  iztech_dataset_root$children$node_id[
    iztech_dataset_root$children$label == "MELIDOS_IZTECH_S004"
  ][[1L]]
)
iztech_s004_comparison <- metadata_hierarchy_context_neighborhood(iztech_s004)

ucr_index <- bundles[["ucr-all-light-variables"]]$index
ucr_medication_rows <- metadata_index_query_groups(
  ucr_index,
  "Medication",
  limit = nrow(ucr_index$groups)
)$rows
ucr_medication_row <- ucr_medication_rows[
  ucr_medication_rows$field == "participant_characteristic_name" &
    ucr_medication_rows$group_context == "medication",
  ,
  drop = FALSE
][1L, , drop = FALSE]
ucr_medication_group <- metadata_index_group(
  ucr_index,
  ucr_medication_row$group_id[[1L]]
)
ucr_roots <- metadata_hierarchy_logical_roots(
  ucr_index,
  ucr_medication_group
)
ucr_variable_root <- metadata_hierarchy_node_context(
  ucr_index,
  ucr_roots$node_id[ucr_roots$label == "variable_inventory"][[1L]]
)
ucr_medi <- metadata_hierarchy_node_context(
  ucr_index,
  ucr_variable_root$children$node_id[
    ucr_variable_root$children$label == "melanopic_edi"
  ][[1L]]
)
ucr_medi_comparison <- metadata_hierarchy_context_neighborhood(ucr_medi)

logical_section_pass <- all(vapply(
  fixture_section_choices,
  function(choices) {
    identical(
      choices[seq_along(required_section_choices)],
      required_section_choices
    )
  },
  logical(1)
)) &&
  identical(ucr_dataset_filter$total, 13L) &&
  identical(nrow(ucr_dataset_filter$rows), 13L) &&
  !ucr_dataset_filter$limited &&
  all(
    ucr_section_index$section_id[
      match(ucr_dataset_filter$rows$group_id, ucr_section_index$group_id)
    ] ==
      "dataset_inventory"
  )
focus_comparison_pass <-
  nrow(
    metadata_hierarchy_context_neighborhood(
      iztech_dataset_root
    )$property_groups
  ) ==
    0L &&
  all(
    c("dataset_id", "device_ids") %in%
      iztech_s004_comparison$property_groups$field
  ) &&
  !any(
    c(
      "participant_age",
      "participant_sex",
      "participant_gender"
    ) %in%
      iztech_s004_comparison$property_groups$field
  ) &&
  identical(
    iztech_s004_comparison$property_groups$entry_count[
      iztech_s004_comparison$property_groups$field == "device_ids"
    ],
    6L
  ) &&
  all(
    c("name", "label", "unit") %in%
      ucr_medi_comparison$property_groups$field
  ) &&
  !any(grepl(
    "participant_characteristic",
    ucr_medi_comparison$property_groups$field,
    fixed = TRUE
  )) &&
  identical(
    ucr_medi_comparison$property_groups$value_preview[
      ucr_medi_comparison$property_groups$field == "unit"
    ],
    "lx"
  )
navigation_correction_contract <- list(
  fixture_section_choices = fixture_section_choices,
  ucr_dataset_inventory_filter = list(
    total = ucr_dataset_filter$total,
    shown = nrow(ucr_dataset_filter$rows),
    limited = ucr_dataset_filter$limited,
    filter_before_cap_pass = logical_section_pass
  ),
  iztech_s004_focus = list(
    root_field_count = nrow(
      metadata_hierarchy_context_neighborhood(
        iztech_dataset_root
      )$property_groups
    ),
    comparison_fields = iztech_s004_comparison$property_groups$field,
    device_ids_entry_count = iztech_s004_comparison$property_groups$entry_count[
      iztech_s004_comparison$property_groups$field == "device_ids"
    ]
  ),
  ucr_melanopic_edi_focus = list(
    comparison_fields = ucr_medi_comparison$property_groups$field,
    unit = ucr_medi_comparison$property_groups$value_preview[
      ucr_medi_comparison$property_groups$field == "unit"
    ]
  ),
  logical_section_pass = logical_section_pass,
  focus_comparison_pass = focus_comparison_pass
)

fixture_records <- lapply(fixtures, function(fixture) {
  review_records[[metadata_fixture_dataset_record_id(fixture$fixture_id)]]
})
names(fixture_records) <- names(fixtures)

benchmark_repeated_focus_delivery <- function(
  request,
  alternate_focus,
  iterations = 120L,
  warmup = 20L
) {
  request <- validate_metadata_focus_request(request)
  capture <- new.env(parent = emptyenv())

  shiny::testServer(
    function(input, output, session) {
      focus_value <- shiny::reactiveVal(NULL)
      hierarchy_focus <- shiny::reactiveVal(NULL)
      activation_ids <- shiny::reactiveVal(character())

      shiny::observeEvent(
        focus_value(),
        {
          current <- focus_value()
          activation_ids(c(activation_ids(), current$request_id))
          hierarchy_focus(current$record_identity)
        },
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

      session$userData$deliver <- function() {
        deliver_metadata_focus_request(focus_value, request)
        session$flushReact()
      }
      session$userData$hierarchy_focus <- hierarchy_focus
      session$userData$activation_ids <- activation_ids
    },
    {
      session$flushReact()
      session$userData$deliver()
      session$userData$hierarchy_focus(alternate_focus)
      session$userData$deliver()
      first_two_ids <- shiny::isolate(session$userData$activation_ids())
      repeated_focus <- shiny::isolate(session$userData$hierarchy_focus())
      timing <- timing_evidence(
        session$userData$deliver,
        iterations = iterations,
        warmup = warmup
      )
      capture$result <- c(
        list(
          operation = "repeated_crosslink_redelivery",
          record_identity = request$record_identity,
          request_id = request$request_id,
          repeated_delivery_count = length(first_two_ids),
          request_id_stable = identical(
            first_two_ids,
            rep(request$request_id, 2L)
          ),
          reapplied_focus = repeated_focus,
          reapplied_focus_pass = identical(
            repeated_focus,
            request$record_identity
          )
        ),
        timing
      )
    }
  )

  capture$result
}

iztech_record <- fixture_records[["iztech-light-subset"]]
repeated_crosslink_contract <- list(
  participant = benchmark_repeated_focus_delivery(
    dashboard_metadata_identity_request(
      iztech_record,
      "participant",
      "IZTECH_S004"
    ),
    alternate_focus = "MELIDOS_IZTECH_S004:20"
  ),
  device = benchmark_repeated_focus_delivery(
    dashboard_metadata_identity_request(
      iztech_record,
      "device",
      "IZTECH_AL03"
    ),
    alternate_focus = "MELIDOS_IZTECH_S004:20"
  )
)
repeated_crosslink_contract$pass <- all(vapply(
  repeated_crosslink_contract,
  function(case) {
    identical(case$repeated_delivery_count, 2L) &&
      isTRUE(case$request_id_stable) &&
      isTRUE(case$reapplied_focus_pass) &&
      isTRUE(case$hard_gate_pass)
  },
  logical(1)
))

benchmark_relationship_integration <- function(fixture_id, record) {
  contexts <- metadata_relationships_for_record(record)
  if (length(contexts) != 1L) {
    stop(
      "Fixture does not retain exactly one relationship context: ",
      fixture_id
    )
  }
  context <- contexts[[1L]]
  namespace_id <- names(contexts)[[1L]]
  variable <- if ("MEDI" %in% names(dataset_prepared_data(record))) {
    "MEDI"
  } else {
    record$analysis_settings$primary_variable
  }
  variable_resolution <- metadata_resolve_variable(record, variable)
  if (is.null(variable_resolution$target)) {
    stop("Fixture variable has no exact metadata target: ", fixture_id)
  }
  if (
    !identical(
      metadata_resolve_focus_request(
        record$metadata_bundle,
        variable_resolution$target
      )$state,
      "resolved"
    )
  ) {
    stop("Fixture variable crosslink does not resolve exactly: ", fixture_id)
  }
  dataset_id <- as.character(context$datasets$Id[[1L]])
  dataset_request <- dashboard_metadata_identity_request(
    record,
    "dataset",
    dataset_id,
    namespace_id,
    paste("Dataset", dataset_id)
  )
  if (is.null(dataset_request)) {
    stop("Fixture dataset crosslink does not resolve exactly: ", fixture_id)
  }
  data <- dataset_prepared_data(record)
  file_group_id <- as.character(context$file_groups$file_group_id[[1L]])
  operations <- list(
    label_unit_resolution = function() {
      metadata_resolve_variable(record, variable)
    },
    exact_variable_crosslink = function() {
      metadata_resolve_focus_request(
        record$metadata_bundle,
        variable_resolution$target
      )
    },
    exact_dataset_crosslink = function() {
      metadata_resolve_focus_request(record$metadata_bundle, dataset_request)
    },
    relationship_selection = function() {
      metadata_relationship_subset(context, file_group_id, dataset_id)
    },
    relationship_context_table = function() {
      dashboard_relationship_view(record, data)
    }
  )
  evidence <- lapply(names(operations), function(operation) {
    c(
      list(operation = operation),
      timing_evidence(operations[[operation]], iterations = 120L)
    )
  })
  list(
    case_id = fixture_id,
    variable = variable,
    relationship_file_group_rows = nrow(context$file_groups),
    relationship_dataset_rows = nrow(context$datasets),
    operations = evidence
  )
}

integration_timing <- lapply(names(fixture_records), function(fixture_id) {
  benchmark_relationship_integration(
    fixture_id,
    fixture_records[[fixture_id]]
  )
})

appended_resolution <- metadata_resolve_variable(
  appended_review,
  appended_review$analysis_settings$primary_variable
)
appended_snapshot <- dashboard_dataset_snapshot(appended_review)
appended_tum_entities <- dashboard_relationship_entities(
  appended_snapshot$relationship_view,
  "TUM_TUM_S001"
)
appended_iztech_entities <- dashboard_relationship_entities(
  appended_snapshot$relationship_view,
  "IZTECH_IZTECH_S004"
)
appended_device_requests <- lapply(
  appended_iztech_entities$device_ids,
  function(device_id) {
    dashboard_metadata_identity_request(
      appended_review,
      "device",
      device_id,
      appended_iztech_entities$namespace_ids,
      paste("Device", device_id)
    )
  }
)
if (any(vapply(appended_device_requests, is.null, logical(1)))) {
  stop("An appended device metadata action does not resolve exactly.")
}
appended_device_action_ids <- vapply(
  appended_device_requests,
  dashboard_device_metadata_input_id,
  character(1)
)
appended_integration_operations <- list(
  appended_relationship_selection = function() {
    dashboard_relationship_entities(
      appended_snapshot$relationship_view,
      "IZTECH_IZTECH_S004"
    )
  },
  appended_mapped_focus_view = function() {
    dashboard_focus_view(appended_snapshot, "tum_MEDI")
  },
  appended_device_action_identity = function() {
    vapply(
      appended_device_requests,
      dashboard_device_metadata_input_id,
      character(1)
    )
  }
)
appended_integration <- list(
  case_id = "tum-iztech-appended",
  focus_variables = appended_snapshot$focus_variables,
  relationship_state = appended_snapshot$relationship_view$state,
  relationship_rows = nrow(appended_snapshot$relationship_view$data),
  tum_namespace_ids = appended_tum_entities$namespace_ids,
  tum_participant_ids = appended_tum_entities$participant_ids,
  iztech_namespace_ids = appended_iztech_entities$namespace_ids,
  iztech_participant_ids = appended_iztech_entities$participant_ids,
  iztech_device_ids = appended_iztech_entities$device_ids,
  device_action_ids = appended_device_action_ids,
  device_action_ids_unique = !anyDuplicated(appended_device_action_ids),
  derived_relationship_index_entries = sum(grepl(
    "/relationship_context",
    appended_review_bundle$index$entries$path,
    fixed = TRUE
  )),
  operations = lapply(names(appended_integration_operations), function(name) {
    c(
      list(operation = name),
      timing_evidence(
        appended_integration_operations[[name]],
        iterations = 120L
      )
    )
  })
)

entity_record <- fixture_records[["iztech-light-subset"]]
entity_snapshot <- dashboard_dataset_snapshot(entity_record)
entity_dataset_ids <- unique(as.character(
  dataset_prepared_data(entity_record)$Id
))
entity_refs <- list(
  participants = dashboard_relationship_entity_refs(
    entity_snapshot$relationship_view,
    entity_dataset_ids,
    "participant"
  ),
  devices = dashboard_relationship_entity_refs(
    entity_snapshot$relationship_view,
    entity_dataset_ids,
    "device"
  )
)
metadata_action_items <- function(record, refs, entity) {
  lapply(seq_len(nrow(refs)), function(index) {
    identity <- refs$identity[[index]]
    label <- paste(
      if (identical(entity, "participant")) "Participant" else "Device",
      identity
    )
    list(
      label = label,
      title = label,
      request = dashboard_metadata_identity_request(
        record,
        entity,
        identity,
        refs$namespace_id[[index]],
        label
      )
    )
  })
}
participant_action_items <- metadata_action_items(
  entity_record,
  entity_refs$participants,
  "participant"
)
device_action_items <- metadata_action_items(
  entity_record,
  entity_refs$devices,
  "device"
)
if (
  any(vapply(
    c(participant_action_items, device_action_items),
    function(item) is.null(item$request),
    logical(1)
  ))
) {
  stop("A bounded entity metadata action does not resolve exactly.")
}
participant_action_ids <- vapply(
  participant_action_items,
  function(item) dashboard_participant_metadata_input_id(item$request),
  character(1)
)
device_action_ids <- vapply(
  device_action_items,
  function(item) dashboard_device_metadata_input_id(item$request),
  character(1)
)
participant_action_html <- htmltools::renderTags(
  dashboard_metadata_entity_group_ui(
    utils::head(participant_action_items, dashboard_metadata_entity_limit()),
    "participant"
  )
)$html
participant_reduction_html <- htmltools::renderTags(
  dashboard_metadata_entity_group_ui(
    participant_action_items,
    "participant"
  )
)$html
device_action_html <- htmltools::renderTags(
  dashboard_metadata_entity_group_ui(device_action_items, "device")
)$html
entity_action_operations <- list(
  entity_reference_resolution = function() {
    list(
      participants = dashboard_relationship_entity_refs(
        entity_snapshot$relationship_view,
        entity_dataset_ids,
        "participant"
      ),
      devices = dashboard_relationship_entity_refs(
        entity_snapshot$relationship_view,
        entity_dataset_ids,
        "device"
      )
    )
  },
  four_participant_actions_render = function() {
    htmltools::renderTags(dashboard_metadata_entity_group_ui(
      utils::head(
        participant_action_items,
        dashboard_metadata_entity_limit()
      ),
      "participant"
    ))$html
  },
  five_participant_reduction_render = function() {
    htmltools::renderTags(dashboard_metadata_entity_group_ui(
      participant_action_items,
      "participant"
    ))$html
  },
  four_device_actions_render = function() {
    htmltools::renderTags(dashboard_metadata_entity_group_ui(
      device_action_items,
      "device"
    ))$html
  }
)
entity_action_contract <- list(
  case_id = "iztech-five-participant-four-device-actions",
  entity_limit = dashboard_metadata_entity_limit(),
  participant_ids = entity_refs$participants$identity,
  participant_action_ids = participant_action_ids,
  participant_reduction_shown = grepl(
    "5 participants in this context",
    participant_reduction_html,
    fixed = TRUE
  ),
  participant_actions_hidden_above_limit = !any(vapply(
    participant_action_ids,
    grepl,
    logical(1),
    x = participant_reduction_html,
    fixed = TRUE
  )),
  first_four_participant_actions_shown = all(vapply(
    utils::head(participant_action_ids, dashboard_metadata_entity_limit()),
    grepl,
    logical(1),
    x = participant_action_html,
    fixed = TRUE
  )),
  device_ids = entity_refs$devices$identity,
  device_action_ids = device_action_ids,
  all_four_device_actions_shown = all(vapply(
    device_action_ids,
    grepl,
    logical(1),
    x = device_action_html,
    fixed = TRUE
  )),
  action_ids_unique = !anyDuplicated(c(
    participant_action_ids,
    device_action_ids
  )),
  operations = lapply(names(entity_action_operations), function(name) {
    c(
      list(operation = name),
      timing_evidence(entity_action_operations[[name]], iterations = 120L)
    )
  })
)
absent_record <- review_records[["dataset_m7absent"]]
fallback_timing <- list(
  list(
    case_id = "tum-iztech-appended",
    variable = appended_review$analysis_settings$primary_variable,
    target_state = if (is.null(appended_resolution$target)) {
      "unavailable"
    } else {
      metadata_resolve_focus_request(
        appended_review$metadata_bundle,
        appended_resolution$target
      )$state
    },
    operation = c(
      list(operation = "appended_label_unit_resolution"),
      timing_evidence(function() {
        metadata_resolve_variable(
          appended_review,
          appended_review$analysis_settings$primary_variable
        )
      })
    )
  ),
  list(
    case_id = "metadata-absent",
    variable = "MEDI",
    label = metadata_resolve_variable(absent_record, "MEDI")$display_label,
    unit = metadata_resolve_variable(absent_record, "MEDI")$unit,
    operation = c(
      list(operation = "raw_name_unit_fallback"),
      timing_evidence(function() {
        metadata_resolve_variable(absent_record, "MEDI")
      })
    )
  )
)

wide_context <- metadata_relationships_for_record(
  fixture_records[["ucr-all-light-variables"]]
)[[1L]]
wide_key_rows <- wide_context$file_groups[,
  c("dataset_internal_id", "file_group_id"),
  drop = FALSE
]
wide_key_index <- rep(
  seq_len(nrow(wide_key_rows)),
  length.out = budgets$relationship_add_min_rows
)
relationship_add_input <- data.frame(
  Id = as.character(wide_key_rows$dataset_internal_id[wide_key_index]),
  file_group_id = as.character(wide_key_rows$file_group_id[wide_key_index]),
  llw_benchmark_row = seq_len(budgets$relationship_add_min_rows),
  stringsAsFactors = FALSE
) |>
  dplyr::group_by(Id, .drop = FALSE)
relationship_add_fields <- setdiff(
  wide_context$resolved_file_group_fields,
  names(relationship_add_input)
)
relationship_add_operation <- function() {
  enriched <- glcdp_api()$add_metadata(
    dataset = relationship_add_input,
    metadata = wide_context$file_groups,
    fields = relationship_add_fields,
    by = "file_group_id",
    resource = NULL,
    overwrite = FALSE
  )
  if (
    nrow(enriched) != nrow(relationship_add_input) ||
      !identical(enriched$Id, relationship_add_input$Id) ||
      !identical(
        enriched$file_group_id,
        relationship_add_input$file_group_id
      ) ||
      !identical(
        enriched$llw_benchmark_row,
        relationship_add_input$llw_benchmark_row
      ) ||
      !identical(
        dplyr::group_vars(enriched),
        dplyr::group_vars(relationship_add_input)
      ) ||
      !identical(
        attr(dplyr::group_data(enriched), ".drop"),
        attr(dplyr::group_data(relationship_add_input), ".drop")
      )
  ) {
    stop(
      "The 50,000-row relationship add changed identity, order, or grouping."
    )
  }
  enriched
}
relationship_add_timing <- timing_evidence(
  relationship_add_operation,
  iterations = 30L,
  warmup = 5L
)
relationship_add_allocated_bytes <- allocated_bytes(
  relationship_add_operation,
  warmup = 2L
)
relationship_add_50000 <- c(
  list(
    case_id = "ucr-derived-relationship-add-50000",
    rows = nrow(relationship_add_input),
    fields = relationship_add_fields,
    input_bytes = as.numeric(object.size(relationship_add_input)),
    allocated_bytes = relationship_add_allocated_bytes,
    allocation_pass = relationship_add_allocated_bytes <=
      budgets$relationship_add_max_alloc_bytes,
    disposable_derived_context = TRUE,
    canonical_fixture_data_unchanged = identical(
      metadata_fingerprint(fixtures[["ucr-all-light-variables"]]$data),
      metadata_fingerprint(dataset_raw_data(
        fixture_records[["ucr-all-light-variables"]]
      ))
    )
  ),
  relationship_add_timing
)

integration_operations <- c(
  unlist(lapply(integration_timing, `[[`, "operations"), recursive = FALSE),
  appended_integration$operations,
  entity_action_contract$operations,
  lapply(fallback_timing, `[[`, "operation"),
  repeated_crosslink_contract[c("participant", "device")],
  list(relationship_add_50000)
)
all_interaction_operations <- c(all_operations, integration_operations)
overlay_authoring <- benchmark_overlay_authoring(review_records, scaled_index)
final_integration <- benchmark_final_integration(review_records)

result <- list(
  report_schema = "llw-metadata-acceptance",
  report_version = "2.13.0",
  benchmarked_at = benchmarked_at,
  baseline = "b704d24ee6eab066163bb88e7125faf19e09543d",
  budgets = budgets,
  overlay_authoring = overlay_authoring,
  final_integration = final_integration,
  environment = list(
    r_version = R.version.string,
    platform = R.version$platform,
    LightLogWeb_version = as.character(utils::packageVersion("LightLogWeb")),
    glcdp_version = as.character(utils::packageVersion("glcdp")),
    glcdp_revision = utils::packageDescription("glcdp")$RemoteSha
  ),
  fixture_manifest_generated_at = manifest$generated_at,
  timing = timings,
  integration_timing = integration_timing,
  appended_integration = appended_integration,
  entity_action_contract = entity_action_contract,
  fallback_timing = fallback_timing,
  relationship_add_50000 = relationship_add_50000,
  memory = memory,
  review_route = review_route,
  navigation_correction_contract = navigation_correction_contract,
  repeated_crosslink_contract = repeated_crosslink_contract,
  gates = list(
    design_target_pass = all(vapply(
      all_interaction_operations,
      `[[`,
      logical(1),
      "design_target_pass"
    )) &&
      review_route$selection$design_target_pass &&
      all(vapply(
        final_integration$cases,
        function(case) {
          case$table_view_timing$design_target_pass &&
            case$quick_open_timing$design_target_pass &&
            all(vapply(case$operations, `[[`, logical(1), "design_target_pass"))
        },
        logical(1)
      )),
    hard_timing_gate_pass = all(vapply(
      all_interaction_operations,
      `[[`,
      logical(1),
      "hard_gate_pass"
    )) &&
      review_route$selection$hard_gate_pass &&
      overlay_authoring$gates$hard_timing_pass &&
      final_integration$gates$hard_timing_pass &&
      final_integration$gates$quick_open_timing_pass &&
      all(vapply(
        final_integration$cases,
        function(case) {
          case$table_view_timing$hard_gate_pass
        },
        logical(1)
      )),
    memory_gate_pass = all(vapply(
      bundle_memory,
      `[[`,
      logical(1),
      "pass"
    )) &&
      memory$combined_bundle_pass &&
      memory$appended_review_bundle_pass &&
      memory$scaled_index_pass &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "filter_choices_allocation_pass"
      )) &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "hierarchy_allocation_pass"
      )) &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "hierarchy_panel_allocation_pass"
      )) &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "hierarchy_dialog_allocation_pass"
      )) &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "focus_contents_allocation_pass"
      )) &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "widest_contents_allocation_pass"
      )) &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "section_index_memory_pass"
      )) &&
      review_route$appended_bundle_bytes <= budgets$retained_bundle_max_bytes &&
      relationship_add_50000$allocation_pass &&
      overlay_authoring$gates$allocation_pass &&
      final_integration$gates$allocation_pass &&
      final_integration$gates$quick_open_allocation_pass &&
      all(vapply(
        final_integration$cases,
        function(case) {
          case$table_view_allocated_bytes <=
            budgets$table_view_restore_max_alloc_bytes
        },
        logical(1)
      )),
    final_integration_invariants_pass = final_integration$gates$invariants_pass &&
      final_integration$gates$size_bounds_pass &&
      final_integration$gates$cache_bounds_pass,
    navigation_correction_pass = navigation_correction_contract$logical_section_pass &&
      navigation_correction_contract$focus_comparison_pass,
    repeated_crosslink_route_pass = isTRUE(
      repeated_crosslink_contract$pass
    ),
    relationship_contract_pass = all(vapply(
      integration_timing,
      function(case) {
        case$relationship_file_group_rows > 0L &&
          case$relationship_dataset_rows > 0L
      },
      logical(1)
    )) &&
      !is.null(appended_resolution$target) &&
      identical(appended_snapshot$relationship_view$state, "ready") &&
      all(
        c(
          "tum_MEDI",
          "iztech_MEDI",
          "iztech_LIGHT"
        ) %in%
          appended_snapshot$focus_variables
      ) &&
      identical(
        appended_tum_entities$namespace_ids,
        "dataset_m7tummedionly"
      ) &&
      identical(appended_tum_entities$participant_ids, "TUM_S001") &&
      identical(
        appended_iztech_entities$namespace_ids,
        "dataset_m7iztechlightsubset"
      ) &&
      identical(appended_iztech_entities$participant_ids, "IZTECH_S004") &&
      identical(
        appended_iztech_entities$device_ids,
        c("IZTECH_AL03", "IZTECH_AL04")
      ) &&
      isTRUE(appended_integration$device_action_ids_unique) &&
      identical(
        appended_integration$device_action_ids,
        vapply(
          appended_device_requests,
          dashboard_device_metadata_input_id,
          character(1)
        )
      ) &&
      identical(entity_action_contract$entity_limit, 4L) &&
      identical(
        entity_action_contract$participant_ids,
        paste0("IZTECH_S00", seq_len(5L))
      ) &&
      identical(
        entity_action_contract$device_ids,
        c(
          "IZTECH_AL03",
          "IZTECH_AL05_FW1_7",
          "IZTECH_AL04",
          "IZTECH_AL06_FW1_7"
        )
      ) &&
      isTRUE(entity_action_contract$participant_reduction_shown) &&
      isTRUE(
        entity_action_contract$participant_actions_hidden_above_limit
      ) &&
      isTRUE(entity_action_contract$first_four_participant_actions_shown) &&
      isTRUE(entity_action_contract$all_four_device_actions_shown) &&
      isTRUE(entity_action_contract$action_ids_unique) &&
      identical(appended_integration$derived_relationship_index_entries, 0L) &&
      identical(fallback_timing[[2L]]$label, "MEDI") &&
      identical(fallback_timing[[2L]]$unit, "unit not specified") &&
      isTRUE(relationship_add_50000$canonical_fixture_data_unchanged),
    appended_file_group_identity_pass = isTRUE(file_group_identity$pass),
    package_reopen_count = 0L,
    record_deserialization_count = 0L
  )
)

output_path <- file.path("dev", "milestone-7-metadata-benchmark.json")
jsonlite::write_json(
  result,
  output_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = 6,
  null = "null"
)

if (!result$gates$hard_timing_gate_pass) {
  stop("At least one metadata or overlay operation exceeded its p95 gate.")
}
if (!result$gates$final_integration_invariants_pass) {
  stop(
    "An integrated metadata edit decoded measurements or invalidated a numeric cache."
  )
}
if (!result$gates$memory_gate_pass) {
  stop("At least one metadata memory budget failed.")
}
if (!result$gates$relationship_contract_pass) {
  stop("At least one metadata relationship or fallback contract failed.")
}
if (!result$gates$appended_file_group_identity_pass) {
  stop("Appended source metadata lost an exact file-group identity.")
}
if (!result$gates$navigation_correction_pass) {
  stop("A metadata navigation correction contract failed.")
}
if (!result$gates$repeated_crosslink_route_pass) {
  stop("A repeated metadata crosslink did not reapply its complete focus.")
}

cat("Milestone 7 metadata acceptance passed.\n")
cat("Evidence:", output_path, "\n")
for (case in result$timing) {
  cat(case$case_id, case$entry_count, "entries\n")
  for (operation in case$operations) {
    cat(
      " ",
      operation$operation,
      "p50",
      format(operation$p50_ms, digits = 4),
      "ms p95",
      format(operation$p95_ms, digits = 4),
      "ms max",
      format(operation$max_ms, digits = 4),
      "ms\n"
    )
  }
  cat(
    "  hierarchy allocation",
    format(case$hierarchy_allocated_bytes, big.mark = ","),
    "bytes\n"
  )
  cat(
    "  hierarchy panel allocation",
    format(case$hierarchy_panel_allocated_bytes, big.mark = ","),
    "bytes\n"
  )
}
for (case in result$integration_timing) {
  cat(case$case_id, case$relationship_file_group_rows, "relationship rows\n")
  for (operation in case$operations) {
    cat(
      " ",
      operation$operation,
      "p50",
      format(operation$p50_ms, digits = 4),
      "ms p95",
      format(operation$p95_ms, digits = 4),
      "ms max",
      format(operation$max_ms, digits = 4),
      "ms\n"
    )
  }
}
cat(
  result$appended_integration$case_id,
  result$appended_integration$relationship_rows,
  "lineage-scoped relationship rows\n"
)
for (operation in result$appended_integration$operations) {
  cat(
    " ",
    operation$operation,
    "p50",
    format(operation$p50_ms, digits = 4),
    "ms p95",
    format(operation$p95_ms, digits = 4),
    "ms max",
    format(operation$max_ms, digits = 4),
    "ms\n"
  )
}
cat(
  result$entity_action_contract$case_id,
  length(result$entity_action_contract$participant_ids),
  "participants and",
  length(result$entity_action_contract$device_ids),
  "devices\n"
)
for (operation in result$entity_action_contract$operations) {
  cat(
    " ",
    operation$operation,
    "p50",
    format(operation$p50_ms, digits = 4),
    "ms p95",
    format(operation$p95_ms, digits = 4),
    "ms max",
    format(operation$max_ms, digits = 4),
    "ms\n"
  )
}
cat(
  result$relationship_add_50000$case_id,
  "p95",
  format(result$relationship_add_50000$p95_ms, digits = 4),
  "ms allocation",
  format(result$relationship_add_50000$allocated_bytes, big.mark = ","),
  "bytes\n"
)
cat("Combined fixture bundle bytes:", memory$combined_bundle_bytes, "\n")
cat("Scaled index bytes:", memory$scaled_index_bytes, "\n")
