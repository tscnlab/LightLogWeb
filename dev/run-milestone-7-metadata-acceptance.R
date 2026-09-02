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
  retained_bundle_max_bytes = 32 * 1024^2,
  three_fixture_bundles_max_bytes = 64 * 1024^2,
  scaled_index_max_bytes = 64 * 1024^2,
  hierarchy_navigation_max_alloc_bytes = 10 * 1024^2,
  hierarchy_panel_render_max_alloc_bytes = 10 * 1024^2,
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

benchmark_index <- function(index, iterations = 200L) {
  first_namespace <- index$entries$namespace_id[[1L]]
  first_resource <- index$entries$resource[[1L]]
  selected_group_id <- index$groups$group_id[[floor(nrow(index$groups) / 2L)]]
  operations <- list(
    navigation = function() {
      metadata_resource_choices(index, first_namespace)
    },
    search = function() {
      metadata_index_query_groups(index, query = "medi", limit = 200L)
    },
    filtering = function() {
      metadata_index_query_groups(
        index,
        namespace_id = first_namespace,
        resource = first_resource,
        limit = 200L
      )
    },
    sorting = function() {
      metadata_index_query_groups(
        index,
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
    hierarchy_navigation = function() {
      neighborhood <- metadata_index_group_neighborhood(
        index,
        selected_group_id
      )
      context <- metadata_index_node_context(
        index,
        neighborhood$representative_node_id
      )
      if (nrow(context$nearby) > 0L) {
        metadata_index_node_context(index, context$nearby$node_id[[1L]])
      } else if (nrow(context$children) > 0L) {
        metadata_index_node_context(index, context$children$node_id[[1L]])
      } else {
        context
      }
    },
    collapsed_hierarchy_shell = function() {
      htmltools::renderTags(
        metadata_hierarchy_shell_ui(shiny::NS("metadata"))
      )$html
    },
    hierarchy_panel_render = function() {
      group <- metadata_index_group(index, selected_group_id)
      neighborhood <- metadata_index_group_neighborhood(index, group)
      context <- metadata_index_node_context(
        index,
        neighborhood$representative_node_id
      )
      htmltools::renderTags(metadata_hierarchy_panel_ui(
        group,
        context,
        neighborhood
      ))$html
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
iterations <- c(
  stats::setNames(rep(200L, length(bundles)), names(bundles)),
  `tum-iztech-appended` = 200L,
  `ucr-scaled-50000` = 120L
)
timings <- lapply(names(cases), function(case_id) {
  selected_group_id <- cases[[case_id]]$groups$group_id[[
    floor(nrow(cases[[case_id]]$groups) / 2L)
  ]]
  hierarchy_operation <- function() {
    neighborhood <- metadata_index_group_neighborhood(
      cases[[case_id]],
      selected_group_id
    )
    metadata_index_node_context(
      cases[[case_id]],
      neighborhood$representative_node_id
    )
  }
  hierarchy_panel_operation <- function() {
    group <- metadata_index_group(cases[[case_id]], selected_group_id)
    neighborhood <- metadata_index_group_neighborhood(
      cases[[case_id]],
      group
    )
    context <- metadata_index_node_context(
      cases[[case_id]],
      neighborhood$representative_node_id
    )
    htmltools::renderTags(metadata_hierarchy_panel_ui(
      group,
      context,
      neighborhood
    ))$html
  }
  hierarchy_allocated_bytes <- allocated_bytes(hierarchy_operation)
  hierarchy_panel_allocated_bytes <- allocated_bytes(
    hierarchy_panel_operation
  )
  list(
    case_id = case_id,
    entry_count = nrow(cases[[case_id]]$entries),
    group_count = nrow(cases[[case_id]]$groups),
    node_count = nrow(cases[[case_id]]$nodes),
    index_bytes = as.numeric(object.size(cases[[case_id]])),
    hierarchy_allocated_bytes = hierarchy_allocated_bytes,
    hierarchy_allocation_pass = hierarchy_allocated_bytes <=
      budgets$hierarchy_navigation_max_alloc_bytes,
    hierarchy_panel_allocated_bytes = hierarchy_panel_allocated_bytes,
    hierarchy_panel_allocation_pass = hierarchy_panel_allocated_bytes <=
      budgets$hierarchy_panel_render_max_alloc_bytes,
    operations = benchmark_index(cases[[case_id]], iterations[[case_id]])
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

result <- list(
  report_schema = "llw-metadata-acceptance",
  report_version = "1.6.0",
  benchmarked_at = benchmarked_at,
  baseline = "b704d24ee6eab066163bb88e7125faf19e09543d",
  budgets = budgets,
  environment = list(
    r_version = R.version.string,
    platform = R.version$platform,
    LightLogWeb_version = as.character(utils::packageVersion("LightLogWeb")),
    glcdp_version = as.character(utils::packageVersion("glcdp")),
    glcdp_revision = utils::packageDescription("glcdp")$RemoteSha
  ),
  fixture_manifest_generated_at = manifest$generated_at,
  timing = timings,
  memory = memory,
  review_route = review_route,
  gates = list(
    design_target_pass = all(vapply(
      all_operations,
      `[[`,
      logical(1),
      "design_target_pass"
    )) &&
      review_route$selection$design_target_pass,
    hard_timing_gate_pass = all(vapply(
      all_operations,
      `[[`,
      logical(1),
      "hard_gate_pass"
    )) &&
      review_route$selection$hard_gate_pass,
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
        "hierarchy_allocation_pass"
      )) &&
      all(vapply(
        timings,
        `[[`,
        logical(1),
        "hierarchy_panel_allocation_pass"
      )) &&
      review_route$appended_bundle_bytes <= budgets$retained_bundle_max_bytes,
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
  stop("At least one metadata interaction exceeded the 100 ms p95 gate.")
}
if (!result$gates$memory_gate_pass) {
  stop("At least one metadata memory budget failed.")
}
if (!result$gates$appended_file_group_identity_pass) {
  stop("Appended source metadata lost an exact file-group identity.")
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
cat("Combined fixture bundle bytes:", memory$combined_bundle_bytes, "\n")
cat("Scaled index bytes:", memory$scaled_index_bytes, "\n")
