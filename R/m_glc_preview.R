glc_source_selection_key <- function(selection) {
  if (is.null(selection)) return(NULL)
  if (
    !inherits(selection, "llw_glc_registry_selection") ||
      !is.list(selection) ||
      !isTRUE(selection$has_latest_pass)
  ) {
    return(NULL)
  }
  paste(selection$repository, tolower(selection$resolved_revision), sep = "@")
}

glc_format_bytes <- function(bytes, unknown = "Unknown") {
  if (length(bytes) != 1L || is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return(unknown)
  }
  units <- c("B", "KiB", "MiB", "GiB", "TiB")
  unit_index <- 1L
  value <- as.numeric(bytes)
  while (value >= 1024 && unit_index < length(units)) {
    value <- value / 1024
    unit_index <- unit_index + 1L
  }
  digits <- if (unit_index == 1L) 0L else 2L
  paste0(
    format(round(value, digits), trim = TRUE, nsmall = digits),
    " ",
    units[[unit_index]]
  )
}

glc_compact_display_value <- function(value) {
  if (is.data.frame(value) || is.matrix(value) || is.array(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  } else if (is.list(value)) {
    value <- unlist(value, recursive = TRUE, use.names = FALSE)
  }
  if (length(value) == 0L || all(is.na(value))) return("Unknown")
  value <- as.character(value[!is.na(value)])
  value <- value[nzchar(value)]
  if (length(value) == 0L) return("Unknown")
  paste(unique(value), collapse = ", ")
}

glc_wizard_steps <- function() {
  data.frame(
    value = c("source", "inspect", "discovery", "structure", "narrow"),
    label = c(
      "Choose source",
      "Inspect package",
      "Inventory & discovery",
      "Choose structure",
      "Narrow with metadata"
    ),
    icon = c(
      "cloud-arrow-down",
      "box-arrow-in-down",
      "search",
      "collection",
      "filter"
    ),
    stringsAsFactors = FALSE
  )
}

glc_wizard_stepper <- function(
  ns,
  active,
  source_ready = FALSE,
  inspection_ready = FALSE,
  plan_ready = FALSE,
  structure_ready = FALSE
) {
  steps <- glc_wizard_steps()
  active_index <- match(active, steps$value)
  if (is.na(active_index)) {
    abort_llw("Unknown GLC wizard step.", type = "validation")
  }
  availability <- c(
    source = TRUE,
    inspect = isTRUE(source_ready),
    discovery = isTRUE(inspection_ready),
    structure = isTRUE(plan_ready),
    narrow = isTRUE(structure_ready)
  )
  completed <- c(
    if (isTRUE(source_ready)) "source",
    if (isTRUE(inspection_ready)) "inspect",
    if (isTRUE(plan_ready)) "discovery",
    if (isTRUE(structure_ready)) "structure"
  )
  tags$ol(
    class = "llw-import-stepper",
    "aria-label" = "GLC package steps",
    lapply(seq_len(nrow(steps)), function(index) {
      value <- steps$value[[index]]
      state <- if (identical(value, active)) {
        "active"
      } else if (value %in% completed) {
        "complete"
      } else {
        "upcoming"
      }
      available <- availability[[value]]
      button <- actionButton(
        ns(paste0("wizard_step_", value)),
        tagList(
          tags$span(
            class = "llw-import-stepper__icon",
            "aria-hidden" = "true",
            bsicons::bs_icon(
              if (identical(state, "complete")) "check2" else
                steps$icon[[index]]
            )
          ),
          tags$span(
            class = "llw-import-stepper__copy",
            tags$small(paste("Step", index)),
            tags$strong(steps$label[[index]])
          ),
          if (identical(state, "active")) {
            tags$span(class = "visually-hidden", "Current step")
          }
        ),
        class = "llw-import-stepper__button",
        "aria-current" = if (identical(state, "active")) "step" else NULL,
        "aria-disabled" = if (!available) "true" else NULL,
        title = if (available) {
          paste("Open step", index, steps$label[[index]])
        } else {
          "Complete the preceding step first"
        }
      )
      if (!available) {
        button <- htmltools::tagAppendAttributes(button, disabled = "disabled")
      }
      tags$li(
        class = paste("llw-import-stepper__step", paste0("is-", state)),
        button
      )
    })
  )
}

glc_task_status_tag <- function(
  status,
  idle_heading,
  idle_message,
  task_label = "Task"
) {
  assert_scalar_string(task_label, "task_label")
  state <- status$state
  callout_state <- if (state %in% c("queued", "running", "finalizing")) {
    "queued"
  } else if (state %in% c("complete", "warning", "error")) {
    state
  } else if (identical(state, "stale")) {
    "warning"
  } else {
    "idle"
  }
  heading <- if (identical(state, "idle")) {
    idle_heading
  } else {
    switch(
      state,
      queued = paste(task_label, "queued"),
      running = paste(task_label, "running"),
      finalizing = paste("Validating", tolower(task_label), "result"),
      complete = paste(task_label, "ready"),
      warning = paste(task_label, "ready with warnings"),
      error = paste(task_label, "failed: retry is available"),
      cancelled = paste(task_label, "cancelled"),
      stale = "Stale result discarded",
      idle_heading
    )
  }
  message <- if (identical(state, "idle")) {
    idle_message
  } else {
    status$message %||% idle_message
  }
  llw_status_callout(
    callout_state,
    message,
    heading = heading,
    live = state %in% c("queued", "running", "finalizing"),
    compact = TRUE
  )
}

glc_task_progress_ui <- function(
  progress,
  total_phases,
  heading,
  idle_message,
  active = FALSE,
  active_label = "Preparing task",
  active_detail = "Starting the requested background operation.",
  terminal_state = NULL,
  terminal_message = NULL
) {
  assert_flag(active, "active")
  assert_scalar_string(active_label, "active_label")
  assert_scalar_string(active_detail, "active_detail")
  if (!is.null(terminal_state)) {
    assert_scalar_string(terminal_state, "terminal_state")
  }
  if (!is.null(terminal_message)) {
    assert_scalar_string(terminal_message, "terminal_message")
  }
  cancelled <- identical(terminal_state, "cancelled")
  if (is.null(progress) || nrow(progress) == 0L) {
    if (cancelled) {
      progress <- data.frame(
        phase = "cancelled",
        label = paste(heading, "cancelled"),
        status = "cancelled",
        elapsed_seconds = NA_real_,
        detail = terminal_message %||%
          "Cancellation was requested before a phase completed.",
        stringsAsFactors = FALSE
      )
    } else if (active) {
      progress <- data.frame(
        phase = "preparing",
        label = active_label,
        status = "running",
        elapsed_seconds = NA_real_,
        detail = active_detail,
        stringsAsFactors = FALSE
      )
    } else {
      return(llw_status_callout(
        "idle",
        idle_message,
        heading = heading,
        compact = TRUE
      ))
    }
  }
  if (cancelled) {
    running_rows <- progress$status == "running"
    if (any(running_rows)) {
      progress$status[running_rows] <- "cancelled"
      progress$detail[running_rows] <- terminal_message %||%
        "Cancellation was requested before this phase completed."
    } else if (!any(progress$status == "cancelled")) {
      progress <- rbind(
        progress,
        data.frame(
          phase = "cancelled",
          label = paste(heading, "cancelled"),
          status = "cancelled",
          elapsed_seconds = NA_real_,
          detail = terminal_message %||% "Cancellation was requested.",
          stringsAsFactors = FALSE
        )
      )
    }
  }
  complete_count <- sum(progress$status == "complete")
  failed <- any(progress$status == "error")
  running <- any(progress$status == "running")
  cancelled <- any(progress$status == "cancelled")
  percentage <- min(
    100,
    round(100 * (complete_count + if (running) 0.5 else 0) / total_phases)
  )
  progress_summary <- if (cancelled) {
    paste(
      "Cancelled after",
      complete_count,
      "of",
      total_phases,
      "phases completed"
    )
  } else {
    paste(complete_count, "of", total_phases, "phases complete")
  }
  if (
    complete_count == total_phases &&
      !failed &&
      !running &&
      !cancelled
  ) {
    return(llw_status_callout(
      "complete",
      paste("All", total_phases, "phases completed successfully."),
      heading = paste(heading, "ready"),
      compact = TRUE
    ))
  }
  tags$div(
    class = "llw-glc-task-progress",
    tags$div(
      class = "d-flex justify-content-between gap-3 mb-2",
      tags$strong(heading),
      tags$span(
        class = "llw-secondary",
        progress_summary
      )
    ),
    tags$div(
      class = "progress mb-3",
      role = "progressbar",
      "aria-label" = heading,
      "aria-valuemin" = "0",
      "aria-valuemax" = "100",
      "aria-valuenow" = as.character(percentage),
      "aria-valuetext" = progress_summary,
      tags$div(
        class = paste(
          "progress-bar",
          if (running) "progress-bar-striped progress-bar-animated",
          if (failed) "bg-danger",
          if (cancelled) "bg-secondary"
        ),
        style = paste0("width: ", percentage, "%"),
        paste0(percentage, "%")
      )
    ),
    tags$ol(
      class = "list-group list-group-numbered mb-0",
      lapply(seq_len(nrow(progress)), function(index) {
        row <- progress[index, , drop = FALSE]
        tags$li(
          class = paste(
            "list-group-item d-flex gap-3 align-items-start",
            if (identical(row$status, "complete")) "list-group-item-success",
            if (identical(row$status, "error")) "list-group-item-danger",
            if (identical(row$status, "cancelled")) "list-group-item-secondary"
          ),
          tags$div(
            class = "flex-grow-1",
            tags$strong(row$label),
            tags$div(class = "small", row$detail)
          ),
          tags$span(
            class = "badge text-bg-light",
            switch(
              row$status,
              complete = "Complete",
              running = "Running",
              error = "Failed",
              cancelled = "Cancelled",
              row$status
            )
          )
        )
      })
    )
  )
}

glc_inspection_request_from_selection <- function(selection, cache_dir) {
  if (
    is.null(selection) ||
      !inherits(selection, "llw_glc_registry_selection") ||
      !isTRUE(selection$has_latest_pass)
  ) {
    abort_llw(
      "Choose a registry source with an exact passing revision first.",
      type = "validation"
    )
  }
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

glc_metric_with_help <- function(icon_name, label, value, detail, help) {
  assert_scalar_string(help, "help")
  htmltools::tagAppendAttributes(
    import_wizard_metric(icon_name, label, value, detail),
    title = help,
    "aria-label" = paste(label, value, help)
  )
}

glc_inspection_participant_count <- function(inspection) {
  if (
    "participant_count" %in%
      names(inspection$summary) &&
      length(inspection$summary$participant_count) > 0L
  ) {
    value <- suppressWarnings(as.numeric(inspection$summary$participant_count[[
      1L
    ]]))
    if (
      length(value) == 1L && is.finite(value) && value >= 0 && value %% 1 == 0
    ) {
      return(as.integer(value))
    }
  }
  length(glc_nonempty_unique(inspection$datasets$participant_id))
}

glc_row_list_values <- function(data, column, index) {
  if (!column %in% names(data)) return(character())
  value <- if (is.list(data[[column]])) {
    data[[column]][[index]]
  } else {
    data[[column]][[index]]
  }
  glc_nonempty_unique(value)
}

glc_modality_catalogue <- function(files) {
  validate_glc_inspection_table(
    files,
    "files",
    c("dataset_id", "file_group_id", "modalities")
  )
  records <- lapply(seq_len(nrow(files)), function(index) {
    modalities <- glc_row_list_values(files, "modalities", index)
    if (length(modalities) == 0L) return(NULL)
    data.frame(
      modality = modalities,
      dataset_id = as.character(files$dataset_id[[index]]),
      file_group_id = as.character(files$file_group_id[[index]]),
      stringsAsFactors = FALSE
    )
  })
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0L) {
    return(data.frame(
      modality = character(),
      dataset_count = integer(),
      file_group_count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  records <- do.call(rbind, records)
  values <- sort(unique(records$modality), method = "radix")
  result <- do.call(
    rbind,
    lapply(values, function(value) {
      rows <- records$modality == value
      data.frame(
        modality = value,
        dataset_count = length(unique(records$dataset_id[rows])),
        file_group_count = length(unique(records$file_group_id[rows])),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(result) <- NULL
  result
}

glc_modality_file_groups <- function(files, modality) {
  assert_scalar_string(modality, "modality")
  keep <- vapply(
    seq_len(nrow(files)),
    function(index) {
      modality %in% glc_row_list_values(files, "modalities", index)
    },
    logical(1)
  )
  glc_nonempty_unique(files$file_group_id[keep])
}

glc_terms_for_modality <- function(inspection, modality = NULL) {
  inspection <- validate_glc_inspection(inspection)
  if (is.null(modality)) return(inspection$terms)
  assert_scalar_string(modality, "modality")
  group_ids <- glc_modality_file_groups(inspection$files, modality)
  variables <- inspection$variables[
    inspection$variables$file_group_id %in% group_ids,
    ,
    drop = FALSE
  ]
  if (nrow(variables) == 0L) return(inspection$terms[0, , drop = FALSE])
  glc_term_catalogue(variables)
}

glc_modality_choice_labels <- function(modalities) {
  labels <- paste0(
    modalities$modality,
    " | ",
    modalities$file_group_count,
    " file group",
    ifelse(modalities$file_group_count == 1L, "", "s")
  )
  stats::setNames(modalities$modality, labels)
}

glc_inventory_metrics_ui <- function(inspection) {
  inspection <- validate_glc_inspection(inspection)
  file_groups <- unique(as.character(inspection$files$file_group_id))
  expected_bytes <- as.numeric(inspection$files$expected_bytes)
  known_bytes <- sum(expected_bytes, na.rm = TRUE)
  unknown_files <- sum(is.na(expected_bytes))
  modalities <- glc_modality_catalogue(inspection$files)
  variable_keys <- unique(data.frame(
    name = as.character(inspection$variables$name),
    description = as.character(inspection$variables$description),
    unit = as.character(inspection$variables$unit),
    stringsAsFactors = FALSE
  ))
  inconsistent_labels <- sum(!inspection$terms$label_consistent)
  tags$div(
    class = "llw-glc-metrics",
    layout_column_wrap(
      width = 1 / 4,
      heights_equal = "row",
      fillable = FALSE,
      glc_metric_with_help(
        "patch-check",
        "Package schema",
        inspection$source$schema_version,
        "Validated package format",
        paste(
          c(
            paste(
              "Exact source:",
              inspection$source$repository,
              "at revision",
              inspection$source$resolved_revision
            ),
            if (length(inspection$warnings) > 0L) {
              paste("Package note:", paste(inspection$warnings, collapse = " "))
            }
          ),
          collapse = " "
        )
      ),
      glc_metric_with_help(
        "database",
        "Datasets",
        format(nrow(inspection$datasets), big.mark = ","),
        "Validated package datasets",
        "The number of validated datasets in the selected package revision."
      ),
      glc_metric_with_help(
        "people",
        "Participants",
        format(glc_inspection_participant_count(inspection), big.mark = ","),
        "Linked in package metadata",
        paste(
          "The number of participants reported by the package summary, or the",
          "number of unique participant links when no summary count is available."
        )
      ),
      glc_metric_with_help(
        "collection",
        "File groups",
        format(length(file_groups), big.mark = ","),
        "Groups used for compatibility checks",
        "A file group is the stable package unit used by glcdp for planning."
      ),
      glc_metric_with_help(
        "file-earmark",
        "Files",
        format(nrow(inspection$files), big.mark = ","),
        if (unknown_files > 0L) {
          paste(unknown_files, "size(s) unavailable")
        } else {
          paste(glc_format_bytes(known_bytes), "listed size")
        },
        paste(
          "The number of measurement files listed by the package. Size values",
          "come from package metadata and do not require opening those files."
        )
      ),
      glc_metric_with_help(
        "columns-gap",
        "Variables",
        format(nrow(variable_keys), big.mark = ","),
        "Unique name, description and unit",
        paste(
          "Variables are deduplicated across file groups by the combination of",
          "source name, description, and unit."
        )
      ),
      glc_metric_with_help(
        "tags",
        "Canonical terms",
        format(nrow(inspection$terms), big.mark = ","),
        "Deduplicated identifiers",
        paste0(
          "Distinct canonical semantic-term identifiers used by variables. ",
          inconsistent_labels,
          " term(s) have inconsistent human labels, so their canonical ",
          "identifier is used as the primary label."
        )
      ),
      glc_metric_with_help(
        "activity",
        "Modalities",
        format(nrow(modalities), big.mark = ","),
        "Measurement kinds",
        "Distinct modality values used across the package's file groups."
      )
    )
  )
}

glc_inventory_detail_ui <- function(inspection) {
  inspection <- validate_glc_inspection(inspection)
  tags$section(
    class = "llw-wizard-field-group llw-glc-explorer-link",
    tags$div(
      tags$h4(class = "mb-1", "Explore the complete package separately"),
      tags$p(
        class = "mb-0 llw-secondary",
        paste(
          "This wizard focuses on selecting compatible package parts for a",
          "later download. For the complete datasets, file groups, variables,",
          "and metadata inventory, use the dedicated glcdp package explorer."
        )
      )
    ),
    tags$div(
      class = "d-flex flex-wrap align-items-center gap-2",
      tags$a(
        class = "btn btn-outline-primary",
        href = "https://tscnlab-glc-dp-r.share.connect.posit.cloud",
        target = "_blank",
        rel = "noopener noreferrer",
        title = "Open the dedicated glcdp package explorer in a new tab",
        bsicons::bs_icon("box-arrow-up-right"),
        " Explore package with glcdp"
      ),
      tags$span(
        class = "small llw-secondary",
        tags$strong("Exact source: "),
        inspection$source$repository,
        " at ",
        tags$code(inspection$source$resolved_revision)
      )
    )
  )
}

glc_filter_terms <- function(terms, query = "") {
  validate_glc_inspection_table(
    terms,
    "term catalogue",
    c(
      "term",
      "label",
      "term_names",
      "source_names",
      "source_labels",
      "descriptions"
    )
  )
  query <- trimws(as.character(query %||% ""))
  if (!nzchar(query) || nrow(terms) == 0L) return(terms)
  normalize_search <- function(value) {
    value <- tolower(as.character(value))
    value <- gsub("[^[:alnum:]]+", " ", value)
    trimws(gsub("[[:space:]]+", " ", value))
  }
  searchable <- vapply(
    seq_len(nrow(terms)),
    function(index) {
      paste(
        terms$term[[index]],
        terms$label[[index]],
        glc_compact_display_value(terms$term_names[[index]]),
        glc_compact_display_value(terms$source_names[[index]]),
        glc_compact_display_value(terms$source_labels[[index]]),
        glc_compact_display_value(terms$descriptions[[index]])
      )
    },
    character(1)
  )
  query <- normalize_search(query)
  searchable <- normalize_search(searchable)
  terms[
    grepl(query, searchable, fixed = TRUE),
    ,
    drop = FALSE
  ]
}

glc_term_choice_labels <- function(terms) {
  if (nrow(terms) == 0L) {
    return(stats::setNames(character(), character()))
  }
  labels <- paste0(
    terms$label,
    " \u2014 ",
    terms$term,
    " \u00b7 ",
    terms$file_group_count,
    " file group",
    ifelse(terms$file_group_count == 1L, "", "s")
  )
  stats::setNames(terms$term, labels)
}

glc_display_list <- function(value, max_items = 6L, empty = "Not available") {
  value <- glc_nonempty_unique(value)
  if (length(value) == 0L) return(empty)
  shown <- utils::head(value, max_items)
  result <- paste(shown, collapse = ", ")
  if (length(value) > length(shown)) {
    result <- paste0(result, " (+", length(value) - length(shown), " more)")
  }
  result
}

glc_pluralized_count <- function(
  count,
  singular,
  plural = paste0(singular, "s")
) {
  if (
    !is.numeric(count) ||
      length(count) != 1L ||
      is.na(count) ||
      !is.finite(count) ||
      count < 0 ||
      count != floor(count)
  ) {
    abort_llw(
      "`count` must be one non-negative whole number.",
      type = "validation"
    )
  }
  assert_scalar_string(singular, "singular")
  assert_scalar_string(plural, "plural")
  label <- if (count == 1L) singular else plural
  paste(format(as.integer(count), big.mark = ","), label)
}

glc_full_variable_list_tag <- function(values, summary_items = 8L) {
  values <- glc_nonempty_unique(values)
  if (
    !is.numeric(summary_items) ||
      length(summary_items) != 1L ||
      is.na(summary_items) ||
      summary_items < 1 ||
      summary_items != floor(summary_items)
  ) {
    abort_llw(
      "`summary_items` must be one positive whole number.",
      type = "validation"
    )
  }
  if (length(values) == 0L) return("No variables available")
  if (length(values) <= summary_items) {
    return(tags$ul(
      class = "llw-glc-variable-list mb-0",
      lapply(values, tags$li)
    ))
  }
  tagList(
    tags$p(
      class = "mb-2",
      tags$strong("Summary: "),
      glc_display_list(values, max_items = summary_items)
    ),
    tags$details(
      class = "llw-glc-variable-disclosure",
      tags$summary(glc_pluralized_count(
        length(values),
        "Show all variable",
        "Show all variables"
      )),
      tags$ul(
        class = "llw-glc-variable-list mt-2 mb-0",
        lapply(values, tags$li)
      )
    )
  )
}

glc_variable_display_values <- function(variables) {
  if (!is.data.frame(variables) || nrow(variables) == 0L) return(character())
  vapply(
    seq_len(nrow(variables)),
    function(index) {
      name <- as.character(variables$name[[index]])
      label <- as.character(variables$label[[index]])
      if (is.na(label) || !nzchar(trimws(label)) || identical(label, name)) {
        name
      } else {
        paste0(label, " (", name, ")")
      }
    },
    character(1)
  ) |>
    glc_nonempty_unique()
}

glc_plan_compatibility_default <- function(plan) {
  if (
    !inherits(plan, "glc_collection_plan") ||
      !is.data.frame(plan$compatibility_sets) ||
      nrow(plan$compatibility_sets) == 0L
  ) {
    return(NULL)
  }
  sets <- plan$compatibility_sets
  order_index <- order(
    -as.integer(sets$dataset_count),
    -as.integer(sets$file_group_count),
    as.character(sets$compatibility_id),
    method = "radix"
  )
  as.character(sets$compatibility_id[[order_index[[1L]]]])
}

glc_compatibility_groups <- function(plan, compatibility_id) {
  if (is.null(compatibility_id)) return(plan$groups[0, , drop = FALSE])
  plan$groups[
    plan$groups$status == "included" &
      plan$groups$compatibility_id == compatibility_id,
    ,
    drop = FALSE
  ]
}

glc_plan_metadata_table <- function(plan, name) {
  table <- plan$metadata[[name]]
  if (!is.data.frame(table)) {
    abort_llw(
      paste("The collection plan lacks metadata table", name),
      type = "validation"
    )
  }
  table
}

glc_compatibility_diagnostics <- function(plan, compatibility_id) {
  diagnostics <- plan$compatibility_diagnostics
  if (!is.data.frame(diagnostics) || nrow(diagnostics) == 0L) {
    return(diagnostics[0L, , drop = FALSE])
  }
  applies <- vapply(
    diagnostics$compatibility_ids,
    function(ids) compatibility_id %in% glc_nonempty_unique(ids),
    logical(1)
  )
  diagnostics <- diagnostics[applies, , drop = FALSE]
  if (nrow(diagnostics) == 0L) return(diagnostics)
  current_order <- ifelse(diagnostics$applies_to_current_plan %in% TRUE, 0L, 1L)
  classification_order <- ifelse(
    diagnostics$classification == "blocking",
    0L,
    1L
  )
  diagnostics[
    order(
      current_order,
      classification_order,
      as.character(diagnostics$variable_name),
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
}

glc_diagnostic_list_tag <- function(diagnostics) {
  if (!is.data.frame(diagnostics) || nrow(diagnostics) == 0L) return(NULL)
  tags$ul(
    class = "mb-0 ps-3",
    lapply(seq_len(nrow(diagnostics)), function(index) {
      row <- diagnostics[index, , drop = FALSE]
      scope <- if (isTRUE(row$applies_to_current_plan[[1L]])) {
        if (identical(row$classification[[1L]], "blocking")) {
          "Blocks this request"
        } else {
          "Handled by glcdp"
        }
      } else {
        "Relevant if variable scope expands"
      }
      tags$li(
        tags$strong(paste0(scope, ": ")),
        as.character(row$message[[1L]])
      )
    })
  )
}

glc_structure_source_tag <- function(context) {
  instruments <- glc_nonempty_unique(context$instrument_names)
  if (length(instruments) > 0L) {
    return(tagList(
      tags$strong("Instrument: "),
      glc_display_list(instruments, max_items = 3L)
    ))
  }
  if (length(context$device_ids) == 0L) return("---")
  device_labels <- if (nrow(context$devices) == 0L) {
    context$device_ids
  } else {
    vapply(
      seq_len(nrow(context$devices)),
      function(index) {
        label <- glc_nonempty_unique(c(
          context$devices$manufacturer[[index]],
          context$devices$model[[index]]
        ))
        if (length(label) == 0L) context$devices$device_id[[index]] else
          paste(label, collapse = " ")
      },
      character(1)
    )
  }
  tagList(
    tags$strong("Device: "),
    glc_display_list(device_labels, max_items = 3L),
    tags$br(),
    tags$strong("Wearing or device location: "),
    glc_display_list(context$positions, max_items = 4L, empty = "---"),
    tags$br(),
    tags$strong("Location type: "),
    glc_display_list(context$location_types, max_items = 4L, empty = "---")
  )
}

glc_compatibility_context <- function(plan, compatibility_id) {
  sets <- plan$compatibility_sets
  row <- sets[sets$compatibility_id == compatibility_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    abort_llw(
      "Choose one available compatibility structure.",
      type = "validation"
    )
  }
  groups <- glc_compatibility_groups(plan, compatibility_id)
  group_ids <- as.character(groups$file_group_id)
  variables <- glc_plan_metadata_table(plan, "file_group_variables")
  variables <- variables[
    as.character(variables$file_group_id) %in% group_ids,
    ,
    drop = FALSE
  ]
  selected <- variables[variables$selected_by_request %in% TRUE, , drop = FALSE]
  selected_keys <- unique(paste(
    selected$name,
    selected$description,
    selected$unit,
    sep = "\r"
  ))
  variable_keys <- paste(
    variables$name,
    variables$description,
    variables$unit,
    sep = "\r"
  )
  other <- variables[!variable_keys %in% selected_keys, , drop = FALSE]
  participants <- glc_plan_metadata_table(plan, "participants")
  participants <- participants[
    participants$participant_id %in% glc_nonempty_unique(groups$participant_id),
    ,
    drop = FALSE
  ]
  devices <- glc_plan_metadata_table(plan, "devices")
  devices <- devices[
    devices$device_id %in% glc_nonempty_unique(groups$device_id),
    ,
    drop = FALSE
  ]
  instruments <- glc_plan_metadata_table(plan, "instruments")
  instruments <- instruments[
    instruments$file_group_id %in% group_ids,
    ,
    drop = FALSE
  ]
  studies <- glc_plan_metadata_table(plan, "studies")
  studies <- studies[
    studies$study_id %in% glc_nonempty_unique(groups$study_id),
    ,
    drop = FALSE
  ]
  modalities <- glc_nonempty_unique(unlist(
    groups$modalities,
    recursive = TRUE,
    use.names = FALSE
  ))
  files <- plan$files[
    plan$files$status == "included" &
      as.character(plan$files$file_group_id) %in% group_ids,
    ,
    drop = FALSE
  ]
  list(
    set = row,
    groups = groups,
    group_ids = group_ids,
    files = files,
    variables = variables,
    selected_variables = glc_variable_display_values(selected),
    other_variables = glc_variable_display_values(other),
    all_variables = glc_variable_display_values(variables),
    descriptions = glc_nonempty_unique(groups$description),
    instructions = glc_nonempty_unique(groups$instructions),
    modalities = modalities,
    roles = glc_nonempty_unique(groups$role),
    data_states = glc_nonempty_unique(groups$data_state),
    formats = glc_nonempty_unique(groups$format),
    timezones = glc_nonempty_unique(groups$timezone),
    participants = participants,
    participant_ids = glc_nonempty_unique(groups$participant_id),
    devices = devices,
    device_ids = glc_nonempty_unique(groups$device_id),
    instruments = instruments,
    instrument_names = glc_nonempty_unique(instruments$instrument_name),
    positions = glc_nonempty_unique(groups$device_location),
    location_types = glc_nonempty_unique(groups$device_location_type),
    studies = studies,
    study_locations = glc_nonempty_unique(studies$geographical_location),
    diagnostics = glc_compatibility_diagnostics(plan, compatibility_id)
  )
}

glc_compatibility_choice_tag <- function(context, index, broadest = FALSE) {
  row <- context$set
  title <- glc_display_list(
    context$selected_variables,
    max_items = 4L,
    empty = "Package key and time columns"
  )
  tags$div(
    class = "llw-glc-unit-choice llw-glc-structure-choice",
    tags$div(
      class = "llw-glc-unit-choice__header",
      tags$div(
        tags$span(class = "llw-wizard-eyebrow", paste("Structure", index)),
        tags$strong(class = "llw-glc-unit-choice__title", title)
      ),
      if (isTRUE(broadest)) {
        tags$span(
          class = "badge text-bg-primary",
          title = paste(
            "This structure has the broadest dataset and file-group coverage.",
            "It is not a scientific preference."
          ),
          "Broadest coverage"
        )
      }
    ),
    tags$p(
      class = "llw-glc-unit-choice__description",
      glc_structure_source_tag(context)
    ),
    tags$dl(
      class = "llw-glc-unit-facts",
      tags$div(
        tags$dt("Coverage"),
        tags$dd(paste(
          glc_pluralized_count(row$file_group_count[[1L]], "file group"),
          "across",
          glc_pluralized_count(row$dataset_count[[1L]], "dataset")
        ))
      ),
      tags$div(
        tags$dt("Files"),
        tags$dd(glc_pluralized_count(row$file_count[[1L]], "file"))
      ),
      tags$div(
        tags$dt("Modalities"),
        tags$dd(glc_display_list(context$modalities))
      ),
      tags$div(tags$dt("Role"), tags$dd(glc_display_list(context$roles))),
      if (nrow(context$diagnostics) > 0L) {
        tags$div(
          class = "llw-glc-unit-facts__wide",
          tags$details(
            class = "llw-glc-card-disclosure",
            tags$summary("Compatibility and harmonization details"),
            glc_diagnostic_list_tag(context$diagnostics)
          )
        )
      },
      tags$div(
        class = "llw-glc-unit-facts__wide",
        tags$dt("Variables selected by this discovery request"),
        tags$dd(glc_display_list(context$selected_variables, max_items = 7L))
      ),
      if (length(context$other_variables) > 0L) {
        tags$div(
          class = "llw-glc-unit-facts__wide",
          tags$details(
            class = "llw-glc-card-disclosure",
            tags$summary(paste(
              glc_pluralized_count(
                length(context$other_variables),
                "other variable"
              ),
              "available"
            )),
            glc_full_variable_list_tag(
              context$other_variables,
              summary_items = 7L
            )
          )
        )
      }
    )
  )
}

glc_compatibility_summary_ui <- function(plan, modality = NULL) {
  sets <- plan$compatibility_sets
  groups <- plan$groups
  included <- sum(groups$status == "included")
  excluded <- sum(groups$status != "included")
  term <- glc_nonempty_unique(plan$request$terms)
  basis <- c(
    if (length(term) > 0L) paste("term", term),
    if (!is.null(modality) && nzchar(modality)) paste("modality", modality)
  )
  diagnostics <- plan$compatibility_diagnostics
  current_diagnostics <- diagnostics[
    diagnostics$applies_to_current_plan %in% TRUE,
    ,
    drop = FALSE
  ]
  prospective_diagnostics <- diagnostics[
    !diagnostics$applies_to_current_plan %in% TRUE,
    ,
    drop = FALSE
  ]
  diagnostic_summary <- if (nrow(diagnostics) == 0L) {
    NULL
  } else {
    tags$details(
      class = "llw-glc-plan-diagnostics mt-3",
      tags$summary(paste(
        glc_pluralized_count(nrow(current_diagnostics), "current diagnostic"),
        "and",
        glc_pluralized_count(
          nrow(prospective_diagnostics),
          "prospective diagnostic"
        )
      )),
      if (nrow(current_diagnostics) > 0L) {
        tagList(
          tags$h4(class = "h6 mt-3", "Current compatibility handling"),
          glc_diagnostic_list_tag(current_diagnostics)
        )
      },
      if (nrow(prospective_diagnostics) > 0L) {
        tagList(
          tags$h4(class = "h6 mt-3", "If more variables are included later"),
          glc_diagnostic_list_tag(prospective_diagnostics)
        )
      }
    )
  }
  tagList(
    layout_column_wrap(
      width = 1 / 3,
      heights_equal = "row",
      fillable = FALSE,
      glc_metric_with_help(
        "collection",
        "Compatible structures",
        format(nrow(sets), big.mark = ","),
        "Provided by glcdp",
        paste(
          "Each structure contains file groups with the same selected columns,",
          "types, factor contract, time handling, modality, role, and data state."
        )
      ),
      glc_metric_with_help(
        "check-circle",
        "Included file groups",
        format(included, big.mark = ","),
        "Matched this request",
        "Stable file groups included in a public glcdp compatibility structure."
      ),
      glc_metric_with_help(
        "dash-circle",
        "Excluded file groups",
        format(excluded, big.mark = ","),
        "Outside this request",
        "File groups excluded by the selected term, modality, or package rules."
      )
    ),
    tags$p(
      class = "small llw-secondary mt-2 mb-0",
      tags$strong("Discovery basis: "),
      if (length(basis) > 0L) paste(basis, collapse = " and ") else
        "all eligible file groups"
    ),
    diagnostic_summary
  )
}

glc_typed_facet_value <- function(value, unit = NA_character_) {
  if (is.list(value) && length(value) == 1L) value <- value[[1L]]
  text <- if (length(value) == 0L || is.null(value) || all(is.na(value))) {
    "Unknown"
  } else {
    paste(as.character(value), collapse = ", ")
  }
  if (!is.na(unit) && nzchar(unit)) paste(text, unit) else text
}

glc_characteristic_catalogue <- function(characteristics, participant_ids) {
  rows <- characteristics[
    characteristics$participant_id %in%
      participant_ids &
      !is.na(characteristics$name) &
      nzchar(trimws(as.character(characteristics$name))),
    ,
    drop = FALSE
  ]
  if (nrow(rows) == 0L) {
    return(data.frame(
      name = character(),
      description = character(),
      unit = character(),
      value_type = character(),
      participant_id = character(),
      value_key = character(),
      value_label = character(),
      numeric_value = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  records <- lapply(seq_len(nrow(rows)), function(index) {
    values <- unlist(
      rows$value[[index]],
      recursive = TRUE,
      use.names = FALSE
    )
    values <- values[!is.na(values)]
    if (length(values) == 0L) return(NULL)
    name <- trimws(as.character(rows$name[[index]]))
    unit <- glc_nonempty_unique(rows$unit[[index]])
    unit <- if (length(unit) == 0L) "" else unit[[1L]]
    description <- glc_nonempty_unique(rows$description[[index]])
    description <- if (length(description) == 0L) "" else description[[1L]]
    value_type <- tolower(trimws(as.character(rows$value_type[[index]])))
    value_type <- if (length(value_type) == 0L || is.na(value_type)) {
      "unknown"
    } else {
      value_type[[1L]]
    }
    value_text <- as.character(values)
    numeric_values <- suppressWarnings(as.numeric(value_text))
    numeric_type <- value_type %in%
      c(
        "double",
        "integer",
        "number",
        "numeric"
      )
    data.frame(
      name = name,
      description = description,
      unit = unit,
      value_type = value_type,
      participant_id = rep(
        as.character(rows$participant_id[[index]]),
        length(values)
      ),
      value_key = value_text,
      value_label = vapply(
        values,
        glc_typed_facet_value,
        character(1),
        unit = unit
      ),
      numeric_value = if (numeric_type) numeric_values else
        rep(NA_real_, length(values)),
      stringsAsFactors = FALSE
    )
  })
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0L) {
    return(data.frame(
      name = character(),
      description = character(),
      unit = character(),
      value_type = character(),
      participant_id = character(),
      value_key = character(),
      value_label = character(),
      numeric_value = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  records <- unique(do.call(rbind, records))
  records[
    order(
      records$name,
      records$value_label,
      records$participant_id,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
}

glc_characteristic_filter_spec <- function(catalogue, name) {
  name <- glc_nonempty_unique(name)
  if (nrow(catalogue) == 0L || length(name) != 1L) {
    return(list(
      type = "categorical",
      name = "",
      label = "",
      records = catalogue[0L, , drop = FALSE],
      slider = NULL
    ))
  }
  records <- catalogue[catalogue$name == name[[1L]], , drop = FALSE]
  if (nrow(records) == 0L) {
    return(list(
      type = "categorical",
      name = "",
      label = "",
      records = records,
      slider = NULL
    ))
  }
  units <- glc_nonempty_unique(records$unit)
  label <- if (length(units) == 1L) {
    paste0(name[[1L]], " (", units[[1L]], ")")
  } else {
    name[[1L]]
  }
  numeric <- all(
    records$value_type %in%
      c(
        "double",
        "integer",
        "number",
        "numeric"
      )
  ) &&
    all(is.finite(records$numeric_value))
  list(
    type = if (numeric) "numeric" else "categorical",
    name = name[[1L]],
    label = label,
    records = records,
    slider = if (numeric) glc_numeric_filter_limits(records$numeric_value) else
      NULL
  )
}

glc_metadata_filter_input_id <- function(id, revision = NULL) {
  assert_scalar_string(id, "metadata filter input id")
  if (is.null(revision)) return(id)
  if (
    length(revision) != 1L ||
      is.na(revision) ||
      !is.numeric(revision) ||
      !is.finite(revision) ||
      revision < 0 ||
      revision != floor(revision)
  ) {
    abort_llw(
      "The metadata filter revision is invalid.",
      type = "validation"
    )
  }
  paste0(id, "_r", as.integer(revision))
}

glc_numeric_filter_limits <- function(values) {
  values <- suppressWarnings(as.numeric(values))
  values <- values[is.finite(values)]
  if (length(values) == 0L) return(NULL)
  limits <- range(values)
  step <- if (all(values == floor(values))) {
    1
  } else if (identical(limits[[1L]], limits[[2L]])) {
    0.1
  } else {
    signif(diff(limits) / 100, 2L)
  }
  list(min = limits[[1L]], max = limits[[2L]], step = step)
}

glc_device_model_catalogue <- function(devices) {
  if (nrow(devices) == 0L) {
    return(data.frame(
      key = character(),
      label = character(),
      device_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  keep <- !is.na(devices$model) & nzchar(trimws(devices$model))
  devices <- devices[keep, , drop = FALSE]
  if (nrow(devices) == 0L) {
    return(data.frame(
      key = character(),
      label = character(),
      device_id = character(),
      stringsAsFactors = FALSE
    ))
  }
  manufacturer <- as.character(devices$manufacturer)
  manufacturer[is.na(manufacturer)] <- ""
  model <- as.character(devices$model)
  unique(data.frame(
    key = paste(manufacturer, model, sep = "\r"),
    label = ifelse(nzchar(manufacturer), paste(manufacturer, model), model),
    device_id = as.character(devices$device_id),
    stringsAsFactors = FALSE
  ))
}

glc_metadata_filter_catalogue <- function(plan, compatibility_id) {
  context <- glc_compatibility_context(plan, compatibility_id)
  participants <- context$participants
  devices <- context$devices
  participant_characteristics <- glc_plan_metadata_table(
    plan,
    "participant_characteristics"
  )
  characteristics <- glc_characteristic_catalogue(
    participant_characteristics,
    context$participant_ids
  )
  device_labels <- if (nrow(devices) == 0L) {
    character()
  } else {
    vapply(
      seq_len(nrow(devices)),
      function(index) {
        values <- glc_nonempty_unique(c(
          devices$manufacturer[[index]],
          devices$model[[index]]
        ))
        if (length(values) == 0L) devices$device_id[[index]] else
          paste(values, collapse = " ")
      },
      character(1)
    )
  }
  list(
    context = context,
    ages = glc_nonempty_unique(participants$age),
    age_values = suppressWarnings(as.numeric(participants$age)),
    sexes = glc_nonempty_unique(participants$sex),
    genders = glc_nonempty_unique(participants$gender),
    characteristics = characteristics,
    positions = context$positions,
    location_types = context$location_types,
    device_labels = stats::setNames(
      as.character(devices$device_id),
      device_labels
    ),
    device_models = glc_device_model_catalogue(devices),
    has_device = any(
      !is.na(context$groups$device_id) &
        nzchar(context$groups$device_id)
    ),
    has_no_device = any(
      is.na(context$groups$device_id) |
        !nzchar(context$groups$device_id)
    )
  )
}

glc_filter_compatibility_groups <- function(
  plan,
  compatibility_id,
  filters = list()
) {
  catalogue <- glc_metadata_filter_catalogue(plan, compatibility_id)
  groups <- catalogue$context$groups
  keep <- rep(TRUE, nrow(groups))
  selected <- function(name)
    glc_nonempty_unique(filters[[name]] %||% character())
  relation <- filters$device_relation %||% "any"
  has_device <- !is.na(groups$device_id) & nzchar(groups$device_id)
  if (identical(relation, "with")) keep <- keep & has_device
  if (identical(relation, "without")) keep <- keep & !has_device
  positions <- selected("positions")
  if (length(positions) > 0L)
    keep <- keep & groups$device_location %in% positions
  location_types <- selected("location_types")
  if (length(location_types) > 0L) {
    keep <- keep & groups$device_location_type %in% location_types
  }
  participants <- catalogue$context$participants
  participant_ids <- as.character(groups$participant_id)
  age_range <- suppressWarnings(as.numeric(filters$age_range %||% numeric()))
  age_range <- age_range[is.finite(age_range)]
  if (length(age_range) == 2L) {
    participant_age <- suppressWarnings(as.numeric(participants$age))
    allowed <- participants$participant_id[
      is.finite(participant_age) &
        participant_age >= min(age_range) &
        participant_age <= max(age_range)
    ]
    keep <- keep & participant_ids %in% allowed
  }
  ages <- selected("ages")
  if (length(ages) > 0L) {
    allowed <- participants$participant_id[
      as.character(participants$age) %in% ages
    ]
    keep <- keep & participant_ids %in% allowed
  }
  sexes <- selected("sexes")
  if (length(sexes) > 0L) {
    allowed <- participants$participant_id[participants$sex %in% sexes]
    keep <- keep & participant_ids %in% allowed
  }
  genders <- selected("genders")
  if (length(genders) > 0L) {
    allowed <- participants$participant_id[participants$gender %in% genders]
    keep <- keep & participant_ids %in% allowed
  }
  characteristic_name <- selected("characteristic_name")
  characteristic_spec <- glc_characteristic_filter_spec(
    catalogue$characteristics,
    characteristic_name
  )
  if (nzchar(characteristic_spec$name)) {
    characteristic_values <- selected("characteristic_values")
    characteristic_range <- suppressWarnings(as.numeric(
      filters$characteristic_range %||% numeric()
    ))
    characteristic_range <- characteristic_range[
      is.finite(characteristic_range)
    ]
    matched <- character()
    if (
      identical(characteristic_spec$type, "numeric") &&
        length(characteristic_range) == 2L
    ) {
      matched <- characteristic_spec$records$participant_id[
        characteristic_spec$records$numeric_value >= min(characteristic_range) &
          characteristic_spec$records$numeric_value <= max(characteristic_range)
      ]
    } else if (
      identical(characteristic_spec$type, "categorical") &&
        length(characteristic_values) > 0L
    ) {
      matched <- characteristic_spec$records$participant_id[
        characteristic_spec$records$value_key %in% characteristic_values
      ]
    }
    if (
      length(characteristic_values) > 0L || length(characteristic_range) == 2L
    ) {
      keep <- keep & participant_ids %in% matched
    }
  }
  device_models <- selected("device_models")
  if (length(device_models) > 0L) {
    allowed <- catalogue$device_models$device_id[
      catalogue$device_models$key %in% device_models
    ]
    keep <- keep & groups$device_id %in% allowed
  }
  device_ids <- selected("device_ids")
  if (length(device_ids) > 0L) keep <- keep & groups$device_id %in% device_ids
  as.character(groups$file_group_id[keep])
}

glc_aggregated_structure_detail_ui <- function(context, selected_ids = NULL) {
  groups <- context$groups
  if (!is.null(selected_ids)) {
    groups <- groups[groups$file_group_id %in% selected_ids, , drop = FALSE]
  }
  variables <- context$variables[
    context$variables$file_group_id %in% groups$file_group_id,
    ,
    drop = FALSE
  ]
  files <- context$files[
    context$files$file_group_id %in% groups$file_group_id,
    ,
    drop = FALSE
  ]
  participants <- glc_nonempty_unique(groups$participant_id)
  devices <- glc_nonempty_unique(groups$device_id)
  positions <- glc_nonempty_unique(groups$device_location)
  locations <- glc_nonempty_unique(groups$device_location_type)
  tags$section(
    class = "llw-glc-selected-structure",
    tags$h3("Selected structure summary"),
    tags$dl(
      class = "llw-glc-unit-facts llw-glc-aggregate-facts",
      tags$div(
        tags$dt("File groups"),
        tags$dd(glc_pluralized_count(nrow(groups), "file group"))
      ),
      tags$div(
        tags$dt("Datasets"),
        tags$dd(glc_pluralized_count(
          length(glc_nonempty_unique(groups$dataset_id)),
          "dataset"
        ))
      ),
      tags$div(
        tags$dt("Files"),
        tags$dd(glc_pluralized_count(nrow(files), "file"))
      ),
      tags$div(
        tags$dt("Participants"),
        tags$dd(
          if (length(participants) == 0L) {
            "Not participant-associated"
          } else {
            glc_pluralized_count(length(participants), "participant")
          }
        )
      ),
      tags$div(
        tags$dt("Devices"),
        tags$dd(
          if (length(devices) == 0L) {
            "Not device-associated"
          } else {
            glc_pluralized_count(length(devices), "device")
          }
        )
      ),
      tags$div(
        tags$dt("Modalities"),
        tags$dd(glc_display_list(context$modalities))
      ),
      tags$div(tags$dt("Roles"), tags$dd(glc_display_list(context$roles))),
      if (length(devices) > 0L) {
        tags$div(
          tags$dt("Wearing or device positions"),
          tags$dd(
            glc_display_list(positions)
          )
        )
      },
      if (length(devices) > 0L) {
        tags$div(
          tags$dt("Location kinds"),
          tags$dd(glc_display_list(locations))
        )
      },
      tags$div(
        class = "llw-glc-unit-facts__wide",
        tags$dt("Instrument or device context"),
        tags$dd(glc_structure_source_tag(context))
      ),
      tags$div(
        class = "llw-glc-unit-facts__wide",
        tags$dt("Variables"),
        tags$dd(glc_full_variable_list_tag(
          glc_variable_display_values(variables),
          summary_items = 10L
        ))
      )
    ),
    tags$details(
      class = "small llw-secondary mt-2",
      tags$summary("Technical compatibility identity"),
      tags$code(context$set$compatibility_id[[1L]])
    )
  )
}

glc_metadata_filter_inputs <- function(ns, catalogue, revision = NULL) {
  input_id <- function(id) {
    ns(glc_metadata_filter_input_id(id, revision))
  }
  add_multi <- function(id, label, choices) {
    choices <- glc_nonempty_unique(choices)
    if (length(choices) == 0L) return(NULL)
    selectInput(
      input_id(id),
      label,
      choices = choices,
      selected = character(),
      multiple = TRUE,
      width = "100%"
    )
  }
  numeric_control <- function(id, label, values) {
    limits <- glc_numeric_filter_limits(values)
    if (is.null(limits)) return(NULL)
    if (identical(limits$min, limits$max)) {
      return(tags$div(
        class = "llw-glc-range-filter",
        tags$strong(label),
        tags$p(
          class = "small llw-secondary mb-0",
          paste("Only", limits$min, "is reported, so no range is needed.")
        )
      ))
    }
    sliderInput(
      input_id(id),
      label,
      min = limits$min,
      max = limits$max,
      value = c(limits$min, limits$max),
      step = limits$step,
      width = "100%"
    )
  }

  characteristic_names <- glc_nonempty_unique(catalogue$characteristics$name)
  characteristic_labels <- vapply(
    characteristic_names,
    function(name) {
      glc_characteristic_filter_spec(catalogue$characteristics, name)$label
    },
    character(1)
  )

  participant_controls <- Filter(
    Negate(is.null),
    list(
      numeric_control(
        "filter_age_range",
        "Participant age range",
        catalogue$age_values
      ),
      add_multi("filter_sex", "Participant sex", catalogue$sexes),
      add_multi("filter_gender", "Participant gender", catalogue$genders),
      if (length(characteristic_names) > 0L) {
        selectInput(
          input_id("filter_characteristic_name"),
          "Characteristic",
          choices = c(
            "No characteristic filter" = "",
            stats::setNames(characteristic_names, characteristic_labels)
          ),
          selected = "",
          multiple = FALSE,
          width = "100%"
        )
      },
      if (length(characteristic_names) > 0L) {
        uiOutput(ns("characteristic_value_filter"))
      }
    )
  )

  device_controls <- if (!catalogue$has_device) {
    list()
  } else {
    Filter(
      Negate(is.null),
      list(
        if (catalogue$has_no_device) {
          selectInput(
            input_id("device_relation"),
            "Device relationship",
            choices = c(
              "Any relationship" = "any",
              "With a linked device" = "with",
              "Without a linked device" = "without"
            ),
            selected = "any",
            width = "100%"
          )
        },
        if (nrow(catalogue$device_models) > 0L) {
          choices <- unique(catalogue$device_models[c("key", "label")])
          selectInput(
            input_id("filter_device_model"),
            "Device model",
            choices = stats::setNames(choices$key, choices$label),
            selected = character(),
            multiple = TRUE,
            width = "100%"
          )
        },
        add_multi(
          "filter_position",
          "Wearing or device location",
          catalogue$positions
        ),
        add_multi(
          "filter_location_type",
          "Location type",
          catalogue$location_types
        )
      )
    )
  }

  tags$div(
    class = "llw-glc-filter-sections",
    tags$section(
      class = "llw-glc-filter-section",
      tags$h4("Participant filters"),
      if (length(participant_controls) == 0L) {
        tags$p(
          class = "small llw-secondary mb-0",
          "No participant filter is available for this structure."
        )
      } else {
        tags$div(class = "llw-glc-facet-grid", participant_controls)
      }
    ),
    if (catalogue$has_device) {
      tags$section(
        class = "llw-glc-filter-section",
        tags$h4("Device filters"),
        if (length(device_controls) == 0L) {
          tags$p(
            class = "small llw-secondary mb-0",
            "The structure is device-associated, but no device facet is reported."
          )
        } else {
          tags$div(class = "llw-glc-facet-grid", device_controls)
        }
      )
    }
  )
}

glc_characteristic_value_input <- function(
  ns,
  catalogue,
  characteristic_name,
  revision = NULL,
  selected = NULL
) {
  spec <- glc_characteristic_filter_spec(
    catalogue$characteristics,
    characteristic_name
  )
  if (!nzchar(spec$name)) return(NULL)
  input_id <- ns(glc_metadata_filter_input_id(
    "filter_characteristic_values",
    revision
  ))
  if (identical(spec$type, "numeric")) {
    limits <- spec$slider
    if (is.null(limits)) return(NULL)
    if (identical(limits$min, limits$max)) {
      return(tags$div(
        class = "llw-glc-range-filter",
        tags$strong("Characteristic value"),
        tags$p(
          class = "small llw-secondary mb-0",
          paste("Only", limits$min, "is reported, so no range is needed.")
        )
      ))
    }
    selected <- suppressWarnings(as.numeric(selected %||% numeric()))
    selected <- selected[is.finite(selected)]
    if (length(selected) != 2L) {
      selected <- c(limits$min, limits$max)
    } else {
      selected <- c(
        max(limits$min, min(selected)),
        min(limits$max, max(selected))
      )
    }
    return(sliderInput(
      input_id,
      paste(spec$label, "range"),
      min = limits$min,
      max = limits$max,
      value = selected,
      step = limits$step,
      width = "100%"
    ))
  }

  choices <- unique(spec$records[c("value_key", "value_label")])
  selected <- intersect(
    glc_nonempty_unique(selected %||% character()),
    choices$value_key
  )
  selectInput(
    input_id,
    paste(spec$label, "values"),
    choices = stats::setNames(choices$value_key, choices$value_label),
    selected = selected,
    multiple = TRUE,
    width = "100%"
  )
}

glc_metadata_filters_from_input <- function(input, catalogue, revision = NULL) {
  input_value <- function(id, default) {
    input[[glc_metadata_filter_input_id(id, revision)]] %||% default
  }
  selected_allowed <- function(id, allowed) {
    intersect(
      glc_nonempty_unique(input_value(id, character())),
      glc_nonempty_unique(allowed)
    )
  }
  age_range <- input_value("filter_age_range", numeric())
  age_limits <- glc_numeric_filter_limits(catalogue$age_values)
  if (
    !is.null(age_limits) &&
      length(age_range) == 2L &&
      isTRUE(all.equal(
        as.numeric(age_range),
        c(age_limits$min, age_limits$max),
        check.attributes = FALSE
      ))
  ) {
    age_range <- numeric()
  }
  characteristic_name <- glc_nonempty_unique(input_value(
    "filter_characteristic_name",
    character()
  ))
  allowed_characteristic_names <- glc_nonempty_unique(
    catalogue$characteristics$name
  )
  characteristic_name <- intersect(
    characteristic_name,
    allowed_characteristic_names
  )
  characteristic_spec <- glc_characteristic_filter_spec(
    catalogue$characteristics,
    characteristic_name
  )
  characteristic_values <- character()
  characteristic_range <- numeric()
  if (nzchar(characteristic_spec$name)) {
    raw_characteristic_value <- input_value(
      "filter_characteristic_values",
      character()
    )
    if (identical(characteristic_spec$type, "numeric")) {
      characteristic_range <- suppressWarnings(as.numeric(
        raw_characteristic_value
      ))
      characteristic_range <- characteristic_range[
        is.finite(characteristic_range)
      ]
      limits <- characteristic_spec$slider
      if (
        !is.null(limits) &&
          length(characteristic_range) == 2L &&
          isTRUE(all.equal(
            characteristic_range,
            c(limits$min, limits$max),
            check.attributes = FALSE
          ))
      ) {
        characteristic_range <- numeric()
      }
    } else {
      characteristic_values <- intersect(
        glc_nonempty_unique(raw_characteristic_value),
        glc_nonempty_unique(characteristic_spec$records$value_key)
      )
    }
  }
  device_relation <- input_value("device_relation", "any")
  if (
    !isTRUE(catalogue$has_device) ||
      !isTRUE(catalogue$has_no_device) ||
      !device_relation %in% c("any", "with", "without")
  ) {
    device_relation <- "any"
  }
  list(
    device_relation = device_relation,
    positions = if (isTRUE(catalogue$has_device)) {
      selected_allowed("filter_position", catalogue$positions)
    } else character(),
    location_types = if (isTRUE(catalogue$has_device)) {
      selected_allowed("filter_location_type", catalogue$location_types)
    } else character(),
    age_range = age_range,
    sexes = selected_allowed("filter_sex", catalogue$sexes),
    genders = selected_allowed("filter_gender", catalogue$genders),
    characteristic_name = characteristic_name,
    characteristic_values = characteristic_values,
    characteristic_range = characteristic_range,
    device_models = if (isTRUE(catalogue$has_device)) {
      selected_allowed("filter_device_model", catalogue$device_models$key)
    } else character()
  )
}

glc_active_metadata_filter_count <- function(filters) {
  scalar_count <- sum(vapply(
    filters[c(
      "positions",
      "location_types",
      "age_range",
      "sexes",
      "genders",
      "device_models"
    )],
    function(value) length(glc_nonempty_unique(value)) > 0L,
    logical(1)
  ))
  characteristic_count <- as.integer(
    length(glc_nonempty_unique(
      filters$characteristic_values %||% character()
    )) >
      0L ||
      length(filters$characteristic_range %||% numeric()) == 2L
  )
  scalar_count +
    characteristic_count +
    as.integer(!identical(filters$device_relation %||% "any", "any"))
}

glc_refinement_file_count <- function(refinement, plan) {
  if (!inherits(refinement, "glc_collection_refinement")) {
    abort_llw("The collection refinement is invalid.", type = "validation")
  }
  if (!inherits(plan, "glc_collection_plan")) {
    abort_llw("The parent collection plan is invalid.", type = "validation")
  }
  group_ids <- glc_nonempty_unique(refinement$groups$file_group_id)
  files <- plan$files[
    plan$files$status == "included" &
      plan$files$file_group_id %in% group_ids,
    ,
    drop = FALSE
  ]
  as.integer(nrow(files))
}

glc_refinement_summary_ui <- function(refinement, plan) {
  groups <- refinement$groups
  file_count <- glc_refinement_file_count(refinement, plan)
  tagList(
    tags$div(
      class = "mt-3",
      llw_status_callout(
        if (isTRUE(refinement$final_selection_required)) "warning" else
          "complete",
        if (isTRUE(refinement$final_selection_required)) {
          paste(
            "glcdp reports more than one compatible final unit inside this",
            "filtered subset. Metadata filtering did not create this",
            "difference. Review the public compatibility diagnostics before",
            "a later read step."
          )
        } else {
          paste(
            "The filtered groups resolve to one final unit.",
            "This check reopened no package and used no network access."
          )
        },
        heading = if (isTRUE(refinement$final_selection_required)) {
          "Filtered subset needs compatibility review"
        } else {
          "One final unit ready for later review"
        },
        compact = TRUE
      )
    ),
    layout_column_wrap(
      width = 1 / 4,
      heights_equal = "row",
      fillable = FALSE,
      glc_metric_with_help(
        "folder-check",
        "File groups",
        format(nrow(groups), big.mark = ","),
        "Retained by filters",
        "Stable file groups passed to the public zero-access refinement API."
      ),
      glc_metric_with_help(
        "database",
        "Datasets",
        format(length(glc_nonempty_unique(groups$dataset_id)), big.mark = ","),
        "Represented in the result",
        "Unique datasets represented by the refined file groups."
      ),
      glc_metric_with_help(
        "file-earmark",
        "Files",
        format(file_count, big.mark = ","),
        "Represented in the result",
        "Files represented by the refined file groups in the public plan."
      ),
      glc_metric_with_help(
        "people",
        "Participants",
        format(
          length(glc_nonempty_unique(groups$participant_id)),
          big.mark = ","
        ),
        "Represented in the result",
        "Unique linked participants represented by the refined file groups."
      )
    )
  )
}

glc_cancel_task_button <- function(ns, input_id, enabled, label) {
  button <- actionButton(
    ns(input_id),
    label,
    icon = icon("ban"),
    class = "btn-outline-secondary btn-lg"
  )
  if (!isTRUE(enabled)) {
    button <- htmltools::tagAppendAttributes(
      button,
      disabled = "disabled",
      "aria-disabled" = "true"
    )
  }
  button
}

glcPreviewUI <- function(id, source_ui = NULL) {
  ns <- NS(id)
  tags$section(
    class = "llw-import-wizard llw-glc-wizard mt-4",
    "aria-labelledby" = ns("wizard_title"),
    tags$style(HTML(paste(
      ".llw-glc-wizard .llw-glc-metrics .llw-wizard-metric {",
      "height: 100%; min-height: 4.25rem; padding: .45rem .7rem;}",
      ".llw-glc-wizard .llw-glc-metrics .llw-wizard-metric__label,",
      ".llw-glc-wizard .llw-glc-metrics .llw-wizard-metric__detail {",
      "overflow-wrap: normal; word-break: normal; hyphens: none;}",
      ".llw-glc-wizard .llw-import-stepper {grid-template-columns:",
      "repeat(5, minmax(0, 1fr));}",
      ".llw-glc-wizard .llw-glc-discovery-card,",
      ".llw-glc-wizard .llw-glc-discovery-card > .card-body,",
      ".llw-glc-wizard .llw-glc-metadata-card,",
      ".llw-glc-wizard .llw-glc-metadata-card > .card-body {",
      "overflow: visible !important;}",
      ".llw-glc-wizard .llw-glc-source > .mb-4 {display: none;}",
      ".llw-glc-wizard .llw-glc-source {margin: 0;}",
      ".llw-glc-explorer-link {display: flex; align-items: center;",
      "justify-content: space-between; gap: 1rem; padding: .85rem 1rem;}",
      ".llw-glc-discovery-grid {display: grid; grid-template-columns:",
      "repeat(2, minmax(0, 1fr)); gap: .75rem 1rem;}",
      ".llw-glc-discovery-grid .form-group {margin-bottom: 0;}",
      ".llw-glc-unit-selector .shiny-options-group {",
      "display: grid; grid-template-columns: repeat(2, minmax(0, 1fr));",
      "gap: 1rem; align-items: stretch;}",
      ".llw-glc-unit-selector .form-check {",
      "position: relative; padding-left: 0; margin: 0; height: 100%;}",
      ".llw-glc-unit-selector .radio {position: relative; display: flex;",
      "min-width: 0; width: 100%; height: 100%; margin: 0;}",
      ".llw-glc-unit-selector .form-check-input {",
      "position: absolute; z-index: 1; margin-left: 1rem; margin-top: 1.35rem;}",
      ".llw-glc-unit-selector .radio > label > input[type=radio] {",
      "position: absolute; z-index: 1; margin-left: 1rem; margin-top: 1.35rem;}",
      ".llw-glc-unit-selector .form-check-label {display: block; width: 100%;",
      "height: 100%;}",
      ".llw-glc-unit-selector .radio > label {display: block; min-width: 0;",
      "width: 100%; height: 100%; margin: 0; padding-left: 0;}",
      ".llw-glc-unit-selector .form-check-label > span {display: block;",
      "height: 100%;}",
      ".llw-glc-unit-selector .radio > label > span {display: block;",
      "min-width: 0; width: 100%; height: 100%;}",
      ".llw-glc-unit-choice {",
      "border: 1px solid var(--bs-border-color); border-radius: .75rem;",
      "box-sizing: border-box; width: 100%;",
      "padding: 1rem 1rem 1rem 3rem; background: var(--bs-body-bg);}",
      ".llw-glc-structure-choice {height: 100%;}",
      ".llw-glc-unit-selector .form-check-input:checked + .form-check-label",
      ".llw-glc-unit-choice {",
      "border-color: var(--bs-primary); box-shadow: 0 0 0 .2rem",
      "color-mix(in srgb, var(--bs-primary) 18%, transparent);}",
      ".llw-glc-unit-selector .form-check-input:focus-visible + .form-check-label",
      ".llw-glc-unit-choice {outline: 3px solid var(--bs-focus-ring-color);",
      "outline-offset: 2px;}",
      ".llw-glc-unit-choice__header {display: flex; justify-content: space-between;",
      "align-items: flex-start; gap: 1rem; margin-bottom: .75rem;}",
      ".llw-glc-unit-choice__title {display: block; font-size: 1.05rem;}",
      ".llw-glc-unit-choice__description {margin-bottom: .75rem;}",
      ".llw-glc-unit-facts {display: grid; grid-template-columns:",
      "repeat(2, minmax(0, 1fr)); gap: .65rem 1.25rem; margin: 0;}",
      ".llw-glc-unit-facts > div {min-width: 0;}",
      ".llw-glc-unit-facts dt {font-size: .8rem; color: var(--bs-secondary-color);}",
      ".llw-glc-unit-facts dd {margin: 0; overflow-wrap: anywhere;}",
      ".llw-glc-unit-facts__wide {grid-column: 1 / -1;}",
      ".llw-glc-card-disclosure {border-top: 1px solid",
      "var(--bs-border-color); padding-top: .45rem;}",
      ".llw-glc-card-disclosure summary {cursor: pointer; font-weight: 600;}",
      ".llw-glc-variable-list {display: grid; grid-template-columns:",
      "repeat(3, minmax(0, 1fr)); gap: .35rem 1.5rem;",
      "padding-left: 1.25rem;}",
      ".llw-glc-variable-list li {overflow-wrap: anywhere;}",
      ".llw-glc-selected-option-status {display: flex; align-items: center;",
      "gap: .5rem; margin-top: 1rem;}",
      ".llw-glc-facet-grid {display: grid; grid-template-columns:",
      "repeat(2, minmax(0, 1fr)); gap: .75rem 1rem;}",
      ".llw-glc-facet-grid .form-group {margin-bottom: 0;}",
      ".llw-glc-filter-sections {display: grid; gap: 1rem;}",
      ".llw-glc-filter-section {border: 1px solid var(--bs-border-color);",
      "border-radius: .65rem; padding: 1rem;}",
      ".llw-glc-filter-section h4 {margin-bottom: .85rem;}",
      ".llw-glc-range-filter {min-width: 0;}",
      ".llw-glc-selected-structure {margin-top: 1.25rem;}",
      "@media (max-width: 56.25rem) {",
      ".llw-glc-wizard .llw-import-stepper {grid-template-columns:",
      "repeat(2, minmax(0, 1fr));}",
      ".llw-glc-metrics > .bslib-grid {grid-template-columns:",
      "repeat(2, minmax(0, 1fr)) !important;}",
      ".llw-glc-unit-selector .shiny-options-group {grid-template-columns: 1fr;}",
      ".llw-glc-variable-list {grid-template-columns: repeat(2, minmax(0, 1fr));}}",
      "@media (max-width: 36rem) {",
      ".llw-glc-wizard .llw-import-stepper {grid-template-columns:",
      "minmax(0, 1fr);}",
      ".llw-glc-metrics > .bslib-grid {grid-template-columns:",
      "minmax(0, 1fr) !important;}",
      ".llw-glc-discovery-grid, .llw-glc-facet-grid,",
      ".llw-glc-variable-list {grid-template-columns: minmax(0, 1fr);}",
      ".llw-glc-explorer-link {align-items: flex-start; flex-direction: column;}}",
      "@media (max-width: 700px) {",
      ".llw-glc-unit-facts {grid-template-columns: 1fr;}",
      ".llw-glc-unit-facts__wide {grid-column: auto;}",
      ".llw-glc-unit-choice__header {flex-direction: column;}}"
    ))),
    tags$header(
      class = "llw-import-wizard__hero",
      tags$span(
        class = "llw-import-wizard__hero-icon",
        "aria-hidden" = "true",
        bsicons::bs_icon("boxes")
      ),
      tags$div(
        tags$p(class = "llw-wizard-eyebrow", "Guided GLC package workflow"),
        tags$h2(
          id = ns("wizard_title"),
          "Choose and prepare a compatible GLC package subset"
        ),
        tags$p(
          paste(
            "Choose one live registry source, inspect its validated package",
            "information, collect structurally compatible file groups, then",
            "narrow them with participant and device metadata."
          )
        )
      ),
      tags$span(
        class = "llw-import-wizard__engine-note",
        bsicons::bs_icon("shield-check"),
        " Metadata-only through Slice 2A"
      )
    ),
    uiOutput(ns("wizard_stepper")),
    navset_hidden(
      id = ns("wizard_steps"),
      selected = "source",
      nav_panel(
        "Choose source",
        value = "source",
        tags$div(
          class = "llw-wizard-step-content",
          import_wizard_step_heading(
            "cloud-arrow-down",
            "Choose from the live registry",
            "Select one validated package source",
            paste(
              "Search the official registry and select one exact repository.",
              "A valid deep link can preselect a source and begin its",
              "metadata-only inspection automatically."
            )
          ),
          source_ui %||%
            llw_status_callout(
              "idle",
              "The host application has not supplied a registry selector.",
              heading = "Source selector unavailable",
              compact = TRUE
            ),
          import_wizard_navigation(
            uiOutput(ns("continue_inspect_button"), inline = TRUE),
            split = TRUE
          )
        )
      ),
      nav_panel(
        "Inspect package",
        value = "inspect",
        tags$div(
          class = "llw-wizard-step-content",
          import_wizard_step_heading(
            "box-arrow-in-down",
            "Review the selected package",
            "Inspect package information and core metadata",
            paste(
              "The descriptor and core-metadata transfer size is unavailable.",
              "Opening this step starts that metadata transfer when needed.",
              "Measurement files remain unopened."
            )
          ),
          tags$div(
            class = "mb-3",
            llw_status_callout(
              "idle",
              paste(
                "The package does not provide a transfer size for its descriptor",
                "and core metadata. Inspection transfers that metadata only."
              ),
              heading = "Metadata transfer size unavailable",
              compact = TRUE
            )
          ),
          tags$div(
            class = "llw-action-row llw-import-actions",
            input_task_button(
              ns("inspect"),
              span(strong("Inspect package")),
              icon = icon("magnifying-glass"),
              class = "btn-primary btn-lg"
            ),
            uiOutput(ns("cancel_inspection_button"), inline = TRUE)
          ),
          card(
            class = "llw-wizard-import-progress",
            card_header(
              tags$span(
                bsicons::bs_icon("list-check"),
                " Inspection progress"
              ),
              container = h3
            ),
            uiOutput(ns("inspection_progress"))
          ),
          uiOutput(ns("inventory_view")),
          import_wizard_navigation(
            actionButton(
              ns("back_source"),
              "Back to source",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary btn-lg"
            ),
            uiOutput(ns("continue_discovery_button"), inline = TRUE),
            split = TRUE
          )
        )
      ),
      nav_panel(
        "Inventory & discovery",
        value = "discovery",
        tags$div(
          class = "llw-wizard-step-content",
          import_wizard_step_heading(
            "search",
            "Choose the data concept",
            "Collect structurally compatible file groups",
            paste(
              "Start with a modality, a canonical data term, or both.",
              "glcdp then compares the selected columns, types, factor contract,",
              "time handling, modality, role, and data state."
            )
          ),
          card(
            class = "llw-glc-discovery-card",
            card_header("Find data by modality and/or term", container = h3),
            tags$div(
              class = "llw-glc-discovery-grid",
              tags$div(uiOutput(ns("modality_selector"))),
              tags$div(uiOutput(ns("term_selector"))),
              tags$div(
                textInput(
                  ns("term_search"),
                  "Optional filter for canonical terms",
                  value = "",
                  placeholder = "For example: melanopic, illuminance, activity",
                  width = "100%"
                )
              ),
              tags$div(uiOutput(ns("term_detail")))
            ),
            uiOutput(ns("discovery_feedback"))
          ),
          tags$div(
            class = "llw-action-row llw-import-actions",
            input_task_button(
              ns("plan"),
              span(strong("Collect compatible groups")),
              icon = icon("layer-group"),
              class = "btn-primary btn-lg"
            ),
            uiOutput(ns("cancel_plan_button"), inline = TRUE)
          ),
          card(
            class = "llw-wizard-import-progress",
            card_header(
              tags$span(
                bsicons::bs_icon("diagram-3"),
                " Planning progress"
              ),
              container = h3
            ),
            uiOutput(ns("plan_progress"))
          ),
          uiOutput(ns("plan_summary")),
          import_wizard_navigation(
            actionButton(
              ns("back_inspect"),
              "Back to inspection",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary btn-lg"
            ),
            uiOutput(ns("continue_structure_button"), inline = TRUE),
            split = TRUE
          )
        )
      ),
      nav_panel(
        "Choose structure",
        value = "structure",
        tags$div(
          class = "llw-wizard-step-content",
          import_wizard_step_heading(
            "collection",
            "Choose one compatible structure",
            "Review the groups that can be read and collected together",
            paste(
              "Each option comes directly from glcdp's public compatibility",
              "planner. Compare the instrument, variable contract, and",
              "aggregated scientific context before continuing."
            )
          ),
          tags$section(
            tags$p(
              class = "llw-secondary",
              paste(
                "Participant and device metadata stay inside each structural",
                "set. They become filters only in the next step."
              )
            ),
            uiOutput(ns("compatibility_selector"))
          ),
          uiOutput(ns("selected_structure_status")),
          uiOutput(ns("structure_detail")),
          import_wizard_navigation(
            actionButton(
              ns("back_discovery"),
              "Back to inventory and discovery",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary btn-lg"
            ),
            uiOutput(ns("continue_narrow_button"), inline = TRUE),
            split = TRUE
          )
        )
      ),
      nav_panel(
        "Narrow with metadata",
        value = "narrow",
        tags$div(
          class = "llw-wizard-step-content",
          import_wizard_step_heading(
            "filter",
            "Apply participant and device filters",
            "Narrow the selected structure with available metadata",
            paste(
              "Numeric fields use inclusive ranges. Device controls appear only",
              "for device-associated groups. The final refinement is an explicit",
              "in-memory check with no package or network access."
            )
          ),
          card(
            class = "llw-glc-metadata-card",
            card_header(
              "Narrow with participant and device metadata",
              container = h3
            ),
            uiOutput(ns("metadata_filters")),
            uiOutput(ns("filter_feedback"))
          ),
          tags$div(
            class = "llw-action-row llw-import-actions",
            uiOutput(ns("refine_button"), inline = TRUE)
          ),
          uiOutput(ns("refinement_summary")),
          tags$p(
            class = "small llw-secondary mt-3",
            paste(
              "Measurement reading, variable-scope decisions, byte and row",
              "review, collection, and dataset-library promotion remain closed."
            )
          ),
          import_wizard_navigation(
            actionButton(
              ns("back_structure"),
              "Back to structure selection",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary btn-lg"
            ),
            split = TRUE
          )
        )
      )
    )
  )
}

glc_inspection_controller_released <- function(
  task_state,
  extended_state,
  active_count
) {
  assert_scalar_string(task_state, "task_state")
  assert_scalar_string(extended_state, "extended_state")
  if (
    !is.numeric(active_count) ||
      length(active_count) != 1L ||
      is.na(active_count) ||
      active_count < 0 ||
      active_count != floor(active_count)
  ) {
    abort_llw(
      "`active_count` must be one non-negative whole number.",
      type = "validation"
    )
  }
  !task_state %in% c("queued", "running", "finalizing") &&
    !identical(extended_state, "running") &&
    identical(as.integer(active_count), 0L)
}

glc_inspection_attempt_is_current <- function(
  attempt,
  context_revision,
  source_key
) {
  if (!is.list(attempt)) return(FALSE)
  identical(attempt$context_revision, as.integer(context_revision)) &&
    identical(attempt$source_key, source_key)
}

glc_inspection_attempt_expired <- function(
  attempt,
  now = Sys.time(),
  timeout_seconds = 15
) {
  if (!is.list(attempt) || is.null(attempt$requested_at)) return(FALSE)
  if (
    !is.numeric(timeout_seconds) ||
      length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) ||
      !is.finite(timeout_seconds) ||
      timeout_seconds <= 0
  ) {
    abort_llw(
      "`timeout_seconds` must be one positive finite number.",
      type = "validation"
    )
  }
  elapsed <- as.numeric(difftime(now, attempt$requested_at, units = "secs"))
  is.finite(elapsed) && elapsed >= timeout_seconds
}

glc_planning_attempt_is_current <- function(
  attempt,
  context_revision,
  source_key,
  term,
  modality
) {
  if (!is.list(attempt) || !is.list(attempt$request)) return(FALSE)
  identical(attempt$context_revision, as.integer(context_revision)) &&
    identical(attempt$source_key, source_key) &&
    identical(attempt$request$term %||% NULL, term %||% NULL) &&
    identical(attempt$request$modality %||% NULL, modality %||% NULL)
}

glcPreviewServer <- function(
  id,
  source_selection,
  runtime,
  cache_dir,
  auto_inspect = NULL,
  inspection_worker = glcdp_inspection_task_worker,
  planning_worker = glcdp_planning_task_worker,
  refine_collection = glcdp_refine_collection
) {
  if (!shiny::is.reactive(source_selection)) {
    abort_llw("source_selection must be reactive.", type = "validation")
  }
  if (!is.null(auto_inspect) && !shiny::is.reactive(auto_inspect)) {
    abort_llw(
      "auto_inspect must be reactive when supplied.",
      type = "validation"
    )
  }
  auto_inspect_enabled <- auto_inspect %||% reactive(FALSE)
  if (!is.list(runtime) || !is.function(runtime$submit)) {
    abort_llw(
      "runtime must be created by new_session_runtime().",
      type = "validation"
    )
  }
  assert_scalar_string(cache_dir, "cache_dir")
  if (
    !is.function(inspection_worker) ||
      !is.function(planning_worker) ||
      !is.function(refine_collection)
  ) {
    abort_llw("GLC Slice 2A workers must be functions.", type = "validation")
  }

  moduleServer(id, function(input, output, session) {
    context_revision <- reactiveVal(0L)
    active_source_key <- reactiveVal(NULL)
    active_arrival_mode <- reactiveVal(NULL)
    inspection_value <- reactiveVal(NULL)
    plan_value <- reactiveVal(NULL)
    selected_term_value <- reactiveVal(NULL)
    selected_modality_value <- reactiveVal(NULL)
    submitted_planning_request <- reactiveVal(NULL)
    inspection_attempted <- reactiveVal(FALSE)
    pending_inspection_attempt <- reactiveVal(NULL)
    inspection_pending_error <- reactiveVal(NULL)
    inspection_controller_revision <- reactiveVal(0L)
    planning_attempted <- reactiveVal(FALSE)
    pending_planning_attempt <- reactiveVal(NULL)
    planning_pending_error <- reactiveVal(NULL)
    planning_terminal_override <- reactiveVal(NULL)
    planning_controller_reset_required <- reactiveVal(FALSE)
    planning_controller_revision <- reactiveVal(0L)
    inspection_progress_path <- reactiveVal(NULL)
    plan_progress_path <- reactiveVal(NULL)
    selected_compatibility_value <- reactiveVal(NULL)
    refinement_value <- reactiveVal(NULL)
    refinement_error <- reactiveVal(NULL)
    metadata_filter_revision <- reactiveVal(0L)
    wizard_step <- reactiveVal("source")

    revision_lookup <- function(context_id) context_revision()

    reset_metadata_filter_state <- function() {
      metadata_filter_revision(metadata_filter_revision() + 1L)
      refinement_value(NULL)
      refinement_error(NULL)
      invisible(NULL)
    }

    create_inspection_task <- function() {
      task <- new_long_task(
        worker = inspection_worker,
        task_type = "glc_discovery",
        runtime = runtime,
        revision_lookup = revision_lookup,
        on_result = function(value, spec) {
          validate_glc_inspection(value)
          inspection_value(value)
          plan_value(NULL)
          planning_attempted(FALSE)
          selected_term_value(NULL)
          selected_modality_value(NULL)
          submitted_planning_request(NULL)
          selected_compatibility_value(NULL)
          reset_metadata_filter_state()
          invisible(NULL)
        }
      )
      bslib::bind_task_button(task$extended_task, "inspect", session = session)
      task
    }
    inspection_task <- create_inspection_task()
    current_inspection_task <- function() {
      inspection_controller_revision()
      inspection_task
    }
    inspection_task_state <- reactive({
      current_inspection_task()$state()
    })
    inspection_task_status <- reactive({
      current_inspection_task()$status()
    })
    inspection_display_status <- reactive({
      pending_error <- inspection_pending_error()
      if (!is.null(pending_error)) {
        return(new_task_status(
          "glc_discovery",
          state = "error",
          message = llw_public_message(pending_error),
          error = pending_error
        ))
      }
      if (!is.null(pending_inspection_attempt())) {
        return(new_task_status(
          "glc_discovery",
          state = "queued",
          message = "Waiting for the previous inspection to stop cleanly."
        ))
      }
      inspection_task_status()
    })
    replace_inspection_task <- function() {
      inspection_task <<- create_inspection_task()
      inspection_controller_revision(inspection_controller_revision() + 1L)
      inspection_task
    }
    create_planning_task <- function() {
      task <- new_long_task(
        worker = planning_worker,
        task_type = "glc_discovery",
        runtime = runtime,
        revision_lookup = revision_lookup,
        on_result = function(value, spec) {
          inspection <- inspection_value()
          req(inspection)
          source <- glc_inspection_source_spec(inspection, cache_dir)
          request <- validate_glc_planning_request(spec$payload$request)
          validate_glc_collection_plan(value, source, request)
          plan_value(value)
          selected_compatibility_value(glc_plan_compatibility_default(value))
          reset_metadata_filter_state()
          invisible(NULL)
        }
      )
      bslib::bind_task_button(
        task$extended_task,
        "plan",
        session = session
      )
      task
    }
    planning_task <- create_planning_task()
    current_planning_task <- function() {
      planning_controller_revision()
      planning_task
    }
    planning_task_state <- reactive({
      current_planning_task()$state()
    })
    planning_task_status <- reactive({
      current_planning_task()$status()
    })
    planning_display_status <- reactive({
      pending_error <- planning_pending_error()
      if (!is.null(pending_error)) {
        return(new_task_status(
          "glc_discovery",
          state = "error",
          message = llw_public_message(pending_error),
          error = pending_error
        ))
      }
      if (!is.null(pending_planning_attempt())) {
        return(new_task_status(
          "glc_discovery",
          state = "queued",
          message = "Waiting for the previous planning task to stop cleanly."
        ))
      }
      terminal_override <- planning_terminal_override()
      if (!is.null(terminal_override)) return(terminal_override)
      planning_task_status()
    })
    replace_planning_task <- function() {
      planning_task <<- create_planning_task()
      planning_controller_revision(planning_controller_revision() + 1L)
      planning_task
    }

    select_wizard_step <- function(value) {
      wizard_step(value)
      bslib::nav_select("wizard_steps", selected = value, session = session)
    }

    submit_pending_inspection <- function() {
      attempt <- isolate(pending_inspection_attempt())
      if (is.null(attempt)) return(invisible(FALSE))
      if (
        !glc_inspection_attempt_is_current(
          attempt,
          isolate(context_revision()),
          glc_source_selection_key(isolate(source_selection()))
        )
      ) {
        pending_inspection_attempt(NULL)
        return(invisible(FALSE))
      }

      if (glc_inspection_attempt_expired(attempt)) {
        pending_inspection_attempt(NULL)
        isolate(current_inspection_task())$cancel()
        inspection_pending_error(new_llw_error(
          message = paste(
            "The previous inspection controller did not release before the",
            "new invocation deadline."
          ),
          type = "resource",
          public_message = paste(
            "The previous inspection is still stopping.",
            "Wait a moment, then retry."
          )
        ))
        return(invisible(FALSE))
      }

      task <- isolate(current_inspection_task())
      released <- glc_inspection_controller_released(
        isolate(task$state()),
        isolate(task$extended_task$status()),
        runtime$active_count()
      )
      if (!released) return(invisible(FALSE))

      if (!identical(isolate(task$state()), "idle")) {
        task <- replace_inspection_task()
      }
      task_id <- task$invoke(
        attempt$payload,
        dataset_id = "glc-source-context"
      )
      if (isFALSE(task_id)) return(invisible(FALSE))
      inspection_progress_path(attempt$progress_path)
      pending_inspection_attempt(NULL)
      invisible(task_id)
    }

    start_inspection <- function() {
      inspection_attempted(TRUE)
      inspection_pending_error(NULL)
      selection <- isolate(source_selection())
      attempted <- tryCatch(
        {
          request <- glc_inspection_request_from_selection(selection, cache_dir)
          progress_path <- file.path(
            runtime$paths$tasks,
            paste0(new_stable_id("glc_inspection_progress"), ".rds")
          )
          pending_inspection_attempt(list(
            payload = new_glcdp_inspection_payload(request, progress_path),
            progress_path = progress_path,
            context_revision = isolate(context_revision()),
            source_key = glc_source_selection_key(selection),
            requested_at = Sys.time()
          ))
          submit_pending_inspection()
        },
        error = identity
      )
      if (inherits(attempted, "error")) {
        pending_inspection_attempt(NULL)
        isolate(current_inspection_task())$reject(attempted)
      }
      invisible(attempted)
    }

    observe({
      req(!is.null(pending_inspection_attempt()))
      invalidateLater(100, session)
      isolate(submit_pending_inspection())
    })

    observe({
      selection <- source_selection()
      key <- glc_source_selection_key(selection)
      previous <- active_source_key()
      arrival_mode <- isTRUE(auto_inspect_enabled())
      if (
        !identical(key, previous) ||
          !identical(arrival_mode, active_arrival_mode())
      ) {
        pending_inspection_attempt(NULL)
        inspection_pending_error(NULL)
        isolate(current_inspection_task())$cancel()
        pending_planning_attempt(NULL)
        planning_pending_error(NULL)
        planning_terminal_override(NULL)
        planning_controller_reset_required(FALSE)
        isolate(current_planning_task())$cancel()
        active_source_key(key)
        active_arrival_mode(arrival_mode)
        context_revision(context_revision() + 1L)
        inspection_value(NULL)
        plan_value(NULL)
        selected_term_value(NULL)
        selected_modality_value(NULL)
        submitted_planning_request(NULL)
        inspection_attempted(FALSE)
        inspection_pending_error(NULL)
        planning_attempted(FALSE)
        inspection_progress_path(NULL)
        plan_progress_path(NULL)
        selected_compatibility_value(NULL)
        reset_metadata_filter_state()
        select_wizard_step("source")
        if (arrival_mode && !is.null(key)) {
          select_wizard_step("inspect")
          start_inspection()
        }
      }
    })

    observeEvent(
      input$inspect,
      {
        start_inspection()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$cancel_inspection,
      {
        pending_inspection_attempt(NULL)
        inspection_pending_error(NULL)
        isolate(current_inspection_task())$cancel()
      },
      ignoreInit = TRUE
    )

    reset_planning_selection <- function() {
      pending_planning_attempt(NULL)
      planning_pending_error(NULL)
      planning_terminal_override(NULL)
      planning_controller_reset_required(FALSE)
      isolate(current_planning_task())$cancel()
      plan_value(NULL)
      submitted_planning_request(NULL)
      planning_attempted(FALSE)
      plan_progress_path(NULL)
      selected_compatibility_value(NULL)
      reset_metadata_filter_state()
      context_revision(context_revision() + 1L)
    }

    observeEvent(
      input$term_id,
      {
        raw_term <- input$term_id
        if (is.null(raw_term)) return(invisible(NULL))
        term <- raw_term %||% NULL
        if (
          !is.null(term) &&
            (!is.character(term) || length(term) != 1L || !nzchar(term))
        ) {
          term <- NULL
        }
        inspection <- isolate(inspection_value())
        modality <- isolate(selected_modality_value())
        available_terms <- if (is.null(inspection)) {
          character()
        } else {
          glc_terms_for_modality(inspection, modality)$term
        }
        if (
          is.null(inspection) ||
            (!is.null(term) && !term %in% available_terms)
        ) {
          term <- NULL
        }
        if (!identical(term, selected_term_value())) {
          selected_term_value(term)
          reset_planning_selection()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$modality_id,
      {
        raw_modality <- input$modality_id
        if (is.null(raw_modality)) return(invisible(NULL))
        modality <- raw_modality %||% NULL
        if (
          !is.null(modality) &&
            (!is.character(modality) ||
              length(modality) != 1L ||
              !nzchar(modality))
        ) {
          modality <- NULL
        }
        inspection <- isolate(inspection_value())
        available <- if (is.null(inspection)) {
          character()
        } else {
          glc_modality_catalogue(inspection$files)$modality
        }
        if (!is.null(modality) && !modality %in% available) modality <- NULL
        if (!identical(modality, selected_modality_value())) {
          selected_modality_value(modality)
          available_terms <- if (is.null(inspection)) {
            character()
          } else {
            glc_terms_for_modality(inspection, modality)$term
          }
          if (
            !is.null(selected_term_value()) &&
              !selected_term_value() %in% available_terms
          ) {
            selected_term_value(NULL)
            updateSelectInput(session, "term_id", selected = "")
          }
          reset_planning_selection()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$term_search,
      {
        inspection <- isolate(inspection_value())
        if (is.null(inspection)) return(invisible(NULL))
        available <- glc_filter_terms(
          glc_terms_for_modality(
            inspection,
            isolate(selected_modality_value())
          ),
          input$term_search %||% ""
        )$term
        term <- isolate(selected_term_value())
        if (!is.null(term) && !term %in% available) {
          selected_term_value(NULL)
          reset_planning_selection()
        }
        invisible(NULL)
      },
      ignoreInit = TRUE
    )

    submit_pending_planning <- function() {
      attempt <- isolate(pending_planning_attempt())
      if (is.null(attempt)) return(invisible(FALSE))
      if (
        !glc_planning_attempt_is_current(
          attempt,
          isolate(context_revision()),
          glc_source_selection_key(isolate(source_selection())),
          isolate(selected_term_value()),
          isolate(selected_modality_value())
        )
      ) {
        pending_planning_attempt(NULL)
        return(invisible(FALSE))
      }

      if (glc_inspection_attempt_expired(attempt)) {
        pending_planning_attempt(NULL)
        isolate(current_planning_task())$cancel()
        planning_pending_error(new_llw_error(
          message = paste(
            "The previous planning controller did not release before the",
            "new invocation deadline."
          ),
          type = "resource",
          public_message = paste(
            "The previous compatibility planning task is still stopping.",
            "Wait a moment, then retry."
          )
        ))
        return(invisible(FALSE))
      }

      task <- isolate(current_planning_task())
      released <- glc_inspection_controller_released(
        isolate(task$state()),
        isolate(task$extended_task$status()),
        runtime$active_count()
      )
      if (!released) return(invisible(FALSE))

      if (
        !identical(isolate(task$state()), "idle") ||
          isTRUE(isolate(planning_controller_reset_required()))
      ) {
        task <- replace_planning_task()
      }
      task_id <- task$invoke(
        attempt$payload,
        dataset_id = "glc-plan-context"
      )
      if (isFALSE(task_id)) return(invisible(FALSE))
      submitted_planning_request(attempt$request)
      plan_progress_path(attempt$progress_path)
      pending_planning_attempt(NULL)
      planning_terminal_override(NULL)
      planning_controller_reset_required(FALSE)
      invisible(task_id)
    }

    start_planning <- function() {
      planning_attempted(TRUE)
      planning_pending_error(NULL)
      plan_value(NULL)
      submitted_planning_request(NULL)
      plan_progress_path(NULL)
      selected_compatibility_value(NULL)
      reset_metadata_filter_state()
      attempted <- tryCatch(
        {
          inspection <- isolate(inspection_value())
          if (is.null(inspection)) {
            abort_llw(
              "Inspect the exact package before planning collectable units.",
              type = "validation"
            )
          }
          term <- isolate(selected_term_value())
          modality <- isolate(selected_modality_value())
          available_terms <- glc_terms_for_modality(
            inspection,
            modality
          )$term
          if (
            !is.null(term) &&
              !term %in% available_terms
          ) {
            abort_llw(
              paste(
                "The selected canonical term is not available for the selected",
                "modality."
              ),
              type = "validation",
              public_message = paste(
                "Choose a canonical data term available for the selected",
                "modality."
              )
            )
          }
          modality_groups <- if (is.null(modality)) {
            character()
          } else {
            glc_modality_file_groups(inspection$files, modality)
          }
          request <- new_glc_planning_request(
            term = term,
            modality = modality,
            file_group_ids = modality_groups
          )
          source <- glc_inspection_source_spec(inspection, cache_dir)
          progress_path <- file.path(
            runtime$paths$tasks,
            paste0(new_stable_id("glc_plan_progress"), ".rds")
          )
          payload <- new_glcdp_planning_payload(source, request, progress_path)
          pending_planning_attempt(list(
            payload = payload,
            request = request,
            progress_path = progress_path,
            context_revision = isolate(context_revision()),
            source_key = glc_source_selection_key(isolate(source_selection())),
            requested_at = Sys.time()
          ))
          submit_pending_planning()
        },
        error = identity
      )
      if (inherits(attempted, "error")) {
        pending_planning_attempt(NULL)
        planning_pending_error(attempted)
      }
      invisible(attempted)
    }

    observe({
      req(!is.null(pending_planning_attempt()))
      invalidateLater(100, session)
      isolate(submit_pending_planning())
    })

    observeEvent(
      input$plan,
      {
        start_planning()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$cancel_plan,
      {
        had_pending <- !is.null(isolate(pending_planning_attempt()))
        pending_planning_attempt(NULL)
        planning_pending_error(NULL)
        cancelled <- isolate(current_planning_task())$cancel()
        if (had_pending && isFALSE(cancelled)) {
          planning_controller_reset_required(TRUE)
          planning_terminal_override(new_task_status(
            "glc_discovery",
            state = "cancelled",
            message = "The queued compatibility planning request was cancelled."
          ))
        }
      },
      ignoreInit = TRUE
    )

    selected_compatibility_id <- reactive({
      plan <- plan_value()
      if (is.null(plan) || nrow(plan$compatibility_sets) == 0L) return(NULL)
      value <- input$compatibility_id %||% selected_compatibility_value()
      if (
        is.null(value) ||
          !value %in% as.character(plan$compatibility_sets$compatibility_id)
      ) {
        value <- glc_plan_compatibility_default(plan)
      }
      value
    })

    observeEvent(
      input$compatibility_id,
      {
        value <- selected_compatibility_id()
        if (!identical(value, selected_compatibility_value())) {
          selected_compatibility_value(value)
          reset_metadata_filter_state()
        }
      },
      ignoreInit = TRUE
    )

    current_metadata_catalogue <- reactive({
      plan <- plan_value()
      compatibility_id <- selected_compatibility_id()
      if (is.null(plan) || is.null(compatibility_id)) return(NULL)
      glc_metadata_filter_catalogue(plan, compatibility_id)
    })

    current_characteristic_name <- reactive({
      catalogue <- current_metadata_catalogue()
      if (is.null(catalogue)) return("")
      input[[glc_metadata_filter_input_id(
        "filter_characteristic_name",
        metadata_filter_revision()
      )]] %||%
        ""
    })

    current_metadata_filters <- reactive({
      catalogue <- current_metadata_catalogue()
      if (is.null(catalogue)) return(list())
      glc_metadata_filters_from_input(
        input,
        catalogue,
        metadata_filter_revision()
      )
    })

    filtered_group_ids <- reactive({
      plan <- plan_value()
      compatibility_id <- selected_compatibility_id()
      if (is.null(plan) || is.null(compatibility_id)) return(character())
      glc_filter_compatibility_groups(
        plan,
        compatibility_id,
        current_metadata_filters()
      )
    })

    observe({
      selected_compatibility_id()
      current_metadata_filters()
      refinement_value(NULL)
      refinement_error(NULL)
    })

    observeEvent(
      input$refine,
      {
        result <- tryCatch(
          {
            plan <- plan_value()
            compatibility_id <- selected_compatibility_id()
            file_group_ids <- filtered_group_ids()
            if (is.null(plan) || is.null(compatibility_id)) {
              abort_llw(
                "Choose one compatible structure before refinement.",
                type = "validation"
              )
            }
            if (length(file_group_ids) == 0L) {
              abort_llw(
                "The current metadata filters retain no file groups.",
                type = "validation",
                public_message = paste(
                  "No file groups match every active metadata filter.",
                  "Adjust the filters before confirming."
                )
              )
            }
            withProgress(
              message = "Confirming filtered groups",
              detail = "Running glcdp's zero-access refinement.",
              value = 0.5,
              refine_collection(plan, compatibility_id, file_group_ids)
            )
          },
          error = identity
        )
        if (inherits(result, "error")) {
          refinement_value(NULL)
          refinement_error(llw_public_message(result))
        } else {
          refinement_error(NULL)
          refinement_value(result)
        }
      },
      ignoreInit = TRUE
    )

    output$wizard_stepper <- renderUI({
      selection <- source_selection()
      glc_wizard_stepper(
        session$ns,
        active = wizard_step(),
        source_ready = !is.null(selection) && isTRUE(selection$has_latest_pass),
        inspection_ready = !is.null(inspection_value()),
        plan_ready = !is.null(plan_value()),
        structure_ready = !is.null(selected_compatibility_id())
      )
    })

    observe(select_wizard_step("source")) |>
      bindEvent(input$wizard_step_source, ignoreInit = TRUE)

    observe({
      selection <- source_selection()
      if (is.null(selection) || !isTRUE(selection$has_latest_pass)) {
        showNotification(
          "Choose a source with an exact passing revision first.",
          type = "warning",
          duration = 5
        )
      } else {
        select_wizard_step("inspect")
        if (
          is.null(inspection_value()) &&
            is.null(pending_inspection_attempt()) &&
            !inspection_task_state() %in% c("queued", "running", "finalizing")
        ) {
          start_inspection()
        }
      }
    }) |>
      bindEvent(input$wizard_step_inspect, ignoreInit = TRUE)

    observe({
      if (is.null(inspection_value())) {
        showNotification(
          "Inspect the package before opening its inventory.",
          type = "warning",
          duration = 5
        )
      } else {
        select_wizard_step("discovery")
      }
    }) |>
      bindEvent(input$wizard_step_discovery, ignoreInit = TRUE)

    observe({
      if (
        is.null(plan_value()) || nrow(plan_value()$compatibility_sets) == 0L
      ) {
        showNotification(
          "Collect compatible groups before choosing a structure.",
          type = "warning",
          duration = 5
        )
      } else {
        select_wizard_step("structure")
      }
    }) |>
      bindEvent(input$wizard_step_structure, ignoreInit = TRUE)

    observe({
      if (is.null(selected_compatibility_id())) {
        showNotification(
          "Choose one compatible structure before metadata narrowing.",
          type = "warning",
          duration = 5
        )
      } else {
        select_wizard_step("narrow")
      }
    }) |>
      bindEvent(input$wizard_step_narrow, ignoreInit = TRUE)

    observe({
      select_wizard_step("inspect")
      if (
        is.null(inspection_value()) &&
          is.null(pending_inspection_attempt()) &&
          !inspection_task_state() %in% c("queued", "running", "finalizing")
      ) {
        start_inspection()
      }
    }) |>
      bindEvent(input$continue_inspect, ignoreInit = TRUE)
    observe(select_wizard_step("source")) |>
      bindEvent(input$back_source, ignoreInit = TRUE)
    observe(select_wizard_step("discovery")) |>
      bindEvent(input$continue_discovery, ignoreInit = TRUE)
    observe(select_wizard_step("inspect")) |>
      bindEvent(input$back_inspect, ignoreInit = TRUE)
    observe(select_wizard_step("structure")) |>
      bindEvent(input$continue_structure, ignoreInit = TRUE)
    observe(select_wizard_step("narrow")) |>
      bindEvent(input$continue_narrow, ignoreInit = TRUE)
    observe(select_wizard_step("discovery")) |>
      bindEvent(input$back_discovery, ignoreInit = TRUE)
    observe(select_wizard_step("structure")) |>
      bindEvent(input$back_structure, ignoreInit = TRUE)

    output$continue_inspect_button <- renderUI({
      selection <- source_selection()
      ready <- !is.null(selection) && isTRUE(selection$has_latest_pass)
      button <- actionButton(
        session$ns("continue_inspect"),
        "Continue and inspect package",
        icon = icon("arrow-right"),
        class = "btn-primary btn-lg",
        title = if (ready) {
          paste(
            "Inspect",
            selection$repository,
            "at exact revision",
            selection$resolved_revision
          )
        } else {
          "Choose a source with an exact passing revision first"
        }
      )
      if (!ready) {
        button <- htmltools::tagAppendAttributes(
          button,
          disabled = "disabled",
          "aria-disabled" = "true"
        )
      }
      button
    })

    output$cancel_inspection_button <- renderUI({
      glc_cancel_task_button(
        session$ns,
        "cancel_inspection",
        !is.null(pending_inspection_attempt()) ||
          inspection_task_state() %in% c("queued", "running", "finalizing"),
        "Cancel inspection"
      )
    })

    output$inspection_progress <- renderUI({
      status <- inspection_display_status()
      state <- status$state
      pending <- !is.null(pending_inspection_attempt())
      active <- pending || state %in% c("queued", "running", "finalizing")
      if (active) {
        invalidateLater(250, session)
      }
      progress <- glc_task_progress_ui(
        read_glc_task_progress(inspection_progress_path()),
        total_phases = 6L,
        heading = "Package inspection",
        idle_message = paste(
          "Start inspection to see each descriptive phase.",
          "Measurement files remain unopened."
        ),
        active = active,
        active_label = "Preparing exact-revision inspection",
        active_detail = paste(
          "Starting the background worker and opening the requested immutable",
          "package revision."
        ),
        terminal_state = if (pending) "queued" else state,
        terminal_message = if (pending) {
          "Waiting for the previous inspection worker to stop cleanly."
        } else {
          status$message %||% NULL
        }
      )
      tagList(
        progress,
        if (
          inspection_attempted() &&
            state %in%
              c(
                "error",
                "stale"
              )
        ) {
          tags$div(
            class = "mt-3",
            glc_task_status_tag(
              status,
              idle_heading = "Inspection has not started",
              idle_message = "Use Inspect package to start.",
              task_label = "Inspection"
            )
          )
        }
      )
    })

    output$continue_discovery_button <- renderUI({
      ready <- !is.null(inspection_value())
      button <- actionButton(
        session$ns("continue_discovery"),
        "Continue to discovery",
        icon = icon("arrow-right"),
        class = "btn-primary btn-lg",
        title = if (ready) {
          "Review the inspected package overview and choose a discovery scope"
        } else {
          "Complete package inspection first"
        }
      )
      if (!ready) {
        button <- htmltools::tagAppendAttributes(
          button,
          disabled = "disabled",
          "aria-disabled" = "true"
        )
      }
      button
    })

    output$inventory_view <- renderUI({
      inspection <- inspection_value()
      if (is.null(inspection)) return(NULL)
      card(
        class = "mt-3",
        card_header("Package overview", container = h3),
        tagList(
          glc_inventory_metrics_ui(inspection),
          glc_inventory_detail_ui(inspection)
        )
      )
    })

    output$term_selector <- renderUI({
      inspection <- inspection_value()
      if (is.null(inspection)) return(NULL)
      scoped_terms <- glc_terms_for_modality(
        inspection,
        selected_modality_value()
      )
      filtered <- glc_filter_terms(
        scoped_terms,
        input$term_search %||% ""
      )
      current <- isolate(selected_term_value())
      if (is.null(current) || !current %in% filtered$term) {
        current <- ""
      }
      selectInput(
        session$ns("term_id"),
        "Canonical data term",
        choices = c(
          "No term filter" = "",
          glc_term_choice_labels(filtered)
        ),
        selected = current,
        width = "100%",
        selectize = FALSE
      )
    })

    output$modality_selector <- renderUI({
      inspection <- inspection_value()
      if (is.null(inspection)) return(NULL)
      modalities <- glc_modality_catalogue(inspection$files)
      if (nrow(modalities) == 0L) {
        return(llw_status_callout(
          "idle",
          "This package provides no modality values. Choose a canonical term.",
          heading = "No modalities available",
          compact = TRUE
        ))
      }
      current <- isolate(selected_modality_value())
      if (is.null(current) || !current %in% modalities$modality) current <- ""
      selectInput(
        session$ns("modality_id"),
        "Modality",
        choices = c(
          "No modality filter" = "",
          glc_modality_choice_labels(modalities)
        ),
        selected = current,
        width = "100%",
        selectize = FALSE
      )
    })

    output$discovery_feedback <- renderUI({
      inspection <- inspection_value()
      if (is.null(inspection)) return(NULL)
      term <- selected_term_value()
      modality <- selected_modality_value()
      scoped_terms <- glc_terms_for_modality(inspection, modality)
      filtered_count <- nrow(glc_filter_terms(
        scoped_terms,
        input$term_search %||% ""
      ))
      scoped_count <- nrow(scoped_terms)
      package_count <- nrow(inspection$terms)
      message <- if (!is.null(term) && !is.null(modality)) {
        paste(
          "Collection will retain variables matching the selected canonical term",
          "within file groups using the selected modality."
        )
      } else if (!is.null(term)) {
        paste(
          "Collection will include all file groups using the selected",
          "canonical term."
        )
      } else if (!is.null(modality)) {
        paste(
          "Collection will compare all variables in stable file groups",
          "using the selected modality."
        )
      } else {
        "Choose at least one canonical term or modality before planning."
      }
      llw_status_callout(
        if (filtered_count == 0L) "warning" else "idle",
        paste(
          filtered_count,
          "of",
          scoped_count,
          "canonical terms match the current text filter",
          if (!is.null(modality)) {
            paste0(
              "within modality ",
              modality,
              "; ",
              package_count,
              " terms exist in the package."
            )
          } else {
            "within the package."
          },
          if (filtered_count == 0L) {
            "Clear or revise the filter to choose a term."
          } else {
            message
          }
        ),
        heading = "Discovery scope",
        compact = TRUE
      )
    })

    output$term_detail <- renderUI({
      inspection <- inspection_value()
      term <- selected_term_value()
      scoped_terms <- if (is.null(inspection)) {
        NULL
      } else {
        glc_terms_for_modality(inspection, selected_modality_value())
      }
      if (
        is.null(inspection) || is.null(term) || !term %in% scoped_terms$term
      ) {
        return(NULL)
      }
      row <- scoped_terms[scoped_terms$term == term, , drop = FALSE]
      tags$div(
        class = "small llw-secondary",
        tags$strong(row$label[[1L]]),
        tags$p(
          class = "mb-0",
          tags$strong("Canonical identifier: "),
          tags$code(row$term[[1L]])
        ),
        tags$p(
          class = "mb-0",
          tags$strong("Coverage: "),
          paste(
            row$dataset_count[[1L]],
            "dataset(s),",
            row$file_group_count[[1L]],
            "file group(s),",
            row$variable_count[[1L]],
            "variable occurrence(s)"
          )
        ),
        tags$p(
          class = "mb-0",
          tags$strong("Source variables: "),
          glc_compact_display_value(row$source_names[[1L]])
        ),
        tags$p(
          class = "mb-0",
          tags$strong("Descriptions: "),
          glc_compact_display_value(row$descriptions[[1L]])
        )
      )
    })

    output$cancel_plan_button <- renderUI({
      glc_cancel_task_button(
        session$ns,
        "cancel_plan",
        !is.null(pending_planning_attempt()) ||
          planning_task_state() %in% c("queued", "running", "finalizing"),
        "Cancel planning"
      )
    })

    output$plan_progress <- renderUI({
      status <- planning_display_status()
      state <- status$state
      pending <- !is.null(pending_planning_attempt())
      active <- pending || state %in% c("queued", "running", "finalizing")
      if (active) {
        invalidateLater(250, session)
      }
      progress <- glc_task_progress_ui(
        read_glc_task_progress(plan_progress_path()),
        total_phases = 3L,
        heading = "Structural compatibility planning",
        idle_message = paste(
          "Choose a canonical term, a modality, or both, then start planning.",
          "Measurement files remain untouched."
        ),
        active = active,
        active_label = "Preparing structural planning",
        active_detail = paste(
          "Starting the background worker and validating the selected term",
          "and modality scope."
        ),
        terminal_state = if (pending) "queued" else state,
        terminal_message = if (pending) {
          "Waiting for the previous planning worker to stop cleanly."
        } else {
          status$message %||% NULL
        }
      )
      tagList(
        progress,
        if (
          planning_attempted() &&
            state %in%
              c(
                "error",
                "stale"
              )
        ) {
          tags$div(
            class = "mt-3",
            glc_task_status_tag(
              status,
              idle_heading = "Planning has not started",
              idle_message = "Use Collect compatible groups to start.",
              task_label = "Compatibility planning"
            )
          )
        }
      )
    })

    output$continue_structure_button <- renderUI({
      ready <- !is.null(plan_value()) &&
        nrow(plan_value()$compatibility_sets) > 0L
      button <- actionButton(
        session$ns("continue_structure"),
        "Review compatible structures",
        icon = icon("arrow-right"),
        class = "btn-primary btn-lg",
        title = if (ready) {
          "Review the structures supplied by glcdp"
        } else {
          "Collect at least one compatible structure first"
        }
      )
      if (!ready) {
        button <- htmltools::tagAppendAttributes(
          button,
          disabled = "disabled",
          "aria-disabled" = "true"
        )
      }
      button
    })

    output$continue_narrow_button <- renderUI({
      ready <- !is.null(selected_compatibility_id())
      button <- actionButton(
        session$ns("continue_narrow"),
        "Continue to metadata narrowing",
        icon = icon("arrow-right"),
        class = "btn-primary btn-lg",
        title = if (ready) {
          "Narrow the selected structure with participant and device metadata"
        } else {
          "Choose one compatible structure first"
        }
      )
      if (!ready) {
        button <- htmltools::tagAppendAttributes(
          button,
          disabled = "disabled",
          "aria-disabled" = "true"
        )
      }
      button
    })

    output$plan_summary <- renderUI({
      plan <- plan_value()
      if (is.null(plan)) return(NULL)
      request <- submitted_planning_request()
      glc_compatibility_summary_ui(plan, modality = request$modality %||% NULL)
    })

    output$compatibility_selector <- renderUI({
      plan <- plan_value()
      if (is.null(plan) || nrow(plan$compatibility_sets) == 0L) {
        return(llw_status_callout(
          "warning",
          "The selected term and modality scope has no compatible structure.",
          heading = "No compatible groups",
          compact = TRUE
        ))
      }
      compatibility_ids <- as.character(
        plan$compatibility_sets$compatibility_id
      )
      default <- glc_plan_compatibility_default(plan)
      choice_names <- lapply(seq_along(compatibility_ids), function(index) {
        glc_compatibility_choice_tag(
          glc_compatibility_context(plan, compatibility_ids[[index]]),
          index,
          broadest = identical(compatibility_ids[[index]], default)
        )
      })
      tags$div(
        class = "llw-glc-unit-selector",
        radioButtons(
          session$ns("compatibility_id"),
          "Compatible structures",
          choiceNames = choice_names,
          choiceValues = compatibility_ids,
          selected = selected_compatibility_id(),
          width = "100%"
        )
      )
    })

    output$selected_structure_status <- renderUI({
      plan <- plan_value()
      compatibility_id <- selected_compatibility_id()
      if (is.null(plan) || is.null(compatibility_id)) return(NULL)
      index <- match(
        compatibility_id,
        as.character(plan$compatibility_sets$compatibility_id)
      )
      tags$p(
        class = "llw-glc-selected-option-status",
        role = "status",
        "aria-live" = "polite",
        bsicons::bs_icon("check2-circle"),
        tags$strong(paste("Structure", index, "selected"))
      )
    })

    output$structure_detail <- renderUI({
      plan <- plan_value()
      compatibility_id <- selected_compatibility_id()
      if (is.null(plan) || is.null(compatibility_id)) return(NULL)
      glc_aggregated_structure_detail_ui(
        glc_compatibility_context(plan, compatibility_id)
      )
    })

    output$metadata_filters <- renderUI({
      catalogue <- current_metadata_catalogue()
      if (is.null(catalogue)) return(NULL)
      glc_metadata_filter_inputs(
        session$ns,
        catalogue,
        metadata_filter_revision()
      )
    })

    output$characteristic_value_filter <- renderUI({
      catalogue <- current_metadata_catalogue()
      if (is.null(catalogue)) return(NULL)
      revision <- metadata_filter_revision()
      selected <- isolate(input[[glc_metadata_filter_input_id(
        "filter_characteristic_values",
        revision
      )]])
      glc_characteristic_value_input(
        session$ns,
        catalogue,
        current_characteristic_name(),
        revision,
        selected = selected
      )
    })

    output$filter_feedback <- renderUI({
      plan <- plan_value()
      compatibility_id <- selected_compatibility_id()
      if (is.null(plan) || is.null(compatibility_id)) return(NULL)
      total <- nrow(glc_compatibility_groups(plan, compatibility_id))
      retained <- length(filtered_group_ids())
      filters <- current_metadata_filters()
      active <- glc_active_metadata_filter_count(filters)
      llw_status_callout(
        if (retained == 0L) "warning" else "idle",
        paste(
          retained,
          "of",
          total,
          "structurally compatible file groups remain.",
          if (active == 0L) {
            "No metadata filter is active."
          } else {
            paste(
              active,
              if (active == 1L) "metadata filter is" else
                "metadata filters are",
              "active."
            )
          },
          if (retained == 0L) {
            "Adjust the filters before confirming."
          } else {
            "Confirm to let glcdp recompute the final unit membership."
          }
        ),
        heading = "Filter result",
        compact = TRUE
      )
    })

    output$refine_button <- renderUI({
      ready <- !is.null(selected_compatibility_id()) &&
        length(filtered_group_ids()) > 0L
      button <- actionButton(
        session$ns("refine"),
        "Confirm filtered groups",
        icon = icon("filter"),
        class = "btn-primary btn-lg",
        title = if (ready) {
          "Run glcdp's in-memory zero-access refinement"
        } else {
          "Retain at least one compatible file group first"
        }
      )
      if (!ready) {
        button <- htmltools::tagAppendAttributes(
          button,
          disabled = "disabled",
          "aria-disabled" = "true"
        )
      }
      button
    })

    output$refinement_summary <- renderUI({
      error <- refinement_error()
      if (!is.null(error)) {
        return(llw_status_callout(
          "danger",
          error,
          heading = "Refinement failed: retry is available",
          compact = TRUE
        ))
      }
      refinement <- refinement_value()
      if (is.null(refinement)) return(NULL)
      glc_refinement_summary_ui(refinement, plan_value())
    })

    observe({
      status <- inspection_display_status()
      req(identical(status$state, "error"))
      showNotification(
        status$message,
        type = "error",
        duration = 8
      )
    })

    observe({
      status <- planning_display_status()
      req(identical(status$state, "error"))
      showNotification(
        status$message,
        type = "error",
        duration = 8
      )
    }) |>
      bindEvent(planning_display_status()$state, ignoreInit = TRUE)

    list(
      inspection = reactive(inspection_value()),
      inspection_status = reactive({
        if (inspection_attempted()) {
          inspection_display_status()
        } else {
          new_task_status("glc_discovery")
        }
      }),
      inspection_controller_revision = reactive(
        inspection_controller_revision()
      ),
      inspection_pending = reactive(!is.null(pending_inspection_attempt())),
      selected_term = reactive(selected_term_value()),
      selected_modality = reactive(selected_modality_value()),
      planning_request = reactive(submitted_planning_request()),
      plan = reactive(plan_value()),
      plan_status = reactive({
        if (planning_attempted()) {
          planning_display_status()
        } else {
          new_task_status("glc_discovery")
        }
      }),
      planning_controller_revision = reactive(
        planning_controller_revision()
      ),
      planning_pending = reactive(!is.null(pending_planning_attempt())),
      selected_compatibility_id = selected_compatibility_id,
      metadata_filter_revision = reactive(metadata_filter_revision()),
      metadata_filters = current_metadata_filters,
      active_metadata_filter_count = reactive(
        glc_active_metadata_filter_count(current_metadata_filters())
      ),
      filtered_file_group_ids = filtered_group_ids,
      refinement = reactive(refinement_value())
    )
  })
}
