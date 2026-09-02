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

glc_format_bytes <- function(bytes, unknown = "---") {
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
  if (length(value) == 0L || all(is.na(value))) return("---")
  value <- as.character(value[!is.na(value)])
  value <- value[nzchar(value)]
  if (length(value) == 0L) return("---")
  value[tolower(trimws(value)) == "unknown"] <- "---"
  paste(unique(value), collapse = ", ")
}

glc_transfer_display_row_limit <- function() 6L

glc_format_phase_elapsed <- function(seconds) {
  seconds <- suppressWarnings(as.numeric(seconds))
  if (length(seconds) != 1L || !is.finite(seconds) || seconds < 0) {
    return(NULL)
  }
  if (seconds < 60) {
    return(paste0(format(round(seconds, 2), nsmall = 2), " s"))
  }
  minutes <- floor(seconds / 60)
  remaining <- round(seconds - minutes * 60)
  paste0(minutes, " min ", remaining, " s")
}

glc_full_import_progress_for_display <- function(
  progress,
  review,
  progress_path,
  now = Sys.time()
) {
  if (
    is.null(progress) ||
      !is.data.frame(progress) ||
      nrow(progress) == 0L ||
      !all(c("phase", "status", "detail") %in% names(progress)) ||
      is.null(review) ||
      !inherits(review, "llw_glc_transfer_review")
  ) {
    return(progress)
  }
  running_read <- progress$phase == "read" & progress$status == "running"
  if (!any(running_read)) return(progress)
  modified <- tryCatch(
    file.info(progress_path)$mtime[[1L]],
    error = function(cnd) as.POSIXct(NA)
  )
  elapsed <- if (length(modified) == 1L && !is.na(modified)) {
    as.numeric(difftime(now, modified, units = "secs"))
  } else {
    NA_real_
  }
  elapsed_label <- glc_format_phase_elapsed(elapsed)
  attr(progress, "llw_elapsed_label") <- elapsed_label
  progress$detail[running_read] <- paste(
    "Processing",
    glc_pluralized_count(nrow(review$files), "selected file"),
    "as one validated batch.",
    paste(
      "The public read interface reports this batch at completion rather",
      "than exposing a reliable per-file count while it runs."
    )
  )
  progress
}

glc_full_import_elapsed_ui <- function(elapsed_label) {
  if (
    is.null(elapsed_label) ||
      length(elapsed_label) != 1L ||
      is.na(elapsed_label) ||
      !nzchar(elapsed_label)
  ) {
    return(NULL)
  }
  tags$div(
    class = "llw-glc-import-elapsed",
    role = "timer",
    `aria-label` = paste("Elapsed transfer time", elapsed_label),
    tags$span(
      class = "llw-glc-import-elapsed__label",
      "Elapsed transfer time"
    ),
    tags$strong(
      class = "llw-glc-import-elapsed__value",
      elapsed_label
    )
  )
}

glc_task_timing_ui <- function(progress) {
  if (
    is.null(progress) ||
      !is.data.frame(progress) ||
      nrow(progress) == 0L ||
      !all(c("label", "status", "elapsed_seconds") %in% names(progress))
  ) {
    return(NULL)
  }
  complete <- progress[
    progress$status == "complete" & is.finite(progress$elapsed_seconds),
    ,
    drop = FALSE
  ]
  if (nrow(complete) == 0L) return(NULL)
  tags$details(
    class = "llw-glc-card-disclosure mt-3",
    tags$summary("Process timing"),
    tags$ul(
      class = "mt-2 mb-0",
      lapply(seq_len(nrow(complete)), function(index) {
        tags$li(
          tags$strong(complete$label[[index]]),
          ": ",
          glc_format_phase_elapsed(complete$elapsed_seconds[[index]])
        )
      })
    )
  )
}

glc_wizard_steps <- function() {
  data.frame(
    value = c(
      "source",
      "inspect",
      "discovery",
      "structure",
      "narrow",
      "variables",
      "preview"
    ),
    label = c(
      "Source",
      "Inspect",
      "Discovery",
      "Selection",
      "Filtering",
      "Variables",
      "Import"
    ),
    icon = c(
      "cloud-arrow-down",
      "box-arrow-in-down",
      "search",
      "collection",
      "filter",
      "columns-gap",
      "table"
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
  structure_ready = FALSE,
  refinement_ready = FALSE,
  scope_ready = FALSE
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
    narrow = isTRUE(structure_ready),
    variables = isTRUE(refinement_ready),
    preview = isTRUE(scope_ready)
  )
  completed <- c(
    if (isTRUE(source_ready)) "source",
    if (isTRUE(inspection_ready)) "inspect",
    if (isTRUE(plan_ready)) "discovery",
    if (isTRUE(structure_ready)) "structure",
    if (isTRUE(refinement_ready)) "narrow",
    if (isTRUE(scope_ready)) "variables"
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
              complete = {
                elapsed <- glc_format_phase_elapsed(row$elapsed_seconds[[1L]])
                if (is.null(elapsed)) "Complete" else paste("Complete", elapsed)
              },
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

glc_planning_progress_for_display <- function(progress) {
  if (is.null(progress) || !is.data.frame(progress) || nrow(progress) == 0L) {
    return(progress)
  }
  required <- c("phase", "detail")
  if (!all(required %in% names(progress))) return(progress)
  planning_phase <- progress$phase == "plan"
  if (any(planning_phase)) {
    progress$detail[planning_phase] <- paste(
      "The term and modality identify candidate file groups.",
      "glcdp validates each candidate's complete file schema to establish",
      "groups that can be read and collected together."
    )
  }
  progress
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

glc_scope_variable_catalogue <- function(inspection, file_group_ids) {
  inspection <- validate_glc_inspection(inspection)
  file_group_ids <- glc_nonempty_unique(file_group_ids)
  if (length(file_group_ids) == 0L) {
    return(data.frame(
      name = character(),
      display = character(),
      group_count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  variables <- inspection$variables[
    inspection$variables$file_group_id %in% file_group_ids,
    ,
    drop = FALSE
  ]
  names <- sort(glc_nonempty_unique(variables$name), method = "radix")
  if (length(names) == 0L) {
    return(data.frame(
      name = character(),
      display = character(),
      group_count = integer(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(names, function(name) {
    current <- variables[variables$name == name, , drop = FALSE]
    labels <- glc_nonempty_unique(current$label)
    descriptions <- glc_nonempty_unique(current$description)
    units <- glc_nonempty_unique(current$unit)
    primary <- if (length(labels) == 1L && !identical(labels, name)) {
      labels
    } else {
      name
    }
    detail <- c(
      paste0("source: ", name),
      if (length(units) > 0L) paste0("unit: ", paste(units, collapse = ", ")),
      if (length(descriptions) > 0L) descriptions[[1L]]
    )
    data.frame(
      name = name,
      display = paste0(primary, " | ", paste(detail, collapse = " | ")),
      group_count = length(glc_nonempty_unique(current$file_group_id)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

glc_scope_mode_choices <- function(parent_request) {
  parent_request <- validate_glc_planning_request(parent_request)
  names <- list()
  values <- character()
  if (!is.null(parent_request$term)) {
    names <- c(
      names,
      list(tags$span(
        tags$strong("Variables matching the discovery term"),
        tags$small(
          class = "d-block llw-secondary",
          paste(
            "Project the exact source variables associated with the selected",
            "canonical term. The validated structure stays fixed."
          )
        )
      ))
    )
    values <- c(values, "discovery")
  }
  names <- c(
    names,
    list(tags$span(
      tags$strong("All variables in the selected unit"),
      tags$small(
        class = "d-block llw-secondary",
        paste(
          "Retain every source variable from the already validated",
          "collectable unit."
        )
      )
    )),
    list(tags$span(
      tags$strong("Custom variables"),
      tags$small(
        class = "d-block llw-secondary",
        paste(
          "Choose exact source variables for the output. This does not",
          "change file groups, structures, or stable identifiers."
        )
      )
    ))
  )
  values <- c(values, "all", "custom")
  list(names = names, values = values)
}

glc_scope_review_ui <- function(review) {
  if (!inherits(review, "llw_glc_projection_review") || !is.list(review)) {
    abort_llw("The variable projection review is invalid.", type = "validation")
  }
  groups <- review$selected_groups
  output_names <- review$selected_source_variables
  automatic_rows <- review$read_columns$automatic %in% TRUE
  automatic_names <- glc_nonempty_unique(
    review$read_columns$name[automatic_rows]
  )
  automatic_output_names <- glc_nonempty_unique(
    review$output_columns$name[review$output_columns$automatic %in% TRUE]
  )
  bytes <- review$byte_evidence
  all_sizes_unknown <- nrow(review$files) > 0L &&
    isTRUE(as.integer(bytes$known_file_count) == 0L) &&
    isTRUE(as.integer(bytes$unknown_file_count) == nrow(review$files))
  evidence_metric <- if (all_sizes_unknown) {
    glc_metric_with_help(
      "columns-gap",
      "Included variables",
      format(length(output_names), big.mark = ","),
      "Selected for the output",
      paste(
        "Exact source variables selected for the later result. No retained",
        "file has a package-listed size, so an uninformative size total is",
        "not shown."
      )
    )
  } else {
    byte_value <- if (isTRUE(bytes$declared_bytes_complete)) {
      glc_format_bytes(bytes$declared_total_bytes)
    } else {
      paste0(
        glc_format_bytes(bytes$known_declared_bytes),
        " known; ",
        bytes$unknown_file_count,
        " unknown"
      )
    }
    glc_metric_with_help(
      "device-hdd",
      "Listed size",
      byte_value,
      if (isTRUE(bytes$declared_bytes_complete)) "Complete" else
        "Partly unknown",
      paste(
        "These are package-listed file sizes. Any unknown size remains",
        "unknown and is not treated as zero."
      )
    )
  }
  tagList(
    llw_status_callout(
      "complete",
      paste(
        "The output projection keeps the exact validated Slice 2A structure,",
        "unit, and file groups. No package was reopened and compatibility was",
        "not replanned."
      ),
      heading = "Output variables are ready for review",
      compact = TRUE
    ),
    tags$div(
      class = "llw-glc-metrics llw-glc-scope-metrics",
      layout_column_wrap(
        width = 1 / 4,
        heights_equal = "row",
        fillable = FALSE,
        glc_metric_with_help(
          "folder-check",
          "File groups",
          format(nrow(groups), big.mark = ","),
          "Fixed by Slice 2A",
          "Stable file groups retained by the selected public planner structure."
        ),
        glc_metric_with_help(
          "database",
          "Datasets",
          format(
            length(glc_nonempty_unique(groups$dataset_id)),
            big.mark = ","
          ),
          "Represented in the unit",
          "Unique datasets represented by the retained file groups."
        ),
        glc_metric_with_help(
          "file-earmark",
          "Files",
          format(nrow(review$files), big.mark = ","),
          "Fixed transfer set",
          "Files associated with the retained collectable unit."
        ),
        evidence_metric
      )
    ),
    tags$section(
      class = "llw-wizard-field-group mt-3",
      tags$h4("Selected output variables"),
      glc_full_variable_list_tag(output_names, summary_items = 12L),
      tags$h4(class = "mt-3", "Automatic read inputs"),
      if (length(automatic_names) == 0L) {
        tags$p(
          class = "llw-secondary mb-0",
          "No additional identity, time, or key columns are required."
        )
      } else {
        tagList(
          tags$p(
            class = "llw-secondary",
            paste(
              "glcdp will also read these identity, time, or key columns to",
              "validate and assemble the requested output. They are not added",
              "to the output unless the public plan lists them there."
            )
          ),
          glc_full_variable_list_tag(automatic_names, summary_items = 12L)
        )
      },
      if (length(automatic_output_names) > 0L) {
        tagList(
          tags$h4(class = "mt-3", "Automatic result columns"),
          tags$p(
            class = "llw-secondary",
            paste(
              "glcdp reserves these standardized identity, time, or file",
              "columns for the later collected result."
            )
          ),
          glc_full_variable_list_tag(
            automatic_output_names,
            summary_items = 12L
          )
        )
      },
      if (nrow(review$diagnostics) > 0L) {
        tags$details(
          class = "llw-glc-card-disclosure mt-3",
          tags$summary("Compatibility and harmonization diagnostics"),
          glc_diagnostic_list_tag(review$diagnostics)
        )
      },
      tags$details(
        class = "llw-glc-card-disclosure mt-3",
        tags$summary("Technical output-projection provenance"),
        tags$dl(
          class = "row mt-2 mb-0",
          tags$dt(class = "col-sm-4", "Exact source"),
          tags$dd(
            class = "col-sm-8",
            paste0(
              review$source$repository,
              "@",
              review$source$resolved_revision
            )
          ),
          tags$dt(class = "col-sm-4", "Projection"),
          tags$dd(
            class = "col-sm-8",
            "Output columns only; compatibility was not replanned"
          ),
          tags$dt(class = "col-sm-4", "Compatibility ID"),
          tags$dd(class = "col-sm-8", review$selected_compatibility_id),
          tags$dt(class = "col-sm-4", "Final unit IDs"),
          tags$dd(
            class = "col-sm-8",
            paste(review$selected_unit_ids, collapse = ", ")
          )
        )
      )
    )
  )
}

glc_scope_review_result_ui <- function(content) {
  tags$div(
    class = "llw-glc-scope-review-result mt-3",
    content
  )
}

glc_transfer_column_list <- function(rows, empty_message) {
  if (!is.data.frame(rows) || !"name" %in% names(rows)) {
    abort_llw("The transfer column contract is invalid.", type = "validation")
  }
  names <- glc_nonempty_unique(rows$name)
  if (length(names) == 0L) {
    return(tags$p(class = "llw-secondary mb-0", empty_message))
  }
  glc_full_variable_list_tag(names, summary_items = 12L)
}

glc_transfer_review_ui <- function(review) {
  validate_glc_transfer_review(review)
  allocation <- review$allocation
  bytes <- review$byte_evidence
  selected_known <- as.integer(bytes$selected_known_file_count)
  selected_unknown <- as.integer(bytes$selected_unknown_file_count)
  known_bytes <- as.numeric(bytes$selected_known_declared_bytes)
  selected_size <- if (selected_known > 0L) {
    paste0(
      glc_format_bytes(known_bytes),
      " known; ",
      selected_unknown,
      " unknown"
    )
  } else {
    paste0(selected_unknown, " unknown")
  }
  allocated_size <- if (bytes$allocated_known_file_count > 0L) {
    paste0(
      glc_format_bytes(bytes$allocated_known_declared_bytes),
      " known; ",
      bytes$allocated_unknown_file_count,
      " unknown"
    )
  } else {
    paste0(bytes$allocated_unknown_file_count, " unknown")
  }
  preview_blocked <- isTRUE(review$resource$blocked)
  apply_blocked <- isTRUE(review$resource$full_import_blocked)
  status <- if (preview_blocked && apply_blocked) "error" else "idle"
  heading <- if (preview_blocked && apply_blocked) {
    "Measurement access is blocked by the session resource limit"
  } else {
    "No measurement transfer has started"
  }
  message <- if (preview_blocked && apply_blocked) {
    paste(
      "The known sizes of both the bounded preview subset and complete",
      "selected file set exceed the active session resource budget.",
      "Narrow the reviewed selection before measurement access."
    )
  } else {
    paste(
      "This review fixes the exact revision, unit, file membership, output",
      "projection, and bounded preview allocation. Only the explicit Preview",
      "or Apply actions below may reopen the package and access measurements."
    )
  }
  tagList(
    llw_status_callout(status, message, heading = heading, compact = TRUE),
    tags$div(
      class = "llw-glc-metrics llw-glc-transfer-metrics mt-3",
      layout_column_wrap(
        width = 1 / 4,
        heights_equal = "row",
        fillable = FALSE,
        glc_metric_with_help(
          "folder-check",
          "File groups",
          format(length(review$fixed$file_group_ids), big.mark = ","),
          "Fixed membership",
          "Exact stable file groups accepted in Slice 2A."
        ),
        glc_metric_with_help(
          "database",
          "Datasets",
          format(
            length(glc_nonempty_unique(review$selected_groups$dataset_id)),
            big.mark = ","
          ),
          "Represented in the unit",
          "Unique datasets represented by the fixed file groups."
        ),
        glc_metric_with_help(
          "file-earmark",
          "Selected files",
          format(nrow(review$files), big.mark = ","),
          "Fixed transfer set",
          "Every file associated with the accepted unit before preview allocation."
        ),
        glc_metric_with_help(
          "device-hdd",
          "Listed size",
          selected_size,
          if (selected_unknown > 0L) "Incomplete evidence" else "Complete",
          paste(
            "Known package-listed bytes plus the count of files whose size",
            "remains unknown. Unknown is never treated as zero."
          )
        ),
        glc_metric_with_help(
          "list-ol",
          "Preview-row budget",
          format(allocation$total_budget, big.mark = ","),
          "Across the subset",
          "Total maximum parsed rows allocated across canonical preview files."
        ),
        glc_metric_with_help(
          "files",
          "Preview-file subset",
          paste0(
            allocation$preview_file_count,
            " of ",
            allocation$available_file_count
          ),
          paste0(length(allocation$omitted_files), " omitted"),
          paste(
            "The canonical subset is ordered by dataset, stable file group,",
            "file position, and declared path."
          )
        ),
        glc_metric_with_help(
          "sort-numeric-down",
          "Rows per file",
          format(allocation$per_file_n_max, big.mark = ","),
          "Parsed-row limit",
          "The public glc_read n_max value applied independently to each preview file."
        ),
        glc_metric_with_help(
          "table",
          "Maximum rows",
          format(allocation$maximum_rows, big.mark = ","),
          paste("Problems:", review$problems_policy),
          "The maximum possible returned rows for this bounded allocation."
        )
      )
    ),
    if (
      isTRUE(review$resource$size_evidence_incomplete) ||
        isTRUE(review$resource$full_import_size_evidence_incomplete)
    ) {
      tags$div(
        class = "mt-3",
        llw_status_callout(
          "warning",
          paste(
            "At least one selected file has no package-listed size, so exact",
            "transfer bytes remain uncertain. Preview opens only its canonical",
            "subset. Apply may transfer every selected file. Continuing",
            "requires the corresponding explicit action, and runtime resource",
            "errors remain recoverable. The 10,000-row preview ceiling does",
            "not cap network bytes and does not apply to the full import."
          ),
          heading = "Transfer size evidence is incomplete",
          compact = TRUE
        )
      )
    },
    if (preview_blocked || apply_blocked) {
      tags$div(
        class = "mt-3",
        llw_status_callout(
          "error",
          paste(
            if (preview_blocked) {
              paste(
                "The bounded preview subset exceeds the active known-byte",
                "resource budget."
              )
            } else {
              "The bounded preview remains within its known-byte resource budget."
            },
            if (apply_blocked) {
              paste(
                "The complete selected file set exceeds the active known-byte",
                "resource budget, so Apply is disabled."
              )
            } else {
              "The complete selected set remains within its known-byte budget."
            }
          ),
          heading = "Resource gate for measurement access",
          compact = TRUE
        )
      )
    },
    tags$div(
      class = "mt-3",
      llw_status_callout(
        "warning",
        paste(
          "The per-file n_max value limits parsed rows returned from each",
          "file. It may not reduce network transfer for monolithic files or",
          "Git-LFS objects, which can require the complete object before",
          "bounded parsing begins."
        ),
        heading = "Parsed rows and transferred bytes are different limits",
        compact = TRUE
      )
    ),
    tags$section(
      class = "llw-wizard-field-group mt-3",
      tags$h4("Selected output variables"),
      glc_full_variable_list_tag(
        review$selected_source_variables,
        summary_items = 12L
      ),
      tags$h4(class = "mt-3", "Automatic read inputs"),
      glc_transfer_column_list(
        review$automatic_read_inputs,
        "No additional identity, time, or key columns are required."
      ),
      tags$h4(class = "mt-3", "Automatic result columns"),
      glc_transfer_column_list(
        review$automatic_result_columns,
        "No automatic result columns are required."
      ),
      tags$details(
        class = "llw-glc-card-disclosure mt-3",
        tags$summary("Canonical preview allocation and exact provenance"),
        tags$dl(
          class = "row mt-2 mb-0",
          tags$dt(class = "col-sm-4", "Exact source"),
          tags$dd(
            class = "col-sm-8",
            paste0(
              review$source$repository,
              "@",
              review$source$resolved_revision
            )
          ),
          tags$dt(class = "col-sm-4", "Compatibility ID"),
          tags$dd(class = "col-sm-8", review$fixed$compatibility_id),
          tags$dt(class = "col-sm-4", "Final unit ID"),
          tags$dd(class = "col-sm-8", review$fixed$unit_id),
          tags$dt(class = "col-sm-4", "Allocated files"),
          tags$dd(
            class = "col-sm-8",
            paste(review$allocation$preview_files, collapse = ", ")
          ),
          tags$dt(class = "col-sm-4", "Allocated listed size"),
          tags$dd(class = "col-sm-8", allocated_size),
          tags$dt(class = "col-sm-4", "Complete selected files"),
          tags$dd(
            class = "col-sm-8",
            paste(review$files$declared_path, collapse = ", ")
          ),
          tags$dt(class = "col-sm-4", "Complete listed size"),
          tags$dd(class = "col-sm-8", selected_size),
          tags$dt(class = "col-sm-4", "Problems policy"),
          tags$dd(
            class = "col-sm-8",
            if (identical(review$problems_policy, "error")) {
              "Error on parsing problems"
            } else {
              "Return warnings for parsing problems"
            }
          )
        )
      )
    )
  )
}

glc_transfer_review_size_labels <- function(review) {
  validate_glc_transfer_review(review)
  bytes <- review$byte_evidence
  selected_known <- as.integer(bytes$selected_known_file_count)
  selected_unknown <- as.integer(bytes$selected_unknown_file_count)
  selected_size <- if (selected_known > 0L) {
    paste0(
      glc_format_bytes(bytes$selected_known_declared_bytes),
      " known; ",
      selected_unknown,
      " unknown"
    )
  } else {
    paste0(selected_unknown, " unknown")
  }
  allocated_size <- if (bytes$allocated_known_file_count > 0L) {
    paste0(
      glc_format_bytes(bytes$allocated_known_declared_bytes),
      " known; ",
      bytes$allocated_unknown_file_count,
      " unknown"
    )
  } else {
    paste0(bytes$allocated_unknown_file_count, " unknown")
  }
  list(
    selected_unknown = selected_unknown,
    selected_size = selected_size,
    allocated_size = allocated_size
  )
}

glc_import_review_ui <- function(review) {
  validate_glc_transfer_review(review)
  sizes <- glc_transfer_review_size_labels(review)
  apply_blocked <- isTRUE(review$resource$full_import_blocked)
  file_group_count <- length(review$fixed$file_group_ids)
  dataset_count <- length(glc_nonempty_unique(
    review$selected_groups$dataset_id
  ))
  file_count <- nrow(review$files)
  known_size_count <- file_count - sizes$selected_unknown
  tagList(
    tags$div(
      class = "llw-glc-import-summary",
      tags$p(
        class = "mb-1",
        tags$strong(
          if (apply_blocked) "Import is blocked" else "Ready to import"
        ),
        ": ",
        paste(
          glc_pluralized_count(file_count, "selected file"),
          "from",
          glc_pluralized_count(dataset_count, "dataset"),
          "in",
          glc_pluralized_count(file_group_count, "file group"),
          "will be read only after you choose Apply."
        )
      ),
      if (known_size_count > 0L) {
        tags$p(
          class = "small llw-secondary mb-0",
          paste("Package-listed size:", sizes$selected_size)
        )
      } else {
        tags$p(
          class = "small llw-secondary mb-0",
          paste(
            "Transfer time depends on the actual size of all selected files;",
            "the package does not list their size."
          )
        )
      }
    ),
    if (apply_blocked) {
      llw_status_callout(
        "error",
        paste(
          "The known sizes of the complete selected file set exceed the",
          "active session resource budget. Narrow the selection before",
          "importing."
        ),
        heading = "Full import is blocked by the session resource limit",
        compact = TRUE
      )
    },
    tags$details(
      class = "llw-glc-card-disclosure llw-glc-import-details",
      tags$summary("Import columns and exact selection"),
      tags$div(
        class = "llw-glc-import-details__body",
        tags$h4("Selected output variables"),
        glc_full_variable_list_tag(
          review$selected_source_variables,
          summary_items = 12L
        ),
        tags$h4("Automatic read inputs"),
        glc_transfer_column_list(
          review$automatic_read_inputs,
          "No additional identity, time, or key columns are required."
        ),
        tags$h4("Automatic result columns"),
        glc_transfer_column_list(
          review$automatic_result_columns,
          "No automatic result columns are required."
        ),
        tags$dl(
          class = "row mb-0",
          tags$dt(class = "col-sm-4", "Exact source"),
          tags$dd(
            class = "col-sm-8",
            paste0(
              review$source$repository,
              "@",
              review$source$resolved_revision
            )
          ),
          tags$dt(class = "col-sm-4", "Compatibility ID"),
          tags$dd(class = "col-sm-8", review$fixed$compatibility_id),
          tags$dt(class = "col-sm-4", "Final unit ID"),
          tags$dd(class = "col-sm-8", review$fixed$unit_id),
          tags$dt(class = "col-sm-4", "Complete selected files"),
          tags$dd(
            class = "col-sm-8",
            paste(review$files$declared_path, collapse = ", ")
          ),
          tags$dt(class = "col-sm-4", "Complete listed size"),
          tags$dd(class = "col-sm-8", sizes$selected_size)
        )
      )
    )
  )
}

glc_preview_review_ui <- function(review) {
  validate_glc_transfer_review(review)
  allocation <- review$allocation
  sizes <- glc_transfer_review_size_labels(review)
  preview_blocked <- isTRUE(review$resource$blocked)
  tags$div(
    class = "llw-glc-preview-review",
    tags$div(
      class = "llw-glc-metrics llw-glc-transfer-metrics",
      layout_column_wrap(
        width = 1 / 4,
        heights_equal = "row",
        fillable = FALSE,
        glc_metric_with_help(
          "list-ol",
          "Preview-row budget",
          format(allocation$total_budget, big.mark = ","),
          "Across the subset",
          "Total maximum parsed rows allocated across preview files."
        ),
        glc_metric_with_help(
          "files",
          "Preview-file subset",
          paste0(
            allocation$preview_file_count,
            " of ",
            allocation$available_file_count
          ),
          paste0(length(allocation$omitted_files), " omitted"),
          paste(
            "The canonical subset is ordered by dataset, stable file group,",
            "file position, and package path."
          )
        ),
        glc_metric_with_help(
          "sort-numeric-down",
          "Rows per file",
          format(allocation$per_file_n_max, big.mark = ","),
          "Parsed-row limit",
          "The public glc_read n_max value applied to each preview file."
        ),
        glc_metric_with_help(
          "table",
          "Maximum rows",
          format(allocation$maximum_rows, big.mark = ","),
          paste("Problems:", review$problems_policy),
          "The maximum possible returned rows for this preview."
        )
      )
    ),
    if (isTRUE(review$resource$size_evidence_incomplete)) {
      llw_status_callout(
        "warning",
        paste(
          "At least one preview file has no package-listed size, so exact",
          "transfer bytes remain uncertain. The row ceiling limits parsed",
          "rows, but does not cap network bytes for monolithic files or",
          "Git-LFS objects."
        ),
        heading = "Preview transfer size is uncertain",
        compact = TRUE
      )
    },
    if (preview_blocked) {
      llw_status_callout(
        "error",
        paste(
          "The preview subset exceeds the active known-byte resource budget,",
          "so Preview is disabled."
        ),
        heading = "Resource gate for preview",
        compact = TRUE
      )
    },
    tags$details(
      class = "llw-glc-card-disclosure",
      tags$summary("Canonical preview allocation and provenance"),
      tags$dl(
        class = "row mt-2 mb-0",
        tags$dt(class = "col-sm-4", "Exact source"),
        tags$dd(
          class = "col-sm-8",
          paste0(
            review$source$repository,
            "@",
            review$source$resolved_revision
          )
        ),
        tags$dt(class = "col-sm-4", "Allocated files"),
        tags$dd(
          class = "col-sm-8",
          paste(review$allocation$preview_files, collapse = ", ")
        ),
        tags$dt(class = "col-sm-4", "Allocated listed size"),
        tags$dd(class = "col-sm-8", sizes$allocated_size),
        tags$dt(class = "col-sm-4", "Problems policy"),
        tags$dd(
          class = "col-sm-8",
          if (identical(review$problems_policy, "error")) {
            "Error on parsing problems"
          } else {
            "Return warnings for parsing problems"
          }
        )
      )
    )
  )
}

glc_transfer_preview_table <- function(
  data,
  caption = "Bounded preview sample. This is not a complete dataset."
) {
  if (!is.data.frame(data)) {
    abort_llw("The bounded preview table is invalid.", type = "validation")
  }
  assert_scalar_string(caption, "caption")
  display_data <- utils::head(data, glc_transfer_display_row_limit())
  tags$div(
    class = "table-responsive llw-glc-preview-table",
    tags$table(
      class = "table table-striped table-sm align-middle mb-0",
      tags$caption(
        class = "caption-top",
        caption
      ),
      tags$thead(tags$tr(lapply(names(display_data), tags$th, scope = "col"))),
      tags$tbody(lapply(seq_len(nrow(display_data)), function(row) {
        tags$tr(lapply(display_data[row, , drop = FALSE], function(value) {
          tags$td(glc_compact_display_value(value))
        }))
      }))
    )
  )
}

new_glc_library_add_request <- function(
  full_import,
  request_id = new_stable_id("glc_library_request")
) {
  validate_glc_full_import_result(full_import)
  assert_scalar_string(request_id, "request_id")
  request <- structure(
    list(
      request_schema = "llw-glc-library-add-request",
      request_version = "1.0.0",
      request_id = request_id,
      full_import_fingerprint = full_import$fingerprint,
      full_import = full_import
    ),
    class = c("llw_glc_library_add_request", "list")
  )
  validate_glc_library_add_request(request)
  request
}

validate_glc_library_add_request <- function(request) {
  if (!inherits(request, "llw_glc_library_add_request") || !is.list(request)) {
    abort_llw(
      "The GLC dataset-library request is invalid.",
      type = "validation"
    )
  }
  required <- c(
    "request_schema",
    "request_version",
    "request_id",
    "full_import_fingerprint",
    "full_import"
  )
  if (
    !all(required %in% names(request)) ||
      !identical(request$request_schema, "llw-glc-library-add-request") ||
      !identical(request$request_version, "1.0.0")
  ) {
    abort_llw(
      "The GLC dataset-library request contract is unsupported.",
      type = "validation"
    )
  }
  assert_scalar_string(request$request_id, "request$request_id")
  validate_glc_full_import_result(request$full_import)
  if (
    !identical(
      request$full_import_fingerprint,
      request$full_import$fingerprint
    )
  ) {
    abort_llw(
      "The GLC dataset-library request no longer matches its full import.",
      type = "stale"
    )
  }
  assert_serializable_value(request, "GLC dataset-library request")
  invisible(request)
}

glc_full_import_result_ui <- function(
  result,
  retained = FALSE,
  progress = NULL,
  library_action = NULL
) {
  if (
    !inherits(result, "llw_glc_full_import_result") ||
      !is.list(result)
  ) {
    abort_llw("The complete import handoff is invalid.", type = "validation")
  }
  assert_flag(retained, "retained")
  fixture <- identical(result$execution_mode, "fixture")
  warnings <- glc_nonempty_unique(c(result$warnings, result$problems))
  factor_rows <- result$actual$factor_evidence
  tags$div(
    class = paste(
      "llw-glc-full-import-result",
      "llw-glc-result-surface"
    ),
    if (retained) {
      tags$p(
        class = paste(
          "small llw-secondary d-flex align-items-center gap-2",
          "llw-glc-last-successful-import"
        ),
        icon("clock-rotate-left", `aria-hidden` = "true"),
        paste(
          "Last successful full import retained while the replacement",
          "attempt finishes or is reviewed."
        )
      )
    },
    llw_status_callout(
      if (fixture || length(warnings) > 0L) "warning" else "complete",
      if (fixture) {
        paste(
          "Injected local records exercised a fresh full read and collection",
          "contract. No selected package or measurement file was opened.",
          "This fixture is not evidence from the selected live package."
        )
      } else {
        paste(
          "Every selected runtime input was freshly read and collected from",
          "the exact revision. Preview rows were not used. The complete",
          "handoff is ephemeral and remains only in this session."
        )
      },
      heading = if (fixture) {
        "Development fixture full import ready"
      } else if (length(warnings) > 0L) {
        "Complete collected import handoff ready with warnings"
      } else {
        "Complete collected import handoff ready"
      },
      compact = TRUE
    ),
    if (!is.null(library_action)) {
      tags$div(class = "llw-glc-library-action", library_action)
    },
    tags$div(
      class = "llw-glc-metrics llw-glc-transfer-metrics",
      layout_column_wrap(
        width = 1 / 4,
        heights_equal = "row",
        fillable = FALSE,
        glc_metric_with_help(
          "folder-check",
          "File groups",
          result$actual$file_group_count,
          "Complete handoff",
          "Stable file groups represented by the fresh full read."
        ),
        glc_metric_with_help(
          "database",
          "Datasets",
          result$actual$dataset_count,
          "Complete handoff",
          "Datasets represented by the complete collected handoff."
        ),
        glc_metric_with_help(
          "files",
          if (fixture) "Fixture inputs" else "Files read",
          result$actual$input_count,
          "Complete selected set",
          if (fixture) {
            paste(
              "Injected fixture inputs exercised the full-read contract.",
              "No selected live-package path was opened."
            )
          } else {
            "Runtime files actually read for the complete selected import."
          }
        ),
        glc_metric_with_help(
          "list-ol",
          "Collected rows",
          result$actual$row_count,
          "Full in-session table",
          "Rows in the complete ephemeral collected handoff."
        )
      )
    ),
    tags$section(
      class = paste(
        "llw-wizard-field-group",
        "llw-glc-full-import-panel"
      ),
      tags$h4("Complete collected handoff, not yet a dataset"),
      tags$p(
        class = "llw-secondary",
        paste(
          "The in-session handoff contains",
          glc_pluralized_count(result$actual$row_count, "row"),
          "and",
          glc_pluralized_count(result$actual$column_count, "column"),
          "from a fresh full read. The table below displays at most",
          glc_transfer_display_row_limit(),
          "rows",
          "from that complete handoff. It is not a bounded preview result."
        )
      ),
      glc_transfer_preview_table(
        result$data,
        caption = paste(
          "Display sample from the complete in-session handoff.",
          "The complete collected table remains in memory and was not",
          "persisted or promoted."
        )
      ),
      tags$details(
        class = "llw-glc-card-disclosure mt-3",
        tags$summary("Column types and factor validation evidence"),
        tags$ul(
          class = "mt-2",
          lapply(seq_len(nrow(result$actual$column_types)), function(index) {
            tags$li(
              tags$code(result$actual$column_types$name[[index]]),
              ": ",
              result$actual$column_types$class[[index]]
            )
          })
        ),
        if (nrow(factor_rows) == 0L) {
          tags$p(
            class = "mb-0",
            "No factor columns are present in the collected handoff."
          )
        } else {
          tags$ul(
            class = "mb-0",
            lapply(seq_len(nrow(factor_rows)), function(index) {
              tags$li(
                tags$code(factor_rows$name[[index]]),
                ": ",
                glc_pluralized_count(
                  factor_rows$level_count[[index]],
                  "factor level"
                ),
                " (",
                paste(factor_rows$levels[[index]], collapse = ", "),
                ")"
              )
            })
          )
        }
      ),
      if (length(warnings) > 0L) {
        tags$details(
          class = "llw-glc-card-disclosure mt-3",
          open = "open",
          tags$summary("Warnings and parsing problems"),
          tags$ul(class = "mt-2 mb-0", lapply(warnings, tags$li))
        )
      },
      glc_task_timing_ui(progress),
      tags$details(
        class = "llw-glc-card-disclosure mt-3",
        tags$summary("Exact full-import provenance"),
        tags$dl(
          class = "row mt-2 mb-0",
          tags$dt(class = "col-sm-4", "Exact source"),
          tags$dd(
            class = "col-sm-8",
            paste0(
              result$source$repository,
              "@",
              result$source$resolved_revision
            )
          ),
          tags$dt(class = "col-sm-4", "Compatibility ID"),
          tags$dd(class = "col-sm-8", result$fixed$compatibility_id),
          tags$dt(class = "col-sm-4", "Final unit ID"),
          tags$dd(class = "col-sm-8", result$fixed$unit_id),
          tags$dt(class = "col-sm-4", "Execution mode"),
          tags$dd(
            class = "col-sm-8",
            if (fixture) "Deterministic local fixture" else "Live full import"
          ),
          tags$dt(
            class = "col-sm-4",
            if (fixture) {
              "Reviewed live-package paths (not opened)"
            } else {
              "Reviewed live-package paths"
            }
          ),
          tags$dd(
            class = "col-sm-8",
            paste(result$reviewed$file_ids, collapse = ", ")
          ),
          tags$dt(
            class = "col-sm-4",
            if (fixture) "Actual fixture inputs" else "Actual runtime inputs"
          ),
          tags$dd(
            class = "col-sm-8",
            paste(result$actual$input_ids, collapse = ", ")
          ),
          tags$dt(class = "col-sm-4", "Read contract"),
          tags$dd(
            class = "col-sm-8",
            paste0(
              "All selected files; n_max = Inf; problems = ",
              result$problems_policy,
              "; no discovery-term filter; standardize = lightlogr"
            )
          ),
          tags$dt(class = "col-sm-4", "Elapsed time"),
          tags$dd(
            class = "col-sm-8",
            paste0(
              format(round(result$runtime$elapsed_seconds, 2), nsmall = 2),
              " s"
            )
          ),
          tags$dt(class = "col-sm-4", "Live measurement access"),
          tags$dd(
            class = "col-sm-8",
            if (isTRUE(result$assurance$no_live_measurement_access)) {
              "None. The reviewed package paths were not opened."
            } else {
              "Occurred for the actual runtime inputs shown above."
            }
          ),
          tags$dt(class = "col-sm-4", "Preview rows used"),
          tags$dd(class = "col-sm-8", "No. Apply performed a fresh full read."),
          tags$dt(class = "col-sm-4", "Persistence and promotion"),
          tags$dd(
            class = "col-sm-8",
            "None. The complete handoff remains ephemeral in this session."
          )
        )
      )
    )
  )
}

glc_library_add_action_ui <- function(
  ns,
  result,
  added = NULL,
  error = NULL,
  status = NULL,
  input_id = "add_to_library",
  show_error = TRUE
) {
  if (!inherits(result, "llw_glc_full_import_result")) return(NULL)
  already_added <-
    is.list(added) &&
    identical(added$full_import_fingerprint, result$fingerprint)
  processing <- is.list(status) &&
    status$state %in% c("preparing", "adding", "opening")
  label <- if (already_added) {
    "Added to dataset library"
  } else if (processing) {
    status$message %||% "Adding dataset to library..."
  } else {
    "Add to dataset library and open dashboard"
  }
  add_button <- actionButton(
    ns(input_id),
    label,
    icon = if (processing) {
      icon("spinner", class = "fa-spin")
    } else {
      icon(if (already_added) "circle-check" else "chart-line")
    },
    class = if (already_added) "btn-success" else "btn-primary",
    `aria-disabled` = if (already_added || processing) "true" else NULL
  )
  if (already_added || processing) {
    add_button <- htmltools::tagAppendAttributes(
      add_button,
      disabled = "disabled"
    )
  }
  tagList(
    add_button,
    if (processing) {
      tags$span(
        class = "llw-glc-library-progress",
        role = "status",
        `aria-live` = "polite",
        status$detail %||%
          "The dataset record and provenance are being prepared."
      )
    },
    if (show_error && !is.null(error)) {
      tags$div(
        class = "llw-glc-library-error",
        llw_status_callout(
          "error",
          error,
          heading = "Dataset was not added",
          compact = TRUE
        )
      )
    }
  )
}

glc_transfer_preview_result_ui <- function(result) {
  if (
    !inherits(result, "llw_glc_transfer_preview_result") ||
      !is.list(result)
  ) {
    abort_llw("The bounded preview result is invalid.", type = "validation")
  }
  warnings <- glc_nonempty_unique(c(result$warnings, result$problems))
  fixture <- identical(result$execution_mode, "fixture")
  file_label <- if (fixture) "Fixture inputs" else "Files read"
  sample_detail <- if (fixture) "Injected sample" else "Runtime sample"
  file_help <- if (fixture) {
    paste(
      "Injected local files represented in this deterministic contract",
      "exercise. No selected package file was opened."
    )
  } else {
    "Files actually represented in this bounded runtime preview."
  }
  tags$div(
    class = "llw-glc-transfer-preview-result mt-3",
    llw_status_callout(
      if (fixture || length(warnings) > 0L) "warning" else "complete",
      if (fixture) {
        paste(
          "Injected local records exercised the reviewed public read and",
          "collect contract. No package or measurement file was opened, so",
          "this sample is not evidence from the selected live package."
        )
      } else {
        paste(
          "Only the canonical preview subset was read and collected.",
          "Unread files were not runtime-validated, and this ephemeral sample",
          "was not imported, persisted, or promoted."
        )
      },
      heading = if (fixture) {
        "Development fixture preview ready"
      } else if (length(warnings) > 0L) {
        "Bounded preview ready with warnings"
      } else {
        "Bounded preview ready"
      },
      compact = TRUE
    ),
    tags$div(
      class = "llw-glc-metrics llw-glc-transfer-metrics mt-3",
      layout_column_wrap(
        width = 1 / 4,
        heights_equal = "row",
        fillable = FALSE,
        glc_metric_with_help(
          "files",
          file_label,
          result$actual$input_count,
          "Canonical subset",
          file_help
        ),
        glc_metric_with_help(
          "folder-check",
          "File groups",
          result$actual$file_group_count,
          sample_detail,
          if (fixture) {
            paste(
              "Stable file groups represented by the injected local fixture",
              "records."
            )
          } else {
            "Stable file groups represented by the files actually previewed."
          }
        ),
        glc_metric_with_help(
          "database",
          "Datasets",
          result$actual$dataset_count,
          sample_detail,
          if (fixture) {
            "Datasets represented by the injected local fixture records."
          } else {
            "Datasets represented by the files actually previewed."
          }
        ),
        glc_metric_with_help(
          "list-ol",
          "Preview rows",
          result$actual$row_count,
          "Ephemeral sample",
          "Rows returned by the bounded read and collection preview."
        )
      )
    ),
    tags$section(
      class = "llw-wizard-field-group mt-3",
      tags$h4("Preview sample, not a dataset"),
      tags$p(
        class = "llw-secondary",
        paste0(
          paste(
            "The sample contains",
            result$actual$column_count,
            "columns. At most",
            result$disclosure$maximum_possible_rows,
            "rows could be returned from the reviewed allocation, with",
            glc_pluralized_count(
              result$disclosure$per_file_n_max,
              "parsed row per file",
              "parsed rows per file"
            )
          ),
          "."
        )
      ),
      glc_transfer_preview_table(result$data),
      tags$details(
        class = "llw-glc-card-disclosure mt-3",
        tags$summary("Column types and runtime limitations"),
        tags$ul(
          class = "mt-2",
          lapply(seq_len(nrow(result$actual$column_types)), function(index) {
            tags$li(
              tags$code(result$actual$column_types$name[[index]]),
              ": ",
              result$actual$column_types$class[[index]]
            )
          })
        ),
        tags$p(
          class = "mb-1",
          paste0(
            glc_pluralized_count(
              result$disclosure$omitted_file_count,
              "file was omitted from this canonical preview",
              "files were omitted from this canonical preview"
            ),
            "."
          )
        ),
        tags$p(
          class = "mb-0",
          if (fixture) {
            paste(
              "This deterministic fixture exercised the reviewed n_max and",
              "collection contract without network access. No selected live",
              "file, omitted file, or transfer size was runtime-validated."
            )
          } else {
            paste(
              "n_max bounded parsed rows per file, but the network transfer may",
              "have been larger for monolithic or Git-LFS files. Unread files",
              "were not validated by this runtime preview."
            )
          }
        )
      ),
      if (length(warnings) > 0L) {
        tags$details(
          class = "llw-glc-card-disclosure mt-3",
          open = "open",
          tags$summary("Warnings and parsing problems"),
          tags$ul(class = "mt-2 mb-0", lapply(warnings, tags$li))
        )
      },
      tags$details(
        class = "llw-glc-card-disclosure mt-3",
        tags$summary("Exact preview provenance"),
        tags$dl(
          class = "row mt-2 mb-0",
          tags$dt(class = "col-sm-4", "Exact source"),
          tags$dd(
            class = "col-sm-8",
            paste0(
              result$source$repository,
              "@",
              result$source$resolved_revision
            )
          ),
          tags$dt(class = "col-sm-4", "Compatibility ID"),
          tags$dd(class = "col-sm-8", result$fixed$compatibility_id),
          tags$dt(class = "col-sm-4", "Final unit ID"),
          tags$dd(class = "col-sm-8", result$fixed$unit_id),
          tags$dt(class = "col-sm-4", "Execution mode"),
          tags$dd(
            class = "col-sm-8",
            if (fixture) "Deterministic local fixture" else "Bounded runtime"
          ),
          tags$dt(
            class = "col-sm-4",
            if (fixture) {
              "Reviewed live-package paths (not opened)"
            } else {
              "Reviewed live-package paths"
            }
          ),
          tags$dd(
            class = "col-sm-8",
            paste(result$reviewed$file_ids, collapse = ", ")
          ),
          tags$dt(
            class = "col-sm-4",
            if (fixture) "Actual fixture inputs" else "Actual runtime inputs"
          ),
          tags$dd(
            class = "col-sm-8",
            paste(result$actual$input_ids, collapse = ", ")
          ),
          tags$dt(class = "col-sm-4", "Live measurement access"),
          tags$dd(
            class = "col-sm-8",
            if (isTRUE(result$assurance$no_live_measurement_access)) {
              "None. The reviewed package paths were not opened."
            } else {
              "The bounded runtime inputs shown above were opened."
            }
          )
        )
      )
    )
  )
}

glc_scope_exclusion_ui <- function(plan) {
  excluded <- plan$groups[plan$groups$status != "included", , drop = FALSE]
  if (nrow(excluded) == 0L) return(NULL)

  messages <- vapply(
    excluded$messages,
    function(value) {
      value <- glc_nonempty_unique(value)
      if (length(value) == 0L) "Excluded by the public plan." else
        paste(value, collapse = " ")
    },
    character(1)
  )
  message_counts <- sort(table(messages), decreasing = TRUE)
  tags$details(
    class = "llw-glc-card-disclosure mt-3",
    tags$summary(glc_pluralized_count(
      nrow(excluded),
      "Show excluded file group",
      "Show excluded file groups"
    )),
    tags$p(
      class = "llw-secondary mt-2",
      paste(
        "These exclusions come directly from the public glcdp plan for the",
        "changed variable request."
      )
    ),
    tags$ul(
      class = "mb-2",
      lapply(seq_along(message_counts), function(index) {
        tags$li(
          tags$strong(paste0(as.integer(message_counts[[index]]), ": ")),
          names(message_counts)[[index]]
        )
      })
    ),
    tags$details(
      class = "llw-glc-variable-disclosure",
      tags$summary("Show exact stable file-group identities"),
      tags$ul(
        class = "llw-glc-variable-list mt-2 mb-0",
        lapply(seq_len(nrow(excluded)), function(index) {
          tags$li(
            tags$code(as.character(excluded$file_group_id[[index]])),
            paste0(" in dataset ", excluded$dataset_id[[index]], ": "),
            messages[[index]]
          )
        })
      )
    )
  )
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
        tags$dt("Complete file schema validated for compatibility"),
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
          "Each structure contains file groups whose complete file schemas",
          "share the columns, types, factor contract, time handling, modality,",
          "role, and data state required for reading and collection."
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
    "---"
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
      scalar_value = logical(),
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
      numeric_value = numeric_values,
      scalar_value = rep(length(values) == 1L, length(values)),
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
      scalar_value = logical(),
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

glc_numeric_characteristic_projection <- function(records) {
  if (
    !is.data.frame(records) ||
      nrow(records) == 0L ||
      !all(
        c(
          "value_type",
          "unit",
          "value_key",
          "numeric_value",
          "scalar_value"
        ) %in%
          names(records)
      ) ||
      !all(records$scalar_value %in% TRUE)
  ) {
    return(NULL)
  }
  value_types <- unique(tolower(trimws(as.character(records$value_type))))
  numeric_types <- c("double", "integer", "number", "numeric")
  character_types <- c("character", "string", "text")
  if (
    !(all(value_types %in% numeric_types) ||
      all(value_types %in% character_types))
  ) {
    return(NULL)
  }
  units <- as.character(records$unit)
  units[is.na(units)] <- ""
  units <- trimws(units)
  if (length(unique(units)) != 1L) return(NULL)

  tokens <- as.character(records$value_key)
  if (anyNA(tokens) || any(!nzchar(trimws(tokens)))) return(NULL)
  numeric_values <- suppressWarnings(as.numeric(trimws(tokens)))
  if (any(!is.finite(numeric_values))) return(NULL)

  token_map <- unique(data.frame(
    original_token = tokens,
    numeric_value = numeric_values,
    unit = units,
    stringsAsFactors = FALSE
  ))
  if (
    length(unique(token_map$original_token)) !=
      length(unique(token_map$numeric_value))
  ) {
    return(NULL)
  }
  token_map[
    order(token_map$numeric_value, token_map$original_token, method = "radix"),
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
      numeric_projection = NULL,
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
      numeric_projection = NULL,
      slider = NULL
    ))
  }
  units <- glc_nonempty_unique(records$unit)
  label <- if (length(units) == 1L) {
    paste0(name[[1L]], " (", units[[1L]], ")")
  } else {
    name[[1L]]
  }
  numeric_projection <- glc_numeric_characteristic_projection(records)
  numeric <- !is.null(numeric_projection)
  list(
    type = if (numeric) "numeric" else "categorical",
    name = name[[1L]],
    label = label,
    records = records,
    numeric_projection = numeric_projection,
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

glc_scope_input_id <- function(id, revision = 0L) {
  assert_scalar_string(id, "variable scope input id")
  if (
    length(revision) != 1L ||
      is.na(revision) ||
      !is.numeric(revision) ||
      !is.finite(revision) ||
      revision < 0 ||
      revision != floor(revision)
  ) {
    abort_llw(
      "The variable scope input revision is invalid.",
      type = "validation"
    )
  }
  paste0(id, "_r", as.integer(revision))
}

glc_characteristic_value_input_id <- function(
  characteristic_name,
  revision = NULL
) {
  id <- glc_metadata_filter_input_id(
    "filter_characteristic_values",
    revision
  )
  if (is.null(revision)) return(id)
  characteristic_name <- glc_nonempty_unique(characteristic_name)
  if (length(characteristic_name) != 1L) return(paste0(id, "_none"))
  encoded_name <- paste0(
    sprintf(
      "%02x",
      as.integer(charToRaw(enc2utf8(characteristic_name[[1L]])))
    ),
    collapse = ""
  )
  paste0(id, "_c", encoded_name)
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

glc_numeric_range_is_full <- function(values, limits) {
  values <- suppressWarnings(as.numeric(values))
  if (
    is.null(limits) ||
      length(values) != 2L ||
      any(!is.finite(values)) ||
      !all(c("min", "max", "step") %in% names(limits))
  ) {
    return(FALSE)
  }
  endpoints <- suppressWarnings(as.numeric(c(limits$min, limits$max)))
  step <- suppressWarnings(as.numeric(limits$step))
  if (length(endpoints) != 2L || any(!is.finite(endpoints))) return(FALSE)
  if (length(step) != 1L || !is.finite(step) || step < 0) step <- 0
  tolerance <- max(
    sqrt(.Machine$double.eps) * max(1, abs(endpoints)),
    step / 2
  )
  selected <- range(values)
  abs(selected[[1L]] - endpoints[[1L]]) <= tolerance &&
    abs(selected[[2L]] - endpoints[[2L]]) <= tolerance
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
  input_id <- ns(glc_characteristic_value_input_id(
    spec$name,
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
  if (glc_numeric_range_is_full(age_range, age_limits)) {
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
    raw_characteristic_value <- input[[glc_characteristic_value_input_id(
      characteristic_spec$name,
      revision
    )]] %||%
      character()
    if (identical(characteristic_spec$type, "numeric")) {
      characteristic_range <- suppressWarnings(as.numeric(
        raw_characteristic_value
      ))
      characteristic_range <- characteristic_range[
        is.finite(characteristic_range)
      ]
      limits <- characteristic_spec$slider
      if (glc_numeric_range_is_full(characteristic_range, limits)) {
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
    characteristic_numeric_tokens = if (
      identical(characteristic_spec$type, "numeric")
    ) {
      characteristic_spec$numeric_projection
    } else {
      NULL
    },
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

glc_cancellation_drain_ui <- function(status, active_runtime_count) {
  if (
    !inherits(status, "llw_task_status") ||
      !identical(status$state, "cancelled") ||
      !is.numeric(active_runtime_count) ||
      length(active_runtime_count) != 1L ||
      is.na(active_runtime_count) ||
      active_runtime_count < 1
  ) {
    return(NULL)
  }
  tags$span(
    class = paste(
      "small llw-secondary d-inline-flex align-items-center gap-2",
      "llw-glc-cancellation-drain"
    ),
    icon("hourglass-half", `aria-hidden` = "true"),
    "Finishing cancellation before retry..."
  )
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
      ".llw-glc-wizard .llw-glc-scope-metrics > .bslib-grid,",
      ".llw-glc-wizard .llw-glc-transfer-metrics > .bslib-grid {",
      "align-items: stretch;}",
      ".llw-glc-wizard .llw-glc-scope-metrics .bslib-grid-item,",
      ".llw-glc-wizard .llw-glc-transfer-metrics .bslib-grid-item {",
      "display: flex; min-width: 0;}",
      ".llw-glc-wizard .llw-glc-scope-metrics .llw-wizard-metric,",
      ".llw-glc-wizard .llw-glc-transfer-metrics .llw-wizard-metric {",
      "flex: 1 1 auto;}",
      ".llw-glc-wizard .llw-import-stepper {grid-template-columns:",
      "repeat(7, minmax(0, 1fr));}",
      ".llw-glc-wizard .llw-import-stepper__copy strong {",
      "overflow: visible; text-overflow: clip; white-space: normal;",
      "line-height: 1.2;}",
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
      ".llw-glc-transfer-controls {display: grid; grid-template-columns:",
      "repeat(2, minmax(0, 1fr)); gap: .75rem 1rem;}",
      ".llw-glc-transfer-controls .form-group {margin-bottom: 0;}",
      ".llw-glc-scope-card > .card-body {gap: 0 !important;}",
      ".llw-glc-scope-card .llw-glc-scope-mode-selector .form-group,",
      ".llw-glc-scope-card .llw-glc-scope-mode-selector .shiny-options-group,",
      ".llw-glc-scope-card .form-group {margin-bottom: 0;}",
      ".llw-glc-scope-card > .card-body > .shiny-html-output:not(:empty) {",
      "margin-top: .65rem;}",
      ".llw-glc-scope-card > .card-body > .shiny-html-output:first-child {",
      "margin-top: 0;}",
      ".llw-glc-scope-feedback-output:not(:empty) {margin-top: 0 !important;}",
      ".llw-glc-scope-feedback-result {margin-top: .75rem !important;}",
      ".llw-glc-scope-actions {display: flex; flex-wrap: wrap; gap: .75rem;",
      "justify-content: flex-end; min-width: 0;}",
      ".llw-glc-import-step {display: grid; gap: .85rem;}",
      ".llw-glc-import-step > * {margin-block: 0 !important;}",
      ".llw-glc-import-stack, .llw-glc-preview-review,",
      ".llw-glc-preview-disclosure__body, .llw-glc-import-details__body {",
      "display: grid; gap: 0; min-width: 0; max-width: 100%;}",
      ".llw-glc-import-stack > *, .llw-glc-preview-review > *,",
      ".llw-glc-preview-disclosure__body > * {margin-block: 0 !important;",
      "min-width: 0; max-width: 100%;}",
      paste(
        ".llw-glc-apply-progress-output:not(:empty),",
        ".llw-glc-full-import-output:not(:empty)"
      ),
      "{margin-top: .75rem !important;}",
      ".llw-glc-import-stack > *:not(:empty) + *:not(:empty),",
      ".llw-glc-preview-review > *:not(:empty) + *:not(:empty),",
      ".llw-glc-preview-disclosure__body > *:not(:empty) + *:not(:empty),",
      ".llw-glc-import-details__body > *:not(:empty) + *:not(:empty) {",
      "margin-top: .75rem !important;}",
      ".llw-glc-import-stack .llw-glc-full-import-result,",
      ".llw-glc-import-stack .llw-glc-transfer-progress,",
      ".llw-glc-preview-disclosure__body .llw-glc-transfer-preview-result,",
      ".llw-glc-preview-disclosure__body .llw-glc-transfer-progress {",
      "margin-top: 0 !important;}",
      paste(
        ".llw-glc-import-stack",
        ".llw-glc-full-import-result.llw-glc-result-surface"
      ),
      "{margin-top: .75rem !important;}",
      ".llw-glc-import-details__body {padding-top: .75rem;}",
      ".llw-glc-import-details__body h4 {margin: 0;}",
      ".llw-glc-import-action-group, .llw-glc-preview-action-group {",
      "margin: 0 !important;}",
      ".llw-glc-library-action {display: flex; flex-wrap: wrap;",
      "align-items: center; gap: .65rem; margin-top: .75rem;}",
      ".llw-glc-library-progress {color: var(--bs-secondary-color);",
      "font-size: .9rem; overflow-wrap: anywhere;}",
      ".llw-glc-library-error {flex-basis: 100%; margin-top: .1rem;}",
      ".llw-glc-library-footer-action {display: flex; justify-content:",
      "flex-end; min-width: 0;}",
      ".llw-glc-import-summary {padding: .1rem 0;}",
      ".llw-glc-preview-disclosure {border: 1px solid var(--bs-border-color);",
      "border-radius: .65rem; background: var(--bs-body-bg); overflow: clip;}",
      ".llw-glc-preview-disclosure > summary {cursor: pointer;",
      "font-size: 1.15rem; font-weight: 700; padding: .85rem 1rem;}",
      ".llw-glc-preview-disclosure[open] > summary {",
      "border-bottom: 1px solid var(--bs-border-color);}",
      ".llw-glc-preview-disclosure__body {padding: 1rem;}",
      ".llw-glc-import-card {border: 1px solid var(--bs-border-color);",
      "border-radius: .65rem; background: var(--bs-body-bg); overflow: visible;",
      "height: auto; min-width: 0; max-width: 100%;}",
      ".llw-glc-import-card__header {margin: 0; padding: .75rem 1rem;",
      "border-bottom: 1px solid var(--bs-border-color); font-size: 1rem;}",
      ".llw-glc-import-card__body {padding: 1rem; overflow: visible;}",
      ".llw-glc-import-card, .llw-glc-import-card__body,",
      ".llw-glc-full-import-result, .llw-glc-transfer-preview-result,",
      ".llw-glc-full-import-result > *,",
      ".llw-glc-transfer-preview-result > *,",
      ".llw-glc-full-import-result .llw-wizard-field-group,",
      ".llw-glc-transfer-preview-result .llw-wizard-field-group {",
      "min-width: 0; max-width: 100%;}",
      ".llw-glc-full-import-result {display: grid; gap: .75rem;}",
      ".llw-glc-full-import-result > * {margin-block: 0 !important;}",
      ".llw-glc-import-elapsed {display: flex; align-items: baseline;",
      "justify-content: space-between; gap: 1rem; padding: .65rem .8rem;",
      "margin-bottom: .75rem; border: 1px solid var(--bs-border-color);",
      "border-radius: .5rem; background: var(--bs-tertiary-bg);}",
      ".llw-glc-import-elapsed__label {font-weight: 600;}",
      ".llw-glc-import-elapsed__value {color: var(--bs-primary);",
      "font-size: 1.2rem; font-variant-numeric: tabular-nums;",
      "white-space: nowrap;}",
      ".llw-glc-full-import-result .llw-wizard-field-group,",
      ".llw-glc-transfer-preview-result .llw-wizard-field-group {",
      "height: auto !important;}",
      ".llw-glc-full-import-panel {display: grid; gap: .75rem;}",
      ".llw-glc-full-import-panel > * {margin-block: 0 !important;}",
      ".llw-glc-full-import-panel > h4 {margin: 0 !important;}",
      ".llw-glc-full-import-result dd,",
      ".llw-glc-transfer-preview-result dd {overflow-wrap: anywhere;}",
      ".llw-glc-preview-table {display: block; width: 100%; max-width: 100%;",
      "min-width: 0; overflow-x: auto; overflow-y: hidden;",
      "-webkit-overflow-scrolling: touch;}",
      ".llw-glc-preview-table table {width: max-content; min-width: 100%;",
      "max-width: none;}",
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
      ".llw-glc-transfer-controls,",
      ".llw-glc-variable-list {grid-template-columns: minmax(0, 1fr);}",
      ".llw-glc-scope-actions {align-items: stretch; flex-direction: column;",
      "width: 100%;}",
      ".llw-glc-scope-actions > .shiny-html-output,",
      ".llw-glc-scope-actions .btn {width: 100%;}",
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
            "narrow them with metadata and choose output variables. An optional",
            "bounded preview or a fresh full Apply import is always explicit."
          )
        )
      ),
      tags$span(
        class = "llw-import-wizard__engine-note",
        bsicons::bs_icon("shield-check"),
        " No transfer until Preview or Apply"
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
              "Opening this step loads the validated package description and",
              "core metadata when needed.",
              "Measurement files remain unopened."
            )
          ),
          tags$div(
            class = "llw-action-row llw-import-actions",
            uiOutput(ns("inspect_button"), inline = TRUE),
            uiOutput(ns("cancel_inspection_button"), inline = TRUE),
            uiOutput(ns("inspection_cancellation_drain"), inline = TRUE)
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
              "A modality, canonical data term, or both identifies candidate",
              "file groups. glcdp then validates their complete file schemas",
              "to establish groups that can be read and collected together.",
              "Output variables are chosen later in Step 6."
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
            uiOutput(ns("cancel_plan_button"), inline = TRUE),
            uiOutput(ns("planning_cancellation_drain"), inline = TRUE)
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
              "Each option comes directly from glcdp's public planner after",
              "validating candidate groups against their complete file",
              "schemas. Step 6 later chooses the output variables."
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
          uiOutput(ns("refinement_summary")),
          tags$p(
            class = "small llw-secondary mt-3",
            paste(
              "Measurement access remains closed until an explicit preview or",
              "Apply action in Step 7. Persistence and dataset-library",
              "promotion remain closed."
            )
          ),
          import_wizard_navigation(
            actionButton(
              ns("back_structure"),
              "Back to structure selection",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary btn-lg"
            ),
            uiOutput(ns("continue_variables_button"), inline = TRUE),
            split = TRUE
          )
        )
      ),
      nav_panel(
        "Choose variables",
        value = "variables",
        tags$div(
          class = "llw-wizard-step-content",
          import_wizard_step_heading(
            "columns-gap",
            "Project the output columns",
            "Choose which variables the later result should retain",
            paste(
              "The compatible structure, final units, and file groups were",
              "already fixed in Slice 2A. This step only chooses output columns",
              "and never reruns compatibility planning."
            )
          ),
          card(
            class = "llw-glc-scope-card",
            card_header("Variable scope", container = h3),
            uiOutput(ns("scope_mode_selector")),
            uiOutput(ns("custom_variable_selector")),
            uiOutput(
              ns("scope_feedback"),
              class = "llw-glc-scope-feedback-output"
            )
          ),
          import_wizard_navigation(
            actionButton(
              ns("back_scope"),
              "Back to metadata narrowing",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary btn-lg"
            ),
            tags$div(
              class = "llw-glc-scope-actions",
              uiOutput(ns("scope_apply_button"), inline = TRUE),
              uiOutput(ns("continue_preview_button"), inline = TRUE)
            ),
            split = TRUE
          ),
          uiOutput(
            ns("scope_review"),
            class = "llw-glc-scope-review-output"
          ),
          tags$p(
            class = "small llw-secondary mt-3",
            paste(
              "This review is serializable and metadata-only. No package handle,",
              "temporary path, credential, measurement preview, read, or collect",
              "result is retained."
            )
          )
        )
      ),
      nav_panel(
        "Import",
        value = "preview",
        tags$div(
          class = "llw-wizard-step-content llw-glc-import-step",
          import_wizard_step_heading(
            "table",
            "Import the selected data",
            "Import the complete selected data",
            paste(
              "The exact revision, finalized unit, file membership, and output",
              "projection stay fixed. Apply starts a fresh read of every",
              "selected file. Preview remains optional."
            )
          ),
          tags$details(
            class = "llw-glc-preview-disclosure",
            tags$summary("Preview"),
            tags$div(
              class = "llw-glc-preview-disclosure__body",
              tags$p(
                class = "llw-secondary mb-0",
                paste(
                  "Optionally inspect a bounded typed sample before import.",
                  "Preview rows are never reused by Apply and are not a",
                  "dataset."
                )
              ),
              tags$div(
                class = "llw-glc-transfer-controls",
                numericInput(
                  ns("transfer_row_budget"),
                  "Total preview-row budget",
                  value = 20L,
                  min = 1L,
                  max = glc_transfer_preview_row_limit(),
                  step = 1L,
                  width = "100%"
                ),
                radioButtons(
                  ns("transfer_problems"),
                  "Parsing problems",
                  choices = c(
                    "Stop with a recoverable error (recommended)" = "error",
                    "Return the preview with warnings (advanced)" = "warn"
                  ),
                  selected = "error",
                  width = "100%"
                )
              ),
              tags$p(
                class = "small llw-secondary mb-0",
                paste(
                  "Enter a whole number from 1 through 10,000. This ceiling",
                  "limits parsed rows, but may not limit network bytes for",
                  "monolithic files or Git-LFS objects."
                )
              ),
              uiOutput(ns("transfer_review")),
              tags$div(
                class = paste(
                  "llw-action-row llw-import-actions",
                  "llw-glc-preview-action-group"
                ),
                uiOutput(ns("transfer_preview_button"), inline = TRUE),
                uiOutput(ns("cancel_transfer_button"), inline = TRUE),
                uiOutput(ns("transfer_cancellation_drain"), inline = TRUE)
              ),
              uiOutput(ns("transfer_progress_card")),
              uiOutput(ns("transfer_preview_result"))
            )
          ),
          tags$section(
            class = "llw-glc-import-card",
            tags$h3(class = "llw-glc-import-card__header", "Import"),
            tags$div(
              class = "llw-glc-import-card__body",
              tags$div(
                class = "llw-glc-import-stack",
                uiOutput(ns("import_review")),
                tags$div(
                  class = paste(
                    "llw-action-row llw-import-actions",
                    "llw-glc-import-action-group"
                  ),
                  uiOutput(ns("apply_full_import_button"), inline = TRUE),
                  uiOutput(ns("cancel_apply_button"), inline = TRUE),
                  uiOutput(ns("apply_cancellation_drain"), inline = TRUE)
                ),
                uiOutput(
                  ns("apply_progress_card"),
                  class = "llw-glc-apply-progress-output"
                ),
                uiOutput(
                  ns("full_import_result"),
                  class = "llw-glc-full-import-output"
                )
              )
            )
          ),
          tags$p(
            class = "small llw-secondary mb-0",
            paste(
              "Preview rows remain samples. A successful Apply result is a",
              "complete collected import handoff held only in this session.",
              "Neither result is persisted, promoted, downloaded, or added to",
              "the dataset library."
            )
          ),
          import_wizard_navigation(
            actionButton(
              ns("back_preview"),
              "Back to output variables",
              icon = icon("arrow-left"),
              class = "btn-outline-secondary btn-lg"
            ),
            uiOutput(ns("library_add_footer_action"), inline = TRUE),
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

glc_transfer_attempt_is_current <- function(
  attempt,
  transfer_revision,
  source_key,
  review_fingerprint
) {
  if (!is.list(attempt)) return(FALSE)
  identical(attempt$transfer_revision, as.integer(transfer_revision)) &&
    identical(attempt$source_key, source_key) &&
    identical(attempt$review_fingerprint, review_fingerprint)
}

glc_full_import_attempt_is_current <- function(
  attempt,
  apply_revision,
  source_key,
  contract_fingerprint
) {
  if (!is.list(attempt)) return(FALSE)
  identical(attempt$apply_revision, as.integer(apply_revision)) &&
    identical(attempt$source_key, source_key) &&
    identical(attempt$contract_fingerprint, contract_fingerprint)
}

glc_full_import_contract_fingerprint <- function(transfer_review) {
  validate_glc_transfer_review(transfer_review)
  glc_transfer_value_fingerprint(list(
    contract_schema = "llw-glc-full-import-contract",
    contract_version = "1.0.0",
    projection_fingerprint = transfer_review$projection_fingerprint,
    source = transfer_review$source,
    fixed = transfer_review$fixed,
    selected_source_variables = transfer_review$selected_source_variables,
    automatic_read_inputs = transfer_review$automatic_read_inputs,
    automatic_result_columns = transfer_review$automatic_result_columns,
    selected_groups = transfer_review$selected_groups,
    files = transfer_review$files,
    full_read = list(
      n_max = Inf,
      problems = "error",
      progress = FALSE,
      standardize = "lightlogr"
    )
  ))
}

glc_full_import_contract_review <- function(transfer_review) {
  validate_glc_transfer_review(transfer_review)
  new_glc_transfer_review(
    transfer_review$projection,
    total_preview_rows = 1L,
    problems = "error",
    available_session_bytes = transfer_review$resource$available_session_bytes
  )
}

glcPreviewServer <- function(
  id,
  source_selection,
  runtime,
  cache_dir,
  auto_inspect = NULL,
  inspection_worker = glcdp_inspection_task_worker,
  planning_worker = glcdp_planning_task_worker,
  preview_worker = glcdp_transfer_preview_task_worker,
  apply_worker = glcdp_full_import_task_worker,
  refine_collection = glcdp_refine_collection,
  task_start_timeout_seconds = 15,
  enable_library_integration = FALSE
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
      !is.function(preview_worker) ||
      !is.function(apply_worker) ||
      !is.function(refine_collection)
  ) {
    abort_llw(
      paste(
        "GLC inspection, planning, preview, full-import, and refinement",
        "workers must be functions."
      ),
      type = "validation"
    )
  }
  assert_flag(enable_library_integration, "enable_library_integration")
  if (
    !is.numeric(task_start_timeout_seconds) ||
      length(task_start_timeout_seconds) != 1L ||
      is.na(task_start_timeout_seconds) ||
      !is.finite(task_start_timeout_seconds) ||
      task_start_timeout_seconds <= 0
  ) {
    abort_llw(
      "`task_start_timeout_seconds` must be one positive finite number.",
      type = "validation"
    )
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
    active_inspection_attempt <- reactiveVal(NULL)
    inspection_pending_error <- reactiveVal(NULL)
    inspection_terminal_override <- reactiveVal(NULL)
    inspection_controller_reset_required <- reactiveVal(FALSE)
    inspection_controller_revision <- reactiveVal(0L)
    planning_attempted <- reactiveVal(FALSE)
    pending_planning_attempt <- reactiveVal(NULL)
    active_planning_attempt <- reactiveVal(NULL)
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
    scope_input_revision <- reactiveVal(0L)
    scope_review_value <- reactiveVal(NULL)
    scope_review_error <- reactiveVal(NULL)
    transfer_revision <- reactiveVal(0L)
    transfer_review_key <- reactiveVal(NULL)
    transfer_review_value <- reactiveVal(NULL)
    transfer_review_error <- reactiveVal(NULL)
    transfer_preview_value <- reactiveVal(NULL)
    transfer_attempted <- reactiveVal(FALSE)
    pending_transfer_attempt <- reactiveVal(NULL)
    active_transfer_attempt <- reactiveVal(NULL)
    transfer_pending_error <- reactiveVal(NULL)
    transfer_terminal_override <- reactiveVal(NULL)
    transfer_controller_reset_required <- reactiveVal(FALSE)
    transfer_controller_revision <- reactiveVal(0L)
    transfer_progress_path <- reactiveVal(NULL)
    transfer_task <- NULL
    apply_revision <- reactiveVal(0L)
    full_import_value <- reactiveVal(NULL)
    library_add_request_value <- reactiveVal(NULL)
    library_added_value <- reactiveVal(NULL)
    library_add_error <- reactiveVal(NULL)
    library_add_status <- reactiveVal(NULL)
    apply_attempted <- reactiveVal(FALSE)
    pending_apply_attempt <- reactiveVal(NULL)
    active_apply_attempt <- reactiveVal(NULL)
    apply_pending_error <- reactiveVal(NULL)
    apply_terminal_override <- reactiveVal(NULL)
    apply_controller_reset_required <- reactiveVal(FALSE)
    apply_controller_revision <- reactiveVal(0L)
    apply_progress_path <- reactiveVal(NULL)
    apply_task <- NULL
    wizard_step <- reactiveVal("source")
    continue_after_planning <- reactiveVal(FALSE)

    revision_lookup <- function(context_id) context_revision()
    transfer_revision_lookup <- function(context_id) transfer_revision()
    apply_revision_lookup <- function(context_id) apply_revision()

    clear_transfer_state <- function(
      reset_inputs = FALSE,
      clear_review = TRUE,
      clear_apply = TRUE
    ) {
      assert_flag(reset_inputs, "reset_inputs")
      assert_flag(clear_review, "clear_review")
      assert_flag(clear_apply, "clear_apply")
      needs_reset <-
        !is.null(isolate(pending_transfer_attempt())) ||
        !is.null(isolate(active_transfer_attempt())) ||
        (!is.null(transfer_task) &&
          !identical(isolate(transfer_task$state()), "idle"))
      apply_needs_reset <-
        !is.null(isolate(pending_apply_attempt())) ||
        !is.null(isolate(active_apply_attempt())) ||
        (!is.null(apply_task) &&
          !identical(isolate(apply_task$state()), "idle"))
      pending_transfer_attempt(NULL)
      active_transfer_attempt(NULL)
      transfer_pending_error(NULL)
      transfer_terminal_override(new_task_status("glc_read"))
      transfer_controller_reset_required(needs_reset)
      transfer_review_key(NULL)
      if (clear_review) transfer_review_value(NULL)
      transfer_review_error(NULL)
      transfer_preview_value(NULL)
      transfer_attempted(FALSE)
      transfer_progress_path(NULL)
      transfer_revision(isolate(transfer_revision()) + 1L)
      if (needs_reset && !is.null(transfer_task)) {
        replace_transfer_task()
      }
      if (clear_apply) {
        pending_apply_attempt(NULL)
        active_apply_attempt(NULL)
        apply_pending_error(NULL)
        apply_terminal_override(new_task_status("glc_read"))
        apply_controller_reset_required(apply_needs_reset)
        full_import_value(NULL)
        library_add_request_value(NULL)
        library_added_value(NULL)
        library_add_error(NULL)
        library_add_status(NULL)
        apply_attempted(FALSE)
        apply_progress_path(NULL)
        apply_revision(isolate(apply_revision()) + 1L)
        if (apply_needs_reset && !is.null(apply_task)) {
          replace_apply_task()
        }
      }
      if (reset_inputs) {
        updateNumericInput(
          session,
          "transfer_row_budget",
          value = 20L
        )
        updateRadioButtons(
          session,
          "transfer_problems",
          selected = "error"
        )
      }
      invisible(NULL)
    }

    clear_scope_state <- function(reset_inputs = FALSE) {
      assert_flag(reset_inputs, "reset_inputs")
      if (reset_inputs) {
        scope_input_revision(isolate(scope_input_revision()) + 1L)
      }
      scope_review_value(NULL)
      scope_review_error(NULL)
      clear_transfer_state(reset_inputs = reset_inputs)
      invisible(NULL)
    }

    reset_metadata_filter_state <- function() {
      metadata_filter_revision(metadata_filter_revision() + 1L)
      refinement_value(NULL)
      refinement_error(NULL)
      clear_scope_state(reset_inputs = TRUE)
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
          active_inspection_attempt(NULL)
          inspection_terminal_override(NULL)
          inspection_controller_reset_required(FALSE)
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
      terminal_override <- inspection_terminal_override()
      if (!is.null(terminal_override)) return(terminal_override)
      inspection_task_status()
    })
    replace_inspection_task <- function() {
      inspection_task <<- create_inspection_task()
      inspection_controller_revision(inspection_controller_revision() + 1L)
      inspection_controller_reset_required(FALSE)
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
          active_planning_attempt(NULL)
          planning_pending_error(NULL)
          planning_terminal_override(NULL)
          planning_controller_reset_required(FALSE)
          plan_value(value)
          selected_compatibility_value(glc_plan_compatibility_default(value))
          reset_metadata_filter_state()
          continue_requested <- isolate(continue_after_planning())
          continue_after_planning(FALSE)
          if (
            isTRUE(continue_requested) &&
              nrow(value$compatibility_sets) > 0L
          ) {
            select_wizard_step("structure")
          }
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
      planning_controller_reset_required(FALSE)
      planning_task
    }
    create_transfer_task <- function() {
      task <- new_long_task(
        worker = preview_worker,
        task_type = "glc_read",
        runtime = runtime,
        revision_lookup = transfer_revision_lookup,
        on_result = function(value, spec) {
          review <- spec$payload$transfer_review
          validate_glc_transfer_preview_result(value, review)
          current_review <- transfer_review_value()
          if (
            is.null(current_review) ||
              !identical(current_review$fingerprint, review$fingerprint)
          ) {
            abort_llw(
              paste(
                "The bounded preview result no longer matches the current",
                "transfer review."
              ),
              type = "stale"
            )
          }
          active_transfer_attempt(NULL)
          transfer_pending_error(NULL)
          transfer_terminal_override(NULL)
          transfer_controller_reset_required(FALSE)
          transfer_preview_value(value)
          invisible(NULL)
        }
      )
      bslib::bind_task_button(
        task$extended_task,
        "transfer_preview",
        session = session
      )
      task
    }
    transfer_task <- create_transfer_task()
    current_transfer_task <- function() {
      transfer_controller_revision()
      transfer_task
    }
    transfer_task_state <- reactive({
      current_transfer_task()$state()
    })
    transfer_task_status <- reactive({
      current_transfer_task()$status()
    })
    transfer_display_status <- reactive({
      pending_error <- transfer_pending_error()
      if (!is.null(pending_error)) {
        return(new_task_status(
          "glc_read",
          state = "error",
          message = llw_public_message(pending_error),
          error = pending_error
        ))
      }
      if (!is.null(pending_transfer_attempt())) {
        return(new_task_status(
          "glc_read",
          state = "queued",
          message = "Waiting for the previous preview task to stop cleanly."
        ))
      }
      terminal_override <- transfer_terminal_override()
      if (!is.null(terminal_override)) return(terminal_override)
      transfer_task_status()
    })
    replace_transfer_task <- function() {
      transfer_task <<- create_transfer_task()
      transfer_controller_revision(transfer_controller_revision() + 1L)
      transfer_controller_reset_required(FALSE)
      transfer_task
    }
    create_apply_task <- function() {
      task <- new_long_task(
        worker = apply_worker,
        task_type = "glc_read",
        runtime = runtime,
        revision_lookup = apply_revision_lookup,
        on_result = function(value, spec) {
          review <- spec$payload$transfer_review
          validate_glc_full_import_result(value, review)
          current_review <- transfer_review_value()
          if (
            is.null(current_review) ||
              !identical(
                glc_full_import_contract_fingerprint(current_review),
                glc_full_import_contract_fingerprint(review)
              )
          ) {
            abort_llw(
              paste(
                "The full-import result no longer matches the current",
                "transfer review."
              ),
              type = "stale"
            )
          }
          active_apply_attempt(NULL)
          apply_pending_error(NULL)
          apply_terminal_override(NULL)
          apply_controller_reset_required(FALSE)
          full_import_value(value)
          library_add_request_value(NULL)
          library_added_value(NULL)
          library_add_error(NULL)
          library_add_status(NULL)
          invisible(NULL)
        }
      )
      bslib::bind_task_button(
        task$extended_task,
        "apply_full_import",
        session = session
      )
      task
    }
    apply_task <- create_apply_task()
    current_apply_task <- function() {
      apply_controller_revision()
      apply_task
    }
    apply_task_state <- reactive({
      current_apply_task()$state()
    })
    apply_task_status <- reactive({
      current_apply_task()$status()
    })
    apply_display_status <- reactive({
      pending_error <- apply_pending_error()
      if (!is.null(pending_error)) {
        return(new_task_status(
          "glc_read",
          state = "error",
          message = llw_public_message(pending_error),
          error = pending_error
        ))
      }
      if (!is.null(pending_apply_attempt())) {
        return(new_task_status(
          "glc_read",
          state = "queued",
          message = "Waiting for the previous full import to stop cleanly."
        ))
      }
      terminal_override <- apply_terminal_override()
      if (!is.null(terminal_override)) return(terminal_override)
      apply_task_status()
    })
    replace_apply_task <- function() {
      apply_task <<- create_apply_task()
      apply_controller_revision(apply_controller_revision() + 1L)
      apply_controller_reset_required(FALSE)
      apply_task
    }

    cancellation_drain_for_display <- function(status) {
      active_count <- runtime$active_count()
      draining <- inherits(status, "llw_task_status") &&
        identical(status$state, "cancelled") &&
        is.numeric(active_count) &&
        length(active_count) == 1L &&
        !is.na(active_count) &&
        active_count > 0
      if (draining) invalidateLater(250, session)
      glc_cancellation_drain_ui(status, active_count)
    }

    build_projection_review <- function(source_variables) {
      inspection <- inspection_value()
      parent_request <- submitted_planning_request()
      parent_plan <- plan_value()
      parent_compatibility_id <- selected_compatibility_id()
      parent_refinement <- refinement_value()
      if (
        is.null(inspection) ||
          is.null(parent_request) ||
          is.null(parent_plan) ||
          is.null(parent_compatibility_id) ||
          is.null(parent_refinement)
      ) {
        abort_llw(
          "Complete and confirm the refined selection before choosing variables.",
          type = "validation"
        )
      }
      source <- glc_inspection_source_spec(inspection, cache_dir)
      new_glc_projection_review(
        source = source,
        source_variables = source_variables,
        parent_request = parent_request,
        parent_plan = parent_plan,
        parent_compatibility_id = parent_compatibility_id,
        parent_refinement = parent_refinement,
        metadata_filters = current_metadata_filters(),
        source_metadata_snapshot = inspection$metadata_snapshot
      )
    }

    prepare_projection_review <- function() {
      scope_review_value(NULL)
      scope_review_error(NULL)
      result <- tryCatch(
        {
          variables <- isolate(scope_projection_variables())
          if (length(variables) == 0L) {
            abort_llw(
              "Choose at least one exact source variable for the output.",
              type = "validation"
            )
          }
          build_projection_review(variables)
        },
        error = identity
      )
      if (inherits(result, "error")) {
        scope_review_error(result)
        return(invisible(FALSE))
      }
      scope_review_value(result)
      invisible(TRUE)
    }

    build_transfer_review <- function(
      total_preview_rows,
      problems
    ) {
      projection <- scope_review_value()
      if (is.null(projection)) {
        abort_llw(
          "Review the output variables before preparing a transfer.",
          type = "validation"
        )
      }
      validate_glc_projection_review(projection)
      available <- as.numeric(runtime$assert_capacity(0))
      new_glc_transfer_review(
        projection_review = projection,
        total_preview_rows = total_preview_rows,
        problems = problems,
        available_session_bytes = available
      )
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

      if (
        glc_inspection_attempt_expired(
          attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        pending_inspection_attempt(NULL)
        inspection_controller_reset_required(TRUE)
        replace_inspection_task()
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

      if (
        !identical(isolate(task$state()), "idle") ||
          isTRUE(isolate(inspection_controller_reset_required()))
      ) {
        task <- replace_inspection_task()
      }
      task_id <- task$invoke(
        attempt$payload,
        dataset_id = "glc-source-context"
      )
      if (isFALSE(task_id)) return(invisible(FALSE))
      inspection_progress_path(attempt$progress_path)
      attempt$invoked_at <- Sys.time()
      active_inspection_attempt(attempt)
      pending_inspection_attempt(NULL)
      inspection_terminal_override(NULL)
      inspection_controller_reset_required(FALSE)
      invisible(task_id)
    }

    start_inspection <- function() {
      inspection_attempted(TRUE)
      inspection_pending_error(NULL)
      inspection_terminal_override(NULL)
      inspection_progress_path(NULL)
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

    inspection_attempt_poll <- reactiveTimer(100, session)
    observe({
      inspection_attempt_poll()
      if (!is.null(pending_inspection_attempt())) {
        result <- isolate(tryCatch(
          submit_pending_inspection(),
          error = identity
        ))
        if (inherits(result, "error")) {
          pending_inspection_attempt(NULL)
          inspection_controller_reset_required(TRUE)
          inspection_pending_error(normalize_task_error(
            result,
            "glc_discovery"
          ))
        }
      }

      attempt <- isolate(active_inspection_attempt())
      if (is.null(attempt)) return(invisible(NULL))
      if (
        !glc_inspection_attempt_is_current(
          attempt,
          isolate(context_revision()),
          glc_source_selection_key(isolate(source_selection()))
        )
      ) {
        active_inspection_attempt(NULL)
        return(invisible(NULL))
      }
      task <- isolate(current_inspection_task())
      task_state <- isolate(task$state())
      progress <- read_glc_task_progress(attempt$progress_path)
      if (!task_state %in% c("queued", "running", "finalizing")) {
        active_inspection_attempt(NULL)
        return(invisible(NULL))
      }
      if (!is.null(progress) && nrow(progress) > 0L) {
        return(invisible(NULL))
      }
      startup_attempt <- attempt
      startup_attempt$requested_at <- attempt$invoked_at
      if (
        glc_inspection_attempt_expired(
          startup_attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        active_inspection_attempt(NULL)
        inspection_controller_reset_required(TRUE)
        context_revision(isolate(context_revision()) + 1L)
        replace_inspection_task()
        inspection_pending_error(new_llw_error(
          message = paste(
            "The inspection worker did not begin before the invocation",
            "deadline."
          ),
          type = "resource",
          public_message = paste(
            "Package inspection could not start in time.",
            "Retry when background capacity is available."
          )
        ))
      }
      invisible(NULL)
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
        inspection_needs_reset <-
          !is.null(isolate(pending_inspection_attempt())) ||
          !is.null(isolate(active_inspection_attempt())) ||
          !identical(isolate(inspection_task_state()), "idle")
        planning_needs_reset <-
          !is.null(isolate(pending_planning_attempt())) ||
          !is.null(isolate(active_planning_attempt())) ||
          !identical(isolate(planning_task_state()), "idle")
        pending_inspection_attempt(NULL)
        active_inspection_attempt(NULL)
        inspection_pending_error(NULL)
        inspection_terminal_override(new_task_status("glc_discovery"))
        inspection_controller_reset_required(inspection_needs_reset)
        pending_planning_attempt(NULL)
        active_planning_attempt(NULL)
        planning_pending_error(NULL)
        planning_terminal_override(new_task_status("glc_discovery"))
        planning_controller_reset_required(planning_needs_reset)
        continue_after_planning(FALSE)
        active_source_key(key)
        active_arrival_mode(arrival_mode)
        context_revision(context_revision() + 1L)
        if (inspection_needs_reset) replace_inspection_task()
        if (planning_needs_reset) replace_planning_task()
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
        had_attempt <-
          !is.null(isolate(pending_inspection_attempt())) ||
          !is.null(isolate(active_inspection_attempt()))
        pending_inspection_attempt(NULL)
        active_inspection_attempt(NULL)
        inspection_pending_error(NULL)
        if (had_attempt) {
          # Do not hard-stop the mirai job here. new_long_task() releases the
          # session runtime slot from its completion observer. Invalidating the
          # context makes the old result stale while allowing that release to
          # happen before a queued retry starts on its fresh controller.
          context_revision(isolate(context_revision()) + 1L)
          inspection_controller_reset_required(TRUE)
          replace_inspection_task()
          inspection_terminal_override(new_task_status(
            "glc_discovery",
            state = "cancelled",
            message = "Cancellation was requested."
          ))
        }
      },
      ignoreInit = TRUE
    )

    reset_planning_selection <- function() {
      planning_needs_reset <-
        !is.null(isolate(pending_planning_attempt())) ||
        !is.null(isolate(active_planning_attempt())) ||
        !identical(isolate(planning_task_state()), "idle")
      pending_planning_attempt(NULL)
      active_planning_attempt(NULL)
      planning_pending_error(NULL)
      planning_terminal_override(new_task_status("glc_discovery"))
      planning_controller_reset_required(planning_needs_reset)
      continue_after_planning(FALSE)
      plan_value(NULL)
      submitted_planning_request(NULL)
      planning_attempted(FALSE)
      plan_progress_path(NULL)
      selected_compatibility_value(NULL)
      reset_metadata_filter_state()
      context_revision(context_revision() + 1L)
      if (planning_needs_reset) replace_planning_task()
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

      if (
        glc_inspection_attempt_expired(
          attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        pending_planning_attempt(NULL)
        planning_controller_reset_required(TRUE)
        replace_planning_task()
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
      attempt$invoked_at <- Sys.time()
      active_planning_attempt(attempt)
      pending_planning_attempt(NULL)
      planning_terminal_override(NULL)
      planning_controller_reset_required(FALSE)
      invisible(task_id)
    }

    start_planning <- function() {
      planning_attempted(TRUE)
      planning_pending_error(NULL)
      planning_terminal_override(NULL)
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
            file_group_ids = modality_groups,
            variable_scope = "all"
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
        continue_after_planning(FALSE)
        planning_pending_error(attempted)
      }
      invisible(attempted)
    }

    planning_attempt_poll <- reactiveTimer(100, session)
    observe({
      planning_attempt_poll()
      if (!is.null(pending_planning_attempt())) {
        result <- isolate(tryCatch(
          submit_pending_planning(),
          error = identity
        ))
        if (inherits(result, "error")) {
          pending_planning_attempt(NULL)
          continue_after_planning(FALSE)
          planning_controller_reset_required(TRUE)
          planning_pending_error(normalize_task_error(
            result,
            "glc_discovery"
          ))
        }
      }

      attempt <- isolate(active_planning_attempt())
      if (is.null(attempt)) return(invisible(NULL))
      if (
        !glc_planning_attempt_is_current(
          attempt,
          isolate(context_revision()),
          glc_source_selection_key(isolate(source_selection())),
          isolate(selected_term_value()),
          isolate(selected_modality_value())
        )
      ) {
        active_planning_attempt(NULL)
        continue_after_planning(FALSE)
        return(invisible(NULL))
      }
      task <- isolate(current_planning_task())
      task_state <- isolate(task$state())
      progress <- read_glc_task_progress(attempt$progress_path)
      if (!task_state %in% c("queued", "running", "finalizing")) {
        active_planning_attempt(NULL)
        if (!identical(task_state, "complete")) {
          continue_after_planning(FALSE)
        }
        return(invisible(NULL))
      }
      if (!is.null(progress) && nrow(progress) > 0L) {
        return(invisible(NULL))
      }
      startup_attempt <- attempt
      startup_attempt$requested_at <- attempt$invoked_at
      if (
        glc_inspection_attempt_expired(
          startup_attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        active_planning_attempt(NULL)
        continue_after_planning(FALSE)
        planning_controller_reset_required(TRUE)
        context_revision(isolate(context_revision()) + 1L)
        replace_planning_task()
        planning_pending_error(new_llw_error(
          message = paste(
            "The planning worker did not begin before the invocation",
            "deadline."
          ),
          type = "resource",
          public_message = paste(
            "Compatibility planning could not start in time.",
            "Retry when background capacity is available."
          )
        ))
      }
      invisible(NULL)
    })

    observeEvent(
      input$plan,
      {
        continue_after_planning(FALSE)
        start_planning()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$cancel_plan,
      {
        had_attempt <-
          !is.null(isolate(pending_planning_attempt())) ||
          !is.null(isolate(active_planning_attempt()))
        pending_planning_attempt(NULL)
        active_planning_attempt(NULL)
        continue_after_planning(FALSE)
        planning_pending_error(NULL)
        if (had_attempt) {
          # Planning shares the same session runtime slot and therefore uses
          # the same logical-cancellation and natural-release invariant.
          context_revision(isolate(context_revision()) + 1L)
          planning_controller_reset_required(TRUE)
          replace_planning_task()
          planning_terminal_override(new_task_status(
            "glc_discovery",
            state = "cancelled",
            message = "Cancellation was requested."
          ))
        }
      },
      ignoreInit = TRUE
    )

    submit_pending_transfer <- function() {
      attempt <- isolate(pending_transfer_attempt())
      if (is.null(attempt)) return(invisible(FALSE))
      review <- isolate(transfer_review_value())
      review_fingerprint <- if (is.null(review)) NULL else review$fingerprint
      if (
        !glc_transfer_attempt_is_current(
          attempt,
          isolate(transfer_revision()),
          glc_source_selection_key(isolate(source_selection())),
          review_fingerprint
        )
      ) {
        pending_transfer_attempt(NULL)
        return(invisible(FALSE))
      }

      if (
        glc_inspection_attempt_expired(
          attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        pending_transfer_attempt(NULL)
        transfer_controller_reset_required(TRUE)
        replace_transfer_task()
        transfer_pending_error(new_llw_error(
          message = paste(
            "The previous bounded preview controller did not release before",
            "the new invocation deadline."
          ),
          type = "resource",
          public_message = paste(
            "The previous bounded preview is still stopping.",
            "Wait a moment, then retry."
          )
        ))
        return(invisible(FALSE))
      }

      task <- isolate(current_transfer_task())
      released <- glc_inspection_controller_released(
        isolate(task$state()),
        isolate(task$extended_task$status()),
        runtime$active_count()
      )
      if (!released) return(invisible(FALSE))

      if (
        !identical(isolate(task$state()), "idle") ||
          isTRUE(isolate(transfer_controller_reset_required()))
      ) {
        task <- replace_transfer_task()
      }
      task_id <- task$invoke(
        attempt$payload,
        dataset_id = "glc-transfer-context"
      )
      if (isFALSE(task_id)) return(invisible(FALSE))
      transfer_progress_path(attempt$progress_path)
      attempt$invoked_at <- Sys.time()
      active_transfer_attempt(attempt)
      pending_transfer_attempt(NULL)
      transfer_terminal_override(NULL)
      transfer_controller_reset_required(FALSE)
      invisible(task_id)
    }

    start_transfer <- function() {
      transfer_attempted(TRUE)
      transfer_pending_error(NULL)
      transfer_terminal_override(NULL)
      transfer_preview_value(NULL)
      transfer_progress_path(NULL)
      attempted <- tryCatch(
        {
          review <- isolate(transfer_review_value())
          if (is.null(review)) {
            abort_llw(
              "Review the bounded transfer contract before previewing.",
              type = "validation"
            )
          }
          validate_glc_transfer_review(review)
          if (isTRUE(review$resource$blocked)) {
            abort_llw(
              paste(
                "Known bytes for the canonical preview subset exceed the",
                "active session resource budget."
              ),
              type = "resource",
              public_message = paste(
                "This bounded preview exceeds the current session resource",
                "limit and cannot start."
              )
            )
          }
          progress_path <- file.path(
            runtime$paths$tasks,
            paste0(new_stable_id("glc_transfer_progress"), ".rds")
          )
          inspection <- isolate(inspection_value())
          if (is.null(inspection)) {
            abort_llw(
              "The accepted inspection is no longer available.",
              type = "stale",
              public_message = paste(
                "The accepted package inspection changed.",
                "Return to inspection before retrying the preview."
              )
            )
          }
          source <- glc_inspection_source_spec(inspection, cache_dir)
          pending_transfer_attempt(list(
            payload = new_glc_transfer_task_payload(
              source,
              review,
              progress_path = progress_path
            ),
            progress_path = progress_path,
            transfer_revision = isolate(transfer_revision()),
            source_key = glc_source_selection_key(isolate(source_selection())),
            review_fingerprint = review$fingerprint,
            requested_at = Sys.time()
          ))
          submit_pending_transfer()
        },
        error = identity
      )
      if (inherits(attempted, "error")) {
        pending_transfer_attempt(NULL)
        transfer_pending_error(normalize_task_error(attempted, "glc_read"))
      }
      invisible(attempted)
    }

    transfer_attempt_poll <- reactiveTimer(100, session)
    observe({
      transfer_attempt_poll()
      if (!is.null(pending_transfer_attempt())) {
        result <- isolate(tryCatch(
          submit_pending_transfer(),
          error = identity
        ))
        if (inherits(result, "error")) {
          pending_transfer_attempt(NULL)
          transfer_controller_reset_required(TRUE)
          transfer_pending_error(normalize_task_error(result, "glc_read"))
        }
      }

      attempt <- isolate(active_transfer_attempt())
      if (is.null(attempt)) return(invisible(NULL))
      review <- isolate(transfer_review_value())
      review_fingerprint <- if (is.null(review)) NULL else review$fingerprint
      if (
        !glc_transfer_attempt_is_current(
          attempt,
          isolate(transfer_revision()),
          glc_source_selection_key(isolate(source_selection())),
          review_fingerprint
        )
      ) {
        active_transfer_attempt(NULL)
        return(invisible(NULL))
      }
      task <- isolate(current_transfer_task())
      task_state <- isolate(task$state())
      progress <- read_glc_task_progress(attempt$progress_path)
      if (!task_state %in% c("queued", "running", "finalizing")) {
        active_transfer_attempt(NULL)
        return(invisible(NULL))
      }
      if (!is.null(progress) && nrow(progress) > 0L) {
        return(invisible(NULL))
      }
      startup_attempt <- attempt
      startup_attempt$requested_at <- attempt$invoked_at
      if (
        glc_inspection_attempt_expired(
          startup_attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        active_transfer_attempt(NULL)
        transfer_controller_reset_required(TRUE)
        transfer_revision(isolate(transfer_revision()) + 1L)
        replace_transfer_task()
        transfer_pending_error(new_llw_error(
          message = paste(
            "The bounded preview worker did not begin before the invocation",
            "deadline."
          ),
          type = "resource",
          public_message = paste(
            "The bounded preview could not start in time.",
            "Retry when background capacity is available."
          )
        ))
      }
      invisible(NULL)
    })

    observeEvent(
      input$transfer_preview,
      {
        start_transfer()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$cancel_transfer,
      {
        had_attempt <-
          !is.null(isolate(pending_transfer_attempt())) ||
          !is.null(isolate(active_transfer_attempt()))
        pending_transfer_attempt(NULL)
        active_transfer_attempt(NULL)
        transfer_pending_error(NULL)
        if (had_attempt) {
          # Invalidating this session-local transfer generation prevents a
          # late preview from applying. The worker retains its runtime slot
          # until its public API call stops naturally or reaches its absolute
          # timeout, after which an immediate retry can start on a fresh task.
          transfer_revision(isolate(transfer_revision()) + 1L)
          transfer_controller_reset_required(TRUE)
          replace_transfer_task()
          transfer_terminal_override(new_task_status(
            "glc_read",
            state = "cancelled",
            message = "Cancellation was requested."
          ))
        }
      },
      ignoreInit = TRUE
    )

    submit_pending_apply <- function() {
      attempt <- isolate(pending_apply_attempt())
      if (is.null(attempt)) return(invisible(FALSE))
      review <- isolate(transfer_review_value())
      contract_fingerprint <- if (is.null(review)) {
        NULL
      } else {
        glc_full_import_contract_fingerprint(review)
      }
      if (
        !glc_full_import_attempt_is_current(
          attempt,
          isolate(apply_revision()),
          glc_source_selection_key(isolate(source_selection())),
          contract_fingerprint
        )
      ) {
        pending_apply_attempt(NULL)
        return(invisible(FALSE))
      }

      if (
        glc_inspection_attempt_expired(
          attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        pending_apply_attempt(NULL)
        apply_controller_reset_required(TRUE)
        replace_apply_task()
        apply_pending_error(new_llw_error(
          message = paste(
            "The previous full-import controller did not release before the",
            "new invocation deadline."
          ),
          type = "resource",
          public_message = paste(
            "The previous full import is still stopping.",
            "Wait a moment, then retry."
          )
        ))
        return(invisible(FALSE))
      }

      task <- isolate(current_apply_task())
      released <- glc_inspection_controller_released(
        isolate(task$state()),
        isolate(task$extended_task$status()),
        runtime$active_count()
      )
      if (!released) return(invisible(FALSE))

      if (
        !identical(isolate(task$state()), "idle") ||
          isTRUE(isolate(apply_controller_reset_required()))
      ) {
        task <- replace_apply_task()
      }
      task_id <- task$invoke(
        attempt$payload,
        dataset_id = "glc-full-import-context"
      )
      if (isFALSE(task_id)) return(invisible(FALSE))
      apply_progress_path(attempt$progress_path)
      attempt$invoked_at <- Sys.time()
      active_apply_attempt(attempt)
      pending_apply_attempt(NULL)
      apply_terminal_override(NULL)
      apply_controller_reset_required(FALSE)
      invisible(task_id)
    }

    start_apply <- function() {
      apply_attempted(TRUE)
      apply_pending_error(NULL)
      apply_terminal_override(NULL)
      apply_progress_path(NULL)
      attempted <- tryCatch(
        {
          review <- isolate(transfer_review_value())
          if (is.null(review)) {
            abort_llw(
              "Review the transfer contract before applying the full import.",
              type = "validation"
            )
          }
          validate_glc_transfer_review(review)
          review <- glc_full_import_contract_review(review)
          if (isTRUE(review$resource$full_import_blocked)) {
            abort_llw(
              paste(
                "Known bytes for the complete selected file set exceed the",
                "active session resource budget."
              ),
              type = "resource",
              public_message = paste(
                "Apply is blocked because the known selected bytes exceed",
                "this session's resource limit. Narrow the selection first."
              )
            )
          }
          runtime$assert_capacity(review$resource$known_selected_bytes)
          progress_path <- file.path(
            runtime$paths$tasks,
            paste0(new_stable_id("glc_full_import_progress"), ".rds")
          )
          inspection <- isolate(inspection_value())
          if (is.null(inspection)) {
            abort_llw(
              "The accepted inspection is no longer available.",
              type = "stale",
              public_message = paste(
                "The accepted package inspection changed.",
                "Return to inspection before retrying Apply."
              )
            )
          }
          source <- glc_inspection_source_spec(inspection, cache_dir)
          pending_apply_attempt(list(
            payload = new_glc_full_import_task_payload(
              source,
              review,
              progress_path = progress_path
            ),
            progress_path = progress_path,
            apply_revision = isolate(apply_revision()),
            source_key = glc_source_selection_key(isolate(source_selection())),
            contract_fingerprint = glc_full_import_contract_fingerprint(review),
            requested_at = Sys.time()
          ))
          submit_pending_apply()
        },
        error = identity
      )
      if (inherits(attempted, "error")) {
        pending_apply_attempt(NULL)
        apply_pending_error(normalize_task_error(attempted, "glc_read"))
      }
      invisible(attempted)
    }

    apply_attempt_poll <- reactiveTimer(100, session)
    observe({
      apply_attempt_poll()
      if (!is.null(pending_apply_attempt())) {
        result <- isolate(tryCatch(
          submit_pending_apply(),
          error = identity
        ))
        if (inherits(result, "error")) {
          pending_apply_attempt(NULL)
          apply_controller_reset_required(TRUE)
          apply_pending_error(normalize_task_error(result, "glc_read"))
        }
      }

      attempt <- isolate(active_apply_attempt())
      if (is.null(attempt)) return(invisible(NULL))
      review <- isolate(transfer_review_value())
      contract_fingerprint <- if (is.null(review)) {
        NULL
      } else {
        glc_full_import_contract_fingerprint(review)
      }
      if (
        !glc_full_import_attempt_is_current(
          attempt,
          isolate(apply_revision()),
          glc_source_selection_key(isolate(source_selection())),
          contract_fingerprint
        )
      ) {
        active_apply_attempt(NULL)
        return(invisible(NULL))
      }
      task <- isolate(current_apply_task())
      task_state <- isolate(task$state())
      progress <- read_glc_task_progress(attempt$progress_path)
      if (!task_state %in% c("queued", "running", "finalizing")) {
        active_apply_attempt(NULL)
        return(invisible(NULL))
      }
      if (!is.null(progress) && nrow(progress) > 0L) {
        return(invisible(NULL))
      }
      startup_attempt <- attempt
      startup_attempt$requested_at <- attempt$invoked_at
      if (
        glc_inspection_attempt_expired(
          startup_attempt,
          timeout_seconds = task_start_timeout_seconds
        )
      ) {
        active_apply_attempt(NULL)
        apply_controller_reset_required(TRUE)
        apply_revision(isolate(apply_revision()) + 1L)
        replace_apply_task()
        apply_pending_error(new_llw_error(
          message = paste(
            "The full-import worker did not begin before the invocation",
            "deadline."
          ),
          type = "resource",
          public_message = paste(
            "The full import could not start in time.",
            "Retry when background capacity is available."
          )
        ))
      }
      invisible(NULL)
    })

    observeEvent(
      input$apply_full_import,
      {
        start_apply()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$cancel_apply,
      {
        had_attempt <-
          !is.null(isolate(pending_apply_attempt())) ||
          !is.null(isolate(active_apply_attempt()))
        pending_apply_attempt(NULL)
        active_apply_attempt(NULL)
        apply_pending_error(NULL)
        if (had_attempt) {
          apply_revision(isolate(apply_revision()) + 1L)
          apply_controller_reset_required(TRUE)
          replace_apply_task()
          apply_terminal_override(new_task_status(
            "glc_read",
            state = "cancelled",
            message = "Cancellation was requested."
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

    refined_file_group_ids <- reactive({
      refinement <- refinement_value()
      if (is.null(refinement)) return(character())
      glc_nonempty_unique(refinement$request$file_group)
    })

    scope_variable_catalogue <- reactive({
      inspection <- inspection_value()
      group_ids <- refined_file_group_ids()
      if (is.null(inspection) || length(group_ids) == 0L) {
        return(data.frame(
          name = character(),
          display = character(),
          group_count = integer(),
          stringsAsFactors = FALSE
        ))
      }
      catalogue <- glc_scope_variable_catalogue(inspection, group_ids)
      catalogue[
        catalogue$group_count == length(group_ids),
        ,
        drop = FALSE
      ]
    })

    selected_scope_mode <- reactive({
      parent_request <- submitted_planning_request()
      if (is.null(parent_request)) return("all")
      allowed <- c(
        if (!is.null(parent_request$term)) "discovery",
        "all",
        "custom"
      )
      default <- if (!is.null(parent_request$term)) "discovery" else "all"
      value <- input[[glc_scope_input_id(
        "scope_mode",
        scope_input_revision()
      )]] %||%
        default
      if (!value %in% allowed) default else value
    })

    selected_custom_variables <- reactive({
      catalogue <- scope_variable_catalogue()
      selected <- glc_nonempty_unique(input[[glc_scope_input_id(
        "scope_custom_variables",
        scope_input_revision()
      )]])
      selected[selected %in% catalogue$name]
    })

    scope_projection_variables <- reactive({
      parent_request <- submitted_planning_request()
      parent_plan <- plan_value()
      refinement <- refinement_value()
      group_ids <- refined_file_group_ids()
      if (
        is.null(parent_request) ||
          is.null(parent_plan) ||
          is.null(refinement) ||
          length(group_ids) == 0L
      ) {
        return(character())
      }
      mode <- selected_scope_mode()
      if (identical(mode, "custom")) return(selected_custom_variables())
      catalogue_names <- scope_variable_catalogue()$name
      if (identical(mode, "all")) return(catalogue_names)
      inspection <- inspection_value()
      term <- parent_request$term
      term_row <- inspection$terms[
        inspection$terms$term == term,
        ,
        drop = FALSE
      ]
      names <- if (nrow(term_row) == 1L) {
        glc_nonempty_unique(term_row$source_names[[1L]])
      } else {
        character()
      }
      names[names %in% catalogue_names]
    })

    observe({
      selected_compatibility_id()
      current_metadata_filters()
      refinement_value(NULL)
      refinement_error(NULL)
      clear_scope_state(reset_inputs = TRUE)
    })

    run_current_refinement <- function() {
      clear_scope_state()
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
                "Adjust the filters before continuing."
              )
            )
          }
          withProgress(
            message = "Confirming the filtered groups",
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
        return(invisible(FALSE))
      }
      refinement_error(NULL)
      refinement_value(result)
      invisible(TRUE)
    }

    # Keep the legacy server event for deterministic module fixtures while the
    # production UI uses the single confirm-and-continue action below.
    observeEvent(
      input$refine,
      run_current_refinement(),
      ignoreInit = TRUE
    )

    observeEvent(
      input$continue_variables,
      {
        if (isTRUE(run_current_refinement())) {
          select_wizard_step("variables")
        }
      },
      ignoreInit = TRUE
    )

    observe({
      revision <- scope_input_revision()
      value <- input[[glc_scope_input_id("scope_mode", revision)]]
      req(!is.null(value))
      clear_scope_state()
    })

    observe({
      revision <- scope_input_revision()
      value <- input[[glc_scope_input_id(
        "scope_custom_variables",
        revision
      )]]
      req(!is.null(value))
      clear_scope_state()
    })

    observeEvent(
      input$scope_apply,
      {
        withProgress(
          message = "Reviewing output variables",
          detail = "Validating the fixed projection without reopening data.",
          value = 0.5,
          prepare_projection_review()
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$continue_preview,
      {
        if (isTRUE(prepare_projection_review())) {
          select_wizard_step("preview")
        }
      },
      ignoreInit = TRUE
    )

    observe({
      projection <- scope_review_value()
      total_preview_rows <- input$transfer_row_budget
      problems <- input$transfer_problems
      if (
        is.null(projection) ||
          is.null(problems)
      ) {
        return(invisible(NULL))
      }
      key <- glc_transfer_value_fingerprint(list(
        projection_fingerprint = glc_transfer_value_fingerprint(projection),
        total_preview_rows = total_preview_rows,
        problems = as.character(problems)
      ))
      if (identical(key, isolate(transfer_review_key()))) {
        return(invisible(NULL))
      }
      clear_transfer_state(
        reset_inputs = FALSE,
        clear_review = FALSE,
        clear_apply = FALSE
      )
      result <- tryCatch(
        build_transfer_review(total_preview_rows, problems),
        error = identity
      )
      transfer_review_key(key)
      if (inherits(result, "error")) {
        transfer_review_error(result)
      } else {
        transfer_review_value(result)
      }
      invisible(NULL)
    })

    output$wizard_stepper <- renderUI({
      selection <- source_selection()
      glc_wizard_stepper(
        session$ns,
        active = wizard_step(),
        source_ready = !is.null(selection) && isTRUE(selection$has_latest_pass),
        inspection_ready = !is.null(inspection_value()),
        plan_ready = !is.null(plan_value()),
        structure_ready = !is.null(selected_compatibility_id()),
        refinement_ready = !is.null(refinement_value()),
        scope_ready = !is.null(scope_review_value())
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
            is.null(active_inspection_attempt())
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
      if (is.null(refinement_value())) {
        showNotification(
          "Confirm the metadata-filtered groups before choosing variables.",
          type = "warning",
          duration = 5
        )
      } else {
        select_wizard_step("variables")
      }
    }) |>
      bindEvent(input$wizard_step_variables, ignoreInit = TRUE)

    observe({
      if (is.null(scope_review_value())) {
        showNotification(
          "Review the output variables before opening transfer review.",
          type = "warning",
          duration = 5
        )
      } else {
        select_wizard_step("preview")
      }
    }) |>
      bindEvent(input$wizard_step_preview, ignoreInit = TRUE)

    observe({
      select_wizard_step("inspect")
      if (
        is.null(inspection_value()) &&
          is.null(pending_inspection_attempt()) &&
          is.null(active_inspection_attempt())
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
    observeEvent(
      input$continue_structure,
      {
        plan <- isolate(plan_value())
        if (
          !is.null(plan) &&
            nrow(plan$compatibility_sets) > 0L
        ) {
          select_wizard_step("structure")
          return(invisible(NULL))
        }
        request_ready <-
          !is.null(isolate(inspection_value())) &&
          (!is.null(isolate(selected_term_value())) ||
            !is.null(isolate(selected_modality_value())))
        busy <-
          !is.null(isolate(pending_planning_attempt())) ||
          !is.null(isolate(active_planning_attempt()))
        if (!request_ready || busy) return(invisible(NULL))
        continue_after_planning(TRUE)
        started <- start_planning()
        if (inherits(started, "error")) {
          continue_after_planning(FALSE)
        }
        invisible(NULL)
      },
      ignoreInit = TRUE
    )
    observe(select_wizard_step("narrow")) |>
      bindEvent(input$continue_narrow, ignoreInit = TRUE)
    observe(select_wizard_step("discovery")) |>
      bindEvent(input$back_discovery, ignoreInit = TRUE)
    observe(select_wizard_step("structure")) |>
      bindEvent(input$back_structure, ignoreInit = TRUE)
    observe(select_wizard_step("narrow")) |>
      bindEvent(input$back_scope, ignoreInit = TRUE)
    observe(select_wizard_step("variables")) |>
      bindEvent(input$back_preview, ignoreInit = TRUE)

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

    output$inspect_button <- renderUI({
      inspected <- !is.null(inspection_value())
      button <- input_task_button(
        session$ns("inspect"),
        span(strong("Inspect package")),
        icon = icon("magnifying-glass"),
        class = "btn-primary btn-lg"
      )
      if (!inspected) return(button)
      tagAppendAttributes(
        button,
        disabled = "disabled",
        `aria-disabled` = "true",
        title = paste(
          "This exact package revision is already inspected.",
          "Change the source to inspect another revision."
        )
      )
    })

    output$cancel_inspection_button <- renderUI({
      glc_cancel_task_button(
        session$ns,
        "cancel_inspection",
        !is.null(pending_inspection_attempt()) ||
          !is.null(active_inspection_attempt()),
        "Cancel inspection"
      )
    })

    output$inspection_cancellation_drain <- renderUI({
      cancellation_drain_for_display(inspection_display_status())
    })

    output$inspection_progress <- renderUI({
      status <- inspection_display_status()
      state <- status$state
      pending <- !is.null(pending_inspection_attempt())
      active <- pending || !is.null(active_inspection_attempt())
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
          "The selected canonical term and modality identify candidate file",
          "groups. glcdp then validates each candidate's complete file schema",
          "to establish groups that can be read and collected together.",
          "Output variables are chosen later in Step 6."
        )
      } else if (!is.null(term)) {
        paste(
          "The selected canonical term identifies candidate file groups.",
          "glcdp then validates each candidate's complete file schema to",
          "establish groups that can be read and collected together.",
          "Output variables are chosen later in Step 6."
        )
      } else if (!is.null(modality)) {
        paste(
          "The selected modality identifies candidate file groups. glcdp",
          "then validates each candidate's complete file schema to establish",
          "groups that can be read and collected together. Output variables",
          "are chosen later in Step 6."
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
          !is.null(active_planning_attempt()),
        "Cancel planning"
      )
    })

    output$planning_cancellation_drain <- renderUI({
      cancellation_drain_for_display(planning_display_status())
    })

    output$plan_progress <- renderUI({
      status <- planning_display_status()
      state <- status$state
      pending <- !is.null(pending_planning_attempt())
      active <- pending || !is.null(active_planning_attempt())
      if (active) {
        invalidateLater(250, session)
      }
      progress <- glc_task_progress_ui(
        glc_planning_progress_for_display(
          read_glc_task_progress(plan_progress_path())
        ),
        total_phases = 3L,
        heading = "Structural compatibility planning",
        idle_message = paste(
          "Choose a canonical term, a modality, or both, then start planning.",
          "Measurement files remain untouched."
        ),
        active = active,
        active_label = "Preparing structural planning",
        active_detail = paste(
          "Starting the background worker. The discovery request identifies",
          "candidate file groups; glcdp validates their complete file schemas."
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
      has_plan <- !is.null(plan_value()) &&
        nrow(plan_value()$compatibility_sets) > 0L
      request_ready <-
        !is.null(inspection_value()) &&
        (!is.null(selected_term_value()) ||
          !is.null(selected_modality_value()))
      busy <-
        !is.null(pending_planning_attempt()) ||
        !is.null(active_planning_attempt())
      ready <- has_plan || (request_ready && !busy)
      button <- actionButton(
        session$ns("continue_structure"),
        if (busy && !has_plan) {
          "Collecting compatible groups..."
        } else {
          "Review compatible structures"
        },
        icon = icon("arrow-right"),
        class = "btn-primary btn-lg",
        title = if (has_plan) {
          "Review the structures supplied by glcdp"
        } else if (busy) {
          "Compatibility planning is in progress"
        } else if (request_ready) {
          paste(
            "Collect compatible groups for this request, then review",
            "the structures supplied by glcdp"
          )
        } else {
          "Choose a modality, canonical term, or both first"
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

    output$continue_variables_button <- renderUI({
      ready <- !is.null(selected_compatibility_id()) &&
        length(filtered_group_ids()) > 0L
      button <- actionButton(
        session$ns("continue_variables"),
        "Confirm and continue to variable selection",
        icon = icon("arrow-right"),
        class = "btn-primary btn-lg",
        title = if (ready) {
          paste(
            "Confirm the current metadata filters with glcdp and continue",
            "to variable selection"
          )
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
      characteristic_name <- current_characteristic_name()
      selected <- isolate(input[[glc_characteristic_value_input_id(
        characteristic_name,
        revision
      )]])
      glc_characteristic_value_input(
        session$ns,
        catalogue,
        characteristic_name,
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
            "Adjust the filters before continuing."
          } else {
            paste(
              "Continue to let glcdp recompute the final unit membership",
              "before variable selection."
            )
          }
        ),
        heading = "Filter result",
        compact = TRUE
      )
    })

    output$refinement_summary <- renderUI({
      error <- refinement_error()
      if (!is.null(error)) {
        return(llw_status_callout(
          "error",
          error,
          heading = "Refinement failed: retry is available",
          compact = TRUE
        ))
      }
      refinement <- refinement_value()
      if (is.null(refinement)) return(NULL)
      glc_refinement_summary_ui(refinement, plan_value())
    })

    output$scope_mode_selector <- renderUI({
      parent_request <- submitted_planning_request()
      refinement <- refinement_value()
      if (is.null(parent_request) || is.null(refinement)) {
        return(llw_status_callout(
          "idle",
          paste(
            "Confirm the metadata-filtered file groups before reviewing the",
            "variable contract."
          ),
          heading = "Variable scope is not available yet",
          compact = TRUE
        ))
      }
      choices <- glc_scope_mode_choices(parent_request)
      default <- if (!is.null(parent_request$term)) "discovery" else "all"
      revision <- scope_input_revision()
      input_id <- glc_scope_input_id("scope_mode", revision)
      current <- isolate(input[[input_id]] %||% default)
      if (!current %in% choices$values) current <- default
      tags$div(
        class = "llw-glc-scope-mode-selector",
        radioButtons(
          session$ns(input_id),
          "Variables to retain",
          choiceNames = choices$names,
          choiceValues = choices$values,
          selected = current,
          width = "100%"
        )
      )
    })

    output$custom_variable_selector <- renderUI({
      if (!identical(selected_scope_mode(), "custom")) return(NULL)
      catalogue <- scope_variable_catalogue()
      if (nrow(catalogue) == 0L) {
        return(llw_status_callout(
          "warning",
          "The refined file groups provide no selectable source variables.",
          heading = "No custom variables available",
          compact = TRUE
        ))
      }
      labels <- stats::setNames(catalogue$name, catalogue$display)
      input_id <- glc_scope_input_id(
        "scope_custom_variables",
        scope_input_revision()
      )
      selectizeInput(
        session$ns(input_id),
        "Exact source variables",
        choices = labels,
        selected = selected_custom_variables(),
        multiple = TRUE,
        width = "100%",
        options = list(
          placeholder = "Choose one or more exact source variables",
          closeAfterSelect = TRUE,
          maxOptions = 500L
        )
      )
    })

    output$scope_feedback <- renderUI({
      parent_request <- submitted_planning_request()
      refinement <- refinement_value()
      if (is.null(parent_request) || is.null(refinement)) return(NULL)
      mode <- selected_scope_mode()
      group_count <- length(refined_file_group_ids())
      selected <- scope_projection_variables()
      if (identical(mode, "custom") && length(selected) == 0L) {
        return(tags$div(
          class = "llw-glc-scope-feedback-result",
          llw_status_callout(
            "warning",
            "Choose at least one exact source variable for the output.",
            heading = "Custom variable selection is empty",
            compact = TRUE
          )
        ))
      }
      tags$div(
        class = "llw-glc-scope-feedback-result",
        llw_status_callout(
          "idle",
          paste(
            glc_pluralized_count(length(selected), "output variable"),
            "will be projected from",
            glc_pluralized_count(group_count, "fixed file group"),
            paste(
              "in the selected collectable unit. Identity, time, and key",
              "columns required by glcdp are shown separately in the review."
            )
          ),
          heading = "Compatibility stays fixed",
          compact = TRUE
        )
      )
    })

    output$scope_apply_button <- renderUI({
      enabled <- length(scope_projection_variables()) > 0L
      button <- actionButton(
        session$ns("scope_apply"),
        "Review output variables",
        icon = icon("table-columns"),
        class = "btn-outline-secondary btn-lg"
      )
      if (enabled) return(button)
      tagAppendAttributes(
        button,
        disabled = "disabled",
        `aria-disabled` = "true",
        title = "Choose at least one exact source variable first."
      )
    })

    output$scope_review <- renderUI({
      error <- scope_review_error()
      if (!is.null(error)) {
        return(glc_scope_review_result_ui(
          llw_status_callout(
            "error",
            error,
            heading = "Output-variable review could not be prepared",
            compact = TRUE
          )
        ))
      }
      review <- scope_review_value()
      if (is.null(review)) return(NULL)
      glc_scope_review_result_ui(glc_scope_review_ui(review))
    })

    output$continue_preview_button <- renderUI({
      ready <- length(scope_projection_variables()) > 0L
      button <- actionButton(
        session$ns("continue_preview"),
        "Continue to Import",
        icon = icon("arrow-right"),
        class = "btn-primary btn-lg",
        title = if (ready) {
          paste(
            "Validate the current output projection and continue to Import.",
            "Nothing transfers until Preview or Apply is chosen."
          )
        } else {
          "Choose at least one exact source variable first"
        }
      )
      if (!ready) {
        button <- tagAppendAttributes(
          button,
          disabled = "disabled",
          `aria-disabled` = "true"
        )
      }
      button
    })

    output$import_review <- renderUI({
      review <- transfer_review_value()
      if (is.null(review)) {
        return(llw_status_callout(
          "idle",
          paste(
            "Choose a valid output projection in Step 6 before importing",
            "the selected data."
          ),
          heading = "Import selection is not available yet",
          compact = TRUE
        ))
      }
      glc_import_review_ui(review)
    })

    output$transfer_review <- renderUI({
      error <- transfer_review_error()
      if (!is.null(error)) {
        review <- transfer_review_value()
        return(tags$div(
          class = "llw-glc-preview-review",
          llw_status_callout(
            "error",
            llw_public_message(error),
            heading = "Correct the preview-row budget",
            compact = TRUE
          ),
          if (!is.null(review)) {
            tagList(
              llw_status_callout(
                "idle",
                paste(
                  "The last valid preview allocation remains visible for",
                  "context, but Preview stays disabled until the budget is",
                  "valid."
                ),
                heading = "Last valid preview retained",
                compact = TRUE
              ),
              glc_preview_review_ui(review)
            )
          }
        ))
      }
      review <- transfer_review_value()
      if (is.null(review)) {
        return(llw_status_callout(
          "idle",
          paste(
            "Review the output variables in Step 6 before preparing the",
            "optional bounded preview."
          ),
          heading = "Preview is not available yet",
          compact = TRUE
        ))
      }
      glc_preview_review_ui(review)
    })

    output$transfer_preview_button <- renderUI({
      review <- transfer_review_value()
      review_error <- transfer_review_error()
      busy <- !is.null(pending_transfer_attempt()) ||
        !is.null(active_transfer_attempt()) ||
        !is.null(pending_apply_attempt()) ||
        !is.null(active_apply_attempt())
      enabled <- !is.null(review) &&
        is.null(review_error) &&
        !isTRUE(review$resource$blocked) &&
        !busy
      button <- input_task_button(
        session$ns("transfer_preview"),
        span(strong("Transfer bounded preview")),
        icon = icon("table"),
        class = "btn-primary btn-lg"
      )
      if (enabled) return(button)
      tagAppendAttributes(
        button,
        disabled = "disabled",
        `aria-disabled` = "true",
        title = if (busy) {
          "A measurement-access task is already active."
        } else if (!is.null(review_error)) {
          llw_public_message(review_error)
        } else if (!is.null(review) && isTRUE(review$resource$blocked)) {
          "The known preview size exceeds this session's resource limit."
        } else {
          "Complete the transfer review first."
        }
      )
    })

    output$cancel_transfer_button <- renderUI({
      glc_cancel_task_button(
        session$ns,
        "cancel_transfer",
        !is.null(pending_transfer_attempt()) ||
          !is.null(active_transfer_attempt()),
        "Cancel preview"
      )
    })

    output$transfer_cancellation_drain <- renderUI({
      cancellation_drain_for_display(transfer_display_status())
    })

    output$transfer_progress_card <- renderUI({
      status <- transfer_display_status()
      state <- status$state
      pending <- !is.null(pending_transfer_attempt())
      active <- pending || !is.null(active_transfer_attempt())
      if (active) invalidateLater(250, session)
      if (
        !transfer_attempted() &&
          !active &&
          identical(state, "idle")
      ) {
        return(NULL)
      }
      if (
        !active &&
          state %in% c("complete", "warning") &&
          !is.null(transfer_preview_value())
      ) {
        return(NULL)
      }
      card(
        class = "llw-glc-transfer-progress mt-3",
        card_header("Bounded preview progress", container = h3),
        glc_task_progress_ui(
          read_glc_task_progress(transfer_progress_path()),
          total_phases = 5L,
          heading = "Bounded typed preview",
          idle_message = paste(
            "Use Transfer bounded preview after reviewing the exact contract.",
            "No measurement file opens automatically."
          ),
          active = active,
          active_label = "Preparing the bounded preview",
          active_detail = paste(
            "Starting a session-local worker for the accepted exact revision",
            "and canonical file allocation."
          ),
          terminal_state = if (pending) "queued" else state,
          terminal_message = if (pending) {
            "Waiting for the previous preview worker to stop cleanly."
          } else {
            status$message %||% NULL
          }
        ),
        if (
          transfer_attempted() &&
            state %in% c("error", "stale")
        ) {
          tags$div(
            class = "mt-3",
            glc_task_status_tag(
              status,
              idle_heading = "Preview has not started",
              idle_message = "Use Transfer bounded preview to start.",
              task_label = "Bounded preview"
            )
          )
        }
      )
    })

    output$transfer_preview_result <- renderUI({
      result <- transfer_preview_value()
      if (is.null(result)) return(NULL)
      glc_transfer_preview_result_ui(result)
    })

    output$apply_full_import_button <- renderUI({
      review <- transfer_review_value()
      completed <- !is.null(full_import_value())
      busy <- !is.null(pending_apply_attempt()) ||
        !is.null(active_apply_attempt()) ||
        !is.null(pending_transfer_attempt()) ||
        !is.null(active_transfer_attempt())
      enabled <- !is.null(review) &&
        !isTRUE(review$resource$full_import_blocked) &&
        !completed &&
        !busy
      button <- input_task_button(
        session$ns("apply_full_import"),
        span(strong("Apply full import")),
        icon = icon("cloud-arrow-down"),
        class = "btn-primary btn-lg"
      )
      if (enabled) return(button)
      tagAppendAttributes(
        button,
        disabled = "disabled",
        `aria-disabled` = "true",
        title = if (busy) {
          "A measurement-access task is already active."
        } else if (completed) {
          paste(
            "This exact selection is already imported in this session.",
            "Change the selection to run a new import."
          )
        } else if (
          !is.null(review) &&
            isTRUE(review$resource$full_import_blocked)
        ) {
          "Known selected bytes exceed this session's full-import resource limit."
        } else {
          "Choose a valid output projection before importing."
        }
      )
    })

    output$cancel_apply_button <- renderUI({
      glc_cancel_task_button(
        session$ns,
        "cancel_apply",
        !is.null(pending_apply_attempt()) ||
          !is.null(active_apply_attempt()),
        "Cancel full import"
      )
    })

    output$apply_cancellation_drain <- renderUI({
      cancellation_drain_for_display(apply_display_status())
    })

    output$apply_progress_card <- renderUI({
      status <- apply_display_status()
      state <- status$state
      pending <- !is.null(pending_apply_attempt())
      active <- pending || !is.null(active_apply_attempt())
      progress_path <- apply_progress_path()
      progress <- glc_full_import_progress_for_display(
        read_glc_task_progress(progress_path),
        transfer_review_value(),
        progress_path
      )
      elapsed_label <- attr(progress, "llw_elapsed_label", exact = TRUE)
      if (active) invalidateLater(250, session)
      if (
        !apply_attempted() &&
          !active &&
          identical(state, "idle")
      ) {
        return(NULL)
      }
      if (
        !active &&
          state %in% c("complete", "warning") &&
          !is.null(full_import_value())
      ) {
        return(NULL)
      }
      card(
        class = "llw-glc-transfer-progress mt-3",
        card_header("Full import progress", container = h3),
        glc_full_import_elapsed_ui(elapsed_label),
        glc_task_progress_ui(
          progress,
          total_phases = 5L,
          heading = "Fresh complete import",
          idle_message = paste(
            "Use Apply full import after reviewing the exact contract.",
            "No full import starts automatically."
          ),
          active = active,
          active_label = "Preparing the fresh full import",
          active_detail = paste(
            "Starting a session-local worker that will reopen the exact",
            "revision and read every selected file without using preview rows."
          ),
          terminal_state = if (pending) "queued" else state,
          terminal_message = if (pending) {
            "Waiting for the previous full-import worker to stop cleanly."
          } else {
            status$message %||% NULL
          }
        ),
        if (
          apply_attempted() &&
            state %in% c("error", "stale")
        ) {
          tags$div(
            class = "mt-3",
            glc_task_status_tag(
              status,
              idle_heading = "Full import has not started",
              idle_message = "Use Apply full import to start.",
              task_label = "Full import"
            )
          )
        }
      )
    })

    output$full_import_result <- renderUI({
      result <- full_import_value()
      if (is.null(result)) return(NULL)
      status <- apply_display_status()
      active <-
        !is.null(pending_apply_attempt()) ||
        !is.null(active_apply_attempt())
      retained <- active ||
        status$state %in%
          c(
            "cancelled",
            "error",
            "stale",
            "queued",
            "running",
            "finalizing"
          )
      glc_full_import_result_ui(
        result,
        retained = retained,
        progress = read_glc_task_progress(apply_progress_path()),
        library_action = if (enable_library_integration) {
          uiOutput(session$ns("library_add_action"))
        }
      )
    })

    output$library_add_action <- renderUI({
      if (!enable_library_integration) return(NULL)
      result <- full_import_value()
      if (is.null(result)) return(NULL)
      glc_library_add_action_ui(
        session$ns,
        result,
        added = library_added_value(),
        error = library_add_error(),
        status = library_add_status(),
        input_id = "add_to_library",
        show_error = TRUE
      )
    })

    output$library_add_footer_action <- renderUI({
      if (!enable_library_integration) return(NULL)
      result <- full_import_value()
      if (is.null(result)) return(NULL)
      tags$div(
        class = "llw-glc-library-footer-action",
        glc_library_add_action_ui(
          session$ns,
          result,
          added = library_added_value(),
          error = library_add_error(),
          status = library_add_status(),
          input_id = "add_to_library_footer",
          show_error = FALSE
        )
      )
    })

    queue_library_add <- function() {
      if (!enable_library_integration) return(invisible(NULL))
      current_status <- isolate(library_add_status())
      if (
        is.list(current_status) &&
          current_status$state %in% c("preparing", "adding", "opening")
      ) {
        return(invisible(NULL))
      }
      result <- isolate(full_import_value())
      req(result)
      library_add_status(list(
        state = "preparing",
        message = "Preparing dataset...",
        detail = paste(
          "Validating the complete handoff and its provenance before it is",
          "added to the session library."
        )
      ))
      request <- tryCatch(
        new_glc_library_add_request(result),
        error = identity
      )
      if (inherits(request, "error")) {
        library_add_status(NULL)
        library_add_error(llw_public_message(normalize_task_error(
          request,
          "preparation"
        )))
        return(invisible(NULL))
      }
      library_add_error(NULL)
      library_add_request_value(request)
      invisible(NULL)
    }

    observeEvent(input$add_to_library, queue_library_add(), ignoreInit = TRUE)
    observeEvent(
      input$add_to_library_footer,
      queue_library_add(),
      ignoreInit = TRUE
    )

    mark_library_add_progress <- function(request_id, state, message, detail) {
      assert_scalar_string(request_id, "request_id")
      assert_scalar_string(state, "state")
      assert_scalar_string(message, "message")
      assert_scalar_string(detail, "detail")
      current <- isolate(library_add_request_value())
      if (is.null(current) || !identical(current$request_id, request_id)) {
        return(invisible(FALSE))
      }
      library_add_status(list(
        state = state,
        message = message,
        detail = detail
      ))
      invisible(TRUE)
    }

    mark_library_added <- function(request_id, dataset_id) {
      assert_scalar_string(request_id, "request_id")
      assert_scalar_string(dataset_id, "dataset_id")
      current <- isolate(library_add_request_value())
      if (is.null(current) || !identical(current$request_id, request_id)) {
        return(invisible(FALSE))
      }
      current_result <- isolate(full_import_value())
      if (
        is.null(current_result) ||
          !identical(
            current$full_import_fingerprint,
            current_result$fingerprint
          )
      ) {
        return(invisible(FALSE))
      }
      library_added_value(list(
        request_id = request_id,
        dataset_id = dataset_id,
        full_import_fingerprint = current$full_import_fingerprint
      ))
      library_add_error(NULL)
      library_add_status(list(
        state = "added",
        message = "Added to dataset library",
        detail = "The dashboard is opening with the newly added dataset."
      ))
      invisible(TRUE)
    }

    mark_library_add_failed <- function(request_id, message) {
      assert_scalar_string(request_id, "request_id")
      assert_scalar_string(message, "message")
      current <- isolate(library_add_request_value())
      if (is.null(current) || !identical(current$request_id, request_id)) {
        return(invisible(FALSE))
      }
      library_add_error(message)
      library_add_status(NULL)
      invisible(TRUE)
    }

    # These uiOutput containers are intentionally empty until their review,
    # task, or result is available. The app stylesheet hides empty outputs, so
    # Shiny would otherwise suspend their first render and leave them empty
    # forever.
    for (output_id in c(
      "import_review",
      "transfer_review",
      "transfer_cancellation_drain",
      "transfer_progress_card",
      "transfer_preview_result",
      "apply_cancellation_drain",
      "apply_progress_card",
      "full_import_result",
      "library_add_action",
      "library_add_footer_action"
    )) {
      outputOptions(output, output_id, suspendWhenHidden = FALSE)
    }

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

    observe({
      status <- transfer_display_status()
      req(identical(status$state, "error"))
      showNotification(
        status$message,
        type = "error",
        duration = 8
      )
    }) |>
      bindEvent(transfer_display_status()$state, ignoreInit = TRUE)

    observe({
      status <- apply_display_status()
      req(identical(status$state, "error"))
      showNotification(
        status$message,
        type = "error",
        duration = 8
      )
    }) |>
      bindEvent(apply_display_status()$state, ignoreInit = TRUE)

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
      inspection_active = reactive(!is.null(active_inspection_attempt())),
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
      planning_active = reactive(!is.null(active_planning_attempt())),
      wizard_step = reactive(wizard_step()),
      continue_after_planning = reactive(continue_after_planning()),
      selected_compatibility_id = selected_compatibility_id,
      metadata_filter_revision = reactive(metadata_filter_revision()),
      scope_input_revision = reactive(scope_input_revision()),
      metadata_filters = current_metadata_filters,
      active_metadata_filter_count = reactive(
        glc_active_metadata_filter_count(current_metadata_filters())
      ),
      filtered_file_group_ids = filtered_group_ids,
      refinement = reactive(refinement_value()),
      scope_variable_catalogue = scope_variable_catalogue,
      scope_mode = selected_scope_mode,
      scope_projection_variables = scope_projection_variables,
      scope_review_error = reactive(scope_review_error()),
      scope_review = reactive(scope_review_value()),
      transfer_review_error = reactive(transfer_review_error()),
      transfer_review = reactive(transfer_review_value()),
      transfer_preview = reactive(transfer_preview_value()),
      transfer_status = reactive({
        if (transfer_attempted()) {
          transfer_display_status()
        } else {
          new_task_status("glc_read")
        }
      }),
      transfer_controller_revision = reactive(
        transfer_controller_revision()
      ),
      transfer_pending = reactive(!is.null(pending_transfer_attempt())),
      transfer_active = reactive(!is.null(active_transfer_attempt())),
      full_import = reactive(full_import_value()),
      library_add_request = reactive(library_add_request_value()),
      library_added = reactive(library_added_value()),
      library_add_status = reactive(library_add_status()),
      mark_library_add_progress = mark_library_add_progress,
      mark_library_added = mark_library_added,
      mark_library_add_failed = mark_library_add_failed,
      apply_status = reactive({
        if (apply_attempted()) {
          apply_display_status()
        } else {
          new_task_status("glc_read")
        }
      }),
      apply_controller_revision = reactive(apply_controller_revision()),
      apply_pending = reactive(!is.null(pending_apply_attempt())),
      apply_active = reactive(!is.null(active_apply_attempt()))
    )
  })
}
