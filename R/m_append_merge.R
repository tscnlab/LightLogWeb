append_input_token <- function(dataset_id) {
  assert_scalar_string(dataset_id, "dataset_id")
  substr(
    digest::digest(dataset_id, algo = "sha256", serialize = FALSE),
    1L,
    10L
  )
}

append_mapping_input_id <- function(dataset_id, field, column = NULL) {
  assert_scalar_string(field, "field")
  token <- append_input_token(dataset_id)
  if (is.null(column)) {
    paste("source", token, field, sep = "_")
  } else {
    assert_scalar_string(column, "column")
    column_token <- substr(
      digest::digest(column, algo = "sha256", serialize = FALSE),
      1L,
      10L
    )
    paste("source", token, field, column_token, sep = "_")
  }
}

append_record_column_choices <- function(record) {
  record <- validate_dataset_record(record)
  data <- dataset_raw_data(record)
  data <- data[append_user_column_names(data)]
  atomic <- names(data)[vapply(
    data,
    function(column) {
      is.atomic(column) && is.null(dim(column))
    },
    logical(1)
  )]
  datetime <- names(data)[vapply(data, inherits, logical(1), "POSIXct")]
  numeric <- names(data)[vapply(
    data,
    function(column) {
      is.numeric(column) && is.null(dim(column)) && !inherits(column, "POSIXct")
    },
    logical(1)
  )]
  id_columns <- setdiff(atomic, datetime)
  participant <- if ("Id" %in% id_columns) {
    c("Id", setdiff(id_columns, "Id"))
  } else {
    id_columns
  }
  preferred_datetime <- if ("Datetime" %in% datetime) {
    c("Datetime", setdiff(datetime, "Datetime"))
  } else {
    datetime
  }
  primary <- record$analysis_settings$primary_variable %||% NA_character_
  measurement <- if (
    is.character(primary) && length(primary) == 1L && primary %in% numeric
  ) {
    c(primary, setdiff(numeric, primary))
  } else {
    numeric
  }
  list(
    participant = participant,
    datetime = preferred_datetime,
    measurement = measurement,
    optional = atomic
  )
}

append_common_measurement_candidates <- function(records, source_ids) {
  source_ids <- intersect(source_ids, names(records))
  if (length(source_ids) < 2L) {
    return(data.frame(column = character(), unit = character()))
  }
  common <- Reduce(
    intersect,
    lapply(source_ids, function(dataset_id) {
      append_record_column_choices(records[[dataset_id]])$measurement
    })
  )
  rows <- lapply(common, function(column) {
    units <- vapply(
      source_ids,
      function(dataset_id) {
        dataset_record_variable_metadata(records[[dataset_id]], column)$unit
      },
      character(1)
    )
    literal_units <- vapply(units, append_literal_unit, character(1))
    if (anyNA(literal_units) || length(unique(literal_units)) != 1L) {
      return(NULL)
    }
    data.frame(
      column = column,
      unit = append_unit_label(units[[1L]]),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame(column = character(), unit = character()))
  }
  do.call(rbind, rows)
}

append_selected_measurement_details <- function(records, source_ids, input) {
  rows <- lapply(source_ids, function(dataset_id) {
    record <- records[[dataset_id]]
    choices <- append_record_column_choices(record)
    if (length(choices$measurement) == 0L) return(NULL)
    column <- input[[append_mapping_input_id(
      dataset_id,
      "measurement"
    )]] %||%
      choices$measurement[[1L]]
    data.frame(
      dataset_id = dataset_id,
      column = column,
      unit = dataset_record_variable_metadata(record, column)$unit,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame(
      dataset_id = character(),
      column = character(),
      unit = character()
    ))
  }
  do.call(rbind, rows)
}

append_combined_measurement_defaults <- function(details) {
  same_name <- nrow(details) > 0L && length(unique(details$column)) == 1L
  literal_units <- vapply(details$unit, append_literal_unit, character(1))
  same_unit <- nrow(details) > 0L &&
    !anyNA(literal_units) &&
    length(unique(literal_units)) == 1L
  list(
    column = if (same_name) details$column[[1L]] else "Measurement",
    unit = if (same_unit) append_unit_label(details$unit[[1L]]) else ""
  )
}

append_radio_group_ui <- function(
  input_id,
  label,
  choice_names,
  choice_values,
  selected,
  disabled_values = character()
) {
  if (length(choice_names) != length(choice_values)) {
    abort_llw("Radio choice names and values must have equal lengths.")
  }
  tags$div(
    id = input_id,
    class = paste(
      "form-group shiny-input-radiogroup shiny-input-container",
      "llw-radio-group"
    ),
    role = "radiogroup",
    `aria-labelledby` = paste0(input_id, "-label"),
    tags$label(
      class = "control-label",
      id = paste0(input_id, "-label"),
      `for` = input_id,
      label
    ),
    tags$div(
      class = "shiny-options-group",
      lapply(seq_along(choice_values), function(index) {
        value <- choice_values[[index]]
        disabled <- value %in% disabled_values
        tags$div(
          class = paste(
            "radio llw-radio-choice",
            if (disabled) "llw-radio--disabled"
          ),
          tags$label(
            tags$input(
              type = "radio",
              name = input_id,
              value = value,
              checked = if (identical(value, selected)) "checked" else NULL,
              disabled = if (disabled) "disabled" else NULL,
              `aria-disabled` = if (disabled) "true" else NULL
            ),
            tags$span(
              class = "llw-radio-choice__label",
              choice_names[[index]]
            )
          )
        )
      })
    )
  )
}

append_measurement_strategy_ui <- function(ns, selected, candidates) {
  identical_available <- nrow(candidates) > 0L
  if (!identical_available && identical(selected, "identical")) {
    selected <- "separate"
  }
  tagList(
    append_radio_group_ui(
      ns("measurement_strategy"),
      "Primary measurement mapping",
      choice_names = list(
        "Keep source measurements separate",
        "Combine into one explicitly confirmed column",
        "Combine one column with an identical name and unit"
      ),
      choice_values = c("separate", "combine", "identical"),
      selected = selected,
      disabled_values = if (identical_available) character() else "identical"
    ),
    if (!identical_available) {
      tags$p(
        class = "llw-secondary small mb-0",
        paste(
          "The identical-column option is unavailable because no numeric",
          "column has the same name and known unit in every selected source."
        )
      )
    }
  )
}

append_time_selection <- function(input, records, source_ids) {
  zones <- vapply(
    source_ids,
    function(dataset_id) {
      record <- records[[dataset_id]]
      choices <- append_record_column_choices(record)
      datetime <- input[[append_mapping_input_id(dataset_id, "datetime")]] %||%
        choices$datetime[[1L]]
      lubridate::tz(dataset_raw_data(record)[[datetime]])
    },
    character(1)
  )
  shared <- unique(zones)
  shared_available <- length(shared) == 1L &&
    !is.na(shared) &&
    nzchar(shared) &&
    shared %in% OlsonNames()
  alignment <- input$time_alignment %||% "preserve_clock"
  if (identical(alignment, "keep_source") && !shared_available) {
    abort_llw(
      paste(
        "Keeping source times unchanged requires every mapped datetime to use",
        "one shared time zone."
      ),
      type = "validation"
    )
  }
  output_timezone <- if (identical(alignment, "keep_source")) {
    shared[[1L]]
  } else {
    trimws(input$output_timezone %||% "UTC")
  }
  if (!output_timezone %in% OlsonNames()) {
    abort_llw("Choose a valid output time zone.", type = "validation")
  }
  list(
    alignment = alignment,
    output_timezone = output_timezone,
    zones = zones,
    shared_available = shared_available,
    shared_timezone = if (shared_available) shared[[1L]] else NA_character_
  )
}

append_time_plan_from_inputs <- function(input, records, source_ids) {
  selection <- append_time_selection(input, records, source_ids)
  rows <- lapply(seq_along(source_ids), function(index) {
    dataset_id <- source_ids[[index]]
    record <- records[[dataset_id]]
    choices <- append_record_column_choices(record)
    datetime <- input[[append_mapping_input_id(dataset_id, "datetime")]] %||%
      choices$datetime[[1L]]
    data.frame(
      Dataset = record$display_name,
      `Main datetime` = datetime,
      `Current time zone` = selection$zones[[index]],
      `After merging` = selection$output_timezone,
      Preserved = append_time_preservation_label(
        selection$alignment,
        selection$zones[[index]],
        selection$output_timezone
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

append_time_controls_ui <- function(ns, input, records, source_ids) {
  selection <- tryCatch(
    append_time_selection(input, records, source_ids),
    error = identity
  )
  if (inherits(selection, "error")) {
    zones <- vapply(
      source_ids,
      function(dataset_id) {
        record <- records[[dataset_id]]
        choices <- append_record_column_choices(record)
        datetime <- input[[append_mapping_input_id(
          dataset_id,
          "datetime"
        )]] %||%
          choices$datetime[[1L]]
        lubridate::tz(dataset_raw_data(record)[[datetime]])
      },
      character(1)
    )
    shared <- unique(zones)
    shared_available <- length(shared) == 1L && shared %in% OlsonNames()
    alignment <- "preserve_clock"
    output_timezone <- "UTC"
  } else {
    shared_available <- selection$shared_available
    alignment <- selection$alignment
    output_timezone <- selection$output_timezone
  }
  if (!shared_available && identical(alignment, "keep_source")) {
    alignment <- "preserve_clock"
    output_timezone <- "UTC"
  }
  tagList(
    append_radio_group_ui(
      ns("time_alignment"),
      "What should remain unchanged?",
      choice_names = list(
        "Preserve clock time (force_tz)" |>
          tooltip2(paste(
            "The written local clock stays the same while the represented",
            "instant is reinterpreted in the output zone. For example,",
            "Europe/Berlin 18:00 becomes UTC 18:00."
          )),
        "Preserve the absolute instant (with_tz)" |>
          tooltip2(paste(
            "The same moment is retained while its clock representation",
            "changes in the output zone. For example, Europe/Berlin 18:00",
            "becomes UTC 16:00 during daylight-saving time."
          )),
        "Keep the shared source time zone unchanged" |>
          tooltip2(paste(
            "No time-zone adjustment is performed. This is available only",
            "when all mapped datetime columns already use one shared zone."
          ))
      ),
      choice_values = c(
        "preserve_clock",
        "preserve_instant",
        "keep_source"
      ),
      selected = alignment,
      disabled_values = if (shared_available) character() else "keep_source"
    ),
    if (identical(alignment, "keep_source")) {
      llw_status_callout(
        "complete",
        paste0(
          "All mapped main datetimes use `",
          output_timezone,
          "`; no time-zone adjustment will be made."
        ),
        heading = "Shared source time zone retained",
        compact = TRUE
      )
    } else {
      tagList(
        selectizeInput(
          ns("output_timezone"),
          "Output time zone",
          choices = stats::setNames(OlsonNames(), OlsonNames()),
          selected = output_timezone,
          width = "100%"
        ),
        llw_status_callout(
          if (identical(alignment, "preserve_clock")) "warning" else "complete",
          if (identical(alignment, "preserve_clock")) {
            paste(
              "Clock labels remain fixed; absolute instants may change. The",
              "preview records this decision and checks DST edge cases."
            )
          } else {
            paste(
              "Absolute instants remain fixed; clock labels are converted to",
              "the selected output time zone."
            )
          },
          heading = if (identical(alignment, "preserve_clock")) {
            "Clock time will be preserved"
          } else {
            "Absolute instants will be preserved"
          },
          compact = TRUE
        )
      )
    },
    if (!shared_available) {
      tags$p(
        class = "llw-secondary small mb-0",
        "Keeping source time unchanged is unavailable because the selected sources do not share one time zone."
      )
    }
  )
}

append_default_source_ids <- function(records, selected_dataset_id = NULL) {
  ids <- names(records)
  if (length(ids) < 2L) return(ids)
  if (!is.null(selected_dataset_id) && selected_dataset_id %in% ids) {
    c(selected_dataset_id, setdiff(ids, selected_dataset_id)[[1L]])
  } else {
    ids[seq_len(2L)]
  }
}

append_source_choice_labels <- function(records) {
  labels <- vapply(
    records,
    function(record) {
      inventory <- dataset_record_inventory(record)
      timezone <- if (is.na(inventory$source_timezone)) {
        "timezone unknown"
      } else {
        inventory$source_timezone
      }
      paste0(
        inventory$display_name,
        " - ",
        format(inventory$rows, big.mark = ","),
        " rows, ",
        timezone
      )
    },
    character(1)
  )
  stats::setNames(names(records), labels)
}

append_wizard_steps <- function() {
  data.frame(
    value = c("sources", "mapping", "time", "review"),
    label = c(
      "Sources & IDs",
      "Time & measurements",
      "Output time",
      "Review & create"
    ),
    icon = c("layers", "sliders", "clock-history", "clipboard-check"),
    stringsAsFactors = FALSE
  )
}

append_wizard_stepper <- function(ns, active, completed = character()) {
  steps <- append_wizard_steps()
  active_index <- match(active, steps$value)
  if (is.na(active_index)) {
    abort_llw("Unknown append-wizard step.", type = "validation")
  }
  if (length(setdiff(completed, steps$value)) > 0L) {
    abort_llw("Unknown completed append-wizard step.", type = "validation")
  }
  tags$ol(
    class = "llw-import-stepper",
    `aria-label` = "Append steps",
    lapply(seq_len(nrow(steps)), function(index) {
      value <- steps$value[[index]]
      state <- if (index == active_index) {
        "active"
      } else if (value %in% completed) {
        "complete"
      } else {
        "upcoming"
      }
      preceding <- if (index == 1L) character() else
        steps$value[seq_len(index - 1L)]
      available <- index == 1L ||
        all(preceding %in% completed) ||
        identical(value, active)
      button <- actionButton(
        ns(paste0("wizard_step_", value)),
        tagList(
          tags$span(
            class = "llw-import-stepper__icon",
            `aria-hidden` = "true",
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
        `aria-current` = if (identical(state, "active")) "step" else NULL,
        `aria-disabled` = if (!available) "true" else NULL,
        title = paste("Open step", index, steps$label[[index]])
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

append_error_step_action_ui <- function(ns, step) {
  steps <- append_wizard_steps()
  index <- match(step, steps$value)
  if (is.na(index)) {
    abort_llw("Unknown append error step.", type = "validation")
  }
  if (identical(step, "review")) {
    return(tags$span(
      class = "llw-status__local-guidance",
      icon("arrow-up", `aria-hidden` = "true"),
      "Change the new dataset name above, then update the preview."
    ))
  }
  actionLink(
    ns("resolve_preview_error"),
    paste0("Go to Step ", index, ": ", steps$label[[index]]),
    icon = icon("arrow-right"),
    class = "llw-status__step-link",
    title = paste("Open", steps$label[[index]], "to resolve this error")
  )
}

append_optional_all_value <- function() "__all_optional_columns__"

append_selected_optional_columns <- function(selected, eligible) {
  selected <- selected %||% character()
  if (append_optional_all_value() %in% selected) return(eligible)
  intersect(selected, eligible)
}

append_identity_card_ui <- function(
  record,
  index,
  input,
  session,
  participant_policy
) {
  choices <- append_record_column_choices(record)
  if (length(choices$participant) == 0L) {
    return(card(
      card_header(record$display_name),
      llw_status_callout(
        "error",
        "This dataset does not expose a usable ID column.",
        heading = "ID mapping unavailable",
        compact = TRUE
      )
    ))
  }
  participant_id <- append_mapping_input_id(record$id, "participant")
  prefix_id <- append_mapping_input_id(record$id, "prefix")
  selected_participant <- isolate(input[[participant_id]]) %||%
    choices$participant[[1L]]
  card(
    class = "llw-append-source-card",
    card_header(paste0("Source ", index, ": ", record$display_name)),
    tags$p(
      class = "llw-secondary",
      "Choose the column that identifies a meaningful person, group, or recording unit."
    ),
    layout_columns(
      col_widths = if (identical(participant_policy, "prefix_source")) {
        c(7, 5)
      } else {
        12
      },
      selectizeInput(
        session$ns(participant_id),
        "ID column",
        choices = choices$participant,
        selected = selected_participant,
        width = "100%"
      ),
      if (identical(participant_policy, "prefix_source")) {
        tagList(
          textInput(
            session$ns(prefix_id),
            "Source prefix",
            value = isolate(input[[prefix_id]]) %||% paste0("S", index),
            width = "100%",
            updateOn = "blur"
          ),
          tags$p(
            class = "llw-secondary small mb-0",
            "The underscore separator is added automatically."
          )
        )
      }
    )
  )
}

append_mapping_card_ui <- function(
  record,
  index,
  input,
  session,
  strategy,
  identical_measurement = NULL
) {
  choices <- append_record_column_choices(record)
  if (
    length(choices$datetime) == 0L ||
      length(choices$measurement) == 0L
  ) {
    return(card(
      card_header(record$display_name),
      llw_status_callout(
        "error",
        paste(
          "This dataset does not expose selectable POSIXct datetime and",
          "numeric measurement columns."
        ),
        heading = "Required mapping unavailable",
        compact = TRUE
      )
    ))
  }
  participant_id <- append_mapping_input_id(record$id, "participant")
  datetime_id <- append_mapping_input_id(record$id, "datetime")
  measurement_id <- append_mapping_input_id(record$id, "measurement")
  measurement_target_id <- append_mapping_input_id(
    record$id,
    "measurement_target"
  )
  optional_id <- append_mapping_input_id(record$id, "optional")
  selected_participant <- isolate(input[[participant_id]]) %||%
    choices$participant[[1L]]
  selected_datetime <- isolate(input[[datetime_id]]) %||%
    choices$datetime[[1L]]
  selected_measurement <- if (identical(strategy, "identical")) {
    identical_measurement
  } else {
    isolate(input[[measurement_id]]) %||% choices$measurement[[1L]]
  }
  if (
    !is.character(selected_measurement) ||
      length(selected_measurement) != 1L ||
      !selected_measurement %in% choices$measurement
  ) {
    selected_measurement <- choices$measurement[[1L]]
  }
  eligible_optional <- setdiff(
    choices$optional,
    c(selected_participant, selected_datetime, selected_measurement)
  )
  selected_optional_input <- isolate(input[[optional_id]]) %||% character()
  selected_optional <- append_selected_optional_columns(
    selected_optional_input,
    eligible_optional
  )
  optional_controls <- lapply(selected_optional, function(column) {
    target_id <- append_mapping_input_id(record$id, "optional_target", column)
    coercion_id <- append_mapping_input_id(
      record$id,
      "optional_coercion",
      column
    )
    layout_columns(
      col_widths = c(7, 5),
      textInput(
        session$ns(target_id),
        paste0("Output name for `", column, "`"),
        value = isolate(input[[target_id]]) %||% column,
        width = "100%",
        updateOn = "blur"
      ),
      selectInput(
        session$ns(coercion_id),
        paste0("Type handling for `", column, "`"),
        choices = c(
          "Keep source type" = "none",
          "Explicitly convert to text" = "character"
        ),
        selected = isolate(input[[coercion_id]]) %||% "none",
        width = "100%"
      )
    )
  })
  card(
    class = "llw-append-source-card",
    card_header(
      paste0("Source ", index, ": ", record$display_name)
    ),
    tags$p(
      class = "llw-secondary",
      paste(
        "Map time and measurement meaning explicitly. Matching names alone do",
        "not establish compatibility."
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      class = "llw-append-core-mapping",
      selectizeInput(
        session$ns(datetime_id),
        "Datetime column",
        choices = choices$datetime,
        selected = selected_datetime,
        width = "100%"
      ),
      if (identical(strategy, "identical")) {
        tags$div(
          class = "llw-append-fixed-mapping",
          tags$span("Primary measurement column"),
          tags$code(selected_measurement),
          tags$small("Shared name and unit retained")
        )
      } else {
        selectizeInput(
          session$ns(measurement_id),
          "Primary measurement column",
          choices = choices$measurement,
          selected = selected_measurement,
          width = "100%"
        )
      }
    ),
    if (identical(strategy, "separate")) {
      textInput(
        session$ns(measurement_target_id),
        "Separate measurement output name",
        value = isolate(input[[measurement_target_id]]) %||%
          paste0("source_", index, "_", selected_measurement),
        width = "100%",
        updateOn = "blur"
      )
    },
    if (length(eligible_optional) > 0L) {
      selectizeInput(
        session$ns(optional_id),
        "Optional columns to retain",
        choices = c(
          "Select all available columns" = append_optional_all_value(),
          stats::setNames(eligible_optional, eligible_optional)
        ),
        selected = intersect(
          selected_optional_input,
          c(append_optional_all_value(), eligible_optional)
        ),
        multiple = TRUE,
        options = list(
          placeholder = "Choose columns, or select all available columns"
        ),
        width = "100%"
      )
    } else {
      tags$p(
        class = "llw-secondary mb-0",
        "No additional columns are available after the required mappings."
      )
    },
    if (length(optional_controls) > 0L) {
      accordion(
        id = session$ns(append_mapping_input_id(
          record$id,
          "optional_advanced"
        )),
        open = FALSE,
        class = "llw-append-advanced-mapping",
        accordion_panel(
          paste0(
            "Advanced names and type handling (",
            length(optional_controls),
            ")"
          ),
          optional_controls
        )
      )
    }
  )
}

append_id_policy_detail_ui <- function(
  ns,
  participant_policy,
  overlap_policy = "error"
) {
  if (identical(participant_policy, "prefix_source")) {
    return(llw_status_callout(
      "complete",
      paste(
        "Every ID becomes prefix_ID. Unique source prefixes therefore prevent",
        "cross-source ID/time collisions."
      ),
      heading = "Cross-source collisions prevented",
      compact = TRUE
    ))
  }
  tagList(
    llw_status_callout(
      "warning",
      paste(
        "Equal ID labels will refer to the same group after append. Choose what",
        "to do if two sources also contain the same instant. A regular `Source`",
        "column will retain each dataset name for later grouping."
      ),
      heading = "Preserved IDs can overlap",
      compact = TRUE
    ),
    radioButtons(
      ns("overlap_policy"),
      "If preserved IDs and timestamps overlap across sources",
      choices = c(
        "Block creation until the ID mapping changes" = "error",
        "Retain every row and mark the overlap" = "keep_marked"
      ),
      selected = overlap_policy,
      width = "100%"
    )
  )
}

append_sources_step_ui <- function(ns) {
  nav_panel(
    "Sources and IDs",
    value = "sources",
    tags$div(
      class = "llw-wizard-step-content",
      import_wizard_step_heading(
        "layers",
        "Choose sources and identity",
        "Select datasets and define their IDs",
        paste(
          "Start with at least two session datasets. Decide whether equal IDs",
          "remain related or receive source-specific prefixes."
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        fillable = FALSE,
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(bsicons::bs_icon("database"), " Source datasets"),
          uiOutput(ns("source_selector")),
          uiOutput(ns("source_selection_status"))
        ),
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(bsicons::bs_icon("person-vcard"), " ID policy"),
          radioButtons(
            ns("participant_policy"),
            "How should source IDs relate?",
            choices = c(
              "Add a unique prefix to each source (safe default)" = "prefix_source",
              "Preserve IDs exactly" = "preserve"
            ),
            selected = "prefix_source",
            width = "100%"
          ),
          uiOutput(ns("id_policy_detail"))
        )
      ),
      tags$div(
        class = "llw-append-source-grid",
        uiOutput(ns("identity_controls"))
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_next_mapping"),
          "Continue to time & measurements",
          icon = icon("arrow-right"),
          class = "btn-primary btn-lg"
        )
      )
    )
  )
}

append_mapping_step_ui <- function(ns) {
  nav_panel(
    "Time and measurements",
    value = "mapping",
    tags$div(
      class = "llw-wizard-step-content",
      import_wizard_step_heading(
        "sliders",
        "Map time and measurements",
        "Define the union without guessing compatibility",
        paste(
          "Choose the timestamp and primary measurement in every source.",
          "Optional columns remain opt-in, including a select-all shortcut."
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        fillable = FALSE,
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(bsicons::bs_icon("columns-gap"), " Measurement layout"),
          uiOutput(ns("measurement_strategy_control")),
          uiOutput(ns("combined_measurement_controls"))
        ),
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(bsicons::bs_icon("shield-check"), " Row safety"),
          llw_status_callout(
            "idle",
            paste(
              "Rows that already share an ID and timestamp within a source are",
              "not created by append. They are retained automatically, counted,",
              "and marked in the result."
            ),
            heading = "Existing duplicates stay visible",
            compact = TRUE
          ),
          tags$p(
            class = "llw-secondary mt-3 mb-0",
            paste(
              "No interpolation, gap filling, implicit unit conversion, or",
              "source-row deletion is performed."
            )
          )
        )
      ),
      tags$div(
        class = "llw-append-source-grid",
        uiOutput(ns("mapping_controls"))
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_back_sources"),
          "Back to sources & IDs",
          icon = icon("arrow-left"),
          class = "btn-outline-secondary btn-lg"
        ),
        actionButton(
          ns("wizard_next_time"),
          "Continue to output time",
          icon = icon("arrow-right"),
          class = "btn-primary btn-lg"
        ),
        split = TRUE
      )
    )
  )
}

append_time_step_ui <- function(ns) {
  nav_panel(
    "Output time",
    value = "time",
    tags$div(
      class = "llw-wizard-step-content",
      import_wizard_step_heading(
        "clock-history",
        "Set the output time basis",
        "Choose whether clock labels or absolute instants stay fixed",
        paste(
          "The selected rule is applied to the main datetime and every other",
          "retained POSIXct column. Original source time context remains recorded."
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        fillable = FALSE,
        tags$section(
          class = "llw-wizard-field-group llw-append-time-controls",
          tags$h4(bsicons::bs_icon("globe2"), " Output time zone"),
          uiOutput(ns("time_controls"))
        ),
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(bsicons::bs_icon("table"), " Adjustment plan"),
          tags$p(
            class = "llw-secondary",
            paste(
              "This table previews the rule for each main datetime column.",
              "The same rule also applies to retained POSIXct columns."
            )
          ),
          tableOutput(ns("time_plan"))
        )
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_back_mapping"),
          "Back to time & measurements",
          icon = icon("arrow-left"),
          class = "btn-outline-secondary btn-lg"
        ),
        actionButton(
          ns("wizard_next_review"),
          "Continue to review & create",
          icon = icon("arrow-right"),
          class = "btn-primary btn-lg"
        ),
        split = TRUE
      )
    )
  )
}

append_review_step_ui <- function(ns) {
  nav_panel(
    "Review and create",
    value = "review",
    tags$div(
      class = "llw-wizard-step-content llw-append-review-content",
      import_wizard_step_heading(
        "clipboard-check",
        "Preview the complete append",
        "Review compatibility before creating a new dataset",
        paste(
          "Preview computes the full mapped union and a bounded row sample.",
          "Creation remains unavailable until that preview is current and safe."
        )
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_back_time"),
          "Back to output time",
          icon = icon("arrow-left"),
          class = "btn-outline-secondary"
        ),
        split = TRUE
      ),
      tags$section(
        class = "llw-append-create-panel",
        textInput(
          ns("display_name"),
          "New dataset name",
          value = "Appended dataset",
          width = "100%",
          updateOn = "blur"
        ),
        uiOutput(ns("preview_actions"))
      ),
      uiOutput(ns("preview_status")),
      uiOutput(ns("diagnostic_summary")),
      navset_card_pill(
        nav_panel(
          "Source comparison",
          tags$div(
            class = "llw-data-region llw-append-profile-region",
            role = "region",
            `aria-label` = "Append source compatibility comparison",
            tabindex = "0",
            uiOutput(ns("source_comparison"))
          )
        ),
        nav_panel(
          "Optional mappings",
          tags$div(
            class = "llw-data-region",
            role = "region",
            `aria-label` = "Append optional column mappings and coercions",
            tabindex = "0",
            tableOutput(ns("optional_comparison"))
          )
        ),
        nav_panel(
          "Row sample",
          tags$div(
            class = "llw-data-region",
            role = "region",
            `aria-label` = "Bounded append result preview",
            tabindex = "0",
            tags$p(
              class = "llw-secondary small llw-append-table-note",
              paste(
                "Internal append-provenance fields are hidden from this",
                "sample and remain stored in the immutable result."
              )
            ),
            DT::dataTableOutput(ns("row_preview"), height = "440px")
          )
        )
      )
    )
  )
}

append_preview_actions_ui <- function(ns, preview_exists, current, can_apply) {
  needs_update <- isTRUE(preview_exists) && !isTRUE(current)
  preview_prominent <- !isTRUE(preview_exists) || needs_update
  preview_label <- if (needs_update) {
    "Update preview"
  } else if (isTRUE(preview_exists)) {
    "Preview again"
  } else {
    "Preview append"
  }
  preview_button <- actionButton(
    ns("preview"),
    preview_label,
    icon = icon(if (needs_update) "rotate-right" else "magnifying-glass"),
    class = if (preview_prominent) {
      "btn-primary btn-lg llw-append-primary-action"
    } else {
      "btn-outline-primary"
    }
  )
  apply_ready <- isTRUE(current) && isTRUE(can_apply)
  apply_button <- actionButton(
    ns("apply"),
    "Create new immutable dataset",
    icon = icon("code-merge"),
    class = if (apply_ready) "btn-primary btn-lg" else "btn-outline-secondary",
    title = if (apply_ready) {
      "Create a new dataset and leave every source unchanged"
    } else {
      "Available after a current preview has no unresolved errors"
    }
  )
  if (!apply_ready) {
    apply_button <- htmltools::tagAppendAttributes(
      apply_button,
      disabled = "disabled",
      `aria-disabled` = "true"
    )
  }
  tags$div(
    class = paste(
      "llw-append-actions",
      if (preview_prominent) "llw-append-actions--preview" else
        "llw-append-actions--create"
    ),
    preview_button,
    apply_button
  )
}

append_diagnostic_summary_ui <- function(preview = NULL) {
  diagnostics <- preview$diagnostics %||% list()
  value <- function(field) {
    current <- diagnostics[[field]]
    if (length(current) != 1L || is.na(current)) "--" else
      format(current, big.mark = ",")
  }
  items <- list(
    list(
      label = "Source datasets",
      value = value("source_datasets"),
      icon = "layers"
    ),
    list(
      label = "Result rows",
      value = value("result_rows"),
      icon = "table"
    ),
    list(
      label = "Existing duplicate rows",
      value = value("duplicate_rows"),
      icon = "copy"
    ),
    list(
      label = "Cross-source overlap rows",
      value = value("overlap_rows"),
      icon = "intersect"
    )
  )
  tags$dl(
    class = "llw-append-diagnostics",
    lapply(items, function(item) {
      tags$div(
        tags$dt(
          tags$span(
            class = "llw-append-diagnostics__icon",
            `aria-hidden` = "true",
            bsicons::bs_icon(item$icon)
          ),
          item$label
        ),
        tags$dd(item$value)
      )
    })
  )
}

append_profile_value <- function(value, suffix = "") {
  if (length(value) != 1L || is.na(value) || !nzchar(as.character(value))) {
    return("Unknown")
  }
  paste0(as.character(value), suffix)
}

append_source_comparison_ui <- function(preview) {
  profiles <- preview$profiles
  cards <- lapply(seq_len(nrow(profiles)), function(index) {
    profile <- profiles[index, , drop = FALSE]
    item <- function(label, value) {
      tags$div(tags$dt(label), tags$dd(value))
    }
    card(
      class = "llw-append-profile-card",
      card_header(profile$dataset[[1L]]),
      tags$dl(
        item("Rows", format(profile$rows[[1L]], big.mark = ",")),
        item("Unique IDs", format(profile$participants[[1L]], big.mark = ",")),
        item(
          "ID mapping",
          tags$code(profile$participant[[1L]])
        ),
        item(
          "Time mapping",
          tagList(
            tags$code(profile$datetime[[1L]]),
            " | source manifest zone ",
            append_profile_value(profile$source_timezone[[1L]]),
            " | recorded POSIXct zone ",
            append_profile_value(profile$datetime_timezone[[1L]]),
            " | output zone ",
            append_profile_value(profile$output_timezone[[1L]]),
            " | ",
            append_time_preservation_label(
              profile$time_alignment[[1L]],
              profile$datetime_timezone[[1L]],
              profile$output_timezone[[1L]]
            )
          )
        ),
        item(
          "Measurement mapping",
          tagList(
            tags$code(profile$measurement[[1L]]),
            " -> ",
            tags$code(profile$measurement_target[[1L]])
          )
        ),
        item("Recorded unit", append_unit_label(profile$unit[[1L]])),
        item("Device", append_profile_value(profile$device[[1L]])),
        item(
          "Dominant sampling",
          append_profile_value(profile$sampling_seconds[[1L]], " seconds")
        ),
        item("Calibration", append_profile_value(profile$calibration[[1L]]))
      )
    )
  })
  tags$div(class = "llw-append-profile-grid", cards)
}

append_merge_ui <- function(id) {
  ns <- NS(id)
  tags$section(
    class = "llw-import-wizard llw-append-wizard",
    `aria-labelledby` = ns("wizard_title"),
    tags$header(
      class = "llw-import-wizard__hero",
      tags$span(
        class = "llw-import-wizard__hero-icon",
        `aria-hidden` = "true",
        bsicons::bs_icon("intersect")
      ),
      tags$div(
        tags$p(class = "llw-wizard-eyebrow", "Dataset library"),
        tags$h2(id = ns("wizard_title"), "Append datasets safely"),
        tags$p(
          paste(
            "Build a row-wise union in four guided steps. Every preview keeps",
            "source rows and context visible; creation leaves every source unchanged."
          )
        )
      ),
      tags$span(
        class = "llw-import-wizard__engine-note",
        bsicons::bs_icon("shield-check"),
        " Immutable sources"
      )
    ),
    uiOutput(ns("wizard_stepper")),
    navset_hidden(
      id = ns("wizard_steps"),
      selected = "sources",
      append_sources_step_ui(ns),
      append_mapping_step_ui(ns),
      append_time_step_ui(ns),
      append_review_step_ui(ns)
    )
  )
}

append_input_value <- function(input, id, default = NULL) {
  value <- input[[id]]
  if (is.null(value) || length(value) == 0L) default else value
}

append_spec_from_inputs <- function(input, records) {
  source_ids <- input$source_ids %||% character()
  if (length(source_ids) < 2L) {
    abort_llw(
      "Choose at least two source datasets before previewing an append.",
      type = "validation",
      public_message = "Choose at least two source datasets."
    )
  }
  if (!all(source_ids %in% names(records))) {
    abort_llw(
      "A selected source dataset is no longer available.",
      type = "validation"
    )
  }
  strategy <- input$measurement_strategy %||% "separate"
  participant_policy <- input$participant_policy %||% "prefix_source"
  identical_candidates <- append_common_measurement_candidates(
    records,
    source_ids
  )
  if (identical(strategy, "identical") && nrow(identical_candidates) == 0L) {
    abort_llw(
      paste(
        "No numeric measurement column has the same name and known unit in",
        "every selected source."
      ),
      type = "validation"
    )
  }
  identical_measurement <- if (identical(strategy, "identical")) {
    input$identical_measurement %||% identical_candidates$column[[1L]]
  } else {
    NULL
  }
  if (
    identical(strategy, "identical") &&
      !identical_measurement %in% identical_candidates$column
  ) {
    abort_llw(
      "Choose an available identical measurement column.",
      type = "validation"
    )
  }
  output_measurement <- if (identical(strategy, "identical")) {
    identical_measurement
  } else {
    trimws(input$output_measurement %||% "")
  }
  output_unit <- if (identical(strategy, "identical")) {
    identical_candidates$unit[
      match(identical_measurement, identical_candidates$column)
    ]
  } else {
    trimws(input$output_unit %||% "")
  }
  mappings <- lapply(seq_along(source_ids), function(index) {
    dataset_id <- source_ids[[index]]
    record <- records[[dataset_id]]
    choices <- append_record_column_choices(record)
    if (
      length(choices$participant) == 0L ||
        length(choices$datetime) == 0L ||
        length(choices$measurement) == 0L
    ) {
      abort_llw(
        paste0(
          "Dataset `",
          record$display_name,
          "` has no complete ID, POSIXct datetime, and numeric measurement mapping."
        ),
        type = "validation",
        public_message = paste0(
          "Dataset `",
          record$display_name,
          "` does not provide every required append mapping."
        )
      )
    }
    participant <- append_input_value(
      input,
      append_mapping_input_id(dataset_id, "participant"),
      choices$participant[[1L]]
    )
    datetime <- append_input_value(
      input,
      append_mapping_input_id(dataset_id, "datetime"),
      choices$datetime[[1L]]
    )
    measurement <- if (identical(strategy, "identical")) {
      identical_measurement
    } else {
      append_input_value(
        input,
        append_mapping_input_id(dataset_id, "measurement"),
        choices$measurement[[1L]]
      )
    }
    optional <- append_input_value(
      input,
      append_mapping_input_id(dataset_id, "optional"),
      character()
    )
    eligible_optional <- setdiff(
      choices$optional,
      c(participant, datetime, measurement)
    )
    optional <- append_selected_optional_columns(optional, eligible_optional)
    optional_targets <- vapply(
      optional,
      function(column) {
        trimws(append_input_value(
          input,
          append_mapping_input_id(dataset_id, "optional_target", column),
          column
        ))
      },
      character(1)
    )
    optional_coercions <- vapply(
      optional,
      function(column) {
        append_input_value(
          input,
          append_mapping_input_id(dataset_id, "optional_coercion", column),
          "none"
        )
      },
      character(1)
    )
    measurement_target <- if (strategy %in% c("combine", "identical")) {
      output_measurement
    } else {
      trimws(append_input_value(
        input,
        append_mapping_input_id(dataset_id, "measurement_target"),
        paste0("source_", index, "_", measurement)
      ))
    }
    new_append_source_mapping(
      dataset_id = dataset_id,
      participant_column = participant,
      datetime_column = datetime,
      measurement_column = measurement,
      measurement_target = measurement_target,
      participant_prefix = if (identical(participant_policy, "prefix_source")) {
        trimws(append_input_value(
          input,
          append_mapping_input_id(dataset_id, "prefix"),
          paste0("S", index)
        ))
      } else {
        NULL
      },
      optional_columns = optional,
      optional_targets = optional_targets,
      optional_coercions = optional_coercions
    )
  })
  time_selection <- append_time_selection(input, records, source_ids)
  overlap_policy <- if (identical(participant_policy, "preserve")) {
    input$overlap_policy %||% "error"
  } else {
    "not_applicable"
  }
  new_append_spec(
    mappings = mappings,
    display_name = trimws(input$display_name %||% ""),
    participant_policy = participant_policy,
    measurement_strategy = strategy,
    duplicate_policy = "keep_marked",
    overlap_policy = overlap_policy,
    time_alignment = time_selection$alignment,
    output_timezone = time_selection$output_timezone,
    output_measurement = if (nzchar(output_measurement)) output_measurement else
      NULL,
    output_unit = if (nzchar(output_unit)) output_unit else NA_character_,
    confirm_quantity = isTRUE(input$confirm_quantity),
    acknowledge_unit_difference = isTRUE(input$acknowledge_unit_difference),
    acknowledge_unknown_units = isTRUE(input$acknowledge_unknown_units),
    acknowledge_device_difference = isTRUE(input$acknowledge_device_difference)
  )
}

append_merge_server <- function(id, datasets, selected_dataset_id) {
  if (
    !shiny::is.reactive(datasets) || !shiny::is.reactive(selected_dataset_id)
  ) {
    abort_llw(
      "`datasets` and `selected_dataset_id` must be reactive inputs.",
      type = "validation"
    )
  }
  moduleServer(id, function(input, output, session) {
    preview_value <- reactiveVal(NULL)
    preview_error <- reactiveVal(NULL)
    added_dataset_value <- reactiveVal(NULL)
    wizard_step <- reactiveVal("sources")
    wizard_completed <- reactiveVal(character())

    selected_source_ids <- reactive({
      intersect(input$source_ids %||% character(), names(datasets()))
    })

    identity_ready <- reactive({
      records <- datasets()
      source_ids <- selected_source_ids()
      if (length(source_ids) < 2L) return(FALSE)
      policy <- input$participant_policy %||% "prefix_source"
      mapped <- vapply(
        source_ids,
        function(dataset_id) {
          choices <- append_record_column_choices(records[[dataset_id]])
          if (length(choices$participant) == 0L) return(FALSE)
          selected <- input[[append_mapping_input_id(
            dataset_id,
            "participant"
          )]] %||%
            choices$participant[[1L]]
          selected %in% choices$participant
        },
        logical(1)
      )
      if (!all(mapped)) return(FALSE)
      if (identical(policy, "prefix_source")) {
        prefixes <- vapply(
          seq_along(source_ids),
          function(index) {
            input[[append_mapping_input_id(source_ids[[index]], "prefix")]] %||%
              paste0("S", index)
          },
          character(1)
        )
        return(all(nzchar(trimws(prefixes))) && !anyDuplicated(prefixes))
      }
      (input$overlap_policy %||% "error") %in% c("error", "keep_marked")
    })

    mapping_ready <- reactive({
      if (!identity_ready()) return(FALSE)
      spec <- tryCatch(
        append_spec_from_inputs(input, datasets()),
        error = identity
      )
      !inherits(spec, "error")
    })

    time_ready <- reactive({
      if (!mapping_ready()) return(FALSE)
      spec <- tryCatch(
        append_spec_from_inputs(input, datasets()),
        error = identity
      )
      if (inherits(spec, "error")) return(FALSE)
      profiles <- lapply(
        spec$mappings,
        function(mapping) append_source_profile(datasets(), mapping)
      )
      length(append_time_issues(profiles, spec)$errors) == 0L
    })

    completed_steps <- reactive({
      completed <- wizard_completed()
      if (!identity_ready()) completed <- character()
      if (!mapping_ready())
        completed <- setdiff(completed, c("mapping", "time", "review"))
      if (!time_ready()) completed <- setdiff(completed, c("time", "review"))
      preview <- preview_value()
      if (
        !is.null(preview) &&
          isTRUE(preview_is_current()) &&
          isTRUE(preview$can_apply)
      ) {
        completed <- unique(c(completed, "review"))
      } else {
        completed <- setdiff(completed, "review")
      }
      completed
    })

    preview_is_current <- reactive({
      preview <- preview_value()
      if (is.null(preview)) return(FALSE)
      current_spec <- tryCatch(
        append_spec_from_inputs(input, datasets()),
        error = identity
      )
      if (inherits(current_spec, "error")) return(FALSE)
      identical(
        append_source_fingerprint(datasets(), current_spec),
        preview$fingerprint
      )
    })

    preview_resolution_step <- reactive({
      error <- preview_error()
      if (inherits(error, "error")) {
        return(append_error_step_from_message(llw_public_message(error)))
      }
      preview <- preview_value()
      if (is.null(preview) || isTRUE(preview$can_apply)) return(NULL)
      preview$error_step %||%
        append_preferred_error_step(character(), preview$errors) %||%
        "mapping"
    })

    select_wizard_step <- function(value) {
      wizard_step(value)
      bslib::nav_select("wizard_steps", selected = value, session = session)
    }

    notify_append <- function(message, type = "warning") {
      showNotification(
        message,
        type = type,
        duration = if (identical(type, "error")) 8 else 6
      )
    }

    open_mapping_step <- function() {
      if (!identity_ready()) {
        notify_append(
          paste(
            "Choose at least two sources, a valid ID column for each source,",
            "and unique non-empty prefixes when prefixing IDs."
          )
        )
        return()
      }
      wizard_completed(unique(c(wizard_completed(), "sources")))
      select_wizard_step("mapping")
    }

    open_time_step <- function() {
      spec <- tryCatch(
        append_spec_from_inputs(input, datasets()),
        error = identity
      )
      if (inherits(spec, "error")) {
        notify_append(llw_public_message(spec), type = "error")
        return()
      }
      wizard_completed(unique(c(wizard_completed(), "sources", "mapping")))
      select_wizard_step("time")
    }

    open_review_step <- function() {
      spec <- tryCatch(
        append_spec_from_inputs(input, datasets()),
        error = identity
      )
      if (inherits(spec, "error")) {
        notify_append(llw_public_message(spec), type = "error")
        return()
      }
      profiles <- lapply(
        spec$mappings,
        function(mapping) append_source_profile(datasets(), mapping)
      )
      time_issues <- append_time_issues(profiles, spec)
      if (length(time_issues$errors) > 0L) {
        notify_append(paste(time_issues$errors, collapse = " "), type = "error")
        return()
      }
      wizard_completed(unique(c(
        wizard_completed(),
        "sources",
        "mapping",
        "time"
      )))
      select_wizard_step("review")
    }

    output$wizard_stepper <- renderUI({
      append_wizard_stepper(
        session$ns,
        active = wizard_step(),
        completed = completed_steps()
      )
    })

    observe(select_wizard_step("sources")) |>
      bindEvent(input$wizard_step_sources, ignoreInit = TRUE)
    observe(open_mapping_step()) |>
      bindEvent(input$wizard_step_mapping, ignoreInit = TRUE)
    observe(open_time_step()) |>
      bindEvent(input$wizard_step_time, ignoreInit = TRUE)
    observe(open_review_step()) |>
      bindEvent(input$wizard_step_review, ignoreInit = TRUE)
    observe(open_mapping_step()) |>
      bindEvent(input$wizard_next_mapping, ignoreInit = TRUE)
    observe(select_wizard_step("sources")) |>
      bindEvent(input$wizard_back_sources, ignoreInit = TRUE)
    observe(open_time_step()) |>
      bindEvent(input$wizard_next_time, ignoreInit = TRUE)
    observe(select_wizard_step("mapping")) |>
      bindEvent(input$wizard_back_mapping, ignoreInit = TRUE)
    observe(open_review_step()) |>
      bindEvent(input$wizard_next_review, ignoreInit = TRUE)
    observe(select_wizard_step("time")) |>
      bindEvent(input$wizard_back_time, ignoreInit = TRUE)
    observe({
      step <- preview_resolution_step()
      req(step)
      select_wizard_step(step)
    }) |>
      bindEvent(input$resolve_preview_error, ignoreInit = TRUE)

    output$source_selector <- renderUI({
      records <- datasets()
      if (length(records) < 2L) {
        return(llw_status_callout(
          "idle",
          paste(
            "Load or import at least two datasets. Ready-to-use examples in",
            "the session sidebar let you test this without repeating import forms."
          ),
          heading = "Two sources are required",
          compact = TRUE
        ))
      }
      selected <- isolate(input$source_ids)
      selected <- intersect(selected %||% character(), names(records))
      if (length(selected) < 2L) {
        selected <- append_default_source_ids(records, selected_dataset_id())
      }
      checkboxGroupInput(
        session$ns("source_ids"),
        "Datasets to append",
        choices = append_source_choice_labels(records),
        selected = selected,
        width = "100%"
      )
    })

    output$source_selection_status <- renderUI({
      if (length(datasets()) < 2L || is.null(input$source_ids)) return(NULL)
      selected_count <- length(selected_source_ids())
      if (selected_count >= 2L) return(NULL)
      llw_status_callout(
        "warning",
        if (selected_count == 0L) {
          "Select at least two datasets before continuing."
        } else {
          "Select one more dataset; append requires at least two sources."
        },
        heading = "At least two datasets are required",
        live = TRUE,
        compact = TRUE
      )
    })

    output$id_policy_detail <- renderUI({
      append_id_policy_detail_ui(
        session$ns,
        input$participant_policy %||% "prefix_source",
        overlap_policy = isolate(input$overlap_policy) %||% "error"
      )
    })

    output$identity_controls <- renderUI({
      records <- datasets()
      source_ids <- selected_source_ids()
      if (length(source_ids) < 2L) return(NULL)
      policy <- input$participant_policy %||% "prefix_source"
      tagList(lapply(seq_along(source_ids), function(index) {
        append_identity_card_ui(
          records[[source_ids[[index]]]],
          index,
          input,
          session,
          policy
        )
      }))
    })

    output$measurement_strategy_control <- renderUI({
      candidates <- append_common_measurement_candidates(
        datasets(),
        selected_source_ids()
      )
      append_measurement_strategy_ui(
        session$ns,
        input$measurement_strategy %||% "separate",
        candidates
      )
    })

    output$mapping_controls <- renderUI({
      records <- datasets()
      source_ids <- selected_source_ids()
      if (length(source_ids) < 2L) return(NULL)
      strategy <- input$measurement_strategy %||% "separate"
      candidates <- append_common_measurement_candidates(records, source_ids)
      identical_measurement <- if (
        identical(strategy, "identical") && nrow(candidates) > 0L
      ) {
        input$identical_measurement %||% candidates$column[[1L]]
      } else {
        NULL
      }
      tagList(lapply(seq_along(source_ids), function(index) {
        dataset_id <- source_ids[[index]]
        input[[append_mapping_input_id(dataset_id, "participant")]]
        input[[append_mapping_input_id(dataset_id, "datetime")]]
        input[[append_mapping_input_id(dataset_id, "measurement")]]
        input[[append_mapping_input_id(dataset_id, "optional")]]
        input$identical_measurement
        append_mapping_card_ui(
          records[[dataset_id]],
          index,
          input,
          session,
          strategy,
          identical_measurement = identical_measurement
        )
      }))
    })

    output$combined_measurement_controls <- renderUI({
      strategy <- input$measurement_strategy %||% "separate"
      if (identical(strategy, "separate")) {
        return(llw_status_callout(
          "idle",
          paste(
            "Each source measurement will remain in its own output column.",
            "You can select a primary analysis variable later."
          ),
          heading = "Source measurements remain separate",
          compact = TRUE
        ))
      }
      records <- datasets()
      source_ids <- selected_source_ids()
      quantity_and_device <- tagList(
        checkboxInput(
          session$ns("confirm_quantity"),
          paste(
            "I confirm the mapped columns represent the intended compatible",
            "quantity; no value conversion is performed."
          ),
          value = isTRUE(isolate(input$confirm_quantity))
        ),
        checkboxInput(
          session$ns("acknowledge_device_difference"),
          paste(
            "I understand that matching names or units do not make different",
            "or unknown devices interchangeable."
          ),
          value = isTRUE(isolate(input$acknowledge_device_difference))
        )
      )
      if (identical(strategy, "identical")) {
        candidates <- append_common_measurement_candidates(records, source_ids)
        if (nrow(candidates) == 0L) {
          return(llw_status_callout(
            "error",
            paste(
              "Choose another measurement layout or select sources with a",
              "shared numeric column name and known unit."
            ),
            heading = "No identical measurement is available",
            compact = TRUE
          ))
        }
        selected <- input$identical_measurement %||% candidates$column[[1L]]
        if (!selected %in% candidates$column)
          selected <- candidates$column[[1L]]
        unit <- candidates$unit[match(selected, candidates$column)]
        return(tagList(
          llw_status_callout(
            "complete",
            paste0(
              "`",
              selected,
              "` will retain its name and recorded unit `",
              unit,
              "` in the merged dataset."
            ),
            heading = "Identical measurement mapping available",
            compact = TRUE
          ),
          selectizeInput(
            session$ns("identical_measurement"),
            "Identical measurement column",
            choices = stats::setNames(candidates$column, candidates$column),
            selected = selected,
            width = "100%"
          ),
          quantity_and_device
        ))
      }
      details <- append_selected_measurement_details(records, source_ids, input)
      defaults <- append_combined_measurement_defaults(details)
      current_name <- isolate(input$output_measurement) %||% ""
      output_name <- if (
        !nzchar(trimws(current_name)) ||
          (identical(current_name, "Measurement") &&
            !identical(defaults$column, "Measurement"))
      ) {
        defaults$column
      } else {
        current_name
      }
      current_unit <- isolate(input$output_unit) %||% ""
      output_unit <- if (!nzchar(trimws(current_unit))) {
        defaults$unit
      } else {
        current_unit
      }
      tagList(
        llw_status_callout(
          "idle",
          paste0(
            "Recorded source units: ",
            append_unit_list(details$unit),
            "."
          ),
          heading = "Unit labels remain explicit",
          compact = TRUE
        ),
        textInput(
          session$ns("output_measurement"),
          "Combined measurement output name",
          value = output_name,
          width = "100%",
          updateOn = "blur"
        ),
        textInput(
          session$ns("output_unit"),
          "Confirmed output unit",
          value = output_unit,
          placeholder = "Enter a unit or write Unknown",
          width = "100%",
          updateOn = "blur"
        ),
        quantity_and_device[[1L]],
        checkboxInput(
          session$ns("acknowledge_unit_difference"),
          paste(
            "If recorded unit labels genuinely differ, I confirm they use the",
            "same numeric scale. Values will be combined without conversion."
          ),
          value = isTRUE(isolate(input$acknowledge_unit_difference))
        ),
        checkboxInput(
          session$ns("acknowledge_unknown_units"),
          paste(
            "I reviewed any unknown units and accept a visible uncertainty",
            "warning in the merged provenance."
          ),
          value = isTRUE(isolate(input$acknowledge_unknown_units))
        ),
        quantity_and_device[[2L]]
      )
    })

    output$time_controls <- renderUI({
      source_ids <- selected_source_ids()
      if (length(source_ids) < 2L) return(NULL)
      append_time_controls_ui(
        session$ns,
        input,
        datasets(),
        source_ids
      )
    })

    output$time_plan <- renderTable(
      {
        source_ids <- selected_source_ids()
        validate(need(
          length(source_ids) >= 2L,
          "Select at least two source datasets."
        ))
        plan <- tryCatch(
          append_time_plan_from_inputs(input, datasets(), source_ids),
          error = identity
        )
        validate(need(
          !inherits(plan, "error"),
          if (inherits(plan, "error")) llw_public_message(plan) else ""
        ))
        plan
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )

    output$preview_actions <- renderUI({
      preview <- preview_value()
      append_preview_actions_ui(
        session$ns,
        preview_exists = !is.null(preview),
        current = preview_is_current(),
        can_apply = !is.null(preview) && isTRUE(preview$can_apply)
      )
    })

    output$diagnostic_summary <- renderUI({
      append_diagnostic_summary_ui(preview_value())
    })

    observe({
      req(input$preview > 0)
      result <- tryCatch(
        {
          spec <- append_spec_from_inputs(input, datasets())
          preview_append_merge(datasets(), spec)
        },
        error = identity
      )
      if (inherits(result, "error")) {
        preview_value(NULL)
        preview_error(result)
      } else {
        preview_value(result)
        preview_error(NULL)
      }
    }) |>
      bindEvent(input$preview, ignoreInit = TRUE)

    observe({
      req(input$apply > 0)
      preview <- preview_value()
      if (is.null(preview)) {
        showNotification(
          "Create a successful, current append preview first.",
          type = "error",
          duration = 8
        )
        return()
      }
      current_spec <- tryCatch(
        append_spec_from_inputs(input, datasets()),
        error = identity
      )
      if (inherits(current_spec, "error")) {
        showNotification(
          llw_public_message(current_spec),
          type = "error",
          duration = 8
        )
        return()
      }
      current_fingerprint <- append_source_fingerprint(datasets(), current_spec)
      if (!identical(current_fingerprint, preview$fingerprint)) {
        showNotification(
          paste(
            "The sources or mappings changed after preview.",
            "Update the preview before creating the dataset."
          ),
          type = "error",
          duration = 8
        )
        return()
      }
      record <- tryCatch(new_appended_dataset_record(preview), error = identity)
      if (inherits(record, "error")) {
        showNotification(
          llw_public_message(record),
          type = "error",
          duration = 8
        )
        return()
      }
      added_dataset_value(record)
      preview_value(NULL)
      preview_error(NULL)
    }) |>
      bindEvent(input$apply, ignoreInit = TRUE)

    output$preview_status <- renderUI({
      error <- preview_error()
      if (inherits(error, "error")) {
        return(llw_status_callout(
          "error",
          llw_public_message(error),
          heading = "Preview could not be created",
          action = append_error_step_action_ui(
            session$ns,
            preview_resolution_step()
          )
        ))
      }
      preview <- preview_value()
      if (is.null(preview)) {
        return(llw_status_callout(
          "idle",
          paste(
            "Review the source-specific mappings, then preview the complete",
            "union. Source datasets will not be changed."
          ),
          heading = "Waiting for a preview"
        ))
      }
      if (!preview_is_current()) {
        return(llw_status_callout(
          "stale",
          paste(
            "A source, mapping, confirmation, or output label changed after",
            "this preview. Select Update preview before creating the dataset."
          ),
          heading = "Preview is out of date"
        ))
      }
      if (!isTRUE(preview$can_apply)) {
        return(tagList(
          llw_status_callout(
            "error",
            paste(preview$errors, collapse = " "),
            heading = "Resolve before creation",
            action = append_error_step_action_ui(
              session$ns,
              preview_resolution_step()
            )
          ),
          if (length(preview$warnings) > 0L)
            llw_status_callout(
              "warning",
              paste(preview$warnings, collapse = " "),
              heading = "Additional warnings"
            )
        ))
      }
      tagList(
        llw_status_callout(
          if (length(preview$warnings) > 0L) "warning" else "complete",
          if (length(preview$warnings) > 0L) {
            paste(preview$warnings, collapse = " ")
          } else {
            paste(
              "Every required mapping and policy is resolved. Create will add",
              "a new immutable record and leave all sources unchanged."
            )
          },
          heading = if (length(preview$warnings) > 0L) {
            "Ready with recorded warnings"
          } else {
            "Ready to create the appended dataset"
          }
        )
      )
    })

    output$source_comparison <- renderUI({
      preview <- preview_value()
      if (is.null(preview)) {
        return(tags$p(
          class = "llw-secondary mb-0",
          "Create a preview to compare the mapped sources."
        ))
      }
      append_source_comparison_ui(preview)
    })
    output$optional_comparison <- renderTable(
      {
        preview <- preview_value()
        req(preview)
        comparison <- preview$optional_comparison
        validate(need(nrow(comparison) > 0L, "No optional columns are mapped."))
        comparison <- comparison[
          c(
            "dataset",
            "source_column",
            "target_column",
            "source_type",
            "coercion"
          )
        ]
        names(comparison) <- c(
          "Dataset",
          "Source column",
          "Output column",
          "Recorded type",
          "Type handling"
        )
        comparison
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )
    output$row_preview <- DT::renderDataTable(
      {
        preview <- preview_value()
        req(preview)
        validate(need(
          nrow(preview$preview_data) > 0L,
          "No row preview is available until the mapped union can be constructed."
        ))
        append_display_data(preview$preview_data)
      },
      options = list(pageLength = 20, scrollX = TRUE)
    )

    list(
      preview = reactive(preview_value()),
      error = reactive(preview_error()),
      is_current = reactive(preview_is_current()),
      add_dataset = reactive(added_dataset_value())
    )
  })
}

append_merge_showcase_records <- function() {
  first <- m1_showcase_record()
  first$display_name <- "UTC MEDI fixture"
  first$factual_metadata$device <- "ActLumus"
  first$factual_metadata$variables <- list(
    MEDI = list(
      label = "Melanopic equivalent daylight illuminance",
      unit = "lux",
      calibration = "Development fixture only"
    )
  )
  first <- validate_dataset_record(first)
  second_data <- data.frame(
    Subject = c("P01", "P01", "P03"),
    Timestamp = as.POSIXct(
      c(
        "2025-12-31 22:00:00",
        "2025-12-31 22:01:00",
        "2025-12-31 22:00:00"
      ),
      tz = "America/New_York"
    ),
    LightValue = c(12, 22, 32),
    Context = c("indoors", "outside", "outside"),
    stringsAsFactors = FALSE
  )
  second <- new_dataset_record(
    raw_data = second_data,
    display_name = "New York differing-schema fixture",
    source_manifest = new_source_manifest(
      source_type = "development_fixture",
      source_timezone = "America/New_York",
      details = list(device = "Fixture sensor")
    ),
    factual_metadata = list(
      device = "Fixture sensor",
      variables = list(
        LightValue = list(
          label = "Fixture light value",
          unit = "lx",
          calibration = "Development fixture only"
        )
      )
    ),
    analysis_settings = list(primary_variable = "LightValue")
  )
  third_data <- data.frame(
    Id = c("B01", "B01", "B02", "B02"),
    Datetime = as.POSIXct(
      c(
        "2025-07-01 18:00:00",
        "2025-07-01 18:01:00",
        "2025-07-01 18:00:00",
        "2025-07-01 18:01:00"
      ),
      tz = "Europe/Berlin"
    ),
    MEDI = c(18, 28, 38, 48),
    DiaryTime = as.POSIXct(
      c(
        "2025-07-01 17:45:00",
        "2025-07-01 17:45:00",
        "2025-07-01 17:50:00",
        "2025-07-01 17:50:00"
      ),
      tz = "Europe/Berlin"
    ),
    Context = c("indoors", "window", "outdoors", "outdoors"),
    stringsAsFactors = FALSE
  )
  third <- new_dataset_record(
    raw_data = third_data,
    display_name = "Berlin matching MEDI fixture",
    source_manifest = new_source_manifest(
      source_type = "development_fixture",
      source_timezone = "Europe/Berlin",
      details = list(device = "ActLumus fixture")
    ),
    factual_metadata = list(
      device = "ActLumus fixture",
      variables = list(
        MEDI = list(
          label = "Melanopic equivalent daylight illuminance",
          unit = "lux",
          calibration = "Development fixture only"
        )
      )
    ),
    analysis_settings = list(primary_variable = "MEDI")
  )
  stats::setNames(
    list(first, second, third),
    c(first$id, second$id, third$id)
  )
}

append_merge_app <- function(...) {
  records <- append_merge_showcase_records()
  ui <- lightlogweb_page(page_fluid(
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell",
      tabindex = "-1",
      append_merge_ui("append"),
      card(
        card_header("Development status"),
        tableOutput("status")
      ),
      card(
        class = "llw-append-development-inspector",
        card_header(
          bsicons::bs_icon("table"),
          " Development dataset inspector"
        ),
        tags$p(
          class = "llw-secondary",
          paste(
            "Show any ready-to-use fixture or newly created append result.",
            "Internal append-provenance fields are hidden from the table but",
            "remain stored. This is a showcase aid, not part of the production",
            "workflow."
          )
        ),
        selectizeInput(
          "inspect_dataset",
          "Dataset to inspect",
          choices = character(),
          width = "100%"
        ),
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Development dataset table",
          tabindex = "0",
          DT::dataTableOutput("inspect_table", height = "420px")
        )
      )
    ),
    theme = lightlogweb_theme()
  ))
  server <- function(input, output, session) {
    current <- reactiveVal(records)
    selected <- reactiveVal(names(records)[[1L]])
    module <- append_merge_server(
      "append",
      datasets = reactive(current()),
      selected_dataset_id = selected
    )
    observe({
      record <- module$add_dataset()
      req(record)
      next_records <- current()
      next_records[[record$id]] <- record
      current(next_records)
      selected(record$id)
    }) |>
      bindEvent(module$add_dataset(), ignoreInit = TRUE)
    observe({
      available <- current()
      choices <- stats::setNames(
        names(available),
        vapply(available, `[[`, character(1), "display_name")
      )
      existing <- isolate(input$inspect_dataset)
      selected_inspector <- if (
        is.character(existing) &&
          length(existing) == 1L &&
          existing %in% names(available)
      ) {
        existing
      } else {
        selected()
      }
      updateSelectizeInput(
        session,
        "inspect_dataset",
        choices = choices,
        selected = selected_inspector,
        server = TRUE
      )
    })
    output$inspect_table <- DT::renderDataTable(
      {
        req(input$inspect_dataset)
        record <- current()[[input$inspect_dataset]]
        req(record)
        append_display_data(dataset_raw_data(record))
      },
      options = list(pageLength = 20, scrollX = TRUE)
    )
    output$status <- renderTable({
      preview <- module$preview()
      data.frame(
        Signal = c("Dataset count", "Preview state", "Selected dataset"),
        Value = c(
          length(current()),
          if (is.null(preview)) "Waiting" else if (!module$is_current()) {
            "Stale"
          } else if (preview$can_apply) {
            "Ready"
          } else {
            "Blocked"
          },
          selected()
        ),
        check.names = FALSE
      )
    })
  }
  shinyApp(ui, server, ...)
}
