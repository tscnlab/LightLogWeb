raw_import_phase_labels <- function() {
  c(
    validation = "Check files and settings",
    import = "Read files with LightLogR",
    normalization = "Verify source filenames and participant IDs",
    quality = "Check data quality",
    preview = "Prepare preview"
  )
}

raw_import_phase_snapshot <- function(
  status,
  result = NULL,
  error = NULL,
  request_ready = FALSE,
  progress = NULL
) {
  labels <- raw_import_phase_labels()
  phases <- data.frame(
    phase = names(labels),
    label = unname(labels),
    status = "pending",
    detail = "Waiting for the previous step.",
    stringsAsFactors = FALSE
  )
  if (inherits(result, "llw_raw_import_result")) {
    matched <- match(phases$phase, result$phases$phase)
    phases$status <- result$phases$status[matched]
    phases$detail <- paste0(
      result$phases$detail[matched],
      " (",
      format(result$phases$elapsed_seconds[matched], trim = TRUE),
      " s)"
    )
    return(phases)
  }
  state <- status$state
  if (isTRUE(request_ready)) {
    phases$status[[1L]] <- "complete"
    phases$detail[[
      1L
    ]] <- "The files and import settings passed the first checks."
  }
  if (identical(state, "queued")) {
    phases$status[[2L]] <- "queued"
    phases$detail[[2L]] <- "The import is waiting to start."
  } else if (identical(state, "running") && is.null(progress)) {
    phases$status[2:4] <- "running"
    phases$detail[2:4] <- c(
      "LightLogR is reading the private copies of the selected files.",
      "The app is verifying source filenames and the participant IDs created by LightLogR.",
      "The app will check all rows for time-series and missing-data problems."
    )
  } else if (identical(state, "finalizing") && is.null(progress)) {
    phases$status[2:4] <- "complete"
    phases$detail[
      2:4
    ] <- "The data checks are complete."
    phases$status[[5L]] <- "running"
    phases$detail[[
      5L
    ]] <- "Preparing summaries and a small preview of the imported rows."
  } else if (state %in% c("cancelled", "stale")) {
    index <- which(phases$status == "pending")[[1L]]
    phases$status[[index]] <- state
    phases$detail[[index]] <- status$message
  } else if (identical(state, "error")) {
    failed_phase <- if (
      inherits(error, "llw_error") &&
        !is.null(error$diagnostics$phase) &&
        error$diagnostics$phase %in% phases$phase
    ) {
      error$diagnostics$phase
    } else {
      "validation"
    }
    failed_index <- match(failed_phase, phases$phase)
    if (failed_index > 1L && isTRUE(request_ready)) {
      phases$status[seq_len(failed_index - 1L)] <- "complete"
    }
    phases$status[[failed_index]] <- "error"
    phases$detail[[failed_index]] <- llw_public_message(error)
  }
  if (
    is.data.frame(progress) &&
      all(
        c("phase", "status", "elapsed_seconds", "detail") %in%
          names(progress)
      ) &&
      !state %in% c("error", "cancelled", "stale")
  ) {
    valid <- progress$phase %in%
      phases$phase &
      progress$status %in% c("running", "complete")
    progress <- progress[valid, , drop = FALSE]
    matched <- match(progress$phase, phases$phase)
    phases$status[matched] <- progress$status
    phases$detail[matched] <- ifelse(
      progress$status == "complete" & is.finite(progress$elapsed_seconds),
      paste0(
        progress$detail,
        " (",
        format(progress$elapsed_seconds, trim = TRUE),
        " s)"
      ),
      progress$detail
    )
  }
  phases
}

raw_import_phase_ui <- function(phases) {
  states <- c(
    pending = "Pending",
    queued = "Queued",
    running = "Running",
    complete = "Complete",
    warning = "Warning",
    error = "Error",
    cancelled = "Cancelled",
    stale = "Stale"
  )
  tags$ol(
    class = "list-group list-group-numbered",
    lapply(seq_len(nrow(phases)), function(index) {
      state <- phases$status[[index]]
      tags$li(
        class = paste(
          "list-group-item d-flex justify-content-between align-items-start",
          if (state %in% c("running", "queued")) "border-primary" else NULL,
          if (identical(state, "error")) "border-danger" else NULL
        ),
        tags$div(
          class = "ms-2 me-auto",
          tags$strong(phases$label[[index]]),
          tags$div(class = "llw-secondary", phases$detail[[index]])
        ),
        tags$span(
          class = "ms-3 text-nowrap",
          states[[state]] %||% state
        )
      )
    })
  )
}

filename_regex_label <- function() {
  span(
    class = "d-inline-flex align-items-center gap-1",
    "Filename regular expression",
    tooltip2(
      bsicons::bs_icon(
        "info-circle",
        title = "Help with filename regular expressions"
      ),
      tags$div(
        tags$p(
          paste(
            "The final file extension is ignored.",
            "If the expression contains parentheses, the first parenthesized",
            "group becomes the participant ID."
          )
        ),
        tags$p(tags$strong("Examples")),
        tags$ul(
          tags$li(
            tags$code(class = "llw-tooltip-code", "^([^_]+)"),
            " gets the first underscore-separated part: ",
            tags$code(class = "llw-tooltip-code", "ID01_visit_1"),
            " becomes ",
            tags$code(class = "llw-tooltip-code", "ID01"),
            "."
          ),
          tags$li(
            tags$code(class = "llw-tooltip-code", "([^_]+)$"),
            " gets the last underscore-separated part: ",
            tags$code(class = "llw-tooltip-code", "ID01_visit_1"),
            " becomes ",
            tags$code(class = "llw-tooltip-code", "1"),
            "."
          ),
          tags$li(
            tags$code(class = "llw-tooltip-code", "^(ID[0-9]+)"),
            " gets an ID at the start, such as ",
            tags$code(class = "llw-tooltip-code", "ID01"),
            "."
          )
        ),
        tags$p(
          class = "mb-0",
          paste(
            "An AI assistant (LLM) can draft an expression from a few example filenames and the IDs you want.",
            "Use made-up examples if filenames are sensitive, and always check the preview."
          )
        )
      ),
      placement = "left",
      options = list(customClass = "llw-tooltip--wide")
    )
  )
}

raw_import_warning_guidance <- function(warning) {
  guidance <- list(
    "Rows are not chronological within every participant." = list(
      meaning = "At least one participant's rows are not ordered from earliest to latest timestamp.",
      action = "Keep the source copy, then sort by participant and timestamp before time-ordered analysis."
    ),
    "Duplicate Id/Datetime pairs are present and were not removed." = list(
      meaning = "Two or more rows share the same participant ID and timestamp; their measured values may still differ.",
      action = "Inspect the rows and resolve them with a documented rule. Remove only exact duplicate rows automatically."
    ),
    "Explicit missing sensor values are present and remain missing." = list(
      meaning = "The files contain blank or missing measurement values at recorded timestamps.",
      action = "Keep them missing during import. Later, exclude, aggregate, or impute them only with a method justified by the analysis."
    ),
    "Implicit gaps are present; no timestamps or exposure values were imputed." = list(
      meaning = "Expected timestamps are absent between recorded observations, so the recording has unobserved intervals.",
      action = "Use the gap information during analysis. Add a regular time grid or impute values later only when the scientific method requires it."
    ),
    "Off-grid observations are present and remain distinct from gaps." = list(
      meaning = "Some timestamps do not fall on the participant's dominant sampling interval.",
      action = "Check device timing and timestamp alignment. Later, retain them or resample with an explicit, documented rule."
    ),
    "Dominant sampling epochs differ between participants." = list(
      meaning = "Participants were recorded most often at different time intervals.",
      action = "Compare metrics that account for interval length, or later resample to a common interval with a justified aggregation rule."
    ),
    "Some participants have too few unique timestamps to estimate an epoch." = list(
      meaning = "There are not enough distinct timestamps to infer a usual sampling interval for every participant.",
      action = "Check whether more files are available. Otherwise, set or document the expected interval before interval-based analysis."
    ),
    "The recording crosses a daylight-saving transition; verify whether the device already adjusted its clock before applying any DST correction." = list(
      meaning = "The selected time zone changes its clock during the recording period.",
      action = "Confirm how the device stored time. Apply a daylight-saving correction only if the device did not already make it."
    )
  )
  guidance[[warning]] %||%
    list(
      meaning = "The full-data review found a condition that may affect interpretation.",
      action = "Open the detailed data review, inspect the affected rows, and document any later correction."
    )
}

raw_import_warning_item_ui <- function(warning) {
  guidance <- raw_import_warning_guidance(warning)
  tags$li(
    tags$span(warning),
    tooltip2(
      bsicons::bs_icon(
        "info-circle",
        title = "Explain this import warning"
      ),
      tags$div(
        tags$p(tags$strong("What this means")),
        tags$p(guidance$meaning),
        tags$p(tags$strong("What you can do later")),
        tags$p(class = "mb-0", guidance$action)
      ),
      placement = "right",
      options = list(customClass = "llw-tooltip--wide")
    )
  )
}

raw_import_diagnostic_guidance <- function(check) {
  guidance <- list(
    "Required identity" = list(
      meaning = "Checks that every time point has a usable participant ID.",
      before = "Correct embedded IDs or the filename-to-ID mapping before import if IDs are missing or wrong.",
      after = "If identity is wrong, correct the source or mapping and import again rather than silently relabelling analyzed data."
    ),
    "Timestamp contract" = list(
      meaning = "Checks that every time point has a valid absolute date and time.",
      before = "Select the correct device format and source time zone, and repair invalid timestamp fields in the source export.",
      after = "Do not continue with time-based analysis until invalid timestamps have been corrected and re-imported."
    ),
    "Names and types" = list(
      meaning = "Checks that column names are unique and that LightLogR returned stable column types.",
      before = "Use one compatible export structure and remove accidental duplicate column names in the source files.",
      after = "Rename or convert variables only as a documented data-preparation step while retaining the raw import."
    ),
    "Ordering" = list(
      meaning = "Checks whether rows run from earlier to later within each participant.",
      before = "If possible, export records in chronological order; import still preserves the received row order.",
      after = "Sort a working copy by participant and timestamp before analyses that depend on sequence."
    ),
    "Duplicate Id/Datetime" = list(
      meaning = "Checks whether multiple rows share one participant ID and timestamp; their measurement values may differ.",
      before = "The import option can remove exact duplicate rows, but it deliberately keeps conflicting rows for review.",
      after = "Inspect affected rows and apply a documented rule for retaining, combining, or excluding them."
    ),
    "Source time zone" = list(
      meaning = "Checks that imported timestamps use the source time zone selected for the logger clock.",
      before = "Choose the time zone used by the logger; do not substitute the computer's current time zone.",
      after = "If the selected zone was wrong, return to import and correct it before deriving local-time results."
    ),
    "Date range" = list(
      meaning = "Shows the earliest and latest imported timestamps in the selected source time zone.",
      before = "Compare the range with deployment records and use the lower date limit only when it reflects the protocol.",
      after = "Investigate unexpected dates and document any later date filtering; the raw import remains unchanged."
    ),
    "Dominant epoch" = list(
      meaning = "Estimates each participant's most common interval between time points.",
      before = "Confirm the logger sampling setting and avoid combining incompatible exports as one dataset.",
      after = "Use interval-aware summaries or resample later with an explicit aggregation rule when epochs differ."
    ),
    "Explicit missing values" = list(
      meaning = "Counts blank measurement values at timestamps that are present in the files.",
      before = "Check whether the original export or another source file contains the missing measurements.",
      after = "Keep them distinct from measured zero; exclude or impute only with a justified analysis method."
    ),
    "Implicit gaps" = list(
      meaning = "Counts expected time points that are absent from a participant's dominant time grid.",
      before = "Check for missing export parts or interrupted recordings before accepting the dataset.",
      after = "Use gap and coverage information in analysis. Creating a complete grid does not itself impute exposure values."
    ),
    "Irregular observations" = list(
      meaning = "Counts timestamps that do not fall on the participant's dominant time grid.",
      before = "Check logger timing, clock changes, and whether files with different sampling settings were combined.",
      after = "Retain them or resample with a documented rule; do not count them as missing time points."
    ),
    "DST transitions" = list(
      meaning = "Checks for daylight-saving and other local-clock discontinuities without changing absolute instants.",
      before = "Confirm whether the logger already adjusted its clock before selecting the DST correction option.",
      after = "Correct clock jumps only when device behavior is known, and preserve the original timestamps for provenance."
    )
  )
  guidance[[check]] %||%
    list(
      meaning = "Explains one aspect of the imported data-quality contract.",
      before = "Review the source files and import settings when this check is unexpected.",
      after = "Keep any correction explicit and documented while retaining the unchanged raw import."
    )
}

raw_import_diagnostic_check_ui <- function(check) {
  tags$span(check)
}

raw_import_diagnostic_help_ui <- function(check) {
  guidance <- raw_import_diagnostic_guidance(check)
  tooltip2(
    bsicons::bs_icon(
      "info-circle",
      title = paste("Explain", check)
    ),
    tags$div(
      tags$p(tags$strong("What this checks")),
      tags$p(guidance$meaning),
      tags$p(tags$strong("Before or during import")),
      tags$p(guidance$before),
      tags$p(tags$strong("After import")),
      tags$p(class = "mb-0", guidance$after)
    ),
    placement = "right",
    options = list(customClass = "llw-tooltip--wide")
  )
}

raw_import_diagnostic_status_ui <- function(status) {
  specs <- list(
    pass = list(label = "Pass", icon = "check-circle-fill", tone = "success"),
    warning = list(
      label = "Warning",
      icon = "exclamation-triangle-fill",
      tone = "warning"
    ),
    information = list(
      label = "Info",
      icon = "info-circle-fill",
      tone = "information"
    ),
    error = list(label = "Error", icon = "x-octagon-fill", tone = "danger")
  )
  spec <- specs[[status]] %||%
    list(
      label = status,
      icon = "question-circle-fill",
      tone = "information"
    )
  tags$span(
    class = paste(
      "llw-diagnostic-status",
      paste0("llw-diagnostic-status--", spec$tone)
    ),
    bsicons::bs_icon(spec$icon),
    tags$span(spec$label)
  )
}

raw_import_diagnostics_table_ui <- function(diagnostics) {
  required <- c("check", "status", "value", "detail")
  if (!is.data.frame(diagnostics) || !all(required %in% names(diagnostics))) {
    abort_llw(
      "`diagnostics` must contain check, status, value, and detail columns.",
      type = "validation"
    )
  }
  tags$table(
    class = "table table-striped align-middle mb-0 llw-diagnostic-table",
    tags$thead(
      tags$tr(
        tags$th(scope = "col", "Check"),
        tags$th(
          scope = "col",
          class = "llw-diagnostic-help",
          tags$span(class = "visually-hidden", "Explanation")
        ),
        tags$th(scope = "col", "Status"),
        tags$th(scope = "col", "Value"),
        tags$th(scope = "col", "Detail")
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(diagnostics)), function(index) {
        tags$tr(
          tags$th(
            scope = "row",
            raw_import_diagnostic_check_ui(diagnostics$check[[index]])
          ),
          tags$td(
            class = "llw-diagnostic-help",
            raw_import_diagnostic_help_ui(diagnostics$check[[index]])
          ),
          tags$td(
            raw_import_diagnostic_status_ui(diagnostics$status[[index]])
          ),
          tags$td(diagnostics$value[[index]]),
          tags$td(diagnostics$detail[[index]])
        )
      })
    )
  )
}

raw_import_eligibility_status_ui <- function(eligible) {
  if (isTRUE(eligible)) {
    return(tags$span(
      class = "llw-diagnostic-status llw-diagnostic-status--success",
      bsicons::bs_icon(
        "check-circle-fill",
        title = "Eligible as the initial analysis focus"
      ),
      tags$span("Eligible")
    ))
  }
  tags$span(
    class = "llw-diagnostic-status llw-diagnostic-status--danger",
    bsicons::bs_icon(
      "x-circle-fill",
      title = "Not eligible as the initial analysis focus"
    ),
    tags$span("Not eligible")
  )
}

raw_import_focus_input_id <- function(index) {
  if (
    !is.numeric(index) ||
      length(index) != 1L ||
      is.na(index) ||
      index < 1 ||
      index != floor(index)
  ) {
    abort_llw("`index` must be one positive whole number.", type = "validation")
  }
  paste0("focus_variable_", as.integer(index))
}

raw_import_focus_selection_ui <- function(
  variable,
  eligible,
  index,
  selected_variable = NULL,
  ns = NULL
) {
  if (!isTRUE(eligible) || is.null(ns)) {
    return(raw_import_eligibility_status_ui(eligible))
  }
  if (!is.function(ns)) {
    abort_llw("`ns` must be a namespace function or NULL.", type = "validation")
  }

  selected <- identical(variable, selected_variable)
  button <- actionButton(
    inputId = ns(raw_import_focus_input_id(index)),
    label = if (selected) "Preselected" else "Select",
    icon = icon(if (selected) "check" else "bullseye"),
    class = paste(
      "btn-sm llw-focus-select",
      if (selected) "btn-success" else "btn-outline-success"
    ),
    title = if (selected) {
      paste(variable, "is preselected as the initial analysis focus")
    } else {
      paste("Preselect", variable, "as the initial analysis focus")
    }
  )
  htmltools::tagAppendAttributes(
    button,
    `aria-label` = if (selected) {
      paste(variable, "is preselected as the initial analysis focus")
    } else {
      paste("Preselect", variable, "as the initial analysis focus")
    },
    `aria-pressed` = if (selected) "true" else "false",
    disabled = if (selected) "disabled" else NULL
  )
}

raw_import_summary_value_ui <- function(value, count = FALSE) {
  if (length(value) == 0L || is.na(value) || !is.finite(value)) {
    return(tags$span(class = "llw-secondary", "\u2014"))
  }
  if (isTRUE(count)) {
    return(format(value, big.mark = ",", scientific = FALSE, trim = TRUE))
  }
  format(
    round(value, digits = 3L),
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

raw_import_variable_review_table_ui <- function(
  review,
  selected_variable = NULL,
  ns = NULL
) {
  required <- c(
    "variable",
    "type",
    "eligible",
    "missing",
    "exact_zero",
    "negative",
    "minimum",
    "q25",
    "median",
    "mean",
    "q75",
    "maximum",
    "reason"
  )
  if (!is.data.frame(review) || !all(required %in% names(review))) {
    abort_llw(
      "`review` must contain eligibility and numeric-summary columns.",
      type = "validation"
    )
  }
  tags$table(
    class = "table table-striped align-middle mb-0 llw-variable-review",
    tags$thead(
      tags$tr(
        tags$th(scope = "col", "Variable"),
        tags$th(scope = "col", "Type"),
        tags$th(scope = "col", "Analysis focus"),
        tags$th(scope = "col", "Missing"),
        tags$th(scope = "col", "Exact zeros"),
        tags$th(scope = "col", "Negative"),
        tags$th(scope = "col", "Minimum"),
        tags$th(scope = "col", "Q1"),
        tags$th(scope = "col", "Median"),
        tags$th(scope = "col", "Mean"),
        tags$th(scope = "col", "Q3"),
        tags$th(scope = "col", "Maximum"),
        tags$th(scope = "col", "Why unavailable")
      )
    ),
    tags$tbody(
      lapply(seq_len(nrow(review)), function(index) {
        eligible <- review$eligible[[index]]
        tags$tr(
          tags$th(scope = "row", review$variable[[index]]),
          tags$td(review$type[[index]]),
          tags$td(raw_import_focus_selection_ui(
            variable = review$variable[[index]],
            eligible = eligible,
            index = index,
            selected_variable = selected_variable,
            ns = ns
          )),
          tags$td(raw_import_summary_value_ui(review$missing[[index]], TRUE)),
          tags$td(raw_import_summary_value_ui(
            review$exact_zero[[index]],
            TRUE
          )),
          tags$td(raw_import_summary_value_ui(review$negative[[index]], TRUE)),
          tags$td(raw_import_summary_value_ui(review$minimum[[index]])),
          tags$td(raw_import_summary_value_ui(review$q25[[index]])),
          tags$td(raw_import_summary_value_ui(review$median[[index]])),
          tags$td(raw_import_summary_value_ui(review$mean[[index]])),
          tags$td(raw_import_summary_value_ui(review$q75[[index]])),
          tags$td(raw_import_summary_value_ui(review$maximum[[index]])),
          tags$td(
            if (isTRUE(eligible)) {
              tags$span(class = "llw-secondary", "\u2014")
            } else {
              review$reason[[index]]
            }
          )
        )
      })
    )
  )
}

UI_accordion_specification <- function(ns) {
  accordion_panel(
    h2(class = "h3", "Import settings"),
    value = "import_specs",
    layout_column_wrap(
      heights_equal = "row",
      fillable = FALSE,
      min_height = "300px",
      tagList(
        h3(class = "h4", "Files and source"),
        fileInput(
          ns("file"),
          span(
            bsicons::bs_icon("1-circle"),
            strong("Choose file(s)")
          ) |>
            tooltip2(
              paste(
                "Choose files from one device and file format, then select",
                "the time zone used by the logger. The app keeps an unchanged",
                "private copy during this session."
              )
            ),
          multiple = TRUE,
          accept = raw_import_accept_extensions(),
          width = "100%"
        ),
        selectizeInput(
          ns("device"),
          span(
            bsicons::bs_icon("2-circle"),
            strong(
              "Select the ",
              a(
                "device",
                href = "https://tscnlab.github.io/LightLogR/reference/import_Dataset.html#devices",
                target = "_blank",
                rel = "noopener noreferrer"
              )
            )
          ),
          choices = c(
            "Choose a device" = "",
            stats::setNames(raw_import_devices(), raw_import_devices())
          ),
          selected = "",
          options = list(placeholder = "Select a device..."),
          width = "100%"
        ),
        selectizeInput(
          ns("tz"),
          span(
            bsicons::bs_icon("3-circle"),
            strong("Select the source time zone")
          ) |>
            tooltip2(
              paste(
                "Choose the time zone used by the logger clock.",
                "The app does not guess this because the wrong time zone changes timestamps."
              )
            ),
          choices = c(
            "Choose a source time zone" = "",
            stats::setNames(OlsonNames(), OlsonNames())
          ),
          selected = "",
          width = "100%"
        ),
        textInput(
          ns("dataset_name"),
          span(
            bsicons::bs_icon("4-circle"),
            strong("Dataset display name")
          ),
          placeholder = "A unique name for this session dataset",
          width = "100%"
        )
      ),
      tagList(
        h3(class = "h4", "File format and import choices"),
        selectizeInput(
          ns("version"),
          span(
            bsicons::bs_icon("5-circle"),
            strong("Device export version")
          ),
          choices = get_versions(""),
          selected = "",
          options = list(placeholder = "Select a device first"),
          width = "100%"
        ) |>
          tooltip2(
            textOutput(ns("version_select")),
            placement = "top",
            options = list(fallbackPlacements = character())
          ),
        dateInput(
          ns("not_before"),
          span(
            bsicons::bs_icon("6-circle"),
            strong("Do not import observations before")
          ),
          weekstart = 1,
          value = "2001-01-01",
          width = "100%"
        ),
        checkboxGroupInput(
          ns("options"),
          span(
            bsicons::bs_icon("7-circle"),
            strong("Optional data changes")
          ),
          choiceNames = list(
            tooltip2(
              "Adjust daylight-saving time (DST) jumps",
              paste(
                "Use this only when the device did not already adjust its clock.",
                "The data-quality report will still show daylight-saving changes."
              )
            ),
            tooltip2(
              "Remove exact duplicate rows",
              paste(
                "This changes the imported data and is recorded in the dataset history.",
                "Rows with the same timestamp but different values are kept and reported."
              )
            )
          ),
          choiceValues = c("dst_jumps", "remove_duplicates")
        ),
        radioButtons(
          ns("id"),
          span(
            bsicons::bs_icon("8-circle"),
            strong("Proposed participant IDs")
          ) |>
            tooltip2(
              "If a file already contains an `Id` column, those values are kept. This choice is used only for files without an `Id` column."
            ),
          choiceNames = c(
            "Use the complete filename stem (1 ID per file)",
            "Use one shared manual ID",
            "Extract with a regular expression"
          ),
          choiceValues = c("automated", "manual", "extract"),
          selected = "automated",
          width = "100%"
        )
      ),
      tagList(
        h3(class = "h4", "Options for this device"),
        p(
          class = "llw-secondary",
          "Extra options appear here only when the selected device needs them."
        ),
        conditionalPanel(
          condition = "input.device == 'VEET'",
          ns = ns,
          selectizeInput(
            inputId = ns("veet_modality"),
            label = "VEET modality",
            choices = c(
              "Ambient light sensor (ALS)" = "ALS",
              "Inertial measurement unit (IMU)" = "IMU",
              "Information (INF)" = "INF",
              "Spectral sensor (PHO)" = "PHO",
              "Time of flight (TOF)" = "TOF"
            ),
            selected = "ALS",
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.id == 'manual'",
          ns = ns,
          textInput(
            inputId = ns("Id_manual"),
            label = "Shared participant ID",
            placeholder = "Participant",
            value = "Participant",
            updateOn = "blur",
            width = "100%"
          )
        ),
        conditionalPanel(
          condition = "input.id == 'extract'",
          ns = ns,
          textInput(
            inputId = ns("Id_extract"),
            label = filename_regex_label(),
            placeholder = "For example: ^(P[0-9]+)_",
            value = "^(.*)$",
            updateOn = "blur",
            width = "100%"
          )
        )
      )
    ),
    layout_column_wrap(
      value_box(
        "Selected files",
        value = textOutput(ns("n_files")),
        showcase = bsicons::bs_icon("journals"),
        theme = "text-secondary",
        class = "llw-value-box",
        textOutput(ns("filenames"))
      ),
      value_box(
        "Proposed participant IDs",
        value = textOutput(ns("n_ids")),
        showcase = bsicons::bs_icon("person-vcard"),
        theme = "text-secondary",
        class = "llw-value-box",
        textOutput(ns("pattern"))
      )
    ),
    card(
      card_header("File and participant ID preview", container = h3),
      uiOutput(ns("mapping_status")),
      tags$div(
        class = "llw-data-region",
        role = "region",
        `aria-label` = "File and participant ID preview",
        tabindex = "0",
        tableOutput(ns("id_mapping"))
      )
    ),
    tags$div(
      class = "llw-action-row llw-import-actions",
      layout_column_wrap(
        width = 1 / 2,
        input_task_button(
          ns("import"),
          span(strong("Check and import")),
          icon = icon("file-import"),
          class = "btn-primary btn-lg"
        ),
        actionButton(
          ns("cancel_import"),
          "Cancel import",
          icon = icon("ban"),
          class = "btn-outline-secondary btn-lg"
        )
      )
    ),
    tags$div(
      class = "mb-3 llw-import-task-status",
      uiOutput(ns("task_status"))
    ),
    card(
      card_header("Import progress", container = h3),
      uiOutput(ns("phase_status"))
    )
  )
}

UI_accordion_summary <- function(ns) {
  accordion_panel(
    h2(class = "h3", "Review imported data"),
    value = "import_summary",
    tags$div(class = "mb-3", uiOutput(ns("quality_callout"))),
    layout_column_wrap(
      value_box(
        "Time points (rows)",
        value = textOutput(ns("quality_rows")),
        showcase = icon("table-list"),
        theme = "text-secondary",
        class = "llw-value-box"
      ),
      value_box(
        "Participants",
        value = textOutput(ns("quality_participants")),
        showcase = icon("people-group"),
        theme = "text-secondary",
        class = "llw-value-box"
      ),
      value_box(
        "Missing time points (missing rows)",
        value = textOutput(ns("quality_gaps")),
        showcase = icon("timeline"),
        theme = "text-secondary",
        class = "llw-value-box"
      ),
      value_box(
        "Numeric variables",
        value = textOutput(ns("quality_variables")),
        showcase = icon("hashtag"),
        theme = "text-secondary",
        class = "llw-value-box"
      )
    ),
    layout_column_wrap(
      card(
        card_header("Recording period by participant", container = h3),
        plotOutput(ns("plot_overview"), height = "460px") |>
          shinycssloaders::withSpinner()
      ),
      card(
        card_header("LightLogR import details", container = h3),
        verbatimTextOutput(ns("import_msg")) |>
          shinycssloaders::withSpinner()
      )
    ),
    accordion(
      id = ns("quality_details_accordion"),
      open = FALSE,
      multiple = FALSE,
      accordion_panel(
        h3(class = "h4", "Data quality checks"),
        value = "quality_checks",
        tags$p(
          class = "llw-secondary",
          paste(
            "Missing values, missing time points, observations outside the expected",
            "time grid, duplicate timestamps, and daylight-saving changes are",
            "reported separately. The app does not fill in missing data."
          )
        ),
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Data quality checks for the imported files",
          tabindex = "0",
          uiOutput(ns("quality_diagnostics"))
        )
      ),
      accordion_panel(
        h3(class = "h4", "Numeric variables and analysis focus"),
        value = "numeric_variables",
        tags$p(
          class = "llw-secondary",
          paste(
            "All imported columns are listed below. A green check marks a numeric",
            "variable that can be the initial focus for plots and analysis; a red cross",
            "marks a column that cannot. You can change the focus later, and the choice",
            "does not alter the imported data. Eligibility does not confirm quantity,",
            "units, calibration, sensor position, or comparability."
          )
        ),
        tags$p(
          class = "llw-secondary",
          paste(
            "Numeric summaries show missing values, exact zeros, negative values, Q1,",
            "median, mean, Q3, and the measured range. Ineligible columns show why they",
            "cannot be selected."
          )
        ),
        tags$div(
          class = "llw-data-region llw-variable-review-table",
          role = "region",
          `aria-label` = "Numeric variables and analysis-focus eligibility",
          tabindex = "0",
          uiOutput(ns("signal_profile"))
        )
      ),
      accordion_panel(
        h3(class = "h4", "Data quality by participant"),
        value = "participant_quality",
        tags$div(
          class = "llw-data-region llw-participant-quality-table",
          role = "region",
          `aria-label` = "Data quality checks by participant",
          tabindex = "0",
          tableOutput(ns("participant_quality"))
        )
      ),
      accordion_panel(
        h3(class = "h4", "Preview of imported rows"),
        value = "row_preview",
        tags$p(class = "llw-secondary", textOutput(ns("preview_notice"))),
        gt::gt_output(ns("import_table")) |>
          shinycssloaders::withSpinner()
      )
    ),
    tags$div(
      class = "d-grid col-12 col-md-6 mx-auto mt-3",
      input_task_button(
        ns("add_dataset"),
        span(strong("Choose focus variable and add dataset")),
        icon = icon("database"),
        width = "100%",
        class = "btn-primary btn-lg"
      )
    )
  )
}

importUI <- function(id) {
  ns <- NS(id)
  accordion(
    multiple = TRUE,
    id = ns("import_accordion"),
    UI_accordion_specification(ns),
    UI_accordion_summary(ns)
  )
}

importServer <- function(id, runtime, color_mode) {
  if (
    !is.list(runtime) ||
      !is.function(runtime$submit) ||
      !is.function(runtime$stage_uploads)
  ) {
    abort_llw(
      "`runtime` must be created by `new_session_runtime()`.",
      type = "validation"
    )
  }
  if (!shiny::is.reactive(color_mode)) {
    abort_llw("`color_mode` must be reactive.", type = "validation")
  }

  moduleServer(id, function(input, output, session) {
    observe({
      device <- input$device %||% ""
      choices <- tryCatch(
        get_versions(device),
        error = function(cnd) get_versions("")
      )
      updateSelectizeInput(
        session,
        inputId = "version",
        choices = choices,
        selected = if (nzchar(device)) "default" else "",
        server = TRUE
      )
    }) |>
      bindEvent(input$device, ignoreInit = FALSE)

    output$version_select <- renderText({
      get_version_description(input$device %||% "", input$version %||% "")
    })

    observe({
      req(nzchar(input$device %||% ""))
      updateTextInput(
        session,
        "dataset_name",
        value = create_dataset_name(input$device)
      )
    }) |>
      bindEvent(input$device, ignoreInit = TRUE)

    mapping_result <- reactive({
      if (is.null(input$file) || nrow(input$file) == 0L) {
        return(NULL)
      }
      tryCatch(
        new_filename_id_mapping(
          original_names = input$file$name,
          id_mode = input$id %||% "automated",
          manual_id = input$Id_manual,
          extract_pattern = input$Id_extract
        ),
        error = identity
      )
    })

    output$n_files <- renderText({
      if (is.null(input$file)) return("0")
      format(nrow(input$file), big.mark = ",")
    })
    output$filenames <- renderText({
      if (is.null(input$file)) return("No files selected")
      paste(basename(input$file$name), collapse = ",\n")
    })
    output$n_ids <- renderText({
      mapping <- mapping_result()
      if (is.null(mapping) || inherits(mapping, "error")) return("0")
      format(length(unique(mapping$proposed_id)), big.mark = ",")
    })
    output$pattern <- renderText({
      mapping <- mapping_result()
      if (is.null(mapping)) return("No mapping yet")
      if (inherits(mapping, "error")) return("Mapping needs attention")
      paste(unique(mapping$proposed_id), collapse = ",\n")
    })
    output$mapping_status <- renderUI({
      mapping <- mapping_result()
      if (is.null(mapping)) {
        return(llw_status_callout(
          "idle",
          "Choose files to see the proposed participant ID for each file.",
          heading = "File and ID preview not ready",
          compact = TRUE
        ))
      }
      if (inherits(mapping, "error")) {
        mapped <- normalize_task_error(mapping, "raw_import")
        return(llw_status_callout(
          "error",
          llw_public_message(mapped),
          heading = "File and ID preview needs attention",
          compact = TRUE
        ))
      }
      message <- if (any(mapping$duplicate_proposed)) {
        paste(
          "Several files have the same filename fallback ID.",
          "This is allowed when the files together form one participant record.",
          "LightLogR creates the final IDs during import; embedded Id values are kept,",
          "and overlapping times are reported after import."
        )
      } else {
        paste(
          "LightLogR creates the final participant IDs during import.",
          "This preview shows the filename fallback used only when a file has no Id column."
        )
      }
      llw_status_callout(
        "complete",
        message,
        heading = "File and ID preview ready",
        compact = TRUE
      )
    })
    output$id_mapping <- renderTable(
      {
        mapping <- mapping_result()
        req(mapping, !inherits(mapping, "error"))
        data.frame(
          File = basename(mapping$original_name),
          `Filename fallback ID (when the file has no Id column)` = mapping$proposed_id,
          Method = mapping$mapping_source,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )

    import_task <- new_long_task(
      worker = raw_import_worker,
      task_type = "raw_import",
      runtime = runtime
    )
    bslib::bind_task_button(import_task$extended_task, "import")
    last_request <- reactiveVal(NULL)
    last_dataset_name <- reactiveVal(NULL)
    focus_selection <- reactiveVal(NULL)
    focus_click_state <- reactiveVal(integer())

    observe({
      attempted <- tryCatch(
        {
          mapping <- mapping_result()
          if (
            is.null(input$file) ||
              !nzchar(input$device %||% "") ||
              !nzchar(input$tz %||% "") ||
              !nzchar(trimws(input$dataset_name %||% "")) ||
              !nzchar(input$id %||% "")
          ) {
            abort_llw(
              "Files, device, source time zone, dataset name, and participant-ID mode are required.",
              type = "validation",
              public_message = paste(
                "Choose files, a device, a source time zone, a dataset name,",
                "and an ID mode before starting the import."
              )
            )
          }
          if (inherits(mapping, "error")) stop(mapping)
          staged_files <- runtime$stage_uploads(input$file)
          request <- new_raw_import_request(
            device = input$device,
            staged_files = staged_files,
            timezone = input$tz,
            not_before = input$not_before,
            options = input$options %||% character(),
            version = input$version,
            id_mode = input$id,
            manual_id = input$Id_manual,
            extract_pattern = input$Id_extract,
            veet_modality = input$veet_modality,
            max_bytes = runtime$profile$max_upload_bytes,
            progress_path = file.path(
              runtime$paths$tasks,
              paste0(new_stable_id("raw_import_progress"), ".rds")
            )
          )
          task_id <- import_task$invoke(request)
          if (!isFALSE(task_id)) {
            last_request(request)
            last_dataset_name(trimws(input$dataset_name))
          }
          task_id
        },
        error = identity
      )
      if (inherits(attempted, "error")) {
        import_task$reject(attempted)
      }
    }) |>
      bindEvent(input$import, ignoreInit = TRUE)

    observe(import_task$cancel()) |>
      bindEvent(input$cancel_import, ignoreInit = TRUE)

    import_result <- reactive({
      req(import_task$state() %in% c("complete", "warning"))
      result <- import_task$result()
      req(inherits(result, "llw_raw_import_result"))
      result
    })

    observe({
      result <- import_result()
      eligible <- result$eligibility$variable[result$eligibility$eligible]
      req(length(eligible) > 0L)
      selected <- if ("MEDI" %in% eligible) "MEDI" else eligible[[1L]]
      focus_selection(selected)
      focus_click_state(integer())
    }) |>
      bindEvent(import_task$state(), ignoreInit = TRUE)

    output$task_status <- renderUI({
      llw_task_status(import_task$status(), context = "Import")
    })
    output$phase_status <- renderUI({
      state <- import_task$state()
      if (state %in% c("queued", "running", "finalizing")) {
        invalidateLater(250, session)
      }
      result <- if (import_task$state() %in% c("complete", "warning")) {
        import_task$result()
      } else {
        NULL
      }
      request <- last_request()
      progress <- if (is.null(request)) {
        NULL
      } else {
        read_raw_import_progress(request$progress_path)
      }
      phases <- raw_import_phase_snapshot(
        import_task$status(),
        result = result,
        error = import_task$error(),
        request_ready = !is.null(request),
        progress = progress
      )
      raw_import_phase_ui(phases)
    })

    observe({
      req(identical(import_task$state(), "error"))
      showNotification(
        llw_public_message(import_task$error()),
        type = "error",
        duration = 8
      )
    }) |>
      bindEvent(import_task$state(), ignoreInit = TRUE)

    observe({
      req(import_task$state() %in% c("complete", "warning"))
      accordion_panel_set("import_accordion", "import_summary")
      show_import_success_modal(import_result()$data)
    }) |>
      bindEvent(import_task$state(), ignoreInit = TRUE)

    output$quality_callout <- renderUI({
      result <- import_result()
      if (length(result$warnings) == 0L) {
        return(llw_status_callout(
          "complete",
          paste(
            "The file structure passed all checks. The app kept an unchanged",
            "private copy of each source file and checked the complete imported dataset."
          ),
          heading = "Ready to review"
        ))
      }
      llw_status_callout(
        "warning",
        tags$ul(lapply(result$warnings, raw_import_warning_item_ui)),
        heading = "Import finished\u2014note these points"
      )
    })
    output$quality_rows <- renderText({
      format(import_result()$quality$summary$rows, big.mark = ",")
    })
    output$quality_participants <- renderText({
      format(import_result()$quality$summary$participants, big.mark = ",")
    })
    output$quality_gaps <- renderText({
      format(
        import_result()$quality$summary$implicit_gap_epochs,
        big.mark = ",",
        scientific = FALSE
      )
    })
    output$quality_variables <- renderText({
      format(import_result()$quality$summary$eligible_variables, big.mark = ",")
    })

    output$plot_overview <- renderPlot({
      mode <- color_mode() %||% "light"
      if (length(mode) != 1L || is.na(mode) || !mode %in% c("light", "dark")) {
        mode <- "light"
      }
      result <- import_result()
      plot_raw_import_overview(result$data, result$quality, mode = mode)
    })
    output$import_msg <- renderText({
      message <- import_result()$msg
      if (length(message) == 0L) {
        return("LightLogR completed without a console summary message.")
      }
      paste(message, collapse = "\n")
    })
    output$quality_diagnostics <- renderUI({
      raw_import_diagnostics_table_ui(
        import_result()$quality$diagnostics
      )
    })
    output$signal_profile <- renderUI({
      quality <- import_result()$quality
      review <- raw_import_variable_review(
        quality$eligibility,
        quality$signal_profile
      )
      raw_import_variable_review_table_ui(
        review,
        selected_variable = focus_selection(),
        ns = session$ns
      )
    })

    observe({
      quality <- import_result()$quality
      review <- raw_import_variable_review(
        quality$eligibility,
        quality$signal_profile
      )
      input_ids <- vapply(
        seq_len(nrow(review)),
        raw_import_focus_input_id,
        character(1)
      )
      clicks <- stats::setNames(
        vapply(
          input_ids,
          function(input_id) as.integer(input[[input_id]] %||% 0L),
          integer(1)
        ),
        input_ids
      )
      previous <- isolate(focus_click_state())
      if (length(previous) == 0L) {
        focus_click_state(clicks)
        return()
      }
      previous <- previous[input_ids]
      previous[is.na(previous)] <- 0L
      changed <- which(clicks > previous)
      focus_click_state(clicks)
      if (length(changed) == 0L) {
        return()
      }
      index <- changed[[length(changed)]]
      if (isTRUE(review$eligible[[index]])) {
        focus_selection(review$variable[[index]])
      }
    })
    output$participant_quality <- renderTable(
      {
        participants <- import_result()$quality$participants
        participants$start <- format(
          participants$start,
          tz = import_result()$quality$summary$source_timezone,
          usetz = TRUE
        )
        participants$end <- format(
          participants$end,
          tz = import_result()$quality$summary$source_timezone,
          usetz = TRUE
        )
        participants$ordered <- ifelse(participants$ordered, "Yes", "No")
        names(participants) <- c(
          "Participant ID",
          "Time points",
          "Start",
          "End",
          "Epoch (s)",
          "Duplicate times",
          "Ordered",
          "Missing values",
          "Missing time points",
          "Gap episodes",
          "Off-grid time points",
          "DST changes",
          "Local-clock changes"
        )
        participants
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )
    output$preview_notice <- renderText(import_result()$preview$notice)
    output$import_table <- gt::render_gt({
      result <- import_result()
      preview <- raw_import_preview_data(
        result$data,
        result$preview$indices
      )
      gt::gt(preview) |>
        gt::opt_interactive(
          use_search = TRUE,
          use_filters = TRUE,
          use_resizers = TRUE,
          use_highlight = TRUE
        )
    })

    observe({
      result <- import_result()
      req(last_dataset_name(), last_request())
      eligible <- result$eligibility[
        result$eligibility$eligible,
        ,
        drop = FALSE
      ]
      if (nrow(eligible) == 0L) {
        showNotification(
          "No numeric variable can be selected. Review the import settings or source file.",
          type = "error",
          duration = NULL
        )
        return()
      }
      selected <- focus_selection()
      if (
        length(selected) != 1L ||
          is.na(selected) ||
          !selected %in% eligible$variable
      ) {
        selected <- if ("MEDI" %in% eligible$variable) "MEDI" else
          eligible$variable[[1L]]
      }
      showModal(modalDialog(
        title = "Choose the initial analysis focus",
        easyClose = TRUE,
        footer = modalButton("Keep reviewing"),
        selectizeInput(
          session$ns("variable"),
          "Focus variable",
          choices = stats::setNames(eligible$variable, eligible$variable),
          selected = selected,
          width = "100%"
        ),
        p(
          paste(
            "You can change this focus later. The imported columns are not changed.",
            "Before analysis, confirm the variable's quantity, units, device",
            "calibration, sensor position, and meaning."
          )
        ),
        actionButton(
          session$ns("add_variable"),
          "Use as analysis focus and add dataset",
          class = "btn-primary btn-lg",
          icon = icon("check"),
          width = "100%"
        )
      ))
    }) |>
      bindEvent(input$add_dataset, ignoreInit = TRUE)

    add_dataset <- reactive({
      result <- import_result()
      request <- last_request()
      eligible <- result$eligibility$variable[result$eligibility$eligible]
      req(length(eligible) > 0L)
      variable <- input$variable
      if (
        length(variable) != 1L || is.na(variable) || !variable %in% eligible
      ) {
        variable <- focus_selection()
      }
      if (
        length(variable) != 1L || is.na(variable) || !variable %in% eligible
      ) {
        variable <- if ("MEDI" %in% eligible) "MEDI" else eligible[[1L]]
      }
      list(
        name = last_dataset_name(),
        data = result$data,
        device = request$import_arguments$device,
        version = request$import_arguments$version,
        tz = request$import_arguments$tz,
        variable = variable,
        source_files = request$source_files,
        import_arguments = raw_import_manifest_arguments(request),
        quality = result$quality,
        eligibility = result$eligibility,
        preflight = result$preflight
      )
    }) |>
      bindEvent(input$add_variable, ignoreInit = TRUE)

    observe(removeModal()) |>
      bindEvent(input$add_variable, ignoreInit = TRUE)

    list(
      add_dataset = add_dataset,
      focus_variable = reactive(focus_selection()),
      mapping = reactive(mapping_result()),
      result = reactive(import_task$result()),
      status = import_task$status,
      error = import_task$error,
      cancel = import_task$cancel
    )
  })
}

import_app <- function(max_upload_mb = 200, workers = 1, ...) {
  runtime_profile <- resolve_runtime_profile(
    "local",
    max_upload_mb = max_upload_mb,
    workers = workers
  )
  ui <- lightlogweb_page(page_fluid(
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell",
      tabindex = "-1",
      llw_view_header(
        "Test page",
        "Test raw data import",
        paste(
          "Use this page to test file upload, participant IDs, data checks,",
          "error recovery, and the dataset returned after import."
        )
      ),
      card(
        card_header("Built-in test datasets", container = h2),
        tags$p(
          class = "llw-secondary",
          paste(
            "These buttons test two datasets without uploading a file.",
            "The IZTECH data are for development only and are provided under CC BY 4.0."
          )
        ),
        layout_column_wrap(
          card(
            card_header("Small LightLogR example", container = h3),
            p(
              "LightLogR sample data with device, study site, measurement, and unit information."
            ),
            actionButton(
              "load_immediate_sample",
              "Load LightLogR sample",
              icon = icon("bolt"),
              class = "btn-outline-primary",
              width = "100%"
            )
          ),
          card(
            card_header("Large IZTECH example", container = h3),
            p(
              "151,200 one-minute measurements from 17 anonymous participants. Stored in the project for offline testing."
            ),
            actionButton(
              "load_large_sample",
              "Load IZTECH snapshot",
              icon = icon("database"),
              class = "btn-outline-primary",
              width = "100%"
            )
          )
        ),
        uiOutput("fixture_status"),
        tableOutput("fixture_summary")
      ),
      importUI("import"),
      card(
        card_header("Imported dataset result", container = h2),
        verbatimTextOutput("returned")
      )
    ),
    theme = lightlogweb_theme()
  ))
  server <- function(input, output, session) {
    runtime <- new_session_runtime(runtime_profile, session = session)
    if (inherits(runtime$startup_error, "llw_error")) {
      showNotification(
        llw_public_message(runtime$startup_error),
        type = "warning",
        duration = 8
      )
    }
    imported <- importServer(
      "import",
      runtime = runtime,
      color_mode = reactive("light")
    )
    fixture_record <- reactiveVal(NULL)
    fixture_error <- reactiveVal(NULL)
    load_fixture <- function(builder) {
      fixture_error(NULL)
      record <- tryCatch(builder(), error = identity)
      if (inherits(record, "error")) {
        fixture_record(NULL)
        fixture_error(normalize_task_error(record, "raw_import"))
        return(invisible(FALSE))
      }
      fixture_record(record)
      invisible(TRUE)
    }
    observe(load_fixture(sample_dataset_record)) |>
      bindEvent(input$load_immediate_sample, ignoreInit = TRUE)
    observe(load_fixture(melidos_iztech_dataset_record)) |>
      bindEvent(input$load_large_sample, ignoreInit = TRUE)
    output$fixture_status <- renderUI({
      error <- fixture_error()
      if (!is.null(error)) {
        return(llw_status_callout(
          "error",
          llw_public_message(error),
          heading = "Test dataset unavailable",
          compact = TRUE
        ))
      }
      record <- fixture_record()
      if (is.null(record)) {
        return(llw_status_callout(
          "idle",
          "Choose either built-in dataset to check that its data and source information load correctly.",
          heading = "No test dataset loaded",
          compact = TRUE
        ))
      }
      llw_status_callout(
        "complete",
        "The test dataset loaded successfully and passed all data checks.",
        heading = paste(record$display_name, "is ready"),
        compact = TRUE
      )
    })
    output$fixture_summary <- renderTable(
      {
        record <- fixture_record()
        req(record)
        quality <- record$provenance$raw_import_quality
        data.frame(
          Measure = c(
            "Rows",
            "Participants",
            "Source time zone",
            "Primary variable",
            "Raw checksum"
          ),
          Value = c(
            format(quality$summary$rows, big.mark = ","),
            format(quality$summary$participants, big.mark = ","),
            quality$summary$source_timezone,
            record$analysis_settings$primary_variable,
            record$raw_checksum
          ),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )
    output$returned <- renderPrint({
      value <- imported$add_dataset()
      if (is.null(value)) {
        return("No dataset has been added yet.")
      }
      list(
        display_name = value$name,
        rows = nrow(value$data),
        source_files = value$source_files$original_name,
        primary_variable = value$variable,
        quality_warnings = value$quality$warnings
      )
    })
  }
  shinyApp(
    ui,
    server,
    onStart = shiny_upload_limit_on_start(runtime_profile),
    ...
  )
}
