import_wizard_steps <- function() {
  data.frame(
    value = c("source", "details", "check", "review"),
    label = c("Source", "Details & IDs", "Check & import", "Review"),
    icon = c(
      "files",
      "sliders",
      "file-earmark-arrow-down",
      "bar-chart"
    ),
    stringsAsFactors = FALSE
  )
}

import_wizard_stepper <- function(
  ns,
  active,
  completed = character(),
  review_available = FALSE
) {
  steps <- import_wizard_steps()
  active_index <- match(active, steps$value)
  if (is.na(active_index)) {
    abort_llw("Unknown import-wizard step.", type = "validation")
  }
  unknown_completed <- setdiff(completed, steps$value)
  if (length(unknown_completed) > 0L) {
    abort_llw("Unknown completed import-wizard step.", type = "validation")
  }

  tags$ol(
    class = "llw-import-stepper",
    `aria-label` = "Import steps",
    lapply(seq_len(nrow(steps)), function(index) {
      value <- steps$value[[index]]
      state <- if (index == active_index) {
        "active"
      } else if (value %in% completed) {
        "complete"
      } else {
        "upcoming"
      }
      available <- !identical(value, "review") ||
        isTRUE(review_available)
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
        title = if (!available) {
          "Available after an import has completed"
        } else {
          paste("Open step", index, steps$label[[index]])
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

import_wizard_step_heading <- function(icon_name, eyebrow, title, description) {
  tags$div(
    class = "llw-wizard-step-heading",
    tags$span(
      class = "llw-wizard-step-heading__icon",
      `aria-hidden` = "true",
      bsicons::bs_icon(icon_name)
    ),
    tags$div(
      tags$p(class = "llw-wizard-eyebrow", eyebrow),
      tags$h3(class = "h2", title),
      tags$p(class = "llw-secondary", description)
    )
  )
}

import_wizard_metric <- function(icon_name, label, value, detail = NULL) {
  tags$div(
    class = "llw-wizard-metric",
    tags$span(
      class = "llw-wizard-metric__icon",
      `aria-hidden` = "true",
      bsicons::bs_icon(icon_name)
    ),
    tags$div(
      tags$span(class = "llw-wizard-metric__label", label),
      tags$strong(class = "llw-wizard-metric__value", value),
      if (!is.null(detail)) {
        tags$div(class = "llw-wizard-metric__detail", detail)
      }
    )
  )
}

import_wizard_navigation <- function(..., split = FALSE) {
  tags$div(
    class = paste(
      "llw-wizard-navigation",
      if (isTRUE(split)) "llw-wizard-navigation--split" else ""
    ),
    ...
  )
}

import_wizard_cancel_button <- function(ns, enabled = FALSE) {
  button <- actionButton(
    ns("cancel_import"),
    "Cancel import",
    icon = icon("ban"),
    class = "btn-outline-secondary btn-lg",
    title = if (isTRUE(enabled)) {
      "Cancel the import currently in progress"
    } else {
      "Available while an import is in progress"
    }
  )
  if (!isTRUE(enabled)) {
    button <- htmltools::tagAppendAttributes(
      button,
      disabled = "disabled",
      `aria-disabled` = "true"
    )
  }
  button
}

import_wizard_check_summary_ui <- function(
  file_count,
  device,
  timezone,
  id_count,
  ready = TRUE
) {
  tags$div(
    class = "llw-wizard-check-summary",
    llw_status_callout(
      if (isTRUE(ready)) "complete" else "warning",
      if (isTRUE(ready)) {
        paste(
          "The required choices and proposed participant IDs are ready.",
          "Starting the import will run the full file and data-quality checks."
        )
      } else {
        paste(
          "Some required choices or proposed participant IDs still need attention.",
          "Return to the earlier steps before starting the import."
        )
      },
      heading = if (isTRUE(ready)) {
        "Setup ready to import"
      } else {
        "Setup needs attention"
      },
      compact = TRUE
    ),
    tags$div(
      class = "llw-wizard-check-summary__metrics",
      import_wizard_metric("files", "Files", file_count),
      import_wizard_metric("cpu", "Device", device),
      import_wizard_metric("globe2", "Source time zone", timezone),
      import_wizard_metric("person-vcard", "Participant IDs", id_count)
    )
  )
}

import_wizard_source_ui <- function(ns) {
  nav_panel(
    "Source",
    value = "source",
    tags$div(
      class = "llw-wizard-step-content",
      import_wizard_step_heading(
        "file-earmark-arrow-up",
        "Choose what to import",
        "Select files and identify their device format",
        paste(
          "Start with exports from one device and one compatible file format.",
          "Files remain unchanged in a private session copy."
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        fillable = FALSE,
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(
            bsicons::bs_icon("files"),
            " Source files"
          ),
          fileInput(
            ns("file"),
            strong("Choose file(s)"),
            multiple = TRUE,
            accept = raw_import_accept_extensions(),
            width = "100%"
          ),
          import_wizard_metric(
            "journals",
            "Selected files",
            textOutput(ns("n_files"), inline = TRUE),
            textOutput(ns("filenames"))
          )
        ),
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(
            bsicons::bs_icon("cpu"),
            " Device format"
          ),
          selectizeInput(
            ns("device"),
            strong(
              "Device ",
              a(
                "format help",
                href = "https://tscnlab.github.io/LightLogR/reference/import_Dataset.html#devices",
                target = "_blank",
                rel = "noopener noreferrer"
              )
            ),
            choices = raw_import_device_choices(),
            selected = "",
            options = list(placeholder = "Select a device..."),
            width = "100%"
          ),
          selectizeInput(
            ns("version"),
            strong("Device export version"),
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
          )
        )
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_next_details"),
          "Continue to details & IDs",
          icon = icon("arrow-right"),
          class = "btn-primary btn-lg"
        )
      )
    )
  )
}

import_wizard_details_ui <- function(ns) {
  nav_panel(
    "Details and IDs",
    value = "details",
    tags$div(
      class = "llw-wizard-step-content",
      import_wizard_step_heading(
        "sliders",
        "Describe the recording",
        "Set time, dataset, and participant-ID details",
        paste(
          "These choices make timestamps and participant mapping explicit.",
          "Optional data changes remain visible and recorded."
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        fillable = FALSE,
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(
            bsicons::bs_icon("clock-history"),
            " Time and dataset"
          ),
          selectizeInput(
            ns("tz"),
            strong("Source time zone"),
            choices = c(
              "Choose a source time zone" = "",
              stats::setNames(OlsonNames(), OlsonNames())
            ),
            selected = "",
            width = "100%"
          ),
          textInput(
            ns("dataset_name"),
            strong("Dataset display name"),
            placeholder = "A unique name for this session dataset",
            width = "100%"
          ),
          dateInput(
            ns("not_before"),
            strong("Do not import observations before"),
            weekstart = 1,
            value = "2001-01-01",
            width = "100%"
          ),
          checkboxGroupInput(
            ns("options"),
            strong("Optional data changes"),
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
          )
        ),
        tags$section(
          class = "llw-wizard-field-group",
          tags$h4(
            bsicons::bs_icon("person-vcard"),
            " Participant IDs"
          ),
          tags$p(
            class = "llw-secondary",
            "LightLogR keeps participant IDs from an existing ",
            tags$code("Id"),
            paste(
              " column. For files without one, choose how the ID should be",
              "created from the filename."
            )
          ),
          radioButtons(
            ns("id"),
            strong("How should IDs be created when a file has no Id column?"),
            choiceNames = c(
              "Use the complete filename stem (1 ID per file)",
              "Use one shared manual ID",
              "Extract with a regular expression"
            ),
            choiceValues = c("automated", "manual", "extract"),
            selected = "automated",
            width = "100%"
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
          ),
          import_wizard_metric(
            "person-vcard",
            "Proposed participant IDs",
            textOutput(ns("n_ids"), inline = TRUE),
            textOutput(ns("pattern"))
          )
        )
      ),
      accordion(
        id = ns("wizard_id_preview"),
        open = FALSE,
        multiple = FALSE,
        class = "llw-wizard-id-preview",
        accordion_panel(
          tags$span(
            bsicons::bs_icon("diagram-3"),
            " Optional file and participant ID preview"
          ),
          value = "id_preview",
          tags$div(
            class = "llw-wizard-id-preview-status",
            uiOutput(ns("mapping_status"))
          ),
          tags$div(
            class = "llw-data-region",
            role = "region",
            `aria-label` = "File and participant ID preview",
            tabindex = "0",
            tableOutput(ns("id_mapping"))
          )
        )
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_back_source"),
          "Back to source",
          icon = icon("arrow-left"),
          class = "btn-outline-secondary btn-lg"
        ),
        actionButton(
          ns("wizard_next_check"),
          "Continue to check & import",
          icon = icon("arrow-right"),
          class = "btn-primary btn-lg"
        ),
        split = TRUE
      )
    )
  )
}

import_wizard_check_ui <- function(ns) {
  nav_panel(
    "Check and import",
    value = "check",
    tags$div(
      class = "llw-wizard-step-content",
      import_wizard_step_heading(
        "file-earmark-arrow-down",
        "Check and read the data",
        "Confirm the setup and start the import",
        paste(
          "Review the readiness summary, then run the LightLogR import.",
          "Progress and the final import status stay visible on this page."
        )
      ),
      uiOutput(ns("wizard_check_summary")),
      tags$div(
        class = "llw-action-row llw-import-actions llw-wizard-import-actions",
        input_task_button(
          ns("import"),
          span(strong("Start import")),
          icon = icon("file-import"),
          class = "btn-primary btn-lg llw-wizard-import-primary"
        ),
        uiOutput(ns("cancel_import_button"), inline = TRUE)
      ),
      card(
        class = "llw-wizard-import-progress",
        card_header(
          tags$span(
            bsicons::bs_icon("list-check"),
            " Import progress"
          ),
          container = h4
        ),
        uiOutput(ns("phase_status"))
      ),
      tags$div(
        class = "llw-import-task-status llw-wizard-import-status",
        uiOutput(ns("task_status"))
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_back_details"),
          "Back to details & IDs",
          icon = icon("arrow-left"),
          class = "btn-outline-secondary btn-lg"
        ),
        actionButton(
          ns("wizard_next_review"),
          "Open imported-data review",
          icon = icon("chart-column"),
          class = "btn-outline-primary btn-lg"
        ),
        split = TRUE
      )
    )
  )
}

import_wizard_review_ui <- function(ns) {
  nav_panel(
    "Review",
    value = "review",
    tags$div(
      class = "llw-wizard-step-content llw-wizard-review-content",
      import_wizard_step_heading(
        "bar-chart",
        "Inspect the result",
        "Review imported data before adding the dataset",
        paste(
          "Review the quality checks, numeric summaries, participant details,",
          "and imported rows, then choose the initial analysis focus."
        )
      ),
      import_wizard_navigation(
        actionButton(
          ns("wizard_back_check"),
          "Back to check & import",
          icon = icon("arrow-left"),
          class = "btn-outline-secondary"
        ),
        split = TRUE
      ),
      UI_import_summary_content(ns)
    )
  )
}

importWizardUI <- function(id) {
  ns <- NS(id)
  tags$section(
    class = "llw-import-wizard",
    `aria-labelledby` = ns("wizard_title"),
    tags$header(
      class = "llw-import-wizard__hero",
      tags$span(
        class = "llw-import-wizard__hero-icon",
        `aria-hidden` = "true",
        bsicons::bs_icon("magic")
      ),
      tags$div(
        tags$p(class = "llw-wizard-eyebrow", "Guided data import"),
        tags$h2(id = ns("wizard_title"), "Import wearable data"),
        tags$p(
          paste(
            "Import wearable data in four guided steps.",
            "LightLogR reads the files, and LightLogWeb keeps the setup,",
            "data checks, and imported result together for review."
          )
        )
      ),
      tags$span(
        class = "llw-import-wizard__engine-note",
        bsicons::bs_icon("shield-check"),
        " Powered by LightLogR"
      )
    ),
    uiOutput(ns("wizard_stepper")),
    navset_hidden(
      id = ns("wizard_steps"),
      selected = "source",
      import_wizard_source_ui(ns),
      import_wizard_details_ui(ns),
      import_wizard_check_ui(ns),
      import_wizard_review_ui(ns)
    )
  )
}

import_wizard_app <- function(max_upload_mb = 200, workers = 1, ...) {
  import_app(
    max_upload_mb = max_upload_mb,
    workers = workers,
    presentation = "wizard",
    ...
  )
}
