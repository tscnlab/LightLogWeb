datasetDashboardUI <- function(id) {
  ns <- NS(id)

  tagList(
    tags$header(
      class = "llw-view-header",
      tags$p(class = "llw-eyebrow", "Selected session dataset"),
      uiOutput(ns("dataset_name")),
      tags$p(
        class = "llw-view-lede",
        paste(
          "Inspect provenance, stable identity, revisions, and the unchanged",
          "canonical source before building analysis steps."
        )
      )
    ),
    layout_column_wrap(
      fill = FALSE,
      value_box(
        title = "Stable dataset ID",
        value = textOutput(ns("dataset_id")),
        showcase = icon("fingerprint"),
        theme = "text-secondary",
        class = "llw-value-box"
      ),
      value_box(
        title = "Revision",
        value = textOutput(ns("revision")),
        showcase = icon("code-branch"),
        theme = "text-primary",
        class = "llw-value-box"
      ),
      value_box(
        title = "Canonical raw rows",
        value = textOutput(ns("raw_rows")),
        showcase = icon("database"),
        theme = "text-secondary",
        class = "llw-value-box"
      ),
      value_box(
        title = "Prepared rows",
        value = textOutput(ns("prepared_rows")),
        showcase = icon("table"),
        theme = "text-secondary",
        class = "llw-value-box"
      )
    ),
    navset_card_pill(
      id = ns("dashboard"),
      nav_panel(
        title = "Architecture status",
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Dataset architecture status",
          tabindex = "0",
          tableOutput(ns("record_status"))
        )
      ),
      nav_panel(
        title = "Prepared data",
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Prepared dataset table; scroll horizontally when needed",
          tabindex = "0",
          DT::dataTableOutput(ns("prepared_table"), height = "600px")
        )
      ),
      nav_panel(
        title = "Canonical raw data",
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Canonical raw dataset table; scroll horizontally when needed",
          tabindex = "0",
          DT::dataTableOutput(ns("raw_table"), height = "600px")
        )
      )
    )
  )
}

format_record_value <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return("Unknown")
  }
  if (length(value) == 1L && is.atomic(value) && is.na(value)) {
    return("Unknown")
  }
  if (inherits(value, "POSIXt")) {
    return(format(value, tz = "UTC", usetz = TRUE))
  }
  if (is.atomic(value)) {
    return(paste(as.character(value), collapse = ", "))
  }
  paste0("", length(value), " item(s)")
}

format_dataset_bytes <- function(bytes) {
  if (!is.numeric(bytes) || length(bytes) != 1L || !is.finite(bytes)) {
    return("Unknown")
  }
  if (bytes < 1024) return(paste0(format(bytes, big.mark = ","), " B"))
  if (bytes < 1024^2) return(sprintf("%.1f KiB", bytes / 1024))
  sprintf("%.1f MiB", bytes / 1024^2)
}

format_dataset_span <- function(start, end) {
  if (length(start) != 1L || length(end) != 1L || is.na(start) || is.na(end)) {
    return("Unknown")
  }
  paste(
    format(start, tz = "UTC", format = "%Y-%m-%d %H:%M:%S UTC"),
    "to",
    format(end, tz = "UTC", format = "%Y-%m-%d %H:%M:%S UTC")
  )
}

datasetDashboardServer <- function(id, dataset, active_panel) {
  if (!shiny::is.reactive(dataset) || !shiny::is.reactive(active_panel)) {
    abort_llw(
      "`dataset` and `active_panel` must be reactive inputs.",
      type = "validation"
    )
  }

  moduleServer(id, function(input, output, session) {
    event_value <- reactiveVal(NULL)
    empty_modal_visible <- FALSE

    observe({
      is_dashboard <- identical(active_panel(), "dashboard")
      missing_dataset <- is.null(dataset())
      if (is_dashboard && missing_dataset && !empty_modal_visible) {
        empty_modal_visible <<- TRUE
        showModal(modalDialog(
          title = "No dataset selected",
          easyClose = TRUE,
          footer = NULL,
          p("Import a dataset or load the test data to continue."),
          actionButton(
            session$ns("to_import"),
            "Go to import",
            class = "btn-primary",
            icon = icon("file-import"),
            width = "100%"
          )
        ))
      }
      if ((!is_dashboard || !missing_dataset) && empty_modal_visible) {
        removeModal()
        empty_modal_visible <<- FALSE
      }
    })

    observe({
      removeModal()
      empty_modal_visible <<- FALSE
      event_value(structure(
        list(id = new_stable_id("dashboard_event"), type = "open_import"),
        class = c("llw_dashboard_event", "list")
      ))
    }) |>
      bindEvent(input$to_import, ignoreInit = TRUE)

    output$dataset_name <- renderUI({
      record <- dataset()
      req(record)
      h1(record$display_name)
    })
    output$dataset_id <- renderText({
      record <- dataset()
      req(record)
      record$id
    })
    output$revision <- renderText({
      record <- dataset()
      req(record)
      record$revision
    })
    output$raw_rows <- renderText({
      record <- dataset()
      req(record)
      nrow(dataset_raw_data(record))
    })
    output$prepared_rows <- renderText({
      record <- dataset()
      req(record)
      nrow(dataset_prepared_data(record))
    })

    output$record_status <- renderTable(
      {
        record <- dataset()
        req(record)
        inventory <- dataset_record_inventory(record)
        warning_state <- if (inventory$warning_count == 0L) {
          "No recorded warnings"
        } else {
          paste0(
            inventory$warning_count,
            " warning(s): ",
            paste(inventory$warnings, collapse = " | ")
          )
        }
        data.frame(
          Field = c(
            "Source type",
            "Original filenames",
            "Available source-file size",
            "Canonical payload size",
            "Device",
            "Participants",
            "Recorded span (absolute instants)",
            "Source timezone",
            "Datetime display timezone",
            "Dominant sampling interval",
            "Primary variable",
            "Primary unit",
            "Calibration evidence",
            "Recipe revision",
            "Committed recipe steps",
            "Draft present",
            "Undo entries",
            "Warning state"
          ),
          Value = c(
            format_record_value(inventory$source_type),
            format_record_value(inventory$source_files),
            format_dataset_bytes(inventory$source_size_bytes),
            format_dataset_bytes(inventory$canonical_size_bytes),
            format_record_value(inventory$device),
            format_record_value(inventory$participants),
            format_dataset_span(
              inventory$span_start_utc,
              inventory$span_end_utc
            ),
            format_record_value(inventory$source_timezone),
            format_record_value(inventory$datetime_timezone),
            if (is.na(inventory$sampling_seconds)) "Unknown" else
              paste(inventory$sampling_seconds, "seconds"),
            format_record_value(inventory$primary_variable),
            format_record_value(inventory$primary_unit),
            format_record_value(inventory$calibration),
            inventory$recipe_revision,
            inventory$recipe_steps,
            !is.null(record$draft),
            length(record$history),
            warning_state
          ),
          check.names = FALSE
        )
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "s"
    )

    output$prepared_table <- DT::renderDataTable(
      {
        record <- dataset()
        req(record)
        dataset_prepared_data(record)
      },
      options = list(pageLength = 25, scrollX = TRUE)
    )

    output$raw_table <- DT::renderDataTable(
      {
        record <- dataset()
        req(record)
        dataset_raw_data(record)
      },
      options = list(pageLength = 25, scrollX = TRUE)
    )

    list(event = reactive(event_value()))
  })
}

dataset_dashboard_app <- function(...) {
  record <- m1_showcase_record()
  ui <- lightlogweb_page(page_fluid(
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell",
      tabindex = "-1",
      datasetDashboardUI("dashboard")
    ),
    theme = lightlogweb_theme()
  ))
  server <- function(input, output, session) {
    datasetDashboardServer(
      "dashboard",
      dataset = reactive(record),
      active_panel = reactive("dashboard")
    )
  }
  shinyApp(ui, server, ...)
}
