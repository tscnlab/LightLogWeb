datasetSidebarUI <- function(id, example_choices = dataset_example_choices()) {
  ns <- NS(id)
  if (!is.character(example_choices) || is.null(names(example_choices))) {
    abort_llw(
      "`example_choices` must be a named character vector.",
      type = "validation"
    )
  }

  sidebar(
    title = "Session datasets",
    class = "llw-dataset-sidebar",
    h2(class = "h5", "Session datasets"),
    uiOutput(ns("dataset_list")),
    hr(),
    h3(class = "h6", "Add datasets"),
    tags$div(
      class = "d-grid gap-2",
      selectInput(
        ns("example_key"),
        "Ready-to-use example",
        choices = example_choices,
        width = "100%"
      ),
      actionButton(
        ns("load_example"),
        "Load selected example" |>
          tooltip2(
            paste(
              "Add a reviewed development dataset directly to this session",
              "without stepping through the import form."
            )
          ),
        icon = icon("flask"),
        class = "btn-outline-secondary"
      ),
      actionButton(
        ns("import_newdata"),
        "Start new import" |>
          tooltip2("Open the raw-file import workflow."),
        icon = icon("file-import"),
        class = "btn-primary"
      )
    ),
    tags$p(
      class = "llw-secondary small mt-2 mb-0",
      paste(
        "Examples are session copies. The import workflow remains available",
        "for new files whenever you need it."
      )
    ),
    hr(),
    h3(class = "h6", "Dataset actions"),
    tags$div(
      class = "d-grid gap-2",
      actionButton(
        ns("append_datasets"),
        "Append datasets" |>
          tooltip2(
            "Open the source-aware row-wise append preview. At least two datasets are required."
          ),
        icon = icon("code-merge"),
        class = "btn-primary"
      ),
      actionButton(
        ns("rename_dataset"),
        "Rename dataset" |>
          tooltip2(
            "Change only the display name; the stable dataset ID is retained."
          ),
        icon = icon("file-signature"),
        class = "btn-outline-secondary"
      ),
      actionButton(
        ns("duplicate_dataset"),
        "Duplicate dataset" |>
          tooltip2(
            "Create an independent session record with a new stable ID while preserving source provenance."
          ),
        icon = icon("copy"),
        class = "btn-outline-secondary"
      ),
      actionButton(
        ns("reset_dataset"),
        "Reset pre-processed data" |>
          tooltip2(
            "Restore pre-processed data to the immutable source data; the reset remains undoable."
          ),
        icon = icon("rotate-left"),
        class = "btn-outline-secondary"
      ),
      actionButton(
        ns("delete_dataset"),
        "Delete dataset" |>
          tooltip2("Remove the selected dataset from this session."),
        icon = icon("trash"),
        class = "btn-outline-danger"
      )
    ),
    uiOutput(ns("selected_summary")),
    tags$p(
      class = "llw-secondary mt-3 mb-0",
      icon("shield-halved", `aria-hidden` = "true"),
      " Session data are kept only for this browser session."
    ),
    open = "desktop",
    width = 300
  )
}

new_dataset_manager_event <- function(type, dataset_id = NULL, value = NULL) {
  type <- match.arg(
    type,
    c(
      "open_import",
      "load_example",
      "open_append",
      "select",
      "rename",
      "duplicate",
      "reset",
      "remove"
    )
  )
  if (type %in% c("select", "rename", "duplicate", "reset", "remove")) {
    assert_scalar_string(dataset_id, "dataset_id")
  } else if (!is.null(dataset_id)) {
    abort_llw(
      paste0("Manager event `", type, "` does not accept a dataset ID."),
      type = "validation"
    )
  }
  if (type %in% c("rename", "duplicate", "load_example")) {
    assert_scalar_string(value, "value")
  } else if (!is.null(value)) {
    abort_llw(
      paste0("Manager event `", type, "` does not accept a value."),
      type = "validation"
    )
  }
  event <- structure(
    list(
      id = new_stable_id("manager_event"),
      type = type,
      dataset_id = dataset_id,
      value = value
    ),
    class = c("llw_dataset_manager_event", "list")
  )
  assert_serializable_value(event, "dataset manager event")
  event
}

dataset_name_entry_dialog <- function(
  ns,
  title,
  description,
  input_id,
  submit_id,
  value,
  submit_label,
  submit_icon,
  error = NULL
) {
  value <- clean_dataset_display_name(value)
  if (!is.null(error)) assert_scalar_string(error, "error")
  modalDialog(
    title = title,
    easyClose = TRUE,
    footer = modalButton("Cancel"),
    if (!is.null(error)) {
      llw_status_callout(
        "error",
        error,
        heading = "Choose a different dataset name",
        compact = TRUE
      )
    },
    tags$p(description),
    textInput(
      ns(input_id),
      "Dataset name",
      value = value,
      width = "100%"
    ),
    actionButton(
      ns(submit_id),
      submit_label,
      class = "btn-primary",
      icon = icon(submit_icon),
      width = "100%"
    )
  )
}

dataset_name_conflict_dialog <- function(
  condition,
  ns = identity,
  value = NULL,
  error = NULL
) {
  if (!inherits(condition, "llw_dataset_name_conflict_error")) {
    abort_llw(
      "A dataset-name conflict condition is required.",
      type = "validation"
    )
  }
  if (!is.function(ns)) {
    abort_llw("`ns` must be a namespace function.", type = "validation")
  }
  value <- value %||% condition$diagnostics$attempted_name %||% ""
  assert_scalar_string(value, "value", allow_empty = TRUE)
  error <- error %||% llw_public_message(condition)
  assert_scalar_string(error, "error")
  modalDialog(
    title = "Choose a different dataset name",
    easyClose = TRUE,
    footer = modalButton("Cancel"),
    llw_status_callout(
      "error",
      error,
      heading = "Dataset name already in use"
    ),
    tags$p(
      class = "mb-3",
      paste(
        "No dataset was added or replaced. Return to the current workflow and",
        "enter another name, or choose a unique name here and retry now."
      )
    ),
    textInput(
      ns("dataset_name_conflict_value"),
      "New dataset name",
      value = trimws(value),
      width = "100%"
    ),
    actionButton(
      ns("dataset_name_conflict_apply"),
      "Use different name",
      class = "btn-primary",
      icon = icon("check"),
      width = "100%"
    )
  )
}

datasetManagerServer <- function(id, datasets, selected_dataset_id) {
  if (
    !shiny::is.reactive(datasets) || !shiny::is.reactive(selected_dataset_id)
  ) {
    abort_llw(
      "`datasets` and `selected_dataset_id` must be reactive inputs.",
      type = "validation"
    )
  }

  moduleServer(id, function(input, output, session) {
    event_value <- reactiveVal(NULL)
    emit <- function(type, dataset_id = NULL, value = NULL) {
      event_value(new_dataset_manager_event(type, dataset_id, value))
    }

    show_rename_dialog <- function(
      record,
      value = record$display_name,
      error = NULL
    ) {
      showModal(dataset_name_entry_dialog(
        session$ns,
        title = "Rename dataset",
        description = paste(
          "Change only the display name; the stable dataset ID and all data",
          "remain unchanged. Names must be unique in this session."
        ),
        input_id = "rename_name",
        submit_id = "rename_dataset_real",
        value = value,
        submit_label = "Rename",
        submit_icon = "file-signature",
        error = error
      ))
    }

    show_duplicate_dialog <- function(record, value = NULL, error = NULL) {
      if (is.null(value)) value <- paste0(record$display_name, " copy")
      showModal(dataset_name_entry_dialog(
        session$ns,
        title = "Duplicate dataset",
        description = paste(
          "Create an independent session record with a new stable ID.",
          "Choose a unique name for the copy."
        ),
        input_id = "duplicate_name",
        submit_id = "duplicate_dataset_real",
        value = value,
        submit_label = "Create duplicate",
        submit_icon = "copy",
        error = error
      ))
    }

    output$dataset_list <- renderUI({
      records <- datasets()
      if (length(records) == 0L) {
        return(llw_status_callout(
          "idle",
          "Import files or load test data to begin.",
          heading = "No datasets in this session",
          compact = TRUE
        ))
      }
      ids <- names(records)
      labels <- vapply(records, `[[`, character(1), "display_name")
      radioButtons(
        session$ns("dataset_select"),
        label = NULL,
        choices = stats::setNames(ids, labels),
        selected = selected_dataset_id(),
        width = "100%"
      )
    })

    output$selected_summary <- renderUI({
      dataset_id <- selected_dataset_id()
      if (is.null(dataset_id)) return(NULL)
      record <- datasets()[[dataset_id]]
      req(record)
      inventory <- dataset_record_inventory(record)
      warning_label <- if (inventory$warning_count == 0L) {
        "No recorded warnings"
      } else {
        paste(inventory$warning_count, "recorded warning(s)")
      }
      tags$div(
        class = "mt-3 pt-3 border-top small",
        tags$div(class = "fw-semibold", "Selected dataset"),
        tags$div(
          class = "llw-secondary",
          paste(
            format(inventory$rows, big.mark = ","),
            "rows |",
            if (is.na(inventory$participants)) "Participants unknown" else
              paste(inventory$participants, "participant(s)")
          )
        ),
        tags$div(
          class = "llw-secondary",
          paste(
            "Source:",
            inventory$source_type,
            "| Zone:",
            if (is.na(inventory$source_timezone)) "Unknown" else
              inventory$source_timezone
          )
        ),
        tags$div(class = "llw-secondary", warning_label)
      )
    })

    observe({
      req(input$dataset_select)
      if (!identical(input$dataset_select, selected_dataset_id())) {
        emit("select", dataset_id = input$dataset_select)
      }
    }) |>
      bindEvent(input$dataset_select, ignoreInit = TRUE)

    observe({
      emit("open_import")
    }) |>
      bindEvent(input$import_newdata, ignoreInit = TRUE)

    observe({
      req(input$example_key)
      emit("load_example", value = input$example_key)
    }) |>
      bindEvent(input$load_example, ignoreInit = TRUE)

    observe({
      emit("open_append")
    }) |>
      bindEvent(input$append_datasets, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      record <- datasets()[[dataset_id]]
      req(record)
      show_duplicate_dialog(record)
    }) |>
      bindEvent(input$duplicate_dataset, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      record <- datasets()[[dataset_id]]
      req(record)
      name <- trimws(input$duplicate_name %||% "")
      if (!nzchar(name)) {
        show_duplicate_dialog(
          record,
          value = record$display_name,
          error = "Enter a non-empty dataset name."
        )
        return()
      }
      existing_names <- vapply(
        datasets(),
        `[[`,
        character(1),
        "display_name"
      )
      conflict <- dataset_display_name_conflict(name, existing_names)
      if (!is.null(conflict)) {
        show_duplicate_dialog(
          record,
          value = name,
          error = dataset_name_conflict_message(name, conflict)
        )
        return()
      }
      removeModal()
      emit("duplicate", dataset_id = dataset_id, value = name)
    }) |>
      bindEvent(input$duplicate_dataset_real, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      record <- datasets()[[dataset_id]]
      req(record)
      showModal(modalDialog(
        title = "Reset pre-processed data?",
        easyClose = TRUE,
        footer = modalButton("Keep current preparation"),
        p(
          "Pre-processed data for ",
          strong(record$display_name),
          " will return to the unchanged source data. This action can be undone."
        ),
        actionButton(
          session$ns("reset_dataset_real"),
          "Reset pre-processed data",
          icon = icon("rotate-left"),
          class = "btn-warning",
          width = "100%"
        )
      ))
    }) |>
      bindEvent(input$reset_dataset, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      removeModal()
      emit("reset", dataset_id = dataset_id)
    }) |>
      bindEvent(input$reset_dataset_real, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      record <- datasets()[[dataset_id]]
      req(record)
      show_rename_dialog(record)
    }) |>
      bindEvent(input$rename_dataset, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      record <- datasets()[[dataset_id]]
      req(record)
      name <- trimws(input$rename_name %||% "")
      if (!nzchar(name)) {
        show_rename_dialog(
          record,
          value = record$display_name,
          error = "Enter a non-empty dataset name."
        )
        return()
      }
      other_names <- vapply(
        datasets()[names(datasets()) != dataset_id],
        `[[`,
        character(1),
        "display_name"
      )
      conflict <- dataset_display_name_conflict(name, other_names)
      if (!is.null(conflict)) {
        show_rename_dialog(
          record,
          value = name,
          error = dataset_name_conflict_message(name, conflict)
        )
        return()
      }
      removeModal()
      emit("rename", dataset_id = dataset_id, value = name)
    }) |>
      bindEvent(input$rename_dataset_real, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      record <- datasets()[[dataset_id]]
      req(record)
      showModal(modalDialog(
        title = "Delete dataset?",
        easyClose = TRUE,
        footer = modalButton("Keep dataset"),
        p(
          "This removes ",
          strong(record$display_name),
          " from the current session."
        ),
        actionButton(
          session$ns("delete_dataset_real"),
          "Delete dataset",
          icon = icon("trash"),
          class = "btn-danger",
          width = "100%"
        )
      ))
    }) |>
      bindEvent(input$delete_dataset, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      removeModal()
      emit("remove", dataset_id = dataset_id)
    }) |>
      bindEvent(input$delete_dataset_real, ignoreInit = TRUE)

    list(event = reactive(event_value()))
  })
}

dataset_manager_app <- function(...) {
  ui <- lightlogweb_page(page_sidebar(
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell",
      tabindex = "-1",
      llw_view_header(
        "Module showcase",
        "Dataset manager",
        "Exercise stable selection, rename, removal, import, and sample events."
      ),
      card(
        card_header("Development status"),
        tags$div(
          class = "llw-data-region",
          role = "region",
          `aria-label` = "Dataset manager development status",
          tabindex = "0",
          tableOutput("status")
        )
      )
    ),
    title = lightlogweb_wordmark(),
    sidebar = datasetSidebarUI("datasets"),
    theme = lightlogweb_theme()
  ))
  server <- function(input, output, session) {
    store <- new_session_store("local")
    first <- m1_showcase_record()
    first$display_name <- "First fixture"
    first <- validate_dataset_record(first)
    second <- m1_showcase_record()
    second$display_name <- "Second fixture"
    second <- validate_dataset_record(second)
    store$dispatch(new_session_event("add", value = first))
    store$dispatch(new_session_event("add", value = second))

    manager <- datasetManagerServer(
      "datasets",
      datasets = store$datasets,
      selected_dataset_id = store$selected_dataset_id
    )
    observe({
      event <- manager$event()
      req(event)
      tryCatch(
        switch(
          event$type,
          open_import = showNotification("Open-import event returned."),
          load_example = showNotification(
            paste("Load-example event returned for", event$value)
          ),
          open_append = showNotification("Open-append event returned."),
          select = store$dispatch(new_session_event(
            "select",
            dataset_id = event$dataset_id
          )),
          rename = store$dispatch(new_session_event(
            "rename",
            dataset_id = event$dataset_id,
            value = event$value
          )),
          duplicate = {
            source <- session_dataset(store$model(), event$dataset_id)
            store$dispatch(new_session_event(
              "add",
              value = duplicate_dataset_record(source, event$value)
            ))
          },
          reset = store$dispatch(new_session_event(
            "reset",
            dataset_id = event$dataset_id
          )),
          remove = store$dispatch(new_session_event(
            "remove",
            dataset_id = event$dataset_id
          ))
        ),
        llw_error = function(cnd) {
          showNotification(llw_public_message(cnd), type = "error")
        }
      )
    }) |>
      bindEvent(manager$event(), ignoreInit = TRUE)

    output$status <- renderTable({
      model <- store$model()
      data.frame(
        Signal = c("Dataset count", "Selected stable ID", "Display names"),
        Value = c(
          length(model$datasets),
          model$selected_dataset_id %||% "None",
          paste(
            vapply(model$datasets, `[[`, character(1), "display_name"),
            collapse = ", "
          )
        ),
        check.names = FALSE
      )
    })
  }
  shinyApp(ui, server, ...)
}
