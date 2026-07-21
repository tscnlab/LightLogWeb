datasetSidebarUI <- function(id) {
  ns <- NS(id)

  sidebar(
    title = "Session datasets",
    class = "llw-dataset-sidebar",
    h2(class = "h5", "Session datasets"),
    uiOutput(ns("dataset_list")),
    hr(),
    h3(class = "h6", "Add datasets"),
    tags$div(
      class = "d-grid gap-2",
      actionButton(
        ns("import_newdata"),
        "Start new import" |>
          tooltip2("Open the raw-file import workflow."),
        icon = icon("file-import"),
        class = "btn-primary"
      ),
      actionButton(
        ns("import_testdata"),
        "Load test data" |>
          tooltip2(
            paste(
              "Immediately load LightLogR's deterministic",
              "sample.data.environment with source and variable metadata."
            )
          ),
        icon = icon("file-medical"),
        class = "btn-outline-secondary"
      )
    ),
    hr(),
    h3(class = "h6", "Dataset actions"),
    tags$div(
      class = "d-grid gap-2",
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
        ns("delete_dataset"),
        "Delete dataset" |>
          tooltip2("Remove the selected dataset from this session."),
        icon = icon("trash"),
        class = "btn-outline-danger"
      )
    ),
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
    c("open_import", "load_sample", "select", "rename", "remove")
  )
  if (type %in% c("select", "rename", "remove")) {
    assert_scalar_string(dataset_id, "dataset_id")
  } else if (!is.null(dataset_id)) {
    abort_llw(
      paste0("Manager event `", type, "` does not accept a dataset ID."),
      type = "validation"
    )
  }
  if (identical(type, "rename")) {
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
      emit("load_sample")
    }) |>
      bindEvent(input$import_testdata, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      record <- datasets()[[dataset_id]]
      req(record)
      showModal(modalDialog(
        title = "Rename dataset",
        easyClose = TRUE,
        footer = modalButton("Cancel"),
        textInput(
          session$ns("rename_name"),
          "Display name",
          value = record$display_name,
          width = "100%",
          updateOn = "blur"
        ),
        actionButton(
          session$ns("rename_dataset_real"),
          "Rename",
          class = "btn-primary",
          icon = icon("file-signature"),
          width = "100%"
        )
      ))
    }) |>
      bindEvent(input$rename_dataset, ignoreInit = TRUE)

    observe({
      dataset_id <- selected_dataset_id()
      req(dataset_id)
      name <- trimws(input$rename_name %||% "")
      if (!nzchar(name)) {
        showNotification(
          "Please enter a non-empty display name.",
          type = "error"
        )
        return()
      }
      other_names <- vapply(
        datasets()[names(datasets()) != dataset_id],
        `[[`,
        character(1),
        "display_name"
      )
      if (name %in% other_names) {
        showNotification(
          "That display name is already used in this session.",
          type = "error"
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
          load_sample = showNotification("Load-sample event returned."),
          select = store$dispatch(new_session_event(
            "select",
            dataset_id = event$dataset_id
          )),
          rename = store$dispatch(new_session_event(
            "rename",
            dataset_id = event$dataset_id,
            value = event$value
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
