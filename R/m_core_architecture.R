m1_showcase_record <- function() {
  data <- data.frame(
    Id = factor(rep(c("P01", "P02"), each = 3L)),
    Datetime = as.POSIXct(
      "2026-01-01 08:00:00",
      tz = "UTC"
    ) +
      rep(c(0, 60, 120), times = 2L),
    MEDI = c(10, 20, 30, 40, 50, 60)
  )
  new_dataset_record(
    raw_data = data,
    display_name = "Milestone 1 fixture",
    source_manifest = new_source_manifest(
      source_type = "development_fixture",
      source_timezone = "UTC"
    ),
    analysis_settings = list(primary_variable = "MEDI")
  )
}

core_architecture_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Draft -> preview -> apply"),
      numericInput(
        ns("preview_rows"),
        "Rows retained by the development preview",
        value = 3,
        min = 0,
        step = 1
      ),
      layout_column_wrap(
        actionButton(
          ns("draft"),
          "Create draft",
          icon = icon("pen"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          ns("preview"),
          "Preview",
          icon = icon("eye"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          ns("apply"),
          "Apply draft",
          icon = icon("check"),
          class = "btn-primary"
        ),
        actionButton(
          ns("reset"),
          "Reset to raw",
          icon = icon("rotate-left"),
          class = "btn-outline-secondary"
        ),
        actionButton(
          ns("undo"),
          "Undo",
          icon = icon("rotate-left"),
          class = "btn-outline-secondary"
        )
      )
    ),
    card(
      card_header("Long-task failure containment"),
      p(
        "These development actions use the same ExtendedTask controller and",
        "revision guard intended for imports, GLC work, preparation, merging,",
        "metrics, and reports."
      ),
      layout_column_wrap(
        input_task_button(ns("task_success"), "Successful task"),
        input_task_button(ns("task_warning"), "Task with warning"),
        input_task_button(ns("task_failure"), "Failed task"),
        input_task_button(ns("task_stale"), "Stale task"),
        actionButton(
          ns("task_cancel"),
          "Cancel current task",
          icon = icon("ban")
        )
      ),
      uiOutput(ns("task_status"))
    ),
    card(
      card_header("Development status"),
      tableOutput(ns("status"))
    )
  )
}

new_core_architecture_event <- function(type, value = NULL) {
  type <- match.arg(
    type,
    c(
      "draft",
      "preview",
      "apply",
      "reset",
      "undo",
      "task_success",
      "task_warning",
      "task_failure",
      "task_stale",
      "task_cancel"
    )
  )
  if (identical(type, "draft")) {
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        value < 0 ||
        value != floor(value)
    ) {
      abort_llw(
        "Draft preview rows must be one non-negative whole number.",
        type = "validation"
      )
    }
    value <- as.integer(value)
  } else if (!is.null(value)) {
    abort_llw(
      paste0("Core event `", type, "` does not accept a value."),
      type = "validation"
    )
  }
  event <- structure(
    list(id = new_stable_id("core_event"), type = type, value = value),
    class = c("llw_core_architecture_event", "list")
  )
  assert_serializable_value(event, "core architecture event")
  event
}

core_architecture_server <- function(id, dataset, task) {
  if (!shiny::is.reactive(dataset)) {
    abort_llw("`dataset` must be reactive.", type = "validation")
  }
  if (!is.list(task) || !shiny::is.reactive(task$status)) {
    abort_llw(
      "`task` must be returned by `new_long_task()`.",
      type = "validation"
    )
  }

  moduleServer(id, function(input, output, session) {
    for (button_id in c(
      "task_success",
      "task_warning",
      "task_failure",
      "task_stale"
    )) {
      bslib::bind_task_button(task$extended_task, button_id)
    }
    event_value <- reactiveVal(NULL)
    emit <- function(type, value = NULL) {
      event_value(new_core_architecture_event(type, value))
    }

    observe(emit("draft", as.integer(input$preview_rows))) |>
      bindEvent(input$draft, ignoreInit = TRUE)
    observe(emit("preview")) |>
      bindEvent(input$preview, ignoreInit = TRUE)
    observe(emit("apply")) |>
      bindEvent(input$apply, ignoreInit = TRUE)
    observe(emit("reset")) |>
      bindEvent(input$reset, ignoreInit = TRUE)
    observe(emit("undo")) |>
      bindEvent(input$undo, ignoreInit = TRUE)
    observe(emit("task_success")) |>
      bindEvent(input$task_success, ignoreInit = TRUE)
    observe(emit("task_warning")) |>
      bindEvent(input$task_warning, ignoreInit = TRUE)
    observe(emit("task_failure")) |>
      bindEvent(input$task_failure, ignoreInit = TRUE)
    observe(emit("task_stale")) |>
      bindEvent(input$task_stale, ignoreInit = TRUE)
    observe(emit("task_cancel")) |>
      bindEvent(input$task_cancel, ignoreInit = TRUE)

    output$task_status <- renderUI({
      llw_task_status(task$status(), context = "Development task")
    })

    output$status <- renderTable(
      {
        record <- dataset()
        req(record)
        data.frame(
          Signal = c(
            "Dataset ID",
            "Revision",
            "Raw rows",
            "Prepared rows",
            "Draft",
            "Preview",
            "Undo entries",
            "Task state"
          ),
          Value = c(
            record$id,
            record$revision,
            nrow(dataset_raw_data(record)),
            nrow(dataset_prepared_data(record)),
            !is.null(record$draft),
            !is.null(record$draft$preview),
            length(record$history),
            task$state()
          ),
          check.names = FALSE
        )
      },
      striped = TRUE,
      bordered = TRUE,
      spacing = "s"
    )

    list(event = reactive(event_value()))
  })
}

core_architecture_app <- function() {
  ui <- lightlogweb_page(page_fluid(
    lightlogweb_head(),
    lightlogweb_skip_link(),
    tags$main(
      id = "llw-main-content",
      class = "llw-main-shell",
      tabindex = "-1",
      llw_view_header(
        "Module showcase",
        "Core workflow architecture",
        paste(
          "Exercise immutable raw data, revisioned workflow changes,",
          "recoverable task failures, and stale-result rejection."
        )
      ),
      core_architecture_ui("core")
    ),
    theme = lightlogweb_theme()
  ))

  server <- function(input, output, session) {
    profile <- resolve_runtime_profile("local", workers = 1)
    runtime <- new_session_runtime(profile, session = session)
    store <- new_session_store(profile$name)
    store$dispatch(new_session_event("add", value = m1_showcase_record()))

    worker <- function(payload, spec) {
      delay <- if (is.null(payload$delay)) 0.75 else payload$delay
      Sys.sleep(delay)
      if (isTRUE(payload$fail)) {
        stop("Development worker failure", call. = FALSE)
      }
      if (isTRUE(payload$warn)) {
        warning("Development worker warning", call. = FALSE)
      }
      list(rows = payload$rows, revision = spec$dataset_revision)
    }
    task <- new_long_task(
      worker = worker,
      task_type = "preparation",
      runtime = runtime,
      revision_lookup = store$revision
    )
    core <- core_architecture_server(
      "core",
      dataset = store$selected_dataset,
      task = task
    )

    replace_record <- function(record) {
      store$dispatch(new_session_event("replace", value = record))
    }
    handle_event <- function(event) {
      record <- store$selected_dataset()
      switch(
        event$type,
        draft = replace_record(stage_dataset_draft(
          record,
          list(
            recipe = new_recipe(list(new_recipe_step(
              type = "preview_rows",
              parameters = list(n = event$value)
            )))
          )
        )),
        preview = replace_record(preview_dataset_draft(
          record,
          function(raw_data, recipe, analysis_settings, factual_metadata) {
            utils::head(raw_data, recipe$steps[[1L]]$parameters$n)
          }
        )),
        apply = replace_record(apply_dataset_draft(record)),
        reset = replace_record(reset_dataset_record(record)),
        undo = replace_record(undo_dataset_record(record)),
        task_success = task$invoke(
          list(rows = nrow(dataset_prepared_data(record))),
          dataset_id = record$id
        ),
        task_warning = task$invoke(
          list(rows = nrow(dataset_prepared_data(record)), warn = TRUE),
          dataset_id = record$id
        ),
        task_failure = task$invoke(
          list(rows = nrow(dataset_prepared_data(record)), fail = TRUE),
          dataset_id = record$id
        ),
        task_stale = {
          task$invoke(
            list(rows = nrow(dataset_prepared_data(record))),
            dataset_id = record$id
          )
          replace_record(reset_dataset_record(record))
        },
        task_cancel = task$cancel()
      )
    }

    observe({
      event <- core$event()
      req(event)
      tryCatch(
        handle_event(event),
        llw_error = function(cnd) {
          showNotification(llw_public_message(cnd), type = "error")
        },
        error = function(cnd) {
          mapped <- normalize_task_error(cnd, "preparation")
          showNotification(llw_public_message(mapped), type = "error")
        }
      )
    }) |>
      bindEvent(core$event(), ignoreInit = TRUE)
  }

  shinyApp(ui = ui, server = server)
}
