#' Launch LightLogWeb
#'
#' @param profile Runtime profile. `"auto"` respects
#'   `LIGHTLOGWEB_PROFILE`; otherwise interactive launches use `"local"` and
#'   non-interactive launches use `"hosted"`.
#' @param max_upload_mb Maximum total upload request size in megabytes.
#' @param workers Number of background workers. `NULL` selects one hosted
#'   worker or at most two local workers. Use `0` for the synchronous fallback.
#'
#' @return A `shiny.appobj` that can be run with [shiny::runApp()].
#' @export
#'
#' @examples
#' if (interactive()) {
#'   LightLogWeb()
#' }
LightLogWeb <- function(
  profile = c("auto", "hosted", "local"),
  max_upload_mb = 200,
  workers = NULL
) {
  runtime_profile <- resolve_runtime_profile(
    profile = profile,
    max_upload_mb = max_upload_mb,
    workers = workers
  )

  ui <- page_navbar(
    title = h1("LightLogWeb"),
    id = "main_nav",
    selected = "Import",
    sidebar = datasetSidebarUI("datasets"),
    nav_panel("Import", importUI("import")),
    nav_panel(
      "Dashboard",
      value = "dashboard",
      datasetDashboardUI("dashboard")
    )
  )

  server <- function(input, output, session) {
    session$allowReconnect(TRUE)
    runtime <- new_session_runtime(runtime_profile, session = session)
    store <- new_session_store(runtime$profile$name)
    if (inherits(runtime$startup_error, "llw_error")) {
      showNotification(
        llw_public_message(runtime$startup_error),
        type = "warning",
        duration = NULL
      )
    }

    imported <- importServer("import", runtime = runtime)
    manager <- datasetManagerServer(
      "datasets",
      datasets = store$datasets,
      selected_dataset_id = store$selected_dataset_id
    )
    dashboard <- datasetDashboardServer(
      "dashboard",
      dataset = store$selected_dataset,
      active_panel = reactive(input$main_nav)
    )

    dispatch_safely <- function(event, success = NULL) {
      tryCatch(
        {
          store$dispatch(event)
          if (!is.null(success)) {
            showNotification(success, type = "message")
          }
          TRUE
        },
        llw_error = function(cnd) {
          showNotification(
            llw_public_message(cnd),
            type = "error",
            duration = NULL
          )
          FALSE
        },
        error = function(cnd) {
          mapped <- normalize_task_error(cnd, "preparation")
          showNotification(
            llw_public_message(mapped),
            type = "error",
            duration = NULL
          )
          FALSE
        }
      )
    }

    observe({
      value <- imported$add_dataset()
      record <- tryCatch(
        new_imported_dataset_record(value),
        error = identity
      )
      if (inherits(record, "error")) {
        mapped <- normalize_task_error(record, "raw_import")
        showNotification(
          llw_public_message(mapped),
          type = "error",
          duration = NULL
        )
        return()
      }
      if (
        dispatch_safely(
          new_session_event("add", value = record),
          paste0("Dataset `", record$display_name, "` added to this session.")
        )
      ) {
        nav_select("main_nav", selected = "dashboard")
      }
    }) |>
      bindEvent(imported$add_dataset(), ignoreInit = TRUE)

    observe({
      event <- manager$event()
      req(event)
      switch(
        event$type,
        open_import = {
          nav_select("main_nav", selected = "Import")
          accordion_panel_open("import-import_accordion", "import_specs")
        },
        load_sample = {
          record <- tryCatch(sample_dataset_record(), error = identity)
          if (inherits(record, "error")) {
            mapped <- normalize_task_error(record, "raw_import")
            showNotification(
              llw_public_message(mapped),
              type = "error",
              duration = NULL
            )
            return()
          }
          if (
            dispatch_safely(
              new_session_event("add", value = record),
              "Test dataset added to this session."
            )
          ) {
            nav_select("main_nav", selected = "dashboard")
          }
        },
        select = dispatch_safely(new_session_event(
          "select",
          dataset_id = event$dataset_id
        )),
        rename = dispatch_safely(
          new_session_event(
            "rename",
            dataset_id = event$dataset_id,
            value = event$value
          ),
          "Dataset display name updated."
        ),
        remove = dispatch_safely(
          new_session_event("remove", dataset_id = event$dataset_id),
          "Dataset removed from this session."
        )
      )
    }) |>
      bindEvent(manager$event(), ignoreInit = TRUE)

    observe({
      event <- dashboard$event()
      req(event)
      if (identical(event$type, "open_import")) {
        nav_select("main_nav", selected = "Import")
        accordion_panel_open("import-import_accordion", "import_specs")
      }
    }) |>
      bindEvent(dashboard$event(), ignoreInit = TRUE)
  }

  on_start <- function() {
    old_options <- options(
      shiny.maxRequestSize = runtime_profile$max_upload_bytes
    )
    shiny::onStop(function() options(old_options))
  }

  shinyApp(ui = ui, server = server, onStart = on_start)
}
