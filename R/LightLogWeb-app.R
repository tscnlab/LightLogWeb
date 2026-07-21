shiny_upload_limit_on_start <- function(runtime_profile) {
  if (!inherits(runtime_profile, "llw_runtime_profile")) {
    abort_llw(
      "`runtime_profile` must be created by `resolve_runtime_profile()`.",
      type = "validation"
    )
  }
  max_upload_bytes <- runtime_profile$max_upload_bytes
  force(max_upload_bytes)
  function() {
    old_options <- options(shiny.maxRequestSize = max_upload_bytes)
    shiny::onStop(function() options(old_options))
  }
}

#' Launch LightLogWeb
#'
#' @param profile Runtime profile. `"auto"` respects
#'   `LIGHTLOGWEB_PROFILE`; otherwise interactive launches use `"local"` and
#'   non-interactive launches use `"hosted"`.
#' @param max_upload_mb Maximum total upload request size in mebibytes (MiB).
#'   Hosted raw-import actions have a 200 MiB safety ceiling. Explicit local
#'   profiles may use a higher limit when disk, parsing time, and memory have
#'   been provisioned accordingly.
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
  import_presentation <- "wizard"
  production_import_ui <- switch(
    import_presentation,
    wizard = importWizardUI,
    accordion = importUI
  )

  ui <- lightlogweb_page(page_navbar(
    title = lightlogweb_wordmark(),
    window_title = "LightLogWeb - Make light exposure legible",
    id = "main_nav",
    selected = "Import",
    theme = lightlogweb_theme(),
    navbar_options = navbar_options(theme = "auto", underline = TRUE),
    header = tagList(
      lightlogweb_head(),
      lightlogweb_skip_link(),
      tags$span(
        id = "llw-main-content",
        class = "visually-hidden",
        tabindex = "-1",
        "Main content"
      )
    ),
    sidebar = datasetSidebarUI("datasets"),
    nav_panel(
      "Import",
      tags$main(
        class = "llw-main-shell",
        `aria-label` = "Import light exposure data",
        llw_view_header(
          "Start with source evidence",
          "Import light exposure data",
          paste(
            "Choose source files and make device, time zone, participant ID,",
            "and cleaning decisions explicit before anything is added to",
            "this session."
          )
        ),
        production_import_ui("import")
      )
    ),
    nav_panel(
      "Dashboard",
      value = "dashboard",
      tags$main(
        class = "llw-main-shell",
        `aria-label` = "Inspect the selected dataset",
        datasetDashboardUI("dashboard")
      )
    ),
    nav_spacer(),
    nav_item(
      tags$div(
        class = "llw-mode-control",
        tags$span(class = "visually-hidden", "Interface color mode"),
        input_dark_mode(id = "color_mode")
      )
    )
  ))

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

    color_mode <- reactive(input$color_mode %||% "light")
    imported <- importServer(
      "import",
      runtime = runtime,
      color_mode = color_mode,
      presentation = import_presentation
    )
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
          imported$open_import()
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
        imported$open_import()
      }
    }) |>
      bindEvent(dashboard$event(), ignoreInit = TRUE)
  }

  on_start <- shiny_upload_limit_on_start(runtime_profile)

  shinyApp(ui = ui, server = server, onStart = on_start)
}
