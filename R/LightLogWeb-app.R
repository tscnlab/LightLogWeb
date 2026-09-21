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

deliver_metadata_focus_request <- function(focus_value, request) {
  if (!is.function(focus_value)) {
    abort_llw(
      "`focus_value` must be a reactive value setter.",
      type = "validation"
    )
  }
  focus_value(NULL)
  focus_value(request)
  invisible(request)
}

new_dataset_example_task_payload <- function(
  key,
  project_root = lightlogweb_development_root()
) {
  assert_scalar_string(key, "key")
  assert_scalar_string(project_root, "project_root")
  project_root <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )
  payload <- list(
    schema_version = "1.0.0",
    key = key,
    project_root = project_root
  )
  assert_serializable_value(payload, "dataset example task payload")
  payload
}

validate_dataset_example_task_payload <- function(payload) {
  if (
    !is.list(payload) ||
      !identical(
        names(payload),
        c("schema_version", "key", "project_root")
      ) ||
      !identical(payload$schema_version, "1.0.0")
  ) {
    abort_llw(
      "The dataset example task payload is malformed or unsupported.",
      type = "validation"
    )
  }
  assert_scalar_string(payload$key, "payload$key")
  assert_scalar_string(payload$project_root, "payload$project_root")
  if (!dir.exists(payload$project_root)) {
    abort_llw(
      "The dataset example project root is unavailable.",
      type = "unavailable_feature",
      public_message = paste(
        "The ready-to-use example files are unavailable in this checkout.",
        "Choose another example or use the import workflow."
      )
    )
  }
  assert_serializable_value(payload, "dataset example task payload")
  payload
}

dataset_example_load_task_worker <- function(payload, spec = NULL) {
  payload <- validate_dataset_example_task_payload(payload)
  load_dataset_example(
    payload$key,
    project_root = payload$project_root
  )
}

new_dataset_example_loader <- function(
  runtime,
  schedule,
  on_busy,
  on_loaded,
  on_failed,
  worker = dataset_example_load_task_worker
) {
  callbacks <- list(
    schedule = schedule,
    on_busy = on_busy,
    on_loaded = on_loaded,
    on_failed = on_failed,
    worker = worker
  )
  if (!all(vapply(callbacks, is.function, logical(1)))) {
    abort_llw(
      "Dataset example loader callbacks and worker must be functions.",
      type = "validation"
    )
  }

  pending <- shiny::reactiveVal(FALSE)
  current_request <- NULL
  handled_task_id <- NULL
  task <- if (identical(worker, dataset_example_load_task_worker)) {
    new_long_task(
      worker = dataset_example_load_task_worker,
      task_type = "preparation",
      runtime = runtime
    )
  } else {
    new_long_task(
      worker = worker,
      task_type = "preparation",
      runtime = runtime
    )
  }

  active <- shiny::reactive({
    isTRUE(pending()) ||
      task$state() %in% c("queued", "running", "finalizing")
  })

  invoke <- function(
    key,
    project_root = lightlogweb_development_root()
  ) {
    if (isTRUE(shiny::isolate(active()))) return(invisible(FALSE))
    payload <- tryCatch(
      new_dataset_example_task_payload(key, project_root),
      error = identity
    )
    if (inherits(payload, "error")) {
      on_failed(normalize_task_error(payload, "preparation"), key, NULL)
      return(invisible(FALSE))
    }

    pending(TRUE)
    current_request <<- payload
    busy_error <- tryCatch(
      {
        on_busy(payload$key)
        NULL
      },
      error = identity
    )
    if (inherits(busy_error, "error")) {
      pending(FALSE)
      on_failed(
        normalize_task_error(busy_error, "preparation"),
        payload$key,
        NULL
      )
      return(invisible(FALSE))
    }

    scheduled <- tryCatch(
      {
        schedule(function() task$invoke(payload))
        TRUE
      },
      error = identity
    )
    if (inherits(scheduled, "error")) {
      pending(FALSE)
      on_failed(
        normalize_task_error(scheduled, "preparation"),
        payload$key,
        NULL
      )
      return(invisible(FALSE))
    }
    invisible(TRUE)
  }

  shiny::observe({
    status <- task$status()
    if (
      !status$state %in%
        c("complete", "warning", "error", "cancelled", "stale") ||
        is.null(status$task_id) ||
        identical(status$task_id, handled_task_id)
    ) {
      return()
    }
    handled_task_id <<- status$task_id
    pending(FALSE)
    key <- current_request$key %||% "selected example"

    if (status$state %in% c("complete", "warning")) {
      callback_error <- tryCatch(
        {
          on_loaded(task$result(), key, status)
          NULL
        },
        error = identity
      )
      if (inherits(callback_error, "error")) {
        on_failed(
          normalize_task_error(callback_error, "preparation"),
          key,
          status
        )
      }
      return()
    }

    condition <- if (identical(status$state, "error")) {
      task$error()
    } else {
      new_llw_error(
        paste("Dataset example task settled as", status$state),
        type = "preparation",
        public_message = paste(
          "Example loading did not complete.",
          "The previous dataset remains selected and you can retry."
        )
      )
    }
    on_failed(condition, key, status)
  })

  list(
    invoke = invoke,
    cancel = task$cancel,
    active = active,
    state = shiny::reactive({
      if (isTRUE(pending()) && identical(task$state(), "idle")) {
        "queued"
      } else {
        task$state()
      }
    }),
    status = task$status,
    result = task$result,
    error = task$error
  )
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
      "GLC Import",
      value = "glc_import",
      tags$main(
        class = "llw-main-shell",
        `aria-label` = "Import a validated GLC package selection",
        glcPreviewUI(
          "glc_preview",
          source_ui = glcSourceUI("glc_source")
        )
      )
    ),
    nav_panel(
      "Dashboard",
      value = "dashboard",
      tags$main(
        class = "llw-main-shell llw-dashboard-shell",
        `aria-label` = "Inspect the selected dataset",
        datasetDashboardUI("dashboard")
      )
    ),
    nav_panel(
      "Metadata",
      value = "metadata",
      tags$main(
        class = "llw-main-shell llw-metadata-shell",
        `aria-label` = "Explore retained dataset metadata",
        metadata_explorer_ui("metadata")
      )
    ),
    nav_panel(
      "Append",
      value = "append",
      tags$main(
        class = "llw-main-shell",
        `aria-label` = "Append session datasets",
        append_merge_ui("append")
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
    metadata_focus_request_value <- reactiveVal(NULL)
    imported <- importServer(
      "import",
      runtime = runtime,
      color_mode = color_mode,
      presentation = import_presentation
    )
    glc_source <- glcSourceServer(
      "glc_source",
      registry_url = glc_default_registry_url()
    )
    glc_import <- glcPreviewServer(
      "glc_preview",
      source_selection = glc_source$selection,
      runtime = runtime,
      cache_dir = runtime$paths$cache,
      auto_inspect = reactive(
        identical(glc_source$deep_link()$state, "matched")
      ),
      enable_library_integration = TRUE
    )
    manager <- datasetManagerServer(
      "datasets",
      datasets = store$datasets,
      selected_dataset_id = store$selected_dataset_id
    )
    dashboard <- datasetDashboardServer(
      "dashboard",
      dataset = store$selected_dataset,
      active_panel = reactive(input$main_nav),
      color_mode = color_mode,
      snapshot_cache_max_bytes = min(
        as.numeric(runtime_profile$cache_max_bytes) * 0.4,
        384 * 1024^2
      ),
      snapshot_cache_max_entries = 3L
    )
    metadata <- metadata_explorer_server(
      "metadata",
      dataset = store$selected_dataset,
      focus_request = reactive(metadata_focus_request_value())
    )
    appended <- append_merge_server(
      "append",
      datasets = store$datasets,
      selected_dataset_id = store$selected_dataset_id
    )
    pending_name_retry <- reactiveVal(NULL)

    observeEvent(
      store$selected_dataset_id(),
      metadata_focus_request_value(NULL),
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )

    restore_manager_selection <- function() {
      manager$resync_selection()
      invisible(NULL)
    }

    open_metadata_context <- function(record, request = NULL) {
      if (is.null(record)) return(invisible(FALSE))
      if (is.null(request)) {
        request <- dashboard_metadata_identity_request(
          record,
          "dataset",
          label = paste("Metadata for", record$display_name)
        )
      }
      if (is.null(request)) {
        bundle <- metadata_bundle_for_record(record)
        if (nrow(bundle$index$groups) > 0L) {
          request <- new_metadata_focus_request(
            dataset_id = record$id,
            group_id = bundle$index$groups$group_id[[1L]],
            label = paste("Metadata for", record$display_name)
          )
        }
      }
      deliver_metadata_focus_request(metadata_focus_request_value, request)
      nav_select("main_nav", selected = "metadata")
      invisible(TRUE)
    }

    dispatch_safely <- function(event, success = NULL, on_success = NULL) {
      tryCatch(
        {
          store$dispatch(event)
          success_message <- if (is.function(success)) {
            success(event)
          } else {
            success
          }
          if (!is.null(success_message)) {
            showNotification(success_message, type = "message")
          }
          if (is.function(on_success)) on_success()
          TRUE
        },
        llw_dataset_name_conflict_error = function(cnd) {
          pending_name_retry(list(
            event = event,
            success = success,
            on_success = on_success,
            condition = cnd
          ))
          showModal(dataset_name_conflict_dialog(cnd))
          FALSE
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

    example_notification_id <- "llw-dataset-example-load"
    update_example_load_button <- function(active) {
      updateActionButton(
        session,
        "datasets-load_example",
        label = if (isTRUE(active)) {
          "Loading selected example..."
        } else {
          "Load selected example"
        },
        icon = if (isTRUE(active)) {
          icon("spinner", class = "fa-spin")
        } else {
          icon("flask")
        },
        disabled = isTRUE(active)
      )
    }
    example_loader <- new_dataset_example_loader(
      runtime = runtime,
      schedule = function(callback) {
        session$onFlushed(
          function() {
            shiny::withReactiveDomain(
              session,
              shiny::isolate(callback())
            )
          },
          once = TRUE
        )
      },
      on_busy = function(key) {
        update_example_load_button(TRUE)
        showNotification(
          paste(
            "Loading the selected ready-to-use example.",
            "Device files are being parsed and validated;",
            "the current dashboard remains available until loading succeeds."
          ),
          id = example_notification_id,
          type = "message",
          duration = NULL,
          closeButton = FALSE
        )
      },
      on_loaded = function(record, key, status) {
        update_example_load_button(FALSE)
        removeNotification(example_notification_id)
        dispatch_safely(
          new_session_event("add", value = record),
          success = function(event) {
            paste0(
              "Example `",
              event$value$display_name,
              "` loaded and selected."
            )
          },
          on_success = function() {
            nav_select("main_nav", selected = "dashboard")
          }
        )
      },
      on_failed = function(condition, key, status) {
        update_example_load_button(FALSE)
        mapped <- normalize_task_error(condition, "preparation")
        showNotification(
          paste(
            llw_public_message(mapped),
            "The previous dataset remains selected.",
            "Choose Load selected example to retry."
          ),
          id = example_notification_id,
          type = "error",
          duration = NULL,
          closeButton = TRUE
        )
      }
    )

    observe({
      context <- pending_name_retry()
      req(context, input$dataset_name_conflict_apply > 0)
      name <- trimws(input$dataset_name_conflict_value %||% "")
      if (!nzchar(name)) {
        showModal(dataset_name_conflict_dialog(
          context$condition,
          value = name,
          error = "Enter a non-empty dataset name."
        ))
        return()
      }
      retry_event <- tryCatch(
        session_event_with_display_name(context$event, name),
        error = identity
      )
      if (inherits(retry_event, "error")) {
        mapped <- normalize_task_error(retry_event, "preparation")
        showNotification(
          llw_public_message(mapped),
          type = "error",
          duration = 8
        )
        return()
      }
      if (
        dispatch_safely(
          retry_event,
          success = context$success,
          on_success = context$on_success
        )
      ) {
        pending_name_retry(NULL)
        removeModal()
      }
    }) |>
      bindEvent(input$dataset_name_conflict_apply, ignoreInit = TRUE)

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
      dispatch_safely(
        new_session_event("add", value = record),
        success = function(event) {
          paste0(
            "Dataset `",
            event$value$display_name,
            "` added to this session."
          )
        },
        on_success = function() {
          nav_select("main_nav", selected = "dashboard")
        }
      )
    }) |>
      bindEvent(imported$add_dataset(), ignoreInit = TRUE)

    observe({
      request <- glc_import$library_add_request()
      req(request)
      request_id <- request$request_id
      current <- glc_import$mark_library_add_progress(
        request_id,
        "preparing",
        "Preparing dataset...",
        paste(
          "The complete collected handoff and its provenance are being",
          "validated."
        )
      )
      if (!isTRUE(current)) return()
      session$onFlushed(
        function() {
          record <- tryCatch(
            {
              validate_glc_library_add_request(request)
              new_glc_dataset_record(request$full_import)
            },
            error = identity
          )
          if (inherits(record, "error")) {
            mapped <- normalize_task_error(record, "preparation")
            glc_import$mark_library_add_failed(
              request_id,
              llw_public_message(mapped)
            )
            showNotification(
              llw_public_message(mapped),
              type = "error",
              duration = NULL
            )
            return()
          }
          current <- glc_import$mark_library_add_progress(
            request_id,
            "adding",
            "Adding to session library...",
            "The validated dataset record is being added to this session."
          )
          if (!isTRUE(current)) return()
          session$onFlushed(
            function() {
              dispatch_safely(
                new_session_event("add", value = record),
                success = function(event) {
                  paste0(
                    "GLC dataset `",
                    event$value$display_name,
                    "` added with its normalized metadata and exact provenance."
                  )
                },
                on_success = function() {
                  glc_import$mark_library_add_progress(
                    request_id,
                    "opening",
                    "Opening dashboard...",
                    "The new session dataset is ready and its dashboard is opening."
                  )
                  glc_import$mark_library_added(request_id, record$id)
                  nav_select("main_nav", selected = "dashboard")
                }
              )
            },
            once = TRUE
          )
        },
        once = TRUE
      )
    }) |>
      bindEvent(glc_import$library_add_request(), ignoreInit = TRUE)

    observe({
      event <- manager$event()
      req(event)
      switch(
        event$type,
        open_import = {
          nav_select("main_nav", selected = "Import")
          imported$open_import()
        },
        load_example = {
          started <- example_loader$invoke(event$value)
          if (
            !isTRUE(started) &&
              isTRUE(isolate(example_loader$active()))
          ) {
            showNotification(
              paste(
                "The selected example is already loading.",
                "The current dataset remains available while it finishes."
              ),
              id = example_notification_id,
              type = "message",
              duration = NULL,
              closeButton = FALSE
            )
          }
        },
        open_append = {
          nav_select("main_nav", selected = "append")
          if (length(store$datasets()) < 2L) {
            showNotification(
              "Load or import at least two datasets before previewing an append.",
              type = "warning"
            )
          }
        },
        open_metadata = {
          target <- store$datasets()[[event$dataset_id]]
          if (is.null(target)) {
            showNotification(
              "The selected dataset is no longer available.",
              type = "error",
              duration = 8
            )
          } else {
            open_metadata_context(target)
          }
        },
        select = {
          target <- store$datasets()[[event$dataset_id]]
          if (is.null(target)) {
            restore_manager_selection()
            showNotification(
              "The selected dataset is no longer available.",
              type = "error",
              duration = 8
            )
          } else {
            started <- dashboard$request_switch(
              target,
              on_success = function() {
                dispatch_safely(new_session_event(
                  "select",
                  dataset_id = event$dataset_id
                ))
              },
              on_error = function(condition) restore_manager_selection()
            )
            if (!isTRUE(started)) {
              restore_manager_selection()
              if (isTRUE(isolate(dashboard$switching()))) {
                showNotification(
                  paste(
                    "A dataset switch is already in progress.",
                    "The current dashboard remains available."
                  ),
                  type = "message",
                  duration = 6
                )
              }
            }
          }
        },
        rename = dispatch_safely(
          new_session_event(
            "rename",
            dataset_id = event$dataset_id,
            value = event$value
          ),
          "Dataset display name updated."
        ),
        duplicate = {
          source <- session_dataset(store$model(), event$dataset_id)
          dispatch_safely(
            new_session_event(
              "add",
              value = duplicate_dataset_record(source, event$value)
            ),
            success = function(session_event) {
              paste0(
                "Created `",
                session_event$value$display_name,
                "` with a new stable ID."
              )
            }
          )
        },
        reset = dispatch_safely(
          new_session_event("reset", dataset_id = event$dataset_id),
          "Pre-processed data reset to the immutable source data."
        ),
        remove = dispatch_safely(
          new_session_event("remove", dataset_id = event$dataset_id),
          "Dataset removed from this session."
        )
      )
    }) |>
      bindEvent(manager$event(), ignoreInit = TRUE)

    observe({
      record <- appended$add_dataset()
      req(record)
      dispatch_safely(
        new_session_event("add", value = record),
        success = function(event) {
          paste0(
            "Appended dataset `",
            event$value$display_name,
            "` created; source datasets were left unchanged."
          )
        },
        on_success = function() {
          nav_select("main_nav", selected = "dashboard")
        }
      )
    }) |>
      bindEvent(appended$add_dataset(), ignoreInit = TRUE)

    observe({
      event <- dashboard$event()
      req(event)
      if (identical(event$type, "open_import")) {
        nav_select("main_nav", selected = "Import")
        imported$open_import()
      } else if (identical(event$type, "open_metadata")) {
        open_metadata_context(
          store$selected_dataset(),
          event$focus_request
        )
      }
    }) |>
      bindEvent(dashboard$event(), ignoreInit = TRUE)
  }

  on_start <- shiny_upload_limit_on_start(runtime_profile)

  shinyApp(ui = ui, server = server, onStart = on_start)
}
