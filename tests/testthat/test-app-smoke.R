test_that("the packaged application can be constructed", {
  app <- LightLogWeb(profile = "hosted", workers = 0)

  expect_s3_class(app, "shiny.appobj")
  expect_type(app$httpHandler, "closure")
  expect_type(app$serverFuncSource, "closure")
  expect_identical(
    names(formals(LightLogWeb)),
    c("profile", "max_upload_mb", "workers")
  )

  app_environment <- environment(app$serverFuncSource())
  production_html <- htmltools::renderTags(app_environment$ui)$html
  expect_identical(app_environment$import_presentation, "wizard")
  expect_match(production_html, "llw-import-wizard", fixed = TRUE)
  expect_match(production_html, "GLC Import", fixed = TRUE)
  expect_match(production_html, "Metadata", fixed = TRUE)
  expect_match(production_html, "Explore metadata", fixed = TRUE)
  expect_match(production_html, "glc_preview-wizard_stepper", fixed = TRUE)
  expect_match(production_html, "Append datasets safely", fixed = TRUE)
  expect_match(production_html, "Ready-to-use example", fixed = TRUE)
  expect_false(grepl("import_accordion", production_html, fixed = TRUE))
})

test_that("full app shares contextual authoring, library and dashboard without payload reloads", {
  skip_if_not(file.exists(file.path(
    m7_fixture_directory(),
    "tum-medi-only.rds"
  )))
  first <- m7_fixture_record()
  second <- m7_fixture_record("iztech-light-subset")
  app <- lightlogweb_app(
    profile = "hosted",
    workers = 0L,
    initial_records = list(first, second),
    registry_loader = function(...) stop("Offline fixture session")
  )
  html <- htmltools::renderTags(environment(app$serverFuncSource())$ui)$html
  expect_match(html, 'id="overlay-edit"', fixed = TRUE)
  expect_match(html, 'id="overlay-add_missing"', fixed = TRUE)
  expect_match(html, "Offline integration showcase", fixed = TRUE)
  test_environment <- environment()
  shiny::testServer(app, {
    session$setInputs(
      main_nav = "metadata",
      `datasets-dataset_select` = first$id
    )
    session$flushReact()
    before <- store$selected_dataset()
    context <- metadata_overlay_context(before)
    rows <- context$catalog
    target <- rows$target_id[
      rows$table == "variables" & rows$field == "label" & rows$name == "MEDI"
    ][[1L]]
    patch <- metadata_overlay_preview(
      before,
      context,
      target,
      "Integrated label"
    )
    summary_before <- output[["datasets-selected_summary"]]
    # Instantiate both real DT outputs before supplying their native view state.
    table_widget <- function(id) {
      jsonlite::fromJSON(
        output[[paste0("dashboard-", id)]],
        simplifyVector = FALSE
      )$x
    }
    raw_widget <- table_widget("raw_table")
    prepared_widget <- table_widget("prepared_table")
    table_state <- list(
      time = 1000,
      columns = vector("list", 3L),
      search = list(search = "S003"),
      order = list(list(2L, "desc")),
      length = 10L,
      start = 10L
    )
    session$setInputs(
      `dashboard-raw_table_state` = table_state,
      `dashboard-raw_table_rows_all` = seq_len(32L),
      `dashboard-prepared_table_state` = table_state,
      `dashboard-prepared_table_rows_all` = seq_len(32L)
    )
    builds <- dashboard$cache_stats()$builds
    id_notifications <- 0L
    tracker <- shiny::observe({
      store$selected_dataset_id()
      id_notifications <<- id_notifications + 1L
    })
    session$flushReact()
    original_unserialize <- base::unserialize
    payloads <- list(
      first$raw_payload,
      first$prepared_payload,
      second$raw_payload
    )
    testthat::local_mocked_bindings(
      unserialize = function(connection, ...) {
        if (
          is.raw(connection) &&
            any(vapply(payloads, identical, logical(1), connection))
        )
          stop("Main-app metadata edit decoded measurements")
        original_unserialize(connection, ...)
      },
      .package = "base",
      .env = test_environment
    )
    updated <- store$apply_overlay(context, patch)
    session$flushReact()
    expect_identical(store$selected_dataset()$id, first$id)
    expect_identical(store$datasets()[[second$id]], second)
    expect_identical(updated$raw_payload, before$raw_payload)
    expect_identical(
      updated$metadata_bundle$sources,
      before$metadata_bundle$sources
    )
    expect_identical(dashboard$cache_stats()$builds, builds)
    expect_match(
      table_widget("raw_table")$container,
      "Integrated label",
      fixed = TRUE
    )
    expect_identical(table_widget("raw_table")$options$search$search, "S003")
    expect_equal(table_widget("raw_table")$options$displayStart, 10L)
    expect_identical(
      table_widget("prepared_table")$options$search$search,
      "S003"
    )
    expect_match(
      paste(as.character(output[["datasets-selected_summary"]]), collapse = ""),
      "Integrated label",
      fixed = TRUE
    )
    expect_identical(id_notifications, 1L)
    expect_gt(nrow(editor$presentation()$entries), 0L)
    store$apply_overlay(context, undo = TRUE)
    session$flushReact()
    expect_identical(
      metadata_resolve_variable(store$selected_dataset(), "MEDI")$unit,
      "lx"
    )
    expect_identical(dashboard$cache_stats()$builds, builds)
    expect_identical(table_widget("raw_table")$options$search$search, "S003")
    expect_equal(table_widget("prepared_table")$options$displayStart, 10L)
    expect_identical(id_notifications, 1L)
    session$setInputs(
      main_nav = "dashboard",
      `dashboard-adjust_variable_metadata` = 1L
    )
    expect_true(editor$status()$editing)
    expect_identical(input$main_nav, "dashboard")
    session$setInputs(
      `overlay-focus_label` = "Dashboard label",
      `overlay-focus_unit` = "klx",
      `overlay-focus_save` = 1L
    )
    expect_false(editor$status()$editing)
    expect_identical(
      metadata_resolve_variable(store$selected_dataset(), "MEDI")$label,
      "Dashboard label"
    )
    expect_identical(
      metadata_resolve_variable(store$selected_dataset(), "MEDI")$unit,
      "klx"
    )
    expect_identical(dashboard$cache_stats()$builds, builds)
    expect_identical(table_widget("raw_table")$options$search$search, "S003")
    expect_equal(table_widget("prepared_table")$options$displayStart, 10L)
    expect_identical(store$datasets()[[second$id]], second)
    expect_identical(
      store$selected_dataset()$analysis_settings,
      before$analysis_settings
    )
    store$apply_overlay(context, undo = TRUE)
    session$flushReact()
    expect_identical(
      metadata_resolve_variable(store$selected_dataset(), "MEDI")$unit,
      "lx"
    )
    tracker$destroy()
  })
})

test_that("the small example links retained facts before editing and after no-op Save or Undo", {
  record <- sample_dataset_record()
  app <- lightlogweb_app(
    profile = "local",
    workers = 0L,
    initial_records = list(record),
    registry_loader = function(...) stop("Offline fixture session")
  )
  test_environment <- environment()
  shiny::testServer(app, {
    session$setInputs(
      main_nav = "dashboard",
      `datasets-dataset_select` = record$id
    )
    session$flushReact()
    before <- store$selected_dataset()
    builds <- dashboard$cache_stats()$builds
    original_unserialize <- base::unserialize
    testthat::local_mocked_bindings(
      unserialize = function(connection, ...) {
        if (is.raw(connection) && identical(connection, record$raw_payload))
          stop("Factual metadata navigation decoded measurements")
        original_unserialize(connection, ...)
      },
      .package = "base",
      .env = test_environment
    )
    session$setInputs(`dashboard-open_variable_metadata` = 1L)
    expect_identical(metadata$status()$external_focus$state, "resolved")
    expect_identical(metadata$selected_group()$entries$field, "label")
    expect_identical(
      metadata$selected_group()$entries$value,
      record$factual_metadata$variables$MEDI$label
    )
    expect_identical(metadata$hierarchy_context()$node$label, "MEDI")
    original_target <- metadata_resolve_variable(before, "MEDI")$target

    session$setInputs(
      main_nav = "dashboard",
      `dashboard-adjust_variable_metadata` = 1L
    )
    expect_true(editor$status()$editing)
    session$setInputs(
      `overlay-focus_label` = record$factual_metadata$variables$MEDI$label,
      `overlay-focus_unit` = "lux",
      `overlay-focus_save` = 1L
    )
    expect_false(editor$status()$editing)
    expect_identical(store$selected_dataset(), before)
    session$setInputs(`dashboard-open_variable_metadata` = 2L)
    expect_identical(metadata$status()$external_focus$state, "resolved")
    expect_identical(metadata$hierarchy_context()$node$label, "MEDI")
    expect_identical(
      metadata_resolve_variable(store$selected_dataset(), "MEDI")$target,
      original_target
    )

    session$setInputs(
      main_nav = "dashboard",
      `dashboard-adjust_variable_metadata` = 2L
    )
    session$setInputs(
      `overlay-focus_label` = "Adjusted example",
      `overlay-focus_save` = 2L
    )
    expect_identical(
      store$selected_dataset()$metadata_bundle$overlay$revision,
      1L
    )
    session$setInputs(`dashboard-open_variable_metadata` = 3L)
    expect_identical(metadata$status()$external_focus$state, "resolved")
    expect_identical(
      metadata$selected_group()$entries$layer,
      "LightLogWeb overlay"
    )
    session$setInputs(`overlay-undo` = 1L)
    expect_null(metadata$status()$external_focus)
    session$setInputs(`dashboard-open_variable_metadata` = 4L)
    expect_identical(metadata$status()$external_focus$state, "resolved")
    expect_identical(
      metadata_resolve_variable(store$selected_dataset(), "MEDI")$target,
      original_target
    )
    expect_identical(store$selected_dataset()$raw_payload, before$raw_payload)
    expect_identical(
      store$selected_dataset()$metadata_bundle$sources,
      before$metadata_bundle$sources
    )
    expect_identical(dashboard$cache_stats()$builds, builds)
  })
})

test_that("the app clears obsolete crosslink feedback after last-value Undo and clear", {
  record <- metadata_explorer_showcase_records()[[3L]]
  app <- lightlogweb_app(
    profile = "local",
    workers = 0L,
    initial_records = list(record),
    registry_loader = function(...) stop("Offline fixture session")
  )
  test_environment <- environment()
  shiny::testServer(app, {
    session$setInputs(
      main_nav = "dashboard",
      `datasets-dataset_select` = record$id
    )
    session$flushReact()
    before <- store$selected_dataset()
    builds <- dashboard$cache_stats()$builds
    original_unserialize <- base::unserialize
    testthat::local_mocked_bindings(
      unserialize = function(connection, ...) {
        if (is.raw(connection) && identical(connection, record$raw_payload))
          stop("Metadata feedback update decoded measurements")
        original_unserialize(connection, ...)
      },
      .package = "base",
      .env = test_environment
    )
    for (action in c("undo", "clear")) {
      step <- if (action == "undo") 1L else 2L
      session$setInputs(`dashboard-adjust_variable_metadata` = step)
      session$setInputs(
        `overlay-focus_label` = "Disposable label",
        `overlay-focus_unit` = "",
        `overlay-focus_save` = step
      )
      session$setInputs(`dashboard-open_variable_metadata` = step)
      expect_identical(metadata$status()$external_focus$state, "resolved")
      expect_match(
        output[["metadata-focus_status"]]$html,
        "exact retained field",
        fixed = TRUE
      )
      if (action == "undo") {
        session$setInputs(`overlay-undo` = 1L)
      } else {
        session$setInputs(
          main_nav = "dashboard",
          `dashboard-adjust_variable_metadata` = 3L
        )
        session$setInputs(`overlay-focus_label` = "", `overlay-focus_save` = 3L)
      }
      session$flushReact()
      expect_null(metadata$status()$external_focus)
      expect_null(output[["metadata-focus_status"]])
      expect_null(metadata$status()$selected_group_id)
      expect_identical(
        nrow(store$selected_dataset()$metadata_bundle$overlay$tables$variables),
        0L
      )
      variable <- metadata_resolve_variable(store$selected_dataset(), "MEDI")
      expect_null(variable$target)
      expect_identical(variable$label, "MEDI")
      expect_identical(variable$unit, "unit not specified")
      expect_identical(dashboard$cache_stats()$builds, builds)
      expect_identical(store$selected_dataset()$raw_payload, before$raw_payload)
      expect_identical(
        store$selected_dataset()$metadata_bundle$sources,
        before$metadata_bundle$sources
      )
    }
  })
})

test_that("identical metadata focus requests are delivered repeatedly", {
  request <- new_metadata_focus_request(
    dataset_id = "dataset_m7iztechlightsubset",
    namespace_id = "dataset_m7iztechlightsubset",
    field = "participant_internal_id",
    path_contains = "/core_metadata/participants",
    record_identity = "IZTECH_S004",
    label = "Participant IZTECH_S004"
  )

  bridge_server <- function(input, output, session) {
    focus_value <- shiny::reactiveVal(NULL)
    hierarchy_focus <- shiny::reactiveVal(NULL)
    activation_ids <- shiny::reactiveVal(character())

    shiny::observeEvent(
      focus_value(),
      {
        current <- focus_value()
        activation_ids(c(activation_ids(), current$request_id))
        hierarchy_focus(current$record_identity)
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    session$userData$deliver <- function(value) {
      deliver_metadata_focus_request(focus_value, value)
    }
    session$userData$focus_value <- focus_value
    session$userData$hierarchy_focus <- hierarchy_focus
    session$userData$activation_ids <- activation_ids
  }

  shiny::testServer(bridge_server, {
    session$flushReact()
    session$userData$deliver(request)
    session$flushReact()
    expect_identical(
      session$userData$hierarchy_focus(),
      "IZTECH_S004"
    )
    expect_identical(session$userData$activation_ids(), request$request_id)

    session$userData$hierarchy_focus("MELIDOS_IZTECH_S004:20")
    session$userData$deliver(request)
    session$flushReact()

    expect_identical(
      session$userData$hierarchy_focus(),
      "IZTECH_S004"
    )
    expect_identical(
      session$userData$activation_ids(),
      rep(request$request_id, 2L)
    )
    expect_identical(
      session$userData$focus_value()$request_id,
      request$request_id
    )
  })
})

test_that("every changed module retains a constructible development app", {
  apps <- list(
    core_architecture_app(),
    dataset_manager_app(),
    dataset_dashboard_app(),
    append_merge_app(),
    metadata_explorer_app(),
    import_app(),
    import_wizard_app()
  )

  expect_true(all(vapply(apps, inherits, logical(1), "shiny.appobj")))
  expect_identical(formals(import_app)$workers, 1)
})

test_that("upload apps apply their configured request limit when they start", {
  main_app <- LightLogWeb(
    profile = "hosted",
    max_upload_mb = 16,
    workers = 0
  )
  showcase <- import_app(max_upload_mb = 16)

  expect_type(main_app$onStart, "closure")
  expect_type(showcase$onStart, "closure")
  expect_equal(
    environment(main_app$onStart)$max_upload_bytes,
    16 * 1024^2
  )
  expect_equal(
    environment(showcase$onStart)$max_upload_bytes,
    16 * 1024^2
  )
})

test_that("dataset example task payloads are exact and serializable", {
  project_root <- lightlogweb_development_root()
  payload <- new_dataset_example_task_payload(
    "sample",
    project_root = project_root
  )

  expect_identical(
    names(payload),
    c("schema_version", "key", "project_root")
  )
  expect_identical(payload$schema_version, "1.0.0")
  expect_identical(payload$key, "sample")
  expect_identical(
    payload$project_root,
    normalizePath(
      project_root,
      winslash = "/",
      mustWork = TRUE
    )
  )
  expect_identical(validate_dataset_example_task_payload(payload), payload)
  expect_error(
    validate_dataset_example_task_payload(payload[-1L]),
    class = "llw_validation_error",
    regexp = "malformed or unsupported"
  )

  record <- dataset_example_load_task_worker(payload)
  expect_s3_class(record, "llw_dataset_record")
  expect_identical(record$display_name, "sample.data.environment")
  expect_silent(assert_serializable_value(record, "example record"))
})

test_that("dataset example loading is staged, recoverable, and session local", {
  loader_server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 0),
      session = session
    )
    state <- new.env(parent = emptyenv())
    state$scheduled <- list()
    state$events <- character()
    state$selected <- "existing_dataset"
    state$loaded <- character()
    state$errors <- character()

    schedule <- function(callback) {
      state$scheduled[[length(state$scheduled) + 1L]] <- function() {
        shiny::withReactiveDomain(
          session,
          shiny::isolate(callback())
        )
      }
    }
    run_next <- function() {
      callback <- state$scheduled[[1L]]
      state$scheduled <- state$scheduled[-1L]
      callback()
    }
    worker <- function(payload, spec) {
      if (identical(payload$key, "broken_example")) {
        stop("private fixture failure", call. = FALSE)
      }
      list(
        id = paste0("dataset_", payload$key),
        display_name = payload$key
      )
    }

    loader <- new_dataset_example_loader(
      runtime = runtime,
      schedule = schedule,
      on_busy = function(key) {
        state$events <- c(state$events, paste0("busy:", key))
      },
      on_loaded = function(record, key, status) {
        state$events <- c(state$events, paste0("loaded:", key))
        state$loaded <- c(state$loaded, record$id)
        state$selected <- record$id
      },
      on_failed = function(condition, key, status) {
        state$events <- c(state$events, paste0("failed:", key))
        state$errors <- c(state$errors, llw_public_message(condition))
      },
      worker = worker
    )

    session$userData$runtime <- runtime
    session$userData$state <- state
    session$userData$loader <- loader
    session$userData$run_next <- run_next
  }

  shiny::testServer(loader_server, {
    loader <- session$userData$loader
    state <- session$userData$state
    root <- lightlogweb_development_root()

    expect_identical(state$selected, "existing_dataset")
    expect_false(isolate(loader$active()))
    expect_true(loader$invoke("slow_example", project_root = root))
    expect_true(isolate(loader$active()))
    expect_identical(isolate(loader$state()), "queued")
    expect_identical(state$events, "busy:slow_example")
    expect_identical(state$selected, "existing_dataset")
    expect_length(state$scheduled, 1L)

    expect_false(loader$invoke("slow_example", project_root = root))
    expect_length(state$scheduled, 1L)
    expect_identical(state$events, "busy:slow_example")

    session$userData$run_next()
    flush_m1_promises(session)
    expect_identical(isolate(loader$state()), "complete")
    expect_false(isolate(loader$active()))
    expect_identical(
      state$events,
      c("busy:slow_example", "loaded:slow_example")
    )
    expect_identical(state$selected, "dataset_slow_example")

    expect_true(loader$invoke("broken_example", project_root = root))
    expect_identical(state$selected, "dataset_slow_example")
    session$userData$run_next()
    suppressWarnings(flush_m1_promises(session))
    expect_identical(isolate(loader$state()), "error")
    expect_identical(state$selected, "dataset_slow_example")
    expect_match(tail(state$events, 1L), "failed:broken_example")
    expect_match(tail(state$errors, 1L), "remain available")
    expect_false(grepl(
      "private fixture failure",
      tail(state$errors, 1L),
      fixed = TRUE
    ))

    expect_true(loader$invoke("retry_example", project_root = root))
    session$userData$run_next()
    flush_m1_promises(session)
    expect_identical(isolate(loader$state()), "complete")
    expect_identical(state$selected, "dataset_retry_example")
    expect_identical(tail(state$loaded, 1L), "dataset_retry_example")

    expect_true(loader$invoke("retry_example", project_root = root))
    expect_false(loader$invoke("retry_example", project_root = root))
    session$userData$run_next()
    flush_m1_promises(session)
    expect_identical(
      sum(state$events == "busy:retry_example"),
      2L
    )
  })

  shiny::testServer(loader_server, {
    expect_false(isolate(session$userData$loader$active()))
    expect_identical(isolate(session$userData$loader$state()), "idle")
    expect_identical(
      session$userData$state$selected,
      "existing_dataset"
    )
    expect_length(session$userData$state$events, 0L)
  })
})
