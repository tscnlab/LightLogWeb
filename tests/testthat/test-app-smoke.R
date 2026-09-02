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
