test_that("the common task layer covers every Milestone 1 workload", {
  expect_setequal(
    long_task_types(),
    c(
      "raw_import",
      "glc_discovery",
      "glc_read",
      "glc_download",
      "preparation",
      "append_merge",
      "metrics",
      "report"
    )
  )
  expect_setequal(
    task_states(),
    c(
      "idle",
      "queued",
      "running",
      "finalizing",
      "complete",
      "warning",
      "error",
      "cancelled",
      "stale"
    )
  )
})

test_that("task state transitions reject impossible lifecycle changes", {
  status <- new_task_status("preparation")
  status <- transition_task_status(status, "queued")
  status <- transition_task_status(status, "running")
  status <- transition_task_status(status, "finalizing")
  status <- transition_task_status(status, "complete")

  expect_identical(status$state, "complete")
  expect_error(
    transition_task_status(status, "running"),
    class = "llw_validation_error",
    regexp = "Invalid task transition"
  )
})

test_that("stale results never call their apply callback", {
  applied <- FALSE
  spec <- new_task_spec(
    "preparation",
    payload = list(rows = 1L),
    dataset_id = "dataset_fixture",
    dataset_revision = 2L
  )
  outcome <- finalize_task_result(
    spec,
    new_task_result(value = "old result"),
    current_revision = 3L,
    on_result = function(value, spec) applied <<- TRUE
  )

  expect_identical(outcome$state, "stale")
  expect_false(outcome$applied)
  expect_false(applied)
})

test_that("a failed ExtendedTask can be retried in the same session", {
  server <- function(input, output, session) {
    profile <- resolve_runtime_profile(
      "hosted",
      workers = 0,
      is_interactive = FALSE,
      available_cores = 2
    )
    runtime <- new_session_runtime(profile, session = session)
    worker <- function(payload, spec) {
      if (isTRUE(payload$fail)) {
        stop("private worker detail", call. = FALSE)
      }
      payload$value
    }
    session$userData$task <- new_long_task(
      worker = worker,
      task_type = "preparation",
      runtime = runtime
    )
  }

  shiny::testServer(server, {
    task <- session$userData$task
    task$invoke(list(fail = TRUE))
    flush_m1_promises(session)
    expect_identical(task$state(), "error")
    expect_s3_class(task$error(), "llw_preparation_error")
    expect_match(llw_public_message(task$error()), "remain available")

    task$invoke(list(fail = FALSE, value = 42L))
    flush_m1_promises(session)
    expect_identical(task$state(), "complete")
    expect_identical(task$result(), 42L)
    expect_null(task$error())
  })
})

test_that("finalization failures release the slot and permit a clean retry", {
  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 0),
      session = session
    )
    fail_finalize <- shiny::reactiveVal(TRUE)
    applied <- shiny::reactiveVal(0L)
    task <- new_long_task(
      worker = function(payload, spec) payload,
      task_type = "preparation",
      runtime = runtime,
      on_result = function(value, spec) {
        if (fail_finalize()) {
          stop("private finalization detail", call. = FALSE)
        }
        applied(applied() + 1L)
      }
    )
    session$userData$runtime <- runtime
    session$userData$task <- task
    session$userData$fail_finalize <- fail_finalize
    session$userData$applied <- applied
  }

  shiny::testServer(server, {
    task <- session$userData$task
    task$invoke(list(value = 1L))
    flush_m1_promises(session)

    expect_identical(task$state(), "error")
    expect_equal(session$userData$runtime$active_count(), 0L)
    expect_false(grepl(
      "private finalization detail",
      llw_public_message(task$error()),
      fixed = TRUE
    ))

    session$userData$fail_finalize(FALSE)
    task$invoke(list(value = 2L))
    flush_m1_promises(session)

    expect_identical(task$state(), "complete")
    expect_identical(task$result(), list(value = 2L))
    expect_identical(session$userData$applied(), 1L)
    expect_equal(session$userData$runtime$active_count(), 0L)
  })
})

test_that("a running task can be cancelled and then retried", {
  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 0),
      session = session
    )
    session$userData$runtime <- runtime
    session$userData$task <- new_long_task(
      worker = function(payload, spec) payload,
      task_type = "preparation",
      runtime = runtime
    )
  }

  shiny::testServer(server, {
    task <- session$userData$task
    task$invoke(list(value = "cancelled"))
    expect_identical(task$state(), "running")
    expect_true(task$cancel())
    expect_identical(task$state(), "cancelled")
    flush_m1_promises(session)
    expect_equal(session$userData$runtime$active_count(), 0L)

    task$invoke(list(value = "retry"))
    flush_m1_promises(session)
    expect_identical(task$state(), "complete")
    expect_identical(task$result(), list(value = "retry"))
  })
})

test_that("the ExtendedTask controller discards a stale worker result", {
  server <- function(input, output, session) {
    profile <- resolve_runtime_profile("hosted", workers = 0)
    runtime <- new_session_runtime(profile, session = session)
    revision <- shiny::reactiveVal(0L)
    applied <- shiny::reactiveVal(FALSE)
    task <- new_long_task(
      worker = function(payload, spec) payload,
      task_type = "preparation",
      runtime = runtime,
      revision_lookup = function(dataset_id) revision(),
      on_result = function(value, spec) applied(TRUE)
    )
    session$userData$task <- task
    session$userData$revision <- revision
    session$userData$applied <- applied
  }

  shiny::testServer(server, {
    task <- session$userData$task
    task$invoke(list(value = "old"), dataset_id = "dataset_fixture")
    session$userData$revision(1L)
    flush_m1_promises(session)

    expect_identical(task$state(), "stale")
    expect_false(session$userData$applied())
    expect_null(task$result())
  })
})

test_that("queued tasks can be cancelled without consuming the hosted slot", {
  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 0),
      session = session
    )
    session$userData$runtime <- runtime
    session$userData$first <- new_long_task(
      worker = function(payload, spec) payload,
      task_type = "preparation",
      runtime = runtime
    )
    session$userData$second <- new_long_task(
      worker = function(payload, spec) payload,
      task_type = "metrics",
      runtime = runtime
    )
  }

  shiny::testServer(server, {
    first <- session$userData$first
    second <- session$userData$second
    first$invoke(list(value = "first"))
    second$invoke(list(value = "second"))

    expect_identical(first$state(), "running")
    expect_identical(second$state(), "queued")
    expect_equal(session$userData$runtime$queued_count(), 1L)
    expect_true(second$cancel())
    expect_identical(second$state(), "cancelled")
    expect_equal(session$userData$runtime$queued_count(), 0L)

    flush_m1_promises(session)
    expect_identical(first$state(), "complete")
    expect_identical(first$result(), list(value = "first"))
    expect_equal(session$userData$runtime$active_count(), 0L)
  })
})

test_that("invalid task payloads become recoverable task errors", {
  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 0),
      session = session
    )
    session$userData$task <- new_long_task(
      worker = function(payload, spec) payload,
      task_type = "report",
      runtime = runtime
    )
  }

  shiny::testServer(server, {
    task <- session$userData$task
    expect_false(task$invoke(new.env(parent = emptyenv())))
    expect_identical(task$state(), "error")
    expect_s3_class(task$error(), "llw_validation_error")

    task$invoke(list(value = 7L))
    flush_m1_promises(session)
    expect_identical(task$state(), "complete")
    expect_identical(task$result(), list(value = 7L))
  })
})

test_that("the asynchronous profile executes serializable work in a mirai daemon", {
  skip_if_not_installed("mirai")
  main_pid <- Sys.getpid()
  startup_error <- NULL
  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 1),
      session = session
    )
    startup_error <<- runtime$startup_error
    session$userData$task <- new_long_task(
      worker = function(payload, spec) {
        if (isTRUE(payload$warn)) {
          warning("daemon warning", call. = FALSE)
        }
        list(value = payload$value, pid = Sys.getpid())
      },
      task_type = "preparation",
      runtime = runtime
    )
  }

  shiny::testServer(server, {
    if (inherits(startup_error, "llw_error")) {
      skip("mirai daemons are unavailable in this test runtime")
    }
    task <- session$userData$task
    task$invoke(list(value = 11L))
    flush_m1_promises(session, times = 200L)

    expect_identical(task$state(), "complete")
    expect_identical(task$result()$value, 11L)
    expect_false(identical(task$result()$pid, main_pid))

    task$invoke(list(value = 12L, warn = TRUE))
    flush_m1_promises(session, times = 200L)
    expect_identical(task$state(), "warning")
    expect_identical(task$status()$warnings, "daemon warning")
  })
})
