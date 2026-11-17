test_that("Prepare saves previews applies and recovers without changing canonical source", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(duration_length = 1L)
      expect_false(result$dirty())
      session$setInputs(step_type = "clock")
      session$setInputs(save_step = 1L)
      expect_length(result$recipe()$steps, 1L)
      expect_false(result$dirty())
      session$setInputs(preview = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      before <- result$preview()$comparison$before$rows
      after <- result$preview()$comparison$after$rows
      expect_lt(after, before)
      session$setInputs(apply = 1L)
      expect_equal(record()$revision, 1L)
      expect_match(result$notice(), "original source")
      m8_wait(
        session,
        function() record()$revision == 1L && !is.null(result$context())
      )
      expect_identical(record()$raw_payload, original$raw_payload)
      expect_equal(nrow(unserialize(record()$prepared_payload)), after)
      expect_equal(result$context()$prepared_rows, after)
      expect_equal(result$context()$rows, before)
      session$setInputs(apply = 3L)
      expect_equal(record()$revision, 1L)
      expect_match(result$notice(), "already applied")
      session$setInputs(undo = 1L)
      m8_wait(
        session,
        function() record()$revision == 2L && !is.null(result$context())
      )
      expect_identical(record()$prepared_payload, original$raw_payload)
      expect_length(record()$recipe$steps, 0L)
    }
  )
})

test_that("unsaved edits and source changes cannot publish a stale preview", {
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(preview = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      session$setInputs(step_type = "clock", clock_start = "bad")
      expect_null(result$preview())
      session$setInputs(save_step = 1L)
      expect_match(result$notice(), "clock times")
      expect_match(output$action_status$html, "Enter clock times")
      expect_match(output$action_status$html, 'role="alert"')
      expect_match(output$action_status$html, "Check the settings")
      expect_length(result$recipe()$steps, 0L)
      session$setInputs(preview = 2L)
      expect_match(result$notice(), "clock times")
      expect_match(
        output$preview_notice$html,
        "save this step and update the preview"
      )
      next_record <- m1_showcase_record()
      record(next_record)
      session$flushReact()
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, next_record$id)
      )
      expect_length(result$recipe()$steps, 0L)
      expect_null(result$preview())
      expect_identical(record()$raw_payload, next_record$raw_payload)
    }
  )
})

test_that("all-removed results and no-history Undo give recoverable feedback", {
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(undo = 1L)
      expect_match(result$notice(), "no applied change")
      session$setInputs(
        step_type = "clock",
        clock_start = "23:51:00",
        clock_end = "23:52:00"
      )
      session$setInputs(save_step = 1L, preview = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$preview()$comparison$after$rows, 0L)
      session$setInputs(ack_loss = TRUE, apply = 1L)
      expect_equal(record()$revision, 0L)
      expect_match(result$notice(), "No rows remain")
      expect_match(output$action_status$html, "No rows remain")

      empty <- new_dataset_record(
        m1_fixture_data()[FALSE, ],
        "Empty source",
        m1_source_manifest()
      )
      record(empty)
      session$flushReact()
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, empty$id)
      )
      m8_set_form(session, result$context())
      session$setInputs(preview = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_match(output$action_status$html, "source has no observations")
      expect_false(grepl("disable or remove", output$action_status$html))
      session$setInputs(apply = 2L)
      expect_match(output$action_status$html, "Choose a dataset")
      expect_identical(record(), empty)
      session$setInputs(cancel = 1L)
      expect_match(output$action_status$html, "No preparation is running")
    }
  )
})

test_that("the retained Prepare showcase and module contract construct", {
  expect_s3_class(prepare_app(workers = 0L), "shiny.appobj")
  expect_error(prepare_server("invalid", list(), list(), identity), "reactive")
  html <- htmltools::renderTags(prepare_ui("a"))$html
  expect_match(html, "a-save_step", fixed = TRUE)
  expect_match(html, "a-action_status", fixed = TRUE)
  expect_match(html, "first 100 rows", fixed = TRUE)
})

test_that("cleared date recovery preserves the draft and endpoint choices", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      m8_set_form(session, result$context(), "dates", date_start = "")
      session$setInputs(save_step = 1L)
      expect_match(
        output$editor_error,
        "Choose the first date.*2026-03-28 to 2026-03-31.*Europe/Berlin"
      )
      expect_length(result$recipe()$steps, 0L)
      session$setInputs(date_start = "2026-03-29", save_step = 2L)
      expect_false(result$dirty())
      saved <- result$recipe()
      session$setInputs(preview = 1L)
      m8_wait(session, function() !is.null(result$preview()))

      m8_set_form(
        session,
        result$context(),
        "datetimes",
        instant_start_time = "10:00",
        instant_end_date = "",
        instant_end_time = "02:00",
        instant_end_zone = "custom",
        instant_end_timezone = "America/New_York"
      )
      session$setInputs(save_step = 3L)
      expect_match(
        output$editor_error,
        "Choose the end date.*2026-03-27 to 2026-03-30.*America/New_York"
      )
      expect_identical(result$recipe(), saved)
      expect_null(result$preview())
      expect_identical(record(), original)
      session$setInputs(instant_end_date = "2026-03-30", save_step = 4L)
      expect_false(result$dirty())
      args <- result$recipe()$steps[[1L]]$parameters$arguments
      expect_identical(args$start, "2026-03-28 10:00:00 +0100")
      expect_identical(args$end, "2026-03-30 02:00:00 -0400")
      expect_identical(args$start_timezone, "native")
      expect_identical(args$end_timezone, "America/New_York")
      session$setInputs(preview = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(record(), original)
    }
  )
})

test_that("bounded date and duration edits invalidate the preview and preserve the source", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      m8_set_form(
        session,
        result$context(),
        "dates",
        date_start = "2040-01-01",
        date_end = "2040-01-01"
      )
      session$setInputs(save_step = 1L)
      expect_match(result$notice(), "available span")
      expect_match(output$editor_error, "available span")
      expect_length(result$recipe()$steps, 0L)
      m8_set_form(
        session,
        result$context(),
        "duration",
        duration_length = 12,
        duration_unit = "hours",
        duration_scope = "identity"
      )
      session$setInputs(save_step = 2L, preview = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$preview()$comparison$after$rows, 72L)
      session$setInputs(duration_from = "end")
      expect_null(result$preview())
      session$setInputs(save_step = 3L, preview = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$preview()$comparison$after$rows, 75L)
      session$setInputs(ack_loss = TRUE, apply = 1L)
      m8_wait(
        session,
        function() record()$revision == 1L && !is.null(result$context())
      )
      expect_identical(record()$raw_payload, original$raw_payload)
      expect_identical(
        record()$recipe$steps[[1L]]$parameters$arguments$from,
        "end"
      )
      session$setInputs(undo = 1L)
      m8_wait(
        session,
        function() record()$revision == 2L && !is.null(result$context())
      )
      expect_identical(record()$prepared_payload, original$raw_payload)
    }
  )
})

test_that("large previews require resource acknowledgement before starting", {
  record <- shiny::reactiveVal(m8_record())
  profile <- resolve_runtime_profile("hosted", workers = 0L)
  profile$cache_max_bytes <- 100 * 1024^2
  runtime <- new_session_runtime(profile, start_workers = FALSE)
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(preview = 1L)
      expect_match(result$notice(), "acknowledge")
      expect_match(output$action_status$html, "acknowledge it before Preview")
      expect_match(
        output$preview_notice$html,
        "select the acknowledgement above"
      )
      expect_null(result$preview())
      expect_false(result$pending())
      expect_equal(record()$revision, 0L)
      session$setInputs(ack_cost = TRUE, preview = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$preview()$comparison$removed_rows, 0L)
    }
  )
})

test_that("preflight refusal is not a loading state and smaller input recovers", {
  initial <- m8_record()
  record <- shiny::reactiveVal(initial)
  profile <- resolve_runtime_profile("hosted", workers = 0L)
  profile$cache_max_bytes <- preparation_memory_estimate(length(
    initial$raw_payload
  )) -
    1
  runtime <- new_session_runtime(profile, start_workers = FALSE)
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      session$flushReact()
      expect_match(result$notice(), "exceeds")
      expect_match(output$action_status$html, "exceeds")
      expect_identical(output$form_loading, "no")
      session$setInputs(preview = 1L)
      expect_match(result$notice(), "exceeds")
      expect_match(output$action_status$html, "exceeds")
      expect_null(output$preview_notice)
      expect_identical(record(), initial)
      smaller <- m1_showcase_record()
      record(smaller)
      session$flushReact()
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, smaller$id)
      )
      expect_match(result$notice(), "Ready")
    }
  )
})

test_that("real Prepare workers cancel drain retry and reject changed-input completions", {
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(resolve_runtime_profile(
    "hosted",
    workers = 1L
  ))
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error"))
    skip("mirai worker sockets are unavailable")
  worker <- function(payload, spec) {
    if (payload$action == "preview") Sys.sleep(0.7)
    LightLogWeb:::preparation_task_worker(payload, spec)
  }
  environment(worker) <- baseenv()
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record),
      worker = worker
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(preview = 1L)
      m8_wait(session, function() result$status()$state == "running")
      expect_match(output$action_status$html, "Preparation running")
      expect_null(output$preview_notice)
      session$setInputs(preview = 2L)
      expect_match(result$notice(), "already running")
      session$setInputs(cancel = 1L)
      expect_identical(result$status()$state, "cancelled")
      session$setInputs(preview = 3L)
      expect_true(result$pending() || result$active())
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(
        result$preview()$candidate$raw_payload,
        record()$raw_payload
      )
      session$setInputs(preview = 4L)
      m8_wait(session, function() result$status()$state == "running")
      session$setInputs(cancel = 2L)
      if (result$active()) {
        expect_match(output$action_status$html, "draining")
        expect_false(grepl("available again", output$action_status$html))
      }
      m8_wait(session, function() !result$active() && !result$pending())
      expect_match(output$action_status$html, "Preparation cancelled")
      expect_match(output$action_status$html, "Preview is available again")
      expect_equal(runtime$active_count(), 0L)
      expect_false(grepl("requested|draining", output$action_status$html))
      expect_identical(output$action_status$html, output$status_text$html)
      expect_null(result$preview())
      expect_identical(record()$prepared_payload, record()$raw_payload)
      session$setInputs(preview = 5L)
      m8_wait(session, function() result$status()$state == "running")
      session$setInputs(step_type = "clock", clock_start = "21:00:00")
      m8_wait(session, function() result$status()$state == "stale")
      expect_null(result$preview())
      expect_equal(record()$revision, 0L)
      session$setInputs(save_step = 1L)
      expect_match(result$notice(), "Queued: preview")
      session$setInputs(preview = 6L)
      m8_wait(session, function() result$status()$state == "running")
      next_record <- m1_showcase_record()
      record(next_record)
      session$flushReact()
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, next_record$id)
      )
      expect_null(result$preview())
      expect_identical(record()$prepared_payload, next_record$raw_payload)
      expect_equal(runtime$active_count(), 0L)
    }
  )
})

test_that("a failed worker retains applied state and the same session can retry", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  attempts <- 0L
  worker <- function(payload, spec) {
    if (payload$action == "preview") {
      attempts <<- attempts + 1L
      if (attempts == 1L)
        abort_llw("Synthetic recoverable preparation failure.", "preparation")
    }
    preparation_task_worker(payload, spec)
  }
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record),
      worker = worker
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(preview = 1L)
      expect_warning(
        m8_wait(session, function() result$status()$state == "error"),
        "ExtendedTask"
      )
      expect_identical(record(), original)
      expect_null(result$preview())
      expect_match(
        output$action_status$html,
        "Synthetic recoverable preparation failure"
      )
      expect_match(output$action_status$html, "Preview failed")
      expect_match(output$action_status$html, 'role="alert"')
      expect_match(output$action_status$html, "Applied data are unchanged")
      expect_match(output$action_status$html, "choose Preview recipe to retry")
      expect_null(output$preview_notice)
      session$setInputs(preview = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_false(grepl("Preview failed|is-error", output$action_status$html))
      expect_match(
        output$action_status$html,
        'aria-live="polite"',
        fixed = TRUE
      )
      expect_identical(
        result$preview()$candidate$raw_payload,
        original$raw_payload
      )
    }
  )
})

test_that("saved edits auto-preview and repeated Apply rebuilds one recipe from source", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(
        step_type = "time",
        time_mode = "clock",
        clock_start = "22:00:00",
        clock_end = "02:00:00",
        save_new = 1L
      )
      m8_wait(session, function() !is.null(result$preview()))
      first_rows <- result$preview()$comparison$after$rows
      id <- result$recipe()$steps[[1]]$id
      session$setInputs(apply = 1L)
      expect_equal(record()$revision, 1L)
      expect_length(result$recipe()$steps, 1L)
      expect_false(is.null(result$preview()))
      session$setInputs(clock_end = "06:00:00", save_step = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_gt(result$preview()$comparison$after$rows, first_rows)
      expect_equal(result$recipe()$steps[[1]]$id, id)
      session$setInputs(apply = 2L)
      expect_equal(record()$revision, 2L)
      raw <- unserialize(original$raw_payload)
      clock <- format(raw$Datetime, "%H:%M:%S", tz = "Europe/Berlin")
      expect_identical(
        unserialize(record()$prepared_payload),
        dplyr::filter(raw, clock >= "22:00:00" | clock < "06:00:00")
      )
      expect_identical(record()$raw_payload, original$raw_payload)
      session$setInputs(save_new = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_length(result$recipe()$steps, 2L)
      expect_false(identical(result$recipe()$steps[[2]]$id, id))
      session$setInputs(undo_edit = 1L)
      m8_set_form(session, result$context())
      m8_wait(session, function() !is.null(result$preview()))
      expect_length(result$recipe()$steps, 1L)
      expect_equal(record()$revision, 2L)
      session$setInputs(reset = 1L)
      m8_wait(
        session,
        function() record()$revision == 3L && !is.null(result$context())
      )
      expect_identical(record()$prepared_payload, original$raw_payload)
      expect_length(result$recipe()$steps, 0L)
    }
  )
})

test_that("removed applied steps remain visible until Apply and recover through Undo or Reset", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(step_type = "clock")
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      applied_id <- result$recipe()$steps[[1L]]$id
      session$setInputs(apply = 1L)
      applied <- record()
      expect_null(output$pending_removals)

      session$setInputs(selected_step = applied_id, remove = 1L)
      m8_set_form(session, result$context())
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(record(), applied)
      expect_length(result$recipe()$steps, 0L)
      expect_identical(
        result$preview()$candidate$prepared_payload,
        original$raw_payload
      )
      expect_match(output$pending_removals$html, "Removal pending")
      expect_match(
        output$pending_removals$html,
        "still affect the prepared data until you Apply"
      )
      expect_match(output$pending_removals$html, "22:00:00 to 06:00:00")
      expect_match(output$recipe_list$html, "1 removal\\(s\\) pending")
      session$setInputs(undo_edit = 1L)
      m8_set_form(session, result$context())
      m8_wait(session, function() !is.null(result$preview()))
      expect_null(output$pending_removals)
      expect_identical(result$recipe(), applied$recipe)
      expect_identical(record(), applied)

      # Removing a step that has never been applied needs no pending marker.
      m8_set_form(
        session,
        result$context(),
        "duration",
        duration_scope = "group"
      )
      session$setInputs(save_new = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        result$recipe()$steps[[2L]]$parameters$arguments$scope,
        "group"
      )
      expect_match(output$duration_scope_help, "No grouping is set")
      session$setInputs(
        selected_step = result$recipe()$steps[[2L]]$id,
        remove = 2L
      )
      m8_set_form(session, result$context())
      m8_wait(session, function() !is.null(result$preview()))
      expect_null(output$pending_removals)
      expect_identical(record(), applied)

      session$setInputs(selected_step = applied_id, remove = 3L)
      m8_set_form(session, result$context())
      m8_wait(session, function() !is.null(result$preview()))
      expect_match(output$pending_removals$html, "Removal pending")
      session$setInputs(apply = 2L)
      expect_null(output$pending_removals)
      expect_length(record()$recipe$steps, 0L)
      expect_identical(record()$prepared_payload, original$raw_payload)
      expect_equal(record()$revision, 2L)

      session$setInputs(undo_edit = 2L)
      m8_set_form(session, result$context())
      m8_wait(session, function() !is.null(result$preview()))
      expect_length(result$recipe()$steps, 1L)
      expect_null(output$pending_removals)
      session$setInputs(apply = 3L)
      session$setInputs(selected_step = applied_id, remove = 4L)
      m8_set_form(session, result$context())
      m8_wait(session, function() !is.null(result$preview()))
      expect_match(output$pending_removals$html, "Removal pending")
      session$setInputs(reset = 1L)
      m8_wait(
        session,
        function() record()$revision == 4L && !is.null(result$context())
      )
      expect_null(output$pending_removals)
      expect_identical(record()$prepared_payload, original$raw_payload)
    }
  )
})

test_that("drag ordering previews the new sequence and supports Undo and repeated drags", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      acknowledge <- function() {
        values <- form_base()
        do.call(
          m8_set_form,
          c(
            list(
              session = session,
              context = editor_context(),
              type = values$step_type
            ),
            values[setdiff(names(values), "step_type")]
          )
        )
      }
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(
        step_type = "clock",
        clock_start = "06:00:00",
        clock_end = "18:00:00",
        save_new = 1L
      )
      m8_wait(session, function() !is.null(result$preview()))
      # Simulate the browser acknowledging controls updated by the worker.
      acknowledge()
      session$setInputs(
        step_type = "duration",
        duration_length = 6,
        duration_unit = "hours",
        save_new = 2L
      )
      # R13 updates inactive dates as part of the save transaction, before the
      # worker runs. Acknowledge that browser update before awaiting the result.
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      # Simulate the browser acknowledging controls updated by the worker.
      acknowledge()
      ids <- vapply(result$recipe()$steps, `[[`, character(1), "id")
      first_rows <- result$preview()$comparison$after$rows
      session$setInputs(selected_step = ids[[1L]], step_order = ids)
      expect_match(output$recipe_choices$html, "Drag the grip")
      session$setInputs(step_order = rev(ids))
      # Reordering refreshes the clean editor before previewing its new prefix.
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      # Simulate the browser acknowledging controls updated by the worker.
      acknowledge()
      expect_identical(
        vapply(result$recipe()$steps, `[[`, character(1), "id"),
        rev(ids)
      )
      expect_identical(input$selected_step, ids[[1L]])
      expect_lt(result$preview()$comparison$after$rows, first_rows)
      expect_identical(record(), original)
      session$setInputs(undo_edit = 1L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      # Simulate the browser acknowledging controls updated by the worker.
      acknowledge()
      expect_identical(
        vapply(result$recipe()$steps, `[[`, character(1), "id"),
        ids
      )
      expect_equal(result$preview()$comparison$after$rows, first_rows)
      # The widget publishes its restored order on load, so the same drag can repeat.
      session$setInputs(step_order = ids)
      session$setInputs(step_order = rev(ids))
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      # Simulate the browser acknowledging controls updated by the worker.
      acknowledge()
      reordered <- result$recipe()
      expect_identical(
        vapply(reordered$steps, `[[`, character(1), "id"),
        rev(ids)
      )
      session$setInputs(step_type = "duration", duration_length = 7)
      expect_true(result$dirty())
      session$setInputs(step_order = ids)
      expect_identical(result$recipe(), reordered)
      expect_match(result$notice(), "Save the form")
      expect_identical(record(), original)
      session$setInputs(new_step = 1L)
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        )
      )
      session$setInputs(step_order = c(ids[[1L]], "step_stale"))
      expect_identical(result$recipe(), reordered)
      expect_match(result$notice(), "recipe changed")
      expect_match(output$recipe_choices$html, ids[[2L]])
    }
  )
})

test_that("browser numeric transport does not leave a form loading", {
  a <- list(value = 82033.56166666666, text = "original", missing = NA_real_)
  b <- list(value = 82033.5616666667, text = "original", missing = NA_real_)
  expect_true(preparation_same_form(a, b))
  b$value <- 82033.56167
  expect_false(preparation_same_form(a, b))
  b <- a
  b$text <- "changed"
  expect_false(preparation_same_form(a, b))
})

testthat::test_that("unchanged preview selections do not block a later participant change", {
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(preview = 1L)
      m8_wait(
        session,
        function() !is.null(result$preview()) && !result$active()
      )
      initial <- result$preview()$comparison$view
      session$setInputs(
        plot_step = "all",
        plot_id = initial$id,
        plot_day = as.character(initial$day)
      )
      session$setInputs(preview = 2L)
      m8_wait(
        session,
        function() !is.null(result$preview()) && !result$active()
      )
      target <- setdiff(unique(initial$days$Id), initial$id)[[1L]]
      session$setInputs(plot_id = target)
      m8_wait(
        session,
        function() identical(result$preview()$comparison$view$id, target),
        seconds = 2
      )
      testthat::expect_identical(result$preview()$comparison$view$id, target)
      view <- result$preview()$comparison$view
      raw <- unserialize(record()$raw_payload)
      expected <- raw[
        as.character(raw$Id) == target &
          format(raw$Datetime, "%Y-%m-%d", tz = result$context()$timezone) ==
            view$day,
        ,
        drop = FALSE
      ]
      testthat::expect_equal(view$plot$Datetime, expected$Datetime)
      testthat::expect_equal(view$plot$Value, expected$MEDI)
      testthat::expect_identical(
        result$preview()$candidate$raw_payload,
        record()$raw_payload
      )
    }
  )
})

test_that("automatic day selection can be pinned and resumed across saved edits", {
  record <- shiny::reactiveVal(m8_record())
  original <- shiny::isolate(record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      wait_preview <- function()
        m8_wait(
          session,
          function()
            !is.null(result$preview()) && !result$active() && !result$pending()
        )
      # testServer does not echo updateSelect* messages as a browser does.
      acknowledge_view <- function() {
        view <- result$preview()$comparison$view
        session$setInputs(
          plot_id = as.character(view$id),
          plot_day = as.character(view$day),
          plot_step = if (is.infinite(view$step)) "all" else
            as.character(view$step)
        )
        invisible(view)
      }
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(
        automatic_day = TRUE,
        step_type = "time",
        time_mode = "clock",
        clock_start = "22:00:00",
        clock_end = "06:00:00",
        save_new = 1L
      )
      wait_preview()
      initial <- acknowledge_view()
      expect_true(result$automatic_day())
      target <- utils::tail(initial$days, 1L)
      session$setInputs(plot_id = target$Id, plot_day = target$Day)
      wait_preview()
      pinned <- acknowledge_view()
      expect_false(result$automatic_day())
      expect_equal(pinned$id, target$Id)
      expect_equal(pinned$day, target$Day)
      session$setInputs(automatic_day = FALSE)
      session$setInputs(clock_end = "12:00:00", save_step = 1L)
      wait_preview()
      changed <- acknowledge_view()
      expect_equal(changed$id, pinned$id)
      expect_equal(changed$day, pinned$day)
      expect_false(result$automatic_day())
      expect_match(changed$caption, "Chosen participant and day")

      session$setInputs(auto_day = 1L)
      wait_preview()
      once <- acknowledge_view()
      expect_false(result$automatic_day())
      expect_false(identical(c(once$id, once$day), c(pinned$id, pinned$day)))
      session$setInputs(clock_end = "08:00:00", save_step = 2L)
      wait_preview()
      saved <- acknowledge_view()
      expect_equal(c(saved$id, saved$day), c(once$id, once$day))

      session$setInputs(automatic_day = TRUE)
      wait_preview()
      resumed <- acknowledge_view()
      expect_true(result$automatic_day())
      expect_match(resumed$caption, "Automatic:")
      # An ordinary date-only change also pins, independently of Id changes.
      different_day <- setdiff(
        initial$days$Day[initial$days$Id == resumed$id],
        resumed$day
      )[[1L]]
      session$setInputs(plot_day = different_day)
      wait_preview()
      dated <- acknowledge_view()
      expect_false(result$automatic_day())
      expect_equal(dated$day, different_day)
      session$setInputs(automatic_day = FALSE, apply = 1L)
      expect_identical(record()$raw_payload, original$raw_payload)
      expect_false(result$automatic_day())
      session$setInputs(preview = 1L)
      wait_preview()
      refreshed <- acknowledge_view()
      expect_equal(c(refreshed$id, refreshed$day), c(dated$id, dated$day))

      session$setInputs(automatic_day = TRUE)
      wait_preview()
      acknowledge_view()
      session$setInputs(
        selected_step = result$recipe()$steps[[1L]]$id,
        toggle = 1L
      )
      wait_preview()
      automatic <- acknowledge_view()
      expect_true(result$automatic_day())
      expect_match(automatic$caption, "No day changes")
      expect_false(identical(
        c(automatic$id, automatic$day),
        c(dated$id, dated$day)
      ))
      session$setInputs(automatic_day = FALSE)
      expect_false(result$automatic_day())
      next_record <- m8_record()
      next_record$id <- "dataset_m8newselection"
      record(next_record)
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, next_record$id)
      )
      expect_true(result$automatic_day())
    }
  )
})

test_that("editor reloads follow entering data and settings, not its enabled flag", {
  context <- m8_context()
  first <- m8_clock(context)
  second <- m8_clock(context, start = "01:00:00", end = "08:00:00")
  recipe <- new_recipe(list(first, second))
  toggled <- edit_preparation_recipe(recipe, "toggle", id = second$id)
  expect_false(preparation_editor_needs_reload(recipe, toggled, second$id))
  expect_false(preparation_editor_needs_reload(recipe, toggled, first$id))
  earlier <- edit_preparation_recipe(recipe, "toggle", id = first$id)
  expect_true(preparation_editor_needs_reload(recipe, earlier, second$id))
  revised <- recipe
  revised$steps[[2L]]$parameters$arguments$end <- "09:00:00"
  expect_true(preparation_editor_needs_reload(recipe, revised, second$id))
  reordered <- edit_preparation_recipe(recipe, "up", id = second$id)
  expect_true(preparation_editor_needs_reload(recipe, reordered, second$id))
})

test_that("acknowledged fields no longer accept old values as pending updates", {
  original <- list(clock = "08:00", endpoint = "23:59")
  expected <- list(clock = "09:00", endpoint = "05:59")
  partial <- list(clock = "09:00", endpoint = "23:59")
  update <- preparation_merge_form_update(partial, expected, list(original))
  expect_identical(update$form, expected)
  expect_identical(update$origins[[1L]]$clock, "09:00")
  reverted <- preparation_merge_form_update(
    original,
    update$form,
    update$origins
  )
  expect_identical(reverted$form$clock, "08:00")
  expect_identical(reverted$form$endpoint, "05:59")
  expect_false(preparation_same_form(reverted$form, expected))
  settled <- preparation_merge_form_update(
    reverted$form,
    reverted$form,
    reverted$origins
  )
  expect_identical(settled$form, reverted$form)
  expect_length(settled$origins, 0L)

  # Capturing a user value also retires old values for that field immediately.
  edited <- preparation_merge_form_update(
    list(clock = "10:00", endpoint = "23:59"),
    expected,
    list(original)
  )
  expect_identical(edited$form$clock, "10:00")
  reverted <- preparation_merge_form_update(
    original,
    edited$form,
    edited$origins
  )
  expect_identical(reverted$form$clock, "08:00")
})

test_that("editor acknowledgements preserve clean state and saved-step navigation", {
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      acknowledge <- function() {
        values <- loading_form() %||% form_base()
        do.call(
          m8_set_form,
          c(
            list(
              session = session,
              context = editor_context(),
              type = values$step_type
            ),
            values[setdiff(names(values), "step_type")]
          )
        )
      }
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      m8_set_form(session, result$context(), "clock")
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      session$setInputs(
        clock_start = "01:00:00",
        clock_end = "08:00:00",
        save_new = 2L
      )
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      ids <- vapply(result$recipe()$steps, `[[`, character(1), "id")
      dirty_notifications <- 0L
      shiny::observe({
        result$dirty()
        dirty_notifications <<- dirty_notifications + 1L
      })
      session$flushReact()
      baseline_notifications <- dirty_notifications

      # An inactive endpoint is updated by the server, just as when a changed
      # prefix alters its default. Partial browser input is not a user edit.
      expected <- form_base()
      expected$instant_end_time <- "05:59:51"
      set_form(expected, ids[[2L]])
      session$flushReact()
      expect_identical(output$form_loading, "yes")
      expect_false(result$dirty())
      session$setInputs(save_step = 1L)
      expect_match(result$notice(), "Wait for the step controls")
      session$setInputs(apply = 1L)
      expect_equal(record()$revision, 0L)
      expect_match(result$notice(), "Preview the current saved recipe")

      # A later saved-step navigation replaces the intended editor transaction.
      session$setInputs(selected_step = ids[[2L]], step_order = rev(ids))
      expect_identical(
        vapply(result$recipe()$steps, `[[`, character(1), "id"),
        rev(ids)
      )
      session$setInputs(step_order = ids)
      expect_identical(
        vapply(result$recipe()$steps, `[[`, character(1), "id"),
        ids
      )
      acknowledge()
      expect_null(loading_form())
      expect_false(result$dirty())
      expect_true(preparation_same_form(form(), form_base()))
      expect_identical(dirty_notifications, baseline_notifications)

      # A genuine edit during an unrelated control update must not be treated
      # as an acknowledgement or lost when another dataset is selected.
      expected <- form_base()
      expected$instant_end_time <- "04:59:51"
      set_form(expected, ids[[2L]])
      session$flushReact()
      session$setInputs(clock_end = "10:00:00")
      expect_true(result$dirty())
      expect_identical(loading_form()$clock_end, "10:00:00")
      expect_match(output$form_status, "Unsaved changes")
      session$setInputs(step_order = rev(ids))
      expect_identical(
        vapply(result$recipe()$steps, `[[`, character(1), "id"),
        ids
      )
      expect_match(result$notice(), "Save the form")
      original <- record()
      other <- m8_record()
      other$id <- "dataset_loadingother"
      record(other)
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, other$id)
      )
      record(original)
      m8_wait(
        session,
        function() identical(result$context()$dataset_id, original$id)
      )
      expect_true(result$dirty())
      expect_identical(loading_form()$clock_end, "10:00:00")
      acknowledge()
      expect_null(loading_form())
      expect_true(result$dirty())
      expect_identical(input$clock_end, "10:00:00")
      session$setInputs(save_step = 2L)
      expect_false(result$dirty())
      expect_identical(
        result$recipe()$steps[[2L]]$parameters$arguments$end,
        "10:00:00"
      )

      # A restored unsaved form is dirty even before its inputs acknowledge.
      baseline <- form_base()
      unsaved <- baseline
      unsaved$clock_end <- "09:00:00"
      set_form(unsaved, ids[[2L]])
      form_base(baseline)
      session$flushReact()
      expect_true(result$dirty())
      session$setInputs(step_order = rev(ids))
      expect_identical(
        vapply(result$recipe()$steps, `[[`, character(1), "id"),
        ids
      )
      expect_match(result$notice(), "Save the form")
      acknowledge()
      expect_null(loading_form())
      expect_true(result$dirty())
      session$setInputs(save_step = 3L)
      expect_false(result$dirty())
      expect_identical(
        result$recipe()$steps[[2L]]$parameters$arguments$end,
        "09:00:00"
      )

      # Load another saved step, acknowledge just its clock end, then change
      # it back to the preceding editor's value while other fields are pending.
      edit_step(ids[[1L]])
      session$flushReact()
      session$setInputs(clock_end = "06:00:00")
      expect_false(is.null(loading_form()))
      session$setInputs(clock_end = "09:00:00")
      expect_true(result$dirty())
      expect_identical(loading_form()$clock_end, "09:00:00")
      session$setInputs(step_order = ids)
      session$setInputs(step_order = rev(ids))
      expect_identical(
        vapply(result$recipe()$steps, `[[`, character(1), "id"),
        ids
      )
      expect_match(result$notice(), "Save the form")
      acknowledge()
      expect_null(loading_form())
      expect_true(result$dirty())
      session$setInputs(save_step = 4L)
      expect_false(result$dirty())
      expect_identical(
        result$recipe()$steps[[1L]]$parameters$arguments$end,
        "09:00:00"
      )
    }
  )
})

test_that("automatic preview coalesces edits and discards cancelled queued work", {
  scheduled <- list()
  local_mocked_bindings(preparation_defer_preview = function(callback) {
    scheduled[[length(scheduled) + 1L]] <<- callback
  })
  record <- shiny::reactiveVal(m8_record())
  runtime <- new_session_runtime(
    resolve_runtime_profile("local", workers = 0L),
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(record()),
      runtime = runtime,
      on_commit = m8_commit(record)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      m8_set_form(session, result$context(), "clock")
      session$setInputs(save_new = 1L)
      expect_true(result$pending())
      expect_length(scheduled, 1L)
      session$setInputs(clock_end = "07:00:00", save_step = 1L)
      expect_length(scheduled, 2L)
      scheduled[[1L]]()
      session$flushReact()
      expect_true(result$pending())
      expect_null(result$preview())
      scheduled[[2L]]()
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(
        result$preview()$candidate$recipe$steps[[1L]]$parameters$arguments$end,
        "07:00:00"
      )
      session$setInputs(clock_end = "08:00:00", save_step = 2L)
      expect_length(scheduled, 3L)
      session$setInputs(cancel = 1L)
      scheduled[[3L]]()
      session$flushReact()
      expect_false(result$pending())
      expect_false(result$active())
      expect_null(result$preview())
      expect_identical(record()$prepared_payload, record()$raw_payload)
      session$setInputs(preview = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_length(scheduled, 3L)
      expect_identical(
        result$preview()$candidate$recipe$steps[[1L]]$parameters$arguments$end,
        "08:00:00"
      )
    }
  )
})
