long_task_types <- function() {
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
}

task_states <- function() {
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
}

new_task_spec <- function(
  task_type,
  payload,
  dataset_id = NULL,
  dataset_revision = NULL,
  id = new_stable_id("task")
) {
  task_type <- match.arg(task_type, long_task_types())
  assert_scalar_string(id, "id")
  if (!is.null(dataset_id)) {
    assert_scalar_string(dataset_id, "dataset_id")
    if (
      !is.numeric(dataset_revision) ||
        length(dataset_revision) != 1L ||
        is.na(dataset_revision) ||
        dataset_revision < 0 ||
        dataset_revision != floor(dataset_revision)
    ) {
      abort_llw(
        "Dataset tasks require one non-negative whole-number revision snapshot.",
        type = "validation"
      )
    }
    dataset_revision <- as.integer(dataset_revision)
  } else if (!is.null(dataset_revision)) {
    abort_llw(
      "`dataset_revision` requires a `dataset_id`.",
      type = "validation"
    )
  }
  assert_serializable_value(payload, "task payload")
  structure(
    list(
      id = id,
      task_type = task_type,
      payload = payload,
      dataset_id = dataset_id,
      dataset_revision = dataset_revision,
      queued_at = Sys.time()
    ),
    class = c("llw_task_spec", "list")
  )
}

new_task_status <- function(
  task_type,
  state = "idle",
  task_id = NULL,
  message = NULL,
  warnings = character(),
  error = NULL,
  queued_at = NULL,
  started_at = NULL,
  completed_at = NULL
) {
  task_type <- match.arg(task_type, long_task_types())
  state <- match.arg(state, task_states())
  structure(
    list(
      task_type = task_type,
      task_id = task_id,
      state = state,
      message = message,
      warnings = warnings,
      error = error,
      queued_at = queued_at,
      started_at = started_at,
      completed_at = completed_at
    ),
    class = c("llw_task_status", "list")
  )
}

allowed_task_transitions <- function(state) {
  switch(
    state,
    idle = "queued",
    queued = c("running", "cancelled", "error"),
    running = c("finalizing", "error", "cancelled", "stale"),
    finalizing = c("complete", "warning", "error", "cancelled", "stale"),
    complete = "queued",
    warning = "queued",
    error = "queued",
    cancelled = "queued",
    stale = "queued",
    character()
  )
}

transition_task_status <- function(
  status,
  state,
  message = NULL,
  warnings = status$warnings,
  error = NULL,
  at = Sys.time()
) {
  if (!inherits(status, "llw_task_status")) {
    abort_llw(
      "`status` must be created by `new_task_status()`.",
      type = "validation"
    )
  }
  state <- match.arg(state, task_states())
  if (!state %in% allowed_task_transitions(status$state)) {
    abort_llw(
      paste0(
        "Invalid task transition from `",
        status$state,
        "` to `",
        state,
        "`."
      ),
      type = "validation"
    )
  }
  status$state <- state
  status$message <- message
  status$warnings <- unique(as.character(warnings))
  status$error <- error
  if (identical(state, "queued")) {
    status$queued_at <- at
    status$started_at <- NULL
    status$completed_at <- NULL
  }
  if (identical(state, "running")) {
    status$started_at <- at
  }
  if (state %in% c("complete", "warning", "error", "cancelled", "stale")) {
    status$completed_at <- at
  }
  status
}

new_task_result <- function(value, warnings = character()) {
  assert_serializable_value(value, "task result")
  assert_character_vector(warnings, "warnings")
  structure(
    list(value = value, warnings = unique(as.character(warnings))),
    class = c("llw_task_result", "list")
  )
}

task_result_is_stale <- function(spec, current_revision) {
  if (!inherits(spec, "llw_task_spec")) {
    abort_llw("`spec` must be an `llw_task_spec`.", type = "validation")
  }
  if (is.null(spec$dataset_id)) {
    return(FALSE)
  }
  if (
    is.null(current_revision) ||
      length(current_revision) != 1L ||
      is.na(current_revision)
  ) {
    return(TRUE)
  }
  !identical(as.integer(current_revision), spec$dataset_revision)
}

finalize_task_result <- function(
  spec,
  result,
  current_revision,
  on_result = function(value, spec) invisible(NULL)
) {
  if (!inherits(result, "llw_task_result")) {
    abort_llw("`result` must be an `llw_task_result`.", type = "validation")
  }
  if (!is.function(on_result)) {
    abort_llw("`on_result` must be a function.", type = "validation")
  }
  if (task_result_is_stale(spec, current_revision)) {
    return(list(
      state = "stale",
      value = NULL,
      warnings = result$warnings,
      applied = FALSE,
      message = paste(
        "The result was discarded because dataset revision",
        spec$dataset_revision,
        "is no longer current."
      )
    ))
  }
  on_result(result$value, spec)
  has_warnings <- length(result$warnings) > 0L
  list(
    state = if (has_warnings) "warning" else "complete",
    value = result$value,
    warnings = result$warnings,
    applied = TRUE,
    message = if (has_warnings) {
      "The task completed with warnings."
    } else {
      "The task completed."
    }
  )
}

run_task_worker_sync <- function(worker, payload, spec) {
  warnings <- character()
  value <- withCallingHandlers(
    worker(payload, spec),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  new_task_result(value, warnings)
}

new_long_task <- function(
  worker,
  task_type,
  runtime,
  revision_lookup = function(dataset_id) NULL,
  on_result = function(value, spec) invisible(NULL)
) {
  if (!is.function(worker)) {
    abort_llw("`worker` must be a function.", type = "validation")
  }
  task_type <- match.arg(task_type, long_task_types())
  if (!is.list(runtime) || !is.function(runtime$submit)) {
    abort_llw(
      "`runtime` must be created by `new_session_runtime()`.",
      type = "validation"
    )
  }
  if (!is.function(revision_lookup) || !is.function(on_result)) {
    abort_llw(
      "`revision_lookup` and `on_result` must be functions.",
      type = "validation"
    )
  }

  status_value <- shiny::reactiveVal(new_task_status(task_type))
  result_value <- shiny::reactiveVal(NULL)
  error_value <- shiny::reactiveVal(NULL)
  current_spec <- NULL
  active_job <- NULL
  handled_id <- NULL
  cancelled_ids <- character()

  extended_task <- shiny::ExtendedTask$new(function(spec) {
    if (runtime$profile$synchronous) {
      return(tryCatch(
        promises::promise_resolve(
          run_task_worker_sync(worker, spec$payload, spec)
        ),
        error = function(cnd) promises::promise_reject(cnd)
      ))
    }

    payload <- spec$payload
    job <- mirai::mirai(
      {
        warnings <- character()
        value <- withCallingHandlers(
          worker(payload, spec),
          warning = function(cnd) {
            warnings <<- c(warnings, conditionMessage(cnd))
            invokeRestart("muffleWarning")
          }
        )
        list(value = value, warnings = unique(warnings))
      },
      .args = list(
        worker = worker,
        payload = spec$payload,
        spec = spec
      ),
      .timeout = runtime$profile$task_timeout_ms,
      .compute = runtime$compute_profile
    )
    active_job <<- job
    job
  })

  set_error_status <- function(condition, mark_handled = TRUE) {
    mapped <- normalize_task_error(condition, task_type)
    error_value(mapped)
    result_value(NULL)
    status <- status_value()
    if (
      identical(status$state, "idle") ||
        status$state %in%
          c(
            "complete",
            "warning",
            "error",
            "cancelled",
            "stale"
          )
    ) {
      status$task_id <- new_stable_id("task")
      status <- transition_task_status(
        status,
        "queued",
        message = "The task request is being validated.",
        warnings = character()
      )
    }
    status_value(transition_task_status(
      status,
      "error",
      message = llw_public_message(mapped),
      error = mapped
    ))
    if (mark_handled && !is.null(current_spec)) {
      handled_id <<- current_spec$id
    }
    invisible(mapped)
  }

  reject <- function(condition) {
    in_flight <- !is.null(current_spec) &&
      !identical(current_spec$id, handled_id)
    if (
      in_flight ||
        status_value()$state %in% c("queued", "running", "finalizing")
    ) {
      return(invisible(FALSE))
    }
    current_spec <<- NULL
    handled_id <<- NULL
    result_value(NULL)
    error_value(NULL)
    status <- status_value()
    status$task_id <- new_stable_id("task")
    status_value(transition_task_status(
      status,
      "queued",
      message = "The task request is being validated.",
      warnings = character()
    ))
    set_error_status(condition, mark_handled = FALSE)
    invisible(status_value()$task_id)
  }

  handle_completion <- function(spec, result) {
    status_value(transition_task_status(
      status_value(),
      "finalizing",
      message = "Finalizing and validating the task result."
    ))
    current_revision <- if (is.null(spec$dataset_id)) {
      NULL
    } else {
      revision_lookup(spec$dataset_id)
    }
    outcome <- finalize_task_result(
      spec,
      result,
      current_revision = current_revision,
      on_result = on_result
    )
    if (outcome$applied) {
      result_value(outcome$value)
    }
    status_value(transition_task_status(
      status_value(),
      outcome$state,
      message = outcome$message,
      warnings = outcome$warnings
    ))
  }

  shiny::observe({
    extended_status <- extended_task$status()
    spec <- current_spec
    if (
      is.null(spec) ||
        identical(spec$id, handled_id) ||
        !extended_status %in% c("success", "error")
    ) {
      return()
    }
    handled_id <<- spec$id
    on.exit(runtime$release(spec$id), add = TRUE)

    if (spec$id %in% cancelled_ids) {
      return()
    }

    if (identical(extended_status, "error")) {
      condition <- tryCatch(extended_task$result(), error = identity)
      set_error_status(condition)
      return()
    }

    result <- extended_task$result()
    if (!inherits(result, "llw_task_result")) {
      result <- new_task_result(result$value, result$warnings)
    }
    tryCatch(
      handle_completion(spec, result),
      error = function(cnd) {
        set_error_status(cnd)
      }
    )
  })

  invoke <- function(payload, dataset_id = NULL) {
    in_flight <- !is.null(current_spec) &&
      !identical(current_spec$id, handled_id)
    if (
      in_flight ||
        status_value()$state %in% c("queued", "running", "finalizing")
    ) {
      return(invisible(FALSE))
    }
    spec <- tryCatch(
      {
        revision <- if (is.null(dataset_id)) {
          NULL
        } else {
          revision_lookup(dataset_id)
        }
        new_task_spec(
          task_type = task_type,
          payload = payload,
          dataset_id = dataset_id,
          dataset_revision = revision
        )
      },
      error = identity
    )
    if (inherits(spec, "error")) {
      reject(spec)
      return(invisible(FALSE))
    }
    current_spec <<- spec
    active_job <<- NULL
    result_value(NULL)
    error_value(NULL)
    status <- status_value()
    status$task_id <- spec$id
    status_value(transition_task_status(
      status,
      "queued",
      message = "The task is queued.",
      warnings = character()
    ))

    start <- function() {
      status_value(transition_task_status(
        status_value(),
        "running",
        message = "The task is running."
      ))
      extended_task$invoke(spec)
    }
    submitted <- tryCatch(
      {
        runtime$register_cancel(spec$id, function() {
          cancelled_ids <<- unique(c(cancelled_ids, spec$id))
          if (!is.null(active_job)) {
            try(mirai::stop_mirai(active_job), silent = TRUE)
          }
        })
        runtime$submit(
          spec$id,
          start,
          on_error = function(cnd) {
            if (!spec$id %in% cancelled_ids) {
              handled_id <<- spec$id
              set_error_status(cnd)
            }
          }
        )
      },
      error = identity
    )
    if (inherits(submitted, "error")) {
      handled_id <<- spec$id
      set_error_status(submitted)
    }
    invisible(spec$id)
  }

  cancel <- function() {
    spec <- current_spec
    if (is.null(spec) || identical(spec$id, handled_id)) {
      return(invisible(FALSE))
    }
    if (identical(status_value()$state, "queued")) {
      if (runtime$cancel_queued(spec$id)) {
        cancelled_ids <<- unique(c(cancelled_ids, spec$id))
        handled_id <<- spec$id
        status_value(transition_task_status(
          status_value(),
          "cancelled",
          message = "The queued task was cancelled."
        ))
        return(invisible(TRUE))
      }
    }
    if (status_value()$state %in% c("running", "finalizing")) {
      cancelled_ids <<- unique(c(cancelled_ids, spec$id))
      if (!is.null(active_job)) {
        try(mirai::stop_mirai(active_job), silent = TRUE)
      }
      status_value(transition_task_status(
        status_value(),
        "cancelled",
        message = "Cancellation was requested."
      ))
      return(invisible(TRUE))
    }
    invisible(FALSE)
  }

  list(
    invoke = invoke,
    reject = reject,
    cancel = cancel,
    status = shiny::reactive(status_value()),
    state = shiny::reactive(status_value()$state),
    result = shiny::reactive(result_value()),
    error = shiny::reactive(error_value()),
    extended_task = extended_task
  )
}
