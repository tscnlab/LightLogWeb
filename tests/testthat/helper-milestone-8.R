# All values in these fixtures are synthetic. They are not published evidence.
m8_record <- function(days = 3L, interval = 1800, edges = FALSE) {
  times <- seq(
    as.POSIXct("2026-03-28 00:00:00", tz = "UTC"),
    by = interval,
    length.out = as.integer(days * 86400 / interval)
  )
  times <- lubridate::with_tz(times, "Europe/Berlin")
  n <- length(times)
  data <- data.frame(
    Id = factor(rep(c("P01", "P01", "P02"), each = n)),
    Datetime = rep(times, 3L),
    MEDI = rep(seq_len(n), 3L),
    llw_source_dataset_id = rep(
      c("dataset_alpha", "dataset_beta", "dataset_alpha"),
      each = n
    ),
    file_group_id = rep(c("alpha:1", "beta:1", "alpha:2"), each = n),
    file.name = rep(c("one.csv", "two.csv", "three.csv"), each = n),
    State = factor(rep(c("awake", "sleep"), length.out = 3L * n))
  )
  if (edges) {
    data$MEDI[c(5L, 7L)] <- NA_real_
    data$Datetime[[20L]] <- data$Datetime[[20L]] + 60
    data$Datetime[[40L]] <- data$Datetime[[39L]]
    data <- data[-50L, ]
  }
  new_dataset_record(
    data,
    "Synthetic multi-day streams",
    new_source_manifest("synthetic", source_timezone = "Europe/Berlin"),
    analysis_settings = list(primary_variable = "MEDI"),
    id = "dataset_m8synthetic"
  )
}

m8_context <- function(record = m8_record())
  preparation_context(record, unserialize(record$raw_payload))

m8_clock <- function(context, start = "22:00:00", end = "06:00:00") {
  new_preparation_step(
    "clock",
    list(
      start = start,
      end = end,
      timezone = "Europe/Berlin",
      anchor = "start_date"
    ),
    context
  )
}

m8_preview <- function(record, recipe)
  preparation_task_worker(list(
    action = "preview",
    record = record,
    recipe = recipe,
    budget = 1024^3
  ))

m8_wait <- function(session, predicate, seconds = 20) {
  deadline <- Sys.time() + seconds
  repeat {
    session$flushReact()
    later::run_now(0.01)
    if (isTRUE(predicate())) return(invisible(TRUE))
    if (Sys.time() > deadline) stop("Timed out waiting for Prepare.")
  }
}

m8_set_form <- function(session, context, type = "participants", ...) {
  values <- preparation_form_defaults(context)
  values$step_type <- type
  values <- utils::modifyList(values, list(...))
  do.call(
    session$setInputs,
    c(
      values,
      list(
        preparation_category = if (
          type %in% c("invalidate", "calibrate", "transform", "variability")
        )
          "values" else if (type %in% preparation_time_types()) "sampling" else
          "filter",
        sampling_type = if (type %in% preparation_time_types()) type else
          "timezone",
        value_type = if (type == "transform") "calibrate" else if (
          type %in% c("invalidate", "calibrate", "variability")
        )
          type else "invalidate"
      )
    )
  )
  session$flushReact()
  invisible(values)
}

m8_commit <- function(record_value) {
  function(candidate, expected) {
    current <- record_value()
    if (
      !identical(current$id, expected$dataset_id) ||
        !identical(current$revision, expected$revision) ||
        !identical(current$raw_payload, candidate$raw_payload)
    )
      abort_llw("The dataset changed; preview again.", "preparation")
    record_value(candidate)
    candidate
  }
}
