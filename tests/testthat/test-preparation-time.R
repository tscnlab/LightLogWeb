time_step <- function(type, args, record)
  new_preparation_step(type, args, m8_context(record))
time_fixture <- function(
  time,
  value = seq_along(time),
  file = rep("a", length(time))
) {
  data <- data.frame(
    Id = factor(rep("P1", length(time))),
    Datetime = time,
    MEDI = as.numeric(value),
    file_group_id = file
  )
  data <- dplyr::group_by(data, Id)
  new_dataset_record(
    data,
    "Time fixture",
    new_source_manifest("synthetic", source_timezone = lubridate::tz(time)),
    analysis_settings = list(primary_variable = "MEDI")
  )
}

test_that("with and force have distinct, reproducible timestamp semantics", {
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  for (method in c("with", "force")) {
    step <- time_step(
      "timezone",
      list(timezone = "Asia/Kolkata", method = method, ambiguous = "error"),
      record
    )
    recipe <- new_recipe(list(step))
    result <- m8_preview(record, recipe)
    data <- unserialize(result$candidate$prepared_payload)
    expected <- if (method == "with")
      lubridate::with_tz(raw$Datetime, "Asia/Kolkata") else
      lubridate::force_tz(raw$Datetime, "Asia/Kolkata")
    expect_identical(data$Datetime, expected)
    expect_identical(data$MEDI, raw$MEDI)
    expect_identical(result$candidate$raw_payload, record$raw_payload)
    expect_identical(result$candidate$metadata_bundle, record$metadata_bundle)
    expect_identical(result$candidate$source_manifest, record$source_manifest)
    expect_equal(result$comparison$removed_rows, 0)
    expect_equal(
      result$comparison$retimed_rows,
      if (method == "with") 0 else nrow(raw)
    )
    expect_identical(
      m8_preview(result$candidate, recipe)$candidate$prepared_payload,
      result$candidate$prepared_payload
    )
    expect_equal(result$comparison$view$timezone, "Asia/Kolkata")
  }
})

test_that("native clock filters follow the preceding zone and allow an empty preview", {
  record <- time_fixture(as.POSIXct("2026-01-01 06:30:00", tz = "UTC"))
  context <- m8_context(record)
  zone <- time_step(
    "timezone",
    list(timezone = "Europe/Berlin", method = "with", ambiguous = "error"),
    record
  )
  clock <- new_preparation_step(
    "clock",
    list(
      start = "06:00:00",
      end = "07:00:00",
      timezone = "native",
      anchor = "start_date"
    ),
    context
  )
  after_zone <- m8_preview(record, new_recipe(list(zone, clock)))
  before_zone <- m8_preview(record, new_recipe(list(clock, zone)))
  expect_equal(after_zone$comparison$after$rows, 0)
  expect_equal(after_zone$comparison$removed_rows, 1)
  expect_equal(before_zone$comparison$after$rows, 1)
  expect_equal(after_zone$comparison$view$timezone, "Europe/Berlin")
  expect_no_error(ggplot2::ggplot_build(preparation_preview_plot(
    after_zone$comparison$view,
    context
  )))
  zone$enabled <- FALSE
  expect_equal(
    m8_preview(record, new_recipe(list(zone, clock)))$comparison$after$rows,
    1
  )
  expect_equal(
    m8_preview(record, new_recipe(list(clock)))$comparison$after$rows,
    1
  )
  expect_identical(clock$parameters$mapping_version, 2L)
  legacy <- new_preparation_step(
    "clock",
    list(
      start = "06:00:00",
      end = "07:00:00",
      timezone = "UTC",
      anchor = "start_date"
    ),
    context
  )
  zone$enabled <- TRUE
  expect_equal(
    m8_preview(record, new_recipe(list(zone, legacy)))$comparison$after$rows,
    1
  )
})

test_that("retimed previews keep continuous source and preview traces separate", {
  record <- time_fixture(as.POSIXct("2026-01-01", tz = "UTC") + (0:11) * 3600)
  step <- time_step(
    "timezone",
    list(
      timezone = "Europe/Berlin",
      method = "force",
      ambiguous = "error"
    ),
    record
  )
  result <- m8_preview(record, new_recipe(list(step)))
  view <- result$comparison$view
  built <- ggplot2::ggplot_build(preparation_preview_plot(
    view,
    m8_context(record)
  ))
  expect_equal(nrow(built$data[[1]]), 12)
  expect_false(anyNA(built$data[[1]]$y))
  expect_equal(nrow(built$data[[3]]), 12)
  expect_false(anyNA(built$data[[3]]$y))
})

test_that("force rejects skipped times and resolves repeated times only explicitly", {
  spring <- time_fixture(as.POSIXct("2026-03-29 02:30:00", tz = "UTC"))
  fall <- time_fixture(as.POSIXct("2026-10-25 02:30:00", tz = "UTC"))
  for (policy in c("error", "earlier", "later")) {
    args <- list(
      timezone = "Europe/Berlin",
      method = "force",
      ambiguous = policy
    )
    expect_error(
      m8_preview(spring, new_recipe(list(time_step("timezone", args, spring)))),
      "skipped or repeated"
    )
    recipe <- new_recipe(list(time_step("timezone", args, fall)))
    if (policy == "error")
      expect_error(m8_preview(fall, recipe), "skipped or repeated") else {
      data <- unserialize(m8_preview(fall, recipe)$candidate$prepared_payload)
      expected <- as.POSIXct(
        if (policy == "earlier") "2026-10-25 00:30:00" else
          "2026-10-25 01:30:00",
        tz = "UTC"
      )
      expect_equal(as.numeric(data$Datetime), as.numeric(expected))
      expect_equal(format(data$Datetime, "%H:%M"), "02:30")
    }
  }
})

test_that("native datetime endpoints keep their instants after a timezone step or reorder", {
  record <- m8_record()
  context <- m8_context(record)
  zone <- time_step(
    "timezone",
    list(
      timezone = "Asia/Kolkata",
      method = "with",
      ambiguous = "error"
    ),
    record
  )
  entering <- preparation_new_step_context(context, new_recipe(list(zone)))
  form <- preparation_form_defaults(entering)
  form$step_type <- "datetimes"
  endpoint <- new_preparation_step(
    "datetimes",
    preparation_form_arguments(form, entering),
    context
  )
  after <- m8_preview(record, new_recipe(list(zone, endpoint)))
  before <- m8_preview(record, new_recipe(list(endpoint, zone)))
  expect_equal(after$comparison$after$rows, context$rows)
  expect_identical(
    after$candidate$prepared_payload,
    before$candidate$prepared_payload
  )
  expect_identical(endpoint$parameters$arguments$start_timezone, "native")
  expect_match(endpoint$parameters$arguments$start, "[+]0530$")
})

test_that("device DST correction uses each recording start and handles half-hour transitions", {
  for (case in list(
    c("Europe/Berlin", "2026-03-29", "3600"),
    c("Europe/Berlin", "2026-10-25", "-3600"),
    c("Australia/Lord_Howe", "2026-10-04", "1800")
  )) {
    time <- seq(
      as.POSIXct(paste(case[[2]], "00:00:00"), tz = case[[1]]),
      by = 1800,
      length.out = 16
    )
    record <- time_fixture(
      c(time, utils::tail(time, 4)),
      file = c(rep("a", 16), rep("b", 4))
    )
    raw <- unserialize(record$raw_payload)
    step <- time_step("dst", list(method = "fixed_start_offset"), record)
    result <- m8_preview(record, new_recipe(list(step)))
    data <- unserialize(result$candidate$prepared_payload)
    # Expected shifts from each timezone's declared transition, independent of helper.
    dst <- lubridate::dst(time)
    shift <- as.numeric(case[[3]]) * (dst != dst[[1L]])
    expect_equal(as.numeric(data$Datetime[1:16]) - as.numeric(time), shift)
    expect_equal(data$Datetime[17:20], raw$Datetime[17:20])
    expect_identical(data$MEDI, raw$MEDI)
    expect_equal(sum(m8_context(record)$time_diagnostics$Offset_changes), 1)
    if (case[[1]] == "Europe/Berlin") {
      reference <- raw |>
        dplyr::group_by(Id, file_group_id) |>
        LightLogR::dst_change_handler()
      expect_equal(as.numeric(data$Datetime), as.numeric(reference$Datetime))
    }
  }
})

test_that("gap planning and expansion preserve duplicates, off-grid readings and groups", {
  record <- m8_record(edges = TRUE)
  raw <- unserialize(record$raw_payload)
  for (bounds in c("recording", "days"))
    for (epoch in c("dominant", "custom")) {
      args <- list(epoch = epoch, seconds = 1800, bounds = bounds)
      step <- time_step("gaps", args, record)
      evaluated <- evaluate_preparation(
        raw,
        new_recipe(list(step)),
        m8_context(record),
        details = TRUE
      )
      reference <- raw |>
        dplyr::group_by(Id, llw_source_dataset_id, file_group_id, file.name) |>
        LightLogR::gap_handler(
          epoch = lubridate::dseconds(1800),
          behavior = "full_sequence",
          full.days = bounds == "days"
        ) |>
        dplyr::ungroup()
      expect_equal(
        dplyr::ungroup(evaluated$data),
        reference,
        ignore_attr = TRUE
      )
      expect_equal(
        sum(is.na(evaluated$source_rows)),
        preparation_gap_plan(raw, args)$added
      )
      original <- !is.na(evaluated$source_rows)
      expect_identical(
        evaluated$data$MEDI[original],
        raw$MEDI[evaluated$source_rows[original]]
      )
      expect_true(all(is.na(evaluated$data$MEDI[!original])))
      expect_true(all(evaluated$data$is.implicit[!original]))
      expect_false(any(evaluated$data$is.implicit[original]))
      expect_identical(
        dplyr::group_vars(evaluated$data),
        dplyr::group_vars(raw)
      )
      expect_equal(sum(evaluated$removed_at), 0)
      twice <- preparation_expand_gaps(evaluated$data, args)$data
      expect_equal(nrow(twice), nrow(evaluated$data))
      expect_equal(sum(twice$is.implicit), sum(evaluated$data$is.implicit))
    }
})

test_that("full local days have DST-aware lengths and no measurement filling", {
  for (case in list(c("2026-03-29", "23"), c("2026-10-25", "25"))) {
    time <- as.POSIXct(
      paste(case[[1]], c("06:00:00", "18:00:00")),
      tz = "Europe/Berlin"
    )
    data <- unserialize(time_fixture(time)$raw_payload)
    args <- list(epoch = "custom", seconds = 3600, bounds = "days")
    result <- preparation_expand_gaps(data, args)
    expect_equal(nrow(result$data), as.numeric(case[[2]]))
    expect_equal(result$plan$added, as.numeric(case[[2]]) - 2)
    expect_equal(sum(!is.na(result$data$MEDI)), 2)
  }
})

test_that("gap expansion stops oversized allocation and handles singleton and empty groups", {
  time <- as.POSIXct("2026-01-01", tz = "UTC") + c(0, 86400 * 100)
  raw <- unserialize(time_fixture(time)$raw_payload)
  args <- list(epoch = "custom", seconds = 1, bounds = "recording")
  expect_error(preparation_gap_plan(raw, args), "approximately.*rows")
  args$seconds <- 60
  expect_error(preparation_gap_plan(raw, args, budget = 1e6), "MiB")
  singleton <- preparation_expand_gaps(raw[1, ], args)$data
  expect_equal(nrow(singleton), 1)
  expect_false(singleton$is.implicit)
  expect_equal(nrow(preparation_expand_gaps(raw[FALSE, ], args)$data), 0)
})

test_that("complete days include a final midnight and preserve duplicate-only input", {
  time <- as.POSIXct("2026-01-01", tz = "UTC") + c(0, 86400)
  raw <- unserialize(time_fixture(time)$raw_payload)
  args <- list(epoch = "custom", seconds = 3600, bounds = "days")
  result <- preparation_expand_gaps(raw, args)
  expect_equal(nrow(result$data), 48)
  expect_equal(result$plan$added, 46)
  raw$Datetime[[2]] <- raw$Datetime[[1]]
  result <- preparation_expand_gaps(raw, args)
  expect_equal(nrow(result$data), 25)
  expect_equal(result$plan$added, 23)
  expect_equal(sum(!is.na(result$data$MEDI)), 2)
  fractional <- unserialize(
    time_fixture(
      as.POSIXct("2026-01-01", tz = "UTC") + c(0, 3.4)
    )$raw_payload
  )
  args$seconds <- 1.7
  plan <- preparation_gap_plan(fractional, args)
  expect_equal(plan$groups$grid, ceiling(86400 / 1.7))
})

test_that("gap row lineage remains correct through later filters and value steps", {
  record <- m8_record(edges = TRUE)
  context <- m8_context(record)
  steps <- list(
    time_step(
      "gaps",
      list(epoch = "custom", seconds = 1800, bounds = "recording"),
      record
    ),
    m8_clock(context, "06:00:00", "12:00:00"),
    new_preparation_step(
      "calibrate",
      list(variable = "MEDI", multiplier = 2, offset = 0, reference = ""),
      context
    )
  )
  result <- m8_preview(record, new_recipe(steps))
  expect_gte(result$comparison$removed_rows, 0)
  expect_gte(result$comparison$added_rows, 0)
  expect_equal(
    result$comparison$after$rows,
    context$rows - result$comparison$removed_rows + result$comparison$added_rows
  )
  expect_true(all(result$comparison$steps$removed >= 0))
  expect_no_error(ggplot2::ggplot_build(preparation_preview_plot(
    result$comparison$view,
    context
  )))
})

test_that("time controls round trip and publish one replayable recipe", {
  record <- shiny::reactiveVal(m8_record(edges = TRUE))
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
      m8_set_form(
        session,
        result$context(),
        "timezone",
        time_timezone = "Asia/Kolkata",
        time_method = "with"
      )
      session$setInputs(save_step = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$recipe()$steps[[1]]$type, "timezone")
      session$setInputs(apply = 1L)
      expect_equal(
        lubridate::tz(unserialize(record()$prepared_payload)$Datetime),
        "Asia/Kolkata"
      )
      expect_identical(record()$raw_payload, original$raw_payload)
      session$setInputs(new_step = 1L)
      expect_equal(form_base()$time_timezone, "Asia/Kolkata")
      m8_set_form(session, editor_context())
      m8_set_form(session, editor_context(), "datetimes")
      session$setInputs(save_step = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      endpoint <- result$recipe()$steps[[2]]
      expect_equal(endpoint$type, "datetimes")
      session$setInputs(selected_step = endpoint$id, edit = 1L)
      expect_equal(form_base()$instant_start_zone, "native")
      expect_equal(form_base()$instant_start_timezone, "Asia/Kolkata")
      expect_identical(
        preparation_form_arguments(form_base(), editor_context()),
        endpoint$parameters$arguments
      )
      session$setInputs(new_step = 2L)
      m8_set_form(session, editor_context())
      m8_set_form(
        session,
        editor_context(),
        "gaps",
        gap_epoch = "custom",
        gap_seconds = 1800,
        gap_bounds = "recording"
      )
      session$setInputs(save_step = 3L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(result$preview()$comparison$added_rows, 3)
      id <- result$recipe()$steps[[3]]$id
      session$setInputs(selected_step = id, edit = 2L)
      expect_equal(form_base()$step_type, "gaps")
      expect_equal(form_base()$gap_epoch, "custom")
      do.call(
        session$setInputs,
        c(
          form_base(),
          list(preparation_category = "sampling", sampling_type = "gaps")
        )
      )
      m8_wait(session, function() is.null(loading_form()))
      expect_false(result$dirty())
      session$setInputs(apply = 2L)
      expect_equal(sum(unserialize(record()$prepared_payload)$is.implicit), 3)
      expect_identical(record()$raw_payload, original$raw_payload)
      session$setInputs(reset = 1L)
      m8_wait(
        session,
        function() identical(record()$prepared_payload, original$raw_payload)
      )
      expect_length(record()$recipe$steps, 0)
    }
  )
})

test_that("undo after DST toggle and reorder refreshes inactive date controls cleanly", {
  time <- lubridate::with_tz(
    as.POSIXct("2026-10-24", tz = "UTC") + (0:143) * 1800,
    "Europe/Berlin"
  )
  record <- shiny::reactiveVal(time_fixture(time))
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
      acknowledge()
      m8_set_form(session, editor_context(), "gaps")
      session$setInputs(save_new = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      m8_set_form(session, editor_context(), "dst")
      session$setInputs(save_new = 2L)
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      id <- result$recipe()$steps[[2]]$id
      session$setInputs(selected_step = id, toggle = 1L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      session$setInputs(undo_edit = 1L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      expect_false(result$dirty())
      expect_null(loading_form())
      expect_equal(form_base()$date_end, "2026-10-26")
      expect_true(result$recipe()$steps[[2]]$enabled)
      session$setInputs(selected_step = id, up = 1L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      session$setInputs(undo_edit = 2L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      expect_false(result$dirty())
      expect_null(loading_form())
      session$setInputs(apply = 1L)
      expect_identical(record()$recipe, result$recipe())
    }
  )
})


test_that("native absolute endpoint editor follows its prefix after reorder and toggle", {
  time <- as.POSIXct("2026-01-02 00:00:00", tz = "Europe/Istanbul") +
    (0:239) * 60
  record <- shiny::reactiveVal(time_fixture(time))
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
      settle <- function() {
        acknowledge()
        m8_wait(session, function() !is.null(result$preview()))
        acknowledge()
        expect_false(result$dirty())
        expect_null(loading_form())
        expect_equal(result$preview()$comparison$after$rows, 240)
      }
      m8_wait(session, function() !is.null(result$context()))
      acknowledge()
      m8_set_form(
        session,
        editor_context(),
        "timezone",
        time_timezone = "Europe/Berlin"
      )
      session$setInputs(save_new = 1L)
      settle()
      zone_id <- result$recipe()$steps[[1]]$id
      session$setInputs(new_step = 1L)
      acknowledge()
      m8_set_form(session, editor_context(), "datetimes")
      session$setInputs(save_new = 2L)
      settle()
      endpoint <- result$recipe()$steps[[2]]
      expect_equal(form_base()$instant_start_date, "2026-01-01")
      session$setInputs(selected_step = endpoint$id, up = 1L)
      settle()
      expect_equal(form_base()$instant_start_date, "2026-01-02")
      expect_equal(form_base()$instant_start_time, "00:00:00")
      expect_identical(result$recipe()$steps[[1]], endpoint)
      ids <- vapply(result$recipe()$steps, `[[`, character(1), "id")
      session$setInputs(step_order = ids)
      session$setInputs(step_order = rev(ids))
      settle()
      expect_equal(form_base()$instant_start_date, "2026-01-01")
      expect_equal(form_base()$instant_start_time, "22:00:00")
      expect_identical(result$recipe()$steps[[2]], endpoint)
      session$setInputs(save_step = 1L)
      settle()
      expect_identical(
        result$recipe()$steps[[2]]$parameters,
        endpoint$parameters
      )
      session$setInputs(selected_step = zone_id, toggle = 1L)
      settle()
      expect_equal(form_base()$instant_start_date, "2026-01-02")
      session$setInputs(apply = 1L)
      expect_identical(record()$recipe, result$recipe())
    }
  )
})

test_that("calendar bounds retain local dates at offset transitions", {
  instants <- as.POSIXct(
    c(
      "2026-03-29 00:30:00",
      "2026-03-29 01:30:00",
      "2026-10-25 00:30:00",
      "2026-10-25 01:30:00",
      "2011-12-30 09:30:00",
      "2011-12-30 10:30:00"
    ),
    tz = "UTC"
  )
  for (zone in c(
    "UTC",
    "Europe/Berlin",
    "America/New_York",
    "Australia/Lord_Howe",
    "Pacific/Apia",
    "Asia/Kolkata"
  )) {
    for (i in c(1L, 3L, 5L)) {
      context <- list(
        start = instants[i],
        end = instants[i + 1L],
        timezone = zone
      )
      expected <- as.Date(format(instants[c(i, i + 1L)], "%Y-%m-%d", tz = zone))
      expect_identical(preparation_date_limits(context), expected)
    }
  }
})

test_that("timezone editor stays clean when an earlier zone is disabled", {
  time <- as.POSIXct("2026-01-02 00:00:00", tz = "Europe/Istanbul") +
    (0:239) * 60
  record <- shiny::reactiveVal(time_fixture(time))
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
      settle <- function() {
        acknowledge()
        m8_wait(session, function() !is.null(result$preview()))
        acknowledge()
        expect_false(result$dirty())
        expect_equal(result$preview()$comparison$after$rows, 240)
      }
      m8_wait(session, function() !is.null(result$context()))
      acknowledge()
      m8_set_form(
        session,
        editor_context(),
        "timezone",
        time_timezone = "Europe/Berlin"
      )
      session$setInputs(save_new = 1L)
      settle()
      first <- result$recipe()$steps[[1]]$id
      session$setInputs(new_step = 1L)
      acknowledge()
      session$setInputs(selected_step = first, toggle = 1L)
      expect_equal(form_base()$date_start, "2026-01-02")
      settle()
      expect_null(editing_id())
      expect_equal(form()$step_type, "participants")
      session$setInputs(toggle = 2L)
      expect_equal(form_base()$date_start, "2026-01-01")
      settle()
      m8_set_form(session, editor_context(), "timezone", time_timezone = "UTC")
      session$setInputs(save_new = 2L)
      settle()
      second <- result$recipe()$steps[[2]]
      expect_equal(form_base()$date_start, "2026-01-01")

      for (i in 3:5) {
        session$setInputs(selected_step = first, toggle = i)
        # Browser date controls must receive valid defaults before their new
        # bounds can clear a now-outside value in the hidden filtering panel.
        expect_equal(
          form_base()$date_start,
          if (i %% 2L) "2026-01-02" else "2026-01-01"
        )
        expect_equal(form_base()$time_timezone, "UTC")
        settle()
        for (name in c("date_start", "instant_start_date")) {
          html <- output[[paste0(name, "_control")]]$html
          first_day <- if (i %% 2L) "2026-01-02" else "2026-01-01"
          expect_match(
            html,
            paste0('data-initial-date="', first_day, '"'),
            fixed = TRUE
          )
          expect_match(
            html,
            paste0('data-min-date="', first_day, '"'),
            fixed = TRUE
          )
        }
        expect_identical(result$recipe()$steps[[2]], second)
      }
      session$setInputs(apply = 1L)
      expect_identical(record()$recipe, result$recipe())
      expect_equal(
        lubridate::tz(
          unserialize(record()$prepared_payload)$Datetime
        ),
        "UTC"
      )
    }
  )
})
