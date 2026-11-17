m8_aggregate_step <- function(
  context,
  length = 15,
  unit = "minutes",
  numeric = "mean",
  missing = "propagate"
)
  new_preparation_step(
    "aggregate",
    list(length = length, unit = unit, numeric = numeric, missing = missing),
    context
  )

test_that("numeric missingness uses each column's input rows and allows the limit", {
  raw <- data.frame(
    Id = "P",
    file_group_id = "file",
    Datetime = as.POSIXct("2026-01-01", tz = "UTC") + 0:14 * 60,
    MEDI = c(1:4, NA, 1:3, NA, NA, rep(NA_real_, 5)),
    Other = c(NA, NA, 1:3, 1:5, 1:5)
  )
  raw$Duration <- lubridate::dseconds(raw$MEDI)
  args <- list(
    length = 5,
    unit = "minutes",
    numeric = "mean",
    missing = "threshold",
    max_missing = 20
  )
  reference <- preparation_variability_reference(raw)
  result <- preparation_aggregate(raw, args, reference, "MEDI")
  expect_equal(result$data$MEDI, c(2.5, NA, NA))
  expect_equal(result$data$Other, c(NA, 3, 3))
  expect_s4_class(result$data$Duration, "Duration")
  expect_equal(as.numeric(result$data$Duration), c(2.5, NA, NA))
  expect_equal(result$note$coverage$Missing_readings_percent, c(20, 40, 100))
  expect_equal(
    result$note$coverage$Missingness_limit_exceeded,
    c(FALSE, TRUE, TRUE)
  )
  args$max_missing <- 100
  args$numeric <- "sum"
  expect_equal(
    preparation_aggregate(raw, args, reference, "MEDI")$data$MEDI,
    c(10, 6, NA)
  )
  args$max_missing <- 0
  expect_true(all(is.na(
    preparation_aggregate(raw, args, reference, "MEDI")$data$MEDI
  )))
  # Duplicate rows count as readings, even though duplicate timestamps cannot
  # increase the separate observed-time coverage.
  duplicate <- rbind(raw[1:5, ], raw[5, ])
  args$numeric <- "mean"
  args$max_missing <- 30
  expect_true(is.na(
    preparation_aggregate(duplicate, args, reference, "MEDI")$data$MEDI
  ))
})

test_that("absent timestamps affect the missingness limit only after explicit Gaps", {
  raw <- data.frame(
    Id = "P",
    file_group_id = "file",
    Datetime = as.POSIXct("2026-01-01", tz = "UTC") + c(0, 60, 240),
    MEDI = c(1, 2, 6)
  )
  record <- new_dataset_record(
    raw,
    "Missingness fixture",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI")
  )
  context <- m8_context(record)
  aggregate <- new_preparation_step(
    "aggregate",
    list(
      length = 5,
      unit = "minutes",
      numeric = "mean",
      missing = "threshold",
      max_missing = 20
    ),
    context
  )
  direct <- m8_preview(record, new_recipe(list(aggregate)))
  expect_equal(unserialize(direct$candidate$prepared_payload)$MEDI, 3)
  expect_equal(
    direct$comparison$aggregation$coverage$Missing_readings_percent,
    0
  )
  gaps <- new_preparation_step(
    "gaps",
    list(epoch = "custom", seconds = 60, bounds = "recording"),
    context
  )
  explicit <- m8_preview(record, new_recipe(list(gaps, aggregate)))
  expect_true(is.na(unserialize(explicit$candidate$prepared_payload)$MEDI))
  expect_equal(
    explicit$comparison$aggregation$coverage$Missing_readings_percent,
    40
  )
  expect_identical(explicit$candidate$raw_payload, record$raw_payload)
})

test_that("common type summaries retain defaults and use chronological first and last", {
  raw <- data.frame(
    Id = "P",
    file_group_id = "file",
    Datetime = as.POSIXct("2026-01-01", tz = "UTC") +
      c(120, 0, 60, 120, 240, 300, 360),
    MEDI = 1:7,
    Text = c("b", "z", "a", "c", NA, NA, NA),
    Flag = c(TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, NA),
    is.implicit = c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
  raw$Category <- ordered(raw$Text, levels = c("z", "a", "b", "c"))
  reference <- preparation_variability_reference(raw)
  legacy <- list(
    length = 5,
    unit = "minutes",
    numeric = "mean",
    missing = "available"
  )
  defaults <- preparation_aggregate(raw, legacy, reference, "MEDI")$data
  public <- LightLogR::aggregate_Datetime(
    preparation_time_groups(raw),
    unit = "5 minutes",
    type = "floor"
  )
  expect_equal(defaults$Text, public$Text)
  expect_equal(defaults$Category, public$Category)
  expect_equal(defaults$Flag, public$Flag)
  for (method in c("first", "last")) {
    result <- preparation_aggregate(
      raw,
      c(legacy, list(categorical = method)),
      reference,
      "MEDI"
    )$data
    expect_identical(
      result$Text,
      if (method == "first") c("z", NA_character_) else
        c(NA_character_, NA_character_)
    )
    expect_true(is.ordered(result$Category))
    expect_identical(levels(result$Category), levels(raw$Category))
    expect_identical(result$is.implicit, c(FALSE, TRUE))
  }
  for (method in c("majority", "any", "all")) {
    result <- preparation_aggregate(
      raw,
      c(legacy, list(logical = method)),
      reference,
      "MEDI"
    )$data
    expect_identical(result$Flag, c(method == "any", NA))
    expect_identical(result$is.implicit, c(FALSE, TRUE))
  }
  tie <- raw[1:2, ]
  expect_true(preparation_aggregate(tie, legacy, reference, "MEDI")$data$Flag)
  # Equal timestamps keep their original row order for a first/last choice.
  tie$Datetime <- tie$Datetime[[1]]
  expect_identical(
    preparation_aggregate(
      tie,
      c(legacy, list(categorical = "first")),
      reference,
      "MEDI"
    )$data$Text,
    "b"
  )
  expect_identical(
    preparation_aggregate(
      tie,
      c(legacy, list(categorical = "last")),
      reference,
      "MEDI"
    )$data$Text,
    "z"
  )
})

test_that("aggregation settings reject invalid limits and remain compatible", {
  legacy <- list(
    length = 5,
    unit = "minutes",
    numeric = "mean",
    missing = "available"
  )
  defaults <- preparation_aggregation_arguments(legacy)
  expect_equal(defaults$categorical, "mode")
  expect_equal(defaults$logical, "majority")
  expect_equal(defaults$missing, "available")
  for (limit in list(-1, 101, NA_real_, Inf, "20", c(20, 30), NULL)) {
    active <- legacy
    active$missing <- "threshold"
    expect_error(
      preparation_aggregation_arguments(c(active, list(max_missing = limit))),
      "maximum missing"
    )
    for (policy in c("propagate", "available")) {
      inactive <- legacy
      inactive$missing <- policy
      expect_equal(
        preparation_aggregation_arguments(c(
          inactive,
          list(max_missing = limit)
        ))$max_missing,
        20
      )
    }
  }
  expect_error(
    preparation_aggregation_arguments(c(legacy, list(categorical = "custom"))),
    "supported"
  )
  expect_error(
    preparation_aggregation_arguments(c(legacy, list(logical = "custom"))),
    "supported"
  )
  expect_error(
    preparation_aggregation_arguments(c(legacy, list(unused = 1))),
    "unsupported"
  )
})

test_that("aggregation matches explicit public LightLogR pipelines", {
  record <- m8_record(days = 1, interval = 60, edges = TRUE)
  raw <- dplyr::group_by(unserialize(record$raw_payload), Id)
  context <- m8_context(record)
  reference <- preparation_variability_reference(raw)
  for (method in c("mean", "median", "min", "max", "sum")) {
    for (missing in c("propagate", "available")) {
      args <- m8_aggregate_step(
        context,
        numeric = method,
        missing = missing
      )$parameters$arguments
      actual <- preparation_aggregate(raw, args, reference, "MEDI")
      handler <- get(
        method,
        envir = if (method == "median") asNamespace("stats") else baseenv()
      )
      expected <- raw |>
        dplyr::group_by(Id, llw_source_dataset_id, file_group_id, file.name) |>
        LightLogR::aggregate_Datetime(
          unit = "15 minutes",
          type = "floor",
          numeric.handler = function(x)
            if (all(is.na(x))) NA_real_ else
              handler(x, na.rm = missing == "available")
        ) |>
        dplyr::group_by(Id)
      expected <- expected[, names(raw)]
      expect_equal(actual$data, expected)
      expect_equal(dplyr::group_vars(actual$data), "Id")
      expect_length(actual$membership, nrow(raw))
    }
  }
  expect_identical(
    unserialize(record$raw_payload),
    unserialize(record$prepared_payload)
  )
})

test_that("editing applied hourly aggregation replays fifteen minutes from raw", {
  record <- m8_record(days = 1, interval = 60)
  context <- m8_context(record)
  first <- m8_preview(
    record,
    new_recipe(list(m8_aggregate_step(context, 1, "hours")))
  )
  second <- m8_preview(
    first$candidate,
    new_recipe(list(m8_aggregate_step(context)))
  )
  fresh <- m8_preview(record, second$candidate$recipe)
  expect_identical(
    second$candidate$prepared_payload,
    fresh$candidate$prepared_payload
  )
  expect_identical(second$candidate$raw_payload, record$raw_payload)
  expect_equal(second$comparison$steps$removed, 0)
  expect_equal(second$comparison$removed_rows, 0)
  expect_equal(
    second$comparison$represented_rows,
    nrow(unserialize(record$raw_payload))
  )
  expect_equal(nrow(second$comparison$view$gaps), 0)
  expect_match(second$comparison$view$caption, "aggregates")
  expect_equal(second$comparison$aggregation$coverage_percent, 100)
})

test_that("many-to-one provenance survives filters before and after aggregation", {
  record <- m8_record(days = 1, interval = 60)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  keep_first <- new_preparation_step(
    "variable",
    list(
      variable = "MEDI",
      mode = "range",
      values = character(),
      lower = 5,
      upper = 1400,
      outside = FALSE,
      missing = "keep"
    ),
    context
  )
  # Filter arguments are taken from the supported form contract.
  form <- preparation_form_defaults(context)
  form$step_type <- "variable"
  form$variable_lower <- 5
  form$variable_upper <- 1400
  keep_first <- new_preparation_step(
    "variable",
    preparation_form_arguments(form, context),
    context
  )
  form$variable_lower <- 100
  form$variable_upper <- 500
  keep_bins <- new_preparation_step(
    "variable",
    preparation_form_arguments(form, context),
    context
  )
  recipe <- new_recipe(list(keep_first, m8_aggregate_step(context), keep_bins))
  result <- evaluate_preparation(raw, recipe, context, TRUE)
  expect_true(any(result$removed_at == 1L))
  expect_true(any(result$removed_at == 3L))
  expect_false(any(result$removed_at == 2L))
  expect_equal(
    sum(result$removed_at == 0L),
    sum(!is.na(result$source_membership))
  )
  expect_true(all(
    result$source_membership[!is.na(result$source_membership)] %in%
      seq_len(nrow(result$data))
  ))
  expect_true(all(result$data$MEDI >= 100 & result$data$MEDI <= 500))
  expect_equal(
    result$steps$source_percent[3],
    100 * sum(result$removed_at == 3L) / nrow(raw)
  )
})

test_that("coverage uses original cadence, distinct timestamps and full bins", {
  start <- as.POSIXct("2026-01-01", tz = "UTC")
  raw <- data.frame(
    Id = "P1",
    Datetime = start + 0:29 * 60,
    MEDI = 1,
    file_group_id = "a"
  )
  data <- raw[c(3:5, 5, 20:30), ]
  data$MEDI[1] <- NA_real_
  reference <- preparation_variability_reference(raw)
  context <- m8_context()
  args <- m8_aggregate_step(context, missing = "available")$parameters$arguments
  result <- preparation_aggregate(data, args, reference, "MEDI")
  expect_equal(result$note$coverage$Observed_seconds, c(120, 660))
  expect_equal(
    result$note$coverage$Coverage_percent,
    c(100 * 2 / 15, 100 * 11 / 15)
  )
  expect_equal(result$note$intended_seconds, 1800)
  expect_equal(result$note$coverage_percent, 100 * 13 / 30)
  expect_equal(result$data$MEDI, c(1, 1))
  data <- raw[-(16:30), ]
  data$MEDI[] <- NA_real_
  expect_true(all(is.na(
    preparation_aggregate(data, args, reference, "MEDI")$data$MEDI
  )))
  expect_equal(
    preparation_aggregate(data, args, reference, "MEDI")$note$observed_seconds,
    0
  )
})

test_that("empty intervals count as missing without being inserted", {
  start <- as.POSIXct("2026-01-01", tz = "UTC")
  raw <- data.frame(
    Id = "P1",
    Datetime = start + 0:44 * 60,
    MEDI = 1,
    file_group_id = "a"
  )
  args <- m8_aggregate_step(m8_context())$parameters$arguments
  result <- preparation_aggregate(
    raw[-(16:30), ],
    args,
    preparation_variability_reference(raw),
    "MEDI"
  )
  expect_equal(nrow(result$data), 2)
  expect_equal(result$note$intended_seconds, 2700)
  expect_equal(result$note$observed_seconds, 1800)
  expect_equal(result$note$coverage_percent, 100 * 2 / 3)
})

test_that("calendar-day bins respect both DST transitions", {
  for (day in c("2026-03-29", "2026-10-25")) {
    start <- as.POSIXct(paste(day, "00:00:00"), tz = "Europe/Berlin")
    end <- start + lubridate::days(1)
    times <- seq(start, end, by = 60)
    times <- times[-length(times)]
    raw <- data.frame(Id = "P", Datetime = times, MEDI = 1, file_group_id = "a")
    args <- m8_aggregate_step(m8_context(), 1, "days")$parameters$arguments
    result <- preparation_aggregate(
      raw,
      args,
      preparation_variability_reference(raw),
      "MEDI"
    )
    expect_equal(nrow(result$data), 1)
    expect_equal(
      result$note$coverage$Window_seconds,
      as.numeric(end - start, units = "secs")
    )
    expect_equal(result$note$coverage_percent, 100)
    hourly <- preparation_aggregate(
      raw,
      m8_aggregate_step(m8_context(), 1, "hours")$parameters$arguments,
      preparation_variability_reference(raw),
      "MEDI"
    )
    expect_equal(nrow(hourly$data), if (day == "2026-03-29") 23 else 25)
    expect_equal(
      hourly$note$coverage$Window_seconds,
      rep(3600, nrow(hourly$data))
    )
    expect_equal(hourly$note$coverage_percent, 100)
  }
})

test_that("aggregation refuses unsupported work without dropping columns", {
  record <- m8_record()
  context <- m8_context(record)
  raw <- unserialize(record$raw_payload)
  args <- m8_aggregate_step(context, 1, "hours")$parameters$arguments
  expect_error(
    preparation_aggregation_arguments(modifyList(args, list(length = 7))),
    "supported"
  )
  expect_error(
    preparation_aggregate(
      raw,
      m8_aggregate_step(context)$parameters$arguments,
      preparation_variability_reference(raw),
      "MEDI"
    ),
    "does not upsample"
  )
  raw$List <- I(rep(list(1:2), nrow(raw)))
  expect_error(
    preparation_aggregate(
      raw,
      args,
      preparation_variability_reference(raw),
      "MEDI"
    ),
    "List"
  )
  expect_error(
    preparation_aggregate(
      unserialize(record$raw_payload),
      args,
      preparation_variability_reference(unserialize(record$raw_payload)),
      "MEDI",
      budget = 1
    ),
    "memory"
  )
})

test_that("gaps, empty data and derived history remain explicit", {
  record <- m8_record(days = 1, interval = 60)
  context <- m8_context(record)
  raw <- unserialize(record$raw_payload)
  raw$is.implicit <- rep(c(TRUE, FALSE), length.out = nrow(raw))
  args <- m8_aggregate_step(context)$parameters$arguments
  result <- preparation_aggregate(
    raw,
    args,
    preparation_variability_reference(raw),
    "MEDI"
  )
  expect_false(any(result$data$is.implicit))
  empty <- preparation_aggregate(
    raw[FALSE, ],
    args,
    preparation_variability_reference(raw),
    "MEDI"
  )
  expect_equal(nrow(empty$data), 0)
  creator <- new_preparation_step(
    "calibrate",
    list(
      variable = "MEDI",
      multiplier = 2,
      offset = 0,
      reference = "",
      output = "MEDI_processed"
    ),
    context
  )
  recipe <- new_recipe(list(m8_aggregate_step(context), creator))
  expect_equal(
    vapply(
      preparation_variable_lineage(recipe)$MEDI_processed$history,
      `[[`,
      "",
      "type"
    ),
    c("aggregate", "calibrate")
  )
  preview <- m8_preview(record, recipe)
  expect_equal(nrow(preview$comparison$value_impact), 0)
  expect_true(preview$comparison$aggregated)
})

test_that("aggregation Save Edit Apply and reset preserve one recipe", {
  record <- shiny::reactiveVal(m8_record(days = 1, interval = 60))
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
      acknowledge()
      m8_set_form(
        session,
        result$context(),
        "aggregate",
        aggregate_interval = "1 hours",
        aggregate_numeric = "sum",
        aggregate_missing = "threshold",
        aggregate_max_missing = 25,
        aggregate_categorical = "last",
        aggregate_logical = "any"
      )
      session$setInputs(save_new = 1L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      acknowledge()
      expect_length(result$recipe()$steps, 1)
      expect_true(result$preview()$comparison$aggregated)
      expect_equal(
        result$recipe()$steps[[1]]$parameters$arguments,
        list(
          length = 1,
          unit = "hours",
          numeric = "sum",
          missing = "threshold",
          categorical = "last",
          logical = "any",
          max_missing = 25
        )
      )
      expect_match(output$impact_summary$html, "Source rows represented")
      expect_match(
        output$aggregation_coverage$html,
        "Low coverage does not change"
      )
      expect_false(is.null(output$bin_coverage))
      session$setInputs(apply = 1L)
      m8_wait(session, function() record()$revision == 1L)
      id <- result$recipe()$steps[[1]]$id
      session$setInputs(selected_step = id, edit = 1L)
      session$flushReact()
      acknowledge()
      expect_equal(form()$aggregate_max_missing, 25)
      expect_equal(form()$aggregate_categorical, "last")
      expect_equal(form()$aggregate_logical, "any")
      session$setInputs(aggregate_interval = "15 minutes")
      session$setInputs(save_step = 2L)
      acknowledge()
      m8_wait(
        session,
        function()
          !is.null(result$preview()) &&
            result$recipe()$steps[[1]]$parameters$arguments$length == 15
      )
      acknowledge()
      expect_length(result$recipe()$steps, 1)
      session$setInputs(apply = 2L)
      m8_wait(session, function() record()$revision == 2L)
      fresh <- m8_preview(original, record()$recipe)
      expect_identical(
        record()$prepared_payload,
        fresh$candidate$prepared_payload
      )
      expect_identical(record()$raw_payload, original$raw_payload)
      applied <- record()
      session$setInputs(
        aggregate_missing = "threshold",
        aggregate_max_missing = 101
      )
      session$setInputs(save_step = 3L)
      expect_match(output$action_status$html, "maximum missing percentage")
      expect_identical(record(), applied)
      session$setInputs(aggregate_missing = "available")
      session$setInputs(save_step = 4L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        result$recipe()$steps[[1]]$parameters$arguments$missing,
        "available"
      )
      expect_equal(
        result$recipe()$steps[[1]]$parameters$arguments$max_missing,
        20
      )
      expect_false(grepl("is-error", output$action_status$html))
      session$setInputs(
        aggregate_missing = "threshold",
        aggregate_max_missing = NULL
      )
      session$setInputs(save_step = 5L)
      expect_match(output$action_status$html, "maximum missing percentage")
      session$setInputs(aggregate_missing = "propagate")
      session$setInputs(save_step = 6L)
      acknowledge()
      m8_wait(session, function() !is.null(result$preview()))
      expect_equal(
        result$recipe()$steps[[1]]$parameters$arguments$missing,
        "propagate"
      )
      expect_identical(record(), applied)
      session$setInputs(reset = 1L)
      m8_wait(session, function() length(record()$recipe$steps) == 0L)
      expect_identical(record()$prepared_payload, original$raw_payload)
    }
  )
})

test_that("multiple aggregation steps follow the explicit ordered pipeline", {
  record <- m8_record(days = 1, interval = 60)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  recipe <- new_recipe(list(
    m8_aggregate_step(context),
    m8_aggregate_step(context, 1, "hours")
  ))
  result <- evaluate_preparation(raw, recipe, context, details = TRUE)
  expected <- raw |>
    dplyr::group_by(Id, llw_source_dataset_id, file_group_id, file.name) |>
    LightLogR::aggregate_Datetime(unit = "15 minutes", type = "floor") |>
    LightLogR::aggregate_Datetime(unit = "1 hours", type = "floor") |>
    dplyr::ungroup()
  expect_equal(result$data, expected[, names(raw)])
  expect_true(all(result$removed_at == 0))
  expect_equal(sum(!is.na(result$source_membership)), nrow(raw))
  preview <- m8_preview(record, recipe)
  expect_length(preview$comparison$aggregations, 2)
  expect_false(preview$comparison$aggregations[[1]]$previous_aggregation)
  expect_true(preview$comparison$aggregations[[2]]$previous_aggregation)
  expect_equal(preview$comparison$removed_rows, 0)
})

test_that("longer clock bins use actual DST boundaries", {
  for (zone in c("Europe/Berlin", "Australia/Lord_Howe")) {
    for (day in if (zone == "Europe/Berlin") c("2026-03-29", "2026-10-25") else
      c("2026-04-05", "2026-10-04")) {
      start <- as.POSIXct(paste(day, "00:00:00"), tz = zone)
      end <- start + lubridate::days(1)
      time <- seq(start, end, by = 60)
      time <- time[-length(time)]
      raw <- data.frame(
        Id = "P",
        Datetime = time,
        MEDI = 1,
        file_group_id = "a"
      )
      for (hours in c(1, 2, 3, 6, 12)) {
        args <- list(
          length = hours,
          unit = "hours",
          numeric = "mean",
          missing = "propagate"
        )
        result <- preparation_aggregate(
          raw,
          args,
          preparation_variability_reference(raw),
          "MEDI"
        )
        expect_equal(
          sum(result$note$coverage$Window_seconds),
          as.numeric(end - start, units = "secs")
        )
        expect_equal(result$note$coverage_percent, 100)
      }
    }
  }
})

test_that("sequential aggregation retains actual input-bin duration across DST", {
  for (zone in c("Europe/Berlin", "Australia/Lord_Howe")) {
    days <- if (zone == "Europe/Berlin") c("2026-03-29", "2026-10-25") else
      c("2026-04-05", "2026-10-04")
    for (day in days) {
      start <- as.POSIXct(paste(day, "00:00:00"), tz = zone)
      end <- start + lubridate::days(1)
      time <- seq(start, end, by = 60)
      raw <- data.frame(
        Id = "P",
        Datetime = time[-length(time)],
        MEDI = 1,
        file_group_id = "a"
      )
      record <- new_dataset_record(
        raw,
        "DST aggregation",
        new_source_manifest("synthetic", source_timezone = zone),
        analysis_settings = list(primary_variable = "MEDI")
      )
      context <- m8_context(record)
      for (first in list(
        m8_aggregate_step(context, 2, "hours"),
        m8_aggregate_step(context, 1, "days")
      )) {
        recipe <- new_recipe(list(first, m8_aggregate_step(context, 1, "days")))
        result <- evaluate_preparation(raw, recipe, context, details = TRUE)
        note <- result$time_notes[[2]]
        expect_equal(
          note$observed_seconds,
          as.numeric(end - start, units = "secs")
        )
        expect_equal(note$coverage_percent, 100)
        expect_equal(result$data$MEDI, 1)
        expect_false(any(grepl("llw_bin", names(result$data))))
      }
    }
  }
})

test_that("retiming earlier aggregates reports unknown input support", {
  record <- m8_record(days = 1, interval = 60)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  for (method in c("with", "force")) {
    timezone <- new_preparation_step(
      "timezone",
      list(timezone = "UTC", method = method, ambiguous = "error"),
      context
    )
    recipe <- new_recipe(list(
      m8_aggregate_step(context),
      timezone,
      m8_aggregate_step(context, 1, "hours")
    ))
    result <- evaluate_preparation(raw, recipe, context, details = TRUE)
    note <- result$time_notes[[3]]
    expect_identical(note$retimed_support, method == "force")
    expect_identical(is.na(note$coverage_percent), method == "force")
    expect_true(all(is.finite(result$data$MEDI)))
    expect_equal(result$steps$removed, c(0, 0, 0))
  }
})

test_that("an epoch cannot contribute duration to the next output bin", {
  time <- as.POSIXct("2026-01-01 00:00:30", tz = "UTC") + 0:29 * 60
  raw <- data.frame(Id = "P", Datetime = time, MEDI = 1, file_group_id = "a")
  # 00:15:30 is absent. The epoch at 00:14:30 ends at its own bin edge,
  # and must not fill the first 30 seconds of the second bin.
  result <- preparation_aggregate(
    raw[-16, ],
    m8_aggregate_step(m8_context())$parameters$arguments,
    preparation_variability_reference(raw),
    "MEDI"
  )
  expect_equal(result$note$coverage$Observed_seconds, c(870, 810))
  expect_equal(result$note$observed_seconds, 1680)
  expect_equal(result$data$MEDI, c(1, 1))
})

test_that("Duration summaries preserve their type across odd even and missing bins", {
  seconds <- c(1:5, NA, 7, NA, 9, NA, rep(NA_real_, 5))
  raw <- data.frame(
    Id = "P",
    Datetime = as.POSIXct("2026-01-01", tz = "UTC") + 0:14 * 60,
    MEDI = 1:15,
    Duration = lubridate::dseconds(seconds),
    file_group_id = "a"
  )
  for (method in c("mean", "median", "min", "max", "sum")) {
    for (missing in c("propagate", "available")) {
      args <- m8_aggregate_step(
        m8_context(),
        5,
        "minutes",
        method,
        missing
      )$parameters$arguments
      result <- preparation_aggregate(
        raw,
        args,
        preparation_variability_reference(raw),
        "MEDI"
      )
      expected <- vapply(
        split(seconds, rep(1:3, each = 5)),
        function(x) {
          if (missing == "available") x <- x[!is.na(x)]
          if (!length(x) || anyNA(x)) NA_real_ else get(method)(x)
        },
        numeric(1)
      )
      expect_s4_class(result$data$Duration, "Duration")
      expect_equal(as.numeric(result$data$Duration), unname(expected))
      expect_equal(result$data$Datetime, raw$Datetime[c(1, 6, 11)])
    }
  }
})
