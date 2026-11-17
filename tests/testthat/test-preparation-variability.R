variability_fixture <- function() {
  data <- data.frame(
    Id = rep(c("A", "B"), each = 20),
    Datetime = rep(
      as.POSIXct("2026-10-25 00:00:00", tz = "UTC") + 0:19 * 60,
      2
    ),
    MEDI = rep(100:119, 2),
    Movement = rep(c(rep(0, 10), rep(c(0, 10), 5)), 2),
    file.name = rep(c("a.csv", "b.csv"), each = 20)
  )
  data$Datetime <- lubridate::with_tz(data$Datetime, "Europe/Berlin")
  new_dataset_record(
    dplyr::group_by(data, Id),
    "Variability fixture",
    new_source_manifest("synthetic", source_timezone = "Europe/Berlin"),
    analysis_settings = list(primary_variable = "MEDI"),
    factual_metadata = list(
      variables = list(MEDI = list(unit = "lx", label = "Melanopic EDI"))
    )
  )
}

variability_step <- function(context, ...) {
  args <- utils::modifyList(
    list(
      variable = "MEDI",
      source = "Movement",
      length = 5,
      unit = "minutes",
      alignment = "centered",
      statistic = "sd",
      threshold = 1,
      direction = "below",
      coverage = 80
    ),
    list(...)
  )
  new_preparation_step("variability", args, context)
}

test_that("rolling SD matches direct sample SD for elapsed-time windows", {
  set.seed(13)
  time <- sort(sample(0:700, 450)) * 10
  value <- rnorm(length(time), mean = 7, sd = 2)
  value[c(20, 30, 41)] <- c(NA, NaN, Inf)
  for (alignment in c("centered", "trailing", "leading")) {
    members <- function(t) switch(alignment,
      centered = time >= t - 90 & time < t + 90,
      trailing = time > t - 180 & time <= t,
      leading = time >= t & time < t + 180
    )
    actual <- preparation_rolling_variability(time, value, 180, alignment, 10)
    expected <- vapply(
      time,
      function(t) {
        stats::sd(value[members(t) & is.finite(value)])
      },
      numeric(1)
    )
    expect_equal(actual$statistic, expected, tolerance = 1e-10)
    # A large additive offset must not collapse a small but real variance.
    shifted <- round(value, 2) + 1e10
    stable <- preparation_rolling_variability(time, shifted, 180, alignment, 10)
    oracle <- vapply(
      time,
      function(t) {
        stats::sd(shifted[members(t) & is.finite(shifted)])
      },
      numeric(1)
    )
    expect_equal(stable$statistic, oracle, tolerance = 1e-6)
  }
})

test_that("constant and very small variable windows survive removal roundoff", {
  set.seed(32)
  for (i in 1:20) {
    value <- c(rnorm(30), rep(0, 20))
    actual <- preparation_rolling_variability(
      seq_along(value),
      value,
      5,
      "trailing",
      1
    )
    expect_identical(actual$statistic[36:50], rep(0, 15))
  }
  value <- c(1e10, 1e10 + 2, rep(c(1e-12, 2e-12, 3e-12), 10))
  actual <- preparation_rolling_variability(
    seq_along(value),
    value,
    5,
    "trailing",
    1
  )
  expected <- vapply(
    7:length(value),
    function(i) stats::sd(value[(i - 4):i]),
    numeric(1)
  )
  expect_equal(
    actual$statistic[7:length(value)] / expected,
    rep(1, length(expected)),
    tolerance = 1e-12
  )
  expect_true(all(actual$statistic[7:length(value)] > 0))
  value <- rep(c(0.1, 0.3, -0.1, -0.3), 30)
  actual <- preparation_rolling_variability(
    seq_along(value),
    value,
    4,
    "trailing",
    1
  )
  expect_identical(actual$mean[5:length(value)], rep(0, length(value) - 4))
  value <- value + 1e-12
  actual <- preparation_rolling_variability(
    seq_along(value),
    value,
    4,
    "trailing",
    1
  )
  expected <- vapply(
    5:length(value),
    function(i) mean(value[(i - 3):i]),
    numeric(1)
  )
  expect_equal(actual$mean[5:length(value)], expected, tolerance = 0)
  expect_true(all(actual$mean[5:length(value)] > 0))
})

test_that("rolling boundary results retain their typed diagnostic columns", {
  empty <- data.frame(
    statistic = numeric(),
    mean = numeric(),
    coverage = numeric(),
    observations = integer(),
    first = integer(),
    last = integer(),
    duplicate = logical(),
    constant = logical()
  )
  expect_identical(
    preparation_rolling_variability(numeric(), numeric(), 60, "centered", 60),
    empty
  )
  unknown <- data.frame(
    statistic = rep(NA_real_, 3),
    mean = rep(NA_real_, 3),
    coverage = rep(0, 3),
    observations = integer(3),
    first = integer(3),
    last = integer(3),
    duplicate = rep(FALSE, 3),
    constant = rep(FALSE, 3)
  )
  for (epoch in c(NA_real_, 0, -1, Inf))
    expect_identical(
      preparation_rolling_variability(0:2 * 60, 1:3, 60, "trailing", epoch),
      unknown
    )
  singleton <- preparation_rolling_variability(0, 1, 60, "centered", 60)
  expect_identical(
    vapply(singleton, typeof, character(1)),
    vapply(empty, typeof, character(1))
  )
  expect_identical(singleton$observations, 1L)
  expect_identical(singleton$statistic, NA_real_)
})

test_that("subprecision positive windows preserve targets as unevaluable", {
  record <- variability_fixture()
  raw <- dataset_raw_data(record)
  reference <- preparation_variability_reference(raw)
  for (alignment in c("centered", "trailing", "leading")) {
    step <- variability_step(
      m8_context(record),
      length = 1e-10,
      unit = "seconds",
      alignment = alignment,
      coverage = 0
    )
    result <- preparation_mask_variability(
      raw,
      step$parameters$arguments,
      reference
    )
    expect_identical(result$data, raw)
    expect_false(any(result$diagnostics$evaluable))
  }
})

test_that("coverage measures observed time without filling missing epochs", {
  time <- 0:9 * 60
  complete <- preparation_window_coverage(
    time,
    rep(TRUE, 10),
    60,
    c(-30, 30, 60, -150),
    c(270, 330, 360, 150)
  )
  expect_equal(complete, c(100, 100, 100, 60))
  observed <- rep(TRUE, 10)
  observed[3] <- FALSE
  explicit <- preparation_window_coverage(time, observed, 60, -30, 270)
  implicit <- preparation_window_coverage(time[-3], rep(TRUE, 9), 60, -30, 270)
  expect_equal(explicit, 80)
  expect_equal(implicit, explicit)
  # Crowded timestamps cannot account for more than their actual occupied time.
  expect_lt(
    preparation_window_coverage(c(0, 1, 2), rep(TRUE, 3), 60, -150, 150),
    25
  )
})

test_that("source-driven masking preserves rows, target metadata and source values", {
  record <- variability_fixture()
  context <- m8_context(record)
  raw <- dataset_raw_data(record)
  step <- variability_step(context, output = "MEDI_processed")
  evaluated <- evaluate_preparation(raw, new_recipe(list(step)), context, TRUE)
  data <- evaluated$data
  expect_identical(data[names(raw)], raw)
  expect_identical(dplyr::group_vars(data), "Id")
  expect_true(all(is.na(data$MEDI_processed[c(2:8, 22:28)])))
  expect_identical(
    data$MEDI_processed[c(1, 20, 21, 40)],
    raw$MEDI[c(1, 20, 21, 40)]
  )
  expect_true(evaluated$steps$unevaluable_windows > 0)
  expect_equal(
    evaluated$steps$evaluated_windows + evaluated$steps$unevaluable_windows,
    nrow(raw)
  )
  expect_match(evaluated$steps$variability_notes, "insufficient coverage")
  preview <- m8_preview(record, new_recipe(list(step)))
  expect_identical(preview$candidate$raw_payload, record$raw_payload)
  metadata <- metadata_resolve_variable(preview$candidate, "MEDI_processed")
  expect_identical(metadata$source_variable, "MEDI")
  expect_identical(metadata$unit, "lx")
  expect_identical(
    metadata$transformation_history[[1]]$arguments$source,
    "Movement"
  )
  expect_equal(
    preview$comparison$invalidated_values,
    sum(is.na(data$MEDI_processed))
  )
  expect_identical(
    dataset_prepared_data(reset_dataset_record(preview$candidate)),
    raw
  )
})

test_that("unevaluable-window policy preserves legacy results or masks the target", {
  record <- variability_fixture()
  context <- m8_context(record)
  raw <- dataset_raw_data(record)
  reference <- preparation_variability_reference(raw)
  cases <- list(
    complete = raw,
    explicit_gap = raw,
    implicit_gap = raw[-(3:7), ],
    duplicates = raw,
    missing_time = raw,
    zero_mean = raw,
    singleton = raw[c(1, 21), ]
  )
  cases$explicit_gap$Movement[3:7] <- NA_real_
  cases$duplicates$Datetime[6] <- cases$duplicates$Datetime[5]
  cases$missing_time$Datetime[6] <- NA
  cases$zero_mean$Movement <- 0
  for (name in names(cases))
    for (output in c("MEDI", "MEDI_processed")) {
      data <- cases[[name]]
      data$MEDI[1] <- NA_integer_
      ref <- if (name == "singleton")
        preparation_variability_reference(data) else reference
      old <- variability_step(
        context,
        statistic = if (name == "zero_mean") "cv" else "sd"
      )
      if (output != "MEDI")
        old <- variability_step(
          context,
          statistic = if (name == "zero_mean") "cv" else "sd",
          output = output
        )
      legacy <- preparation_mask_variability(
        data,
        old$parameters$arguments,
        ref
      )
      args <- old$parameters$arguments
      args$unevaluable <- "keep"
      keep <- preparation_mask_variability(data, args, ref)
      expect_identical(keep, legacy, info = paste(name, output))
      args$no_change <- "rule"
      expect_identical(preparation_mask_variability(data, args, ref), legacy)
      args$unevaluable <- "missing"
      masked <- preparation_mask_variability(data, args, ref)
      expected <- legacy$data[[output]]
      expected[!legacy$diagnostics$evaluable] <- NA
      expect_identical(
        masked$data[[output]],
        expected,
        info = paste(name, output)
      )
      expect_identical(masked$diagnostics, legacy$diagnostics)
      expect_identical(
        masked$data[setdiff(names(data), output)],
        data[setdiff(names(data), output)]
      )
      expect_identical(dplyr::group_vars(masked$data), dplyr::group_vars(data))
    }
  step <- variability_step(
    context,
    output = "MEDI_processed",
    unevaluable = "missing"
  )
  expect_identical(step$parameters$mapping_version, 3L)
  expect_silent(validate_preparation_recipe(new_recipe(list(step)), context))
  preview <- m8_preview(record, new_recipe(list(step)))
  metadata <- metadata_resolve_variable(preview$candidate, "MEDI_processed")
  expect_identical(
    metadata$transformation_history[[1]]$arguments$unevaluable,
    "missing"
  )
  expect_match(preparation_step_label(step, 1), "unevaluable: NA")
  expect_identical(preview$candidate$raw_payload, record$raw_payload)
  args <- step$parameters$arguments
  args$output <- NULL
  retargeted <- preparation_step_arguments(step, args)
  expect_identical(retargeted$parameters$mapping_version, 3L)
  expect_silent(validate_preparation_recipe(
    new_recipe(list(retargeted)),
    context
  ))
  invalid <- retargeted
  invalid$parameters$mapping_version <- 1L
  expect_error(
    validate_preparation_recipe(new_recipe(list(invalid)), context),
    "version 3"
  )
  invalid <- retargeted
  invalid$parameters$arguments$unevaluable <- NULL
  expect_error(
    validate_preparation_recipe(new_recipe(list(invalid)), context),
    "version 3"
  )
})

test_that("CV uses the absolute mean and leaves undefined windows unchanged", {
  raw <- dataset_raw_data(variability_fixture())
  raw$Movement <- rep(c(-1, -2, -3, -4, -5), 8)
  reference <- preparation_variability_reference(raw)
  context <- m8_context(variability_fixture())
  args <- variability_step(
    context,
    statistic = "cv",
    threshold = 40,
    direction = "above"
  )$parameters$arguments
  result <- preparation_mask_variability(raw, args, reference)
  expect_equal(result$diagnostics$statistic[4], 100 * stats::sd(-1:-5) / 3)
  expect_true(is.na(result$data$MEDI[4]))
  raw$Movement <- 0
  zero <- preparation_mask_variability(raw, args, reference)
  expect_identical(zero$data$MEDI, raw$MEDI)
  expect_false(any(zero$diagnostics$evaluable))
  expect_true("undefined CV" %in% zero$diagnostics$reason)
  args$statistic <- "sd"
  args$threshold <- 0
  equal <- preparation_mask_variability(raw, args, reference)
  expect_identical(equal$data$MEDI, raw$MEDI)
  args$direction <- "below"
  equal <- preparation_mask_variability(raw, args, reference)
  expect_identical(equal$data$MEDI, raw$MEDI)
})

test_that("forward CV separates constant sources from invalid windows at each anchor", {
  record <- variability_fixture()
  raw <- dataset_raw_data(record)
  raw$Movement[1:10] <- c(0, 0, 2, 2, -2, 2, NA, 5, 5.001, 5)
  raw$Movement[11:20] <- 0
  raw$MEDI[12] <- NA_integer_
  ref <- preparation_variability_reference(raw)
  step <- variability_step(m8_context(record), length = 2, alignment = "leading",
    statistic = "cv", threshold = 5, coverage = 80, no_change = "keep",
    unevaluable = "missing", output = "checked")
  args <- step$parameters$arguments
  keep <- preparation_mask_variability(raw, args, ref)
  rows <- c(1L, 2L, 3L, 4L, 8L, 12L, 19L, 20L)
  expect_identical(keep$diagnostics$reason[rows], c(
    "no change", "assessed", "no change", "undefined CV", "assessed",
    "no change", "insufficient coverage", "insufficient coverage"
  ))
  # The reading exactly two minutes ahead belongs to the next window.
  expect_identical(keep$data$checked[rows], c(100L, 101L, 102L, NA_integer_,
    NA_integer_, NA_integer_, NA_integer_, NA_integer_))
  expect_true(is.na(keep$diagnostics$statistic[1L]))
  expect_equal(keep$diagnostics$statistic[3L], 0)
  args$no_change <- "missing"
  args$unevaluable <- "keep"
  mask <- preparation_mask_variability(raw, args, ref)
  expect_identical(mask$diagnostics, keep$diagnostics)
  expect_identical(mask$data$checked[rows], c(NA_integer_, 101L, NA_integer_,
    103L, NA_integer_, NA_integer_, 118L, 119L))
  expect_identical(mask$data[names(raw)], raw)
  # Only supported windows can use the constant policy, even for all-zero data.
  duplicate <- raw
  duplicate$Datetime[14] <- duplicate$Datetime[13]
  expect_identical(preparation_mask_variability(duplicate, args, ref)$diagnostics$reason[13],
    "duplicate timestamps")
  single <- raw[1L, ]
  expect_identical(preparation_mask_variability(single, args,
    preparation_variability_reference(single))$diagnostics$reason, "unknown sampling interval")
  args$coverage <- 0
  expect_identical(preparation_mask_variability(raw, args, ref)$diagnostics$reason[20],
    "fewer than two finite observations")
  # Flatness uses exact readings even if their sample variance underflows.
  tiny <- preparation_rolling_variability(0:2, c(0, 1e-200, 1e-200), 2, "leading", 1)
  expect_identical(tiny$constant, c(FALSE, TRUE, FALSE))
})

test_that("forward and no-change settings survive recipe validation and retargeting", {
  record <- variability_fixture()
  context <- m8_context(record)
  for (step in list(variability_step(context, alignment = "leading"),
                    variability_step(context, no_change = "missing"))) {
    expect_identical(step$parameters$mapping_version, 4L)
    args <- step$parameters$arguments
    args$output <- "MEDI_checked"
    revised <- preparation_step_arguments(step, args)
    expect_identical(revised$parameters$mapping_version, 4L)
    recipe <- new_recipe(list(revised))
    expect_silent(validate_preparation_recipe(recipe, context))
    saved <- m8_preview(record, recipe)$candidate$recipe$steps[[1L]]
    expect_identical(saved$parameters$arguments, args)
    revised$parameters$mapping_version <- 3L
    expect_error(validate_preparation_recipe(new_recipe(list(revised)), context),
      "Version 4")
  }
})

test_that("gaps and duplicate timestamps remain unevaluable with unchanged targets", {
  record <- variability_fixture()
  raw <- dataset_raw_data(record)
  context <- m8_context(record)
  reference <- preparation_variability_reference(raw)
  args <- variability_step(context)$parameters$arguments
  missing <- raw
  missing$Movement[3:7] <- NA_real_
  explicit <- preparation_mask_variability(missing, args, reference)
  implicit <- preparation_mask_variability(raw[-(3:7), ], args, reference)
  expect_equal(
    implicit$diagnostics$coverage,
    explicit$diagnostics$coverage[-(3:7)]
  )
  expect_identical(implicit$data$MEDI[1:3], raw$MEDI[c(1, 2, 8)])
  duplicate <- raw
  duplicate$Datetime[6] <- duplicate$Datetime[5]
  dup <- preparation_mask_variability(duplicate, args, reference)
  expect_identical(dup$data$MEDI[5:6], raw$MEDI[5:6])
  expect_identical(dup$diagnostics$reason[5:6], rep("duplicate timestamps", 2))
  only_one <- raw[c(1, 21), ]
  single <- preparation_mask_variability(
    only_one,
    args,
    preparation_variability_reference(only_one)
  )
  expect_identical(single$data$MEDI, only_one$MEDI)
  expect_true(all(single$diagnostics$reason == "unknown sampling interval"))
  empty <- preparation_mask_variability(raw[FALSE, ], args, reference)
  expect_identical(empty$data, raw[FALSE, ])
})

test_that("files and current groups are isolated and order is preserved", {
  record <- variability_fixture()
  raw <- dataset_raw_data(record)
  raw$Id <- "A"
  raw$Movement[21:40] <- 100
  raw$Phase <- rep(rep(c("first", "second"), each = 10), 2)
  raw <- dplyr::group_by(raw, Phase)
  context <- m8_context(record)
  args <- variability_step(context, threshold = 0.1)$parameters$arguments
  reference <- preparation_variability_reference(raw)
  ordered <- preparation_mask_variability(raw, args, reference)
  set.seed(5)
  order <- sample(seq_len(nrow(raw)))
  shuffled <- preparation_mask_variability(raw[order, ], args, reference)
  expect_identical(shuffled$data, ordered$data[order, ])
  expect_equal(
    shuffled$diagnostics,
    ordered$diagnostics[order, ],
    ignore_attr = TRUE
  )
  expect_false(ordered$diagnostics$evaluable[10])
  expect_equal(ordered$diagnostics$statistic[25], 0)
  # Repeat a clock hour across DST. Windows still use elapsed seconds.
  time <- as.numeric(seq(
    as.POSIXct("2026-10-25 00:30:00", tz = "UTC"),
    by = 60,
    length.out = 120
  ))
  stats <- preparation_rolling_variability(time, 1:120, 300, "centered", 60)
  expect_equal(stats$statistic[60], stats::sd(58:62))
})

test_that("source pinning and fallback are independent of target chain changes", {
  record <- variability_fixture()
  context <- m8_context(record)
  creator <- new_preparation_step(
    "calibrate",
    list(
      variable = "Movement",
      multiplier = 2,
      offset = 0,
      reference = "",
      output = "Movement_processed"
    ),
    context
  )
  mask <- variability_step(context, source = "Movement_processed")
  previous <- new_recipe(list(creator, mask))
  reordered <- preparation_reconnect_order(
    previous,
    new_recipe(list(mask, creator)),
    context
  )
  expect_identical(reordered$steps[[1]]$parameters$arguments$source, "Movement")
  expect_identical(reordered$steps[[1]]$parameters$arguments$variable, "MEDI")
  expect_silent(validate_preparation_recipe(reordered, context))
  removed <- preparation_reconnect_sources(
    previous,
    new_recipe(list(mask)),
    context
  )
  expect_identical(removed$steps[[1]]$parameters$arguments$source, "Movement")
  expect_match(
    preparation_chain_changes(previous, removed),
    "variability source: Movement"
  )
  pinned <- preparation_reconnect_sources(previous, previous, context)
  expect_identical(pinned, previous)
  changed <- mask
  changed$parameters$arguments$variable <- "Movement"
  changed_recipe <- preparation_reconnect_sources(
    previous,
    new_recipe(list(creator, changed)),
    context
  )
  expect_identical(
    changed_recipe$steps[[2]]$parameters$arguments$source,
    "Movement_processed"
  )
  # Moving a target consumer earlier can make it the creator. Source still
  # falls back, rather than mistakenly reading its own not-yet-created output.
  creator$parameters$arguments$variable <- "MEDI"
  creator$parameters$arguments$output <- "MEDI_processed"
  mask <- variability_step(
    context,
    variable = "MEDI_processed",
    source = "MEDI_processed"
  )
  original <- new_recipe(list(creator, mask))
  moved <- preparation_reconnect_order(
    original,
    new_recipe(list(mask, creator)),
    context
  )
  expect_identical(moved$steps[[1]]$parameters$arguments$variable, "MEDI")
  expect_identical(
    moved$steps[[1]]$parameters$arguments$output,
    "MEDI_processed"
  )
  expect_identical(moved$steps[[1]]$parameters$arguments$source, "MEDI")
  expect_silent(validate_preparation_recipe(moved, context))
})

test_that("variability validates typed arguments and refuses output collisions", {
  record <- variability_fixture()
  context <- m8_context(record)
  for (patch in list(
    list(length = 0),
    list(length = Inf),
    list(coverage = 101),
    list(coverage = -1),
    list(threshold = -1),
    list(statistic = "mean"),
    list(alignment = "bad"),
    list(direction = "bad"),
    list(no_change = "bad"),
    list(no_change = NA_character_),
    list(unevaluable = "bad"),
    list(unevaluable = NA_character_),
    list(unit = "days")
  ))
    expect_error(
      do.call(variability_step, c(list(context), patch)),
      class = "llw_error"
    )
  expect_error(
    validate_preparation_recipe(
      new_recipe(list(variability_step(context, source = "absent"))),
      context
    ),
    "source.*unavailable"
  )
  expect_error(
    validate_preparation_recipe(
      new_recipe(list(variability_step(context, output = "Movement"))),
      context
    ),
    "already exists"
  )
  defaults <- preparation_form_defaults(context)
  expect_identical(defaults$variability_source, defaults$measurement_variable)
  expect_identical(defaults$measurement_output, defaults$measurement_variable)
  expect_equal(defaults$variability_coverage, 80)
  expect_identical(defaults$variability_unevaluable, "keep")
  expect_identical(defaults$variability_no_change, "rule")
})

test_that("strict threshold equality uses a direct check at floating-point boundaries", {
  data <- data.frame(
    Id = "A",
    Datetime = as.POSIXct("2026-01-01", tz = "UTC") +
      c(0, 1, 2, 60, 120, 180, 240, 300),
    MEDI = 10,
    Movement = c(1, 2, 3, 1, 2, 3, 1, 2)
  )
  record <- new_dataset_record(
    data,
    "Threshold equality",
    new_source_manifest("synthetic", source_timezone = "UTC")
  )
  context <- m8_context(record)
  step <- variability_step(context, coverage = 0, direction = "above")
  evaluated <- evaluate_preparation(data, new_recipe(list(step)), context, TRUE)
  expect_identical(stats::sd(c(3, 1, 2)), 1)
  expect_equal(tail(evaluated$data$MEDI, 1), 10)
  step$parameters$arguments$threshold <- 1 - 1e-12
  evaluated <- evaluate_preparation(data, new_recipe(list(step)), context, TRUE)
  expect_true(is.na(tail(evaluated$data$MEDI, 1)))
  step$parameters$arguments$threshold <- 1 + 1e-12
  step$parameters$arguments$direction <- "below"
  evaluated <- evaluate_preparation(data, new_recipe(list(step)), context, TRUE)
  expect_true(is.na(tail(evaluated$data$MEDI, 1)))
})

test_that("variability saves, previews, edits and applies through the module", {
  original <- variability_fixture()
  record <- shiny::reactiveVal(original)
  runtime <- new_session_runtime(resolve_runtime_profile(
    "hosted",
    workers = 1L
  ))
  on.exit(runtime$cleanup(), add = TRUE)
  if (inherits(runtime$startup_error, "llw_error"))
    skip("mirai worker sockets are unavailable")
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
        "variability",
        measurement_output = "MEDI_processed",
        variability_source = "Movement",
        variability_length = 5
      )
      session$setInputs(save_new = 1L)
      expect_null(editor_error())
      m8_set_form(
        session,
        preparation_new_step_context(
          result$context(),
          result$recipe(),
          variable_catalog()
        ),
        "variability",
        variability_length = 5
      )
      m8_wait(session, function() !is.null(result$preview()))
      step <- result$recipe()$steps[[1]]
      expect_identical(step$type, "variability")
      expect_identical(step$parameters$arguments$source, "Movement")
      expect_identical(step$parameters$arguments$output, "MEDI_processed")
      expect_match(as.character(output$comparison), "Unevaluable windows")
      expect_identical(record(), original)
      session$setInputs(apply = 1L)
      expect_true("MEDI_processed" %in% names(dataset_prepared_data(record())))
      expect_identical(record()$raw_payload, original$raw_payload)
      session$setInputs(selected_step = step$id, edit = 1L)
      expect_identical(form_base()$variability_source, "Movement")
      expect_equal(form_base()$variability_length, 5)
      m8_set_form(
        session,
        result$context(),
        "variability",
        measurement_output = "MEDI_processed",
        variability_source = "Movement",
        variability_length = 5
      )
      expect_null(loading_form())
      expect_false(result$dirty())
      session$setInputs(
        variability_statistic = "cv",
        variability_threshold = 50,
        variability_alignment = "leading",
        variability_no_change = "keep",
        variability_unevaluable = "missing"
      )
      session$setInputs(save_step = 1L)
      m8_wait(session, function() !is.null(result$preview()))
      expect_identical(
        result$recipe()$steps[[1]]$parameters$arguments$statistic,
        "cv"
      )
      expect_true(result$preview()$comparison$steps$unevaluable_windows > 0)
      expect_true(result$preview()$comparison$steps$no_change_windows > 0)
      expect_match(as.character(output$comparison), "Windows assessed as constant")
      updated <- result$recipe()$steps[[1]]
      expect_identical(updated$parameters$arguments$unevaluable, "missing")
      expect_identical(updated$parameters$arguments$no_change, "keep")
      expect_identical(updated$parameters$mapping_version, 4L)
      expect_match(output$recipe_choices$html, "target set to NA")
      session$setInputs(apply = 2L)
      session$setInputs(selected_step = step$id, edit = 2L)
      expect_identical(form_base()$variability_unevaluable, "missing")
      expect_identical(form_base()$variability_alignment, "leading")
      expect_identical(form_base()$variability_no_change, "keep")
    }
  )
})
