test_that("typed variable filters preserve values and make NA policy explicit", {
  record <- m8_record(edges = TRUE)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  args <- list(
    variable = "MEDI",
    mode = "range",
    values = character(),
    lower = 20,
    upper = 70,
    outside = FALSE,
    missing = "keep"
  )
  evaluate <- function(args)
    evaluate_preparation(
      raw,
      new_recipe(list(new_preparation_step("variable", args, context))),
      context
    )
  expect_identical(
    evaluate(args),
    raw[
      is.na(raw$MEDI) | (!is.na(raw$MEDI) & raw$MEDI >= 20 & raw$MEDI <= 70),
    ]
  )
  args$missing <- "drop"
  expect_identical(
    evaluate(args),
    raw[!is.na(raw$MEDI) & raw$MEDI >= 20 & raw$MEDI <= 70, ]
  )
  args$outside <- TRUE
  expect_identical(
    evaluate(args),
    raw[!is.na(raw$MEDI) & (raw$MEDI < 20 | raw$MEDI > 70), ]
  )
  args$missing <- "only"
  expect_identical(evaluate(args), raw[is.na(raw$MEDI), ])
  args <- list(
    variable = "State",
    mode = "categories",
    values = "sleep",
    lower = 0,
    upper = 1,
    outside = FALSE,
    missing = "drop"
  )
  expect_identical(evaluate(args), raw[raw$State == "sleep", ])
  args$mode <- c("range", "categories")
  expect_error(evaluate(args), "mode")
  expect_identical(record$raw_payload, serialize(raw, NULL, version = 3L))
})

test_that("day and night use a public solar calculation or an existing classification", {
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  args <- list(
    method = "coordinates",
    state = "day",
    latitude = 48.52,
    longitude = 9.06,
    solar_depression = 6,
    missing = "drop"
  )
  recipe <- new_recipe(list(new_preparation_step("photoperiod", args, context)))
  reference <- LightLogR::add_photoperiod(
    data.frame(Datetime = raw$Datetime),
    coordinates = c(48.52, 9.06),
    solarDep = 6
  )$photoperiod.state
  expect_identical(
    evaluate_preparation(raw, recipe, context),
    raw[!is.na(reference) & reference == "day", ]
  )
  raw$photoperiod.state <- reference
  raw$photoperiod.state[c(1, 2)] <- NA
  context$has_photoperiod <- TRUE
  args$method <- "existing"
  args$state <- "night"
  args$missing <- "keep"
  recipe <- new_recipe(list(new_preparation_step("photoperiod", args, context)))
  expect_identical(
    evaluate_preparation(raw, recipe, context),
    raw[
      is.na(raw$photoperiod.state) |
        (!is.na(raw$photoperiod.state) & raw$photoperiod.state == "night"),
    ]
  )
  args$latitude <- 91
  args$method <- "coordinates"
  expect_error(new_preparation_step("photoperiod", args, context), "latitude")
})

test_that("all coverage bases preserve current groups and match the public function", {
  # Named edge case: a missing span, NA values, duplicate times and grouping
  # that intentionally differs from the participant/source identity.
  original <- unserialize(m8_record(edges = TRUE)$raw_payload)
  for (groups in list(character(), "Id", c("Id", "State"))) {
    raw <- dplyr::group_by(original, dplyr::across(dplyr::all_of(groups)))
    record <- new_dataset_record(
      raw,
      "Synthetic coverage grouping check",
      new_source_manifest("synthetic", source_timezone = "Europe/Berlin"),
      analysis_settings = list(primary_variable = "MEDI")
    )
    context <- m8_context(record)
    expect_identical(context$group_columns, groups)
    for (basis in c(
      "full_day",
      "observed_span",
      "full_group",
      "observed_group"
    )) {
      args <- list(variable = "MEDI", minimum = 90, basis = basis)
      step <- new_preparation_step("coverage", args, context)
      expect_identical(step$parameters$mapping_version, 2L)
      recipe <- new_recipe(list(step))
      reference <- raw
      reference$.index <- seq_len(nrow(raw))
      reference <- LightLogR::remove_partial_data(
        reference,
        Variable.colname = MEDI,
        threshold.missing = 1 - args$minimum / 100,
        by.date = basis %in% c("full_day", "observed_span"),
        show.result = FALSE,
        handle.gaps = basis %in% c("full_day", "full_group")
      )
      actual <- evaluate_preparation(raw, recipe, context)
      expect_identical(actual, raw[sort(unique(reference$.index)), ])
      expect_identical(dplyr::group_vars(actual), groups)
      result <- m8_preview(record, recipe)
      expect_identical(result$candidate$raw_payload, record$raw_payload)
      expect_equal(result$comparison$removed_rows, nrow(raw) - nrow(reference))
    }
  }
})

test_that("legacy daily coverage retains its stored identity grouping", {
  record <- m8_record(edges = TRUE)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  for (basis in c("full_day", "observed_span")) {
    args <- list(variable = "MEDI", minimum = 90, basis = basis)
    step <- new_preparation_step("coverage", args, context)
    step$parameters$mapping_version <- 1L
    recipe <- new_recipe(list(step))
    reference <- raw
    reference$.index <- seq_len(nrow(raw))
    reference <- dplyr::group_by(
      reference,
      dplyr::across(dplyr::all_of(context$identity_columns))
    )
    reference <- LightLogR::remove_partial_data(
      reference,
      Variable.colname = MEDI,
      threshold.missing = 1 - args$minimum / 100,
      by.date = TRUE,
      show.result = FALSE,
      handle.gaps = basis == "full_day"
    )
    expect_identical(
      evaluate_preparation(raw, recipe, context),
      raw[sort(unique(reference$.index)), ]
    )
    recipe$steps[[1]]$parameters$arguments$basis <- "full_group"
    expect_error(validate_preparation_recipe(recipe, context), "version 2")
  }
})

test_that("whole-group coverage distinguishes gaps and removes complete groups", {
  # Named edge case: one observed day followed by a short second day, plus a
  # complete group with missing values and a singleton group.
  times <- seq(as.POSIXct("2026-01-01", tz = "UTC"), by = 1800, length.out = 96)
  raw <- data.frame(
    Id = rep(c("A", "B", "C"), c(60, 96, 1)),
    Datetime = c(times[seq_len(60)], times, times[[1]]),
    MEDI = c(rep(1, 60), rep(1, 48), rep(NA_real_, 48), 1)
  ) |>
    dplyr::group_by(Id)
  record <- new_dataset_record(
    raw,
    "Synthetic coverage boundaries",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI")
  )
  context <- m8_context(record)
  results <- lapply(
    c("full_day", "observed_span", "full_group", "observed_group"),
    function(basis) {
      recipe <- new_recipe(list(new_preparation_step(
        "coverage",
        list(variable = "MEDI", minimum = 75, basis = basis),
        context
      )))
      evaluate_preparation(raw, recipe, context)
    }
  )
  expect_gt(nrow(results[[1]]), nrow(results[[3]]))
  expect_gt(nrow(results[[4]]), nrow(results[[3]]))
  expect_false(any(vapply(results, function(x) "C" %in% x$Id, logical(1))))
  expect_setequal(unique(results[[4]]$Id), "A")
  expect_equal(nrow(results[[4]]), sum(raw$Id == "A"))
  sparse <- raw[raw$Id == "A", ]
  sparse$Datetime[[2]] <- sparse$Datetime[[1]] + 0.001
  expect_error(
    preparation_filter_coverage(
      sparse,
      list(variable = "MEDI", minimum = 75, basis = "full_group"),
      context
    ),
    "too many temporary rows"
  )
})

test_that("step accounting and informative days describe the full source", {
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  step1 <- new_preparation_step(
    "variable",
    list(
      variable = "MEDI",
      mode = "range",
      values = character(),
      lower = 40,
      upper = 105,
      outside = FALSE,
      missing = "drop"
    ),
    context
  )
  step2 <- m8_clock(context)
  recipe <- new_recipe(list(step1, step2))
  result <- m8_preview(record, recipe)
  first_keep <- raw$MEDI >= 40 & raw$MEDI <= 105
  clock <- format(raw$Datetime, "%H:%M:%S", tz = "Europe/Berlin")
  second_keep <- clock >= "22:00:00" | clock < "06:00:00"
  expect_equal(
    result$comparison$steps$removed,
    c(sum(!first_keep), sum(first_keep & !second_keep))
  )
  expect_equal(
    result$comparison$steps$source_percent,
    100 * c(sum(!first_keep), sum(first_keep & !second_keep)) / nrow(raw)
  )
  expect_equal(
    result$comparison$steps$previous_percent,
    c(
      100 * mean(!first_keep),
      100 * sum(first_keep & !second_keep) / sum(first_keep)
    )
  )
  expect_equal(
    result$comparison$removed_at,
    ifelse(!first_keep, 1L, ifelse(!second_keep, 2L, 0L))
  )
  view <- result$comparison$view
  partial <- view$days[
    view$days$Removed > 0 & view$days$Removed < view$days$Rows,
  ]
  selected <- partial[partial$Id == view$id & partial$Day == view$day, ]
  expect_equal(abs(selected$Fraction - .51), min(abs(partial$Fraction - .51)))
  expect_identical(
    view$plot$Value,
    raw$MEDI[
      as.character(raw$Id) == view$id &
        format(raw$Datetime, "%Y-%m-%d", tz = context$timezone) == view$day
    ]
  )
  manual <- preparation_day_view(
    raw,
    result$comparison$removed_at,
    context,
    list(id = "P02", day = "2026-03-28", step = 1L)
  )
  expect_equal(manual$id, "P02")
  expect_equal(manual$day, "2026-03-28")
  expect_true(all(
    manual$plot$Kept == (manual$plot$Value >= 40 & manual$plot$Value <= 105)
  ))
  expect_equal(preparation_recipe_status(recipe, recipe, 2), "Applied")
  changed <- recipe
  changed$steps[[1]]$enabled <- FALSE
  expect_equal(preparation_recipe_status(changed, recipe, 2), "Preview")
  expect_equal(preparation_recipe_status(changed, recipe, 1), "Disabled")
})

test_that("duration coverage matches remove_partial_data for every current grouping and basis", {
  original <- unserialize(m8_record(edges = TRUE)$raw_payload)
  for (groups in list(character(), "Id", c("Id", "State"))) {
    raw <- dplyr::group_by(original, dplyr::across(dplyr::all_of(groups)))
    record <- new_dataset_record(
      raw,
      "Duration coverage parity",
      new_source_manifest("synthetic", source_timezone = "Europe/Berlin")
    )
    context <- m8_context(record)
    for (basis in c(
      "full_day",
      "observed_span",
      "full_group",
      "observed_group"
    )) {
      for (mode in c("available_duration", "missing_duration")) {
        args <- list(
          variable = "MEDI",
          threshold = if (mode == "available_duration") 16 else 2,
          basis = basis,
          mode = mode,
          unit = "hours"
        )
        step <- new_preparation_step("coverage", args, context)
        expect_identical(step$parameters$mapping_version, 3L)
        threshold <- if (mode == "available_duration")
          -lubridate::dhours(16) else lubridate::dhours(2)
        reference <- raw
        reference$.index <- seq_len(nrow(raw))
        reference <- LightLogR::remove_partial_data(
          reference,
          MEDI,
          threshold.missing = threshold,
          by.date = basis %in% c("full_day", "observed_span"),
          show.result = FALSE,
          handle.gaps = basis %in% c("full_day", "full_group")
        )
        actual <- evaluate_preparation(raw, new_recipe(list(step)), context)
        expect_identical(actual, raw[sort(unique(reference$.index)), ])
      }
    }
  }
})

test_that("coverage duration boundary equality keeps or removes whole days and groups", {
  raw <- dplyr::group_by(
    data.frame(
      Id = rep(c("A", "B"), each = 24),
      Datetime = rep(as.POSIXct("2026-01-01", tz = "UTC") + 0:23 * 3600, 2),
      MEDI = c(rep(1, 16), rep(NA_real_, 8), rep(1, 15), rep(NA_real_, 9))
    ),
    Id
  )
  record <- new_dataset_record(
    raw,
    "Duration equality",
    new_source_manifest("synthetic", source_timezone = "UTC")
  )
  context <- m8_context(record)
  for (basis in c(
    "full_day",
    "observed_span",
    "full_group",
    "observed_group"
  )) {
    for (mode in c("available_duration", "missing_duration")) {
      args <- list(
        variable = "MEDI",
        threshold = if (mode == "available_duration") 16 else 8,
        basis = basis,
        mode = mode,
        unit = "hours"
      )
      step <- new_preparation_step("coverage", args, context)
      actual <- evaluate_preparation(raw, new_recipe(list(step)), context)
      expect_identical(actual, raw[raw$Id == "A", ])
      expect_equal(sum(is.na(actual$MEDI)), 8)
      step$parameters$mapping_version <- 2L
      expect_error(
        validate_preparation_recipe(new_recipe(list(step)), context),
        "version 3"
      )
    }
  }
  args <- list(
    variable = "MEDI",
    threshold = 0,
    basis = "observed_group",
    mode = "available_duration",
    unit = "hours"
  )
  expect_error(
    new_preparation_step("coverage", args, context),
    "greater than zero"
  )
  args$mode <- "missing_duration"
  step <- new_preparation_step("coverage", args, context)
  expect_equal(
    nrow(evaluate_preparation(raw, new_recipe(list(step)), context)),
    0
  )
  args$unit <- "months"
  expect_error(
    new_preparation_step("coverage", args, context),
    "supported unit"
  )
  args$unit <- "days"
  args$threshold <- .Machine$double.xmax
  expect_error(
    new_preparation_step("coverage", args, context),
    "finite duration"
  )
})
