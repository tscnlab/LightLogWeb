test_that("complete selection recipes equal a short independent public R pipeline", {
  record <- m8_record(edges = TRUE)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  participants <- new_preparation_step(
    "participants",
    list(
      identities = context$identities[1L, , drop = FALSE],
      identity_columns = context$identity_columns
    ),
    context
  )
  dates <- new_preparation_step(
    "dates",
    list(start = "2026-03-28", end = "2026-03-30", timezone = "Europe/Berlin"),
    context
  )
  instants <- new_preparation_step(
    "datetimes",
    list(
      start = "2026-03-28 10:00:00 +0100",
      end = "2026-03-30 08:00:00 +0200"
    ),
    context
  )
  recipe <- new_recipe(list(participants, dates, instants, m8_clock(context)))

  # Deliberately independent: literal source identities, public range calls and
  # a separate character-clock predicate. No executor/helper is used below.
  expected <- raw |>
    dplyr::filter(
      Id == "P01",
      llw_source_dataset_id == "dataset_alpha",
      file_group_id == "alpha:1",
      file.name == "one.csv"
    ) |>
    LightLogR::filter_Date(
      start = "2026-03-28",
      end = "2026-03-30",
      tz = "Europe/Berlin"
    ) |>
    LightLogR::filter_Datetime(
      start = as.POSIXct("2026-03-28 09:00:00", tz = "UTC"),
      end = as.POSIXct("2026-03-30 06:00:00", tz = "UTC"),
      tz = "UTC"
    ) |>
    dplyr::filter(
      format(Datetime, "%H:%M:%S", tz = "Europe/Berlin") >= "22:00:00" |
        format(Datetime, "%H:%M:%S", tz = "Europe/Berlin") < "06:00:00"
    )
  value <- m8_preview(record, recipe)
  expect_identical(unserialize(value$candidate$prepared_payload), expected)
  expect_identical(value$candidate$raw_payload, record$raw_payload)
  expect_identical(value$candidate$metadata_bundle, record$metadata_bundle)
  expect_identical(value$candidate$source_manifest, record$source_manifest)
  expect_identical(value$candidate$provenance, record$provenance)
  expect_identical(value$candidate$recipe, recipe)
  expect_identical(
    serialize(recipe, NULL),
    serialize(unserialize(serialize(recipe, NULL)), NULL)
  )
  expect_equal(nrow(preparation_call_map()), 16L)
})

test_that("clock selections preserve grouped input and match independent filters", {
  source <- m8_record(edges = TRUE)
  data <- dplyr::group_by(unserialize(source$raw_payload), Id, file_group_id)
  record <- new_dataset_record(
    data,
    "Grouped clock fixture",
    source$source_manifest,
    analysis_settings = source$analysis_settings
  )
  context <- m8_context(record)
  for (bounds in list(c("06:00:00", "12:00:00"), c("22:00:00", "06:00:00"))) {
    start <- bounds[[1L]]
    end <- bounds[[2L]]
    recipe <- new_recipe(list(m8_clock(context, start, end)))
    # The reference computes clock strings inside each group, independently
    # of the executor's numeric seconds and full-row selection mask.
    expected <- if (start < end) {
      dplyr::filter(
        data,
        format(Datetime, "%H:%M:%S", tz = "Europe/Berlin") >= start,
        format(Datetime, "%H:%M:%S", tz = "Europe/Berlin") < end
      )
    } else {
      dplyr::filter(
        data,
        format(Datetime, "%H:%M:%S", tz = "Europe/Berlin") >= start |
          format(Datetime, "%H:%M:%S", tz = "Europe/Berlin") < end
      )
    }
    value <- m8_preview(record, recipe)
    result <- unserialize(value$candidate$prepared_payload)
    expect_identical(result, expected)
    expect_identical(dplyr::group_vars(result), c("Id", "file_group_id"))
    expect_identical(value$candidate$raw_payload, record$raw_payload)
  }
})

test_that("date and datetime endpoints and both repeated DST hours are explicit", {
  raw <- data.frame(
    Id = "P01",
    Datetime = as.POSIXct(
      c(
        "2026-10-24 20:00:00",
        "2026-10-25 00:30:00",
        "2026-10-25 01:30:00",
        "2026-10-25 04:59:59",
        "2026-10-25 05:00:00",
        "2026-10-25 12:00:00"
      ),
      tz = "UTC"
    ),
    MEDI = 1:6
  )
  record <- new_dataset_record(raw, "Synthetic autumn", m1_source_manifest())
  context <- m8_context(record)
  result <- evaluate_preparation(
    raw,
    new_recipe(list(m8_clock(context))),
    context
  )
  expect_identical(result$MEDI, 1:4)
  preview <- m8_preview(record, new_recipe(list(m8_clock(context))))
  expect_identical(
    preview$comparison$after_sample$Window_start_date,
    rep("2026-10-24", 4L)
  )
  expect_false(
    "Window_start_date" %in%
      names(unserialize(preview$candidate$prepared_payload))
  )
  expect_identical(as.numeric(result$Datetime), as.numeric(raw$Datetime[1:4]))
  dates <- new_preparation_step(
    "dates",
    list(start = "2026-10-25", end = "2026-10-25", timezone = "Europe/Berlin"),
    context
  )
  expect_identical(
    evaluate_preparation(raw, new_recipe(list(dates)), context)$MEDI,
    2:6
  )
  exact <- new_preparation_step(
    "datetimes",
    list(
      start = "2026-10-25 02:30:00 +0100",
      end = "2026-10-25 05:59:59 +0100"
    ),
    context
  )
  expect_identical(
    evaluate_preparation(raw, new_recipe(list(exact)), context)$MEDI,
    3L
  )
  expect_error(preparation_instant("2026-10-25 02:30:00"), "explicit offset")
  expect_error(preparation_instant("2026-02-30 02:30:00 +0100"), "valid date")
  expect_error(preparation_instant("2026-01-01 02:30:00 +1500"), "UTC offset")
  expect_error(m8_clock(context, "06:00:00", "06:00:00"), "Equal clock")
  expect_error(m8_clock(context, "25:00:00", "06:00:00"), "clock times")
})

test_that("empty output, disabled steps and edits always start from raw", {
  record <- m8_record()
  context <- m8_context(record)
  narrow <- new_recipe(list(m8_clock(context, "01:00:00", "02:00:00")))
  applied <- m8_preview(record, narrow)$candidate
  wide <- new_recipe(list(m8_clock(context, "00:00:00", "12:00:00")))
  fresh <- m8_preview(record, wide)
  replacement <- m8_preview(applied, wide)
  expect_identical(
    replacement$candidate$prepared_payload,
    fresh$candidate$prepared_payload
  )
  expect_gt(
    replacement$comparison$after$rows,
    nrow(unserialize(applied$prepared_payload))
  )
  disabled <- edit_preparation_recipe(
    narrow,
    "toggle",
    id = narrow$steps[[1L]]$id
  )
  expect_identical(
    m8_preview(applied, disabled)$candidate$prepared_payload,
    record$raw_payload
  )
  dates <- new_preparation_step(
    "dates",
    list(start = "2040-01-01", end = "2040-01-01", timezone = "UTC"),
    context
  )
  empty <- m8_preview(record, new_recipe(list(dates, m8_clock(context))))
  expect_identical(empty$comparison$after$rows, 0L)
  expect_identical(empty$comparison$removed_fraction, 1)
  expect_identical(
    names(unserialize(empty$candidate$prepared_payload)),
    names(unserialize(record$raw_payload))
  )
  reset <- reset_dataset_record(replacement$candidate)
  expect_identical(reset$prepared_payload, record$raw_payload)
  expect_length(reset$recipe$steps, 0L)
  expect_identical(
    undo_dataset_record(reset)$prepared_payload,
    replacement$candidate$prepared_payload
  )
  expect_identical(
    undo_dataset_record(applied)$prepared_payload,
    record$raw_payload
  )
})

test_that("identities, versions and fixed typed arguments reject unsafe recipes", {
  record <- m8_record()
  context <- m8_context(record)
  step <- m8_clock(context)
  changed <- step
  changed$parameters$dataset_id <- "dataset_other"
  expect_error(
    validate_preparation_recipe(new_recipe(list(changed)), context),
    "another dataset"
  )
  changed <- step
  changed$parameters$arguments$expression <- "system('unsafe')"
  expect_error(
    validate_preparation_recipe(new_recipe(list(changed)), context),
    "unsupported"
  )
  expect_error(
    new_preparation_step("clock", list(start = quote(Sys.time())), context),
    "incomplete"
  )
  expect_error(
    validate_preparation_recipe(new_recipe(list(step), version = 2L), context),
    "version 1"
  )
  expect_error(
    new_preparation_step(
      "participants",
      list(
        identities = context$identities[FALSE, ],
        identity_columns = context$identity_columns
      ),
      context
    ),
    "at least one"
  )
  keys <- context$identities[1L, , drop = FALSE]
  keys$Id <- "missing"
  other <- new_preparation_step(
    "participants",
    list(identities = keys, identity_columns = context$identity_columns),
    context
  )
  expect_error(
    validate_preparation_recipe(new_recipe(list(other)), context),
    "not present"
  )
  expect_error(
    preparation_check_budget(record, 1),
    class = "llw_resource_error"
  )
})

test_that("recipe order and stable IDs survive edit disable remove and serialization", {
  context <- m8_context()
  a <- m8_clock(context)
  b <- m8_clock(context, "00:00:00", "12:00:00")
  recipe <- new_recipe(list(a, b))
  expect_identical(
    edit_preparation_recipe(recipe, "up", id = b$id)$steps,
    list(b, a)
  )
  expect_identical(
    edit_preparation_recipe(recipe, "down", id = a$id)$steps,
    list(b, a)
  )
  expect_identical(edit_preparation_recipe(recipe, "up", id = a$id), recipe)
  expect_identical(
    edit_preparation_recipe(
      recipe,
      "reorder",
      ordered_ids = c(b$id, a$id)
    )$steps,
    list(b, a)
  )
  expect_identical(
    edit_preparation_recipe(recipe, "reorder", ordered_ids = c(a$id, b$id)),
    recipe
  )
  for (invalid in list(
    c(a$id, a$id),
    c(a$id, "step_stale"),
    a$id,
    c(a$id, NA_character_),
    1:2,
    NULL
  ))
    expect_error(
      edit_preparation_recipe(recipe, "reorder", ordered_ids = invalid),
      "recipe changed"
    )
  revised <- a
  revised$parameters$arguments$end <- "07:00:00"
  expect_identical(
    edit_preparation_recipe(recipe, "save", step = revised)$steps,
    list(revised, b)
  )
  expect_identical(
    edit_preparation_recipe(recipe, "remove", id = a$id)$steps,
    list(b)
  )
  expect_error(
    edit_preparation_recipe(recipe, "remove", id = "step_missing"),
    "Select"
  )
})

test_that("frozen public-derived and appended metadata fixtures remain unchanged", {
  fixtures <- metadata_explorer_fixture_records(m7_fixture_directory())
  for (record in fixtures) {
    raw <- unserialize(record$raw_payload)
    context <- m8_context(record)
    step <- new_preparation_step(
      "participants",
      list(
        identities = context$identities[1L, , drop = FALSE],
        identity_columns = context$identity_columns
      ),
      context
    )
    value <- m8_preview(record, new_recipe(list(step)))
    expect_identical(value$candidate$raw_payload, record$raw_payload)
    expect_identical(value$candidate$metadata_bundle, record$metadata_bundle)
    expect_identical(
      names(unserialize(value$candidate$prepared_payload)),
      names(raw)
    )
    expect_true(all(
      value$comparison$after_sample$Id == context$identities$Id[[1L]]
    ))
  }
})

test_that("display bounds, observed-span diagnostics and stale bindings are honest", {
  record <- m8_record(edges = TRUE)
  value <- m8_preview(record, new_recipe())
  expect_lte(nrow(value$comparison$before_sample), 100L)
  expect_lte(ncol(value$comparison$before_sample), 8L)
  expect_lte(nrow(value$comparison$plot), 2000L)
  expect_equal(value$comparison$before$identities, 3L)
  expect_equal(value$comparison$before$participants, 2L)
  expect_equal(value$comparison$before$missing, 2L)
  expect_equal(value$comparison$before$duplicates, 2L)
  expect_equal(value$comparison$invalidated_values, 0L)
  expect_match(
    value$comparison$table$Preview[
      value$comparison$table$Measure == "Intended-window coverage"
    ],
    "no intended window"
  )
  expect_true(preparation_preview_matches(value, record, new_recipe()))
  expect_false(preparation_preview_matches(
    value,
    rename_dataset_record(record, "Changed"),
    new_recipe()
  ))
  expect_false(preparation_preview_matches(
    value,
    record,
    new_recipe(list(m8_clock(m8_context(record))))
  ))
})

test_that("a genuinely empty raw fixture has an honest empty comparison", {
  data <- m1_fixture_data()[FALSE, ]
  record <- new_dataset_record(data, "Synthetic empty", m1_source_manifest())
  value <- m8_preview(record, new_recipe())
  expect_equal(value$comparison$before$rows, 0L)
  expect_equal(value$comparison$after$rows, 0L)
  expect_equal(value$comparison$removed_rows, 0L)
  expect_equal(value$comparison$before$identities, 0L)
})

test_that("missing file identity never becomes a stream-specific diagnostic", {
  fixtures <- metadata_explorer_fixture_records(m7_fixture_directory())
  standalone <- fixtures[[2L]]
  appended <- fixtures[[4L]]
  standalone_context <- m8_context(standalone)
  context <- m8_context(appended)
  expect_true(standalone_context$file_identity_complete)
  expect_equal(sum(grepl("S004", standalone_context$identities$Id)), 2L)
  expect_false(context$file_identity_complete)
  chosen <- context$identities[
    context$identities$Id == "IZTECH_IZTECH_S004",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(chosen), 1L)
  step <- new_preparation_step(
    "participants",
    list(identities = chosen, identity_columns = context$identity_columns),
    context
  )
  value <- m8_preview(appended, new_recipe(list(step)))
  raw <- unserialize(appended$raw_payload)
  expected <- raw[
    raw$Id == "IZTECH_IZTECH_S004" &
      raw$llw_source_dataset_id == "dataset_m7iztechlightsubset",
    ,
    drop = FALSE
  ]
  expect_identical(unserialize(value$candidate$prepared_payload), expected)
  expect_identical(value$candidate$raw_payload, appended$raw_payload)
  expect_equal(value$comparison$after$rows, 64L)
  expect_equal(value$comparison$after$identities, 1L)
  expect_match(value$comparison$after$epochs, "file identity not retained")
  expect_true(is.na(value$comparison$after$gaps))
  expect_true(is.na(value$comparison$after$duplicates))
  temporal <- grepl("interval|gaps|duplicate", value$comparison$table$Measure)
  expect_true(all(grepl(
    "file identity not retained",
    value$comparison$table$Preview[temporal]
  )))
  expect_match(preparation_step_label(step, 1L), "selected identities")

  # Absence, blank and partially missing identifiers are not proof of a file.
  expect_false(preparation_has_file_identity(data.frame(Id = "P01")))
  expect_false(preparation_has_file_identity(data.frame(
    file_group_id = c("file:1", NA_character_),
    file.name = c("one.csv", " ")
  )))
  expect_true(preparation_has_file_identity(data.frame(
    file_group_id = c("file:1", NA_character_),
    file.name = c(NA, "two.csv")
  )))
})

test_that("native selection ignores display preferences and date controls are bounded", {
  record <- m8_record()
  record$analysis_settings$analysis_timezone <- "America/New_York"
  context <- m8_context(record)
  expect_identical(context$timezone, "Europe/Berlin")
  form <- preparation_form_defaults(context)
  form$step_type <- "dates"
  args <- preparation_form_arguments(form, context)
  expect_identical(args$timezone, "native")
  expect_identical(c(args$start, args$end), c("2026-03-28", "2026-03-31"))
  form$date_start <- "2026-03-27"
  expect_error(preparation_form_arguments(form, context), "available span")
  form$step_type <- "clock"
  expect_identical(
    preparation_form_arguments(form, context)$timezone,
    "native"
  )
})

test_that("cleared date controls name the field and its current available range", {
  context <- m8_context()
  fields <- c(
    date_start = "first",
    date_end = "last",
    instant_start_date = "start",
    instant_end_date = "end"
  )
  for (field in names(fields)) {
    for (missing in list("", NA_character_, character())) {
      form <- preparation_form_defaults(context)
      form$step_type <- if (startsWith(field, "instant_")) "datetimes" else
        "dates"
      form[[field]] <- missing
      expect_error(
        preparation_form_arguments(form, context),
        paste0(
          "missing or was cleared.*Choose the ",
          fields[[field]],
          " date.*2026-03-28 to 2026-03-31.*Europe/Berlin"
        )
      )
    }
  }
  form <- preparation_form_defaults(context)
  form$step_type <- "datetimes"
  form$instant_end_zone <- "custom"
  form$instant_end_timezone <- "America/New_York"
  form$instant_end_date <- ""
  expect_error(
    preparation_form_arguments(form, context),
    "Choose the end date.*2026-03-27 to 2026-03-30.*America/New_York"
  )
  form$instant_end_date <- "2026-03-31"
  expect_error(
    preparation_form_arguments(form, context),
    "outside the available range.*Choose the end date.*2026-03-30"
  )
})

test_that("datetime endpoint controls resolve distinct zones without guessing DST", {
  context <- m8_context()
  form <- preparation_form_defaults(context)
  form$step_type <- "datetimes"
  form$instant_start_date <- "2026-03-28"
  form$instant_start_time <- "10:00"
  form$instant_end_date <- "2026-03-30"
  form$instant_end_time <- "02:00:00"
  form$instant_end_zone <- "custom"
  form$instant_end_timezone <- "America/New_York"
  args <- preparation_form_arguments(form, context)
  expect_identical(args$start, "2026-03-28 10:00:00 +0100")
  expect_identical(args$end, "2026-03-30 02:00:00 -0400")
  expect_identical(args$start_timezone, "native")
  expect_identical(args$end_timezone, "America/New_York")
  step <- new_preparation_step("datetimes", args, context)
  expect_identical(step$parameters$mapping_version, 2L)
  mismatched <- step
  mismatched$parameters$mapping_version <- 1L
  expect_error(
    validate_preparation_recipe(new_recipe(list(mismatched)), context),
    "mapping version"
  )
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  expected <- LightLogR::filter_Datetime(
    raw,
    start = as.POSIXct("2026-03-28 09:00:00", tz = "UTC"),
    end = as.POSIXct("2026-03-30 06:00:00", tz = "UTC"),
    tz = "UTC"
  )
  expect_identical(
    evaluate_preparation(raw, new_recipe(list(step)), context),
    expected
  )
  expect_error(
    preparation_local_instant("2026-03-29", "02:30:00", "Europe/Berlin"),
    "does not exist"
  )
  expect_error(
    preparation_local_instant("2026-10-25", "02:30:00", "Europe/Berlin"),
    "occurs twice"
  )
  first <- preparation_local_instant(
    "2026-10-25",
    "02:30:00 +0200",
    "Europe/Berlin"
  )
  second <- preparation_local_instant(
    "2026-10-25",
    "02:30:00 +0100",
    "Europe/Berlin"
  )
  expect_equal(
    as.numeric(difftime(
      preparation_instant(second),
      preparation_instant(first),
      units = "hours"
    )),
    1
  )
  expect_error(
    preparation_local_instant("2026-10-25", "02:30:00 +0300", "Europe/Berlin"),
    "offset does not match"
  )
  form$instant_end_date <- "2026-03-31"
  expect_error(preparation_form_arguments(form, context), "available span")
  form$instant_end_timezone <- "made/up"
  expect_error(preparation_form_arguments(form, context), "Olson")
})

test_that("length selection distinguishes overall, source, participant and recording anchors", {
  raw <- data.frame(
    Id = rep(c("P01", "P01", "P02"), each = 5L),
    Datetime = as.POSIXct("2026-03-28", tz = "UTC") +
      rep(c(0, 86400, 172800), each = 5L) +
      rep(c(0, 6, 12, 18, 24) * 3600, 3L),
    MEDI = c(NA_real_, 2:15),
    llw_source_dataset_id = rep(c("a", "b", "a"), each = 5L),
    file_group_id = rep(c("a:1", "b:1", "a:2"), each = 5L)
  )
  record <- new_dataset_record(
    raw,
    "Synthetic staggered recordings",
    m1_source_manifest()
  )
  context <- m8_context(record)
  first <- list(
    overall = 1:2,
    source = c(1:2, 6:7),
    participant = c(1:2, 11:12),
    identity = c(1:2, 6:7, 11:12)
  )
  last <- list(
    overall = 13:15,
    source = c(8:10, 13:15),
    participant = c(8:10, 13:15),
    identity = c(3:5, 8:10, 13:15)
  )
  for (scope in names(first))
    for (anchor in c("start", "end")) {
      step <- new_preparation_step(
        "duration",
        list(length = 12, unit = "hours", from = anchor, scope = scope),
        context
      )
      rows <- if (anchor == "start") first[[scope]] else last[[scope]]
      value <- m8_preview(record, new_recipe(list(step)))
      expect_identical(
        unserialize(value$candidate$prepared_payload),
        raw[rows, , drop = FALSE]
      )
      expect_identical(value$candidate$raw_payload, record$raw_payload)
    }
  # A preceding filter changes which observations reach the duration step.
  dates <- new_preparation_step(
    "dates",
    list(start = "2026-03-29", end = "2026-03-31", timezone = "UTC"),
    context
  )
  length_step <- new_preparation_step(
    "duration",
    list(length = 12, unit = "hours", from = "start", scope = "overall"),
    context
  )
  expected <- LightLogR::filter_Date(
    raw,
    start = "2026-03-29",
    end = "2026-03-31",
    tz = "UTC"
  )
  expect_identical(
    evaluate_preparation(raw, new_recipe(list(dates, length_step)), context),
    expected[1:3, , drop = FALSE]
  )
})

test_that("duration uses actual current groups independently of participant and file identities", {
  raw <- data.frame(
    Id = rep(c("P01", "P01", "P02"), each = 5L),
    Datetime = as.POSIXct("2026-03-28", tz = "UTC") +
      rep(c(0, 86400, 172800), each = 5L) +
      rep(c(0, 6, 12, 18, 24) * 3600, 3L),
    MEDI = c(NA_real_, 2:15),
    file.name = rep(c("one.csv", "two.csv", "three.csv"), each = 5L),
    State = rep(c("early", "early", "early", "late", "late"), 3L)
  )
  grouped <- dplyr::group_by(raw, Id, State)
  record <- new_dataset_record(
    grouped,
    "Synthetic current groups",
    m1_source_manifest()
  )
  context <- m8_context(record)
  first <- list(
    group = c(1L, 4L, 11L, 14L),
    participant = c(1L, 11L),
    identity = c(1L, 6L, 11L)
  )
  last <- list(
    group = c(7:10, 12:15),
    participant = c(9:10, 14:15),
    identity = c(4:5, 9:10, 14:15)
  )
  for (scope in names(first))
    for (anchor in c("start", "end")) {
      step <- new_preparation_step(
        "duration",
        list(length = 6, unit = "hours", from = anchor, scope = scope),
        context
      )
      rows <- if (anchor == "start") first[[scope]] else last[[scope]]
      value <- m8_preview(record, new_recipe(list(step)))
      expect_identical(
        unserialize(value$candidate$prepared_payload),
        grouped[rows, , drop = FALSE]
      )
      expect_identical(value$candidate$raw_payload, record$raw_payload)
      expect_identical(
        dplyr::group_vars(unserialize(value$candidate$prepared_payload)),
        c("Id", "State")
      )
    }
  group_step <- new_preparation_step(
    "duration",
    list(length = 6, unit = "hours", from = "start", scope = "group"),
    context
  )
  recipe <- new_recipe(list(group_step))
  # Evaluation uses the incoming groups, not a cached assumption of Id groups.
  by_state <- dplyr::group_by(raw, State)
  expect_identical(
    evaluate_preparation(by_state, recipe, context),
    by_state[c(1L, 4L), ]
  )
  expect_identical(
    evaluate_preparation(raw, recipe, context),
    raw[1L, , drop = FALSE]
  )
  # User grouping columns and existing row markers must never replace row indices.
  colliding <- raw
  names(colliding)[names(colliding) == "State"] <- ".row"
  colliding$.llw_duration_row <- "original value"
  colliding <- dplyr::group_by(colliding, .row)
  expect_identical(
    evaluate_preparation(colliding, recipe, context),
    colliding[c(1L, 4L), ]
  )
  # Earlier filters move the start within each surviving group.
  dates <- new_preparation_step(
    "dates",
    list(start = "2026-03-29", end = "2026-03-31", timezone = "UTC"),
    context
  )
  expect_identical(
    evaluate_preparation(grouped, new_recipe(list(dates, group_step)), context),
    grouped[c(5L, 6L, 11L, 14L), ]
  )
  expect_match(preparation_step_label(group_step, 1L), "per current group")
  expect_identical(unserialize(serialize(recipe, NULL)), recipe)
})

test_that("calendar length follows native clock time and elapsed length follows seconds", {
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  make <- function(unit, from)
    new_preparation_step(
      "duration",
      list(length = 2, unit = unit, from = from, scope = "overall"),
      context
    )
  expected <- raw[
    raw$Datetime < as.POSIXct("2026-03-30 01:00:00", tz = "Europe/Berlin"),
    ,
    drop = FALSE
  ]
  expect_identical(
    evaluate_preparation(
      raw,
      new_recipe(list(make("calendar_days", "start"))),
      context
    ),
    expected
  )
  elapsed <- raw[
    as.numeric(raw$Datetime) < as.numeric(min(raw$Datetime)) + 48 * 3600,
    ,
    drop = FALSE
  ]
  expect_identical(
    evaluate_preparation(
      raw,
      new_recipe(list(make("elapsed_days", "start"))),
      context
    ),
    elapsed
  )
  expect_gt(nrow(elapsed), nrow(expected))
  expected_end <- raw[
    raw$Datetime >= as.POSIXct("2026-03-29 01:30:00", tz = "Europe/Berlin"),
    ,
    drop = FALSE
  ]
  expect_identical(
    evaluate_preparation(
      raw,
      new_recipe(list(make("calendar_days", "end"))),
      context
    ),
    expected_end
  )
  for (date in c("2026-03-28", "2026-10-24")) {
    input <- data.frame(
      Id = "P01",
      Datetime = as.POSIXct(paste(date, "02:30:00"), tz = "Europe/Berlin") +
        c(0, 86400 * 2),
      MEDI = 1:2
    )
    ambiguous <- new_dataset_record(
      input,
      "Synthetic clock transition",
      m1_source_manifest()
    )
    ctx <- m8_context(ambiguous)
    step <- new_preparation_step(
      "duration",
      list(
        length = 1,
        unit = "calendar_days",
        from = "start",
        scope = "overall"
      ),
      ctx
    )
    expect_error(
      evaluate_preparation(input, new_recipe(list(step)), ctx),
      "skipped or repeated"
    )
  }
  expect_error(
    preparation_arguments(
      "duration",
      list(length = 0, unit = "hours", from = "start", scope = "overall")
    ),
    "positive"
  )
  expect_error(
    preparation_arguments(
      "duration",
      list(
        length = 1.5,
        unit = "calendar_days",
        from = "start",
        scope = "overall"
      )
    ),
    "whole number"
  )
  expect_error(
    preparation_arguments(
      "duration",
      list(length = 1, unit = "hours", from = "start", scope = "arbitrary")
    ),
    "supported"
  )
})

test_that("source duration scope needs complete retained source identities", {
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  raw$llw_source_dataset_id[[1L]] <- NA_character_
  record <- new_dataset_record(
    raw,
    "Synthetic incomplete source identity",
    m1_source_manifest()
  )
  context <- m8_context(record)
  expect_false(context$source_identity_complete)
  step <- new_preparation_step(
    "duration",
    list(length = 1, unit = "hours", from = "start", scope = "source"),
    context
  )
  expect_error(
    evaluate_preparation(raw, new_recipe(list(step)), context),
    "source dataset identifiers"
  )
})
