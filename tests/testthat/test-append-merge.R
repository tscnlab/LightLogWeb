test_that("dataset inventory reports provenance, dimensions, time, and unknowns", {
  record <- m4_record("Inventory fixture")
  inventory <- dataset_record_inventory(record)

  expect_s3_class(inventory, "llw_dataset_inventory")
  expect_identical(inventory$id, record$id)
  expect_equal(inventory$rows, 3L)
  expect_equal(inventory$participants, 2L)
  expect_equal(inventory$sampling_seconds, 60)
  expect_identical(inventory$source_timezone, "UTC")
  expect_identical(inventory$primary_unit, "lux")
  expect_equal(inventory$source_size_bytes, 1024)
  expect_equal(inventory$recipe_revision, 0L)

  unknown <- m4_record(
    "Unknown metadata",
    unit = NA_character_,
    device = NA_character_,
    source_timezone = NA_character_
  )
  unknown_inventory <- dataset_record_inventory(unknown)
  expect_true(is.na(unknown_inventory$source_timezone))
  expect_true(is.na(unknown_inventory$primary_unit))
  expect_true(is.na(unknown_inventory$device))
})

test_that("dataset duplication creates a new independent stable record", {
  source <- m4_record("Source")
  duplicate <- duplicate_dataset_record(source, "Source copy")

  expect_false(identical(duplicate$id, source$id))
  expect_identical(duplicate$raw_payload, source$raw_payload)
  expect_identical(duplicate$prepared_payload, source$prepared_payload)
  expect_identical(duplicate$raw_checksum, source$raw_checksum)
  expect_identical(duplicate$provenance$duplicated_from$dataset_id, source$id)
  expect_equal(duplicate$revision, 0L)
  expect_length(duplicate$history, 0L)
  expect_null(duplicate$draft)
  expect_identical(source$display_name, "Source")
})

test_that("a differing-schema append uses a union and preserves source context", {
  records <- m4_records()
  source_checksums <- vapply(records, `[[`, character(1), "raw_checksum")
  source_clock_labels <- lapply(records, function(record) {
    data <- dataset_raw_data(record)
    datetime <- if ("Datetime" %in% names(data)) data$Datetime else
      data$LocalTime
    format(
      datetime,
      tz = lubridate::tz(datetime),
      format = "%Y-%m-%d %H:%M:%S"
    )
  })
  preview <- preview_append_merge(records, m4_separate_spec(records))

  expect_true(preview$can_apply)
  expect_equal(preview$diagnostics$result_rows, 6L)
  expect_equal(preview$diagnostics$duplicate_rows, 0L)
  expect_equal(preview$diagnostics$overlap_rows, 0L)
  expect_identical(
    preview$time_plan$preservation,
    c(
      "Clock time preserved; instant preserved",
      "Clock time preserved; instant reinterpreted"
    )
  )
  merged_record <- new_appended_dataset_record(preview)
  merged <- dataset_raw_data(merged_record)

  expect_identical(lubridate::tz(merged$Datetime), "UTC")
  expect_setequal(
    unique(merged$llw_source_timezone),
    c("UTC", "Europe/Berlin")
  )
  expect_true(all(nzchar(merged$llw_local_datetime)))
  expect_true(all(
    c(
      "first_MEDI",
      "second_light",
      "first_context",
      "second_context",
      "Temperature"
    ) %in%
      names(merged)
  ))
  expect_equal(sum(!is.na(merged$first_MEDI)), 3L)
  expect_equal(sum(!is.na(merged$second_light)), 3L)
  for (dataset_id in names(records)) {
    source_rows <- merged[merged$llw_source_dataset_id == dataset_id, ]
    source_rows <- source_rows[order(source_rows$llw_source_row), ]
    expect_equal(
      format(
        source_rows$Datetime,
        tz = "UTC",
        format = "%Y-%m-%d %H:%M:%S"
      ),
      source_clock_labels[[dataset_id]]
    )
  }
  expect_true(all(grepl("^[AB]_", merged$Id)))
  expect_false(any(grepl("::", merged$Id, fixed = TRUE)))
  expect_identical(
    vapply(records, `[[`, character(1), "raw_checksum"),
    source_checksums
  )
  expect_identical(merged_record$source_manifest$source_type, "append_merge")
  expect_identical(merged_record$source_manifest$source_timezone, "UTC")
  expect_length(merged_record$provenance$append_merge$source_records, 2L)
  expect_null(merged_record$analysis_settings$primary_variable)
  provenance_eligibility <- merged_record$provenance$primary_variable_eligibility
  expect_false(provenance_eligibility$eligible[
    provenance_eligibility$variable == "llw_source_row"
  ])

  displayed <- append_display_data(preview$preview_data)
  expect_false(any(append_provenance_columns() %in% names(displayed)))
  expect_true(all(c("Id", "Datetime", "first_MEDI") %in% names(displayed)))
  expect_type(displayed$Datetime, "character")
  expect_match(displayed$Datetime[[1L]], "\\+0000 \\[UTC\\]$")
})

test_that("append provenance remains stored but cannot be remapped", {
  records <- m4_records()
  preview <- preview_append_merge(records, m4_separate_spec(records))
  appended <- new_appended_dataset_record(preview)
  appended_data <- dataset_raw_data(appended)
  choices <- append_record_column_choices(appended)
  selectable <- unique(unlist(choices, use.names = FALSE))

  expect_true(all(append_provenance_columns() %in% names(appended_data)))
  expect_false(any(append_provenance_columns() %in% selectable))
  expect_true("first_MEDI" %in% choices$measurement)
  expect_error(
    new_append_source_mapping(
      appended$id,
      "Id",
      "Datetime",
      "llw_source_row",
      "invalid_measurement"
    ),
    class = "llw_validation_error",
    regexp = "Internal append-provenance columns cannot be mapped"
  )
})

test_that("append can preserve absolute instants instead of clock labels", {
  records <- m4_records()
  source_instants <- lapply(records, function(record) {
    data <- dataset_raw_data(record)
    datetime <- if ("Datetime" %in% names(data)) data$Datetime else
      data$LocalTime
    as.numeric(datetime)
  })
  preview <- preview_append_merge(
    records,
    m4_separate_spec(records, time_alignment = "preserve_instant")
  )

  expect_true(preview$can_apply)
  expect_identical(
    preview$time_plan$preservation,
    c(
      "Clock time preserved; instant preserved",
      "Absolute instant preserved; clock time converted"
    )
  )
  merged <- unserialize(preview$merged_payload)
  for (dataset_id in names(records)) {
    source_rows <- merged[merged$llw_source_dataset_id == dataset_id, ]
    source_rows <- source_rows[order(source_rows$llw_source_row), ]
    expect_equal(
      as.numeric(source_rows$Datetime),
      source_instants[[dataset_id]]
    )
  }
})

test_that("optional mappings cannot collide with structural or measurement outputs", {
  records <- m4_records()
  ids <- names(records)
  first <- new_append_source_mapping(
    ids[[1L]],
    "Id",
    "Datetime",
    "MEDI",
    "first_MEDI",
    "A",
    optional_columns = "Context",
    optional_targets = "second_light"
  )
  second <- new_append_source_mapping(
    ids[[2L]],
    "Participant",
    "LocalTime",
    "LightValue",
    "second_light",
    "B"
  )

  expect_error(
    new_append_spec(
      list(first, second),
      "Collision",
      "prefix_source",
      "separate",
      "keep_marked",
      "not_applicable"
    ),
    class = "llw_validation_error",
    regexp = "Optional outputs"
  )
  expect_error(
    new_append_source_mapping(
      ids[[1L]],
      "Id",
      "Datetime",
      "MEDI",
      "first_MEDI",
      "A",
      optional_columns = "Id",
      optional_targets = "original_id"
    ),
    class = "llw_validation_error",
    regexp = "cannot repeat"
  )
})

test_that("combining primary measurements requires quantity, unit, and device decisions", {
  records <- m4_records()
  ids <- names(records)
  mappings <- list(
    new_append_source_mapping(
      ids[[1L]],
      "Id",
      "Datetime",
      "MEDI",
      "Combined",
      "A"
    ),
    new_append_source_mapping(
      ids[[2L]],
      "Participant",
      "LocalTime",
      "LightValue",
      "Combined",
      "B"
    )
  )
  unconfirmed <- new_append_spec(
    mappings,
    "Combined fixture",
    "prefix_source",
    "combine",
    "keep_marked",
    "not_applicable",
    output_measurement = "Combined",
    output_unit = "lux"
  )
  blocked <- preview_append_merge(records, unconfirmed)
  expect_false(blocked$can_apply)
  expect_match(paste(blocked$errors, collapse = " "), "Confirm")
  expect_match(paste(blocked$errors, collapse = " "), "devices")

  confirmed <- new_append_spec(
    mappings,
    "Combined fixture",
    "prefix_source",
    "combine",
    "keep_marked",
    "not_applicable",
    output_measurement = "Combined",
    output_unit = "lux",
    confirm_quantity = TRUE,
    acknowledge_device_difference = TRUE
  )
  ready <- preview_append_merge(records, confirmed)
  expect_true(ready$can_apply)
  expect_match(paste(ready$warnings, collapse = " "), "devices")
  merged <- new_appended_dataset_record(ready)
  expect_identical(merged$analysis_settings$primary_variable, "Combined")
  expect_identical(
    merged$factual_metadata$variables$Combined$unit,
    "lux"
  )
})

test_that("unit uncertainty and differing labels require explicit decisions", {
  records <- m4_records()
  records[[2L]]$factual_metadata$variables$LightValue$unit <- NA_character_
  records[[2L]] <- validate_dataset_record(records[[2L]])
  ids <- names(records)
  mappings <- list(
    new_append_source_mapping(
      ids[[1L]],
      "Id",
      "Datetime",
      "MEDI",
      "Combined",
      "A"
    ),
    new_append_source_mapping(
      ids[[2L]],
      "Participant",
      "LocalTime",
      "LightValue",
      "Combined",
      "B"
    )
  )
  spec <- function(
    acknowledge_unknown_units = FALSE,
    acknowledge_unit_difference = FALSE
  )
    new_append_spec(
      mappings,
      "Unknown-unit fixture",
      "prefix_source",
      "combine",
      "keep_marked",
      "not_applicable",
      output_measurement = "Combined",
      output_unit = "lux",
      confirm_quantity = TRUE,
      acknowledge_unit_difference = acknowledge_unit_difference,
      acknowledge_unknown_units = acknowledge_unknown_units,
      acknowledge_device_difference = TRUE
    )
  blocked <- preview_append_merge(records, spec())
  expect_false(blocked$can_apply)
  expect_match(paste(blocked$errors, collapse = " "), "unknown units")
  acknowledged <- preview_append_merge(records, spec(TRUE))
  expect_true(acknowledged$can_apply)
  expect_match(
    paste(acknowledged$warnings, collapse = " "),
    "Unknown source units"
  )

  records[[2L]]$factual_metadata$variables$LightValue$unit <- "W/m2"
  records[[2L]] <- validate_dataset_record(records[[2L]])
  mismatch <- preview_append_merge(records, spec(TRUE))
  expect_false(mismatch$can_apply)
  expect_match(
    paste(mismatch$errors, collapse = " "),
    "Recorded source units"
  )
  expect_match(paste(mismatch$errors, collapse = " "), "W/m2", fixed = TRUE)
  accepted_mismatch <- preview_append_merge(records, spec(TRUE, TRUE))
  expect_true(accepted_mismatch$can_apply)
  expect_match(
    paste(accepted_mismatch$warnings, collapse = " "),
    "same numeric scale"
  )

  records[[2L]]$factual_metadata$variables$LightValue$unit <- "lx"
  records[[2L]] <- validate_dataset_record(records[[2L]])
  alias <- preview_append_merge(records, spec())
  expect_true(alias$can_apply)
  alias_warning <- paste(alias$warnings, collapse = " ")
  expect_match(alias_warning, "Recorded source unit label `lx`")
  expect_match(alias_warning, "alias of output unit `lux`")
  expect_false(grepl("`lux`, `lx`", alias_warning, fixed = TRUE))
  expect_false(grepl(
    "unit label `lux` is treated",
    alias_warning,
    fixed = TRUE
  ))
})

test_that("identical measurement layout retains a shared name and unit", {
  showcase <- append_merge_showcase_records()
  first <- showcase[[1L]]
  matching <- showcase[[3L]]
  records <- stats::setNames(
    list(first, matching),
    c(first$id, matching$id)
  )
  candidates <- append_common_measurement_candidates(records, names(records))

  expect_equal(candidates$column, "MEDI")
  expect_equal(candidates$unit, "lux")
  mappings <- lapply(seq_along(records), function(index) {
    record <- records[[index]]
    new_append_source_mapping(
      record$id,
      "Id",
      "Datetime",
      "MEDI",
      "MEDI",
      paste0("S", index)
    )
  })
  spec <- new_append_spec(
    mappings,
    "Identical MEDI append",
    "prefix_source",
    "identical",
    output_measurement = "MEDI",
    output_unit = "lux",
    confirm_quantity = TRUE,
    acknowledge_device_difference = TRUE
  )
  preview <- preview_append_merge(records, spec)

  expect_true(preview$can_apply)
  expect_false(any(grepl("unit", preview$errors, ignore.case = TRUE)))
  merged <- new_appended_dataset_record(preview)
  expect_identical(merged$analysis_settings$primary_variable, "MEDI")
  expect_identical(merged$factual_metadata$variables$MEDI$unit, "lux")
  expect_true("MEDI" %in% names(dataset_raw_data(merged)))
})

test_that("identical measurement layout is unavailable for differing schemas", {
  records <- m4_records()
  candidates <- append_common_measurement_candidates(records, names(records))
  expect_equal(nrow(candidates), 0L)

  disabled <- htmltools::renderTags(
    append_measurement_strategy_ui(NS("append"), "separate", candidates)
  )$html
  expect_match(disabled, 'value="identical" disabled="disabled"', fixed = TRUE)
  expect_match(disabled, "same name and known unit", fixed = TRUE)
})

test_that("combined measurement defaults follow equal source names and units", {
  matching <- data.frame(
    dataset_id = c("one", "two"),
    column = c("MEDI", "MEDI"),
    unit = c("lux", "lux")
  )
  differing <- matching
  differing$column[[2L]] <- "LightValue"
  differing$unit[[2L]] <- "lx"

  expect_identical(
    append_combined_measurement_defaults(matching),
    list(column = "MEDI", unit = "lux")
  )
  expect_identical(
    append_combined_measurement_defaults(differing),
    list(column = "Measurement", unit = "")
  )
})

test_that("time alignment distinguishes clock labels from absolute instants", {
  berlin <- as.POSIXct("2025-07-01 18:00:00", tz = "Europe/Berlin")
  utc <- as.POSIXct("2025-07-01 18:00:00", tz = "UTC")
  clock <- append_adjust_datetime(berlin, "UTC", "preserve_clock")
  instant <- append_adjust_datetime(berlin, "UTC", "preserve_instant")
  unchanged_clock <- append_adjust_datetime(utc, "UTC", "preserve_clock")
  unchanged_instant <- append_adjust_datetime(utc, "UTC", "preserve_instant")

  expect_identical(lubridate::tz(clock), "UTC")
  expect_identical(lubridate::tz(instant), "UTC")
  expect_identical(format(clock, tz = "UTC", format = "%H:%M"), "18:00")
  expect_identical(format(instant, tz = "UTC", format = "%H:%M"), "16:00")
  expect_equal(as.numeric(instant), as.numeric(berlin))
  expect_false(isTRUE(all.equal(as.numeric(clock), as.numeric(berlin))))
  expect_equal(as.numeric(unchanged_clock), as.numeric(utc))
  expect_equal(as.numeric(unchanged_instant), as.numeric(utc))
  expect_identical(
    append_time_preservation_label("preserve_clock", "UTC", "UTC"),
    "Clock time preserved; instant preserved"
  )
  expect_identical(
    append_time_preservation_label("preserve_instant", "UTC", "UTC"),
    "Clock time preserved; instant preserved"
  )
  expect_identical(
    append_time_preservation_label(
      "preserve_clock",
      "Europe/Berlin",
      "UTC"
    ),
    "Clock time preserved; instant reinterpreted"
  )
  expect_identical(
    append_time_preservation_label(
      "preserve_instant",
      "Europe/Berlin",
      "UTC"
    ),
    "Absolute instant preserved; clock time converted"
  )
})

test_that("display tables make timezone transformations explicit", {
  source <- as.POSIXct(
    "2025-12-31 22:00:00",
    tz = "America/New_York"
  )
  clock <- append_adjust_datetime(source, "UTC", "preserve_clock")
  instant <- append_adjust_datetime(source, "UTC", "preserve_instant")

  source_display <- append_display_data(data.frame(Datetime = source))
  clock_display <- append_display_data(data.frame(Datetime = clock))
  instant_display <- append_display_data(data.frame(Datetime = instant))

  expect_identical(
    source_display$Datetime,
    "2025-12-31T22:00:00-0500 [America/New_York]"
  )
  expect_identical(
    clock_display$Datetime,
    "2025-12-31T22:00:00+0000 [UTC]"
  )
  expect_identical(
    instant_display$Datetime,
    "2026-01-01T03:00:00+0000 [UTC]"
  )
  expect_false(isTRUE(all.equal(as.numeric(clock), as.numeric(source))))
  expect_equal(as.numeric(instant), as.numeric(source))
})

test_that("preserve-clock rejects ambiguous or nonexistent DST labels", {
  nonexistent_clock <- as.POSIXct("2025-03-30 02:30:00", tz = "UTC")
  expect_error(
    append_adjust_datetime(
      nonexistent_clock,
      "Europe/Berlin",
      "preserve_clock"
    ),
    class = "llw_validation_error",
    regexp = "daylight-saving"
  )
})

test_that("time alignment applies to every retained POSIXct column", {
  add_optional_time <- function(record, datetime_column) {
    data <- dataset_raw_data(record)
    data$EventTime <- data[[datetime_column]] + 15 * 60
    new_dataset_record(
      raw_data = data,
      display_name = record$display_name,
      source_manifest = record$source_manifest,
      factual_metadata = record$factual_metadata,
      analysis_settings = record$analysis_settings
    )
  }
  first <- add_optional_time(m4_record("First timed"), "Datetime")
  second <- add_optional_time(
    m4_record(
      "Second timed",
      timezone = "Europe/Berlin",
      schema = "different"
    ),
    "LocalTime"
  )
  records <- stats::setNames(list(first, second), c(first$id, second$id))
  mappings <- list(
    new_append_source_mapping(
      first$id,
      "Id",
      "Datetime",
      "MEDI",
      "first_MEDI",
      "A",
      optional_columns = "EventTime",
      optional_targets = "first_event"
    ),
    new_append_source_mapping(
      second$id,
      "Participant",
      "LocalTime",
      "LightValue",
      "second_light",
      "B",
      optional_columns = "EventTime",
      optional_targets = "second_event"
    )
  )
  preview <- preview_append_merge(
    records,
    new_append_spec(
      mappings,
      "Timed append",
      "prefix_source",
      "separate",
      time_alignment = "preserve_instant",
      output_timezone = "America/New_York"
    )
  )

  expect_true(preview$can_apply)
  merged <- new_appended_dataset_record(preview)
  data <- dataset_raw_data(merged)
  expect_identical(lubridate::tz(data$Datetime), "America/New_York")
  expect_identical(lubridate::tz(data$first_event), "America/New_York")
  expect_identical(lubridate::tz(data$second_event), "America/New_York")
  expect_identical(
    merged$source_manifest$source_timezone,
    "America/New_York"
  )
  expect_identical(
    merged$analysis_settings$analysis_timezone,
    "America/New_York"
  )
})

test_that("keeping source time unchanged requires one shared time zone", {
  first <- m4_record("First UTC")
  second <- m4_record("Second UTC", schema = "different")
  records <- stats::setNames(list(first, second), c(first$id, second$id))
  ready <- preview_append_merge(
    records,
    m4_separate_spec(
      records,
      time_alignment = "keep_source",
      output_timezone = "UTC"
    )
  )
  expect_true(ready$can_apply)

  differing <- m4_records()
  blocked <- preview_append_merge(
    differing,
    m4_separate_spec(
      differing,
      time_alignment = "keep_source",
      output_timezone = "UTC"
    )
  )
  expect_false(blocked$can_apply)
  expect_match(paste(blocked$errors, collapse = " "), "one shared time zone")
  expect_identical(blocked$error_step, "time")
})

test_that("optional type conflicts require separate targets or explicit text coercion", {
  first <- m4_record("Character optional")
  second <- m4_record(
    "Factor optional",
    optional_as_factor = TRUE,
    instants = c(1767258000, 1767258060, 1767258000)
  )
  records <- stats::setNames(list(first, second), c(first$id, second$id))
  mapping <- function(record, prefix, coercion)
    new_append_source_mapping(
      record$id,
      "Id",
      "Datetime",
      "MEDI",
      paste0(prefix, "_MEDI"),
      prefix,
      optional_columns = "Context",
      optional_targets = "Context",
      optional_coercions = coercion
    )
  make_spec <- function(coercion)
    new_append_spec(
      list(mapping(first, "A", coercion), mapping(second, "B", coercion)),
      "Optional mapping",
      "prefix_source",
      "separate",
      "keep_marked",
      "not_applicable"
    )
  blocked <- preview_append_merge(records, make_spec("none"))
  expect_false(blocked$can_apply)
  expect_match(paste(blocked$errors, collapse = " "), "incompatible types")
  expect_identical(blocked$error_step, "mapping")
  coerced <- preview_append_merge(records, make_spec("character"))
  expect_true(coerced$can_apply)
  expect_match(paste(coerced$warnings, collapse = " "), "as text")
  expect_type(unserialize(coerced$merged_payload)$Context, "character")
})

test_that("existing duplicates are retained and preserved IDs govern overlap", {
  first <- m4_record(
    "Duplicate source",
    instants = c(1767254400, 1767254400, 1767254460),
    participant = c("P01", "P01", "P01")
  )
  second <- m4_record("Overlap source")
  records <- stats::setNames(list(first, second), c(first$id, second$id))
  mappings <- list(
    new_append_source_mapping(
      first$id,
      "Id",
      "Datetime",
      "MEDI",
      "first_MEDI",
      "A"
    ),
    new_append_source_mapping(
      second$id,
      "Id",
      "Datetime",
      "MEDI",
      "second_MEDI",
      "B"
    )
  )
  error_spec <- new_append_spec(
    mappings,
    "Blocked collisions",
    "preserve",
    "separate",
    "keep_marked",
    "error"
  )
  blocked <- preview_append_merge(records, error_spec)
  expect_false(blocked$can_apply)
  expect_identical(blocked$error_step, "sources")
  expect_gt(blocked$diagnostics$duplicate_rows, 0L)
  expect_gt(blocked$diagnostics$overlap_rows, 0L)
  expect_match(paste(blocked$warnings, collapse = " "), "retains every row")

  keep_spec <- new_append_spec(
    mappings,
    "Marked collisions",
    "preserve",
    "separate",
    "keep_marked",
    "keep_marked"
  )
  kept <- preview_append_merge(records, keep_spec)
  expect_true(kept$can_apply)
  data <- unserialize(kept$merged_payload)
  expect_equal(sum(data$llw_duplicate_key), kept$diagnostics$duplicate_rows)
  expect_equal(sum(data$llw_overlap_key), kept$diagnostics$overlap_rows)
  expect_equal(nrow(data), 6L)
  expect_setequal(unique(data$Source), c("Duplicate source", "Overlap source"))

  prefixed <- preview_append_merge(
    records,
    new_append_spec(
      mappings,
      "Prefixed collisions",
      "prefix_source",
      "separate",
      "keep_marked",
      "not_applicable"
    )
  )
  expect_true(prefixed$can_apply)
  expect_gt(prefixed$diagnostics$duplicate_rows, 0L)
  expect_equal(prefixed$diagnostics$overlap_rows, 0L)
  expect_false("Source" %in% names(unserialize(prefixed$merged_payload)))

  record <- new_appended_dataset_record(kept)
  expect_identical(
    record$source_manifest$details$source_group_column,
    "Source"
  )
})

test_that("missing optional metadata remains visible without blocking a separate append", {
  records <- m4_records()
  records[[2L]]$source_manifest$source_timezone <- NA_character_
  records[[2L]]$factual_metadata$device <- NULL
  records[[2L]]$source_manifest$details$device <- NULL
  records[[2L]] <- validate_dataset_record(records[[2L]])

  preview <- preview_append_merge(records, m4_separate_spec(records))
  expect_true(preview$can_apply)
  expect_match(
    paste(preview$warnings, collapse = " "),
    "source timezone is unknown"
  )
  expect_true(any(is.na(preview$profiles$source_timezone)))
  expect_true(any(is.na(preview$profiles$device)))
})

test_that("missing required mapping types return a recoverable named error", {
  valid <- m4_record("Valid source")
  invalid <- new_dataset_record(
    raw_data = data.frame(
      Id = "P01",
      Datetime = "2026-01-01 08:00:00",
      Value = 1,
      stringsAsFactors = FALSE
    ),
    display_name = "String-time source",
    source_manifest = new_source_manifest(
      source_type = "m4_fixture",
      source_timezone = "UTC"
    ),
    analysis_settings = list(primary_variable = "Value")
  )
  records <- stats::setNames(
    list(valid, invalid),
    c(valid$id, invalid$id)
  )

  expect_error(
    append_spec_from_inputs(
      list(
        source_ids = names(records),
        measurement_strategy = "separate"
      ),
      records
    ),
    class = "llw_validation_error",
    regexp = "String-time source"
  )
})

test_that("append UI helpers expose select-all and state-dependent actions", {
  eligible <- c("Context", "Temperature")
  expect_identical(
    append_selected_optional_columns(append_optional_all_value(), eligible),
    eligible
  )
  expect_identical(
    append_selected_optional_columns("Context", eligible),
    "Context"
  )

  initial <- htmltools::renderTags(
    append_preview_actions_ui(NS("append"), FALSE, FALSE, FALSE)
  )$html
  expect_match(initial, "Preview append", fixed = TRUE)
  expect_match(initial, "llw-append-primary-action", fixed = TRUE)
  expect_match(initial, 'disabled="disabled"', fixed = TRUE)

  stale <- htmltools::renderTags(
    append_preview_actions_ui(NS("append"), TRUE, FALSE, TRUE)
  )$html
  expect_match(stale, "Update preview", fixed = TRUE)

  ready <- htmltools::renderTags(
    append_preview_actions_ui(NS("append"), TRUE, TRUE, TRUE)
  )$html
  expect_match(ready, "Preview again", fixed = TRUE)
  expect_false(grepl('id="append-apply" disabled=', ready, fixed = TRUE))

  mapping_action <- htmltools::renderTags(
    append_error_step_action_ui(NS("append"), "mapping")
  )$html
  expect_match(mapping_action, "Go to Step 2", fixed = TRUE)
  expect_match(mapping_action, "Time &amp; measurements", fixed = TRUE)

  local_action <- htmltools::renderTags(
    append_error_step_action_ui(NS("append"), "review")
  )$html
  expect_match(local_action, "Change the new dataset name above", fixed = TRUE)
})

test_that("mapping cards keep core controls content-sized", {
  record <- append_merge_showcase_records()[[2L]]
  optional_id <- append_mapping_input_id(record$id, "optional")
  input <- stats::setNames(list("Context"), optional_id)
  session <- list(ns = NS("append"))
  html <- htmltools::renderTags(
    append_mapping_card_ui(
      record,
      index = 2L,
      input = input,
      session = session,
      strategy = "separate"
    )
  )$html
  core_layout <- regmatches(
    html,
    regexpr(
      "<bslib-layout-columns[^>]*llw-append-core-mapping[^>]*>",
      html,
      perl = TRUE
    )
  )

  expect_length(core_layout, 1L)
  expect_false(grepl("html-fill-item", core_layout, fixed = TRUE))
  expect_match(html, "llw-append-advanced-mapping", fixed = TRUE)
})

test_that("append UI exposes output-time explanations and compact icon metrics", {
  records <- m4_records()
  controls <- htmltools::renderTags(
    append_time_controls_ui(NS("append"), list(), records, names(records))
  )$html
  expect_match(controls, "Preserve clock time", fixed = TRUE)
  expect_match(controls, "Preserve the absolute instant", fixed = TRUE)
  expect_match(controls, "force_tz", fixed = TRUE)
  expect_match(controls, "with_tz", fixed = TRUE)
  expect_match(controls, "llw-radio-choice__label", fixed = TRUE)
  expect_match(
    controls,
    'value="keep_source" disabled="disabled"',
    fixed = TRUE
  )

  diagnostics <- htmltools::renderTags(
    append_diagnostic_summary_ui(list(
      diagnostics = list(
        source_datasets = 2L,
        result_rows = 6L,
        duplicate_rows = 0L,
        overlap_rows = 0L
      )
    ))
  )$html
  expect_equal(
    stringr::str_count(diagnostics, "llw-append-diagnostics__icon"),
    4L
  )
  expect_match(diagnostics, "Source datasets", fixed = TRUE)
})

test_that("showcase includes a matching fixture for identical-column testing", {
  records <- append_merge_showcase_records()
  expect_length(records, 3L)
  expect_true(any(grepl(
    "matching MEDI",
    vapply(
      records,
      `[[`,
      character(1),
      "display_name"
    )
  )))
  candidates <- append_common_measurement_candidates(
    records,
    names(records)[c(1L, 3L)]
  )
  expect_equal(candidates$column, "MEDI")
  expect_equal(candidates$unit, "lux")
})

test_that("append preview rejects an existing dataset name without renaming it", {
  records <- m4_records()
  spec <- m4_separate_spec(records)
  spec$display_name <- records[[1L]]$display_name
  preview <- preview_append_merge(records, spec)

  expect_false(preview$can_apply)
  expect_identical(preview$error_step, "review")
  expect_match(paste(preview$errors, collapse = " "), "already in use")
})

test_that("source prefixes only appear for the prefixed ID policy", {
  record <- m4_record("Identity UI")
  session <- list(ns = NS("append"))
  prefixed <- htmltools::renderTags(
    append_identity_card_ui(
      record,
      1L,
      list(),
      session,
      "prefix_source"
    )
  )$html
  preserved <- htmltools::renderTags(
    append_identity_card_ui(record, 1L, list(), session, "preserve")
  )$html

  expect_match(prefixed, "Source prefix", fixed = TRUE)
  expect_false(grepl("Source prefix", preserved, fixed = TRUE))
  expect_match(prefixed, "ID column", fixed = TRUE)
  expect_false(grepl("Participant column", prefixed, fixed = TRUE))
})
