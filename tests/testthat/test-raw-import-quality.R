test_that("quality audit separates gaps, irregularity, duplicates, and missingness", {
  data <- m3_quality_fixture()
  data$is.implicit <- c(FALSE, NA, FALSE, FALSE, FALSE, FALSE)
  quality <- summarize_raw_import_quality(data, "UTC")
  participant <- quality$participants

  expect_s3_class(quality, "llw_raw_import_quality")
  expect_identical(participant$dominant_epoch_seconds, 60)
  expect_identical(participant$implicit_gap_epochs, 1)
  expect_identical(nrow(quality$gaps), 1L)
  expect_identical(quality$gaps$lag_seconds, 85)
  expect_identical(quality$gaps$dominant_epoch_seconds, 60)
  expect_identical(
    quality$gaps$start,
    as.POSIXct("2026-01-01 00:02:35", tz = "UTC")
  )
  expect_identical(
    quality$gaps$end,
    as.POSIXct("2026-01-01 00:04:00", tz = "UTC")
  )
  expect_identical(participant$irregular_observations, 1L)
  expect_identical(participant$duplicate_timestamp_rows, 2L)
  expect_identical(participant$explicit_missing_values, 1L)
  expect_match(
    paste(quality$warnings, collapse = " "),
    "no timestamps.*imputed"
  )
})

test_that("fast gap counts respect each participant's dominant epoch", {
  ids <- sprintf("P%02d", seq_len(6L))
  data <- do.call(
    rbind,
    lapply(seq_along(ids), function(index) {
      datetime <- seq(
        as.POSIXct("2026-01-01 08:00:00", tz = "UTC") +
          (index - 1L) * 1800,
        by = "1 min",
        length.out = 90L
      )
      if (index %% 2L == 0L) {
        datetime <- datetime[-(20:35)]
      }
      if (index == 3L) {
        datetime <- datetime[c(1L, 90L)]
      }
      data.frame(
        Id = factor(ids[[index]], levels = ids),
        Datetime = datetime,
        MEDI = seq_along(datetime),
        file.name = ids[[index]],
        stringsAsFactors = FALSE
      )
    })
  )

  quality <- summarize_raw_import_quality(data, "UTC")
  gap_counts <- stats::setNames(
    quality$participants$implicit_gap_epochs,
    quality$participants$Id
  )
  expect_equal(unname(gap_counts[c("P02", "P04", "P06")]), rep(16, 3L))
  expect_equal(unname(gap_counts[c("P01", "P03", "P05")]), rep(0, 3L))
  expect_identical(nrow(quality$gaps), 3L)
  expect_setequal(quality$gaps$Id, c("P02", "P04", "P06"))
  expect_true(all(
    quality$gaps$lag_seconds > quality$gaps$dominant_epoch_seconds
  ))
})

test_that("quality review derives gap intervals without changing imported data", {
  data <- m3_quality_fixture()
  before <- data

  quality <- summarize_raw_import_quality(data, "UTC")

  expect_identical(data, before)
  expect_identical(
    names(quality$gaps),
    c(
      "Id",
      "start",
      "end",
      "lag_seconds",
      "dominant_epoch_seconds"
    )
  )
})

test_that("quality audit reports ordering, ranges, zeros, and negatives", {
  data <- data.frame(
    Id = factor(rep("P01", 3L)),
    Datetime = as.POSIXct(
      c(
        "2026-01-01 00:01:00",
        "2026-01-01 00:00:00",
        "2026-01-01 00:02:00"
      ),
      tz = "UTC"
    ),
    MEDI = c(0, -1, 2),
    file.name = "P01",
    stringsAsFactors = FALSE
  )

  quality <- summarize_raw_import_quality(data, "UTC")
  profile <- quality$signal_profile[
    quality$signal_profile$variable == "MEDI",
    ,
    drop = FALSE
  ]

  expect_false(quality$participants$ordered)
  expect_match(paste(quality$warnings, collapse = " "), "not chronological")
  expect_identical(quality$summary$start, min(data$Datetime))
  expect_identical(quality$summary$end, max(data$Datetime))
  expect_identical(quality$summary$participants, 1L)
  expect_identical(profile$exact_zero, 1L)
  expect_identical(profile$negative, 1L)
  expect_identical(profile$minimum, -1)
  expect_equal(profile$q25, -0.5)
  expect_identical(profile$median, 0)
  expect_equal(profile$mean, 1 / 3)
  expect_identical(profile$q75, 1)
  expect_identical(profile$maximum, 2)
})

test_that("canonical validation blocks missing columns and wrong time zones", {
  missing_id <- data.frame(
    Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
    MEDI = 1
  )
  expect_error(
    summarize_raw_import_quality(missing_id, "UTC"),
    class = "llw_import_error",
    regexp = "missing column"
  )

  character_time <- data.frame(Id = "P01", Datetime = "2026-01-01", MEDI = 1)
  expect_error(
    summarize_raw_import_quality(character_time, "UTC"),
    class = "llw_import_error",
    regexp = "not POSIXct"
  )

  wrong_zone <- m1_fixture_data()
  expect_error(
    summarize_raw_import_quality(wrong_zone, "Europe/Berlin"),
    class = "llw_import_error",
    regexp = "selected source time zone"
  )

  duplicate_names <- m1_fixture_data()
  names(duplicate_names)[[3L]] <- "Id"
  expect_error(
    summarize_raw_import_quality(duplicate_names, "UTC"),
    class = "llw_import_error",
    regexp = "duplicate column name"
  )
})

test_that("primary-variable eligibility gives a reason for every column", {
  data <- m1_fixture_data()
  data$label <- "ambient"
  data$all_missing <- NA_real_
  data$infinite <- c(1, Inf, 3)
  data$matrix_value <- I(matrix(seq_len(6L), nrow = 3L, ncol = 2L))
  eligibility <- primary_variable_eligibility(data)

  expect_s3_class(eligibility, "llw_variable_eligibility")
  expect_identical(eligibility$variable, names(data))
  expect_true(eligibility$eligible[eligibility$variable == "MEDI"])
  expect_identical(
    eligibility$reason[eligibility$variable == "MEDI"],
    "Eligible numeric column."
  )
  expect_false(eligibility$eligible[eligibility$variable == "Id"])
  expect_false(eligibility$eligible[eligibility$variable == "label"])
  expect_false(eligibility$eligible[eligibility$variable == "all_missing"])
  expect_false(eligibility$eligible[eligibility$variable == "infinite"])
  expect_false(eligibility$eligible[eligibility$variable == "matrix_value"])
  expect_true(all(nzchar(eligibility$reason)))

  profile <- raw_import_signal_profile(data, eligibility)
  review <- raw_import_variable_review(eligibility, profile)
  expect_s3_class(review, "llw_variable_review")
  expect_true(review$eligible[review$variable == "MEDI"])
  expect_identical(review$reason[review$variable == "MEDI"], "")
  expect_false(review$eligible[review$variable == "Id"])
  expect_match(
    review$reason[review$variable == "Id"],
    "Structural import column",
    fixed = TRUE
  )
  expect_true(is.na(review$median[review$variable == "Id"]))
})

test_that("DST transitions are diagnosed without changing absolute instants", {
  datetime <- as.POSIXct(
    c("2026-03-29 01:30:00", "2026-03-29 03:30:00"),
    tz = "Europe/Berlin"
  )
  data <- data.frame(
    Id = factor(c("P01", "P01")),
    Datetime = datetime,
    MEDI = c(1, 2)
  )
  before <- as.numeric(data$Datetime)

  quality <- summarize_raw_import_quality(data, "Europe/Berlin")

  expect_identical(as.numeric(data$Datetime), before)
  expect_identical(quality$participants$dst_transitions, 1L)
  expect_identical(quality$participants$local_clock_anomalies, 1L)
  expect_match(paste(quality$warnings, collapse = " "), "daylight-saving")
})

test_that("bounded previews are deterministic and explicitly labelled", {
  full <- raw_import_preview_indices(3L, max_rows = 10L)
  reduced <- raw_import_preview_indices(1000L, max_rows = 100L)

  expect_identical(full$indices, 1:3)
  expect_false(full$reduced)
  expect_length(reduced$indices, 100L)
  expect_identical(head(reduced$indices), 1:6)
  expect_identical(tail(reduced$indices), 995:1000)
  expect_true(reduced$reduced)
  expect_match(reduced$notice, "Quality diagnostics use the complete")

  grouped <- data.frame(
    Id = factor(c("P01", "P02")),
    value = c(1, 2)
  ) |>
    dplyr::group_by(Id)
  preview <- raw_import_preview_data(grouped, c(1L, 2L))
  expect_false(dplyr::is_grouped_df(preview))
  expect_identical(names(preview), c("Id", "value"))
  expect_identical(as.character(preview$Id), c("P01", "P02"))
})

test_that("raw import overview uses readable axis text", {
  data <- m3_quality_fixture()
  quality <- summarize_raw_import_quality(data, "UTC")
  plot <- plot_raw_import_overview(data, quality, mode = "light")
  colors <- lightlogweb_plot_colors("light")

  expect_identical(plot$theme$axis.text$size, 11)
  expect_identical(plot$theme$axis.title$size, 12)
  expect_s3_class(plot$theme$plot.caption, "element_markdown")
  expect_false(identical(colors[["primary"]], colors[["end"]]))
  expect_false(identical(colors[["primary"]], colors[["gap"]]))
  expect_match(plot$labels$caption, colors[["start"]], fixed = TRUE)
  expect_match(plot$labels$caption, colors[["end"]], fixed = TRUE)
  expect_match(plot$labels$caption, colors[["gap"]], fixed = TRUE)
  expect_match(plot$labels$caption, "Times shown in <b>UTC</b>", fixed = TRUE)
  expect_match(
    plot$labels$caption,
    "font-weight:700'>Start</span>",
    fixed = TRUE
  )
  expect_match(
    plot$labels$caption,
    "font-weight:700'>End</span>",
    fixed = TRUE
  )
  expect_match(
    plot$labels$caption,
    "font-weight:700'>Gap interval</span>",
    fixed = TRUE
  )
  expect_match(
    plot$labels$caption,
    "participant's dominant epoch.",
    fixed = TRUE
  )
  expect_false(grepl("Missing-time-point", plot$labels$caption, fixed = TRUE))
  expect_false(grepl("Data quality checks", plot$labels$caption, fixed = TRUE))
  expect_false(grepl("No missing rows", plot$labels$caption, fixed = TRUE))
  expect_false(grepl("All participant IDs", plot$labels$caption, fixed = TRUE))

  second_participant <- data
  second_participant$Id <- factor("P02")
  second_participant$Datetime <- second_participant$Datetime + 3600
  capped_data <- rbind(data, second_participant)
  capped_data$Id <- factor(
    as.character(capped_data$Id),
    levels = c("P01", "P02")
  )
  capped_quality <- summarize_raw_import_quality(capped_data, "UTC")
  capped_plot <- plot_raw_import_overview(
    capped_data,
    capped_quality,
    mode = "light",
    max_ids = 1L
  )
  expect_match(
    capped_plot$labels$caption,
    "Showing first 1 of 2 participant IDs.",
    fixed = TRUE
  )
  expect_identical(attr(plot, "llw_overview_source"), "LightLogR::gg_overview")
  expect_true(any(vapply(
    plot$layers,
    function(layer) {
      all(c("lag_seconds", "dominant_epoch_seconds") %in% names(layer$data))
    },
    logical(1)
  )))
  interval_layers <- which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomLinerange"),
    logical(1)
  ))
  expect_length(interval_layers, 2L)
  expect_equal(plot$layers[[interval_layers[[1L]]]]$aes_params$linewidth, 2.2)
  expect_equal(plot$layers[[interval_layers[[2L]]]]$aes_params$linewidth, 3.8)
  expect_identical(
    plot$layers[[interval_layers[[2L]]]]$aes_params$linetype,
    "22"
  )
  expect_identical(
    plot$layers[[interval_layers[[2L]]]]$aes_params$colour,
    colors[["gap"]]
  )
})

test_that("phase snapshots name every visible import phase", {
  status <- new_task_status("raw_import", state = "running")
  phases <- raw_import_phase_snapshot(status, request_ready = TRUE)

  expect_identical(phases$phase, names(raw_import_phase_labels()))
  expect_identical(phases$status[[1L]], "complete")
  expect_true(all(phases$status[2:4] == "running"))
  expect_identical(phases$status[[5L]], "pending")
})

test_that("phase snapshots show worker progress one phase at a time", {
  status <- new_task_status("raw_import", state = "running")
  progress <- rbind(
    raw_import_phase_row("validation", 0.2, "Files checked."),
    data.frame(
      phase = "import",
      status = "running",
      elapsed_seconds = NA_real_,
      detail = "LightLogR is reading files.",
      stringsAsFactors = FALSE
    )
  )

  phases <- raw_import_phase_snapshot(
    status,
    request_ready = TRUE,
    progress = progress
  )

  expect_identical(
    phases$status,
    c("complete", "running", "pending", "pending", "pending")
  )
  expect_match(phases$detail[[1L]], "0.2 s", fixed = TRUE)
  expect_identical(phases$detail[[2L]], "LightLogR is reading files.")
})

test_that("the immediate LightLogR sample has metadata and quality provenance", {
  record <- sample_dataset_record()

  expect_s3_class(record, "llw_dataset_record")
  expect_identical(
    record$source_manifest$details$object,
    "sample.data.environment"
  )
  expect_identical(record$analysis_settings$primary_variable, "MEDI")
  expect_s3_class(
    record$provenance$raw_import_quality,
    "llw_raw_import_quality"
  )
  expect_identical(record$factual_metadata$variables$MEDI$unit, "lux")
})

test_that("the IZTECH development snapshot is pinned, attributed, and usable", {
  path <- development_large_dataset_path()
  skip_if_not(file.exists(path), "optional development snapshot is not present")

  record <- melidos_iztech_dataset_record(path)
  data <- dataset_raw_data(record)

  expect_s3_class(record, "llw_dataset_record")
  expect_identical(dim(data), c(151200L, 37L))
  expect_identical(length(unique(data$Id)), 17L)
  expect_identical(lubridate::tz(data$Datetime), "Europe/Istanbul")
  expect_identical(record$analysis_settings$primary_variable, "MEDI")
  expect_identical(record$source_manifest$details$license, "CC BY 4.0")
  expect_identical(
    record$source_manifest$details$source_blob,
    "8a6aef8c7c0e842eaa167d22fef634848b57508a"
  )
  expect_identical(
    record$provenance$snapshot_sha256,
    "sha256:939bea8c78578db3b9840e20a505b0366e3c3ee347c1d838f2c6781844a65ab8"
  )

  altered <- tempfile(fileext = ".rds")
  on.exit(unlink(altered, force = TRUE), add = TRUE)
  expect_true(file.copy(path, altered))
  bytes <- readBin(altered, what = "raw", n = file.info(altered)$size)
  last <- length(bytes)
  bytes[[last]] <- as.raw(bitwXor(as.integer(bytes[[last]]), 1L))
  writeBin(bytes, altered)
  expect_error(
    melidos_iztech_dataset_record(altered),
    class = "llw_resource_error",
    regexp = "checksum"
  )
})
