gap_preview_fixture <- function(n = 8L) {
  raw <- data.frame(
    Id = "P1",
    Datetime = as.POSIXct("2026-01-01 12:00:00", tz = "UTC") +
      seq.int(0, length.out = n) * 10,
    MEDI = as.numeric(seq_len(n)),
    file_group_id = "file_a"
  )
  new_dataset_record(
    dplyr::group_by(raw, Id),
    "Gap marks",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI")
  )
}

test_that("absent source timestamps become explicit blue gaps after expansion", {
  record <- gap_preview_fixture()
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  filter <- new_preparation_step(
    "variable",
    list(variable = "MEDI", mode = "range", lower = 3, upper = 4,
         outside = TRUE, missing = "keep", values = character()),
    context
  )
  gaps <- new_preparation_step(
    "gaps", list(epoch = "dominant", seconds = 10, bounds = "recording"),
    context
  )
  filtered <- m8_preview(record, new_recipe(list(filter)))
  expanded <- m8_preview(record, new_recipe(list(filter, gaps)))
  expect_setequal(filtered$comparison$view$gaps$Datetime, raw$Datetime[3:4])
  expect_true(all(filtered$comparison$view$gaps$Kind == "implicit"))
  expect_equal(filtered$comparison$after$rows, 6)
  expect_false(anyNA(unserialize(filtered$candidate$prepared_payload)$MEDI))

  result <- unserialize(expanded$candidate$prepared_payload)
  reference <- LightLogR::gap_handler(raw[-c(3, 4), ], epoch = "10 seconds")
  expect_identical(result$Datetime, reference$Datetime)
  expect_identical(result$MEDI, reference$MEDI)
  expect_true(all(result$is.implicit[3:4]))
  expect_setequal(expanded$comparison$view$gaps$Datetime, raw$Datetime[3:4])
  expect_true(all(expanded$comparison$view$gaps$Kind == "explicit"))
  expect_identical(expanded$candidate$raw_payload, record$raw_payload)

  # Prefix selection shows the filter before its later gap expansion.
  prefix <- preparation_task_worker(list(
    action = "view", record = record, recipe = new_recipe(list(filter, gaps)),
    view = list(id = "P1", day = "2026-01-01", step = "1"), budget = 1024^3
  ))$view
  expect_identical(prefix$gaps, filtered$comparison$view$gaps)
})

test_that("NA marks include existing and newly missing values, but not log exclusions", {
  record <- gap_preview_fixture()
  raw <- unserialize(record$raw_payload)
  raw$MEDI <- c(NA, 0, -1, 3, 4, 5, Inf, 7)
  after <- raw
  after$MEDI[4] <- NA_real_
  context <- m8_context(record)
  view <- preparation_day_view(raw, integer(8), context,
                               prepared = after, source_rows = 1:8)
  expect_setequal(view$gaps$Datetime, raw$Datetime[c(1, 4)])
  expect_true(all(view$gaps$Kind == "explicit"))
  for (scale in c("linear", "log", "symlog")) {
    settings <- preparation_axis_settings(record, list(scale = scale))
    plot <- preparation_preview_plot(view, context, settings)
    built <- ggplot2::ggplot_build(plot)
    rugs <- Filter(function(layer) inherits(layer$geom, "GeomRug"), plot$layers)
    expect_setequal(rugs[[2]]$data$Datetime, raw$Datetime[c(1, 4)])
    expect_true("#3675C5" %in% unlist(lapply(built$data, `[[`, "colour")))
  }
})

test_that("gap matching respects duplicates, recording identity and current groups", {
  raw <- unserialize(gap_preview_fixture()$raw_payload)
  raw$State <- "one"
  raw <- dplyr::bind_rows(raw, raw[3, ], raw[3, ], raw[3, ])
  raw$file_group_id[10] <- "file_b"
  raw$State[11] <- "two"
  raw <- dplyr::group_by(raw, Id, State)
  marks <- function(removed, after = raw[!removed, ]) preparation_gap_marks(
    raw, after, removed, "MEDI", "P1", "2026-01-01", "UTC"
  )
  # One duplicate remains in the same group and file.
  expect_equal(nrow(marks(seq_len(11) == 3)), 0)
  # A different file or group cannot replace both lost copies.
  removed <- seq_len(11) %in% c(3, 9)
  expect_equal(marks(removed)$Datetime, raw$Datetime[3])
  expect_equal(marks(removed)$Kind, "implicit")
  # Coexisting NA in another recording remains visible alongside the absence.
  after <- raw[!removed, ]
  after$MEDI[after$file_group_id == "file_b"] <- NA_real_
  expect_setequal(marks(removed, after)$Kind, c("implicit", "explicit"))
})

test_that("marks survive line sampling, whole-day removal and time-zone changes", {
  record <- gap_preview_fixture(6000L)
  raw <- unserialize(record$raw_payload)
  context <- m8_context(record)
  sampled <- unique(round(seq(1, 4320, length.out = 2000)))
  removed_row <- setdiff(2:4319, sampled)[1]
  removed <- seq_len(nrow(raw)) == removed_row
  view <- preparation_day_view(raw, as.integer(removed), context,
                               list(id = "P1", day = "2026-01-01"))
  expect_equal(nrow(view$plot), 2000)
  expect_false(raw$Datetime[removed_row] %in% view$plot$Datetime)
  expect_equal(view$gaps$Datetime, raw$Datetime[removed_row])

  empty <- preparation_day_view(raw, rep(1L, nrow(raw)), context,
                                list(id = "P1", day = "2026-01-01"),
                                prepared = raw[FALSE, ], source_rows = integer())
  expect_equal(nrow(empty$gaps), 4320)
  expect_true(all(empty$gaps$Kind == "implicit"))

  # Retiming is not row removal. NA marks use the preview's actual instants.
  after <- raw
  after$Datetime <- lubridate::force_tz(after$Datetime, "Europe/Berlin")
  after$MEDI[2] <- NA_real_
  view <- preparation_day_view(raw, integer(nrow(raw)), context,
                               list(id = "P1", day = "2026-01-01"),
                               prepared = after, source_rows = seq_len(nrow(raw)))
  expect_equal(view$gaps$Kind, "explicit")
  expect_equal(as.numeric(view$gaps$Datetime), as.numeric(after$Datetime[2]))
})

test_that("pre-existing absent timestamps are not labelled as recipe removals", {
  record <- gap_preview_fixture()
  raw <- unserialize(record$raw_payload)[-4, ]
  raw$MEDI[5] <- NA_real_
  marks <- preparation_gap_marks(raw, raw, rep(FALSE, 7), "MEDI",
                                 "P1", "2026-01-01", "UTC")
  expect_equal(marks$Kind, "explicit")
  expect_equal(marks$Datetime, raw$Datetime[5])
})
