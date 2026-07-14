test_that("dataset objects preserve immutable raw data", {
  original <- make_regular_light()
  dataset <- llw_dataset(original, metadata = list(variable = "MEDI"), name = "regular")
  prepared <- llw_prepare(dataset, value_range = c(0, 300), interval = "5 min")

  expect_s3_class(dataset, "llw_dataset")
  expect_identical(dataset$raw_data, dataset$prepared_data)
  expect_identical(dataset$raw_data, prepared$raw_data)
  expect_lt(nrow(prepared$prepared_data), nrow(dataset$prepared_data))
  expect_true(".llw_invalid_reason" %in% names(prepared$prepared_data))
})

test_that("quality detects missingness, duplicates, gaps, and irregular timestamps", {
  data <- make_regular_light(days = 1)
  data$MEDI[[10]] <- NA_real_
  data <- data[-20, ]
  data$Datetime[[30]] <- data$Datetime[[29]] + 30
  data <- dplyr::bind_rows(data, data[40, ])
  quality <- llw_quality(llw_dataset(data, metadata = list(daily_missing_max = 0.0001)))$value

  expect_gt(quality$overview$explicit_missing, 0)
  expect_gt(quality$overview$duplicate_rows, 0)
  expect_true(isTRUE(quality$overview$has_gaps))
  expect_true(isTRUE(quality$overview$has_irregulars))
  expect_gt(nrow(quality$gap_summary), 0)
  expect_false(quality$by_day$valid)
})

test_that("quality handles multiple IDs and sampling epochs", {
  one <- make_regular_light(days = 1, epoch = 60, id = "one")
  five <- make_regular_light(days = 1, epoch = 300, id = "five")
  quality <- llw_quality(llw_dataset(dplyr::bind_rows(one, five)))$value
  expect_equal(quality$overview$ids, 2)
  expect_equal(sort(unique(quality$by_day$epoch_seconds)), c(60, 300))
})

test_that("normalized import and participant ID preview are deterministic", {
  path <- tempfile(fileext = ".csv")
  data <- make_regular_light(days = 1)
  utils::write.csv(data, path, row.names = FALSE)
  imported <- llw_import(path, device = "normalized", timezone = "UTC")

  expect_s3_class(imported, "llw_dataset")
  expect_equal(nrow(imported$raw_data), nrow(data))
  expect_equal(llw_preview_ids(c("P01_eye.csv", "P02_eye.csv"), "extract")$Id, c("P01", "P02"))
  expect_error(llw_preview_ids("P01.csv", "extract", pattern = "Z(.*)"), "did not match")
})

test_that("malformed data and incompatible merges fail clearly", {
  expect_error(llw_dataset(data.frame(Id = "P01", MEDI = 1)), "Datetime")
  expect_error(llw_dataset(data.frame(Id = "P01", Datetime = "now", MEDI = 1)), "POSIXct")
  one <- llw_dataset(make_regular_light(), metadata = list(timezone = "UTC"))
  two <- llw_dataset(make_regular_light(tz = "Europe/Berlin"), metadata = list(timezone = "Europe/Berlin"))
  expect_error(llw_merge(one, two), "same analysis timezone")
})
