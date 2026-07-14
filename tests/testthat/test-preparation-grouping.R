test_that("DST day coverage uses actual local day length", {
  spring_start <- as.POSIXct("2025-03-30 00:00:00", tz = "Europe/Berlin")
  spring_end <- as.POSIXct("2025-03-31 00:00:00", tz = "Europe/Berlin")
  spring <- tibble::tibble(Id = "P01", Datetime = seq(spring_start, spring_end - 60, by = 60), MEDI = 1)
  fall_start <- as.POSIXct("2025-10-26 00:00:00", tz = "Europe/Berlin")
  fall_end <- as.POSIXct("2025-10-27 00:00:00", tz = "Europe/Berlin")
  fall <- tibble::tibble(Id = "P02", Datetime = seq(fall_start, fall_end - 60, by = 60), MEDI = 1)

  spring_quality <- llw_quality(llw_dataset(spring, metadata = list(timezone = "Europe/Berlin")))$value
  fall_quality <- llw_quality(llw_dataset(fall, metadata = list(timezone = "Europe/Berlin")))$value
  expect_equal(spring_quality$by_day$expected, 1380)
  expect_equal(fall_quality$by_day$expected, 1500)
  expect_equal(c(spring_quality$by_day$coverage, fall_quality$by_day$coverage), c(1, 1))
})

test_that("timezone conversion and reinterpretation have distinct semantics", {
  dataset <- llw_dataset(make_regular_light(tz = "UTC"), metadata = list(timezone = "UTC"))
  preserved <- llw_prepare(dataset, timezone = "Europe/Berlin", timezone_mode = "preserve_instant", gap_policy = "leave")
  reinterpreted <- llw_prepare(dataset, timezone = "Europe/Berlin", timezone_mode = "reinterpret", gap_policy = "leave")
  expect_equal(as.numeric(dataset$prepared_data$Datetime), as.numeric(preserved$prepared_data$Datetime))
  expect_false(identical(as.numeric(dataset$prepared_data$Datetime), as.numeric(reinterpreted$prepared_data$Datetime)))
  expect_equal(format(dataset$prepared_data$Datetime[[1]], tz = "UTC"), format(reinterpreted$prepared_data$Datetime[[1]], tz = "Europe/Berlin"))
})

test_that("preparation inserts explicit gaps, invalidates ranges, and filters incomplete days", {
  data <- make_regular_light(days = 2)
  data$MEDI[[5]] <- -10
  data <- data[-seq(1441, 2000), ]
  dataset <- llw_dataset(data)
  prepared <- llw_prepare(dataset, value_range = c(0, 1000), gap_policy = "explicit_na", daily_missing_max = 0.2)

  expect_true(any(prepared$prepared_data$.llw_invalid_reason == "outside_0_1000", na.rm = TRUE))
  expect_true(anyNA(prepared$prepared_data$MEDI))
  expect_equal(length(unique(as.Date(prepared$prepared_data$Datetime))), 1)
})

test_that("cross-midnight annotations and grouping are auditable", {
  data <- make_regular_light(start = "2025-01-01 00:00:00", days = 2, tz = "Europe/Berlin")
  dataset <- llw_dataset(data, metadata = list(timezone = "Europe/Berlin", coordinates = c(48.1, 11.6)))
  intervals <- data.frame(
    Id = "P01",
    start = as.POSIXct("2025-01-01 22:00:00", tz = "Europe/Berlin"),
    end = as.POSIXct("2025-01-02 07:00:00", tz = "Europe/Berlin"),
    State = "sleep"
  )
  annotated <- llw_annotate(dataset, intervals)
  grouped <- llw_group(annotated, dimensions = c("participant", "date", "day_type", "clock_window", "photoperiod", "annotation"), clock_window = c(22, 7), annotation = "State")

  expect_equal(sum(annotated$prepared_data$State == "sleep", na.rm = TRUE), 9 * 60)
  expect_true(all(c("Id", ".llw_date", ".llw_day_type", ".llw_clock_window", "photoperiod.state", "State") %in% grouped$groups))
  expect_equal(unique(grouped$prepared_data$.llw_clock_window[which(grouped$prepared_data$State == "sleep")]), "inside")
})

test_that("grouping validates prerequisites", {
  dataset <- llw_dataset(make_regular_light())
  expect_error(llw_group(dataset, dimensions = "photoperiod"), "coordinates")
  expect_error(llw_group(dataset, dimensions = "annotation", annotation = "Sleep"), "Missing annotation")
  expect_error(llw_group(dataset, dimensions = "clock_window", clock_window = c(-1, 6)), "between 0 and 24")
})
