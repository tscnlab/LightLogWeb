test_that("the ready-to-use catalog exposes small, device-file, and IZTECH options", {
  catalog <- dataset_example_catalog()

  expect_true(all(
    c(
      "sample",
      "actlumus_synthetic",
      "actlumus_4789",
      "iztech"
    ) %in%
      catalog$key
  ))
  expect_true(catalog$available[catalog$key == "sample"])
  expect_identical(
    catalog$available[catalog$key == "actlumus_synthetic"],
    file.exists(development_devicefile_path("actlumus_synthetic"))
  )
  expect_identical(
    catalog$available[catalog$key == "actlumus_4789"],
    file.exists(development_devicefile_path("actlumus_4789"))
  )
  expect_identical(
    catalog$available[catalog$key == "iztech"],
    file.exists(development_large_dataset_path())
  )
  expect_match(
    catalog$description[catalog$key == "actlumus_synthetic"],
    "without stepping through the import form|Already configured",
    ignore.case = TRUE
  )
  choices <- dataset_example_choices()
  expect_setequal(unname(choices), catalog$key[catalog$available])
})

test_that("the small package sample loads directly as an immutable record", {
  sample <- load_dataset_example("sample")

  expect_s3_class(sample, "llw_dataset_record")
  expect_identical(sample$source_manifest$source_type, "package_sample")
})

test_that("the synthetic device file loads directly when it is available", {
  catalog <- dataset_example_catalog()
  skip_if_not(
    catalog$available[catalog$key == "actlumus_synthetic"],
    "synthetic ActLumus fixture is not present"
  )

  device <- load_dataset_example("actlumus_synthetic")

  expect_s3_class(device, "llw_dataset_record")
  expect_identical(device$source_manifest$source_type, "raw_upload")
  expect_identical(
    device$source_manifest$details$development_example$key,
    "actlumus_synthetic"
  )
  expect_identical(
    device$source_manifest$details$development_example$configured_source_timezone,
    "UTC"
  )
  expect_identical(device$factual_metadata$variables$MEDI$unit, "lux")
  expect_gt(nrow(dataset_raw_data(device)), 0L)
  expect_identical(dataset_raw_data(device), dataset_prepared_data(device))
  expect_false(any(grepl(
    "llw-ready-device",
    unlist(device$source_manifest, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))
})

test_that("the provided ActLumus device file is a one-click reviewed record", {
  catalog <- dataset_example_catalog()
  skip_if_not(
    catalog$available[catalog$key == "actlumus_4789"],
    "provided ActLumus fixture is not present"
  )

  device <- load_dataset_example("actlumus_4789")
  data <- dataset_raw_data(device)

  expect_s3_class(device, "llw_dataset_record")
  expect_identical(nrow(data), 4686L)
  expect_identical(lubridate::tz(data$Datetime), "Europe/Berlin")
  expect_identical(device$source_manifest$original_filenames, "4789.txt")
  expect_identical(
    device$source_manifest$details$development_example$key,
    "actlumus_4789"
  )
  expect_identical(device$factual_metadata$variables$MEDI$unit, "lux")
  expect_false(any(grepl(
    "llw-ready-device",
    unlist(device$source_manifest, recursive = TRUE, use.names = FALSE),
    fixed = TRUE
  )))
})

test_that("unavailable optional examples fail with a recoverable message", {
  root <- tempfile("llw-empty-examples-")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  writeLines(
    c("Package: LightLogWeb", "Version: 0.0.0.9000"),
    file.path(root, "DESCRIPTION")
  )

  catalog <- dataset_example_catalog(root)
  expect_true(catalog$available[catalog$key == "sample"])
  expect_false(catalog$available[catalog$key == "iztech"])
  expect_error(
    load_dataset_example("iztech", root),
    class = "llw_unavailable_feature_error",
    regexp = "not present"
  )
})
