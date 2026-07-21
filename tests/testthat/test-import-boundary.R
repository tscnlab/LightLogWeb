test_that("uploads keep safe original basenames in isolated directories", {
  root <- tempfile("llw-upload-root-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source <- tempfile(fileext = ".csv")
  on.exit(unlink(source, force = TRUE), add = TRUE)
  bytes <- charToRaw("Id,Datetime,MEDI\nP01,2026-01-01 08:00:00,10\n")
  writeBin(bytes, source)
  files <- data.frame(
    name = "../unsafe name.csv",
    datapath = source,
    stringsAsFactors = FALSE
  )

  staged <- stage_import_files(files, root)

  expect_s3_class(staged, "llw_staged_uploads")
  expect_identical(staged$original_name, "unsafe name.csv")
  expect_identical(basename(staged$staged_path), "unsafe name.csv")
  expect_identical(basename(dirname(staged$staged_path)), "0001")
  expect_true(startsWith(staged$staged_path, normalizePath(root)))
  expect_identical(readBin(staged$staged_path, "raw", n = length(bytes)), bytes)
  expect_identical(staged$sha256[[1L]], sha256_file(source))
  expect_match(staged$sha256, "^sha256:[0-9a-f]{64}$")
  expect_equal(staged$size_bytes, length(bytes))
  if (.Platform$OS.type != "windows") {
    expect_identical(
      as.integer(file.info(dirname(staged$staged_path))$mode),
      448L
    )
  }
})

test_that("raw import requests reject a modified staged source", {
  root <- tempfile("llw-tamper-root-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source <- tempfile(fileext = ".txt")
  on.exit(unlink(source, force = TRUE), add = TRUE)
  writeBin(charToRaw("original bytes"), source)
  staged <- stage_import_files(
    data.frame(name = "fixture.txt", datapath = source),
    root
  )

  writeBin(charToRaw("modified bytes"), staged$staged_path)

  expect_error(
    new_raw_import_request(
      device = "ActLumus",
      staged_files = staged,
      timezone = "UTC",
      not_before = as.Date("2001-01-01"),
      id_mode = "automated"
    ),
    class = "llw_resource_error",
    regexp = "changed"
  )
})

test_that("the worker rechecks staged bytes after request construction", {
  fixture <- m3_stage_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(fixture$source, force = TRUE), add = TRUE)
  request <- m3_raw_import_request(fixture$staged)
  path <- fixture$staged$staged_path[[1L]]
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  last <- length(bytes)
  bytes[[last]] <- as.raw(bitwXor(as.integer(bytes[[last]]), 1L))
  writeBin(bytes, path)

  expect_error(
    raw_import_worker(request, spec = NULL),
    class = "llw_resource_error",
    regexp = "changed after it was copied"
  )
})

test_that("raw import requests snapshot serializable source provenance", {
  root <- tempfile("llw-request-root-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  source <- tempfile(fileext = ".txt")
  on.exit(unlink(source, force = TRUE), add = TRUE)
  writeLines("fixture", source, useBytes = TRUE)
  staged <- stage_import_files(
    data.frame(name = "fixture.txt", datapath = source),
    root
  )
  request <- new_raw_import_request(
    device = "ActLumus",
    staged_files = staged,
    timezone = "UTC",
    not_before = as.Date("2001-01-01"),
    id_mode = "automated"
  )
  manifest_arguments <- raw_import_manifest_arguments(request)

  expect_s3_class(request, "llw_raw_import_request")
  expect_null(nonserializable_path(request))
  expect_identical(request$import_arguments$filename, staged$staged_path)
  expect_identical(manifest_arguments$filename, "fixture.txt")
  expect_false(any(grepl(root, unlist(manifest_arguments), fixed = TRUE)))

  record <- new_imported_dataset_record(list(
    name = "Imported fixture",
    data = m1_fixture_data(),
    device = "ActLumus",
    tz = "UTC",
    variable = "MEDI",
    source_files = staged,
    import_arguments = manifest_arguments
  ))
  expect_identical(record$source_manifest$hashes[[1L]], staged$sha256[[1L]])
  expect_identical(dataset_raw_data(record), m1_fixture_data())
  expect_match(record$raw_checksum, "^sha256:[0-9a-f]{64}$")
  expect_false(any(grepl(root, unlist(record$source_manifest), fixed = TRUE)))
})

test_that("the runtime rejects uploads above request or session limits", {
  source <- tempfile(fileext = ".csv")
  on.exit(unlink(source, force = TRUE), add = TRUE)
  writeBin(as.raw(rep(1L, 2048L)), source)
  files <- data.frame(name = "large.csv", datapath = source)

  upload_profile <- resolve_runtime_profile(
    "hosted",
    max_upload_mb = 0.001,
    workers = 0
  )
  upload_runtime <- new_session_runtime(upload_profile, session = NULL)
  on.exit(upload_runtime$cleanup(), add = TRUE)
  expect_error(
    upload_runtime$stage_uploads(files),
    class = "llw_resource_error",
    regexp = "upload exceeds"
  )

  session_profile <- resolve_runtime_profile("hosted", workers = 0)
  session_profile$session_temp_max_bytes <- 1024
  session_runtime <- new_session_runtime(session_profile, session = NULL)
  on.exit(session_runtime$cleanup(), add = TRUE)
  expect_error(
    session_runtime$stage_uploads(files),
    class = "llw_resource_error",
    regexp = "temporary-storage limit"
  )
})

test_that("import success summaries handle grouped, ungrouped, and empty data", {
  grouped <- dplyr::group_by(m1_fixture_data(), Id)

  expect_identical(import_success_summary(grouped)$groups, 2L)
  expect_identical(import_success_summary(m1_fixture_data())$groups, 2L)
  expect_identical(import_success_summary(data.frame())$groups, 0L)
  expect_error(
    import_success_summary(list()),
    class = "llw_import_error"
  )
})
