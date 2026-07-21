test_that("the browser extension adapter covers every current LightLogR device", {
  contract <- raw_import_extension_contract()
  devices <- raw_import_devices()

  expect_setequal(names(contract), devices)
  expect_length(contract$GENEActiv_GGIR, 0L)
  expect_true(all(c("txt", "zip") %in% contract$ActLumus))
  expect_true(all(c("csv", "txt", "zip") %in% contract$VEET))
  expect_true("xls" %in% contract$Clouclip)
})

test_that("the app device selector can restore every device after a reset", {
  choices <- raw_import_device_choices()

  expect_identical(unname(choices[[1L]]), "")
  expect_identical(names(choices)[[1L]], "Choose a device")
  expect_setequal(unname(choices[-1L]), raw_import_devices())
  expect_true("GENEActiv_GGIR" %in% unname(choices))
})

test_that("device versions come from the current LightLogR public registry", {
  versions <- raw_import_supported_versions("ActLumus")

  expect_named(versions, c("Device", "Version", "Default", "Description"))
  expect_true(any(versions$Default))
  expect_identical(normalize_raw_import_version("ActLumus", ""), "default")
  expect_true("default" %in% unname(get_versions("ActLumus")))
  expect_match(
    get_version_description("ActLumus", "default"),
    "LightLogR format"
  )
})

test_that("filename mappings support several files per participant", {
  automated <- new_filename_id_mapping(
    c("P01_visit.txt.zip", "P02_visit.csv"),
    "automated"
  )
  expect_identical(automated$proposed_id, c("P01_visit.txt", "P02_visit"))

  extracted <- new_filename_id_mapping(
    c("P01_visit.txt", "P02_visit.txt"),
    "extract",
    extract_pattern = "^(P[0-9]+)_"
  )
  expect_identical(extracted$proposed_id, c("P01", "P02"))

  extracted_parts <- new_filename_id_mapping(
    c("P01_a.txt", "P01_b.txt"),
    "extract",
    extract_pattern = "^(P[0-9]+)_"
  )
  expect_identical(extracted_parts$proposed_id, c("P01", "P01"))
  expect_true(all(extracted_parts$duplicate_proposed))

  shared <- new_filename_id_mapping(
    c("visit_a.txt", "visit_b.txt"),
    "manual",
    manual_id = "P01"
  )
  expect_true(all(shared$duplicate_proposed))
})

test_that("raw preflight rejects invalid zones, devices, and extensions", {
  fixture <- m3_stage_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(fixture$source, force = TRUE), add = TRUE)

  expect_error(
    m3_raw_import_request(fixture$staged, timezone = "Not/AZone"),
    class = "llw_validation_error",
    regexp = "Unknown IANA"
  )
  expect_error(
    new_raw_import_request(
      device = "ImaginaryLogger",
      staged_files = fixture$staged,
      timezone = "UTC",
      not_before = as.Date("2001-01-01")
    ),
    class = "llw_validation_error",
    regexp = "Unsupported LightLogR device"
  )
  expect_error(
    new_raw_import_request(
      device = "ActLumus",
      staged_files = fixture$staged,
      timezone = "UTC",
      not_before = as.Date("2001-01-01"),
      version = "not-a-format-version"
    ),
    class = "llw_validation_error",
    regexp = "Unsupported version"
  )
  expect_error(
    new_raw_import_request(
      device = "GENEActiv_GGIR",
      staged_files = fixture$staged,
      timezone = "UTC",
      not_before = as.Date("2001-01-01")
    ),
    class = "llw_unavailable_feature_error",
    regexp = "requires a directory import"
  )

  bad_extension <- m3_stage_fixture(original_name = "P01_actlumus.xlsx")
  on.exit(
    unlink(bad_extension$root, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  on.exit(unlink(bad_extension$source, force = TRUE), add = TRUE)
  expect_error(
    m3_raw_import_request(bad_extension$staged),
    class = "llw_validation_error",
    regexp = "Unsupported extension"
  )
})

test_that("ZIP preflight rejects unsafe and ambiguous archives", {
  expect_true(zip_entry_is_unsafe("../participant.txt"))
  expect_true(zip_entry_is_unsafe("folder/../../participant.txt"))
  expect_true(zip_entry_is_unsafe("C:/participant.txt"))
  expect_false(zip_entry_is_unsafe("folder/participant.txt"))
  expect_true(zip_entry_is_metadata("__MACOSX/._participant.txt"))

  skip_if(!nzchar(Sys.which("zip")), "a zip executable is unavailable")
  root <- tempfile("llw-multi-zip-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  files <- file.path(root, c("P01.txt", "P02.txt"))
  writeLines("first", files[[1L]])
  writeLines("second", files[[2L]])
  archive <- file.path(root, "two-files.zip")
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  suppressWarnings(utils::zip(archive, basename(files), flags = "-q"))

  expect_error(
    validate_raw_zip(
      archive,
      "two-files.zip",
      allowed_extensions = c("txt", "zip"),
      max_bytes = 1024^2
    ),
    class = "llw_validation_error",
    regexp = "contains 2 files"
  )
  expect_error(
    validate_raw_zip(
      archive,
      "two-files.zip",
      allowed_extensions = c("txt", "zip"),
      max_bytes = 1
    ),
    class = "llw_resource_error",
    regexp = "unsafe expanded size"
  )
})

test_that("the raw pipeline keeps LightLogR IDs and audits complete data", {
  fixture <- m3_stage_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(fixture$source, force = TRUE), add = TRUE)
  request <- m3_raw_import_request(fixture$staged)
  request$progress_path <- tempfile("raw-import-progress-", fileext = ".rds")
  on.exit(unlink(request$progress_path, force = TRUE), add = TRUE)
  before <- fixture$staged$sha256

  result <- suppressWarnings(raw_import_worker(request, spec = NULL))

  expect_s3_class(result, "llw_raw_import_result")
  expect_identical(unique(as.character(result$data$Id)), "P01_actlumus")
  expect_identical(unique(result$data$file.name), "P01_actlumus")
  expect_identical(result$quality$summary$rows, 3L)
  expect_identical(result$quality$summary$participants, 1L)
  expect_identical(result$quality$summary$implicit_gap_epochs, 0)
  expect_identical(result$structure$files$id_source, "filename_mapping")
  expect_match(paste(result$msg, collapse = " "), "P01_actlumus")
  expect_false(grepl("0001", paste(result$msg, collapse = " "), fixed = TRUE))
  expect_true(result$eligibility$eligible[
    result$eligibility$variable == "MEDI"
  ])
  expect_identical(result$phases$phase, names(raw_import_phase_labels()))
  progress <- read_raw_import_progress(request$progress_path)
  expect_identical(progress$phase, names(raw_import_phase_labels()))
  expect_true(all(progress$status == "complete"))
  expect_identical(sha256_file(fixture$staged$staged_path), before)
  expect_false(any(grepl(
    fixture$root,
    unlist(raw_import_manifest_arguments(request)),
    fixed = TRUE
  )))
})

test_that("normalization never overwrites an ID produced by LightLogR", {
  fixture <- m3_stage_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(fixture$source, force = TRUE), add = TRUE)
  request <- m3_raw_import_request(fixture$staged)
  staged_key <- raw_filename_stem(fixture$staged$original_name)
  imported <- data.frame(
    Id = factor(rep("embedded-42", 2L)),
    Datetime = as.POSIXct(
      c("2026-01-01 08:00:00", "2026-01-01 08:01:00"),
      tz = "UTC"
    ),
    MEDI = c(10, 20),
    file.name = staged_key,
    check.names = FALSE
  )
  structure_preview <- structure(
    list(
      files = data.frame(
        source_index = 1L,
        original_name = fixture$staged$original_name,
        id_source = "embedded",
        stringsAsFactors = FALSE
      )
    ),
    class = c("llw_raw_import_structure", "list")
  )

  embedded <- normalize_raw_import_data(
    imported,
    request,
    structure_preview = structure_preview
  )
  expect_identical(unique(as.character(embedded$Id)), "embedded-42")

  structure_preview$files$id_source <- "filename_mapping"
  mapped <- normalize_raw_import_data(
    imported,
    request,
    structure_preview = structure_preview
  )
  expect_identical(unique(as.character(mapped$Id)), "embedded-42")
})

test_that("multiple compatible files retain distinct source and participant labels", {
  root <- tempfile("llw-multi-source-")
  dir.create(root, recursive = TRUE)
  sources <- c(tempfile(fileext = ".txt"), tempfile(fileext = ".txt"))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(sources, force = TRUE), add = TRUE)
  m3_write_actlumus_fixture(sources[[1L]], medi = c(10, 20, 30))
  m3_write_actlumus_fixture(sources[[2L]], medi = c(40, 50, 60))
  staged <- stage_import_files(
    data.frame(
      name = c("P01_visit.txt", "P02_visit.txt"),
      datapath = sources,
      stringsAsFactors = FALSE
    ),
    root
  )

  result <- suppressWarnings(raw_import_worker(
    m3_raw_import_request(staged),
    spec = NULL
  ))

  expect_identical(nrow(result$data), 6L)
  expect_setequal(
    as.character(unique(result$data$Id)),
    c("P01_visit", "P02_visit")
  )
  expect_setequal(unique(result$data$file.name), c("P01_visit", "P02_visit"))
  expect_identical(
    result$structure$files$id_source,
    c("filename_mapping", "filename_mapping")
  )
})

test_that("multiple compatible files can form one participant record", {
  root <- tempfile("llw-multi-participant-")
  dir.create(root, recursive = TRUE)
  sources <- c(tempfile(fileext = ".txt"), tempfile(fileext = ".txt"))
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(sources, force = TRUE), add = TRUE)
  m3_write_actlumus_fixture(sources[[1L]], medi = c(10, 20, 30))
  m3_write_actlumus_fixture(
    sources[[2L]],
    datetimes = c(
      "01/01/2026 08:03:00",
      "01/01/2026 08:04:00",
      "01/01/2026 08:05:00"
    ),
    medi = c(40, 50, 60)
  )
  staged <- stage_import_files(
    data.frame(
      name = c("ID01_visit_1.txt", "ID01_visit_2.txt"),
      datapath = sources,
      stringsAsFactors = FALSE
    ),
    root
  )
  request <- new_raw_import_request(
    device = "ActLumus",
    staged_files = staged,
    timezone = "UTC",
    not_before = as.Date("2001-01-01"),
    version = "default",
    id_mode = "extract",
    extract_pattern = "^(ID[0-9]+)_"
  )

  result <- suppressWarnings(raw_import_worker(request, spec = NULL))

  expect_identical(nrow(result$data), 6L)
  expect_identical(unique(as.character(result$data$Id)), "ID01")
  expect_setequal(
    unique(result$data$file.name),
    c("ID01_visit_1", "ID01_visit_2")
  )
  expect_true(all(request$id_mapping$duplicate_proposed))
  expect_length(result$preflight$warnings, 0L)
})

test_that("LightLogR's bundled compressed ActLumus export stays importable offline", {
  source <- system.file(
    "extdata/205_actlumus_Log_1020_20230904101707532.txt.zip",
    package = "LightLogR"
  )
  expect_true(file.exists(source))
  root <- tempfile("llw-actlumus-zip-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  staged <- stage_import_files(
    data.frame(
      name = basename(source),
      datapath = source,
      stringsAsFactors = FALSE
    ),
    root
  )

  result <- suppressWarnings(raw_import_worker(
    new_raw_import_request(
      device = "ActLumus",
      staged_files = staged,
      timezone = "Europe/Berlin",
      not_before = as.Date("2001-01-01"),
      version = "default",
      id_mode = "automated"
    ),
    spec = NULL
  ))

  expect_s3_class(result, "llw_raw_import_result")
  expect_gt(nrow(result$data), 1000L)
  expect_identical(result$preflight$files$extension, "zip")
  expect_identical(
    unique(result$data$file.name),
    "205_actlumus_Log_1020_20230904101707532.txt"
  )
})

test_that("VEET ZIP exports are safely unpacked without changing provenance", {
  skip_if(!nzchar(Sys.which("zip")), "a zip executable is unavailable")
  fixture <- m3_stage_veet_fixture(compressed = TRUE)
  on.exit(
    unlink(fixture$source_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  on.exit(
    unlink(fixture$upload_root, recursive = TRUE, force = TRUE),
    add = TRUE
  )
  source_hash <- fixture$staged$sha256
  request <- new_raw_import_request(
    device = "VEET",
    staged_files = fixture$staged,
    timezone = "UTC",
    not_before = as.Date("2001-01-01"),
    version = "initial",
    id_mode = "automated",
    veet_modality = "ALS",
    max_bytes = 10 * 1024^2
  )

  result <- suppressWarnings(raw_import_worker(request, spec = NULL))

  expect_s3_class(result, "llw_raw_import_result")
  expect_identical(nrow(result$data), 3L)
  expect_identical(unique(as.character(result$data$Id)), "01_VEET_L.csv")
  expect_identical(unique(result$data$file.name), "01_VEET_L.csv")
  expect_equal(result$data$Lux, c(123, 145, 167))
  expect_identical(result$preflight$files$extension, "zip")
  expect_identical(
    result$preflight$files$expanded_bytes,
    as.numeric(file.info(fixture$source)$size)
  )
  expect_identical(sha256_file(fixture$staged$staged_path), source_hash)
  expect_identical(
    request$import_arguments$filename,
    fixture$staged$staged_path
  )
  expect_identical(
    raw_import_manifest_arguments(request)$filename,
    "01_VEET_L.csv.zip"
  )
})

test_that("a serializable raw request runs in the configured mirai worker", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  fixture <- m3_stage_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(fixture$source, force = TRUE), add = TRUE)
  request <- m3_raw_import_request(fixture$staged)
  startup_error <- NULL

  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 1),
      session = session
    )
    startup_error <<- runtime$startup_error
    session$userData$task <- new_long_task(
      worker = raw_import_worker,
      task_type = "raw_import",
      runtime = runtime
    )
  }

  shiny::testServer(server, {
    if (inherits(startup_error, "llw_error")) {
      skip("mirai daemons are unavailable in this test runtime")
    }
    task <- session$userData$task
    task$invoke(request)
    flush_m1_promises(session, times = 400L)

    expect_true(task$state() %in% c("complete", "warning"))
    expect_s3_class(task$result(), "llw_raw_import_result")
    expect_identical(
      unique(as.character(task$result()$data$Id)),
      "P01_actlumus"
    )
  })
})

test_that("malformed and identical-row files fail recoverably", {
  malformed_root <- tempfile("llw-malformed-")
  dir.create(malformed_root, recursive = TRUE)
  malformed_source <- tempfile(fileext = ".txt")
  writeLines("not an ActLumus export", malformed_source)
  malformed <- stage_import_files(
    data.frame(name = "P01_bad.txt", datapath = malformed_source),
    malformed_root
  )
  on.exit(unlink(malformed_root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(malformed_source, force = TRUE), add = TRUE)

  malformed_error <- tryCatch(
    suppressWarnings(raw_import_worker(m3_raw_import_request(malformed), NULL)),
    error = identity
  )
  expect_s3_class(malformed_error, "llw_import_error")
  expect_match(conditionMessage(malformed_error), "could not parse")
  expect_match(
    llw_public_message(malformed_error),
    "could not parse `P01_bad.txt`"
  )

  duplicate <- m3_stage_fixture(
    datetimes = c(
      "01/01/2026 08:00:00",
      "01/01/2026 08:00:00",
      "01/01/2026 08:01:00"
    ),
    medi = c(10, 10, 20)
  )
  on.exit(unlink(duplicate$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(duplicate$source, force = TRUE), add = TRUE)
  expect_error(
    suppressWarnings(raw_import_worker(
      m3_raw_import_request(duplicate$staged),
      NULL
    )),
    class = "llw_validation_error",
    regexp = "identical rows"
  )

  request <- m3_raw_import_request(
    duplicate$staged,
    options = "remove_duplicates"
  )
  result <- suppressWarnings(raw_import_worker(request, NULL))
  expect_identical(nrow(result$data), 2L)
})

test_that("scaled request-boundary fixtures stay deterministic and local", {
  source <- tempfile(fileext = ".txt")
  root <- tempfile("llw-scaled-limit-")
  dir.create(root, recursive = TRUE)
  on.exit(unlink(source, force = TRUE), add = TRUE)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  connection <- file(source, open = "wb")
  seek(connection, where = 1024^2 - 2L, origin = "start", rw = "write")
  writeBin(as.raw(0L), connection)
  close(connection)
  staged <- stage_import_files(
    data.frame(name = "near-limit.txt", datapath = source),
    root
  )
  mapping <- new_filename_id_mapping(staged$original_name, "automated")

  preflight <- preflight_raw_import_request(
    staged,
    device = "ActLumus",
    version = "default",
    timezone = "UTC",
    id_mapping = mapping,
    max_bytes = 1024^2
  )
  expect_lt(preflight$total_bytes, preflight$max_bytes)
  expect_gt(preflight$total_bytes, preflight$max_bytes * 0.99)

  local_preflight <- preflight_raw_import_request(
    staged,
    device = "ActLumus",
    version = "default",
    timezone = "UTC",
    id_mapping = mapping,
    max_bytes = 512 * 1024^2
  )
  expect_equal(local_preflight$max_bytes, 512 * 1024^2)

  expect_error(
    preflight_raw_import_request(
      staged,
      device = "ActLumus",
      version = "default",
      timezone = "UTC",
      id_mapping = mapping,
      max_bytes = 1024^2 - 2L
    ),
    class = "llw_resource_error",
    regexp = "exceeds"
  )
})
