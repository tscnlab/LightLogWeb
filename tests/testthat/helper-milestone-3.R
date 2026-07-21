m3_actlumus_header <- function() {
  c(
    "#ActLogModel=2.0.0",
    "+-------------+ Condor Instruments Report +-------------+",
    "SOFTWARE_VERSION : 2.1.0",
    "LOG_FILE_VERSION : 1.0.3",
    "DEVICE_MODEL : AL0101",
    "+-------------------------------------------------------+",
    paste(
      c(
        "DATE/TIME",
        "MS",
        "EVENT",
        "TEMPERATURE",
        "EXT TEMPERATURE",
        "ORIENTATION",
        "PIM",
        "PIMn",
        "TAT",
        "TATn",
        "ZCM",
        "ZCMn",
        "LIGHT",
        "AMB LIGHT",
        "RED LIGHT",
        "GREEN LIGHT",
        "BLUE LIGHT",
        "IR LIGHT",
        "UVA LIGHT",
        "UVB LIGHT",
        "STATE",
        "CAP_SENS_1",
        "CAP_SENS_2",
        "F1",
        "F2",
        "F3",
        "F4",
        "F5",
        "F6",
        "F7",
        "F8",
        "MELANOPIC EDI",
        "CLEAR"
      ),
      collapse = ";"
    )
  )
}

m3_actlumus_row <- function(datetime, medi) {
  values <- c(
    datetime,
    "0",
    "0",
    "22.5",
    "0",
    "1",
    "10",
    "1",
    "0",
    "0",
    "1",
    "0.1",
    "10",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "4",
    "100",
    "100",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    "0",
    as.character(medi),
    "0"
  )
  paste(values, collapse = ";")
}

m3_write_actlumus_fixture <- function(
  path,
  datetimes = c(
    "01/01/2026 08:00:00",
    "01/01/2026 08:01:00",
    "01/01/2026 08:02:00"
  ),
  medi = c(10, 20, 30)
) {
  stopifnot(length(datetimes) == length(medi))
  lines <- c(
    m3_actlumus_header(),
    vapply(
      seq_along(datetimes),
      function(index) m3_actlumus_row(datetimes[[index]], medi[[index]]),
      character(1)
    )
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

m3_stage_fixture <- function(
  original_name = "P01_actlumus.txt",
  datetimes = c(
    "01/01/2026 08:00:00",
    "01/01/2026 08:01:00",
    "01/01/2026 08:02:00"
  ),
  medi = c(10, 20, 30)
) {
  root <- tempfile("llw-m3-upload-")
  dir.create(root, recursive = TRUE)
  source <- tempfile(fileext = ".txt")
  m3_write_actlumus_fixture(source, datetimes, medi)
  staged <- stage_import_files(
    data.frame(
      name = original_name,
      datapath = source,
      stringsAsFactors = FALSE
    ),
    root
  )
  list(root = root, source = source, staged = staged)
}

m3_raw_import_request <- function(staged, timezone = "UTC", ...) {
  new_raw_import_request(
    device = "ActLumus",
    staged_files = staged,
    timezone = timezone,
    not_before = as.Date("2001-01-01"),
    version = "default",
    id_mode = "automated",
    ...
  )
}

m3_write_veet_fixture <- function(path) {
  writeLines(
    c(
      "1704067200,ALS,100,1,1,1,0,100,10,0,123",
      "1704067260,ALS,100,1,1,1,0,120,11,0,145",
      "1704067320,ALS,100,1,1,1,0,140,12,0,167"
    ),
    path,
    useBytes = TRUE
  )
  invisible(path)
}

m3_stage_veet_fixture <- function(compressed = FALSE) {
  source_root <- tempfile("llw-m3-veet-source-")
  upload_root <- tempfile("llw-m3-veet-upload-")
  dir.create(source_root, recursive = TRUE)
  dir.create(upload_root, recursive = TRUE)
  source <- file.path(source_root, "01_VEET_L.csv")
  m3_write_veet_fixture(source)
  upload <- source
  if (isTRUE(compressed)) {
    upload <- file.path(source_root, "01_VEET_L.csv.zip")
    old_working_directory <- setwd(source_root)
    on.exit(setwd(old_working_directory), add = TRUE)
    suppressWarnings(utils::zip(
      zipfile = upload,
      files = basename(source),
      flags = "-q"
    ))
  }
  staged <- stage_import_files(
    data.frame(
      name = basename(upload),
      datapath = upload,
      stringsAsFactors = FALSE
    ),
    upload_root
  )
  list(
    source_root = source_root,
    upload_root = upload_root,
    source = source,
    upload = upload,
    staged = staged
  )
}

m3_quality_fixture <- function() {
  data.frame(
    Id = factor(rep("P01", 6L)),
    Datetime = as.POSIXct(
      c(
        "2026-01-01 00:00:00",
        "2026-01-01 00:01:00",
        "2026-01-01 00:02:00",
        "2026-01-01 00:02:35",
        "2026-01-01 00:04:00",
        "2026-01-01 00:04:00"
      ),
      tz = "UTC"
    ),
    MEDI = c(1, NA, 3, 4, 5, 6),
    file.name = "P01_actlumus",
    check.names = FALSE
  )
}
