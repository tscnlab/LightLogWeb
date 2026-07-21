#!/usr/bin/env Rscript

# Development-only acceptance checks for the locally supplied `testdevices/`
# exports. The folder is intentionally excluded from package builds and may be
# absent on another checkout.

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Install devtools before running this acceptance script.")
}
if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Install digest before running this acceptance script.")
}

devtools::load_all(quiet = TRUE)

test_root <- normalizePath("testdevices/VEET", mustWork = FALSE)
if (!dir.exists(test_root)) {
  stop("The development-only testdevices/VEET folder is unavailable.")
}

cases <- data.frame(
  generation = c("initial", "initial", "current", "current"),
  container = c("plain", "zip", "plain", "zip"),
  filename = c(
    "01_VEET_L.csv",
    "01_VEET_L.csv.zip",
    "02_VEET_L.csv",
    "02_VEET_L.csv.zip"
  ),
  version = c("initial", "initial", "default", "default"),
  stringsAsFactors = FALSE
)

run_case <- function(filename, version) {
  source <- file.path(test_root, filename)
  if (!file.exists(source)) {
    stop("Missing development export: ", source)
  }
  upload_root <- tempfile("llw-testdevice-")
  dir.create(upload_root, recursive = TRUE)
  on.exit(unlink(upload_root, recursive = TRUE, force = TRUE), add = TRUE)

  staged <- stage_import_files(
    data.frame(
      name = filename,
      datapath = source,
      stringsAsFactors = FALSE
    ),
    upload_root
  )
  request <- new_raw_import_request(
    device = "VEET",
    staged_files = staged,
    timezone = "Europe/Berlin",
    not_before = as.Date("2001-01-01"),
    version = version,
    id_mode = "automated",
    veet_modality = "ALS",
    max_bytes = 1024^3
  )
  elapsed <- system.time(
    result <- suppressWarnings(raw_import_worker(request, spec = NULL))
  )[["elapsed"]]
  payload <- result$data |>
    dplyr::ungroup() |>
    dplyr::select(-Id, -file.name) |>
    as.data.frame()

  list(
    rows = nrow(result$data),
    participant_id = paste(
      unique(as.character(result$data$Id)),
      collapse = ","
    ),
    source_key = paste(unique(result$data$file.name), collapse = ","),
    expanded_mib = round(result$preflight$expanded_bytes / 1024^2, 1),
    elapsed_seconds = round(elapsed, 2),
    payload_sha256 = digest::digest(payload, algo = "sha256", serialize = TRUE)
  )
}

results <- lapply(seq_len(nrow(cases)), function(index) {
  c(
    as.list(cases[index, , drop = FALSE]),
    run_case(cases$filename[[index]], cases$version[[index]])
  )
})
report <- do.call(
  rbind,
  lapply(results, as.data.frame, stringsAsFactors = FALSE)
)

for (generation in unique(report$generation)) {
  pair <- report[report$generation == generation, , drop = FALSE]
  stopifnot(nrow(pair) == 2L)
  stopifnot(length(unique(pair$rows)) == 1L)
  stopifnot(length(unique(pair$payload_sha256)) == 1L)
}

print(
  report[, c(
    "generation",
    "container",
    "version",
    "rows",
    "participant_id",
    "source_key",
    "expanded_mib",
    "elapsed_seconds",
    "payload_sha256"
  )],
  row.names = FALSE
)
cat(
  "\nVEET ZIP/plain payload equivalence passed for both format generations.\n"
)
