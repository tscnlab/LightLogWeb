# Offline Slice 4 showcase. All additional time/scale values are synthetic R
# fixtures. This launcher never opens a published package or reads live data.
pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
source("tests/testthat/helper-milestone-8.R", local = TRUE)

m8_iztech_record <- function() {
  original <- load_dataset_example("iztech", getwd())
  raw <- unserialize(original$raw_payload)
  participants <- utils::head(sort(unique(as.character(raw$Id))), 4L)
  subset <- raw[as.character(raw$Id) %in% participants, , drop = FALSE]
  manifest <- original$source_manifest
  manifest$details$showcase_subset <- list(
    participant_ids = participants,
    selection = "All observations for the first four sorted participant IDs."
  )
  manifest$details$purpose <- "Milestone 8 offline showcase default"
  new_dataset_record(
    raw_data = subset,
    display_name = "IZTECH light glasses · 4 participants",
    source_manifest = manifest,
    factual_metadata = original$factual_metadata,
    analysis_settings = original$analysis_settings,
    id = "dataset_m8iztechfour"
  )
}

m8_scale_record <- function(n = 250000L) {
  per_id <- as.integer(n / 20L)
  data <- data.frame(
    Id = factor(rep(sprintf("P%02d", 1:20), each = per_id)),
    Datetime = rep(
      as.POSIXct("2026-01-01", tz = "UTC") + seq_len(per_id) * 10,
      20L
    ),
    MEDI = rep(seq_len(100L), length.out = per_id * 20L),
    file_group_id = factor(rep(sprintf("synthetic:%02d", 1:20), each = per_id))
  )
  new_dataset_record(
    data,
    paste("Synthetic scale:", n, "rows"),
    new_source_manifest("synthetic", source_timezone = "UTC"),
    analysis_settings = list(primary_variable = "MEDI"),
    id = paste0("dataset_m8scale", n)
  )
}

m8_dst_record <- function(season = c("spring", "autumn")) {
  season <- match.arg(season)
  day <- if (season == "spring") "2026-03-29" else "2026-10-25"
  transition <- as.POSIXct(paste(day, "01:00:00"), tz = "UTC")
  time <- lubridate::with_tz(
    transition + seq(-18 * 3600, 30 * 3600, by = 600),
    "Europe/Berlin"
  )
  hours <- as.numeric(difftime(time, transition, units = "hours"))
  # Deliberately synthetic peaks make a one-hour timestamp shift visible.
  data <- data.frame(
    Id = factor(rep("DST_demo", length(time))),
    Datetime = time,
    MEDI = 0.2 + 800 * exp(-((hours - 2) / 0.7)^2) +
      80 * exp(-((hours - 8) / 1.5)^2),
    file_group_id = paste0("dst_demo_", season)
  )
  description <- if (season == "spring")
    "on 29 March 2026 the local clock jumps from 01:59 to 03:00." else
    "on 25 October 2026 the hour from 02:00 to 02:59 occurs twice."
  manifest <- new_source_manifest("synthetic", source_timezone = "Europe/Berlin")
  manifest$details$showcase_note <- paste(
    "Synthetic DST example:", description,
    "Try Time & sampling → Device DST, save the step, and compare", paste0(day, "."),
    "This illustrates the correction for a device that kept its starting offset; the clock change alone does not establish a device error."
  )
  new_dataset_record(
    data,
    if (season == "spring")
      "DST example · spring clock jump · Europe/Berlin" else
      "DST example · autumn repeated hour · Europe/Berlin",
    manifest,
    analysis_settings = list(primary_variable = "MEDI"),
    id = paste0("dataset_m8dst", season)
  )
}

m8_showcase_records <- function() {
  records <- metadata_explorer_fixture_records(
    "tests/testthat/fixtures/milestone-7"
  )
  synthetic <- m8_record(edges = TRUE)
  synthetic$display_name <- "Synthetic multi-day / spring DST / stream identities"
  fall <- m8_record()
  fall_data <- unserialize(fall$raw_payload)
  fall_data$Datetime <- lubridate::with_tz(
    as.POSIXct("2026-10-24", tz = "UTC") +
      rep(seq(0, by = 1800, length.out = 144), 3),
    "Europe/Berlin"
  )
  fall_data <- fall_data[-c(30:36, 200:210), ]
  fall <- new_dataset_record(
    fall_data,
    "Synthetic autumn DST and gaps",
    new_source_manifest("synthetic", source_timezone = "Europe/Berlin"),
    analysis_settings = list(primary_variable = "MEDI"),
    id = "dataset_m8falltime"
  )
  slow <- m8_record()
  slow$id <- "dataset_m8slow"
  slow$display_name <- "Synthetic slow preview: cancellation and stale-result checks"
  slow$provenance$demo_delay <- TRUE
  recovery <- m8_record()
  recovery$id <- "dataset_m8recovery"
  recovery$display_name <- "Synthetic recovery: first Preview fails, retry succeeds"
  recovery$provenance$demo_failure_once <- TRUE
  empty <- new_dataset_record(
    unserialize(synthetic$raw_payload)[FALSE, ],
    "Synthetic truly empty source",
    new_source_manifest("synthetic", source_timezone = "UTC"),
    id = "dataset_m8empty"
  )
  c(
    list(m8_iztech_record(), m8_dst_record("spring"), m8_dst_record("autumn"), synthetic, fall),
    records,
    list(slow, recovery, empty, m8_scale_record())
  )
}

m8_showcase_worker <- function(payload, spec) {
  if (
    payload$action == "preview" && isTRUE(payload$record$provenance$demo_delay)
  )
    Sys.sleep(3)
  if (
    payload$action == "preview" &&
      isTRUE(payload$record$provenance$demo_failure_once)
  ) {
    # Each session has its own daemon and its own temporary directory.
    marker <- file.path(tempdir(), "llw-m8-first-preview-failed")
    if (!file.exists(marker)) {
      writeLines("Synthetic failure already exercised", marker)
      LightLogWeb:::abort_llw(
        "Synthetic recovery check: this first Preview failed. Choose Preview recipe again to retry. Applied data are unchanged.",
        "preparation"
      )
    }
  }
  LightLogWeb:::preparation_task_response(payload, spec)
}
environment(m8_showcase_worker) <- baseenv()

milestone_8_prepare_app <- function(profile = "local", workers = 1L) {
  prepare_app(
    m8_showcase_records(),
    profile = profile,
    workers = workers,
    worker = m8_showcase_worker
  )
}

# Run with: source("dev/milestone-8-prepare-app.R")
# shiny::runApp(milestone_8_prepare_app(), host = "127.0.0.1", port = 7391)
