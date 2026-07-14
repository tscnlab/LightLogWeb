# Build the small, attributed MeLiDos fixtures shipped with LightLogWeb.
#
# Run from the package root. By default this downloads through melidosData.
# During development, LLW_MELIDOS_CACHE may point at a directory containing
# SITE-light.rds, wearlog.rds, and sleepdiaries.rds as created during source QA.

sites <- c("TUM", "KNUST", "UCR", "RISE")
modalities <- c(
  TUM = "light_glasses_1minute",
  KNUST = "light_chest_1minute",
  UCR = "light_wrist_1minute",
  RISE = "light_glasses_1minute"
)
repositories <- c(
  TUM = "HildenEtAl_Dataset_2025",
  KNUST = "AkuffoEtAl_Dataset_2025",
  UCR = "Sancho-SalasEtAl_Dataset_2025",
  RISE = "NilssonTengelinEtAl_Dataset_2026"
)
commits <- c(
  TUM = "eeafeea65ee0d2e0f3a5ab9f17b0f22b29e63e57",
  KNUST = "ea49a38edfb1f6397302f37776a4496ed0ca2dd5",
  UCR = "ed90d9ac91b9fa81c0869dafbe6efb3c8d9faed3",
  RISE = "12ca7139b78949ab84e0be92822d03910c1e47c8"
)
versions <- c(TUM = "1.0.1", KNUST = "1.0.1", UCR = "1.0.0", RISE = "1.0.1")
dois <- c(
  TUM = "10.5281/zenodo.16893901",
  KNUST = "10.5281/zenodo.15576731",
  UCR = "10.5281/zenodo.17289456",
  RISE = "10.5281/zenodo.18925834"
)
timezones <- c(TUM = "Europe/Berlin", KNUST = "Africa/Accra", UCR = "America/Costa_Rica", RISE = "Europe/Stockholm")
coordinates <- list(TUM = c(48.1333, 11.5667), KNUST = c(6.675007, -1.572644), UCR = c(9.9372, -84.0509), RISE = c(57.71567, 12.89087))

cache <- Sys.getenv("LLW_MELIDOS_CACHE", unset = "")
read_light <- function(site) {
  if (nzchar(cache)) readRDS(file.path(cache, paste0(site, "-light.rds"))) else melidosData::load_data(modalities[[site]], site = site)
}
read_auxiliary <- function(modality) {
  if (nzchar(cache)) readRDS(file.path(cache, paste0(modality, ".rds"))) else melidosData::load_data(modality, site = sites)
}

wear <- read_auxiliary("wearlog")
sleep <- read_auxiliary("sleepdiaries")
output <- file.path("inst", "extdata", "melidos")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
records <- vector("list", length(sites))

for (i in seq_along(sites)) {
  site <- sites[[i]]
  original <- read_light(site)
  source_id <- as.character(original$Id[[1]])
  first_day <- as.Date(original$Datetime[as.character(original$Id) == source_id][[1]], tz = timezones[[site]])
  start <- as.POSIXct(as.character(first_day), tz = timezones[[site]])
  end <- start + lubridate::days(2)
  light <- original[as.character(original$Id) == source_id & original$Datetime >= start & original$Datetime < end, c("Id", "Datetime", "MEDI", "LIGHT", "position", "is.implicit")]
  light$Id <- paste0(site, "_fixture_01")
  light <- tibble::as_tibble(light)

  wear_site <- wear[[site]]
  wear_rows <- wear_site[as.character(wear_site$Id) == source_id & wear_site$end > start & wear_site$start < end, c("Id", "start", "end", "state")]
  if (nrow(wear_rows)) names(wear_rows)[names(wear_rows) == "state"] <- "State"
  sleep_site <- sleep[[site]]
  sleep_rows <- sleep_site[as.character(sleep_site$Id) == source_id & sleep_site$wake > start & sleep_site$sleep < end, c("Id", "sleep", "wake")]
  if (nrow(sleep_rows)) {
    names(sleep_rows)[names(sleep_rows) == "sleep"] <- "start"
    names(sleep_rows)[names(sleep_rows) == "wake"] <- "end"
    sleep_rows$State <- "sleep_diary"
  }
  intervals <- dplyr::bind_rows(
    if (nrow(wear_rows)) dplyr::mutate(wear_rows, source = "wearlog") else NULL,
    if (nrow(sleep_rows)) dplyr::mutate(sleep_rows, source = "sleepdiary") else NULL
  )
  if (nrow(intervals)) intervals$Id <- paste0(site, "_fixture_01")
  intervals <- tibble::as_tibble(intervals)

  light_path <- file.path(output, paste0(tolower(site), "-light.rds"))
  interval_path <- file.path(output, paste0(tolower(site), "-intervals.rds"))
  saveRDS(light, light_path, version = 3)
  saveRDS(intervals, interval_path, version = 3)
  records[[i]] <- data.frame(
    site = site,
    modality = modalities[[site]],
    position = unique(light$position)[[1]],
    timezone = timezones[[site]],
    latitude = coordinates[[site]][[1]],
    longitude = coordinates[[site]][[2]],
    source_repository = paste0("https://github.com/MeLiDosProject/", repositories[[site]]),
    source_version = versions[[site]],
    source_commit = commits[[site]],
    doi = dois[[site]],
    license = "CC-BY-4.0",
    melidosData_version = as.character(utils::packageVersion("melidosData")),
    rows_light = nrow(light),
    rows_intervals = nrow(intervals),
    light_file = basename(light_path),
    light_md5 = unname(tools::md5sum(light_path)),
    intervals_file = basename(interval_path),
    intervals_md5 = unname(tools::md5sum(interval_path)),
    transformation = "First two complete local dates of one already anonymous participant; ID replaced by a fixture identifier; selected light columns only.",
    stringsAsFactors = FALSE
  )
}

manifest <- dplyr::bind_rows(records)
utils::write.csv(manifest, file.path(output, "manifest.csv"), row.names = FALSE)
jsonlite::write_json(manifest, file.path(output, "manifest.json"), pretty = TRUE, dataframe = "rows", auto_unbox = TRUE)
