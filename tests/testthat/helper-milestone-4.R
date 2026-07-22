m4_record <- function(
  name,
  timezone = "UTC",
  unit = "lux",
  device = "ActLumus",
  schema = c("canonical", "different"),
  instants = c(1767254400, 1767254460, 1767254400),
  participant = c("P01", "P01", "P02"),
  source_timezone = timezone,
  optional_as_factor = FALSE
) {
  schema <- match.arg(schema)
  datetime <- as.POSIXct(instants, origin = "1970-01-01", tz = timezone)
  optional <- c("inside", "outside", "inside")
  if (isTRUE(optional_as_factor)) optional <- factor(optional)
  if (identical(schema, "canonical")) {
    data <- data.frame(
      Id = participant,
      Datetime = datetime,
      MEDI = c(10, 20, 30),
      Context = optional,
      stringsAsFactors = FALSE
    )
    primary <- "MEDI"
  } else {
    data <- data.frame(
      Participant = participant,
      LocalTime = datetime,
      LightValue = c(40, 50, 60),
      Context = optional,
      Temperature = c(20, 21, 22),
      stringsAsFactors = FALSE
    )
    primary <- "LightValue"
  }
  variables <- list()
  variables[[primary]] <- list(
    label = paste(name, "measurement"),
    unit = unit,
    calibration = if (is.na(unit)) NA_character_ else "Fixture calibration"
  )
  new_dataset_record(
    raw_data = data,
    display_name = name,
    source_manifest = new_source_manifest(
      source_type = "m4_fixture",
      original_filenames = paste0(gsub(" ", "-", tolower(name)), ".csv"),
      hashes = paste0(
        "sha256:",
        strrep(substr(digest::digest(name), 1L, 1L), 64L)
      ),
      source_timezone = source_timezone,
      details = list(device = device, size_bytes = 1024)
    ),
    factual_metadata = list(device = device, variables = variables),
    analysis_settings = list(
      primary_variable = primary,
      analysis_timezone = timezone
    )
  )
}

m4_records <- function(...) {
  first <- m4_record("First source", ...)
  second <- m4_record(
    "Second source",
    timezone = "Europe/Berlin",
    schema = "different",
    device = "Other logger",
    ...
  )
  stats::setNames(list(first, second), c(first$id, second$id))
}

m4_separate_spec <- function(records, ...) {
  ids <- names(records)
  first <- new_append_source_mapping(
    dataset_id = ids[[1L]],
    participant_column = "Id",
    datetime_column = "Datetime",
    measurement_column = "MEDI",
    measurement_target = "first_MEDI",
    participant_prefix = "A",
    optional_columns = "Context",
    optional_targets = "first_context"
  )
  second <- new_append_source_mapping(
    dataset_id = ids[[2L]],
    participant_column = "Participant",
    datetime_column = "LocalTime",
    measurement_column = "LightValue",
    measurement_target = "second_light",
    participant_prefix = "B",
    optional_columns = c("Context", "Temperature"),
    optional_targets = c("second_context", "Temperature")
  )
  new_append_spec(
    mappings = list(first, second),
    display_name = "Appended fixture",
    participant_policy = "prefix_source",
    measurement_strategy = "separate",
    duplicate_policy = "keep_marked",
    overlap_policy = "not_applicable",
    ...
  )
}
