if (!requireNamespace("melidosData", quietly = TRUE)) {
  stop(
    "melidosData is required to regenerate the Milestone 3 fixture.",
    call. = FALSE
  )
}
if (utils::packageVersion("melidosData") != "1.0.6") {
  stop(
    "The pinned fixture generator requires melidosData 1.0.6.",
    call. = FALSE
  )
}

output <- file.path(
  "dev",
  "fixtures",
  "melidos-iztech-light-glasses-1minute.rds"
)
refresh <- identical(Sys.getenv("LIGHTLOGWEB_REFRESH_M3_FIXTURE"), "1")
if (file.exists(output) && !refresh) {
  stop(
    paste0(
      "Fixture already exists at `",
      output,
      "`. Set LIGHTLOGWEB_REFRESH_M3_FIXTURE=1 to replace it deliberately."
    ),
    call. = FALSE
  )
}

dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
dataset <- melidosData::load_data(
  modality = "light_glasses_1minute",
  site = "IZTECH"
)
if (!is.data.frame(dataset) || nrow(dataset) == 0L) {
  stop("melidosData returned no IZTECH observations.", call. = FALSE)
}
required <- c("Id", "Datetime")
missing <- setdiff(required, names(dataset))
if (length(missing) > 0L) {
  stop(
    paste0(
      "The downloaded dataset is missing column(s): ",
      paste(missing, collapse = ", "),
      "."
    ),
    call. = FALSE
  )
}
expected_names <- c(
  "Id",
  "Datetime",
  "EVENT",
  "TEMPERATURE",
  "ORIENTATION",
  "PIM",
  "PIMn",
  "TAT",
  "TATn",
  "ZCM",
  "ZCMn",
  "LIGHT",
  "IR.LIGHT",
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
  "MEDI",
  "CLEAR",
  "STATE",
  "MS",
  "EXT.TEMPERATURE",
  "AMB.LIGHT",
  "RED.LIGHT",
  "GREEN.LIGHT",
  "BLUE.LIGHT",
  "UVA.LIGHT",
  "UVB.LIGHT",
  "file.name",
  "position",
  "is.implicit"
)
if (
  !identical(dim(dataset), c(151200L, 37L)) ||
    !identical(names(dataset), expected_names) ||
    !identical(lubridate::tz(dataset$Datetime), "Europe/Istanbul") ||
    length(unique(dataset$Id)) != 17L
) {
  stop(
    paste(
      "The upstream IZTECH dataset no longer matches the pinned Milestone 3",
      "contract. Review the source change and update fixture provenance before",
      "refreshing the snapshot."
    ),
    call. = FALSE
  )
}

saveRDS(dataset, output, version = 3, compress = "xz")
cat(
  output,
  nrow(dataset),
  ncol(dataset),
  digest::digest(file = output, algo = "sha256", serialize = FALSE),
  sep = "\t"
)
cat("\n")
