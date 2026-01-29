datasets <- shiny::reactiveValues()

datasets[["dataset_one"]] <- list(
  data = data.frame(a = 1, b = 2),
  metadata = list(
    variable = "A",
    variable_name = "Alpha",
    variable_unit = "units",
    tz = "UTC",
    device = "DeviceX"
  ),
  import_specs = list(
    file_names = c("file1.csv", "file2.csv"),
    options = "dst_jumps",
    version = "default",
    not_before = as.Date("2020-01-01"),
    id_strategy = "manual",
    id_value = "P1"
  ),
  import_call = shinymeta::metaExpr(
    rlang::expr(LightLogR::import_Dataset(device = "DeviceX"))
  )
)

datasets[["dataset_two"]] <- list(
  data = data.frame(a = 1),
  metadata = list(
    variable = "B",
    variable_name = "Beta",
    variable_unit = "units",
    tz = "UTC",
    device = "DeviceY"
  ),
  import_specs = list(
    file_names = "file3.csv",
    options = character(),
    version = "default",
    not_before = as.Date("2021-01-01"),
    id_strategy = "automated",
    id_value = NA_character_
  ),
  import_call = shinymeta::metaExpr(
    rlang::expr(LightLogR::import_Dataset(device = "DeviceY"))
  )
)

report_data <- build_report_data(datasets)
stopifnot(report_data$dataset_count == 2)
stopifnot(identical(report_data$dataset_names, c("dataset_one", "dataset_two")))

report_qmd <- build_report_qmd(report_data)
stopifnot(grepl("Multiple datasets imported: Yes", report_qmd))
stopifnot(grepl("dataset_one", report_qmd))
stopifnot(grepl("dataset_two", report_qmd))
