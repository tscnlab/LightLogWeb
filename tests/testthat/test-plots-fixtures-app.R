test_that("every visualization returns a typed plot and plotted data", {
  dataset <- llw_dataset(
    sample_participant(),
    metadata = list(variable = "MEDI", variable_unit = "lx", timezone = "Europe/Berlin", coordinates = c(48.1, 11.6))
  )
  intervals <- data.frame(
    Id = "Participant",
    start = min(dataset$prepared_data$Datetime),
    end = min(dataset$prepared_data$Datetime) + 3600,
    State = "sleep"
  )
  dataset <- llw_annotate(dataset, intervals)
  types <- LightLogWeb:::llw_plot_types()$id
  for (type in types) {
    args <- list(x = dataset, type = type, id = "Participant")
    if (type == "state") args$state <- "State"
    result <- rlang::exec(llw_plot, !!!args)
    expect_s3_class(result, "llw_result")
    expect_s3_class(result$value, "ggplot")
    expect_true(is.data.frame(result$data), info = type)
    expect_silent(ggplot2::ggplot_build(result$value))
  }
})

test_that("vdiffr covers every visualization type", {
  skip_if_not_installed("vdiffr")
  dataset <- llw_dataset(llw_fixture("TUM"), metadata = list(variable = "MEDI", variable_unit = "lx", timezone = "Europe/Berlin", coordinates = c(48.1333, 11.5667)))
  dataset <- llw_annotate(dataset, llw_fixture("TUM", intervals = TRUE), output_col = "State", overwrite = TRUE)
  for (type in LightLogWeb:::llw_plot_types()$id) {
    args <- list(x = dataset, type = type)
    if (type == "state") args$state <- "State"
    vdiffr::expect_doppelganger(paste("LightLogWeb", type), rlang::exec(llw_plot, !!!args)$value)
  }
})

test_that("MeLiDos fixtures are minimal, varied, and checksum verified", {
  manifest_path <- system.file("extdata", "melidos", "manifest.csv", package = "LightLogWeb")
  if (!nzchar(manifest_path)) manifest_path <- file.path("inst", "extdata", "melidos", "manifest.csv")
  manifest <- utils::read.csv(manifest_path)
  expect_equal(manifest$site, c("TUM", "KNUST", "UCR", "RISE"))
  expect_setequal(manifest$position, c("glasses", "chest", "wrist"))
  expect_true(all(manifest$license == "CC-BY-4.0"))
  for (site in manifest$site) {
    data <- llw_fixture(site)
    intervals <- llw_fixture(site, intervals = TRUE)
    row <- manifest[manifest$site == site, ]
    expect_equal(nrow(data), row$rows_light)
    expect_equal(nrow(intervals), row$rows_intervals)
    expect_setequal(names(data), c("Id", "Datetime", "MEDI", "LIGHT", "position", "is.implicit"))
    expect_equal(unname(tools::md5sum(system.file("extdata", "melidos", row$light_file, package = "LightLogWeb"))), row$light_md5)
  }
})

test_that("application and startup module wiring initialize without hidden calculations", {
  called <- FALSE
  module <- example_module()
  module$server <- function(id, context) {
    called <<- TRUE
    expect_true(all(c("raw", "prepared", "grouped", "metadata", "recipe", "selection", "commit", "publish") %in% names(context)))
  }
  app <- LightLogWeb(modules = list(module))
  expect_s3_class(app, "shiny.appobj")
  suppressWarnings(shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    expect_true(called)
    expect_true(any(grepl("LightLog", as.character(output$llw_root))))
  }))
})
