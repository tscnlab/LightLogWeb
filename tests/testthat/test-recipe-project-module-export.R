test_that("recipe execution and generated code are equivalent", {
  dataset <- llw_dataset(sample_participant())
  recipe <- llw_recipe() |>
    llw_recipe_add("preparation", list(gap_policy = "leave", interval = "5 min"), id = "prepare") |>
    llw_recipe_add("grouping", list(dimensions = "participant"), id = "group") |>
    llw_recipe_add("metric", list(metrics = list(list(id = "dose"))), id = "metric") |>
    llw_recipe_add("visualization", list(type = "timeline", id = "Participant"), id = "plot")
  result <- llw_run(dataset, recipe)
  script <- llw_build_script(recipe, data_expression = "sample_participant()", include_session_info = FALSE)
  environment <- new.env(parent = environment())
  eval(parse(text = paste(script, collapse = "\n")), envir = environment)

  expect_equal(environment$analysis$dataset$prepared_data, result$dataset$prepared_data)
  expect_equal(environment$analysis$metrics$metric$value, result$metrics$metric$value)
  expect_equal(environment$analysis$plots$plot$data, result$plots$plot$data)
  expect_silent(parse(text = paste(script, collapse = "\n")))
})

test_that("recipe editing supports validation, undo, redo, and ordering", {
  recipe <- llw_recipe() |>
    llw_recipe_add("grouping", list(dimensions = "participant"), id = "one") |>
    llw_recipe_add("metric", list(metrics = list(list(id = "dose"))), id = "two")
  moved <- llw_recipe_move(recipe, "two", "up")
  disabled <- llw_recipe_update(moved, "two", enabled = FALSE)
  removed <- llw_recipe_remove(disabled, "one")

  expect_equal(vapply(moved$steps, `[[`, character(1), "id"), c("two", "one"))
  expect_false(disabled$steps[[1]]$enabled)
  expect_length(removed$steps, 1)
  expect_length(llw_recipe_undo(removed)$steps, 2)
  expect_length(llw_recipe_redo(llw_recipe_undo(removed))$steps, 1)
  expect_error(llw_recipe_add(llw_recipe(), "metric", list(unknown = 1)), "Unknown parameters")
  expect_equal(llw_recipe_schema("metric")$required, "metrics")
})

test_that("custom modules validate inputs and return typed reproducible results", {
  dataset <- llw_dataset(sample_participant())
  module <- example_module()
  result <- llw_run_module(dataset, module, list(offset = 2))
  expect_s3_class(result, "llw_result")
  expect_equal(result$value, mean(dataset$prepared_data$MEDI) + 2)
  expect_equal(result$provenance$module_version, "1.0.0")

  missing <- example_module(required_columns = c("Id", "Missing"))
  expect_error(llw_run_module(dataset, missing), "requires columns")
  invalid <- module
  invalid$run <- function(data, metadata, params) 1
  expect_error(llw_run_module(dataset, invalid, list(offset = 0)), "must return an `llw_result`")
  malformed_code <- module
  malformed_code$code <- function(params) "("
  expect_error(llw_run_module(dataset, malformed_code, list(offset = 0)))
  expect_error(LightLogWeb(modules = list(module, module)), "IDs must be unique")
})

test_that("missing and incompatible modules are recoverable", {
  dataset <- llw_dataset(sample_participant())
  module <- example_module()
  recipe <- llw_recipe() |>
    llw_recipe_add("module", list(offset = 1), id = "module-step", module_id = module$id, module_version = module$version)
  complete <- llw_run(dataset, recipe, list(module))
  missing <- llw_run(dataset, recipe, list())
  incompatible <- llw_run(dataset, recipe, list(example_module("2.0.0")))
  expect_length(complete$modules, 1)
  expect_match(missing$messages, "unavailable")
  expect_match(incompatible$messages, "does not match")
})

test_that("project bundles round-trip with and without participant data", {
  dataset <- llw_dataset(sample_participant(), provenance = list(files = list(md5 = "known")))
  recipe <- llw_recipe() |> llw_recipe_add("metric", list(metrics = list(list(id = "dose"))), id = "metric")
  analysis <- llw_run(dataset, recipe)
  private_path <- tempfile(fileext = ".llw")
  complete_path <- tempfile(fileext = ".llw")
  llw_save_project(analysis, path = private_path)
  llw_save_project(analysis, path = complete_path, include_data = TRUE)
  private <- llw_load_project(private_path)
  complete <- llw_load_project(complete_path)

  expect_null(private$dataset)
  expect_s3_class(complete$dataset, "llw_dataset")
  expect_equal(complete$dataset$raw_data, dataset$raw_data)
  expect_false(private$manifest$includes_raw_data)
  expect_true(complete$manifest$includes_raw_data)
  expect_s3_class(llw_relink_project(private, dataset), "llw_project")
  mismatch <- dataset
  mismatch$provenance$files$md5 <- "different"
  expect_error(llw_relink_project(private, mismatch), "do not match")
})

test_that("projects disable unavailable module steps", {
  module <- example_module()
  dataset <- llw_dataset(sample_participant())
  recipe <- llw_recipe() |> llw_recipe_add("module", list(offset = 0), module_id = module$id, module_version = module$version)
  path <- tempfile(fileext = ".llw")
  llw_save_project(dataset, recipe, path)
  loaded <- llw_load_project(path, modules = list())
  expect_false(loaded$recipe$steps[[1]]$enabled)
  expect_match(loaded$warnings, "was disabled")
})

test_that("all export formats reopen successfully", {
  dataset <- llw_dataset(sample_participant(), metadata = list(coordinates = c(48.1, 11.6)))
  plot <- llw_plot(dataset, "timeline", id = "Participant")
  metric <- llw_metrics(dataset, "dose")
  csv <- tempfile(fileext = ".csv")
  rds <- tempfile(fileext = ".rds")
  json <- tempfile(fileext = ".json")
  png <- tempfile(fileext = ".png")
  svg <- tempfile(fileext = ".svg")
  pdf <- tempfile(fileext = ".pdf")

  llw_export(metric, csv)
  llw_export(metric, rds)
  llw_export(llw_run(dataset), json)
  llw_export(plot, png)
  llw_export(plot, svg)
  llw_export(plot, pdf)
  expect_gt(nrow(utils::read.csv(csv)), 0)
  expect_s3_class(readRDS(rds), "llw_result")
  expect_equal(jsonlite::read_json(json)$format, "LightLogWeb manifest")
  expect_identical(readBin(png, "raw", 8), as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  expect_match(readLines(svg, n = 2), "<svg", all = FALSE)
  expect_equal(readChar(pdf, 4), "%PDF")
})

test_that("scripts, manifests, and errors remain reviewable", {
  recipe <- llw_recipe() |> llw_recipe_add("metric", list(metrics = list(list(id = "dose"))), id = "dose")
  manifest <- llw_manifest(llw_dataset(make_regular_light()), recipe)
  manifest$generated_at <- "<generated-at>"
  manifest$session_info <- "<session-info>"
  expect_snapshot(cat(llw_build_script(recipe, data_expression = "data.frame()", include_session_info = FALSE), sep = "\n"))
  expect_snapshot(str(manifest, max.level = 2, give.attr = FALSE))
  expect_snapshot(error = TRUE, llw_recipe_add(llw_recipe(), "metric", list(not_a_parameter = 1)))
})
