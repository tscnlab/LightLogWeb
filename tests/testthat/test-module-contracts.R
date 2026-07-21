test_that("dataset manager returns explicit stable-ID events", {
  first <- m1_record("First")
  second <- m1_record("Second")
  records <- stats::setNames(list(first, second), c(first$id, second$id))
  datasets <- shiny::reactiveVal(records)
  selected <- shiny::reactiveVal(first$id)

  shiny::testServer(
    datasetManagerServer,
    args = list(datasets = datasets, selected_dataset_id = selected),
    {
      returned <- session$getReturned()
      session$setInputs(
        import_newdata = 0,
        import_testdata = 0,
        dataset_select = first$id,
        rename_dataset_real = 0,
        delete_dataset_real = 0
      )
      session$setInputs(import_newdata = 1)
      expect_identical(returned$event()$type, "open_import")

      session$setInputs(dataset_select = second$id)
      expect_identical(returned$event()$type, "select")
      expect_identical(returned$event()$dataset_id, second$id)

      session$setInputs(rename_name = "Renamed", rename_dataset_real = 1)
      expect_identical(returned$event()$type, "rename")
      expect_identical(returned$event()$dataset_id, first$id)
      expect_identical(returned$event()$value, "Renamed")

      session$setInputs(delete_dataset_real = 1)
      expect_identical(returned$event()$type, "remove")
      expect_identical(returned$event()$dataset_id, first$id)
    }
  )
})

test_that("dashboard communicates its empty-state action through an event", {
  dataset <- shiny::reactiveVal(NULL)
  panel <- shiny::reactiveVal("dashboard")

  shiny::testServer(
    datasetDashboardServer,
    args = list(dataset = dataset, active_panel = panel),
    {
      returned <- session$getReturned()
      session$setInputs(to_import = 0)
      session$setInputs(to_import = 1)
      expect_identical(returned$event()$type, "open_import")

      dataset(m1_record())
      session$flushReact()
      expect_identical(output$raw_rows, "3")
      expect_identical(output$prepared_rows, "3")
    }
  )
})

test_that("import validation failures are returned as recoverable task state", {
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    session = NULL
  )
  on.exit(runtime$cleanup(), add = TRUE)

  shiny::testServer(
    importServer,
    args = list(runtime = runtime, color_mode = shiny::reactive("light")),
    {
      returned <- session$getReturned()
      expect_type(returned$open_import, "closure")
      session$setInputs(
        device = "",
        tz = "UTC",
        dataset_name = "",
        id = "automated",
        not_before = as.Date("2001-01-01"),
        options = character(),
        version = "",
        import = 0
      )
      session$setInputs(import = 1)
      session$flushReact()

      expect_identical(returned$status()$state, "error")
      expect_s3_class(returned$error(), "llw_validation_error")
      expect_match(
        llw_public_message(returned$error()),
        "Choose files"
      )

      session$setInputs(import = 2)
      session$flushReact()
      expect_identical(returned$status()$state, "error")
    }
  )
})

test_that("import UI explains participant-ID mapping choices", {
  html <- htmltools::renderTags(
    importUI("import")
  )$html

  expect_match(
    html,
    "Use the complete filename stem (1 ID per file)",
    fixed = TRUE
  )
  expect_match(html, "An AI assistant (LLM)", fixed = TRUE)
  expect_match(html, "^([^_]+)", fixed = TRUE)
  expect_match(html, "([^_]+)$", fixed = TRUE)
  expect_match(html, "llw-tooltip-code", fixed = TRUE)
  expect_match(html, 'placement="top"', fixed = TRUE)
  expect_match(html, "fallbackPlacements&quot;:[]", fixed = TRUE)
  expect_match(html, "Numeric variables", fixed = TRUE)
  expect_false(grepl("Detailed data review", html, fixed = TRUE))
  expect_match(html, 'data-value="quality_checks"', fixed = TRUE)
  expect_match(html, 'data-value="numeric_variables"', fixed = TRUE)
  expect_match(html, 'data-value="participant_quality"', fixed = TRUE)
  expect_match(html, 'data-value="row_preview"', fixed = TRUE)
  expect_match(html, "Numeric variables and analysis focus", fixed = TRUE)
  expect_match(html, "Time points (rows)", fixed = TRUE)
  expect_match(html, "Missing time points (missing rows)", fixed = TRUE)
  expect_match(html, "llw-variable-review-table", fixed = TRUE)
  expect_match(html, "llw-participant-quality-table", fixed = TRUE)
  expect_match(html, "mx-auto mt-3", fixed = TRUE)
  expect_match(
    html,
    'data-bs-parent="#import-quality_details_accordion"',
    fixed = TRUE
  )
  expect_false(grepl(
    'data-bs-parent="#import-import_accordion"',
    html,
    fixed = TRUE
  ))
  expect_false(grepl("Analysis focus variable", html, fixed = TRUE))
})

test_that("raw import diagnostics pair color with an icon and text", {
  diagnostics <- data.frame(
    check = c("Identity", "Gaps", "Date range"),
    status = c("pass", "warning", "information"),
    value = c("Ready", "2", "2026-01-01"),
    detail = c("Complete", "Review", "Context"),
    stringsAsFactors = FALSE
  )
  html <- htmltools::renderTags(
    raw_import_diagnostics_table_ui(diagnostics)
  )$html

  expect_match(html, "llw-diagnostic-status--success", fixed = TRUE)
  expect_match(html, "llw-diagnostic-status--warning", fixed = TRUE)
  expect_match(html, "llw-diagnostic-status--information", fixed = TRUE)
  expect_match(html, "Pass", fixed = TRUE)
  expect_match(html, "Warning", fixed = TRUE)
  expect_match(html, "Info", fixed = TRUE)
  expect_match(html, "What this checks", fixed = TRUE)
  expect_match(html, "Before or during import", fixed = TRUE)
  expect_match(html, "After import", fixed = TRUE)
  expect_match(html, "Explain Gaps", fixed = TRUE)
  expect_match(html, "visually-hidden\">Explanation", fixed = TRUE)
  expect_equal(
    stringr::str_count(html, "llw-diagnostic-help"),
    nrow(diagnostics) + 1L
  )
})

test_that("variable review pairs eligibility color with an icon and text", {
  data <- m1_fixture_data()
  eligibility <- primary_variable_eligibility(data)
  profile <- raw_import_signal_profile(data, eligibility)
  review <- raw_import_variable_review(eligibility, profile)
  html <- htmltools::renderTags(
    raw_import_variable_review_table_ui(review)
  )$html

  expect_match(html, "Analysis focus", fixed = TRUE)
  expect_match(html, "llw-diagnostic-status--success", fixed = TRUE)
  expect_match(html, "llw-diagnostic-status--danger", fixed = TRUE)
  expect_match(html, "Eligible", fixed = TRUE)
  expect_match(html, "Not eligible", fixed = TRUE)
  expect_match(html, "Why unavailable", fixed = TRUE)
  expect_match(html, "Structural import column", fixed = TRUE)
})

test_that("eligible variables can be preselected from the review table", {
  data <- m1_fixture_data()
  data$Photopic <- c(1, 2, 3)
  eligibility <- primary_variable_eligibility(data)
  profile <- raw_import_signal_profile(data, eligibility)
  review <- raw_import_variable_review(eligibility, profile)
  eligible <- review$variable[review$eligible]
  expect_gte(length(eligible), 2L)

  html <- htmltools::renderTags(raw_import_variable_review_table_ui(
    review,
    selected_variable = eligible[[1L]],
    ns = identity
  ))$html

  expect_match(html, "Preselected", fixed = TRUE)
  expect_match(html, ">Select</span>", fixed = TRUE)
  expect_match(html, "llw-focus-select", fixed = TRUE)
  expect_match(html, "focus_variable_", fixed = TRUE)
  expect_match(html, "disabled=\"disabled\"", fixed = TRUE)
  expect_match(html, "Not eligible", fixed = TRUE)
})

test_that("raw import warning bullets explain meaning and later action", {
  html <- htmltools::renderTags(
    raw_import_warning_item_ui(
      "Implicit gaps are present; no timestamps or exposure values were imputed."
    )
  )$html

  expect_match(html, "What this means", fixed = TRUE)
  expect_match(html, "unobserved intervals", fixed = TRUE)
  expect_match(html, "What you can do later", fixed = TRUE)
  expect_match(html, "scientific method", fixed = TRUE)
  expect_match(html, "Explain this import warning", fixed = TRUE)
})

test_that("a valid raw import returns an explicit reviewed dataset event", {
  fixture <- m3_stage_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(fixture$source, force = TRUE), add = TRUE)
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    session = NULL
  )
  on.exit(runtime$cleanup(), add = TRUE)
  upload <- data.frame(
    name = "P01_actlumus.txt",
    size = file.info(fixture$source)$size,
    type = "text/plain",
    datapath = fixture$source,
    stringsAsFactors = FALSE
  )

  shiny::testServer(
    importServer,
    args = list(runtime = runtime, color_mode = shiny::reactive("light")),
    {
      returned <- session$getReturned()
      session$setInputs(
        file = upload,
        device = "ActLumus",
        tz = "UTC",
        dataset_name = "Valid fixture",
        id = "automated",
        not_before = as.Date("2001-01-01"),
        options = character(),
        version = "default",
        import = 0,
        add_dataset = 0,
        add_variable = 0
      )
      session$setInputs(import = 1)
      flush_m1_promises(session, times = 40L)

      expect_true(returned$status()$state %in% c("complete", "warning"))
      expect_s3_class(returned$result(), "llw_raw_import_result")
      expect_identical(
        unique(as.character(returned$result()$data$Id)),
        "P01_actlumus"
      )
      expect_identical(returned$focus_variable(), "MEDI")
      eligibility <- returned$result()$eligibility
      alternative <- eligibility$variable[
        eligibility$eligible & eligibility$variable != "MEDI"
      ][[1L]]
      alternative_index <- match(alternative, eligibility$variable)
      focus_input <- stats::setNames(
        list(1L),
        raw_import_focus_input_id(alternative_index)
      )
      do.call(session$setInputs, focus_input)
      session$flushReact()
      expect_identical(returned$focus_variable(), alternative)
      expect_error(
        returned$add_dataset(),
        class = "shiny.silent.error"
      )

      session$setInputs(add_dataset = 1)
      session$flushReact()
      session$setInputs(variable = alternative, add_variable = 1)
      session$flushReact()
      value <- returned$add_dataset()
      expect_identical(value$name, "Valid fixture")
      expect_identical(value$variable, alternative)
      expect_s3_class(value$quality, "llw_raw_import_quality")
    }
  )
})

test_that("a malformed import can be corrected and retried in one module session", {
  malformed <- tempfile(fileext = ".txt")
  writeLines("not an ActLumus export", malformed, useBytes = TRUE)
  on.exit(unlink(malformed, force = TRUE), add = TRUE)
  valid <- m3_stage_fixture()
  on.exit(unlink(valid$root, recursive = TRUE, force = TRUE), add = TRUE)
  on.exit(unlink(valid$source, force = TRUE), add = TRUE)
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    session = NULL
  )
  on.exit(runtime$cleanup(), add = TRUE)
  upload <- function(name, path) {
    data.frame(
      name = name,
      size = file.info(path)$size,
      type = "text/plain",
      datapath = path,
      stringsAsFactors = FALSE
    )
  }

  shiny::testServer(
    importServer,
    args = list(runtime = runtime, color_mode = shiny::reactive("light")),
    {
      returned <- session$getReturned()
      session$setInputs(
        file = upload("P01_bad.txt", malformed),
        device = "ActLumus",
        tz = "UTC",
        dataset_name = "Retry fixture",
        id = "automated",
        not_before = as.Date("2001-01-01"),
        options = character(),
        version = "default",
        import = 0,
        add_dataset = 0,
        add_variable = 0
      )
      session$setInputs(import = 1)
      flush_m1_promises(session, times = 40L)

      expect_identical(returned$status()$state, "error")
      expect_s3_class(returned$error(), "llw_import_error")
      expect_match(llw_public_message(returned$error()), "could not parse")

      session$setInputs(file = upload("P01_actlumus.txt", valid$source))
      session$setInputs(import = 2)
      flush_m1_promises(session, times = 40L)

      expect_true(returned$status()$state %in% c("complete", "warning"))
      expect_s3_class(returned$result(), "llw_raw_import_result")
      expect_error(
        returned$add_dataset(),
        class = "shiny.silent.error"
      )
      session$setInputs(add_dataset = 1)
      session$flushReact()
      session$setInputs(variable = "MEDI", add_variable = 1)
      session$flushReact()
      expect_identical(returned$add_dataset()$name, "Retry fixture")
      expect_identical(returned$add_dataset()$variable, "MEDI")
    }
  )
})
