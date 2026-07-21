test_that("the import wizard preserves the accepted import controls and outputs", {
  html <- htmltools::renderTags(importWizardUI("import"))$html
  contract_ids <- c(
    "file",
    "device",
    "version",
    "version_select",
    "veet_modality",
    "n_files",
    "filenames",
    "tz",
    "dataset_name",
    "not_before",
    "options",
    "id",
    "Id_manual",
    "Id_extract",
    "n_ids",
    "pattern",
    "wizard_stepper",
    "wizard_id_preview",
    "mapping_status",
    "id_mapping",
    "wizard_check_summary",
    "import",
    "cancel_import_button",
    "task_status",
    "phase_status",
    "quality_callout",
    "quality_rows",
    "quality_participants",
    "quality_gaps",
    "quality_variables",
    "plot_overview",
    "import_msg",
    "quality_details_accordion",
    "quality_diagnostics",
    "signal_profile",
    "participant_quality",
    "preview_notice",
    "import_table",
    "add_dataset"
  )

  for (id in contract_ids) {
    expect_equal(
      stringr::str_count(html, paste0('id="import-', id, '"')),
      1L,
      info = paste("Expected exactly one wizard element for", id)
    )
  }

  expect_match(html, 'data-value="source"', fixed = TRUE)
  expect_match(html, 'data-value="details"', fixed = TRUE)
  expect_match(html, 'data-value="check"', fixed = TRUE)
  expect_match(html, 'data-value="review"', fixed = TRUE)
  expect_false(grepl('data-value="import"', html, fixed = TRUE))
  expect_match(html, "Guided data import", fixed = TRUE)
  expect_match(html, "Powered by LightLogR", fixed = TRUE)
  expect_match(html, "four guided steps", fixed = TRUE)
  expect_false(grepl("Experimental wizard layout", html, fixed = TRUE))
  expect_false(grepl("This test reorganizes", html, fixed = TRUE))
  expect_match(html, "Optional file and participant ID preview", fixed = TRUE)
  expect_match(html, "keeps participant IDs from an existing", fixed = TRUE)
  expect_false(grepl("Fallback ID", html, fixed = TRUE))
  expect_false(grepl("Filename fallback ID", html, fixed = TRUE))
  expect_match(html, "Start import", fixed = TRUE)
  expect_match(html, "Numeric variables and analysis focus", fixed = TRUE)
  expect_match(html, "Preview of imported rows", fixed = TRUE)
  expect_match(html, "An AI assistant (LLM)", fixed = TRUE)
  expect_false(grepl('id="import-import_accordion"', html, fixed = TRUE))
  expect_false(grepl("llw-wizard-step-card", html, fixed = TRUE))
  expect_equal(stringr::str_count(html, "llw-wizard-step-content"), 4L)

  check_position <- stringr::str_locate(
    html,
    'id="import-wizard_check_summary"'
  )[[1L]]
  import_position <- stringr::str_locate(html, 'id="import-import"')[[1L]]
  progress_position <- stringr::str_locate(html, 'id="import-phase_status"')[[
    1L
  ]]
  status_position <- stringr::str_locate(html, 'id="import-task_status"')[[1L]]
  expect_lt(check_position, import_position)
  expect_lt(import_position, progress_position)
  expect_lt(progress_position, status_position)
})

test_that("the wizard stepper reflects readiness and gates review until import", {
  ns <- function(id) paste0("import-", id)
  html <- htmltools::renderTags(
    import_wizard_stepper(
      ns,
      "check",
      completed = character(),
      review_available = FALSE
    )
  )$html

  expect_equal(stringr::str_count(html, "is-complete"), 0L)
  expect_equal(stringr::str_count(html, "is-active"), 1L)
  expect_equal(stringr::str_count(html, "is-upcoming"), 3L)
  expect_equal(stringr::str_count(html, "llw-import-stepper__button"), 4L)
  expect_match(html, 'id="import-wizard_step_source"', fixed = TRUE)
  expect_match(html, 'id="import-wizard_step_details"', fixed = TRUE)
  expect_match(html, 'id="import-wizard_step_check"', fixed = TRUE)
  expect_match(html, 'id="import-wizard_step_review"', fixed = TRUE)
  expect_false(grepl('id="import-wizard_step_import"', html, fixed = TRUE))
  expect_match(html, 'aria-current="step"', fixed = TRUE)
  expect_match(html, "Current step", fixed = TRUE)
  expect_match(html, "Check &amp; import", fixed = TRUE)

  review_button <- stringr::str_extract(
    html,
    '<button[^>]*id="import-wizard_step_review"[^>]*>'
  )
  expect_match(review_button, 'disabled="disabled"', fixed = TRUE)
  expect_match(review_button, 'aria-disabled="true"', fixed = TRUE)

  available_html <- htmltools::renderTags(
    import_wizard_stepper(
      ns,
      "check",
      completed = c("source", "details"),
      review_available = TRUE
    )
  )$html
  expect_equal(stringr::str_count(available_html, "is-complete"), 2L)
  available_review_button <- stringr::str_extract(
    available_html,
    '<button[^>]*id="import-wizard_step_review"[^>]*>'
  )
  expect_false(grepl(
    'disabled="disabled"',
    available_review_button,
    fixed = TRUE
  ))
  expect_false(grepl(
    'aria-disabled="true"',
    available_review_button,
    fixed = TRUE
  ))

  reviewed_html <- htmltools::renderTags(
    import_wizard_stepper(
      ns,
      "review",
      completed = c("source", "details", "check"),
      review_available = TRUE
    )
  )$html
  expect_equal(stringr::str_count(reviewed_html, "is-complete"), 3L)
})

test_that("the wizard cancel button follows the active import state", {
  ns <- function(id) paste0("import-", id)
  disabled_html <- htmltools::renderTags(
    import_wizard_cancel_button(ns, enabled = FALSE)
  )$html
  enabled_html <- htmltools::renderTags(
    import_wizard_cancel_button(ns, enabled = TRUE)
  )$html

  expect_match(disabled_html, 'id="import-cancel_import"', fixed = TRUE)
  expect_match(disabled_html, 'disabled="disabled"', fixed = TRUE)
  expect_match(disabled_html, 'aria-disabled="true"', fixed = TRUE)
  expect_false(grepl('disabled="disabled"', enabled_html, fixed = TRUE))
  expect_false(grepl('aria-disabled="true"', enabled_html, fixed = TRUE))
})

test_that("the wizard check summary is concise and reports readiness", {
  ready_html <- htmltools::renderTags(
    import_wizard_check_summary_ui(
      file_count = "20",
      device = "ActLumus",
      timezone = "Europe/Berlin",
      id_count = "20",
      ready = TRUE
    )
  )$html
  warning_html <- htmltools::renderTags(
    import_wizard_check_summary_ui(
      file_count = "0",
      device = "Not selected",
      timezone = "Not selected",
      id_count = "Not ready",
      ready = FALSE
    )
  )$html

  expect_match(ready_html, "Setup ready to import", fixed = TRUE)
  expect_match(ready_html, "Starting the import will run", fixed = TRUE)
  expect_equal(
    stringr::str_count(ready_html, 'class="llw-wizard-metric"'),
    4L
  )
  expect_match(warning_html, "Setup needs attention", fixed = TRUE)
})
