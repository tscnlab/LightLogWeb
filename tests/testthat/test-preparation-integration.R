m8_integration_form <- function(session, context, type = "participants", ...) {
  values <- utils::modifyList(preparation_form_defaults(context), list(...))
  values$step_type <- type
  values$preparation_category <- if (type %in% preparation_time_types())
    "sampling" else if (
    type %in% c("invalidate", "calibrate", "transform", "variability")
  )
    "values" else "filter"
  values$sampling_type <- if (type %in% preparation_time_types()) type else
    "timezone"
  values$value_type <- if (type == "transform") "calibrate" else if (
    type %in% c("invalidate", "calibrate", "variability")
  )
    type else "invalidate"
  names(values) <- paste0("prepare-", names(values))
  do.call(session$setInputs, values)
  session$flushReact()
}

test_that("saved preparation drafts are serializable and separate from applied data", {
  record <- m8_record()
  recipe <- new_recipe(list(m8_clock(m8_context(record))))
  draft <- preparation_stage_recipe(record, recipe, preparation_binding(record))
  expect_identical(draft$raw_payload, record$raw_payload)
  expect_identical(draft$prepared_payload, record$prepared_payload)
  expect_identical(draft$recipe, record$recipe)
  expect_identical(draft$revision, record$revision)
  expect_identical(draft$history, record$history)
  expect_identical(preparation_saved_recipe(draft), recipe)
  expect_null(draft$draft$preview)
  restored <- validate_dataset_record(unserialize(serialize(draft, NULL)))
  expect_identical(preparation_saved_recipe(restored), recipe)
  renamed <- rename_dataset_record(restored, "Renamed with a draft")
  expect_identical(preparation_saved_recipe(renamed), recipe)
  expect_length(
    preparation_saved_recipe(reset_dataset_record(renamed))$steps,
    0L
  )
  expect_identical(
    dashboard_record_signature(draft),
    dashboard_record_signature(record)
  )
  expect_true(dataset_record_same_data(draft, record))
})

test_that("Prepare commits reject different datasets and changed revisions", {
  record <- m8_record()
  preview <- m8_preview(record, new_recipe(list(m8_clock(m8_context(record)))))
  expect_identical(
    preparation_commit_record(record, preview$candidate, preview$binding),
    preview$candidate
  )
  changed <- rename_dataset_record(record, "New name")
  expect_error(
    preparation_commit_record(changed, preview$candidate, preview$binding),
    "dataset changed"
  )
  different <- record
  different$id <- "dataset_other"
  expect_error(
    preparation_commit_record(different, preview$candidate, preview$binding),
    "dataset changed"
  )
  expect_error(
    preparation_commit_record(record, different, preview$binding),
    "does not belong"
  )
  expect_error(
    preparation_stage_recipe(
      changed,
      preview$candidate$recipe,
      preview$binding
    ),
    "dataset changed"
  )
})

test_that("dashboard focus selects a source or derived preview without changing the recipe", {
  record <- m8_record()
  raw <- unserialize(record$raw_payload)
  raw$Movement <- raw$MEDI / 10
  record <- new_dataset_record(
    raw,
    "Two measurements",
    record$source_manifest,
    factual_metadata = list(
      variables = list(MEDI = list(label = "Melanopic EDI", unit = "lx"))
    ),
    analysis_settings = list(primary_variable = "MEDI")
  )
  context <- m8_context(record)
  expect_match(context$measurement_labels$MEDI, "Melanopic EDI.*MEDI.*lx")
  expect_match(
    context$measurement_labels$Movement,
    "Movement.*unit not specified"
  )
  step <- new_preparation_step(
    "calibrate",
    list(
      variable = "MEDI",
      multiplier = 2,
      offset = 0,
      reference = "",
      output = "MEDI_processed"
    ),
    context
  )
  recipe <- new_recipe(list(step))
  view <- preparation_task_worker(list(
    action = "preview",
    record = record,
    recipe = recipe,
    budget = 1024^3,
    focus_variable = "Movement"
  ))
  expect_identical(view$candidate$recipe, recipe)
  expect_identical(view$context$variable, "Movement")
  expect_match(
    view$comparison$view$transformation_note,
    "Dashboard focus: Movement"
  )
  expect_equal(
    view$comparison$view$plot$Value,
    view$comparison$view$plot$Preview_value
  )
  applied <- m8_preview(record, recipe)$candidate
  requested <- preparation_focus_request(
    m8_context(applied),
    applied,
    "MEDI_processed"
  )
  expect_identical(
    preparation_focus_context(requested, recipe)$variable,
    "MEDI_processed"
  )
  fallback <- preparation_focus_context(requested, new_recipe())
  expect_identical(fallback$variable, "MEDI")
  expect_match(fallback$transformation_note, "Not created at this point")
})

test_that("the full app keeps drafts across navigation and applies to the selected dataset only", {
  first <- m8_record()
  second <- m1_showcase_record()
  app <- lightlogweb_app(
    profile = "local",
    workers = 0L,
    initial_records = list(first, second),
    initial_panel = "dashboard",
    registry_loader = function(...) stop("Offline")
  )
  shiny::testServer(app, {
    session$setInputs(main_nav = "dashboard")
    session$flushReact()
    expect_null(prepared$context())
    session$setInputs(main_nav = "prepare")
    m8_wait(session, function() !is.null(prepared$context()))
    m8_integration_form(session, prepared$context())
    expect_identical(output[["prepare-flow_editing"]], "no")
    dashboard_revision <- dashboard$ready()$revision
    m8_integration_form(session, prepared$context(), "clock")
    session$setInputs(`prepare-save_new` = 1L)
    m8_wait(session, function() !is.null(prepared$preview()))
    saved <- prepared$recipe()
    expect_length(saved$steps, 1L)
    expect_identical(preparation_saved_recipe(store$selected_dataset()), saved)
    expect_identical(dashboard$ready()$revision, dashboard_revision)
    expect_identical(
      store$selected_dataset()$prepared_payload,
      first$raw_payload
    )
    session$setInputs(main_nav = "dashboard")
    session$setInputs(main_nav = "prepare")
    expect_identical(prepared$recipe(), saved)
    store$select_dataset(second$id)
    m8_wait(
      session,
      function() identical(prepared$context()$dataset_id, second$id)
    )
    m8_integration_form(session, prepared$context())
    expect_length(prepared$recipe()$steps, 0L)
    store$select_dataset(first$id)
    m8_wait(
      session,
      function() identical(prepared$context()$dataset_id, first$id)
    )
    expect_identical(prepared$recipe(), saved)
    m8_integration_form(session, prepared$context(), "clock")
    session$setInputs(`prepare-preview` = 1L)
    m8_wait(session, function() !is.null(prepared$preview()))
    expected_rows <- prepared$preview()$comparison$after$rows
    session$setInputs(`prepare-apply` = 1L)
    session$flushReact()
    expect_identical(store$selected_dataset()$recipe, saved)
    expect_equal(
      nrow(dataset_prepared_data(store$selected_dataset())),
      expected_rows
    )
    expect_identical(store$selected_dataset()$raw_payload, first$raw_payload)
    expect_null(store$selected_dataset()$draft)
    expect_identical(store$datasets()[[second$id]], second)
    expect_equal(dashboard$status()$prepared_rows, expected_rows)
    expect_identical(dashboard$ready()$dataset_id, first$id)
    session$setInputs(`prepare-reset` = 1L)
    m8_wait(
      session,
      function()
        identical(store$selected_dataset()$prepared_payload, first$raw_payload)
    )
    expect_length(store$selected_dataset()$recipe$steps, 0L)
    m8_wait(session, function() !is.null(prepared$context()))
    m8_integration_form(session, prepared$context())
    m8_wait(session, function() !prepared$active() && !prepared$pending())
    expect_length(prepared$recipe()$steps, 0L)
    expect_null(store$selected_dataset()$draft)
    session$setInputs(main_nav = "dashboard")
    session$setInputs(main_nav = "prepare")
    expect_length(prepared$recipe()$steps, 0L)
    expect_null(store$selected_dataset()$draft)
  })
})

test_that("dashboard sampling and coverage use the prepared interval after aggregation", {
  record <- m8_record(days = 1L, interval = 60)
  step <- new_preparation_step(
    "aggregate",
    list(
      length = 15,
      unit = "minutes",
      numeric = "mean",
      missing = "available"
    ),
    m8_context(record)
  )
  applied <- m8_preview(record, new_recipe(list(step)))$candidate
  snapshot <- dashboard_dataset_snapshot(applied)
  expect_identical(dashboard_sampling_label(snapshot$quality), "60 s")
  expect_identical(dashboard_sampling_label(snapshot$prepared_quality), "900 s")
  prepared <- dashboard_focus_view(snapshot, "MEDI", "preprocessed")
  source <- dashboard_focus_view(snapshot, "MEDI", "source")
  expect_identical(prepared$quality, snapshot$prepared_quality)
  expect_identical(source$quality, snapshot$quality)
  expect_equal(
    prepared$coverage,
    dashboard_daily_coverage(
      snapshot$prepared_data,
      prepared$quality,
      snapshot$display_timezone,
      "MEDI"
    )
  )
  expect_equal(
    prepared$coverage[["Focus coverage (%)"]],
    source$coverage[["Focus coverage (%)"]]
  )
})

test_that("full-app Prepare inherits dashboard focus and axis settings", {
  record <- m7_fixture_record("iztech-light-subset")
  app <- lightlogweb_app(
    profile = "local",
    workers = 0L,
    initial_records = list(record),
    initial_panel = "dashboard",
    registry_loader = function(...) stop("Offline")
  )
  shiny::testServer(app, {
    session$setInputs(main_nav = "dashboard")
    session$flushReact()
    session$setInputs(
      `dashboard-focus_variable` = "LIGHT",
      `dashboard-plot_scale` = "linear"
    )
    session$flushReact()
    session$setInputs(main_nav = "prepare")
    m8_wait(
      session,
      function() identical(prepared$context()$requested_variable, "LIGHT")
    )
    m8_integration_form(session, prepared$context())
    m8_wait(session, function() !is.null(prepared$preview()))
    expect_identical(prepared$preview()$context$variable, "LIGHT")
    expect_match(output[["prepare-plot_axis_label"]], "Linear")
    expect_match(prepared$context()$measurement_labels$LIGHT, "lx")
    session$setInputs(main_nav = "dashboard", `dashboard-plot_scale` = "log")
    session$setInputs(main_nav = "prepare")
    expect_match(output[["prepare-plot_axis_label"]], "Log")
  })
})

test_that("applied derived measurements are selectable and source views use their origin", {
  source <- m7_fixture_record("iztech-light-subset")
  step <- new_preparation_step(
    "calibrate",
    list(
      variable = "LIGHT",
      output = "LIGHT_scaled",
      multiplier = 2,
      offset = 0,
      reference = ""
    ),
    m8_context(source)
  )
  applied <- m8_preview(source, new_recipe(list(step)))$candidate
  snapshot <- dashboard_dataset_snapshot(applied)
  expect_true("LIGHT_scaled" %in% snapshot$focus_variables)
  expect_identical(applied$raw_payload, source$raw_payload)
  label <- names(snapshot$focus_variable_choices)[
    snapshot$focus_variable_choices == "LIGHT_scaled"
  ]
  expect_identical(label, "Photopic illuminance [LIGHT_scaled] \u00b7 lx")
  expect_true(
    label %in%
      names(dashboard_metadata_display_table(
        snapshot$prepared_data,
        applied
      ))
  )
  expect_identical(
    dashboard_focus_view(snapshot, "LIGHT_scaled")$focus_variable,
    "LIGHT_scaled"
  )
  raw_view <- dashboard_focus_view(snapshot, "LIGHT_scaled", "source")
  expect_identical(raw_view$focus_variable, "LIGHT")
  expect_identical(raw_view$data, snapshot$raw_data)
  expect_match(
    dashboard_source_focus_note(snapshot, "LIGHT_scaled"),
    "original variable LIGHT"
  )
  expect_true(
    "LIGHT" %in% dashboard_main_columns(snapshot, "LIGHT_scaled", "source")
  )
  expect_false(
    "LIGHT_scaled" %in%
      dashboard_main_columns(snapshot, "LIGHT_scaled", "source")
  )
  reset <- dashboard_dataset_snapshot(reset_dataset_record(applied))
  expect_false("LIGHT_scaled" %in% reset$focus_variables)
  inspected <- preparation_task_worker(list(
    action = "inspect",
    record = applied,
    budget = 1024^3,
    focus_variable = "LIGHT_scaled"
  ))

  app <- lightlogweb_app(
    profile = "local",
    workers = 0L,
    initial_records = list(applied),
    initial_panel = "dashboard",
    registry_loader = function(...) stop("Offline")
  )
  shiny::testServer(app, {
    session$setInputs(
      main_nav = "dashboard",
      `dashboard-focus_variable` = "LIGHT_scaled"
    )
    expect_identical(dashboard$focus_variable(), "LIGHT_scaled")
    session$setInputs(`dashboard-data_stage` = "source")
    expect_match(
      output[["dashboard-explore_context"]]$html,
      "original variable LIGHT"
    )
    expect_match(
      output[["dashboard-raw_state_note"]]$html,
      "original variable LIGHT"
    )
    session$setInputs(main_nav = "prepare")
    m8_wait(
      session,
      function()
        identical(prepared$context()$requested_variable, "LIGHT_scaled")
    )
    m8_integration_form(
      session,
      preparation_new_step_context(
        prepared$context(),
        prepared$recipe(),
        inspected$catalogs
      )
    )
    m8_wait(session, function() !is.null(prepared$preview()))
    expect_identical(
      prepared$preview()$comparison$view$variable,
      "LIGHT_scaled"
    )
    expect_match(
      prepared$preview()$comparison$view$transformation_note,
      "Dashboard focus: LIGHT_scaled"
    )
  })
})

test_that("metadata edits preserve the saved recipe and invalidate an older preview", {
  record <- m7_fixture_record()
  app <- lightlogweb_app(
    profile = "local",
    workers = 0L,
    initial_records = list(record),
    initial_panel = "prepare",
    registry_loader = function(...) stop("Offline")
  )
  shiny::testServer(app, {
    session$setInputs(main_nav = "prepare")
    m8_wait(session, function() !is.null(prepared$context()))
    m8_integration_form(session, prepared$context())
    m8_integration_form(session, prepared$context(), "clock")
    session$setInputs(`prepare-save_new` = 1L)
    m8_wait(session, function() !is.null(prepared$preview()))
    old_preview <- prepared$preview()
    saved <- prepared$recipe()
    session$setInputs(main_nav = "metadata")
    before <- store$selected_dataset()
    context <- metadata_overlay_context(before)
    rows <- context$catalog
    target <- rows$target_id[
      rows$table == "variables" & rows$field == "label" & rows$name == "MEDI"
    ][[1L]]
    patch <- metadata_overlay_preview(
      before,
      context,
      target,
      "Updated light label"
    )
    store$apply_overlay(context, patch)
    session$flushReact()
    expect_identical(preparation_saved_recipe(store$selected_dataset()), saved)
    expect_error(
      store$commit_preparation(old_preview$candidate, old_preview$binding),
      "dataset changed"
    )
    session$setInputs(main_nav = "prepare")
    m8_wait(session, function() !is.null(prepared$context()))
    m8_integration_form(session, prepared$context(), "clock")
    session$setInputs(`prepare-preview` = 1L)
    m8_wait(session, function() !is.null(prepared$preview()))
    expect_match(
      prepared$context()$measurement_labels$MEDI,
      "Updated light label"
    )
    session$setInputs(`prepare-apply` = 1L)
    session$flushReact()
    expect_match(
      preparation_variable_label(store$selected_dataset(), "MEDI"),
      "Updated light label"
    )
    expect_identical(store$selected_dataset()$recipe, saved)
    expect_identical(store$selected_dataset()$raw_payload, record$raw_payload)
  })
})
