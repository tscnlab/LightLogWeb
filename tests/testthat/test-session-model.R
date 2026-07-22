test_that("dataset records keep an immutable canonical raw payload", {
  record <- m1_record()
  original_payload <- record$raw_payload
  copy <- dataset_raw_data(record)
  copy$MEDI[[1L]] <- 999

  expect_s3_class(record, "llw_dataset_record")
  expect_match(record$id, "^dataset_[a-z0-9]+$")
  expect_identical(dataset_raw_data(record), m1_fixture_data())
  expect_false(identical(dataset_raw_data(record), copy))
  expect_true(dataset_raw_is_unchanged(record, original_payload))
  expect_null(nonserializable_path(record))
})

test_that("draft, preview, apply, reset, and undo are revisioned pure changes", {
  record <- m1_record()
  original_payload <- record$raw_payload
  recipe <- new_recipe(list(new_recipe_step(
    type = "preview_rows",
    parameters = list(n = 1L)
  )))

  draft <- stage_dataset_draft(record, list(recipe = recipe))
  expect_null(record$draft)
  expect_s3_class(draft$draft, "llw_dataset_draft")
  expect_equal(draft$revision, 0L)

  previewed <- preview_dataset_draft(
    draft,
    function(raw_data, recipe, analysis_settings, factual_metadata) {
      utils::head(raw_data, recipe$steps[[1L]]$parameters$n)
    }
  )
  expect_s3_class(previewed$draft$preview, "llw_dataset_preview")
  expect_equal(previewed$draft$preview$summary$prepared_rows, 1L)

  applied <- apply_dataset_draft(previewed)
  expect_equal(applied$revision, 1L)
  expect_equal(nrow(dataset_prepared_data(applied)), 1L)
  expect_true(dataset_raw_is_unchanged(applied, original_payload))

  reset <- reset_dataset_record(applied)
  expect_equal(reset$revision, 2L)
  expect_identical(dataset_prepared_data(reset), dataset_raw_data(reset))
  expect_identical(reset$prepared_payload, reset$raw_payload)
  expect_true(dataset_raw_is_unchanged(reset, original_payload))

  undone <- undo_dataset_record(reset)
  expect_equal(undone$revision, 3L)
  expect_equal(nrow(dataset_prepared_data(undone)), 1L)
  expect_true(dataset_raw_is_unchanged(undone, original_payload))
})

test_that("a preview cannot be applied after its base revision changes", {
  record <- stage_dataset_draft(
    m1_record(),
    list(recipe = new_recipe(list(new_recipe_step(type = "identity"))))
  )
  record <- preview_dataset_draft(
    record,
    function(raw_data, recipe, analysis_settings, factual_metadata) raw_data
  )
  record$revision <- record$revision + 1L

  expect_error(
    apply_dataset_draft(record),
    class = "llw_validation_error",
    regexp = "stale"
  )
})

test_that("stable IDs decouple selection from display names", {
  first <- m1_record("First")
  second <- m1_record("Second")
  model <- new_session_model("hosted")
  model <- session_add_dataset(model, first)
  model <- session_add_dataset(model, second, select = FALSE)

  expect_false(identical(first$id, second$id))
  expect_identical(model$selected_dataset_id, first$id)
  renamed <- rename_dataset_record(first, "Renamed")
  model <- session_replace_dataset(model, renamed)
  expect_identical(names(model$datasets), c(first$id, second$id))
  expect_identical(session_dataset(model, first$id)$display_name, "Renamed")
})

test_that("dataset names are uniquely enforced after trimming and case folding", {
  first <- m1_record("Exposure study")
  conflicting <- m1_record("  exposure STUDY  ")
  other <- m1_record("Other dataset")
  model <- session_add_dataset(new_session_model("hosted"), first)

  expect_identical(conflicting$display_name, "exposure STUDY")
  expect_error(
    session_add_dataset(model, conflicting),
    class = "llw_dataset_name_conflict_error",
    regexp = "already exists"
  )

  model <- session_add_dataset(model, other)
  renamed <- rename_dataset_record(other, "EXPOSURE STUDY")
  expect_error(
    session_replace_dataset(model, renamed),
    class = "llw_dataset_name_conflict_error"
  )
  expect_identical(
    dataset_display_name_conflict(" exposure study ", "Exposure study"),
    "Exposure study"
  )
})

test_that("name-conflict retries preserve event intent and source records", {
  record <- m1_record("Existing name")
  add_event <- new_session_event("add", value = record)
  renamed_add <- session_event_with_display_name(
    add_event,
    "  New incoming name  "
  )

  expect_identical(renamed_add$type, "add")
  expect_identical(renamed_add$value$display_name, "New incoming name")
  expect_identical(record$display_name, "Existing name")
  expect_identical(renamed_add$value$id, record$id)

  rename_event <- new_session_event(
    "rename",
    dataset_id = record$id,
    value = "Conflicting name"
  )
  renamed_retry <- session_event_with_display_name(rename_event, "Unique name")
  expect_identical(renamed_retry$type, "rename")
  expect_identical(renamed_retry$dataset_id, record$id)
  expect_identical(renamed_retry$value, "Unique name")
})

test_that("separate Shiny sessions do not share dataset state", {
  first_session_model <- NULL
  server <- function(input, output, session) {
    session$userData$store <- new_session_store("hosted")
  }

  shiny::testServer(server, {
    store <- session$userData$store
    store$dispatch(new_session_event("add", value = m1_record("Session one")))
    first_session_model <<- shiny::isolate(store$model())
    expect_length(shiny::isolate(store$datasets()), 1L)
  })

  shiny::testServer(server, {
    store <- session$userData$store
    expect_length(shiny::isolate(store$datasets()), 0L)
    expect_null(shiny::isolate(store$selected_dataset_id()))
  })

  expect_length(first_session_model$datasets, 1L)
  expect_null(nonserializable_path(first_session_model))
})

test_that("imperative store dispatch is safe during top-level server setup", {
  model <- NULL
  server <- function(input, output, session) {
    store <- new_session_store("hosted")
    store$dispatch(new_session_event("add", value = m1_record("Initial")))
    model <<- shiny::isolate(store$model())
  }

  shiny::testServer(server, {
    expect_length(model$datasets, 1L)
    expect_identical(session_dataset(model)$display_name, "Initial")
  })
})

test_that("canonical raw checksums detect direct payload tampering", {
  record <- m1_record()
  record$raw_payload[[1L]] <- as.raw(bitwXor(
    as.integer(record$raw_payload[[1L]]),
    1L
  ))

  expect_error(
    validate_dataset_record(record),
    class = "llw_validation_error",
    regexp = "immutable checksum"
  )
})

test_that("recipes use stable, enabled, serializable steps and cache keys", {
  step <- new_recipe_step(
    type = "filter_dates",
    parameters = list(start = as.Date("2026-01-01")),
    enabled = FALSE
  )
  recipe <- new_recipe(list(step))
  record <- m1_record()
  record <- stage_dataset_draft(record, list(recipe = recipe))
  record <- preview_dataset_draft(
    record,
    function(raw_data, recipe, analysis_settings, factual_metadata) raw_data
  )
  record <- apply_dataset_draft(record)

  expect_match(step$id, "^step_[a-z0-9]+$")
  expect_false(step$enabled)
  expect_match(recipe_fingerprint(recipe), "^sha256:[0-9a-f]{64}$")
  expect_match(
    dataset_cache_key(record),
    paste0("^prepared-", record$id, "-", record$revision, "-")
  )
  expect_null(nonserializable_path(record))
})

test_that("session events validate their explicit payload contracts", {
  record <- m1_record()
  add <- new_session_event("add", value = record)

  expect_s3_class(add, "llw_session_event")
  expect_null(nonserializable_path(add))
  expect_error(
    new_session_event("remove"),
    class = "llw_validation_error",
    regexp = "requires a dataset ID"
  )
  expect_error(
    new_session_event("reset", dataset_id = record$id, value = TRUE),
    class = "llw_validation_error",
    regexp = "does not accept a value"
  )
})
