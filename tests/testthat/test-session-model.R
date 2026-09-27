test_that("store byte accounts cover Apply, shifted history, Undo and replacement", {
  domain <- shiny::MockShinySession$new()
  on.exit(domain$close(), add = TRUE)
  store <- shiny::withReactiveDomain(domain, new_session_store("local"))
  record <- m7_fixture_record("tum-medi-only")
  store$dispatch(new_session_event("add", value = record))
  context <- metadata_overlay_context(record)
  target <- context$catalog$target_id[
    context$catalog$field == "unit" & context$catalog$name == "MEDI"
  ][[1L]]
  check_bytes <- function(record) {
    expect_gte(store$record_bytes(record), as.numeric(object.size(record)))
    expect_equal(store$record_bytes(record), store$record_bytes(record))
  }
  check_bytes(record)
  for (i in seq_len(22L)) {
    record <- store$apply_overlay(
      context,
      metadata_overlay_preview(
        record,
        context,
        target,
        paste0("custom-unit-", i)
      )
    )
    check_bytes(record)
  }
  expect_length(record$history, 20L)
  for (i in seq_len(3L)) {
    record <- store$apply_overlay(context, undo = TRUE)
    check_bytes(record)
  }
  record <- store$apply_overlay(
    context,
    metadata_overlay_preview(
      record,
      context,
      target,
      scope = "single",
      action = "remove"
    )
  )
  check_bytes(record)
  store$dispatch(new_session_event(
    "rename",
    dataset_id = record$id,
    value = "Renamed"
  ))
  check_bytes(shiny::isolate(store$selected_dataset()))
  store$dispatch(new_session_event("remove", dataset_id = record$id))
  expect_null(shiny::isolate(store$selected_dataset()))
  other <- m1_record()
  store$dispatch(new_session_event("add", value = other))
  check_bytes(other)
})

test_that("quick edits create one store history entry and remain session-local", {
  domain <- shiny::MockShinySession$new()
  on.exit(domain$close(), add = TRUE)
  record <- m7_fixture_record("tum-medi-only")
  first <- shiny::withReactiveDomain(domain, new_session_store("local"))
  second <- shiny::withReactiveDomain(domain, new_session_store("local"))
  for (store in list(first, second))
    store$dispatch(new_session_event("add", value = record))
  context <- metadata_overlay_context(record)
  updated <- first$apply_overlay(
    context,
    metadata_overlay_focus_preview(
      record,
      context,
      "MEDI",
      "Quick label",
      "klx"
    )
  )
  expect_length(updated$history, 1L)
  expect_identical(updated$metadata_bundle$overlay$revision, 1L)
  expect_identical(shiny::isolate(second$selected_dataset()), record)
  expect_gte(first$record_bytes(updated), as.numeric(object.size(updated)))
  restored <- first$apply_overlay(context, undo = TRUE)
  expect_equal(nrow(restored$metadata_bundle$overlay$tables$variables), 0L)
  expect_identical(restored$raw_payload, record$raw_payload)
  expect_identical(
    restored$metadata_bundle$sources,
    record$metadata_bundle$sources
  )
})

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

test_that("copy and recovery keep source and metadata-free annotations usable", {
  for (record in list(m7_fixture_record(), m1_record("No source metadata"))) {
    edited <- m7_overlay_edit(record, "label", "Local display label")
    copy <- duplicate_dataset_record(edited, "Independent copy")
    expect_false(identical(copy$id, edited$id))
    expect_identical(
      copy$metadata_bundle$sources,
      edited$metadata_bundle$sources
    )
    expect_identical(copy$raw_payload, edited$raw_payload)
    expect_identical(copy$prepared_payload, edited$prepared_payload)
    expect_identical(
      metadata_resolve_variable(copy, "MEDI")$label,
      "Local display label"
    )
    context <- metadata_overlay_context(copy)
    target <- context$catalog$target_id[
      context$catalog$table == "variables" &
        context$catalog$field == "label" &
        context$catalog$name == "MEDI"
    ][[1L]]
    removed <- apply_metadata_overlay(
      copy,
      context,
      metadata_overlay_preview(copy, context, target, action = "remove")
    )
    expect_false(identical(
      metadata_resolve_variable(removed, "MEDI")$label,
      "Local display label"
    ))
    expect_identical(
      metadata_resolve_variable(edited, "MEDI")$label,
      "Local display label"
    )
    reset <- reset_dataset_record(copy)
    expect_identical(reset$metadata_bundle, copy$metadata_bundle)
    recovered <- unserialize(serialize(reset, NULL, version = 3L))
    expect_identical(recovered, reset)
    expect_identical(
      metadata_resolve_variable(recovered, "MEDI")$label,
      "Local display label"
    )
  }
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
