test_that("Dashboard quick dialog is atomic, recoverable and never writes on open or Escape", {
  record <- m7_fixture_record("iztech-light-subset")
  current <- shiny::reactiveVal(record)
  commit <- function(context, preview = NULL, undo = FALSE) {
    value <- shiny::isolate(current())
    value <- if (undo) undo_metadata_overlay(value, context) else
      apply_metadata_overlay(value, context, preview)
    current(value)
    value
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(dataset = current, apply_callback = commit),
    {
      session$flushReact()
      returned <- session$getReturned()
      returned$open_focus(record$id, "MEDI")
      session$flushReact()
      expect_identical(current(), record)
      expect_true(returned$status()$editing)
      expect_identical(focus_draft()$state$fields$unit$value, "lx")
      session$setInputs(
        focus_label = "Changed label",
        focus_unit = "unit not specified",
        focus_save = 1L
      )
      expect_identical(current(), record)
      expect_true(returned$status()$editing)
      expect_match(returned$status()$error, "unit", ignore.case = TRUE)
      session$setInputs(focus_unit = "klx", focus_save = 2L)
      expect_false(returned$status()$editing)
      expect_identical(current()$metadata_bundle$overlay$revision, 1L)
      expect_length(current()$history, 1L)
      saved <- current()
      session$setInputs(focus_save = 3L)
      expect_identical(current(), saved)
      returned$open_focus(record$id, "MEDI")
      expect_identical(focus_draft()$state$fields$label$value, "Changed label")
      session$setInputs(
        focus_label = "Not saved",
        focus_escape = list(dirty = TRUE, tick = 1)
      )
      expect_false(returned$status()$editing)
      expect_identical(current(), saved)
      returned$open_focus(record$id, "MEDI")
      session$setInputs(focus_discard = 1L)
      expect_identical(current(), saved)
      returned$open_focus(record$id, "MEDI")
      stale <- saved
      stale$revision <- stale$revision + 1L
      current(stale)
      session$setInputs(focus_save = 4L)
      expect_identical(current(), stale)
      expect_match(returned$status()$error, "stale")
      expect_identical(input$focus_label, "Not saved")
      session$setInputs(focus_discard = 2L, undo = 1L)
      expect_equal(nrow(current()$metadata_bundle$overlay$tables$variables), 0L)
      expect_identical(current()$raw_payload, record$raw_payload)
    }
  )
})

test_that("quick editor refreshes cached identities after Prepare creates a variable", {
  source <- m8_record()
  current <- shiny::reactiveVal(source)
  commit <- function(context, preview = NULL, undo = FALSE) {
    value <- shiny::isolate(current())
    value <- if (undo) undo_metadata_overlay(value, context) else
      apply_metadata_overlay(value, context, preview)
    current(value)
    value
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(dataset = current, apply_callback = commit),
    {
      returned <- session$getReturned()
      session$flushReact()
      returned$open_focus(source$id, "MEDI")
      expect_true(returned$status()$editing)
      session$setInputs(focus_discard = 1L)
      creator <- new_preparation_step(
        "calibrate",
        list(
          variable = "MEDI",
          multiplier = 2,
          offset = 0,
          reference = "",
          output = "MEDI_processed"
        ),
        m8_context(source)
      )
      applied <- m8_preview(source, new_recipe(list(creator)))$candidate
      current(applied)
      session$flushReact()
      returned$open_focus(source$id, "MEDI_processed")
      expect_true(returned$status()$editing)
      expect_identical(focus_draft()$variable, "MEDI_processed")
      expect_length(focus_draft()$context$preparation_identities, 1L)
      session$setInputs(
        focus_label = "Processed signal",
        focus_unit = "lx",
        focus_save = 1L
      )
      expect_false(returned$status()$editing)
      expect_identical(
        metadata_resolve_variable(current(), "MEDI_processed")$label,
        "Processed signal"
      )
      expect_identical(current()$prepared_payload, applied$prepared_payload)
      expect_identical(
        metadata_resolve_variable(current(), "MEDI")$label,
        "MEDI"
      )
      returned$open_focus(source$id, "MEDI_processed")
      expect_identical(
        focus_draft()$state$fields$label$value,
        "Processed signal"
      )
      session$setInputs(focus_discard = 2L)
    }
  )
})

test_that("overlay module previews batch defaults and applies only reviewed inputs", {
  record <- m7_fixture_record("iztech-light-subset")
  current <- shiny::reactiveVal(record)
  context <- metadata_overlay_context(record)
  target <- context$catalog$target_id[context$catalog$field == "sex"][[1L]]
  commit <- function(context, preview = NULL, undo = FALSE) {
    value <- shiny::isolate(current())
    value <- if (undo) undo_metadata_overlay(value, context) else
      apply_metadata_overlay(value, context, preview)
    current(value)
    value
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(dataset = current, apply_callback = commit),
    {
      session$flushReact()
      expect_null(output$undo_action)
      session$setInputs(edit = 1L)
      session$setInputs(
        field = "participants/sex",
        scope = "all",
        matching_id = target,
        target_id = "all",
        operation = "set",
        text_value = "A user assertion",
        resolve_legacy = FALSE
      )
      session$setInputs(review = 1L)
      expect_match(
        output$value_input$html,
        'data-update-on="blur"',
        fixed = TRUE
      )
      expect_equal(nrow(session$getReturned()$preview()$targets), 5L)
      expect_true(session$getReturned()$status()$preview_ready)
      session$setInputs(text_value = "Changed after review")
      expect_null(session$getReturned()$preview())
      session$setInputs(apply = 1L)
      expect_match(session$getReturned()$status()$error, "Review changes again")
      expect_identical(current()$metadata_bundle$overlay$revision, 0L)
      session$setInputs(review = 2L, apply = 2L)
      expect_identical(current()$metadata_bundle$overlay$revision, 1L)
      expect_false(session$getReturned()$status()$editing)
      expect_match(
        output$undo_action$html,
        "Undo latest metadata edit",
        fixed = TRUE
      )
      expect_equal(
        nrow(current()$metadata_bundle$overlay$tables$participants),
        5L
      )
      session$setInputs(apply = 3L)
      expect_identical(current()$metadata_bundle$overlay$revision, 1L)
      session$setInputs(undo = 1L)
      expect_identical(current()$metadata_bundle$overlay$revision, 2L)
      expect_equal(
        nrow(current()$metadata_bundle$overlay$tables$participants),
        0L
      )
      expect_null(output$undo_action)
      expect_match(
        output$notice$html,
        "Latest metadata edit undone",
        fixed = TRUE
      )
      expect_identical(
        current()$metadata_bundle$sources,
        record$metadata_bundle$sources
      )
      expect_identical(current()$raw_payload, record$raw_payload)
    }
  )
})

test_that("single-item drafts survive errors and stale revisions without retargeting", {
  record <- m7_fixture_record("iztech-light-subset")
  current <- shiny::reactiveVal(record)
  context <- metadata_overlay_context(record)
  target <- context$catalog$target_id[context$catalog$field == "sex"][[1L]]
  commit <- function(context, preview = NULL, undo = FALSE) {
    value <- apply_metadata_overlay(shiny::isolate(current()), context, preview)
    current(value)
    value
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(dataset = current, apply_callback = commit),
    {
      session$flushReact()
      session$setInputs(edit = 1L)
      session$setInputs(
        field = "participants/sex",
        scope = "single",
        matching_id = target,
        target_id = target,
        operation = "set",
        text_value = "",
        resolve_legacy = FALSE
      )
      session$setInputs(review = 1L)
      expect_match(session$getReturned()$status()$error, "non-empty")
      expect_true(session$getReturned()$status()$editing)
      session$setInputs(text_value = "Exactly one participant", review = 2L)
      expect_equal(nrow(session$getReturned()$preview()$targets), 1L)
      external <- current()
      external$revision <- external$revision + 1L
      current(external)
      session$flushReact()
      session$setInputs(apply = 1L)
      expect_match(session$getReturned()$status()$error, "stale")
      session$setInputs(refresh = 1L)
      expect_identical(input$text_value, "Exactly one participant")
      session$setInputs(review = 3L, apply = 2L)
      expect_equal(
        nrow(current()$metadata_bundle$overlay$tables$participants),
        1L
      )
      session$setInputs(edit = 2L)
      switched <- current()
      switched$id <- "dataset_otherselection"
      current(switched)
      session$flushReact()
      session$setInputs(review = 4L)
      expect_match(
        session$getReturned()$status()$error,
        "selected dataset changed"
      )
      session$setInputs(discard = 1L)
      expect_false(session$getReturned()$status()$editing)
      expect_identical(current()$id, "dataset_otherselection")
    }
  )
})

test_that("source variables and exact items remain separate and invalidate previews", {
  record <- m7_fixture_record("iztech-light-subset")
  context <- metadata_overlay_context(record)
  units <- metadata_overlay_editor_targets(context, "variables/unit")
  medi <- units[units$name == "MEDI", ]
  light <- units[units$name == "LIGHT", ]
  current <- shiny::reactiveVal(record)
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = function(...) stop("must not commit")
    ),
    {
      session$flushReact()
      session$setInputs(edit = 1L)
      session$setInputs(
        field = "variables/unit",
        matching_id = medi$target_id[[1L]],
        scope = "all",
        target_id = "all",
        operation = "set",
        text_value = "lx"
      )
      expect_length(item_choices(), 1L)
      expect_match(
        names(item_choices()),
        "All 6 variable occurrences",
        fixed = TRUE
      )
      for (id in medi$entity_id)
        expect_match(names(item_choices()), id, fixed = TRUE)
      expect_identical(unique(targets()$name), "MEDI")
      expect_equal(nrow(current_values()), 6L)
      expect_match(output$current_values$html, "Retained source", fixed = TRUE)
      session$setInputs(review = 1L)
      expect_equal(nrow(session$getReturned()$preview()$targets), 6L)
      session$setInputs(scope = "single")
      expect_null(session$getReturned()$preview())
      expect_equal(length(item_choices()), 6L)
      expect_identical(input$matching_id, medi$target_id[[1L]])
      session$setInputs(target_id = medi$target_id[[4L]], review = 2L)
      expect_identical(
        session$getReturned()$preview()$targets$target_id,
        medi$target_id[[4L]]
      )
      session$setInputs(target_id = medi$target_id[[5L]])
      expect_null(session$getReturned()$preview())
      expect_identical(current_values()$Item, medi$entity_id[[5L]])
      session$setInputs(review = 3L)
      session$setInputs(matching_id = light$target_id[[1L]])
      expect_null(session$getReturned()$preview())
      expect_identical(unname(item_choices()), light$target_id)
      expect_error(selected_anchor(), class = "shiny.silent.error")
      session$setInputs(apply = 1L)
      expect_match(session$getReturned()$status()$error, "Review changes again")
      expect_identical(current()$metadata_bundle$overlay$revision, 0L)
      session$setInputs(target_id = light$target_id[[2L]], review = 4L)
      expect_identical(
        session$getReturned()$preview()$targets$target_id,
        light$target_id[[2L]]
      )
      session$setInputs(scope = "all", target_id = "all")
      expect_null(session$getReturned()$preview())
      expect_equal(nrow(targets()), 6L)
      expect_identical(unique(targets()$name), "LIGHT")
      session$setInputs(target_id = "", review = 5L)
      expect_null(session$getReturned()$preview())
      expect_match(
        session$getReturned()$status()$error,
        "Choose a metadata field and an available item",
        fixed = TRUE
      )
      expect_identical(input$text_value, "lx")
    }
  )
})

test_that("only variable metadata needs an additional source-variable choice", {
  record <- m7_fixture_record("iztech-light-subset")
  modals <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendModal <- function(type, message) {
    if (identical(type, "show")) modals[[length(modals) + 1L]] <<- message
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = shiny::reactive(record),
      apply_callback = function(...) stop("must not commit")
    ),
    session = mock_session,
    {
      session$flushReact()
      session$setInputs(add_missing = 1L, field = "sites/country")
      html <- tail(modals, 1L)[[1L]]$html
      expect_match(html, "Source variable", fixed = TRUE)
      expect_false(grepl("Matching field", html, fixed = TRUE))
      expect_match(html, "data-display-if=", fixed = TRUE)
      expect_match(html, "input.field", fixed = TRUE)
      expect_match(html, "variables/label", fixed = TRUE)
      expect_match(html, "variables/unit", fixed = TRUE)
      expect_match(html, "data-ns-prefix=", fixed = TRUE)
      expect_equal(nrow(matching_rows()), 1L)
      anchor <- matching_rows()$target_id[[1L]]
      session$setInputs(
        matching_id = anchor,
        scope = "all",
        target_id = "all"
      )
      expect_equal(nrow(targets()), 5L)
      expect_true(all(targets()$field == "country"))
      expect_length(item_choices(), 1L)
      session$setInputs(scope = "single")
      expect_length(item_choices(), 5L)
      session$setInputs(target_id = unname(item_choices()[[2L]]))
      expect_equal(nrow(targets()), 1L)
      session$setInputs(discard = 1L)
      session$setInputs(edit = 1L, field = "variables/unit")
      expect_true(all(matching_rows()$table == "variables"))
      expect_true(all(c("MEDI", "LIGHT") %in% matching_rows()$name))
      session$setInputs(field = "participants/sex")
      expect_equal(nrow(matching_rows()), 1L)
      expect_true(all(field_rows()$field == "sex"))
    }
  )
})

test_that("current-value information distinguishes mixed source and committed assertions", {
  record <- m7_fixture_record("iztech-light-subset")
  context <- metadata_overlay_context(record)
  participants <- metadata_overlay_editor_targets(context, "participants/sex")
  patch <- metadata_overlay_preview(
    record,
    context,
    participants$target_id[[2L]],
    "Owner assertion",
    scope = "single"
  )
  record <- apply_metadata_overlay(record, context, patch)
  table <- metadata_overlay_current_table(
    record$metadata_bundle$overlay,
    participants
  )
  expect_equal(nrow(table), 5L)
  expect_identical(table$Current[[2L]], "Owner assertion")
  expect_identical(
    table$Value_from,
    c("Source metadata", "Committed overlay", rep("Source metadata", 3L))
  )
  expect_false(identical(table$Source_value[[2L]], table$Current[[2L]]))
  expect_match(
    metadata_overlay_value_summary(table$Current),
    "Owner assertion (1)",
    fixed = TRUE
  )
  missing <- participants[1:2, ]
  missing$source_value <- list(NA_character_, "")
  empty_overlay <- metadata_overlay_upgrade(
    new_metadata_overlay(source_fingerprint = context$source_fingerprint),
    context$catalog
  )
  expect_identical(
    metadata_overlay_current_table(empty_overlay, missing)$Current,
    c("Not specified", "Empty text")
  )
  missing$field <- "unit"
  expect_identical(
    metadata_overlay_current_table(empty_overlay, missing)$Current[[1L]],
    "Unit not specified"
  )

  current <- shiny::reactiveVal(record)
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = function(...) stop("not used")
    ),
    {
      session$flushReact()
      session$setInputs(
        edit = 1L,
        field = "participants/sex",
        matching_id = participants$target_id[[1L]],
        scope = "all",
        target_id = "all"
      )
      expect_match(
        output$current_values$html,
        "Owner assertion (1)",
        fixed = TRUE
      )
      expect_match(
        output$current_values$html,
        "1 committed field override(s)",
        fixed = TRUE
      )
      expect_match(
        output$current_values$html,
        "Committed overlay revision 1",
        fixed = TRUE
      )
      session$setInputs(text_value = "Unapplied value")
      expect_false(grepl(
        "Unapplied value",
        output$current_values$html,
        fixed = TRUE
      ))
      external <- apply_metadata_overlay(
        current(),
        context,
        metadata_overlay_preview(
          current(),
          context,
          participants$target_id[[3L]],
          "External assertion",
          scope = "single"
        )
      )
      current(external)
      session$flushReact()
      expect_false("External assertion" %in% current_values()$Current)
      session$setInputs(refresh = 1L)
      expect_true("External assertion" %in% current_values()$Current)
      expect_identical(input$text_value, "Unapplied value")
    }
  )
})

test_that("item choices cover devices, coordinates, appended collisions and wide inventories", {
  record <- m7_fixture_record("iztech-light-subset")
  context <- metadata_overlay_context(record)
  devices <- metadata_overlay_editor_targets(context, "devices/model")
  choices <- metadata_overlay_item_choices(
    devices,
    "single",
    unique(context$catalog$source_scope)
  )
  expect_identical(unname(choices), devices$target_id)
  expect_true(all(c("IZTECH_AL03", "IZTECH_AL04") %in% names(choices)))
  colliding <- devices[1:2, ]
  colliding$entity_id <- "same-device-id"
  colliding$source_scope <- c("source-a", "source-b")
  choices <- metadata_overlay_item_choices(
    colliding,
    "single",
    colliding$source_scope
  )
  expect_length(unique(names(choices)), 2L)
  expect_match(names(choices)[[1L]], "Source 1", fixed = TRUE)
  expect_match(names(choices)[[2L]], "Source 2", fixed = TRUE)
  empty_overlay <- metadata_overlay_upgrade(
    record$metadata_bundle$overlay,
    context$catalog
  )
  table <- metadata_overlay_current_table(
    empty_overlay,
    colliding[2L, ],
    colliding$source_scope
  )
  expect_match(table$Source, "Source 2:", fixed = TRUE)
  wide <- devices[rep(1L, 5000L), ]
  wide$target_id <- paste0("target-", seq_len(nrow(wide)))
  wide$entity_id <- paste0("device-", seq_len(nrow(wide)))
  choices <- metadata_overlay_item_choices(
    wide,
    "all",
    unique(wide$source_scope)
  )
  expect_identical(unname(choices), "all")
  expect_match(names(choices), "All 5000 devices", fixed = TRUE)
  expect_match(names(choices), "4980 more", fixed = TRUE)
  expect_lt(nchar(names(choices)), 500L)
  expect_length(
    metadata_overlay_item_choices(wide, "single", unique(wide$source_scope)),
    5000L
  )

  coordinates <- metadata_overlay_editor_targets(context, "sites/latitude")
  current <- shiny::reactiveVal(record)
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = function(...) stop("not used")
    ),
    {
      session$flushReact()
      session$setInputs(
        edit = 1L,
        field = "sites/latitude",
        matching_id = coordinates$target_id[[1L]],
        scope = "single",
        target_id = coordinates$target_id[[2L]]
      )
      expect_equal(nrow(current_values()), 2L)
      expect_setequal(current_values()$Field, c("Latitude", "Longitude"))
      expect_equal(length(unique(current_values()$Item)), 1L)
    }
  )
})

test_that("the overlay module has no standalone export state or handlers", {
  record <- m7_fixture_record("tum-medi-only")
  current <- shiny::reactiveVal(record)
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = function(...) stop("not used")
    ),
    {
      session$flushReact()
      before <- current()
      session$setInputs(prepare_export = 1L)
      expect_false("export_ready" %in% names(session$getReturned()$status()))
      expect_identical(session$getReturned()$status()$last_action, "Ready")
      expect_identical(current(), before)
    }
  )
  server_source <- paste(
    deparse(body(metadata_overlay_server)),
    collapse = "\n"
  )
  expect_false(grepl(
    "downloadHandler|prepare_export|prepared_export",
    server_source
  ))
  for (helper in c(
    "metadata_overlay_export_json",
    "metadata_overlay_export_csv",
    "metadata_overlay_encode_value"
  )) {
    expect_false(exists(
      helper,
      envir = asNamespace("LightLogWeb"),
      inherits = FALSE
    ))
  }
})

test_that("overlay UI is namespaced, native and keeps scope explicit", {
  ui <- as.character(metadata_overlay_ui("example"))
  expect_match(ui, "example-edit", fixed = TRUE)
  expect_match(ui, "llw-overlay-edit", fixed = TRUE)
  dependencies <- htmltools::findDependencies(metadata_overlay_ui("example"))
  navigation <- Filter(
    function(x) x$name == "llw-metadata-navigation",
    dependencies
  )
  expect_length(navigation, 1L)
  expect_identical(navigation[[1L]]$version, "1.2.5")
  expect_match(ui, "example-undo_action", fixed = TRUE)
  expect_match(ui, "example-notice", fixed = TRUE)
  expect_false(grepl(
    "prepare_export|downloads|Export committed|Edit LightLogWeb metadata|bslib-card",
    ui
  ))
  expect_error(
    metadata_overlay_server("bad", list(), function(...) NULL),
    "requires a dataset reactive"
  )
  expect_s3_class(
    metadata_overlay_app(metadata_explorer_showcase_records()),
    "shiny.appobj"
  )
})

test_that("undo and feedback live in the existing LightLogWeb overlay card", {
  record <- m7_fixture_record("tum-medi-only")
  actions <- metadata_overlay_ui("overlay", contextual = TRUE)
  ui <- metadata_source_overlay_ui(
    record$metadata_bundle,
    editable = TRUE,
    overlay_actions = actions
  )
  overlay <- as.character(htmltools::tagQuery(ui)$find(
    ".llw-metadata-layer-card--overlay"
  )$selectedTags()[[1L]])
  source <- as.character(htmltools::tagQuery(ui)$find(
    ".llw-metadata-layer-card--source"
  )$selectedTags()[[1L]])
  expect_match(overlay, "LightLogWeb overlay", fixed = TRUE)
  expect_match(overlay, "overlay-undo_action", fixed = TRUE)
  expect_match(overlay, "overlay-notice", fixed = TRUE)
  expect_false(grepl("overlay-undo_action|overlay-notice", source))
  shiny::testServer(
    metadata_explorer_server,
    args = list(
      dataset = shiny::reactive(record),
      editable = TRUE,
      overlay_actions = actions
    ),
    {
      session$flushReact()
      expect_match(
        output$layer_summary$html,
        "overlay-undo_action",
        fixed = TRUE
      )
      expect_false(grepl(
        "Edit LightLogWeb metadata|Export committed",
        output$layer_summary$html
      ))
    }
  )
})

test_that("edit and missing metadata actions share the stable explorer toolbar", {
  ui <- metadata_explorer_ui(
    "metadata",
    field_actions = metadata_overlay_context_ui("overlay")
  )
  toolbar <- htmltools::tagQuery(ui)$find(
    ".llw-metadata-navigation-toolbar"
  )$selectedTags()
  expect_length(toolbar, 1L)
  html <- as.character(toolbar[[1L]])
  for (id in c(
    "metadata-hierarchy_up",
    "overlay-edit",
    "overlay-add_missing",
    "metadata-item_count"
  )) {
    expect_match(html, paste0('id="', id, '"'), fixed = TRUE)
  }
  expect_match(html, "Edit selected field", fixed = TRUE)
  expect_match(html, "Add missing metadata", fixed = TRUE)
  expect_match(html, "does not depend on the selected field", fixed = TRUE)
  expect_false(grepl(
    "selection_status|Add missing annotation",
    as.character(ui)
  ))
  expect_length(
    htmltools::tagQuery(ui)$find("#overlay-edit")$selectedTags(),
    1L
  )
  expect_length(
    htmltools::tagQuery(ui)$find("#overlay-add_missing")$selectedTags(),
    1L
  )
  # The authoring toolbar is mounted by the UI, not replaced by renderUI on
  # Apply, Undo or a metadata-absent dataset. Dialog focus can return to it.
  expect_false(grepl(
    'id="metadata-hierarchy_shell"',
    as.character(ui),
    fixed = TRUE
  ))
  expect_match(as.character(ui), 'id="metadata-entry_detail"', fixed = TRUE)
  shiny::testServer(
    metadata_explorer_server,
    args = list(
      dataset = shiny::reactive(
        metadata_explorer_showcase_records()[["dataset_m7absent"]]
      )
    ),
    {
      session$flushReact()
      expect_match(
        output$hierarchy_content$html,
        "No retained metadata fields",
        fixed = TRUE
      )
      expect_null(output$item_properties)
    }
  )
})

test_that("coordinate inputs flush the latest value before reviewing", {
  record <- m7_fixture_record("tum-medi-only")
  current <- shiny::reactiveVal(record)
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = function(...) stop("not used")
    ),
    {
      session$flushReact()
      session$setInputs(edit = 1L, field = "sites/latitude")
      expect_equal(
        lengths(regmatches(
          output$value_input$html,
          gregexpr(
            'data-update-on="blur"',
            output$value_input$html,
            fixed = TRUE
          )
        )),
        2L
      )
    }
  )
})

test_that("contextual editing fixes the selected field and protects it from retargeting", {
  record <- m7_fixture_record("iztech-light-subset")
  context <- metadata_overlay_context(record)
  index <- record$metadata_bundle$index
  entries <- index$entries
  id <- entries$group_id[
    entries$field == "dataset_file_variables_units" &
      entries$group_context == "MEDI"
  ][[1L]]
  group <- metadata_index_group(index, id)
  selected <- shiny::reactiveVal(list(
    group = group,
    focus_path = sub("/[^/]+$", "", group$entries$path[[5L]])
  ))
  current <- shiny::reactiveVal(record)
  commit <- function(context, preview = NULL, undo = FALSE) {
    value <- apply_metadata_overlay(shiny::isolate(current()), context, preview)
    current(value)
    value
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = commit,
      selection = selected
    ),
    {
      session$flushReact()
      expect_true(selected_field()$editable)
      expected <- selected_field()$target_id
      session$setInputs(
        edit = 1L,
        scope = "all",
        target_id = "all",
        operation = "set",
        text_value = "custom unit"
      )
      expect_identical(draft_field(), "variables/unit")
      session$setInputs(
        field = "participants/sex",
        matching_id = "spoofed",
        review = 1L
      )
      expect_identical(draft_field(), "variables/unit")
      expect_equal(nrow(session$getReturned()$preview()$targets), 6L)
      expect_identical(
        unique(session$getReturned()$preview()$targets$name),
        "MEDI"
      )
      session$setInputs(scope = "single", target_id = expected, review = 2L)
      expect_identical(
        session$getReturned()$preview()$targets$target_id,
        expected
      )
      selected(NULL)
      session$flushReact()
      expect_identical(draft_field(), "variables/unit")
      session$setInputs(apply = 1L)
      expect_equal(nrow(current()$metadata_bundle$overlay$tables$variables), 1L)
      expect_identical(
        current()$metadata_bundle$sources,
        record$metadata_bundle$sources
      )
      expect_identical(current()$raw_payload, record$raw_payload)
      session$setInputs(edit = 2L)
      expect_false(session$getReturned()$status()$editing)
      expect_match(
        selected_field()$reason,
        "Choose a metadata field",
        fixed = TRUE
      )
    }
  )
  html <- as.character(metadata_overlay_ui("overlay", contextual = TRUE))
  expect_false(grepl('id="overlay-edit"', html, fixed = TRUE))
  html <- as.character(metadata_overlay_context_ui("overlay"))
  expect_match(html, "Edit selected field", fixed = TRUE)
  expect_match(html, "disabled", fixed = TRUE)
})

test_that("Escape dismisses clean drafts and protects dirty drafts without applying", {
  current <- shiny::reactiveVal(m7_fixture_record("tum-medi-only"))
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = function(...) stop("must not apply")
    ),
    {
      session$flushReact()
      session$setInputs(edit = 1L)
      expect_true(session$getReturned()$status()$editing)
      session$setInputs(escape = list(dirty = FALSE))
      expect_false(session$getReturned()$status()$editing)
      session$setInputs(edit = 2L, text_value = "Uncommitted")
      session$setInputs(escape = list(dirty = TRUE))
      expect_true(session$getReturned()$status()$editing)
      expect_true(session$getReturned()$status()$confirming_discard)
      expect_match(output$close_actions$html, "Keep editing", fixed = TRUE)
      session$setInputs(keep_editing = 1L)
      expect_false(session$getReturned()$status()$confirming_discard)
      expect_identical(input$text_value, "Uncommitted")
      session$setInputs(escape = list(dirty = TRUE), confirm_discard = 1L)
      expect_false(session$getReturned()$status()$editing)
      expect_identical(current()$metadata_bundle$overlay$revision, 0L)
      session$setInputs(edit = 3L, escape = list(dirty = "invalid"))
      expect_true(session$getReturned()$status()$editing)
      expect_false(session$getReturned()$status()$confirming_discard)
      session$setInputs(discard = 1L)
      expect_false(session$getReturned()$status()$editing)
    }
  )
})

test_that("Review restores owned dialog focus for success, repeats and validation", {
  current <- shiny::reactiveVal(m7_fixture_record("iztech-light-subset"))
  context <- metadata_overlay_context(shiny::isolate(current()))
  units <- metadata_overlay_editor_targets(context, "variables/unit")
  target <- units$target_id[units$name == "MEDI"][[1L]]
  messages <- list()
  mock_session <- shiny::MockShinySession$new()
  mock_session$sendCustomMessage <- function(type, message) {
    if (identical(type, "llw-overlay-dialog-focus")) {
      messages[[length(messages) + 1L]] <<- message
    }
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = function(...) stop("Review and Escape must not apply")
    ),
    session = mock_session,
    {
      session$flushReact()
      session$setInputs(edit = 1L)
      session$setInputs(
        field = "variables/unit",
        scope = "single",
        matching_id = target,
        target_id = target,
        operation = "set",
        text_value = "photons cm^-2 s^-1"
      )
      expect_length(messages, 0L)
      for (review_number in 1:2) {
        session$setInputs(review = review_number)
        expect_true(session$getReturned()$status()$preview_ready)
        expect_length(messages, review_number)
        expect_identical(
          tail(messages, 1L)[[1L]],
          list(
            target = session$ns("draft"),
            keep = session$ns("keep_editing"),
            review = session$ns("review"),
            mode = "review"
          )
        )
      }
      reviewed <- session$getReturned()$preview()
      session$setInputs(escape = list(dirty = FALSE))
      expect_true(session$getReturned()$status()$confirming_discard)
      expect_identical(tail(messages, 1L)[[1L]]$mode, "confirm")
      session$setInputs(keep_editing = 1L)
      expect_false(session$getReturned()$status()$confirming_discard)
      expect_identical(tail(messages, 1L)[[1L]]$mode, "resume")
      expect_identical(session$getReturned()$preview(), reviewed)
      expect_identical(input$text_value, "photons cm^-2 s^-1")
      session$setInputs(text_value = "not specified", review = 3L)
      expect_null(session$getReturned()$preview())
      expect_match(session$getReturned()$status()$error, "placeholder")
      expect_identical(tail(messages, 1L)[[1L]]$mode, "review")
      expect_true(session$getReturned()$status()$editing)
      session$setInputs(text_value = "lx", review = 4L)
      expect_false(any(session$getReturned()$preview()$changed))
      expect_match(
        output$apply_action$html,
        "Confirm unchanged values",
        fixed = TRUE
      )
      expect_identical(tail(messages, 1L)[[1L]]$mode, "review")
      session$setInputs(escape = list(dirty = FALSE))
      expect_true(session$getReturned()$status()$confirming_discard)
      session$setInputs(confirm_discard = 1L)
      expect_false(session$getReturned()$status()$editing)
      expect_identical(current()$metadata_bundle$overlay$revision, 0L)
      expect_length(messages, 7L)
    }
  )
})

test_that("missing-metadata entry works without source fields and retains exact targets", {
  original <- metadata_explorer_showcase_records()[["dataset_m7absent"]]
  current <- shiny::reactiveVal(original)
  selected <- shiny::reactive(NULL)
  commit <- function(context, preview = NULL, undo = FALSE) {
    value <- apply_metadata_overlay(shiny::isolate(current()), context, preview)
    current(value)
    value
  }
  shiny::testServer(
    metadata_overlay_server,
    args = list(
      dataset = current,
      apply_callback = commit,
      selection = selected
    ),
    {
      session$flushReact()
      expect_match(
        selected_field()$reason,
        "Add missing metadata",
        fixed = TRUE
      )
      expect_gt(length(missing_targets()), 0L)
      session$setInputs(add_missing = 1L, field = "variables/unit")
      anchor <- field_rows()$target_id[[1L]]
      session$setInputs(
        matching_id = anchor,
        scope = "all",
        target_id = "all",
        operation = "set",
        text_value = "lx",
        review = 1L
      )
      expect_equal(nrow(session$getReturned()$preview()$targets), 1L)
      expect_match(
        output$current_values$html,
        "Unit not specified",
        fixed = TRUE
      )
      reviewed <- session$getReturned()$preview()
      session$setInputs(escape = list(dirty = FALSE))
      expect_true(session$getReturned()$status()$confirming_discard)
      expect_identical(current()$metadata_bundle$overlay$revision, 0L)
      session$setInputs(keep_editing = 1L)
      expect_identical(session$getReturned()$preview(), reviewed)
      expect_identical(input$text_value, "lx")
      session$setInputs(apply = 1L)
      expect_identical(
        current()$metadata_bundle$overlay$tables$variables$unit,
        "lx"
      )
      expect_identical(
        current()$metadata_bundle$sources,
        original$metadata_bundle$sources
      )
      expect_identical(current()$raw_payload, original$raw_payload)
      current(m7_fixture_record("tum-medi-only"))
      session$flushReact()
      session$setInputs(add_missing = 2L, field = "datasets/title")
      expect_true(all(field_rows()$table == "datasets"))
      expect_true(all(field_rows()$field == "title"))
      expect_false(anchor %in% field_rows()$target_id)
      session$setInputs(discard = 1L)
    }
  )
})
