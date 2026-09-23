overlay_test_record <- function() {
  make_source <- function(id, sex, unit)
    new_metadata_source_namespace(
      id,
      paste("Source", id),
      "test",
      records = list(
        core_metadata = list(
          participants = data.frame(
            participant_internal_id = c("P1", "P2"),
            participant_sex = sex
          ),
          devices = data.frame(
            device_internal_id = "device",
            device_manufacturer = "Maker",
            device_model = "Logger"
          )
        ),
        dataset_inventory = data.frame(
          dataset_id = c("D1", "D2"),
          timezone = "UTC",
          latitude = c(48, 49),
          longitude = c(11, 12)
        ),
        variable_inventory = data.frame(
          dataset_id = c("D1", "D2", "D1"),
          file_group_id = c("D1:1", "D2:1", "D1:1"),
          name = c("MEDI", "MEDI", "Id"),
          label = c("Melanopic EDI", "Melanopic EDI", "Participant ID"),
          unit = c(unit, unit, NA_character_),
          type = c("numeric", "numeric", "string")
        )
      )
    )
  sources <- list(
    a = make_source("a", c("female", "male"), "lx"),
    b = make_source("b", c("not recorded", "intersex"), "custom unit")
  )
  new_dataset_record(
    data.frame(
      Id = c("P1", "P2"),
      Datetime = as.POSIXct(
        c("2026-01-01 12:00:00", "2026-01-01 12:01:00"),
        tz = "UTC"
      ),
      MEDI = c(1, 2)
    ),
    "Overlay test",
    new_source_manifest("test", source_timezone = "UTC"),
    metadata_bundle = new_metadata_bundle(sources),
    id = "dataset_overlaytest"
  )
}

overlay_target <- function(context, field, table = NULL, name = NULL) {
  rows <- context$catalog[context$catalog$field == field, ]
  if (!is.null(table)) rows <- rows[rows$table == table, ]
  if (!is.null(name)) rows <- rows[rows$name == name, ]
  rows$target_id[[1L]]
}

test_that("unit placeholders cannot enter batch or single assertions", {
  for (fixture in c(
    "tum-medi-only",
    "iztech-light-subset",
    "ucr-all-light-variables"
  )) {
    record <- m7_fixture_record(fixture)
    context <- metadata_overlay_context(record)
    id <- overlay_target(context, "unit", "variables", "MEDI")
    for (scope in c("all", "single")) {
      for (value in c(
        "not specified",
        " Unit NOT specified ",
        "not_specified",
        "not-recorded",
        "N/A",
        "unknown",
        "missing",
        "not applicable"
      )) {
        expect_error(
          metadata_overlay_preview(record, context, id, value, scope),
          "not a placeholder",
          info = paste(fixture, scope, value)
        )
      }
      patch <- metadata_overlay_preview(
        record,
        context,
        id,
        "photons / cm2 / s (custom)",
        scope
      )
      changed <- apply_metadata_overlay(record, context, patch)
      expect_identical(
        changed$metadata_bundle$sources,
        record$metadata_bundle$sources
      )
      expect_true(
        "photons / cm2 / s (custom)" %in%
          changed$metadata_bundle$overlay$tables$variables$unit
      )
      no_op <- metadata_overlay_preview(
        changed,
        context,
        id,
        "photons / cm2 / s (custom)",
        scope
      )
      expect_false(any(no_op$changed))
      remove <- metadata_overlay_preview(
        changed,
        context,
        id,
        scope = scope,
        action = "remove"
      )
      removed <- apply_metadata_overlay(changed, context, remove)
      expect_false(any(
        metadata_overlay_values(
          removed$metadata_bundle$overlay,
          remove$targets
        )$overridden
      ))
      undone <- undo_metadata_overlay(changed, context)
      expect_identical(
        undone$metadata_bundle$sources,
        record$metadata_bundle$sources
      )
    }
  }
})

test_that("effective presentation retains exact source facts and all/single occurrence distinctions", {
  records <- metadata_explorer_fixture_records(m7_fixture_directory())[1:4]
  for (record in records) {
    context <- metadata_overlay_context(record)
    entries <- context$source_index$entries
    groups <- unique(entries$group_id[
      entries$field == "dataset_file_variables_labels" &
        entries$group_context == "MEDI"
    ])
    id <- overlay_target(context, "label", "variables", "MEDI")
    for (scope in c("all", "single")) {
      changed <- apply_metadata_overlay(
        record,
        context,
        metadata_overlay_preview(record, context, id, "test", scope)
      )
      projected <- metadata_overlay_presentation(
        context,
        changed$metadata_bundle$overlay
      )
      expect_true(all(projected$entries$value == "test"))
      expected <- metadata_overlay_target_scope(context, id, scope)$target_id
      map <- context$navigation_targets
      mapped <- map$path[
        map$target_id %in% expected & map$path %in% entries$path
      ]
      expect_setequal(projected$entries$path, mapped)
      for (group_id in groups) {
        group <- metadata_index_group(context$source_index, group_id)
        html <- as.character(metadata_group_values_ui(group, projected))
        affected <- any(group$entries$path %in% mapped)
        expect_identical(grepl("Current value", html, fixed = TRUE), affected)
        if (affected) {
          expect_match(html, "Original source value", fixed = TRUE)
          expect_match(html, "test", fixed = TRUE)
          expect_match(html, group$entries$value[[1L]], fixed = TRUE)
          rows <- metadata_present_groups(group$group, projected)
          expect_true(rows$has_override[[1L]])
          expect_match(
            as.character(metadata_property_choice_ui(rows)),
            "Includes overlay",
            fixed = TRUE
          )
        }
      }
      source_entries <- changed$metadata_bundle$index$entries
      facts <- names(entries)[!startsWith(names(entries), "sort_rank_")]
      expect_identical(
        source_entries[source_entries$layer == "Immutable source", facts],
        entries[, facts]
      )
      expect_identical(
        changed$metadata_bundle$sources,
        record$metadata_bundle$sources
      )
      removed <- apply_metadata_overlay(
        changed,
        context,
        metadata_overlay_preview(
          changed,
          context,
          id,
          action = "remove",
          scope = scope
        )
      )
      expect_equal(
        nrow(
          metadata_overlay_presentation(
            context,
            removed$metadata_bundle$overlay
          )$entries
        ),
        0L
      )
      undone <- undo_metadata_overlay(changed, context)
      expect_equal(
        nrow(
          metadata_overlay_presentation(
            context,
            undone$metadata_bundle$overlay
          )$entries
        ),
        0L
      )
    }
  }
})

test_that("missing annotations cannot alias unrelated metadata or extend past eligible identities", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  missing <- metadata_overlay_missing_targets(context)
  rows <- context$catalog[context$catalog$target_id %in% missing, ]
  expect_setequal(
    unique(paste(rows$table, rows$field)),
    c("datasets title", "participants label", "sites site", "sites country")
  )
  target <- rows$target_id[[1L]]
  preview <- metadata_overlay_preview(
    record,
    context,
    target,
    "New annotation",
    eligible_ids = target
  )
  expect_identical(preview$target_ids, target)
  changed <- apply_metadata_overlay(record, context, preview)
  expect_identical(
    changed$metadata_bundle$sources,
    record$metadata_bundle$sources
  )
  expect_error(
    metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "sex"),
      "Wrong target",
      eligible_ids = missing
    ),
    "missing annotation target"
  )
  testthat::local_mocked_bindings(
    unserialize = function(...) stop("must not deserialize"),
    .package = "base"
  )
  expect_silent(metadata_overlay_missing_targets(context))
  expect_silent(metadata_overlay_presentation(
    context,
    changed$metadata_bundle$overlay
  ))
})

test_that("unresolved overlay fields explain their layer and unavailable binding", {
  record <- metadata_explorer_fixture_records(m7_fixture_directory())[[4L]]
  context <- metadata_overlay_context(record)
  changed <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "unit", "variables", "MEDI"),
      "klx",
      scope = "single"
    )
  )
  index <- changed$metadata_bundle$index
  rows <- index$groups[index$groups$layer == "LightLogWeb overlay", ]
  group <- metadata_index_group(
    index,
    rows$group_id[rows$field == "title"][[1L]]
  )
  selected <- metadata_overlay_explorer_selection(
    context,
    list(group = group),
    changed$metadata_bundle$overlay
  )
  expect_false(selected$editable)
  expect_match(selected$reason, "legacy overlay", fixed = TRUE)
  expect_match(selected$reason, "not applied", fixed = TRUE)
  expect_false(grepl(
    "source field is read-only",
    selected$reason,
    fixed = TRUE
  ))
})

test_that("a half-missing coordinate pair uses its existing contextual route", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  latitude <- context$catalog$source_path[context$catalog$field == "latitude"]
  context$source_index$entries <- context$source_index$entries[
    !context$source_index$entries$path %in% latitude,
  ]
  eligible <- metadata_overlay_missing_targets(context)
  expect_false(any(
    context$catalog$field[context$catalog$target_id %in% eligible] %in%
      c("latitude", "longitude")
  ))
})

test_that("explorer fields resolve through exact source paths and retained variable keys", {
  records <- metadata_explorer_fixture_records(m7_fixture_directory())
  for (record in records[1:4]) {
    context <- metadata_overlay_context(record)
    index <- record$metadata_bundle$index
    entries <- index$entries
    candidates <- entries[
      entries$field == "dataset_file_variables_units" &
        entries$group_context == "MEDI",
    ]
    expect_gt(nrow(candidates), 0L)
    group <- metadata_index_group(index, candidates$group_id[[1L]])
    selection <- metadata_overlay_explorer_selection(
      context,
      list(group = group),
      record$metadata_bundle$overlay
    )
    expect_true(selection$editable)
    expect_identical(selection$field, "variables/unit")
    expect_identical(selection$source_name, "MEDI")
    expect_equal(length(selection$target_ids), nrow(group$entries))
    targets <- context$catalog[
      match(selection$target_ids, context$catalog$target_id),
    ]
    expect_identical(unique(targets$name), "MEDI")
    expect_equal(length(unique(targets$source_scope)), 1L)
    # A sibling's exact parent path, not a label match, anchors single-item mode.
    path <- sub("/[^/]+$", "", tail(group$entries$path, 1L))
    anchored <- metadata_overlay_explorer_selection(
      context,
      list(group = group, focus_path = path),
      record$metadata_bundle$overlay
    )
    expect_identical(anchored$target_id, tail(selection$target_ids, 1L))
    source <- record$metadata_bundle$sources
    expect_identical(record$metadata_bundle$sources, source)
  }
})

test_that("unrelated units and conflicting alias values cannot be edited through navigation", {
  record <- m7_fixture_record("iztech-light-subset")
  context <- metadata_overlay_context(record)
  index <- record$metadata_bundle$index
  groups <- index$groups
  for (field in c(
    "participant_internal_id",
    "parameter_unit",
    "datasheet_channel_unit",
    "unit"
  )) {
    rows <- groups[groups$field == field & groups$resource == "core_metadata", ]
    expect_gt(nrow(rows), 0L)
    selection <- metadata_overlay_explorer_selection(
      context,
      list(group = metadata_index_group(index, rows$group_id[[1L]])),
      record$metadata_bundle$overlay
    )
    expect_false(selection$editable)
    expect_match(selection$reason, "read-only", fixed = TRUE)
  }
  alias <- which(
    index$entries$field == "dataset_file_variables_units" &
      index$entries$group_context == "MEDI"
  )[[1L]]
  index$entries$value[[alias]] <- "contradictory source unit"
  mapping <- metadata_overlay_navigation_targets(index, context$catalog)
  expect_false(index$entries$path[[alias]] %in% mapping$path)
})

test_that("overlay navigation resolves current qualified identities, including collisions and coordinates", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  changed <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "sex"),
      "An assertion"
    )
  )
  groups <- changed$metadata_bundle$index$groups
  group <- metadata_index_group(
    changed$metadata_bundle$index,
    groups$group_id[
      groups$field == "sex" &
        groups$namespace_id == metadata_overlay_namespace_id()
    ][[1L]]
  )
  selected <- metadata_overlay_explorer_selection(
    context,
    list(group = group),
    changed$metadata_bundle$overlay
  )
  expect_true(selected$editable)
  targets <- context$catalog[
    match(selected$target_ids, context$catalog$target_id),
  ]
  expect_equal(nrow(targets), 4L)
  expect_equal(length(unique(targets$source_scope)), 2L)
  groups <- record$metadata_bundle$index$groups
  group <- metadata_index_group(
    record$metadata_bundle$index,
    groups$group_id[groups$field == "longitude"][[1L]]
  )
  selected <- metadata_overlay_explorer_selection(
    context,
    list(group = group),
    record$metadata_bundle$overlay
  )
  expect_true(selected$editable)
  expect_identical(selected$field, "sites/latitude")
  expect_identical(selected$label, "Coordinates (latitude and longitude)")
})

test_that("version 2 qualifies colliding participant identities and keeps v1 readable", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  target <- overlay_target(context, "sex")
  preview <- metadata_overlay_preview(
    record,
    context,
    target,
    "user-supplied value"
  )
  expect_identical(preview$scope, "all")
  expect_equal(nrow(preview$targets), 4L)
  expect_length(unique(preview$targets$source_scope), 2L)
  changed <- apply_metadata_overlay(record, context, preview)
  expect_identical(changed$metadata_bundle$overlay$overlay_version, "2.0.0")
  expect_identical(record$metadata_bundle$overlay$overlay_version, "1.0.0")
  expect_equal(nrow(changed$metadata_bundle$overlay$tables$participants), 4L)
  expect_identical(changed$metadata_bundle$overlay$revision, 1L)
  expect_identical(changed$raw_payload, record$raw_payload)
  expect_identical(changed$prepared_payload, record$prepared_payload)
  expect_identical(
    changed$metadata_bundle$sources,
    record$metadata_bundle$sources
  )
  expect_identical(changed$analysis_settings, record$analysis_settings)
  expect_identical(changed$recipe, record$recipe)
  expect_silent(validate_dataset_record(changed))
  one <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(
      record,
      context,
      target,
      "different",
      scope = "single"
    )
  )
  expect_equal(nrow(one$metadata_bundle$overlay$tables$participants), 1L)
  exact <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(
      record,
      context,
      target,
      "  exact supplied text  ",
      scope = "single"
    )
  )
  expect_identical(
    exact$metadata_bundle$overlay$tables$participants$sex,
    "  exact supplied text  "
  )
})

test_that("batch units match the exact variable and single scope one occurrence", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  target <- overlay_target(context, "unit", name = "MEDI")
  preview <- metadata_overlay_preview(
    record,
    context,
    target,
    "new precise unit"
  )
  expect_equal(nrow(preview$targets), 4L)
  expect_true(all(preview$targets$name == "MEDI"))
  one <- metadata_overlay_preview(
    record,
    context,
    target,
    "new precise unit",
    scope = "single"
  )
  expect_equal(nrow(one$targets), 1L)
  expect_error(
    metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "unit", name = "Id"),
      "lx"
    ),
    "do not apply"
  )
  expect_error(
    metadata_overlay_preview(record, context, target, "Unknown"),
    "placeholder"
  )
  expect_error(
    metadata_overlay_preview(record, context, target, ""),
    "non-empty"
  )
})

test_that("atomic apply rejects altered and stale previews without partial changes", {
  record <- overlay_test_record()
  original <- record
  context <- metadata_overlay_context(record)
  preview <- metadata_overlay_preview(
    record,
    context,
    overlay_target(context, "sex"),
    "explicit assertion"
  )
  tampered <- preview
  tampered$target_ids <- rev(tampered$target_ids)
  expect_error(
    apply_metadata_overlay(record, context, tampered),
    "target or preview changed"
  )
  stale <- record
  stale$revision <- stale$revision + 1L
  expect_error(apply_metadata_overlay(stale, context, preview), "stale")
  expect_identical(record, original)
  changed <- apply_metadata_overlay(record, context, preview)
  expect_error(apply_metadata_overlay(changed, context, preview), "stale")
  expect_identical(changed$metadata_bundle$overlay$revision, 1L)
  other <- record
  other$id <- "dataset_other"
  expect_error(
    apply_metadata_overlay(other, context, preview),
    "source changed"
  )
})

test_that("no-op, remove and undo preserve versions and unrelated history", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  target <- overlay_target(context, "sex")
  no_op <- metadata_overlay_preview(
    record,
    context,
    target,
    "female",
    scope = "single"
  )
  expect_false(any(no_op$changed))
  expect_identical(apply_metadata_overlay(record, context, no_op), record)
  expect_error(
    undo_metadata_overlay(record, context),
    "no latest metadata edit"
  )
  changed <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(record, context, target, "assertion")
  )
  removed <- apply_metadata_overlay(
    changed,
    context,
    metadata_overlay_preview(changed, context, target, action = "remove")
  )
  expect_equal(nrow(removed$metadata_bundle$overlay$tables$participants), 0L)
  expect_identical(removed$metadata_bundle$overlay$revision, 2L)
  undone <- undo_metadata_overlay(removed, context)
  expect_equal(nrow(undone$metadata_bundle$overlay$tables$participants), 4L)
  expect_identical(undone$metadata_bundle$overlay$revision, 3L)
  expect_identical(undone$raw_payload, record$raw_payload)
  renamed <- rename_dataset_record(changed, "Another title")
  expect_error(
    undo_metadata_overlay(renamed, context),
    "Other changes are protected"
  )
})

test_that("paired coordinates validate every target before commit", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  target <- overlay_target(context, "latitude")
  expect_error(
    metadata_overlay_preview(record, context, target, 45),
    "paired finite"
  )
  expect_error(
    metadata_overlay_preview(
      record,
      context,
      target,
      c(latitude = 91, longitude = 20)
    ),
    "paired finite"
  )
  preview <- metadata_overlay_preview(
    record,
    context,
    target,
    c(latitude = 50, longitude = 10),
    scope = "single"
  )
  expect_equal(nrow(preview$targets), 2L)
  result <- apply_metadata_overlay(record, context, preview)
  expect_equal(result$metadata_bundle$overlay$tables$sites$latitude, 50)
  expect_equal(result$metadata_bundle$overlay$tables$sites$longitude, 10)
  expect_error(
    metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "source_timezone"),
      "not/a/zone"
    ),
    "IANA"
  )
})

test_that("ambiguous legacy values are preserved rather than fanned out", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  tables <- metadata_empty_overlay_tables()
  tables$participants <- data.frame(
    participant_id = "P1",
    label = "Old ambiguous label"
  )
  old <- new_metadata_overlay(
    record$metadata_bundle$source_fingerprint,
    tables = tables,
    revision = 2L,
    unknown = list(extra = c("", NA_character_))
  )
  upgraded <- metadata_overlay_upgrade(old, context$catalog)
  expect_equal(nrow(upgraded$tables$participants), 0L)
  expect_length(upgraded$unresolved, 1L)
  expect_identical(upgraded$unresolved[[1L]]$row, tables$participants)
  expect_identical(upgraded$unknown, old$unknown)
  expect_identical(upgraded$revision, 2L)
  record$metadata_bundle$overlay <- old
  target <- overlay_target(context, "label", "participants")
  expect_error(
    metadata_overlay_preview(record, context, target, "New label"),
    "no unique source"
  )
  preview <- metadata_overlay_preview(
    record,
    context,
    target,
    "New label",
    scope = "single",
    resolve_legacy = TRUE
  )
  expect_identical(preview$legacy[[1L]], "Old ambiguous label")
  result <- apply_metadata_overlay(record, context, preview)
  expect_equal(nrow(result$metadata_bundle$overlay$tables$participants), 1L)
  expect_identical(
    result$metadata_bundle$overlay$unresolved[[1L]]$row,
    tables$participants
  )
  expect_silent(metadata_overlay_preview(
    result,
    context,
    target,
    "Another label",
    scope = "single"
  ))
  expect_error(
    metadata_overlay_preview(result, context, target, "Another label"),
    "no unique source"
  )
})

test_that("session record recovery preserves typed overlays and unknown fields without exports", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  record <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "sex"),
      "@User, \"exact\""
    )
  )
  overlay <- record$metadata_bundle$overlay
  overlay$unknown <- list(
    empty = character(),
    null = NULL,
    logical = c(TRUE, NA, FALSE),
    numbers = c(0, -0, pi, NA_real_, NaN, Inf, -Inf),
    integer = c(1L, NA_integer_),
    text = c("", NA_character_, "Ü,\n\t\"\\", "=SUM(1+1)", "\001\b\f\r"),
    raw = as.raw(c(0, 255)),
    factor = factor(c("x", NA), levels = c("x", "y")),
    date = as.Date("2026-01-01"),
    time = as.POSIXct("2026-01-01", tz = "Europe/Berlin"),
    complex = complex(real = c(1, NA_real_, 3), imaginary = c(2, 4, NA_real_)),
    matrix = matrix(1:4, 2L, dimnames = list(c("a", "b"), c("x", "y")))
  )
  record$metadata_bundle <- new_metadata_bundle(
    record$metadata_bundle$sources,
    overlay = overlay
  )
  record <- validate_dataset_record(record)
  model <- session_add_dataset(new_session_model("hosted"), record)
  recovered <- session_dataset(
    unserialize(serialize(model, NULL, version = 3)),
    record$id
  )
  expect_identical(recovered$metadata_bundle$overlay, overlay)
  expect_identical(
    recovered$metadata_bundle$sources,
    record$metadata_bundle$sources
  )
  expect_identical(recovered$raw_payload, record$raw_payload)
})

test_that("metadata-absent imports provide only observed identities and retained facts", {
  record <- new_dataset_record(
    data.frame(
      Id = c("P1", "P2"),
      Datetime = as.POSIXct(c("2026-01-01", "2026-01-02"), tz = "UTC"),
      MEDI = 1:2
    ),
    "Optional metadata absent",
    new_source_manifest("test", source_timezone = "UTC")
  )
  context <- metadata_overlay_context(record)
  expect_setequal(
    context$catalog$participant_id[context$catalog$table == "participants"],
    c("P1", "P2")
  )
  expect_false(any(context$catalog$table == "devices"))
  expect_true(all(is.na(context$catalog$source_path)))
  result <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "sex"),
      "Explicit annotation"
    )
  )
  expect_silent(validate_dataset_record(result))
  factual <- record
  factual$factual_metadata <- list(
    variables = list(MEDI = list(label = "Melanopic EDI", unit = "lx"))
  )
  factual$metadata_bundle <- new_metadata_bundle_from_factual(
    factual$factual_metadata,
    factual$id,
    factual$display_name,
    "test"
  )
  actual <- metadata_overlay_context(factual)$catalog
  expect_identical(
    actual$source_value[[which(
      actual$table == "variables" & actual$field == "unit"
    )]],
    "lx"
  )
})

test_that("session overlay writes are scoped and reads never decode measurements", {
  record <- overlay_test_record()
  other <- overlay_test_record()
  other$id <- "dataset_otheroverlay"
  other$display_name <- "Other library dataset"
  store <- new_session_store("local")
  store$dispatch(new_session_event("add", value = record))
  store$dispatch(new_session_event("add", value = other))
  store$dispatch(new_session_event("select", dataset_id = record$id))
  context <- metadata_overlay_context(record)
  preview <- metadata_overlay_preview(
    record,
    context,
    overlay_target(context, "sex"),
    "Chosen value"
  )
  testthat::local_mocked_bindings(
    unserialize = function(...) stop("measurement decode"),
    .package = "base"
  )
  changed <- store$apply_overlay(context, preview)
  expect_identical(shiny::isolate(store$selected_dataset()), changed)
  expect_identical(shiny::isolate(store$datasets())[[other$id]], other)
  expect_identical(store$revision(record$id), changed$revision)
  undone <- store$apply_overlay(context, undo = TRUE)
  expect_identical(undone$raw_payload, record$raw_payload)
  store$select_dataset(other$id)
  expect_identical(shiny::isolate(store$selected_dataset()), other)
  expect_error(
    store$apply_overlay(context, preview),
    "selected dataset changed"
  )
  expect_error(
    store$select_dataset("not-a-dataset"),
    "available library dataset"
  )
})

test_that("incremental overlay changes retain exact index paths, identities and sort order", {
  for (field in c("sex", "unit", "site", "model")) {
    record <- overlay_test_record()
    context <- metadata_overlay_context(record)
    target <- overlay_target(context, field)
    populated <- apply_metadata_overlay(
      record,
      context,
      metadata_overlay_preview(record, context, target, "First assertion")
    )
    changed <- apply_metadata_overlay(
      populated,
      context,
      metadata_overlay_preview(
        populated,
        context,
        target,
        "Second assertion",
        "single"
      )
    )
    removed <- apply_metadata_overlay(
      changed,
      context,
      metadata_overlay_preview(
        changed,
        context,
        target,
        action = "remove",
        scope = "single"
      )
    )
    restored <- undo_metadata_overlay(removed, context)
    expect_identical(
      restored$metadata_bundle$overlay$tables,
      changed$metadata_bundle$overlay$tables
    )
    expect_identical(restored$revision, removed$revision + 1L)
    expect_identical(
      restored$metadata_bundle$overlay$revision,
      removed$metadata_bundle$overlay$revision + 1L
    )
    expect_identical(restored$raw_payload, removed$raw_payload)
    expect_identical(
      restored$metadata_bundle$sources,
      record$metadata_bundle$sources
    )
    for (result in list(changed, removed, restored)) {
      actual <- result$metadata_bundle$index
      rebuilt <- metadata_overlay_patch_index(
        context,
        result$metadata_bundle$overlay
      )
      for (column in c("path", "field", "value", "source")) {
        rank <- paste0("sort_rank_", column)
        expect_identical(
          actual$entries$entry_id[order(actual$entries[[rank]])],
          rebuilt$entries$entry_id[order(rebuilt$entries[[rank]])]
        )
      }
      for (component in c("entries", "nodes", "groups")) {
        key <- c(entries = "entry_id", nodes = "node_id", groups = "group_id")[[
          component
        ]]
        fields <- setdiff(
          names(actual[[component]]),
          c(
            "sort_rank_path",
            "sort_rank_field",
            "sort_rank_value",
            "sort_rank_source",
            "parent_row"
          )
        )
        left <- actual[[component]][
          order(actual[[component]][[key]]),
          fields,
          drop = FALSE
        ]
        right <- rebuilt[[component]][
          order(rebuilt[[component]][[key]]),
          fields,
          drop = FALSE
        ]
        rownames(left) <- rownames(right) <- NULL
        expect_identical(left, right)
      }
    }
  }
})

test_that("cached overlay row identities remain table- and source-qualified", {
  context <- metadata_overlay_context(overlay_test_record())
  contract <- metadata_overlay_key_contract(metadata_overlay_version())
  for (name in names(contract)) {
    rows <- context$catalog[context$catalog$table == name, ]
    keys <- metadata_overlay_row_key(rows, contract[[name]])
    expected <- rows$target_id[match(keys, keys)]
    expect_identical(
      metadata_overlay_row_targets(context, rows, name, contract[[name]]),
      expected
    )
  }
  entries <- context$source_index$entries
  positions <- seq.int(1L, nrow(entries), by = 3L)
  expect_identical(
    metadata_index_groups(entries, positions),
    metadata_index_groups(entries[positions, , drop = FALSE])
  )
  expect_identical(
    metadata_index_groups(entries, integer()),
    metadata_index_groups(entries[FALSE, , drop = FALSE])
  )
  keys <- c("source_scope", "participant_id")
  before <- data.frame(
    source_scope = c("A", "A", "B"),
    participant_id = c("P1", "P2", "P1")
  )
  expect_identical(metadata_overlay_match_rows(before, before, keys), 1:3)
  expect_identical(
    metadata_overlay_match_rows(before[c(3L, 1L), ], before, keys),
    c(3L, 1L)
  )
  invalid <- before[2L, ]
  invalid$source_scope <- "B"
  expect_identical(
    metadata_overlay_match_rows(invalid, before, keys),
    NA_integer_
  )
  expect_identical(
    metadata_overlay_match_rows(before[FALSE, ], before, keys),
    integer()
  )
})

test_that("compacted overlay paths preserve lexical ordering across row 10", {
  ids <- paste0("D", seq_len(14L))
  source <- new_metadata_source_namespace(
    "path-order",
    "Path ordering regression",
    "test",
    records = list(
      variable_inventory = data.frame(
        dataset_id = ids,
        file_group_id = paste0(ids, ":1"),
        name = "MEDI",
        label = "Melanopic EDI",
        unit = "lx",
        type = "numeric"
      )
    )
  )
  record <- new_dataset_record(
    data.frame(
      Id = ids,
      Datetime = as.POSIXct("2026-01-01", tz = "UTC"),
      MEDI = 1
    ),
    "Path ordering regression",
    new_source_manifest("test", source_timezone = "UTC"),
    metadata_bundle = new_metadata_bundle(list(`path-order` = source))
  )
  context <- metadata_overlay_context(record)
  targets <- context$catalog$target_id[context$catalog$field == "unit"]
  populated <- apply_metadata_overlay(
    record,
    context,
    metadata_overlay_preview(record, context, targets[[1L]], "Custom unit")
  )
  for (position in c(1L, 10L, 14L)) {
    patch <- metadata_overlay_preview(
      populated,
      context,
      targets[[position]],
      scope = "single",
      action = "remove"
    )
    removed <- apply_metadata_overlay(populated, context, patch)
    rebuilt <- metadata_overlay_patch_index(
      context,
      removed$metadata_bundle$overlay
    )
    entries <- removed$metadata_bundle$index$entries
    expect_identical(
      entries$entry_id[order(entries$sort_rank_path)],
      rebuilt$entries$entry_id[order(rebuilt$entries$sort_rank_path)]
    )
    expect_identical(entries$path, rebuilt$entries$path)
    fallback <- context
    fallback$overlay_paths_disjoint <- FALSE
    fallback$overlay_labels_disjoint <- FALSE
    conservative <- apply_metadata_overlay(
      populated,
      fallback,
      patch
    )$metadata_bundle$index
    expect_identical(
      entries$entry_id[order(entries$sort_rank_path)],
      conservative$entries$entry_id[order(conservative$entries$sort_rank_path)]
    )
    expect_identical(
      entries$entry_id[order(entries$sort_rank_source)],
      conservative$entries$entry_id[order(
        conservative$entries$sort_rank_source
      )]
    )
  }
})

test_that("overlay edits use no measurement deserialization and retain source index IDs", {
  record <- overlay_test_record()
  context <- metadata_overlay_context(record)
  preview <- metadata_overlay_preview(
    record,
    context,
    overlay_target(context, "sex"),
    "chosen value"
  )
  testthat::local_mocked_bindings(
    unserialize = function(...) stop("measurement decode"),
    .package = "base"
  )
  changed <- apply_metadata_overlay(record, context, preview)
  undone <- undo_metadata_overlay(changed, context)
  expect_identical(changed$raw_payload, undone$raw_payload)
  index <- changed$metadata_bundle$index
  original <- context$source_index
  keep <- index$entries$layer == "Immutable source"
  expect_identical(index$entries$entry_id[keep], original$entries$entry_id)
  expect_identical(index$entries$path[keep], original$entries$path)
  expect_false(anyDuplicated(index$entries$entry_id) > 0L)
  expect_false(anyDuplicated(index$nodes$node_id) > 0L)
})

test_that("public offline fixtures keep five participants and the two-device case", {
  skip_if_not_installed("jsonlite")
  directory <- m7_fixture_directory()
  skip_if_not(file.exists(file.path(directory, "iztech-light-subset.rds")))
  for (fixture in c(
    "tum-medi-only",
    "iztech-light-subset",
    "ucr-all-light-variables"
  )) {
    record <- m7_fixture_record(fixture)
    context <- metadata_overlay_context(record)
    preview <- metadata_overlay_preview(
      record,
      context,
      overlay_target(context, "sex"),
      "User annotation"
    )
    expect_equal(nrow(preview$targets), 5L, info = fixture)
    changed <- apply_metadata_overlay(record, context, preview)
    expect_identical(changed$raw_payload, record$raw_payload, info = fixture)
    expect_identical(
      changed$metadata_bundle$sources,
      record$metadata_bundle$sources,
      info = fixture
    )
    if (fixture == "iztech-light-subset") {
      units <- metadata_overlay_preview(
        record,
        context,
        overlay_target(context, "unit", name = "MEDI"),
        "explicit custom unit"
      )
      expect_equal(nrow(units$targets), 6L)
      expect_equal(
        sum(grepl("S004", units$targets$entity_id, fixed = TRUE)),
        2L
      )
    }
  }
})
