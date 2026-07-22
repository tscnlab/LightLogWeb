new_source_manifest <- function(
  source_type,
  original_filenames = character(),
  hashes = character(),
  import_arguments = list(),
  source_timezone = NA_character_,
  imported_at = Sys.time(),
  details = list()
) {
  assert_scalar_string(source_type, "source_type")
  assert_character_vector(original_filenames, "original_filenames")
  assert_character_vector(hashes, "hashes")
  if (!is.list(import_arguments) || !is.list(details)) {
    abort_llw(
      "`import_arguments` and `details` must be lists.",
      type = "validation"
    )
  }
  if (
    !is.character(source_timezone) ||
      length(source_timezone) != 1L ||
      (!is.na(source_timezone) && !nzchar(source_timezone))
  ) {
    abort_llw(
      "`source_timezone` must be one string or `NA_character_`.",
      type = "validation"
    )
  }
  imported_at <- tryCatch(as.POSIXct(imported_at, tz = "UTC"), error = identity)
  if (
    inherits(imported_at, "error") ||
      length(imported_at) != 1L ||
      is.na(imported_at)
  ) {
    abort_llw(
      "`imported_at` must identify one valid instant.",
      type = "validation"
    )
  }
  if (length(hashes) > 0L && length(hashes) != length(original_filenames)) {
    abort_llw(
      "`hashes` must be empty or contain one hash per original filename.",
      type = "validation"
    )
  }
  if (
    length(hashes) > 0L &&
      any(!grepl("^sha256:[0-9a-f]{64}$", hashes))
  ) {
    abort_llw(
      "Source hashes must use the `sha256:<64 lowercase hex characters>` form.",
      type = "validation"
    )
  }
  if (length(hashes) > 0L) {
    names(hashes) <- original_filenames
  }
  assert_serializable_value(import_arguments, "import_arguments")
  assert_serializable_value(details, "details")

  structure(
    list(
      source_type = source_type,
      original_filenames = original_filenames,
      hashes = hashes,
      import_arguments = import_arguments,
      source_timezone = source_timezone,
      imported_at = imported_at,
      details = details
    ),
    class = c("llw_source_manifest", "list")
  )
}

validate_source_manifest <- function(manifest) {
  if (!inherits(manifest, "llw_source_manifest") || !is.list(manifest)) {
    abort_llw(
      "Source manifests must be created by `new_source_manifest()`.",
      type = "validation"
    )
  }
  required <- c(
    "source_type",
    "original_filenames",
    "hashes",
    "import_arguments",
    "source_timezone",
    "imported_at",
    "details"
  )
  if (!all(required %in% names(manifest))) {
    abort_llw("Source manifest fields are incomplete.", type = "validation")
  }
  rebuilt <- new_source_manifest(
    source_type = manifest$source_type,
    original_filenames = unname(manifest$original_filenames),
    hashes = unname(manifest$hashes),
    import_arguments = manifest$import_arguments,
    source_timezone = manifest$source_timezone,
    imported_at = manifest$imported_at,
    details = manifest$details
  )
  rebuilt
}

new_recipe_step <- function(
  type,
  parameters = list(),
  enabled = TRUE,
  id = new_stable_id("step")
) {
  assert_scalar_string(id, "id")
  if (!grepl("^step_[a-z0-9]+$", id)) {
    abort_llw(
      "Recipe step IDs must begin with `step_`.",
      type = "validation"
    )
  }
  assert_scalar_string(type, "type")
  if (!is.list(parameters)) {
    abort_llw("`parameters` must be a list.", type = "validation")
  }
  assert_flag(enabled, "enabled")
  assert_serializable_value(parameters, "recipe step parameters")
  structure(
    list(
      id = id,
      type = type,
      parameters = parameters,
      enabled = enabled
    ),
    class = c("llw_recipe_step", "list")
  )
}

validate_recipe_step <- function(step) {
  if (!inherits(step, "llw_recipe_step") || !is.list(step)) {
    abort_llw(
      "Recipe steps must be created by `new_recipe_step()`.",
      type = "validation"
    )
  }
  required <- c("id", "type", "parameters", "enabled")
  if (!all(required %in% names(step))) {
    abort_llw("Recipe step fields are incomplete.", type = "validation")
  }
  new_recipe_step(
    type = step$type,
    parameters = step$parameters,
    enabled = step$enabled,
    id = step$id
  )
}

new_recipe <- function(steps = list(), version = 1L) {
  if (!is.list(steps)) {
    abort_llw("`steps` must be a list.", type = "validation")
  }
  if (
    !is.numeric(version) ||
      length(version) != 1L ||
      is.na(version) ||
      version < 1 ||
      version != floor(version)
  ) {
    abort_llw(
      "`version` must be one positive whole number.",
      type = "validation"
    )
  }
  if (length(steps) > 0L) {
    steps <- lapply(steps, validate_recipe_step)
    step_ids <- vapply(steps, `[[`, character(1), "id")
    if (anyDuplicated(step_ids)) {
      abort_llw("Recipe step IDs must be unique.", type = "validation")
    }
  }
  assert_serializable_value(steps, "steps")
  structure(
    list(version = as.integer(version), steps = steps),
    class = c("llw_recipe", "list")
  )
}

validate_recipe <- function(recipe) {
  if (!inherits(recipe, "llw_recipe") || !is.list(recipe)) {
    abort_llw(
      "Recipes must be created by `new_recipe()`.",
      type = "validation"
    )
  }
  new_recipe(steps = recipe$steps, version = recipe$version)
}

recipe_fingerprint <- function(recipe) {
  recipe <- validate_recipe(recipe)
  paste0("sha256:", digest::digest(recipe, algo = "sha256", serialize = TRUE))
}

serialize_dataset_data <- function(data, arg = "data") {
  if (!is.data.frame(data)) {
    abort_llw(
      paste0(
        "`",
        arg,
        "` must be a data frame or tibble; supplied class `",
        class(data)[[1L]],
        "`."
      ),
      type = "validation"
    )
  }
  serialize(data, connection = NULL, version = 3)
}

clean_dataset_display_name <- function(display_name) {
  assert_scalar_string(display_name, "display_name")
  display_name <- trimws(display_name)
  if (!nzchar(display_name)) {
    abort_llw(
      "Dataset display names cannot be empty or whitespace only.",
      type = "validation",
      public_message = "Enter a non-empty dataset name."
    )
  }
  display_name
}

dataset_display_name_key <- function(display_name) {
  tolower(clean_dataset_display_name(display_name))
}

dataset_display_name_conflict <- function(display_name, existing_names) {
  display_name <- clean_dataset_display_name(display_name)
  assert_character_vector(existing_names, "existing_names")
  if (length(existing_names) == 0L) return(NULL)
  keys <- vapply(existing_names, dataset_display_name_key, character(1))
  index <- match(dataset_display_name_key(display_name), keys)
  if (is.na(index)) NULL else existing_names[[index]]
}

dataset_name_conflict_message <- function(display_name, existing_name) {
  display_name <- clean_dataset_display_name(display_name)
  existing_name <- clean_dataset_display_name(existing_name)
  paste0(
    "Dataset name `",
    display_name,
    "` is already in use as `",
    existing_name,
    "`. Choose a different name; surrounding spaces and capitalization do not make names unique."
  )
}

assert_dataset_display_name_available <- function(
  display_name,
  existing_names
) {
  display_name <- clean_dataset_display_name(display_name)
  conflict <- dataset_display_name_conflict(display_name, existing_names)
  if (!is.null(conflict)) {
    abort_llw(
      paste0("Dataset display name `", display_name, "` already exists."),
      type = "validation",
      public_message = dataset_name_conflict_message(display_name, conflict),
      diagnostics = list(
        attempted_name = display_name,
        existing_name = conflict
      ),
      subclass = "llw_dataset_name_conflict_error"
    )
  }
  invisible(display_name)
}

new_dataset_record <- function(
  raw_data,
  display_name,
  source_manifest,
  factual_metadata = list(),
  analysis_settings = list(),
  recipe = new_recipe(),
  provenance = list(),
  id = new_stable_id("dataset")
) {
  assert_scalar_string(id, "id")
  if (!grepl("^dataset_[a-z0-9]+$", id)) {
    abort_llw(
      "`id` must be a stable dataset identifier beginning with `dataset_`.",
      type = "validation"
    )
  }
  display_name <- clean_dataset_display_name(display_name)
  if (!inherits(source_manifest, "llw_source_manifest")) {
    abort_llw(
      "`source_manifest` must be created by `new_source_manifest()`.",
      type = "validation"
    )
  }
  if (!is.list(factual_metadata) || !is.list(analysis_settings)) {
    abort_llw(
      "`factual_metadata` and `analysis_settings` must be lists.",
      type = "validation"
    )
  }
  if (!inherits(recipe, "llw_recipe")) {
    abort_llw(
      "`recipe` must be created by `new_recipe()`.",
      type = "validation"
    )
  }
  if (!is.list(provenance)) {
    abort_llw("`provenance` must be a list.", type = "validation")
  }
  assert_serializable_value(factual_metadata, "factual_metadata")
  assert_serializable_value(analysis_settings, "analysis_settings")
  assert_serializable_value(provenance, "provenance")

  raw_payload <- serialize_dataset_data(raw_data, "raw_data")
  record <- structure(
    list(
      id = id,
      display_name = display_name,
      raw_payload = raw_payload,
      raw_checksum = sha256_raw(raw_payload),
      prepared_payload = raw_payload,
      source_manifest = source_manifest,
      factual_metadata = factual_metadata,
      analysis_settings = analysis_settings,
      recipe = recipe,
      revision = 0L,
      provenance = provenance,
      draft = NULL,
      history = list()
    ),
    class = c("llw_dataset_record", "list")
  )
  validate_dataset_record(record)
}

validate_dataset_record <- function(record) {
  if (!inherits(record, "llw_dataset_record") || !is.list(record)) {
    abort_llw(
      "Dataset records must be created by `new_dataset_record()`.",
      type = "validation"
    )
  }
  required <- c(
    "id",
    "display_name",
    "raw_payload",
    "raw_checksum",
    "prepared_payload",
    "source_manifest",
    "factual_metadata",
    "analysis_settings",
    "recipe",
    "revision",
    "provenance",
    "draft",
    "history"
  )
  missing <- setdiff(required, names(record))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "Dataset record is missing field(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "validation"
    )
  }
  assert_scalar_string(record$id, "record$id")
  if (!grepl("^dataset_[a-z0-9]+$", record$id)) {
    abort_llw("Dataset record ID is invalid.", type = "validation")
  }
  assert_scalar_string(record$display_name, "record$display_name")
  if (
    !identical(
      record$display_name,
      clean_dataset_display_name(record$display_name)
    )
  ) {
    abort_llw(
      "Dataset display names cannot contain surrounding whitespace.",
      type = "validation"
    )
  }
  if (!is.raw(record$raw_payload) || !is.raw(record$prepared_payload)) {
    abort_llw(
      "Dataset payloads must be serialized raw vectors.",
      type = "validation"
    )
  }
  if (
    !is.numeric(record$revision) ||
      length(record$revision) != 1L ||
      is.na(record$revision) ||
      record$revision < 0 ||
      record$revision != floor(record$revision)
  ) {
    abort_llw(
      "Dataset revision must be one non-negative whole number.",
      type = "validation"
    )
  }
  if (!is.list(record$history)) {
    abort_llw("Dataset history must be a list.", type = "validation")
  }
  assert_scalar_string(record$raw_checksum, "record$raw_checksum")
  if (!identical(record$raw_checksum, sha256_raw(record$raw_payload))) {
    abort_llw(
      "Canonical raw data failed their immutable checksum.",
      type = "validation",
      public_message = paste(
        "The canonical source data failed an integrity check.",
        "No derived result was applied."
      )
    )
  }
  record$source_manifest <- validate_source_manifest(record$source_manifest)
  record$recipe <- validate_recipe(record$recipe)
  if (
    !is.list(record$factual_metadata) ||
      !is.list(record$analysis_settings) ||
      !is.list(record$provenance)
  ) {
    abort_llw(
      "Dataset metadata, analysis settings, and provenance must be lists.",
      type = "validation"
    )
  }
  assert_serializable_value(record$factual_metadata, "record$factual_metadata")
  assert_serializable_value(
    record$analysis_settings,
    "record$analysis_settings"
  )
  assert_serializable_value(record$provenance, "record$provenance")
  if (length(record$history) > 0L) {
    record$history <- lapply(record$history, validate_dataset_snapshot)
  }
  if (!is.null(record$draft)) {
    record$draft <- validate_dataset_draft(record$draft, record$id)
  }
  raw_data <- tryCatch(unserialize(record$raw_payload), error = identity)
  prepared_data <- tryCatch(
    unserialize(record$prepared_payload),
    error = identity
  )
  if (inherits(raw_data, "error") || !is.data.frame(raw_data)) {
    abort_llw("Canonical raw data are corrupt.", type = "validation")
  }
  if (inherits(prepared_data, "error") || !is.data.frame(prepared_data)) {
    abort_llw("Prepared data are corrupt.", type = "validation")
  }
  record$revision <- as.integer(record$revision)
  record
}

dataset_raw_data <- function(record) {
  record <- validate_dataset_record(record)
  unserialize(record$raw_payload)
}

dataset_prepared_data <- function(record) {
  record <- validate_dataset_record(record)
  unserialize(record$prepared_payload)
}

dataset_raw_is_unchanged <- function(record, raw_payload) {
  record <- validate_dataset_record(record)
  is.raw(raw_payload) && identical(record$raw_payload, raw_payload)
}

dataset_cache_key <- function(record) {
  record <- validate_dataset_record(record)
  paste(
    "prepared",
    record$id,
    record$revision,
    sub("^sha256:", "", recipe_fingerprint(record$recipe)),
    sep = "-"
  )
}

dataset_snapshot <- function(record) {
  list(
    display_name = record$display_name,
    prepared_payload = record$prepared_payload,
    factual_metadata = record$factual_metadata,
    analysis_settings = record$analysis_settings,
    recipe = record$recipe
  )
}

validate_dataset_values <- function(values, include_prepared = FALSE) {
  if (!is.list(values)) {
    abort_llw("Dataset snapshot values must be a list.", type = "validation")
  }
  required <- c(
    "display_name",
    "factual_metadata",
    "analysis_settings",
    "recipe"
  )
  if (include_prepared) {
    required <- c(required, "prepared_payload")
  }
  if (!all(required %in% names(values))) {
    abort_llw("Dataset snapshot fields are incomplete.", type = "validation")
  }
  assert_scalar_string(values$display_name, "snapshot$display_name")
  if (!is.list(values$factual_metadata) || !is.list(values$analysis_settings)) {
    abort_llw(
      "Snapshot metadata and analysis settings must be lists.",
      type = "validation"
    )
  }
  values$recipe <- validate_recipe(values$recipe)
  if (include_prepared) {
    if (!is.raw(values$prepared_payload)) {
      abort_llw(
        "Snapshot prepared data must be a serialized raw vector.",
        type = "validation"
      )
    }
    prepared <- tryCatch(unserialize(values$prepared_payload), error = identity)
    if (inherits(prepared, "error") || !is.data.frame(prepared)) {
      abort_llw("Snapshot prepared data are corrupt.", type = "validation")
    }
  }
  assert_serializable_value(values, "dataset snapshot")
  values
}

validate_dataset_snapshot <- function(snapshot) {
  validate_dataset_values(snapshot, include_prepared = TRUE)
}

validate_dataset_draft <- function(draft, dataset_id) {
  if (!inherits(draft, "llw_dataset_draft") || !is.list(draft)) {
    abort_llw("Dataset draft is invalid.", type = "validation")
  }
  if (
    !is.numeric(draft$base_revision) ||
      length(draft$base_revision) != 1L ||
      is.na(draft$base_revision) ||
      draft$base_revision < 0 ||
      draft$base_revision != floor(draft$base_revision)
  ) {
    abort_llw("Dataset draft revision is invalid.", type = "validation")
  }
  draft$base_revision <- as.integer(draft$base_revision)
  draft$values <- validate_dataset_values(draft$values)
  if (!is.null(draft$preview)) {
    preview <- draft$preview
    if (!inherits(preview, "llw_dataset_preview") || !is.list(preview)) {
      abort_llw("Dataset preview is invalid.", type = "validation")
    }
    if (!identical(preview$dataset_id, dataset_id)) {
      abort_llw(
        "Dataset preview belongs to another dataset.",
        type = "validation"
      )
    }
    if (!is.raw(preview$prepared_payload)) {
      abort_llw("Dataset preview payload is invalid.", type = "validation")
    }
    preview$values <- validate_dataset_values(preview$values)
    assert_character_vector(preview$warnings, "preview$warnings")
    assert_serializable_value(preview, "dataset preview")
    draft$preview <- preview
  }
  draft
}

push_dataset_history <- function(record, snapshot, limit = 20L) {
  history <- c(list(snapshot), record$history)
  record$history <- utils::head(history, n = limit)
  record
}

stage_dataset_draft <- function(record, changes = list()) {
  record <- validate_dataset_record(record)
  if (!is.list(changes) || is.null(names(changes))) {
    abort_llw("`changes` must be a named list.", type = "validation")
  }
  allowed <- c(
    "display_name",
    "factual_metadata",
    "analysis_settings",
    "recipe"
  )
  unknown <- setdiff(names(changes), allowed)
  if (length(unknown) > 0L) {
    abort_llw(
      paste0("Unknown draft field(s): ", paste(unknown, collapse = ", "), "."),
      type = "validation"
    )
  }

  values <- dataset_snapshot(record)
  values$prepared_payload <- NULL
  if ("display_name" %in% names(changes)) {
    assert_scalar_string(changes$display_name, "changes$display_name")
    values$display_name <- changes$display_name
  }
  if ("factual_metadata" %in% names(changes)) {
    if (!is.list(changes$factual_metadata)) {
      abort_llw("Draft factual metadata must be a list.", type = "validation")
    }
    values$factual_metadata <- utils::modifyList(
      values$factual_metadata,
      changes$factual_metadata
    )
  }
  if ("analysis_settings" %in% names(changes)) {
    if (!is.list(changes$analysis_settings)) {
      abort_llw("Draft analysis settings must be a list.", type = "validation")
    }
    values$analysis_settings <- utils::modifyList(
      values$analysis_settings,
      changes$analysis_settings
    )
  }
  if ("recipe" %in% names(changes)) {
    if (!inherits(changes$recipe, "llw_recipe")) {
      abort_llw(
        "Draft recipe must be created by `new_recipe()`.",
        type = "validation"
      )
    }
    values$recipe <- changes$recipe
  }
  assert_serializable_value(values, "draft values")

  record$draft <- structure(
    list(
      base_revision = record$revision,
      values = values,
      preview = NULL
    ),
    class = c("llw_dataset_draft", "list")
  )
  validate_dataset_record(record)
}

preview_dataset_draft <- function(record, evaluator) {
  record <- validate_dataset_record(record)
  if (is.null(record$draft)) {
    abort_llw("There is no draft to preview.", type = "validation")
  }
  if (!is.function(evaluator)) {
    abort_llw("`evaluator` must be a function.", type = "validation")
  }
  if (!identical(record$draft$base_revision, record$revision)) {
    abort_llw(
      "The draft is stale because the dataset revision changed.",
      type = "validation"
    )
  }

  warnings <- character()
  prepared <- withCallingHandlers(
    evaluator(
      raw_data = dataset_raw_data(record),
      recipe = record$draft$values$recipe,
      analysis_settings = record$draft$values$analysis_settings,
      factual_metadata = record$draft$values$factual_metadata
    ),
    warning = function(cnd) {
      warnings <<- c(warnings, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    }
  )
  prepared_payload <- serialize_dataset_data(prepared, "preview result")
  record$draft$preview <- structure(
    list(
      dataset_id = record$id,
      base_revision = record$revision,
      values = record$draft$values,
      prepared_payload = prepared_payload,
      summary = list(
        raw_rows = nrow(dataset_raw_data(record)),
        prepared_rows = nrow(prepared),
        raw_columns = ncol(dataset_raw_data(record)),
        prepared_columns = ncol(prepared)
      ),
      warnings = unique(warnings)
    ),
    class = c("llw_dataset_preview", "list")
  )
  validate_dataset_record(record)
}

apply_dataset_draft <- function(record) {
  record <- validate_dataset_record(record)
  preview <- record$draft$preview
  if (is.null(record$draft) || is.null(preview)) {
    abort_llw(
      "A successful preview is required before Apply.",
      type = "validation"
    )
  }
  if (
    !identical(preview$dataset_id, record$id) ||
      !identical(preview$base_revision, record$revision)
  ) {
    abort_llw(
      "The preview is stale because the dataset revision changed.",
      type = "validation"
    )
  }

  record <- push_dataset_history(record, dataset_snapshot(record))
  record$display_name <- preview$values$display_name
  record$factual_metadata <- preview$values$factual_metadata
  record$analysis_settings <- preview$values$analysis_settings
  record$recipe <- preview$values$recipe
  record$prepared_payload <- preview$prepared_payload
  record$revision <- record$revision + 1L
  record["draft"] <- list(NULL)
  validate_dataset_record(record)
}

reset_dataset_record <- function(record) {
  record <- validate_dataset_record(record)
  record <- push_dataset_history(record, dataset_snapshot(record))
  record$prepared_payload <- record$raw_payload
  record$recipe <- new_recipe()
  record$revision <- record$revision + 1L
  record["draft"] <- list(NULL)
  validate_dataset_record(record)
}

undo_dataset_record <- function(record) {
  record <- validate_dataset_record(record)
  if (length(record$history) == 0L) {
    abort_llw("There is no applied change to undo.", type = "validation")
  }
  snapshot <- record$history[[1L]]
  record$history <- record$history[-1L]
  record$display_name <- snapshot$display_name
  record$prepared_payload <- snapshot$prepared_payload
  record$factual_metadata <- snapshot$factual_metadata
  record$analysis_settings <- snapshot$analysis_settings
  record$recipe <- snapshot$recipe
  record$revision <- record$revision + 1L
  record["draft"] <- list(NULL)
  validate_dataset_record(record)
}

rename_dataset_record <- function(record, display_name) {
  record <- validate_dataset_record(record)
  display_name <- clean_dataset_display_name(display_name)
  if (identical(record$display_name, display_name)) {
    return(record)
  }
  record <- push_dataset_history(record, dataset_snapshot(record))
  record$display_name <- display_name
  record$revision <- record$revision + 1L
  record["draft"] <- list(NULL)
  validate_dataset_record(record)
}

new_session_model <- function(
  profile,
  datasets = list(),
  selected_dataset_id = NULL,
  id = new_stable_id("session")
) {
  assert_scalar_string(profile, "profile")
  assert_scalar_string(id, "id")
  model <- structure(
    list(
      schema_version = 1L,
      id = id,
      profile = profile,
      datasets = datasets,
      selected_dataset_id = selected_dataset_id
    ),
    class = c("llw_session_model", "list")
  )
  validate_session_model(model)
}

validate_session_model <- function(model) {
  if (!inherits(model, "llw_session_model") || !is.list(model)) {
    abort_llw(
      "Session models must be created by `new_session_model()`.",
      type = "validation"
    )
  }
  if (!is.list(model$datasets)) {
    abort_llw("Session datasets must be a list.", type = "validation")
  }
  if (length(model$datasets) > 0L) {
    records <- lapply(model$datasets, validate_dataset_record)
    ids <- unname(vapply(records, `[[`, character(1), "id"))
    if (anyDuplicated(ids) || !identical(names(model$datasets), ids)) {
      abort_llw(
        "Session datasets must be named by unique stable dataset IDs.",
        type = "validation"
      )
    }
    display_names <- vapply(records, `[[`, character(1), "display_name")
    display_name_keys <- vapply(
      display_names,
      dataset_display_name_key,
      character(1)
    )
    if (anyDuplicated(display_name_keys)) {
      abort_llw(
        paste(
          "Dataset display names must be unique within a session after",
          "trimming whitespace and ignoring capitalization."
        ),
        type = "validation"
      )
    }
  }
  if (
    !is.null(model$selected_dataset_id) &&
      !model$selected_dataset_id %in% names(model$datasets)
  ) {
    abort_llw(
      "The selected dataset ID is not present in this session.",
      type = "validation"
    )
  }
  model
}

session_dataset <- function(model, dataset_id = model$selected_dataset_id) {
  model <- validate_session_model(model)
  if (is.null(dataset_id)) {
    return(NULL)
  }
  assert_scalar_string(dataset_id, "dataset_id")
  record <- model$datasets[[dataset_id]]
  if (is.null(record)) {
    abort_llw(
      paste0("Unknown dataset ID `", dataset_id, "`."),
      type = "validation"
    )
  }
  record
}

session_add_dataset <- function(model, record, select = TRUE) {
  model <- validate_session_model(model)
  record <- validate_dataset_record(record)
  assert_flag(select, "select")
  if (record$id %in% names(model$datasets)) {
    abort_llw(
      paste0("Dataset ID `", record$id, "` already exists in this session."),
      type = "validation"
    )
  }
  existing_names <- vapply(
    model$datasets,
    `[[`,
    character(1),
    "display_name"
  )
  assert_dataset_display_name_available(record$display_name, existing_names)
  model$datasets[[record$id]] <- record
  if (select || is.null(model$selected_dataset_id)) {
    model$selected_dataset_id <- record$id
  }
  validate_session_model(model)
}

session_replace_dataset <- function(model, record) {
  model <- validate_session_model(model)
  record <- validate_dataset_record(record)
  if (!record$id %in% names(model$datasets)) {
    abort_llw(
      paste0("Cannot replace unknown dataset ID `", record$id, "`."),
      type = "validation"
    )
  }
  other_names <- vapply(
    model$datasets[names(model$datasets) != record$id],
    `[[`,
    character(1),
    "display_name"
  )
  assert_dataset_display_name_available(record$display_name, other_names)
  model$datasets[[record$id]] <- record
  validate_session_model(model)
}

session_select_dataset <- function(model, dataset_id) {
  model <- validate_session_model(model)
  session_dataset(model, dataset_id)
  model$selected_dataset_id <- dataset_id
  validate_session_model(model)
}

session_remove_dataset <- function(model, dataset_id) {
  model <- validate_session_model(model)
  session_dataset(model, dataset_id)
  model$datasets[[dataset_id]] <- NULL
  if (identical(model$selected_dataset_id, dataset_id)) {
    remaining <- names(model$datasets)
    selected <- if (length(remaining) == 0L) {
      NULL
    } else {
      remaining[[1L]]
    }
    model["selected_dataset_id"] <- list(selected)
  }
  validate_session_model(model)
}

new_session_event <- function(type, dataset_id = NULL, value = NULL) {
  type <- match.arg(
    type,
    c("add", "replace", "select", "remove", "rename", "reset", "undo")
  )
  if (!is.null(dataset_id)) {
    assert_scalar_string(dataset_id, "dataset_id")
  }
  if (type %in% c("select", "remove", "rename", "reset", "undo")) {
    if (is.null(dataset_id)) {
      abort_llw(
        paste0("Session event `", type, "` requires a dataset ID."),
        type = "validation"
      )
    }
  } else if (!is.null(dataset_id)) {
    abort_llw(
      paste0("Session event `", type, "` does not accept a dataset ID."),
      type = "validation"
    )
  }
  if (type %in% c("add", "replace")) {
    value <- validate_dataset_record(value)
  } else if (identical(type, "rename")) {
    assert_scalar_string(value, "value")
  } else if (!is.null(value)) {
    abort_llw(
      paste0("Session event `", type, "` does not accept a value."),
      type = "validation"
    )
  }
  event <- structure(
    list(
      id = new_stable_id("event"),
      type = type,
      dataset_id = dataset_id,
      value = value
    ),
    class = c("llw_session_event", "list")
  )
  assert_serializable_value(event, "session event")
  event
}

session_event_with_display_name <- function(event, display_name) {
  if (!inherits(event, "llw_session_event")) {
    abort_llw(
      "`event` must be created by `new_session_event()`.",
      type = "validation"
    )
  }
  display_name <- clean_dataset_display_name(display_name)
  switch(
    event$type,
    add = {
      record <- event$value
      record$display_name <- display_name
      new_session_event("add", value = validate_dataset_record(record))
    },
    replace = new_session_event(
      "replace",
      value = rename_dataset_record(event$value, display_name)
    ),
    rename = new_session_event(
      "rename",
      dataset_id = event$dataset_id,
      value = display_name
    ),
    abort_llw(
      paste0(
        "Session event `",
        event$type,
        "` cannot be retried with another dataset name."
      ),
      type = "validation"
    )
  )
}

reduce_session_model <- function(model, event) {
  model <- validate_session_model(model)
  if (!inherits(event, "llw_session_event")) {
    abort_llw(
      "Session changes must be represented by `llw_session_event` values.",
      type = "validation"
    )
  }
  switch(
    event$type,
    add = session_add_dataset(model, event$value),
    replace = session_replace_dataset(model, event$value),
    select = session_select_dataset(model, event$dataset_id),
    remove = session_remove_dataset(model, event$dataset_id),
    rename = {
      record <- session_dataset(model, event$dataset_id)
      session_replace_dataset(
        model,
        rename_dataset_record(record, event$value)
      )
    },
    reset = {
      record <- session_dataset(model, event$dataset_id)
      session_replace_dataset(model, reset_dataset_record(record))
    },
    undo = {
      record <- session_dataset(model, event$dataset_id)
      session_replace_dataset(model, undo_dataset_record(record))
    }
  )
}

new_session_store <- function(profile) {
  assert_scalar_string(profile, "profile")
  model_value <- shiny::reactiveVal(new_session_model(profile = profile))

  model <- shiny::reactive(model_value())
  datasets <- shiny::reactive(model_value()$datasets)
  selected_dataset_id <- shiny::reactive(model_value()$selected_dataset_id)
  selected_dataset <- shiny::reactive({
    current <- model_value()
    session_dataset(current, current$selected_dataset_id)
  })

  dispatch <- function(event) {
    current_model <- shiny::isolate(model_value())
    next_model <- reduce_session_model(current_model, event)
    model_value(next_model)
    invisible(next_model)
  }

  list(
    model = model,
    datasets = datasets,
    selected_dataset_id = selected_dataset_id,
    selected_dataset = selected_dataset,
    revision = function(dataset_id) {
      record <- session_dataset(shiny::isolate(model_value()), dataset_id)
      if (is.null(record)) NULL else record$revision
    },
    dispatch = dispatch
  )
}
