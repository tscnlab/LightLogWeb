# Saved recipe drafts contain only serializable settings. Previews and decoded
# measurements remain disposable, local to the active Prepare module.
preparation_saved_recipe <- function(record) {
  if (is.null(record)) return(new_recipe())
  if (identical(record$draft$kind, "preparation"))
    record$draft$values$recipe else record$recipe
}

preparation_assert_current <- function(record, expected) {
  if (
    is.null(record) ||
      !identical(record$id, expected$dataset_id) ||
      !identical(record$revision, expected$revision) ||
      !identical(record$raw_checksum, expected$raw_checksum)
  )
    abort_llw(
      "The dataset changed. Preview again before applying.",
      "preparation"
    )
  invisible(TRUE)
}

preparation_stage_recipe <- function(record, recipe, expected) {
  preparation_assert_current(record, expected)
  recipe <- validate_recipe(recipe)
  if (length(recipe$steps) > 30L)
    abort_llw("A recipe can contain at most 30 steps.", "preparation")
  if (identical(recipe, record$recipe)) {
    record["draft"] <- list(NULL)
  } else {
    values <- dataset_snapshot(record)
    values$prepared_payload <- NULL
    values$recipe <- recipe
    record$draft <- structure(
      list(
        base_revision = record$revision,
        values = values,
        preview = NULL,
        kind = "preparation"
      ),
      class = c("llw_dataset_draft", "list")
    )
  }
  record
}

preparation_commit_record <- function(record, candidate, expected) {
  preparation_assert_current(record, expected)
  candidate <- validate_dataset_record(candidate)
  if (
    !identical(candidate$id, record$id) ||
      !identical(candidate$raw_payload, record$raw_payload) ||
      !identical(candidate$revision, record$revision + 1L)
  )
    abort_llw(
      "This preparation result does not belong to the current dataset revision.",
      "preparation"
    )
  candidate
}

preparation_variable_label <- function(record, variable) {
  info <- dashboard_variable_metadata(record, variable)
  label <- info$label %||% variable
  # The raw name remains visible even when metadata supplies a friendlier label.
  if (
    !identical(label, variable) &&
      !grepl(paste0("[", variable, "]"), label, fixed = TRUE)
  )
    label <- paste0(label, " [", variable, "]")
  paste0(label, " (", metadata_display_unit(info$unit), ")")
}

preparation_focus_request <- function(context, record, variable = NULL) {
  if (is.null(variable)) return(context)
  if (!is.character(variable) || length(variable) != 1L || is.na(variable))
    abort_llw("Choose one dashboard focus variable.", "preparation")
  lineage <- preparation_variable_lineage(record$recipe)[[variable]]
  source <- lineage$source %||% variable
  if (!source %in% context$measurements) return(context)
  context$requested_variable <- variable
  context$requested_source <- source
  context$variable <- source
  context$variable_label <- context$measurement_labels[[source]]
  context
}

# Follow only the selected measurement's creation chain. An unrelated sensor
# used by another step must not take over the Dashboard's focus.
preparation_follow_focus <- function(
  variable,
  recipe,
  previous = new_recipe()
) {
  if (is.null(variable)) return(NULL)
  original <- preparation_variable_lineage(previous)[[variable]]
  available <- preparation_variable_lineage(recipe)
  current <- variable
  if (
    !is.null(original) &&
      is.null(available[[variable]]) &&
      !identical(original$source, variable)
  ) {
    ancestors <- rev(vapply(
      Filter(
        function(event) !is.null(event$arguments$variable),
        original$history
      ),
      function(event) event$arguments$variable,
      character(1)
    ))
    ancestors <- c(ancestors, original$source)
    current <- ancestors[ancestors %in% c(names(available), original$source)][[
      1L
    ]]
    # Removing the selected branch returns to its surviving parent. Following
    # that parent's other creation steps would silently select a sibling.
    return(current)
  }
  for (step in recipe$steps) {
    args <- step$parameters$arguments
    if (
      isTRUE(step$enabled) &&
        step$type %in%
          c("invalidate", "calibrate", "transform", "variability") &&
        identical(args$variable, current)
    )
      current <- args$output %||% current
  }
  current
}
