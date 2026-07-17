llw_error_types <- function() {
  c(
    "import",
    "validation",
    "resource",
    "preparation",
    "grouping",
    "metric",
    "export",
    "network",
    "unavailable_feature"
  )
}

new_llw_error <- function(
  message,
  type,
  public_message = message,
  diagnostics = list(),
  parent = NULL,
  subclass = NULL
) {
  type <- match.arg(type, llw_error_types())
  if (!is.character(message) || length(message) != 1L || is.na(message)) {
    stop("`message` must be one non-missing string.", call. = FALSE)
  }
  if (
    !is.character(public_message) ||
      length(public_message) != 1L ||
      is.na(public_message)
  ) {
    stop("`public_message` must be one non-missing string.", call. = FALSE)
  }
  if (!is.list(diagnostics)) {
    stop("`diagnostics` must be a list.", call. = FALSE)
  }

  structure(
    list(
      message = message,
      call = NULL,
      type = type,
      public_message = public_message,
      diagnostic_id = new_stable_id("diagnostic"),
      diagnostics = diagnostics,
      parent = parent
    ),
    class = unique(c(
      subclass,
      paste0("llw_", type, "_error"),
      "llw_error",
      "error",
      "condition"
    ))
  )
}

abort_llw <- function(
  message,
  type,
  public_message = message,
  diagnostics = list(),
  parent = NULL,
  subclass = NULL
) {
  stop(new_llw_error(
    message = message,
    type = type,
    public_message = public_message,
    diagnostics = diagnostics,
    parent = parent,
    subclass = subclass
  ))
}

llw_public_message <- function(condition) {
  if (inherits(condition, "llw_error")) {
    return(condition$public_message)
  }
  "The operation could not be completed. Your current session data are unchanged."
}

glcdp_condition_classes <- function(condition) {
  classes <- class(condition)
  remote_classes <- condition$condition.class
  unique(c(classes, as.character(remote_classes)))
}

glcdp_condition_category <- function(classes) {
  groups <- list(
    lfs = c(
      "glcdp_malformed_lfs_pointer",
      "glcdp_external_lfs",
      "glcdp_lfs_repository_unknown",
      "glcdp_lfs_response",
      "glcdp_lfs_missing_object",
      "glcdp_lfs_auth_or_quota",
      "glcdp_lfs_object_error",
      "glcdp_lfs_network_error",
      "glcdp_lfs_size_mismatch",
      "glcdp_lfs_hash_mismatch",
      "glcdp_lfs_download_error"
    ),
    compatibility = c(
      "glcdp_incompatible_group_files",
      "glcdp_incompatible_collection",
      "glcdp_standard_column_conflict",
      "glcdp_unsupported_schema"
    ),
    import = c(
      "glcdp_empty_data",
      "glcdp_header_not_found",
      "glcdp_import_problem",
      "glcdp_timezone_error",
      "glcdp_datetime_metadata",
      "glcdp_datetime_parse",
      "glcdp_type_parse",
      "glcdp_unsupported_data_format",
      "glcdp_missing_column",
      "glcdp_extra_column"
    ),
    resource = c(
      "glcdp_file_exists",
      "glcdp_unsupported_storage"
    ),
    transport = c(
      "glcdp_http_error",
      "glcdp_http_status",
      "glcdp_truncated_tree"
    ),
    discovery = c(
      "glcdp_registry_error",
      "glcdp_no_passing_revision",
      "glcdp_unknown_dataset"
    ),
    validation = c(
      "glcdp_schema_error",
      "glcdp_missing_descriptor",
      "glcdp_missing_resource",
      "glcdp_missing_path",
      "glcdp_ambiguous_path",
      "glcdp_dataset_id_error",
      "glcdp_unsupported_metadata_format",
      "glcdp_json_error"
    )
  )

  for (group in names(groups)) {
    if (any(classes %in% groups[[group]])) {
      return(group)
    }
  }
  if (any(grepl("^glcdp_http_[0-9]{3}$", classes))) {
    return("transport")
  }
  "validation"
}

map_glcdp_condition <- function(condition) {
  classes <- glcdp_condition_classes(condition)
  category <- glcdp_condition_category(classes)
  type <- switch(
    category,
    lfs = "network",
    compatibility = "validation",
    import = "import",
    resource = "resource",
    transport = "network",
    discovery = "validation",
    validation = "validation"
  )
  public_message <- switch(
    category,
    discovery = paste(
      "The GLC source could not be discovered or opened.",
      "Refresh the registry or choose another validated revision."
    ),
    validation = paste(
      "The GLC package does not satisfy the supported schema or selection",
      "contract. No session data were changed."
    ),
    transport = paste(
      "The GLC service could not be reached.",
      "Check the connection and try again; current session data are unchanged."
    ),
    lfs = paste(
      "A Git LFS object could not be retrieved or verified.",
      "Check package availability or quota, then retry."
    ),
    import = paste(
      "The selected GLC files could not be imported.",
      "Review the file and variable selection, then retry."
    ),
    compatibility = paste(
      "The selected GLC file groups are not compatible.",
      "Import compatible partitions separately."
    ),
    resource = paste(
      "The GLC operation could not complete within the available file or",
      "disk resources."
    )
  )

  original_message <- tryCatch(
    conditionMessage(condition),
    error = function(cnd) "Unknown glcdp failure"
  )
  new_llw_error(
    message = original_message,
    type = type,
    public_message = public_message,
    diagnostics = list(
      source = "glcdp",
      category = category,
      classes = classes,
      original_message = original_message,
      stack_trace = condition$stack.trace
    ),
    parent = condition,
    subclass = paste0("llw_glcdp_", category, "_error")
  )
}

task_error_type <- function(task_type) {
  switch(
    task_type,
    raw_import = "import",
    glc_discovery = "network",
    glc_read = "import",
    glc_download = "network",
    preparation = "preparation",
    append_merge = "preparation",
    metrics = "metric",
    report = "export",
    "validation"
  )
}

normalize_task_error <- function(condition, task_type) {
  if (inherits(condition, "llw_error")) {
    return(condition)
  }
  classes <- glcdp_condition_classes(condition)
  if (any(startsWith(classes, "glcdp_"))) {
    return(map_glcdp_condition(condition))
  }

  type <- task_error_type(task_type)
  original_message <- tryCatch(
    conditionMessage(condition),
    error = function(cnd) "Unknown task failure"
  )
  public_message <- switch(
    type,
    import = "Import failed. Review the source files and settings, then retry.",
    preparation = "Preparation failed. The previously applied data remain available.",
    metric = "The metric calculation failed. Other results and session data remain available.",
    export = "Export failed. The analysis remains available in this session.",
    network = "The network operation failed. Check the connection and retry.",
    unavailable_feature = "This feature is not available in the current runtime.",
    "The task failed. Current session data are unchanged and you can retry."
  )
  new_llw_error(
    message = original_message,
    type = type,
    public_message = public_message,
    diagnostics = list(
      task_type = task_type,
      classes = classes,
      original_message = original_message,
      stack_trace = condition$stack.trace
    ),
    parent = condition
  )
}
