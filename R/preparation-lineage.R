# A target is a typed column name, never an expression. Source names and names
# reserved by other steps are protected even when those steps are disabled.
preparation_output_name <- function(name) {
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(trimws(name)) ||
      nchar(name) > 100L ||
      grepl("[[:cntrl:]]", name) ||
      startsWith(trimws(name), ".llw_")
  )
    abort_llw(
      "Enter a variable name of 1 to 100 characters, without control characters or the reserved .llw_ prefix.",
      "preparation"
    )
  trimws(name)
}

preparation_output_names <- function(recipe) {
  unname(vapply(
    Filter(
      function(s)
        s$type %in%
          c("invalidate", "calibrate", "transform", "variability") &&
          !is.null(s$parameters$arguments$output),
      recipe$steps
    ),
    function(s) s$parameters$arguments$output,
    character(1)
  ))
}

preparation_suggest_name <- function(variable, existing) {
  base <- paste0(substr(variable, 1L, 85L), "_processed")
  name <- base
  suffix <- 2L
  while (name %in% existing) {
    name <- paste0(base, "_", suffix)
    suffix <- suffix + 1L
  }
  name
}

preparation_variable_choices <- function(available, selected, labels = NULL) {
  choices <- stats::setNames(
    available,
    vapply(available, function(name) labels[[name]] %||% name, character(1))
  )
  if (length(selected) == 1L && nzchar(selected) && !selected %in% available)
    choices <- c(
      choices,
      stats::setNames(
        selected,
        paste0(selected, " (unavailable at this step)")
      )
    )
  choices
}

preparation_value_step <- function(step) {
  step$type %in% c("invalidate", "calibrate", "transform", "variability")
}

preparation_step_arguments <- function(step, args) {
  step$parameters$arguments <- preparation_arguments(step$type, args)
  if (preparation_value_step(step))
    step$parameters$mapping_version <- preparation_mapping_version(step$type, args)
  step
}

# An explicit change to a step's destination follows its consumers, including
# branches, but never changes a preceding step or an unrelated input column.
preparation_retarget_following <- function(previous, recipe, id) {
  ids <- vapply(previous$steps, `[[`, "", "id")
  position <- match(id, ids)
  if (is.na(position)) return(recipe)
  old <- previous$steps[[position]]
  current <- recipe$steps[[position]]
  if (!preparation_value_step(old) || !preparation_value_step(current))
    return(recipe)
  old_args <- old$parameters$arguments
  args <- current$parameters$arguments
  from <- old_args$output %||% old_args$variable
  to <- args$output %||% args$variable
  if (identical(from, to)) return(recipe)
  for (i in seq_along(recipe$steps)) {
    if (i <= position) next
    step <- recipe$steps[[i]]
    args <- step$parameters$arguments
    if (!identical(args$variable, from)) next
    args$variable <- to
    recipe$steps[[i]] <- preparation_step_arguments(step, args)
  }
  new_recipe(recipe$steps, recipe$version)
}

# Resolve only a known derived column back through its original parents. Never
# substitute an unrelated measurement when a variable has no recorded lineage.
preparation_reconnect_order <- function(previous, recipe, context) {
  parents <- list()
  for (step in previous$steps) {
    args <- step$parameters$arguments
    if (preparation_value_step(step) && !is.null(args$output))
      parents[[args$output]] <- args$variable
  }
  available <- context$columns %||% names(context$variables)
  promoted <- character()
  for (i in seq_along(recipe$steps)) {
    step <- recipe$steps[[i]]
    args <- step$parameters$arguments
    if (!step$enabled || is.null(args$variable)) next
    if (
      preparation_value_step(step) &&
        !is.null(args$output) &&
        args$output %in% promoted
    ) {
      # An earlier consumer now creates this column. The old creator continues
      # the same chain using that result rather than writing over another input.
      args$variable <- args$output
      args$output <- NULL
    } else if (!args$variable %in% available) {
      original <- args$variable
      source <- original
      visited <- character()
      while (!source %in% available && !is.null(parents[[source]])) {
        if (source %in% visited) break
        visited <- c(visited, source)
        source <- parents[[source]]
      }
      if (!source %in% available) next
      args$variable <- source
      if (preparation_value_step(step) && is.null(args$output)) {
        args$output <- original
        promoted <- c(promoted, original)
      }
    }
    recipe$steps[[i]] <- preparation_step_arguments(step, args)
    if (preparation_value_step(step) && !is.null(args$output))
      available <- union(available, args$output)
  }
  preparation_reconnect_sources(
    previous,
    new_recipe(recipe$steps, recipe$version),
    context
  )
}

# The check channel is independent of target/output retargeting. Follow its
# recorded ancestors only when that channel is unavailable at its new position.
preparation_reconnect_sources <- function(previous, recipe, context) {
  parents <- list()
  for (step in c(previous$steps, recipe$steps)) {
    args <- step$parameters$arguments
    if (
      preparation_value_step(step) &&
        !is.null(args$output) &&
        is.null(parents[[args$output]])
    )
      parents[[args$output]] <- args$variable
  }
  available <- context$columns %||% names(context$variables)
  for (i in seq_along(recipe$steps)) {
    step <- recipe$steps[[i]]
    if (!step$enabled) next
    args <- step$parameters$arguments
    if (
      step$type %in%
        c("variability", "invalidate") &&
        !is.null(args$source) &&
        !args$source %in% available
    ) {
      source <- args$source
      visited <- character()
      while (
        !source %in% available &&
          !is.null(parents[[source]]) &&
          !source %in% visited
      ) {
        visited <- c(visited, source)
        source <- parents[[source]]
      }
      if (source %in% available) {
        args$source <- source
        recipe$steps[[i]] <- preparation_step_arguments(step, args)
      }
    }
    if (preparation_value_step(step) && !is.null(args$output))
      available <- union(available, args$output)
  }
  new_recipe(recipe$steps, recipe$version)
}

preparation_chain_changes <- function(previous, recipe) {
  ids <- vapply(previous$steps, `[[`, "", "id")
  changes <- character()
  for (i in seq_along(recipe$steps)) {
    step <- recipe$steps[[i]]
    old <- match(step$id, ids)
    if (is.na(old)) next
    args <- step$parameters$arguments
    before <- previous$steps[[old]]$parameters$arguments
    if (
      identical(args$variable, before$variable) &&
        identical(args$output, before$output) &&
        identical(args$source, before$source)
    )
      next
    if (is.null(args$variable)) next
    changes <- c(
      changes,
      paste0(
        "Step ",
        i,
        " now uses ",
        args$variable,
        if (preparation_value_step(step))
          paste0(" and writes to ", args$output %||% args$variable),
        if (step$type == "variability")
          paste0("; variability source: ", args$source),
        if (step$type == "invalidate" && !is.null(args$source))
          paste0("; condition source: ", args$source),
        "."
      )
    )
  }
  changes
}

preparation_merge_time_context <- function(context, time) {
  # An empty preview has no new span. Keep valid source-derived date bounds.
  if (!all(is.finite(c(as.numeric(time$start), as.numeric(time$end)))))
    time[c("start", "end")] <- NULL
  utils::modifyList(context, time)
}

preparation_new_step_context <- function(context, recipe, snapshot = NULL) {
  result <- preparation_recipe_context(context, recipe)
  catalog <- preparation_cached_variables(snapshot, context, recipe)
  if (!is.null(catalog)) result$variables <- catalog
  n <- length(recipe$steps)
  if (!is.null(catalog) && length(snapshot$time_after) >= n)
    result <- preparation_merge_time_context(result, snapshot$time_after[[n]])
  for (step in recipe$steps) {
    if (
      step$enabled &&
        preparation_value_step(step) &&
        !is.null(step$parameters$arguments$output)
    )
      result$variable <- step$parameters$arguments$output
  }
  result
}

preparation_catalog_snapshot <- function(
  context,
  recipe,
  variables_after,
  time_after = NULL
) {
  list(
    dataset_id = context$dataset_id,
    raw_checksum = context$raw_checksum,
    recipe = recipe,
    variables_after = variables_after,
    time_after = time_after
  )
}

preparation_cached_variables <- function(snapshot, context, prefix) {
  n <- length(prefix$steps)
  if (
    is.null(snapshot) ||
      !n ||
      !identical(snapshot$dataset_id, context$dataset_id) ||
      !identical(snapshot$raw_checksum, context$raw_checksum) ||
      length(snapshot$recipe$steps) < n ||
      !identical(snapshot$recipe$steps[seq_len(n)], prefix$steps)
  )
    return(NULL)
  snapshot$variables_after[[n]]
}

# Copy the lineage at the moment a column branches. Later changes to its parent
# must not be attributed to the already-created child.
preparation_variable_lineage <- function(recipe) {
  lineage <- list()
  shared_history <- list()
  for (step in recipe$steps) {
    if (step$enabled && step$type == "aggregate") {
      event <- list(list(
        step_id = step$id,
        type = step$type,
        mapping_version = step$parameters$mapping_version,
        arguments = step$parameters$arguments
      ))
      shared_history <- c(shared_history, event)
      lineage <- lapply(lineage, function(info) {
        info$history <- c(info$history, event)
        info
      })
      next
    }
    if (
      !step$enabled ||
        !step$type %in% c("invalidate", "calibrate", "transform", "variability")
    )
      next
    args <- step$parameters$arguments
    parent <- lineage[[args$variable]] %||%
      list(source = args$variable, history = shared_history)
    target <- args$output %||% args$variable
    parent$history <- c(
      parent$history,
      list(list(
        step_id = step$id,
        type = step$type,
        mapping_version = step$parameters$mapping_version,
        arguments = args
      ))
    )
    lineage[[target]] <- parent
  }
  lineage
}

# Keep imported metadata untouched. The applied recipe supplies a reproducible
# derived layer, so Reset, Undo and replacing the recipe restore it atomically.
preparation_derived_metadata <- function(
  record,
  variable,
  include_overlay = TRUE
) {
  lineage <- preparation_variable_lineage(record$recipe)[[variable]]
  if (is.null(lineage) || identical(lineage$source, variable)) return(NULL)
  creation <- Filter(
    function(event) identical(event$arguments$output, variable),
    lineage$history
  )[[1L]]
  inherited <- metadata_resolve_variable(record, creation$arguments$variable)
  resolved <- inherited
  resolved$raw_name <- variable
  # Presentation helpers add the raw name once, separately from the label.
  resolved$display_label <- inherited$display_label
  resolved$inherited_metadata <- inherited
  resolved$inherited_factual_metadata <-
    record$factual_metadata$variables[[lineage$source]] %||% list()
  resolved$source_variable <- lineage$source
  resolved$transformation_history <- lineage$history
  resolved$provenance <- paste("Derived from", lineage$source)
  # An inherited unit already describes the parent's transformations. Only
  # an additional transform on this branch needs the derived-unit qualifier.
  inherited_transforms <- vapply(
    Filter(
      function(event) identical(event$type, "transform"),
      inherited$transformation_history
    ),
    `[[`,
    "",
    "step_id"
  )
  nonlinear <- any(vapply(
    lineage$history,
    function(x)
      identical(x$type, "transform") &&
        !x$step_id %in% inherited_transforms,
    logical(1)
  ))
  resolved$source_unit <- inherited$source_unit %||% inherited$unit
  if (nonlinear && !startsWith(inherited$unit, "transformed values; source:")) {
    resolved$unit <- paste0("transformed values; source: ", inherited$unit)
    resolved$unit_provenance <- "Preparation recipe; source unit retained in inherited metadata"
  }
  if (include_overlay) {
    override <- preparation_metadata_override(record, variable)
    for (field in c("label", "unit")) {
      value <- override[[field]]
      if (length(value) != 1L || is.na(value) || !nzchar(value)) next
      resolved[[field]] <- value
      resolved[[paste0(field, "_provenance")]] <- "LightLogWeb overlay"
      if (field == "label") resolved$display_label <- value
      resolved$provenance <- paste(
        "LightLogWeb overlay; derived from",
        lineage$source
      )
    }
    if (nrow(override)) {
      resolved$target <- new_metadata_focus_request(
        record$id,
        namespace_id = metadata_overlay_namespace_id(),
        field = "name",
        path_contains = "/variables",
        record_identity = override$file_group_id[[1L]],
        label = paste("Variable metadata for", variable),
        request_id = paste0(
          "metadata_focus_",
          digest::digest(override, algo = "xxhash64")
        )
      )
    }
  }
  resolved
}

# Bind assertions to the applied variable's creator, not merely a reusable
# column name. A replacement creator must not inherit an unrelated old edit.
preparation_metadata_identities <- function(record) {
  lineage <- preparation_variable_lineage(record$recipe)
  result <- list()
  for (variable in names(lineage)) {
    info <- lineage[[variable]]
    if (identical(info$source, variable)) next
    creator <- Filter(
      function(event) identical(event$arguments$output, variable),
      info$history
    )
    if (!length(creator)) next
    result[[variable]] <- list(
      source_scope = metadata_overlay_scope(c("llw_preparation", record$id)),
      dataset_id = record$id,
      file_group_id = paste0("llw:prepare:", creator[[1L]]$step_id),
      name = variable,
      source = info$source
    )
  }
  result
}

preparation_metadata_override <- function(record, variable) {
  identity <- preparation_metadata_identities(record)[[variable]]
  values <- record$metadata_bundle$overlay$tables$variables
  if (is.null(identity) || !"source_scope" %in% names(values))
    return(values[FALSE, , drop = FALSE])
  keep <- rep(TRUE, nrow(values))
  for (key in c("source_scope", "dataset_id", "file_group_id", "name"))
    keep <- keep & values[[key]] == identity[[key]]
  values[which(keep), , drop = FALSE]
}

# Extend only the schema here. Actual numeric/category summaries are returned
# by the preview worker and do not run on the Shiny session thread.
preparation_recipe_context <- function(context, recipe) {
  for (step in recipe$steps) {
    if (step$enabled && step$type == "timezone") {
      args <- step$parameters$arguments
      context$timezone <- args$timezone
      context$start <- if (args$method == "with")
        lubridate::with_tz(context$start, args$timezone) else
        lubridate::force_tz(context$start, args$timezone, roll_dst = "NA")
      context$end <- if (args$method == "with")
        lubridate::with_tz(context$end, args$timezone) else
        lubridate::force_tz(context$end, args$timezone, roll_dst = "NA")
    }
    if (
      !step$enabled ||
        !step$type %in% c("invalidate", "calibrate", "transform", "variability")
    )
      next
    args <- step$parameters$arguments
    target <- args$output %||% args$variable
    if (!args$variable %in% context$measurements) next
    context$measurements <- union(context$measurements, target)
    context$variables[[target]] <- list(
      numeric = TRUE,
      categories = character(),
      categories_complete = FALSE,
      lower = 0,
      upper = 100000,
      missing = NA_integer_,
      derived = TRUE
    )
    context$measurement_labels[[target]] <-
      context$measurement_labels[[args$variable]]
    if (!identical(target, args$variable)) {
      context$measurement_labels[[target]] <- paste0(
        context$measurement_labels[[args$variable]],
        " \u2192 ",
        target
      )
    }
    context$variable_labels[[target]] <- context$measurement_labels[[target]]
  }
  context
}
