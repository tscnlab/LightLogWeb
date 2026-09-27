metadata_state_label <- function(state) {
  switch(
    state,
    complete = "Complete retained source",
    partial = "Partial source metadata",
    absent = "Optional metadata absent",
    "Unknown metadata state"
  )
}

metadata_state_callout <- function(bundle) {
  states <- vapply(bundle$sources, `[[`, character(1), "state")
  if (all(states == "absent")) {
    return(llw_status_callout(
      "warning",
      paste(
        "No optional metadata were supplied. This dataset remains fully",
        "usable with raw names and the fallback `unit not specified`."
      ),
      heading = "Optional metadata absent",
      compact = TRUE
    ))
  }
  if (any(states != "complete")) {
    return(llw_status_callout(
      "warning",
      paste(
        "Some optional metadata are unavailable or incomplete. Retained",
        "values remain searchable, and missing values are not inferred."
      ),
      heading = "Partial metadata",
      compact = TRUE
    ))
  }
  llw_status_callout(
    "complete",
    paste(
      "All metadata resources retained for this import are available in",
      "memory. This status does not revalidate the source package."
    ),
    heading = "Retained source metadata ready",
    compact = TRUE
  )
}

metadata_source_choices <- function(bundle) {
  ids <- names(bundle$sources)
  labels <- vapply(
    bundle$sources,
    function(source) {
      paste0(source$label, " (", metadata_state_label(source$state), ")")
    },
    character(1)
  )
  c("All source datasets" = "all", stats::setNames(ids, labels))
}

metadata_layer_choices <- function(bundle) {
  overlay_rows <- bundle$index$entries$namespace_id ==
    metadata_overlay_namespace_id()
  if (!any(overlay_rows)) return(c("All metadata" = "all"))
  c(
    "All metadata" = "all",
    "Source metadata" = "Immutable source",
    "LightLogWeb overlay" = "LightLogWeb overlay"
  )
}

metadata_entry_choice_label <- function(row, max_value = 72L) {
  value <- row$value[[1L]]
  if (is.na(value) || !nzchar(value)) value <- "not specified"
  if (nchar(value, type = "width") > max_value) {
    value <- paste0(substr(value, 1L, max_value - 1L), "...")
  }
  paste0("[", row$resource[[1L]], "] ", row$field[[1L]], ": ", value)
}

metadata_result_choices <- function(rows) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) return(character())
  labels <- vapply(
    seq_len(nrow(rows)),
    function(index) {
      metadata_entry_choice_label(rows[index, , drop = FALSE])
    },
    character(1)
  )
  stats::setNames(rows$entry_id, labels)
}

metadata_result_table_ui <- function(rows, max_rows = 25L) {
  shown <- utils::head(rows, max_rows)
  if (nrow(shown) == 0L) return(NULL)
  value <- shown$value
  value[is.na(value) | !nzchar(value)] <- "not specified"
  body <- lapply(seq_len(nrow(shown)), function(index) {
    provenance_class <- if (
      identical(shown$layer[[index]], "LightLogWeb overlay")
    ) {
      "llw-metadata-provenance--overlay"
    } else {
      "llw-metadata-provenance--source"
    }
    tags$tr(
      tags$td(shown$source_label[[index]]),
      tags$td(shown$resource[[index]]),
      tags$td(tags$code(shown$field[[index]])),
      tags$td(value[[index]]),
      tags$td(tags$span(
        class = paste("llw-metadata-provenance", provenance_class),
        shown$layer[[index]]
      ))
    )
  })
  tags$div(
    class = "table-responsive llw-metadata-results-table",
    tags$table(
      class = "table table-hover table-sm align-middle mb-0",
      tags$caption(
        class = "visually-hidden",
        "Filtered metadata entries"
      ),
      tags$thead(tags$tr(
        tags$th(scope = "col", "Source"),
        tags$th(scope = "col", "Section"),
        tags$th(scope = "col", "Field"),
        tags$th(scope = "col", "Value"),
        tags$th(scope = "col", "Provenance")
      )),
      tags$tbody(body)
    )
  )
}

metadata_friendly_name <- function(value) {
  if (identical(value, "factual_metadata")) return("Dataset details")
  value <- gsub("[_-]+", " ", value)
  tools::toTitleCase(value)
}

metadata_group_path_segments <- function(group) {
  prefix <- paste0(group$namespace_id[[1L]], "/", group$resource[[1L]])
  path <- group$group_path[[1L]]
  relative <- if (startsWith(path, prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    path
  }
  relative <- sub("^/", "", relative)
  nested <- if (nzchar(relative)) {
    strsplit(relative, "/", fixed = TRUE)[[1L]]
  } else {
    character()
  }
  segments <- c(group$resource[[1L]], nested)
  if (
    length(segments) >= 5L &&
      identical(segments[[1L]], "metadata_bundle") &&
      identical(segments[[2L]], "sources")
  ) {
    records_boundary <- which(segments == "records")
    records_boundary <- records_boundary[records_boundary > 2L]
    if (
      length(records_boundary) > 0L &&
        records_boundary[[1L]] < length(segments)
    ) {
      segments <- segments[-seq_len(records_boundary[[1L]])]
    }
  }
  if (
    length(segments) == 0L ||
      !identical(utils::tail(segments, 1L), group$field[[1L]])
  ) {
    segments <- c(segments, group$field[[1L]])
  }
  context <- metadata_nonempty_text(group$group_context[[1L]])
  if (length(context) > 0L && length(segments) > 0L) {
    segments <- append(segments, context[[1L]], after = length(segments) - 1L)
  }
  segments
}

metadata_group_section_id <- function(group) {
  if (!is.data.frame(group) || nrow(group) != 1L) {
    abort_llw(
      "A metadata section can only be derived from one grouped field.",
      type = "validation"
    )
  }
  resource <- metadata_nonempty_text(group$resource[[1L]])
  if (length(resource) > 0L && !identical(resource[[1L]], "metadata_bundle")) {
    return(resource[[1L]])
  }
  segments <- metadata_group_path_segments(group)
  if (length(segments) > 0L) segments[[1L]] else "metadata"
}

metadata_section_label <- function(section_id) {
  switch(
    section_id,
    core_metadata = "Core Metadata",
    dataset_inventory = "Dataset Inventory",
    variable_inventory = "Variable Inventory",
    factual_metadata = "Dataset details",
    metadata_bundle = "Retained Source Metadata",
    metadata_friendly_name(section_id)
  )
}

metadata_section_index <- function(index) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  groups <- index$groups
  if (nrow(groups) == 0L) {
    return(data.frame(
      group_id = character(),
      namespace_id = character(),
      layer = character(),
      section_id = character(),
      section_label = character(),
      stringsAsFactors = FALSE
    ))
  }
  section_ids <- vapply(
    seq_len(nrow(groups)),
    function(row) metadata_group_section_id(groups[row, , drop = FALSE]),
    character(1)
  )
  data.frame(
    group_id = groups$group_id,
    namespace_id = groups$namespace_id,
    layer = groups$layer,
    section_id = section_ids,
    section_label = vapply(
      section_ids,
      metadata_section_label,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

metadata_section_choices <- function(
  section_index,
  namespace_id = "all",
  layer = "all"
) {
  keep <- rep(TRUE, nrow(section_index))
  if (!identical(namespace_id, "all")) {
    keep <- keep & section_index$namespace_id %in% namespace_id
  }
  if (!identical(layer, "all")) {
    keep <- keep & section_index$layer %in% layer
  }
  sections <- unique(
    section_index[keep, c("section_id", "section_label"), drop = FALSE]
  )
  if (nrow(sections) == 0L) return(c("All sections" = "all"))
  preferred <- match(
    sections$section_id,
    c("core_metadata", "dataset_inventory", "variable_inventory")
  )
  ordering <- order(
    is.na(preferred),
    preferred,
    sections$section_label,
    sections$section_id,
    method = "radix"
  )
  sections <- sections[ordering, , drop = FALSE]
  c(
    "All sections" = "all",
    stats::setNames(sections$section_id, sections$section_label)
  )
}

metadata_section_filter_visible <- function(choices) {
  length(setdiff(unique(unname(choices)), "all")) > 1L
}

metadata_index_query_groups_by_section <- function(
  index,
  section_index = NULL,
  query = "",
  namespace_id = NULL,
  section_id = "all",
  layer = NULL,
  sort_by = c("path", "field", "value", "source"),
  descending = FALSE,
  limit = 200L
) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  assert_scalar_string(section_id, "section_id")
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      limit < 1 ||
      limit != floor(limit)
  ) {
    abort_llw(
      "Metadata result limit must be a positive whole number.",
      type = "validation"
    )
  }
  if (is.null(section_index)) section_index <- metadata_section_index(index)
  result <- metadata_index_query_groups(
    index,
    query = query,
    namespace_id = namespace_id,
    resource = NULL,
    layer = layer,
    sort_by = match.arg(sort_by),
    descending = descending,
    limit = max(1L, nrow(index$groups))
  )
  rows <- result$rows
  if (!identical(section_id, "all")) {
    group_ids <- section_index$group_id[
      section_index$section_id == section_id
    ]
    rows <- rows[rows$group_id %in% group_ids, , drop = FALSE]
  }
  total <- nrow(rows)
  rows <- utils::head(rows, as.integer(limit))
  rownames(rows) <- NULL
  structure(
    list(rows = rows, total = total, limited = total > nrow(rows)),
    class = c("llw_metadata_group_query", "list")
  )
}

metadata_group_location_text <- function(group) {
  segments <- metadata_group_path_segments(group)
  paste(
    vapply(segments, metadata_friendly_name, character(1)),
    collapse = " / "
  )
}

# Add a current-value preview without modifying retained index/search facts.
metadata_present_groups <- function(groups, presentation = NULL) {
  if (is.null(presentation) || nrow(groups) == 0L) return(groups)
  position <- match(groups$group_id, presentation$groups$group_id)
  groups$has_override <- !is.na(position)
  groups$value_preview[
    groups$has_override
  ] <- presentation$groups$value_preview[position[groups$has_override]]
  groups
}

metadata_override_badge <- function(group) {
  if (!isTRUE(group$has_override[[1L]])) return(NULL)
  tags$span(
    class = "llw-metadata-provenance llw-metadata-provenance--overlay",
    "Includes overlay"
  )
}

metadata_group_choice_ui <- function(group, show_source = FALSE) {
  provenance_class <- if (identical(group$layer[[1L]], "LightLogWeb overlay")) {
    "llw-metadata-provenance--overlay"
  } else {
    "llw-metadata-provenance--source"
  }
  count_label <- if (group$record_count[[1L]] > 1L) {
    paste(format(group$record_count[[1L]], big.mark = ","), "records")
  } else if (group$entry_count[[1L]] > 1L) {
    paste(format(group$entry_count[[1L]], big.mark = ","), "values")
  } else {
    "1 value"
  }
  tags$div(
    class = "llw-metadata-result-option",
    tags$div(
      class = "llw-metadata-result-option__heading",
      tags$code(group$field[[1L]]),
      tags$span(class = "llw-metadata-result-option__count", count_label)
    ),
    tags$div(
      class = "llw-metadata-result-option__location",
      metadata_group_location_text(group)
    ),
    tags$div(
      class = "llw-metadata-result-option__value",
      group$value_preview[[1L]]
    ),
    tags$div(
      class = "llw-metadata-result-option__provenance",
      tags$span(
        class = paste("llw-metadata-provenance", provenance_class),
        if (identical(group$layer[[1L]], "LightLogWeb overlay")) {
          "Overlay"
        } else {
          "Source metadata"
        }
      ),
      metadata_override_badge(group),
      if (isTRUE(show_source)) tags$span(group$source_label[[1L]])
    )
  )
}

metadata_result_list_ui <- function(
  rows,
  input_id,
  selected = NULL,
  show_source = FALSE
) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) return(NULL)
  if (is.null(selected) || !selected %in% rows$group_id) {
    selected <- character()
  }
  tags$div(
    class = "llw-metadata-result-list",
    radioButtons(
      input_id,
      tags$span(class = "visually-hidden", "Matching metadata fields"),
      choiceNames = lapply(
        seq_len(nrow(rows)),
        function(index) {
          metadata_group_choice_ui(
            rows[index, , drop = FALSE],
            show_source = show_source
          )
        }
      ),
      choiceValues = rows$group_id,
      selected = selected,
      width = "100%"
    )
  )
}

metadata_group_location_ui <- function(group, show_source = FALSE) {
  segments <- metadata_group_path_segments(group)
  levels <- c(
    "Section",
    rep("Subsection", max(length(segments) - 2L, 0L)),
    if (length(segments) > 1L) "Field" else character()
  )
  context <- metadata_nonempty_text(group$group_context[[1L]])
  if (length(context) > 0L && length(levels) >= 2L) {
    levels[[length(levels) - 1L]] <- "Metadata item"
  }
  if (length(levels) != length(segments)) {
    levels <- c(rep("Subsection", length(segments) - 1L), "Field")
  }
  items <- lapply(seq_along(segments), function(index) {
    tags$li(
      class = if (index == length(segments)) "is-current" else NULL,
      `aria-current` = if (index == length(segments)) "page" else NULL,
      tags$span(
        class = "llw-metadata-location__level",
        levels[[index]]
      ),
      tags$strong(metadata_friendly_name(segments[[index]])),
      tags$code(segments[[index]])
    )
  })
  if (isTRUE(show_source)) {
    items <- c(
      list(tags$li(
        tags$span(class = "llw-metadata-location__level", "Source dataset"),
        tags$strong(group$source_label[[1L]]),
        tags$code(group$namespace_id[[1L]])
      )),
      items
    )
  }
  tags$section(
    class = "llw-metadata-location",
    tags$h4("Where this field lives"),
    tags$nav(
      `aria-label` = "Metadata hierarchy",
      tags$ol(items)
    )
  )
}

metadata_property_choice_ui <- function(group) {
  count <- if (group$record_count[[1L]] > 1L) {
    paste(format(group$record_count[[1L]], big.mark = ","), "records")
  } else if (group$entry_count[[1L]] > 1L) {
    paste(format(group$entry_count[[1L]], big.mark = ","), "values")
  } else {
    "1 value"
  }
  preview <- group$value_preview[[1L]]
  if (is.na(preview) || !nzchar(preview)) preview <- "not specified"
  tags$div(
    class = "llw-metadata-property-option",
    tags$div(
      class = "llw-metadata-property-option__heading",
      tags$strong(metadata_friendly_name(group$field[[1L]])),
      tags$span(count)
    ),
    tags$code(group$field[[1L]]),
    tags$span(class = "llw-metadata-property-option__preview", preview),
    metadata_override_badge(group)
  )
}

metadata_property_group_list_ui <- function(
  groups,
  input_id,
  selected_group_id,
  class = NULL,
  label = "Metadata item properties"
) {
  if (!is.data.frame(groups) || nrow(groups) == 0L) return(NULL)
  selected <- selected_group_id
  if (
    is.null(selected) ||
      length(selected) != 1L ||
      is.na(selected) ||
      !selected %in% groups$group_id
  ) {
    selected <- character()
  }
  tags$div(
    class = paste("llw-metadata-property-list", class),
    radioButtons(
      input_id,
      tags$span(class = "visually-hidden", label),
      choiceNames = lapply(seq_len(nrow(groups)), function(index) {
        metadata_property_choice_ui(groups[index, , drop = FALSE])
      }),
      choiceValues = groups$group_id,
      selected = selected,
      width = "100%"
    )
  )
}

metadata_hierarchy_context_neighborhood <- function(context) {
  if (
    is.null(context) ||
      !inherits(context, "llw_metadata_node_context")
  ) {
    return(NULL)
  }
  item_nodes <- context$exact_nodes
  if (!is.data.frame(item_nodes) || nrow(item_nodes) == 0L) {
    item_nodes <- context$node
  }
  representative_node_id <- if (nrow(item_nodes) > 0L) {
    item_nodes$node_id[[1L]]
  } else {
    ""
  }
  structure(
    list(
      selected_group_id = NULL,
      item_nodes = item_nodes,
      property_entries = context$entries,
      property_groups = context$groups,
      representative_node_id = representative_node_id
    ),
    class = c("llw_metadata_neighborhood", "list")
  )
}

metadata_item_properties_ui <- function(
  neighborhood,
  input_id,
  selected_group_id,
  children = NULL,
  child_input_id = metadata_hierarchy_action_id,
  show_areas = FALSE
) {
  if (
    !isTRUE(show_areas) &&
      (is.null(neighborhood) ||
        !inherits(neighborhood, "llw_metadata_neighborhood"))
  ) {
    return(NULL)
  }
  has_children <- is.data.frame(children) && nrow(children) > 0L
  groups <- neighborhood$property_groups
  has_fields <- is.data.frame(groups) && nrow(groups) > 0L
  properties <- if (!has_fields) {
    llw_status_callout(
      "idle",
      if (has_children) {
        "Open a contained item below to reach its fields. The previously selected field remains unchanged."
      } else {
        "No retained fields or deeper items are available here. Choose an earlier path level or use One level up."
      },
      heading = "No fields at this hierarchy focus",
      compact = TRUE
    )
  } else {
    metadata_property_group_list_ui(
      groups,
      input_id,
      selected_group_id
    )
  }
  tags$div(
    class = "llw-metadata-item-properties",
    `aria-label` = "Fields and contained metadata",
    if (!has_fields && !isTRUE(show_areas)) properties,
    tags$div(
      class = "llw-metadata-focus-contents",
      if (has_children)
        metadata_hierarchy_branch_list_ui(
          children,
          child_input_id
        ),
      if (has_fields) properties
    )
  )
}

metadata_item_count_ui <- function(neighborhood) {
  if (is.null(neighborhood)) return(NULL)
  instance_count <- nrow(neighborhood$item_nodes)
  if (instance_count == 0L) return(NULL)
  tags$span(
    class = "llw-metadata-relationship-badge",
    paste(
      format(instance_count, big.mark = ","),
      if (instance_count == 1L) "retained item" else "retained items"
    )
  )
}

metadata_hierarchy_action_id <- function(node_id) {
  if (length(node_id) == 0L) return(character())
  paste0("hierarchy_enter_", node_id)
}

metadata_hierarchy_branch_list_ui <- function(nodes, input_id) {
  enter_icon <- icon("chevron-right")
  tags$div(
    class = "llw-metadata-branch-list",
    role = "group",
    `aria-label` = "Contained metadata",
    lapply(seq_len(nrow(nodes)), function(index) {
      node <- nodes[index, , drop = FALSE]
      actionButton(
        input_id(node$node_id[[1L]]),
        label = metadata_hierarchy_node_choice_ui(node),
        icon = enter_icon,
        `aria-label` = paste("Open", metadata_friendly_name(node$label[[1L]])),
        class = "llw-metadata-branch-action"
      )
    })
  )
}

metadata_hierarchy_kind_label <- function(kind) {
  switch(
    kind,
    source = "Source dataset",
    resource = "Section",
    group = "Metadata group",
    table = "Collection",
    record = "Metadata item",
    "Hierarchy node"
  )
}

metadata_hierarchy_node_choice_ui <- function(
  node,
  show_anchor = FALSE,
  show_instance = FALSE
) {
  label <- node$label[[1L]]
  if (is.na(label) || !nzchar(label)) label <- "Unnamed metadata node"
  facts <- character()
  logical_item_count <- if ("logical_item_count" %in% names(node)) {
    node$logical_item_count[[1L]]
  } else {
    1L
  }
  if (logical_item_count > 1L) {
    facts <- c(
      facts,
      paste(
        format(logical_item_count, big.mark = ","),
        "grouped items"
      )
    )
  }
  if (node$direct_value_count[[1L]] > 0L) {
    facts <- c(
      facts,
      paste(
        format(node$direct_value_count[[1L]], big.mark = ","),
        if (node$direct_value_count[[1L]] == 1L) "value" else "values"
      )
    )
  }
  if (node$child_node_count[[1L]] > 0L) {
    facts <- c(
      facts,
      paste(
        format(node$child_node_count[[1L]], big.mark = ","),
        if (node$child_node_count[[1L]] == 1L) "child" else "children"
      )
    )
  }
  tags$div(
    class = "llw-metadata-node-option",
    if (isTRUE(show_anchor)) {
      tags$span(
        class = "llw-metadata-node-option__anchor",
        paste("Nearby under", node$anchor_label[[1L]])
      )
    },
    if (isTRUE(show_instance) && "instance_record" %in% names(node)) {
      tags$span(
        class = "llw-metadata-node-option__anchor",
        paste("Retained record", node$instance_record[[1L]])
      )
    },
    tags$div(
      class = "llw-metadata-node-option__heading",
      tags$strong(metadata_friendly_name(label)),
      tags$span(metadata_hierarchy_kind_label(node$kind[[1L]]))
    ),
    if (length(facts) > 0L) {
      tags$span(
        class = "llw-metadata-node-option__counts",
        paste(facts, collapse = " | ")
      )
    }
  )
}

metadata_hierarchy_node_list_ui <- function(
  nodes,
  input_id,
  label,
  selected = character(),
  class = NULL,
  show_anchor = FALSE,
  show_instance = FALSE
) {
  if (!is.data.frame(nodes) || nrow(nodes) == 0L) return(NULL)
  if (
    length(selected) != 1L ||
      is.na(selected) ||
      !selected %in% nodes$node_id
  ) {
    selected <- character()
  }
  tags$div(
    class = paste("llw-metadata-node-list", class),
    radioButtons(
      input_id,
      tags$span(class = "visually-hidden", label),
      choiceNames = lapply(seq_len(nrow(nodes)), function(index) {
        metadata_hierarchy_node_choice_ui(
          nodes[index, , drop = FALSE],
          show_anchor = show_anchor,
          show_instance = show_instance
        )
      }),
      choiceValues = nodes$node_id,
      selected = selected,
      width = "100%"
    )
  )
}

metadata_hierarchy_collapse_nodes <- function(index, nodes) {
  if (!is.data.frame(nodes)) {
    abort_llw("Metadata hierarchy nodes are invalid.", type = "validation")
  }
  nodes <- metadata_index_decorate_nodes(index, nodes)
  if (nrow(nodes) == 0L) {
    nodes$logical_item_count <- integer()
    return(nodes)
  }
  key_fields <- c("namespace_id", "layer", "resource", "kind", "label")
  key_parts <- lapply(key_fields, function(field) {
    value <- as.character(nodes[[field]])
    value[is.na(value)] <- "<missing>"
    value
  })
  keys <- do.call(paste, c(key_parts, sep = "\r"))
  logical_keys <- unique(keys)
  result <- nodes[match(logical_keys, keys), , drop = FALSE]
  result$logical_item_count <- integer(nrow(result))
  for (index_row in seq_len(nrow(result))) {
    members <- which(keys == logical_keys[[index_row]])
    result$logical_item_count[[index_row]] <- length(members)
    result$direct_value_count[[index_row]] <- sum(
      nodes$direct_value_count[members],
      na.rm = TRUE
    )
    result$child_node_count[[index_row]] <- sum(
      nodes$child_node_count[members],
      na.rm = TRUE
    )
    if (length(members) > 1L) result$record_identity[[index_row]] <- ""
  }
  rownames(result) <- NULL
  result
}

metadata_hierarchy_same_level_nodes <- function(index, node_row) {
  nodes <- index$nodes
  node <- nodes[node_row, , drop = FALSE]
  parent_id <- node$parent_node_id[[1L]]
  if (is.na(parent_id) || !nzchar(parent_id)) {
    return(metadata_index_decorate_nodes(index, node))
  }
  same <- nodes$parent_node_id == parent_id
  for (field in c("namespace_id", "layer", "resource", "kind", "label")) {
    target <- node[[field]][[1L]]
    values <- nodes[[field]]
    same_field <- if (is.na(target)) {
      is.na(values)
    } else {
      !is.na(values) & values == target
    }
    same <- same & same_field
  }
  same[is.na(same)] <- FALSE
  peers <- nodes[which(same), , drop = FALSE]
  ordering <- order(peers$path, peers$node_id, method = "radix")
  peers <- peers[ordering, , drop = FALSE]
  rownames(peers) <- NULL
  metadata_index_decorate_nodes(index, peers)
}

metadata_hierarchy_node_context <- function(index, node_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  assert_scalar_string(node_id, "node_id")
  node_row <- match(node_id, index$nodes$node_id)
  if (is.na(node_row)) return(NULL)

  exact_nodes <- metadata_hierarchy_same_level_nodes(index, node_row)
  node <- metadata_hierarchy_collapse_nodes(index, exact_nodes)
  node <- node[1L, , drop = FALSE]
  exact_node_ids <- exact_nodes$node_id
  entry_rows <- which(index$entries$node_id %in% exact_node_ids)
  entries <- index$entries[entry_rows, , drop = FALSE]
  if (nrow(entries) > 0L) {
    ordering <- order(entries$path, entries$entry_id, method = "radix")
    entries <- entries[ordering, , drop = FALSE]
  }
  group_ids <- unique(entries$group_id)
  group_rows <- match(group_ids, index$groups$group_id)
  groups <- index$groups[group_rows[!is.na(group_rows)], , drop = FALSE]
  child_rows <- which(index$nodes$parent_node_id %in% exact_node_ids)
  children <- metadata_hierarchy_collapse_nodes(
    index,
    index$nodes[child_rows, , drop = FALSE]
  )
  ancestors <- metadata_index_decorate_nodes(
    index,
    metadata_index_node_ancestors(index, node$node_id[[1L]])
  )
  ancestors$logical_item_count <- 1L
  current_row <- match(node$node_id[[1L]], ancestors$node_id)
  if (!is.na(current_row)) {
    shared <- intersect(names(ancestors), names(node))
    ancestors[current_row, shared] <- node[1L, shared]
  }
  rownames(entries) <- NULL
  rownames(groups) <- NULL
  structure(
    list(
      node = node,
      exact_nodes = exact_nodes,
      ancestors = ancestors,
      entries = entries,
      groups = groups,
      children = children
    ),
    class = c("llw_metadata_node_context", "list")
  )
}

metadata_hierarchy_logical_roots <- function(index, group) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  if (is.character(group)) group <- metadata_index_group(index, group)
  if (is.null(group) || !inherits(group, "llw_metadata_group")) {
    return(index$nodes[FALSE, , drop = FALSE])
  }
  selected <- group$group
  nodes <- index$nodes
  scope_rows <- which(
    nodes$namespace_id == selected$namespace_id[[1L]] &
      nodes$layer == selected$layer[[1L]]
  )
  if (length(scope_rows) == 0L) return(nodes[FALSE, , drop = FALSE])

  record_containers <- nodes$node_id[scope_rows[
    nodes$kind[scope_rows] == "group" &
      nodes$label[scope_rows] == "records" &
      grepl("/records$", nodes$path[scope_rows])
  ]]
  root_rows <- scope_rows[
    nodes$parent_node_id[scope_rows] %in% record_containers
  ]

  resource_rows <- scope_rows[
    nodes$kind[scope_rows] == "resource" &
      nodes$label[scope_rows] != "metadata_bundle"
  ]
  if (length(resource_rows) > 0L) {
    child_rows <- scope_rows[
      nodes$kind[scope_rows] %in% c("group", "table")
    ]
    child_parents <- nodes$parent_node_id[child_rows]
    child_match <- match(nodes$node_id[resource_rows], child_parents)
    duplicate_children <- child_rows[child_match]
    unique_child_parent <- !duplicated(child_parents) &
      !duplicated(child_parents, fromLast = TRUE)
    valid_duplicate <- !is.na(child_match) &
      unique_child_parent[child_match] &
      nodes$path[duplicate_children] == nodes$path[resource_rows] &
      nodes$label[duplicate_children] == nodes$label[resource_rows]
    duplicate_children[!valid_duplicate] <- NA_integer_
    root_rows <- c(
      root_rows,
      ifelse(is.na(duplicate_children), resource_rows, duplicate_children)
    )
  }

  root_rows <- unique(root_rows)
  if (length(root_rows) == 0L && nrow(group$entries) > 0L) {
    ancestors <- metadata_index_node_ancestors(
      index,
      group$entries$node_id[[1L]]
    )
    meaningful <- which(
      !ancestors$kind %in% c("source", "resource") &
        !ancestors$label %in% c("metadata_bundle", "sources", "records")
    )
    if (length(meaningful) > 0L) {
      root_rows <- match(
        ancestors$node_id[[meaningful[[1L]]]],
        nodes$node_id
      )
    }
  }
  root_rows <- root_rows[!is.na(root_rows)]
  roots <- nodes[root_rows, , drop = FALSE]
  if (nrow(roots) == 0L) return(roots)

  key <- paste(roots$path, roots$label, sep = "\r")
  roots <- roots[!duplicated(key), , drop = FALSE]
  known_order <- match(
    roots$label,
    c("core_metadata", "dataset_inventory", "variable_inventory"),
    nomatch = 4L
  )
  ordering <- order(known_order, roots$path, roots$node_id, method = "radix")
  roots <- roots[ordering, , drop = FALSE]
  rownames(roots) <- NULL
  roots
}

metadata_hierarchy_active_root_id <- function(context, roots) {
  if (
    is.null(context) ||
      !inherits(context, "llw_metadata_node_context") ||
      !is.data.frame(roots) ||
      nrow(roots) == 0L
  ) {
    return("")
  }
  candidates <- context$ancestors$node_id[
    context$ancestors$node_id %in% roots$node_id
  ]
  if (length(candidates) == 0L) "" else candidates[[1L]]
}

metadata_hierarchy_visible_path <- function(context, roots) {
  if (is.null(context) || !inherits(context, "llw_metadata_node_context")) {
    return(data.frame())
  }
  path <- context$ancestors
  active_root <- metadata_hierarchy_active_root_id(context, roots)
  if (nzchar(active_root)) {
    start <- match(active_root, path$node_id)
    path <- if (start < nrow(path)) {
      path[seq.int(start + 1L, nrow(path)), , drop = FALSE]
    } else {
      path[FALSE, , drop = FALSE]
    }
  } else {
    keep <- !path$kind %in% c("source") &
      !path$label %in% c("metadata_bundle", "sources", "records")
    path <- path[keep, , drop = FALSE]
  }
  if (nrow(path) == 0L) return(path)
  duplicate_key <- paste(path$path, path$label, sep = "\r")
  remove_wrapper <- path$kind == "resource" &
    duplicated(duplicate_key, fromLast = TRUE)
  path <- path[!remove_wrapper, , drop = FALSE]
  rownames(path) <- NULL
  path
}

metadata_hierarchy_full_path <- function(context, roots) {
  if (is.null(context) || !inherits(context, "llw_metadata_node_context")) {
    return(data.frame())
  }
  path <- context$ancestors
  active_root <- metadata_hierarchy_active_root_id(context, roots)
  if (nzchar(active_root)) {
    path <- path[
      seq.int(match(active_root, path$node_id), nrow(path)),
      ,
      drop = FALSE
    ]
  } else {
    path <- path[
      !path$kind %in% c("source") &
        !path$label %in% c("metadata_bundle", "sources", "records"),
      ,
      drop = FALSE
    ]
  }
  duplicate_key <- paste(path$path, path$label, sep = "\r")
  path <- path[
    !(path$kind == "resource" & duplicated(duplicate_key, fromLast = TRUE)),
    ,
    drop = FALSE
  ]
  rownames(path) <- NULL
  path
}

metadata_hierarchy_parent_id <- function(context, roots) {
  if (is.null(context) || !inherits(context, "llw_metadata_node_context")) {
    return(NULL)
  }
  path <- metadata_hierarchy_full_path(context, roots)
  if (nrow(path) <= 1L) return("metadata_areas")
  path$node_id[[nrow(path) - 1L]]
}

metadata_hierarchy_path_ui <- function(path, input_id, compact = TRUE) {
  if (nrow(path) == 0L) return(NULL)
  shown <- if (isTRUE(compact)) utils::tail(path, 4L) else path
  tags$div(
    class = paste(
      "llw-metadata-breadcrumbs",
      if (isTRUE(compact)) "is-compact" else "is-full"
    ),
    style = paste0(
      "--llw-path-levels: ",
      nrow(shown),
      "; --llw-path-small-levels: ",
      min(1L, nrow(shown)),
      ";"
    ),
    radioButtons(
      input_id,
      tags$span(
        class = "visually-hidden",
        "Jump to a level in the metadata path"
      ),
      choiceNames = lapply(seq_len(nrow(shown)), function(index) {
        node <- shown[index, , drop = FALSE]
        label <- metadata_friendly_name(node$label[[1L]])
        tags$span(
          class = "llw-metadata-breadcrumb",
          title = paste(metadata_hierarchy_kind_label(node$kind[[1L]]), label),
          tags$span(
            class = "llw-metadata-location__level",
            metadata_hierarchy_kind_label(node$kind[[1L]])
          ),
          tags$strong(label),
          tags$code(node$label[[1L]])
        )
      }),
      choiceValues = shown$node_id,
      selected = character(),
      width = "100%"
    )
  )
}

metadata_hierarchy_context_ui <- function(
  context,
  roots,
  input_ids,
  selected_group_id
) {
  if (is.null(context) || !inherits(context, "llw_metadata_node_context")) {
    return(NULL)
  }
  path <- metadata_hierarchy_full_path(context, roots)
  tags$div(
    class = "llw-app llw-metadata-navigation-dialog",
    tags$p(
      "Choose any level to jump there. The properties below the path follow your focus."
    ),
    metadata_hierarchy_path_ui(path, input_ids$path, compact = FALSE)
  )
}

metadata_hierarchy_summary_ui <- function(group, context) {
  if (is.null(group) || is.null(context)) {
    return(tags$span("Choose a matching field first"))
  }
  tags$span(
    class = "llw-metadata-hierarchy-summary__state",
    paste("Hierarchy focus:", metadata_friendly_name(context$node$label[[1L]]))
  )
}

metadata_hierarchy_shell_ui <- function(ns, field_actions = NULL) {
  card(
    id = ns("hierarchy_focus"),
    class = "llw-metadata-path-card llw-metadata-navigation-card",
    tabindex = "-1",
    `aria-label` = "Metadata explorer",
    card_header(
      class = "llw-metadata-navigation-header",
      tags$span("Metadata explorer"),
      bslib::popover(
        tags$button(
          id = ns("explorer_help_button"),
          type = "button",
          class = "btn btn-link llw-icon-button",
          `aria-label` = "How to use the metadata explorer",
          icon("circle-question")
        ),
        paste(
          "Arrows open branches. Radios select a field without leaving this level.",
          "Use One level up or a path block to move back.",
          "The left \u2026 opens the full path.",
          "Values stay within one exact source namespace and are never reconciled or inferred."
        ),
        title = "Explore metadata",
        id = ns("explorer_help"),
        placement = "bottom",
        options = list(
          trigger = "click",
          container = "body",
          animation = FALSE,
          customClass = "llw-metadata-help-popup"
        )
      )
    ),
    card_body(
      class = "llw-metadata-navigation-body",
      uiOutput(ns("hierarchy_content")),
      tags$div(
        class = "llw-metadata-navigation-toolbar",
        actionButton(
          ns("hierarchy_up"),
          "One level up",
          icon = icon("arrow-up"),
          disabled = TRUE,
          class = "btn-outline-secondary"
        ),
        field_actions,
        uiOutput(ns("item_count"), class = "llw-metadata-item-count")
      ),
      uiOutput(ns("item_properties"))
    )
  )
}

metadata_hierarchy_panel_ui <- function(
  group,
  context,
  roots,
  input_ids = NULL,
  show_areas = FALSE
) {
  if (isTRUE(show_areas) && !is.null(group)) {
    return(tags$div(
      class = "llw-metadata-hierarchy-panel llw-metadata-location",
      `data-selected-group` = group$group$group_id[[1L]],
      tags$span(class = "llw-metadata-location__level", "Navigation"),
      tags$strong("Metadata areas")
    ))
  }
  if (is.null(group) || is.null(context)) {
    return(tags$p(
      class = "llw-secondary mb-0",
      "Choose a matching field to see its path and explore nearby metadata."
    ))
  }
  input_ids <- utils::modifyList(
    list(
      path = "metadata_hierarchy_path_node_id",
      full_path = "metadata_hierarchy_full_path"
    ),
    input_ids %||% list()
  )
  path <- metadata_hierarchy_full_path(context, roots)
  tags$div(
    class = "llw-metadata-hierarchy-panel llw-metadata-location",
    `data-selected-group` = group$group$group_id[[1L]],
    tags$div(
      class = "llw-metadata-path-row",
      actionButton(
        input_ids$full_path,
        label = tags$span(`aria-hidden` = "true", "\u2026"),
        `aria-label` = "Show full metadata path",
        title = "Show the full path and jump to an earlier level",
        class = paste(
          "btn-outline-secondary llw-metadata-path-overflow",
          if (nrow(path) > 4L) "has-overflow",
          if (nrow(path) > 1L) "has-mobile-overflow"
        )
      ),
      metadata_hierarchy_path_ui(path, input_ids$path)
    )
  )
}

metadata_navigation_dependency <- function() {
  htmltools::htmlDependency(
    name = "llw-metadata-navigation",
    version = "1.2.5",
    src = c(file = lightlogweb_asset_dir()),
    script = "js/metadata-navigation.js",
    all_files = FALSE
  )
}
metadata_group_values_ui <- function(group, presentation = NULL) {
  entries <- group$entries
  values <- entries$value
  values[is.na(values) | !nzchar(values)] <- "not specified"
  source_values <- values
  position <- match(entries$path, presentation$entries$path)
  overridden <- !is.na(position)
  values[overridden] <- presentation$entries$value[position[overridden]]
  types <- entries$value_type
  types[overridden] <- presentation$entries$value_type[position[overridden]]
  has_overrides <- any(overridden)
  semantic_items <- entries$group_context
  semantic_items[is.na(semantic_items)] <- ""
  show_semantic_item <- any(nzchar(semantic_items))
  record_heading <- if (
    any(grepl("/dataset_file", entries$path, fixed = TRUE)) ||
      any(grepl("^[^:]+:[^:]+$", entries$record))
  ) {
    "Dataset / file group"
  } else {
    "Record"
  }
  rows <- lapply(seq_len(nrow(entries)), function(index) {
    cells <- list(tags$td(tags$code(entries$record[[index]])))
    if (show_semantic_item) {
      cells <- c(
        cells,
        list(tags$td(
          if (nzchar(semantic_items[[index]])) {
            tags$code(semantic_items[[index]])
          } else {
            "not specified"
          }
        ))
      )
    }
    cells <- c(
      cells,
      list(
        tags$td(values[[index]]),
        if (has_overrides)
          tags$td(
            tags$span(
              class = paste(
                "llw-metadata-provenance",
                if (overridden[[index]]) "llw-metadata-provenance--overlay" else
                  "llw-metadata-provenance--source"
              ),
              if (overridden[[index]]) "Overlay" else "Source"
            )
          ),
        if (has_overrides) tags$td(source_values[[index]]),
        tags$td(types[[index]])
      )
    )
    do.call(tags$tr, cells)
  })
  headings <- list(tags$th(scope = "col", record_heading))
  if (show_semantic_item) {
    headings <- c(
      headings,
      list(tags$th(scope = "col", "Semantic item"))
    )
  }
  headings <- c(
    headings,
    list(
      tags$th(scope = "col", if (has_overrides) "Current value" else "Value"),
      if (has_overrides) tags$th(scope = "col", "Value from"),
      if (has_overrides) tags$th(scope = "col", "Original source value"),
      tags$th(scope = "col", "Type")
    )
  )
  tagList(
    tags$div(
      class = "table-responsive llw-metadata-group-values",
      tags$table(
        class = "table table-hover table-sm align-middle mb-0",
        tags$caption(
          class = "visually-hidden",
          if (has_overrides)
            "Record identities, current values and retained source values for the selected metadata field" else
            "Retained metadata identities and values for the selected metadata field"
        ),
        tags$thead(do.call(tags$tr, headings)),
        tags$tbody(rows)
      )
    ),
    tags$details(
      class = "llw-metadata-exact-paths",
      tags$summary(
        paste(
          "Exact retained paths",
          paste0("(", format(nrow(entries), big.mark = ","), ")")
        )
      ),
      tags$ul(lapply(seq_len(nrow(entries)), function(index) {
        tags$li(
          tags$span(entries$record[[index]]),
          tags$code(entries$path[[index]])
        )
      }))
    )
  )
}

metadata_display_source_fingerprint <- function(
  bundle,
  namespace_id,
  path = NULL
) {
  bundle <- validate_metadata_bundle(bundle)
  assert_scalar_string(namespace_id, "namespace_id")
  if (!namespace_id %in% names(bundle$sources)) {
    abort_llw(
      "The metadata source namespace is unavailable.",
      type = "validation"
    )
  }
  if (!is.null(path)) assert_scalar_string(path, "path")

  source <- bundle$sources[[namespace_id]]
  nested_depth <- 0L
  repeat {
    nested <- source$records$metadata_bundle %||% NULL
    if (!inherits(nested, "llw_metadata_bundle")) break
    nested <- validate_metadata_bundle(nested)
    nested_ids <- names(nested$sources)
    matches <- if (is.null(path)) {
      if (length(nested_ids) == 1L) nested_ids else character()
    } else {
      nested_ids[vapply(
        nested_ids,
        function(id) grepl(paste0("/", id, "/"), path, fixed = TRUE),
        logical(1)
      )]
    }
    if (length(matches) != 1L) break
    source <- nested$sources[[matches[[1L]]]]
    nested_depth <- nested_depth + 1L
  }

  list(
    fingerprint = source$fingerprint,
    label = if (nested_depth > 0L) {
      "Retained source fingerprint"
    } else if (
      inherits(
        bundle$sources[[namespace_id]]$records$metadata_bundle %||% NULL,
        "llw_metadata_bundle"
      )
    ) {
      "Namespace wrapper fingerprint"
    } else {
      "Source fingerprint"
    }
  )
}

metadata_group_detail_ui <- function(
  group,
  bundle,
  show_source = FALSE,
  presentation = NULL
) {
  if (is.null(group) || !inherits(group, "llw_metadata_group")) {
    return(llw_status_callout(
      "idle",
      "Choose a field in the matching-results list to inspect it.",
      heading = "No metadata field selected",
      compact = TRUE
    ))
  }
  row <- group$group
  is_overlay <- identical(
    row$namespace_id[[1L]],
    metadata_overlay_namespace_id()
  )
  fingerprint_contract <- if (is_overlay) {
    list(
      fingerprint = bundle$overlay$source_fingerprint,
      label = "Bound source fingerprint"
    )
  } else {
    metadata_display_source_fingerprint(
      bundle,
      row$namespace_id[[1L]],
      row$group_path[[1L]]
    )
  }
  provenance_class <- if (is_overlay) {
    "llw-metadata-provenance--overlay"
  } else {
    "llw-metadata-provenance--source"
  }
  provenance_label <- if (is_overlay) {
    paste("LightLogWeb overlay revision", bundle$overlay$revision)
  } else {
    "Immutable source metadata"
  }
  card(
    class = "llw-metadata-detail-card",
    card_header("Selected metadata field"),
    tags$section(
      class = "llw-metadata-values-section",
      tags$h4("Values in this field"),
      tags$code(row$field[[1L]]),
      tags$p(
        class = "llw-secondary",
        paste(
          "Repeated records are kept together here rather than shown as",
          "duplicate search results. No values are reconciled or inferred."
        )
      ),
      if (!is_overlay && any(group$entries$path %in% presentation$entries$path))
        tags$p(
          class = "llw-secondary",
          paste(
            "Current values include explicitly committed overlay assertions at revision",
            presentation$revision,
            ". Original source values are retained unchanged alongside them."
          )
        ),
      metadata_group_values_ui(group, presentation)
    ),
    tags$h4(class = "llw-metadata-field-description", "About this field"),
    tags$dl(
      class = "llw-metadata-detail-list",
      tags$div(tags$dt("Field"), tags$dd(tags$code(row$field[[1L]]))),
      if (isTRUE(show_source))
        tags$div(
          tags$dt(if (is_overlay) "Metadata layer" else "Source dataset"),
          tags$dd(row$source_label[[1L]])
        ),
      tags$div(
        tags$dt("Retained values"),
        tags$dd(format(row$entry_count[[1L]], big.mark = ","))
      ),
      tags$div(
        tags$dt("Records represented"),
        tags$dd(format(row$record_count[[1L]], big.mark = ","))
      ),
      tags$div(tags$dt("Value type"), tags$dd(row$value_type[[1L]])),
      tags$div(
        tags$dt("Provenance"),
        tags$dd(tags$span(
          class = paste("llw-metadata-provenance", provenance_class),
          provenance_label
        ))
      ),
      tags$div(
        tags$dt(fingerprint_contract$label),
        tags$dd(tags$code(paste0(
          substr(fingerprint_contract$fingerprint, 1L, 23L),
          "..."
        )))
      )
    ),
    tags$details(
      class = "llw-metadata-group-path",
      tags$summary("Exact grouped location"),
      tags$code(row$group_path[[1L]])
    ),
    tags$p(
      class = "llw-secondary small mb-0",
      if (is_overlay) {
        paste(
          "This versioned LightLogWeb value is separate from the immutable",
          "source and does not change or revalidate its package."
        )
      } else {
        paste(
          "LightLogWeb displays these retained values without changing or",
          "revalidating the source package."
        )
      }
    )
  )
}

metadata_entry_breadcrumb_ui <- function(entry) {
  parts <- data.frame(
    level = c("Source", "Section", "Record", "Field"),
    value = c(
      entry$source_label[[1L]],
      entry$resource[[1L]],
      entry$record[[1L]],
      entry$field[[1L]]
    ),
    stringsAsFactors = FALSE
  )
  parts <- parts[!is.na(parts$value) & nzchar(parts$value), , drop = FALSE]
  tags$nav(
    class = "llw-metadata-breadcrumb",
    `aria-label` = "Metadata location",
    tags$ol(lapply(seq_len(nrow(parts)), function(index) {
      attributes <- list(
        class = if (index == nrow(parts)) "is-current" else NULL
      )
      if (index == nrow(parts)) attributes$`aria-current` <- "page"
      do.call(
        tags$li,
        c(
          attributes,
          list(
            tags$span(
              class = "llw-metadata-breadcrumb__level",
              paste0(parts$level[[index]], ":")
            ),
            tags$code(parts$value[[index]])
          )
        )
      )
    }))
  )
}

metadata_entry_detail_ui <- function(entry, bundle) {
  if (is.null(entry) || nrow(entry) == 0L) {
    return(llw_status_callout(
      "idle",
      "Choose a result to see its full source path and provenance.",
      heading = "No metadata result selected",
      compact = TRUE
    ))
  }
  value <- entry$value[[1L]]
  if (is.na(value) || !nzchar(value)) value <- "not specified"
  is_overlay <- identical(
    entry$namespace_id[[1L]],
    metadata_overlay_namespace_id()
  )
  fingerprint_contract <- if (is_overlay) {
    list(
      fingerprint = bundle$overlay$source_fingerprint,
      label = "Bound source fingerprint"
    )
  } else {
    metadata_display_source_fingerprint(
      bundle,
      entry$namespace_id[[1L]],
      entry$path[[1L]]
    )
  }
  provenance_class <- if (is_overlay) {
    "llw-metadata-provenance--overlay"
  } else {
    "llw-metadata-provenance--source"
  }
  provenance_label <- if (is_overlay) {
    paste("LightLogWeb overlay revision", bundle$overlay$revision)
  } else {
    "Immutable source"
  }
  contract_label <- if (is_overlay) {
    paste("Overlay contract", bundle$overlay$overlay_version)
  } else {
    metadata_state_label(entry$state[[1L]])
  }
  card(
    class = "llw-metadata-detail-card",
    card_header("Selected metadata value"),
    metadata_entry_breadcrumb_ui(entry),
    tags$dl(
      class = "llw-metadata-detail-list",
      tags$div(tags$dt("Field"), tags$dd(tags$code(entry$field[[1L]]))),
      tags$div(tags$dt("Value"), tags$dd(value)),
      tags$div(tags$dt("Value type"), tags$dd(entry$value_type[[1L]])),
      tags$div(
        tags$dt("Exact source path"),
        tags$dd(tags$code(entry$path[[1L]]))
      ),
      tags$div(
        tags$dt(if (is_overlay) "Overlay contract" else "Source state"),
        tags$dd(contract_label)
      ),
      tags$div(
        tags$dt("Provenance"),
        tags$dd(
          tags$span(
            class = paste("llw-metadata-provenance", provenance_class),
            provenance_label
          )
        )
      ),
      tags$div(
        tags$dt(fingerprint_contract$label),
        tags$dd(tags$code(paste0(
          substr(fingerprint_contract$fingerprint, 1L, 23L),
          "..."
        )))
      )
    ),
    tags$p(
      class = "llw-secondary small mb-0",
      if (is_overlay) {
        paste(
          "This versioned LightLogWeb value is separate from the immutable",
          "source and does not change or revalidate its package."
        )
      } else {
        paste(
          "LightLogWeb displays this retained value without changing or",
          "revalidating the source package."
        )
      }
    )
  )
}

metadata_fixture_contract_label <- function(selection) {
  if (!is.list(selection)) return(NULL)
  variables <- metadata_nonempty_text(selection$requested_variables)
  if (identical(selection$request_mode %||% NULL, "all")) {
    return(paste(
      "All variables from the selected compatible unit",
      paste0("(", length(variables), " source variables)")
    ))
  }
  if (identical(variables, "MEDI")) {
    return("MEDI only (exact source variable: MEDI)")
  }
  if (length(variables) > 0L) {
    return(paste0("Explicit subset: ", paste(variables, collapse = ", ")))
  }
  NULL
}

metadata_fixture_dataset_record_id <- function(fixture_id) {
  assert_scalar_string(fixture_id, "fixture_id")
  ids <- c(
    `tum-medi-only` = "dataset_m7tummedionly",
    `iztech-light-subset` = "dataset_m7iztechlightsubset",
    `ucr-all-light-variables` = "dataset_m7ucrallvariables"
  )
  id <- unname(ids[fixture_id])
  if (length(id) != 1L || is.na(id)) {
    abort_llw(
      paste0("Unknown Milestone 7 fixture identity `", fixture_id, "`."),
      type = "validation"
    )
  }
  id
}

metadata_append_review_contract <- function(sources) {
  if (!is.list(sources) || length(sources) != 2L) {
    abort_llw(
      "The offline append review state requires exactly two fixture sources.",
      type = "validation"
    )
  }
  required <- c(
    "fixture_id",
    "label",
    "dataset_record_id",
    "package_dataset_ids",
    "retained_rows",
    "citation",
    "repository",
    "exact_revision",
    "license",
    "package_schema_version"
  )
  scalar_text_fields <- setdiff(
    required,
    c("package_dataset_ids", "retained_rows")
  )
  sources <- lapply(sources, function(source) {
    if (!is.list(source) || !all(required %in% names(source))) {
      abort_llw(
        "An offline append review source descriptor is incomplete.",
        type = "validation"
      )
    }
    for (field in scalar_text_fields) {
      assert_scalar_string(source[[field]], paste0("source$", field))
    }
    dataset_ids <- metadata_nonempty_text(source$package_dataset_ids)
    if (length(dataset_ids) != 5L || anyDuplicated(dataset_ids)) {
      abort_llw(
        "An offline review source must retain five source dataset identities.",
        type = "validation"
      )
    }
    if (
      !is.numeric(source$retained_rows) ||
        length(source$retained_rows) != 1L ||
        is.na(source$retained_rows) ||
        source$retained_rows < 1L
    ) {
      abort_llw(
        "An offline review source retained-row count is invalid.",
        type = "validation"
      )
    }
    source$package_dataset_ids <- dataset_ids
    source$retained_rows <- as.integer(source$retained_rows)
    source[required]
  })
  fixture_ids <- unname(vapply(
    sources,
    function(source) source$fixture_id,
    character(1)
  ))
  if (!identical(fixture_ids, c("tum-medi-only", "iztech-light-subset"))) {
    abort_llw(
      "The offline append review state must retain the TUM and IZTECH fixtures in contract order.",
      type = "validation"
    )
  }
  list(
    review_state_id = "tum-iztech-appended",
    label = "TUM and IZTECH appended offline metadata review",
    derived_fixture = TRUE,
    generated_data_file = NULL,
    construction = paste(
      "Constructed in memory from two frozen offline RDS fixtures through",
      "LightLogWeb's append contract. No source package is reopened."
    ),
    sources = sources,
    source_variable_mappings = list(
      list(
        fixture_id = "tum-medi-only",
        source_variable = "MEDI",
        result_column = "tum_MEDI"
      ),
      list(
        fixture_id = "iztech-light-subset",
        source_variable = "MEDI",
        result_column = "iztech_MEDI"
      ),
      list(
        fixture_id = "iztech-light-subset",
        source_variable = "LIGHT",
        result_column = "iztech_LIGHT"
      )
    ),
    retained_rows = sum(vapply(
      sources,
      function(source) source$retained_rows,
      integer(1)
    )),
    participant_policy = "prefix_source",
    time_alignment = "preserve_instant",
    lineage_column = "llw_source_dataset_id",
    namespace_policy = paste(
      "One exact namespace per immediate source dataset. Source fields and",
      "measurement columns are not flattened or semantically reconciled."
    ),
    overlay = list(
      revision = 1L,
      source_bound = TRUE,
      table = "datasets",
      key = "dataset_m7appendedreview",
      field = "title",
      value = "Offline appended metadata review",
      source_mutated = FALSE
    ),
    offline_test_policy = list(
      source_package_reopen = FALSE,
      retained_public_measurement_values = TRUE,
      privacy_filtering = FALSE
    )
  )
}
metadata_append_review_source <- function(record) {
  record <- validate_dataset_record(record)
  details <- record$source_manifest$details %||% list()
  source <- details$source %||% list()
  selection <- details$selection %||% list()
  repository <- source$repository %||% ""
  if (nzchar(repository) && !grepl("^https?://", repository)) {
    repository <- paste0("https://github.com/", repository)
  }
  citation <- source$citation_doi %||% ""
  if (nzchar(citation) && !grepl("^https?://", citation)) {
    citation <- paste0("https://doi.org/", citation)
  }
  list(
    fixture_id = details$fixture_id %||% "",
    label = record$display_name,
    dataset_record_id = record$id,
    package_dataset_ids = selection$dataset_ids %||%
      selection$dataset_id %||%
      character(),
    retained_rows = nrow(dataset_raw_data(record)),
    citation = citation,
    repository = repository,
    exact_revision = source$revision %||% "",
    license = source$license %||% "",
    package_schema_version = source$schema_version %||% ""
  )
}

metadata_explorer_append_review_record <- function(records) {
  if (!is.list(records) || is.null(names(records))) {
    abort_llw(
      "Offline append review sources must be named dataset records.",
      type = "validation"
    )
  }
  source_ids <- vapply(
    c("tum-medi-only", "iztech-light-subset"),
    metadata_fixture_dataset_record_id,
    character(1)
  )
  if (!all(source_ids %in% names(records))) {
    abort_llw(
      "The TUM and IZTECH offline fixtures are required for append review.",
      type = "validation"
    )
  }
  source_records <- records[source_ids]
  source_records <- lapply(source_records, validate_dataset_record)
  names(source_records) <- source_ids
  mappings <- list(
    new_append_source_mapping(
      dataset_id = source_ids[[1L]],
      participant_column = "participant_Id",
      datetime_column = "Datetime",
      measurement_column = "MEDI",
      measurement_target = "tum_MEDI",
      participant_prefix = "TUM"
    ),
    new_append_source_mapping(
      dataset_id = source_ids[[2L]],
      participant_column = "participant_Id",
      datetime_column = "Datetime",
      measurement_column = "MEDI",
      measurement_target = "iztech_MEDI",
      participant_prefix = "IZTECH",
      optional_columns = "LIGHT",
      optional_targets = "iztech_LIGHT"
    )
  )
  spec <- new_append_spec(
    mappings = mappings,
    display_name = "TUM and IZTECH appended metadata review",
    participant_policy = "prefix_source",
    measurement_strategy = "separate",
    time_alignment = "preserve_instant",
    output_timezone = "UTC"
  )
  preview <- preview_append_merge(source_records, spec)
  if (!isTRUE(preview$can_apply)) {
    abort_llw(
      paste(
        "The deterministic offline append review state could not be built:",
        paste(preview$errors, collapse = " ")
      ),
      type = "validation"
    )
  }
  record <- new_appended_dataset_record(preview)
  record$id <- "dataset_m7appendedreview"
  review_contract <- metadata_append_review_contract(
    lapply(source_records, metadata_append_review_source)
  )

  source_bundle <- record$metadata_bundle
  overlay_tables <- metadata_empty_overlay_tables()
  overlay_tables$datasets <- data.frame(
    dataset_id = record$id,
    title = review_contract$overlay$value,
    stringsAsFactors = FALSE
  )
  record$metadata_bundle <- new_metadata_bundle(
    source_bundle$sources,
    overlay = new_metadata_overlay(
      source_bundle$source_fingerprint,
      tables = overlay_tables,
      revision = review_contract$overlay$revision
    )
  )

  manifest <- record$source_manifest
  details <- manifest$details
  details$derived_fixture <- TRUE
  details$fixture_id <- review_contract$review_state_id
  details$fixture_contract <- paste(
    "Separate mappings: TUM MEDI to tum_MEDI; IZTECH MEDI to",
    "iztech_MEDI; IZTECH LIGHT to iztech_LIGHT"
  )
  details$review_state <- review_contract
  details$disclaimer <- paste(
    "This review state is constructed offline from two bounded public-data",
    "fixtures. It is not either complete published dataset and is not fresh",
    "or live measurement evidence. Exact retained source values remain",
    "separated by lineage and namespace."
  )
  source_times <- vapply(
    source_records,
    function(source_record) {
      as.numeric(source_record$source_manifest$imported_at)
    },
    numeric(1)
  )
  record$source_manifest <- new_source_manifest(
    source_type = manifest$source_type,
    original_filenames = unname(manifest$original_filenames),
    hashes = unname(manifest$hashes),
    import_arguments = manifest$import_arguments,
    source_timezone = manifest$source_timezone,
    imported_at = as.POSIXct(
      max(source_times),
      origin = "1970-01-01",
      tz = "UTC"
    ),
    details = details
  )
  record$factual_metadata$derived_fixture <- review_contract$label
  record$factual_metadata$review_state_id <- review_contract$review_state_id
  record$provenance$offline_review_state <- review_contract
  validate_dataset_record(record)
}

metadata_record_identity_ui <- function(record) {
  if (is.null(record)) return(NULL)
  if (!is.list(record)) {
    abort_llw("Selected dataset context is invalid.", type = "validation")
  }
  details <- record$source_manifest$details %||% list()
  source <- details$source %||% list()
  selection <- details$selection %||% list()
  bundle <- metadata_bundle_for_record(record)
  first_source <- bundle$sources[[1L]]
  provenance <- first_source$provenance %||% list()
  review_state <- details$review_state %||% NULL
  repository <- source$repository %||% provenance$repository %||% NULL
  revision <- source$revision %||%
    source$resolved_revision %||%
    provenance$resolved_revision %||%
    NULL
  contract <- details$fixture_contract %||%
    metadata_fixture_contract_label(selection)
  derived <- isTRUE(details$derived_fixture)

  facts <- list(
    tags$div(
      tags$dt("Record source"),
      tags$dd(record$source_manifest$source_type)
    )
  )
  if (!is.null(contract)) {
    facts <- c(
      facts,
      list(tags$div(tags$dt("Variable contract"), tags$dd(contract)))
    )
  }
  if (is.list(review_state)) {
    source_items <- lapply(review_state$sources, function(review_source) {
      tags$li(
        tags$strong(review_source$label),
        tags$div(
          "Namespace ",
          tags$code(review_source$dataset_record_id)
        ),
        tags$div(
          "Public package ",
          tags$code(sub(
            "^https://github.com/",
            "",
            review_source$repository
          ))
        ),
        tags$div(
          "Exact revision ",
          tags$code(review_source$exact_revision)
        ),
        tags$div(
          "License ",
          tags$code(review_source$license),
          "; package datasets ",
          tags$code(paste(
            review_source$package_dataset_ids,
            collapse = ", "
          ))
        )
      )
    })
    facts <- c(
      facts,
      list(tags$div(
        tags$dt("Source metadata namespaces"),
        tags$dd(tags$ul(
          class = "llw-metadata-source-list",
          source_items
        ))
      )),
      list(tags$div(
        tags$dt("Row lineage"),
        tags$dd(tags$code(review_state$lineage_column))
      )),
      list(tags$div(
        tags$dt("Conflict handling"),
        tags$dd(review_state$namespace_policy)
      )),
      list(tags$div(
        tags$dt("Review overlay"),
        tags$dd(paste(
          "Revision",
          review_state$overlay$revision,
          "is bound to the combined source fingerprint and remains separate."
        ))
      ))
    )
  }
  if (is.null(review_state) && !is.null(repository)) {
    facts <- c(
      facts,
      list(tags$div(
        tags$dt("Public source package"),
        tags$dd(tags$code(repository))
      ))
    )
  }
  if (is.null(review_state) && !is.null(revision)) {
    facts <- c(
      facts,
      list(tags$div(
        tags$dt("Exact source revision"),
        tags$dd(tags$code(revision))
      ))
    )
  }
  dataset_ids <- metadata_nonempty_text(
    selection$dataset_ids %||% selection$dataset_id
  )
  if (is.null(review_state) && length(dataset_ids) > 0L) {
    facts <- c(
      facts,
      list(tags$div(
        tags$dt("Selected source datasets"),
        tags$dd(tags$code(paste(dataset_ids, collapse = ", ")))
      ))
    )
  }
  file_groups <- metadata_nonempty_text(selection$file_group_ids)
  if (length(file_groups) > 0L) {
    facts <- c(
      facts,
      list(tags$div(
        tags$dt("Selected file group"),
        tags$dd(tags$code(paste(file_groups, collapse = ", ")))
      ))
    )
  }

  card(
    class = "llw-metadata-record-card",
    card_header("Selected dataset context"),
    tags$div(
      class = "llw-metadata-record-heading",
      tags$h3(record$display_name),
      if (derived) {
        tags$span(
          class = "llw-metadata-provenance llw-metadata-provenance--fixture",
          if (is.list(review_state)) {
            "Derived offline append"
          } else {
            "Derived offline fixture"
          }
        )
      }
    ),
    tags$dl(class = "llw-metadata-detail-list", facts),
    if (derived) {
      tags$p(
        class = "llw-secondary small mb-0",
        details$disclaimer %||%
          paste(
            "This bounded fixture is not the complete published dataset and",
            "is not fresh or live measurement evidence. It is loaded offline."
          )
      )
    }
  )
}

metadata_result_summary_message <- function(bundle, result) {
  if (nrow(bundle$index$entries) == 0L) {
    return(paste(
      "This dataset contains no retained optional metadata.",
      "It remains fully usable with raw variable names and the fallback",
      "`unit not specified`."
    ))
  }
  shown <- nrow(result$rows)
  if (result$total == 0L) {
    return(paste(
      "No retained metadata match the current search and filters.",
      "Clear or broaden them to recover the full context."
    ))
  }
  if (isTRUE(result$limited)) {
    return(paste(
      format(result$total, big.mark = ","),
      "matching fields.",
      format(shown, big.mark = ","),
      "are listed to keep this view responsive. Narrow the search to find",
      "a specific field or value."
    ))
  }
  paste(format(result$total, big.mark = ","), "matching fields.")
}

metadata_source_overlay_ui <- function(
  bundle,
  editable = FALSE,
  overlay_actions = NULL
) {
  source_count <- length(bundle$sources)
  overlay_rows <- sum(vapply(
    bundle$overlay$tables,
    nrow,
    integer(1)
  ))
  layout_columns(
    col_widths = c(6, 6),
    fill = FALSE,
    card(
      class = "llw-metadata-layer-card llw-metadata-layer-card--source",
      card_header("Source metadata"),
      tags$div(
        class = "llw-metadata-layer-heading",
        tags$span(
          class = "llw-metadata-provenance llw-metadata-provenance--source",
          "Immutable source"
        ),
        tags$strong(paste(source_count, "source namespace(s)"))
      ),
      tags$p(
        class = "llw-secondary mb-0",
        paste(
          "Retained exactly by namespace. Appended sources are never",
          "flattened or silently reconciled."
        )
      )
    ),
    card(
      class = "llw-metadata-layer-card llw-metadata-layer-card--overlay",
      card_header("LightLogWeb overlay"),
      tags$div(
        class = "llw-metadata-layer-heading",
        tags$span(
          class = "llw-metadata-provenance llw-metadata-provenance--overlay",
          paste("Overlay revision", bundle$overlay$revision)
        ),
        tags$strong(paste(overlay_rows, "overlay value(s)"))
      ),
      tags$p(
        class = "llw-secondary mb-0",
        if (editable)
          "The versioned overlay is separate from the source. Edit selected field updates a supported selection. Add missing metadata offers approved fields without a selectable source field, when imported identities are available. Source facts stay unchanged." else
          paste(
            "The versioned overlay is separate from the source.",
            "Editing is unavailable in this read-only view."
          )
      ),
      overlay_actions
    )
  )
}

metadata_filter_selection <- function(value, choices) {
  if (
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      value %in% unname(choices)
  )
    value else unname(choices[[1L]])
}

metadata_filter_choices <- function(
  bundle,
  section_index = NULL,
  namespace_id = "all",
  layer = "all"
) {
  source_choices <- c("All source datasets" = "all")
  layer_choices <- c("All metadata" = "all")
  section_choices <- c("All sections" = "all")
  sort_choices <- c(
    "Metadata location" = "path",
    "Field name" = "field",
    "Value" = "value"
  )
  if (!is.null(bundle)) {
    if (length(bundle$sources) > 1L) {
      source_choices <- metadata_source_choices(bundle)
      sort_choices <- c(sort_choices, "Source dataset" = "source")
    }
    layer_choices <- metadata_layer_choices(bundle)
    if (is.null(section_index)) {
      section_index <- metadata_section_index(bundle$index)
    }
    section_choices <- metadata_section_choices(
      section_index,
      namespace_id = metadata_filter_selection(namespace_id, source_choices),
      layer = metadata_filter_selection(layer, layer_choices)
    )
  }
  list(
    namespace = source_choices,
    layer = layer_choices,
    resource = section_choices,
    sort_by = sort_choices
  )
}

metadata_filter_visibility <- function(choices) {
  list(
    namespace = length(choices$namespace) > 1L,
    layer = length(choices$layer) > 1L,
    resource = metadata_section_filter_visible(choices$resource)
  )
}

metadata_filter_inventory <- function(section_index) {
  if (is.null(section_index)) return(NULL)
  unique(section_index[,
    c("namespace_id", "layer", "section_id", "section_label"),
    drop = FALSE
  ])
}

metadata_section_request_valid <- function(request) {
  whole_number <- function(value) {
    is.numeric(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      is.finite(value) &&
      value >= 0 &&
      value <= 2^53 - 1 &&
      value == floor(value)
  }
  scalar_text <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nchar(value, type = "bytes") <= 4096L
  }
  is.list(request) &&
    whole_number(request$epoch) &&
    whole_number(request$generation) &&
    request$generation > 0 &&
    all(vapply(
      request[c("namespace", "layer", "resource")],
      scalar_text,
      TRUE
    )) &&
    all(c("namespace", "layer", "resource") %in% names(request))
}

metadata_section_reply <- function(request, bundle, section_index, epoch) {
  if (!metadata_section_request_valid(request) || request$epoch != epoch) {
    return(NULL)
  }
  choices <- metadata_filter_choices(
    bundle,
    section_index,
    request$namespace,
    request$layer
  )
  if (
    !request$namespace %in% unname(choices$namespace) ||
      !request$layer %in% unname(choices$layer)
  )
    return(NULL)
  visible <- metadata_section_filter_visible(choices$resource)
  list(
    kind = "choices",
    epoch = epoch,
    generation = request$generation,
    namespace = request$namespace,
    layer = request$layer,
    choices = lapply(seq_along(choices$resource), function(i) {
      list(
        value = unname(choices$resource[[i]]),
        label = names(choices$resource)[[i]]
      )
    }),
    selected = if (visible) {
      metadata_filter_selection(request$resource, choices$resource)
    } else "all",
    visible = visible
  )
}

metadata_filter_grid_ui <- function(ns) {
  if (!is.function(ns)) {
    abort_llw("`ns` must be a namespace function.", type = "validation")
  }
  # Keep native inputs mounted. Recreating this row in response to its own
  # selections can turn delayed input-binding echoes into a feedback loop.
  choices <- metadata_filter_choices(NULL)
  optional <- function(id, label) {
    conditionalPanel(
      paste0("output.filter_visibility && output.filter_visibility.", id),
      ns = ns,
      selectInput(
        ns(id),
        label,
        choices = choices[[id]],
        selected = "all",
        selectize = FALSE,
        width = "100%"
      )
    )
  }
  tags$div(
    id = ns("filter_grid"),
    class = "llw-metadata-filter-grid",
    `data-section-request` = ns("section_request"),
    `data-section-input` = ns("resource"),
    `data-section-source` = ns("namespace"),
    `data-section-layer` = ns("layer"),
    optional("namespace", "Source dataset"),
    optional("layer", "Metadata layer"),
    tags$div(
      class = "llw-metadata-section-slot",
      tagAppendAttributes(
        selectInput(
          ns("resource"),
          "Section",
          choices = choices$resource,
          selected = "all",
          selectize = FALSE,
          width = "100%"
        ),
        .cssSelector = "select",
        disabled = "disabled",
        `aria-busy` = "true",
        `aria-describedby` = ns("section_status")
      ),
      tags$div(
        class = "llw-metadata-section-feedback",
        tags$span(
          id = ns("section_status"),
          class = "llw-metadata-section-status",
          role = "status",
          `aria-live` = "polite",
          "Updating sections..."
        ),
        tags$button(
          type = "button",
          class = "btn btn-link llw-metadata-section-retry",
          "Retry sections"
        )
      )
    ),
    selectInput(
      ns("sort_by"),
      "Sort results by",
      choices = choices$sort_by,
      selected = "path",
      selectize = FALSE,
      width = "100%"
    )
  )
}

metadata_explorer_ui <- function(id, field_actions = NULL) {
  ns <- NS(id)
  tags$section(
    metadata_navigation_dependency(),
    class = "llw-metadata-explorer",
    `aria-labelledby` = ns("heading"),
    card(
      class = "llw-metadata-search-card",
      card_header("Find metadata"),
      layout_columns(
        col_widths = c(8, 4),
        fill = FALSE,
        textInput(
          ns("query"),
          "Search field names and values",
          placeholder = "For example: melanopic, timezone, calibration",
          width = "100%"
        ),
        tags$div(
          class = "llw-metadata-clear-wrap",
          actionButton(
            ns("clear_query"),
            "Clear search",
            icon = icon("circle-xmark"),
            class = "btn-outline-secondary",
            width = "100%"
          )
        )
      ),
      metadata_filter_grid_ui(ns),
      checkboxInput(
        ns("descending"),
        "Reverse sort order",
        value = FALSE
      ),
      tags$div(
        class = "llw-metadata-search-results",
        uiOutput(ns("result_summary")),
        uiOutput(ns("result_list"))
      )
    ),
    # Authoring controls stay mounted for focus return and remain available
    # when the selected dataset has no retained metadata fields yet.
    if (is.null(field_actions)) uiOutput(ns("hierarchy_shell")) else
      metadata_hierarchy_shell_ui(ns, field_actions),
    uiOutput(ns("entry_detail")),
    tags$div(
      class = "llw-metadata-explorer__heading",
      tags$h2(id = ns("heading"), "Dataset and metadata context"),
      tags$p(
        class = "llw-secondary",
        "Retained source facts and the LightLogWeb overlay for the selected dataset."
      )
    ),
    uiOutput(ns("dataset_state")),
    uiOutput(ns("dataset_identity")),
    uiOutput(ns("layer_summary")),
    uiOutput(ns("focus_status"))
  )
}

metadata_explorer_server <- function(
  id,
  dataset,
  focus_request = NULL,
  editable = FALSE,
  presentation = NULL,
  overlay_actions = NULL
) {
  if (!shiny::is.reactive(dataset)) {
    abort_llw("`dataset` must be a reactive input.", type = "validation")
  }
  if (is.null(focus_request)) focus_request <- reactive(NULL)
  if (!shiny::is.reactive(focus_request)) {
    abort_llw("`focus_request` must be a reactive input.", type = "validation")
  }
  if (is.null(presentation)) presentation <- reactive(NULL)
  if (!shiny::is.reactive(presentation)) {
    abort_llw("`presentation` must be a reactive input.", type = "validation")
  }

  moduleServer(id, function(input, output, session) {
    record <- reactive({
      dataset()
    })

    bundle <- reactive({
      value <- record()
      if (is.null(value)) return(NULL)
      metadata_bundle_for_record(value)
    })

    index <- reactive({
      value <- bundle()
      if (is.null(value)) return(NULL)
      value$index
    })

    section_index <- reactive({
      value <- index()
      if (is.null(value)) return(NULL)
      metadata_section_index(value)
    })

    filter_inventory <- reactive(metadata_filter_inventory(section_index()))

    filter_choices <- reactive({
      metadata_filter_choices(
        bundle(),
        filter_inventory(),
        namespace_id = input$namespace,
        layer = input$layer
      )
    })

    selected_namespace <- reactive({
      metadata_filter_selection(input$namespace, filter_choices()$namespace)
    })

    selected_layer <- reactive({
      metadata_filter_selection(input$layer, filter_choices()$layer)
    })

    section_choices <- reactive(filter_choices()$resource)

    selected_section <- reactive({
      choices <- section_choices()
      if (!metadata_section_filter_visible(choices)) return("all")
      metadata_filter_selection(input$resource, choices)
    })

    output$filter_visibility <- reactive({
      metadata_filter_visibility(filter_choices())
    })
    outputOptions(output, "filter_visibility", suspendWhenHidden = FALSE)

    previous_filter_choices <- list()
    previous_filter_identity <- NULL
    previous_section_revision <- NULL
    section_epoch <- reactiveVal(0)
    latest_section_generation <- 0
    refresh_section <- function(reset = FALSE, selected = NULL) {
      epoch <- isolate(section_epoch()) + 1
      section_epoch(epoch)
      latest_section_generation <<- 0
      session$sendCustomMessage(
        "llw-metadata-section",
        list(
          kind = "context",
          target = session$ns("filter_grid"),
          epoch = epoch,
          reset = reset,
          selected = selected
        )
      )
    }
    observe({
      choices <- filter_choices()
      identity <- list(
        id = record()$id,
        source = bundle()$source_fingerprint
      )
      reset <- !identical(identity, previous_filter_identity)
      previous_filter_identity <<- identity
      for (id in setdiff(names(choices), "resource")) {
        changed <- !identical(choices[[id]], previous_filter_choices[[id]])
        reset_selection <- reset && !identical(id, "sort_by")
        if (!changed && !reset_selection) next
        selected <- if (reset_selection) "all" else
          metadata_filter_selection(isolate(input[[id]]), choices[[id]])
        updateSelectInput(
          session,
          id,
          choices = if (changed) choices[[id]] else NULL,
          selected = selected
        )
        previous_filter_choices[[id]] <<- choices[[id]]
      }
      revision <- bundle()$overlay$revision
      if (reset || !identical(revision, previous_section_revision)) {
        previous_section_revision <<- revision
        refresh_section(reset = reset)
      }
    })

    observeEvent(
      input$section_request,
      {
        request <- input$section_request
        if (!metadata_section_request_valid(request)) return()
        epoch <- section_epoch()
        if (request$epoch != epoch) {
          session$sendCustomMessage(
            "llw-metadata-section",
            list(
              kind = "context",
              target = session$ns("filter_grid"),
              epoch = epoch,
              reset = FALSE
            )
          )
          return()
        }
        if (request$generation <= latest_section_generation) return()
        latest_section_generation <<- request$generation
        reply <- metadata_section_reply(
          request,
          bundle(),
          filter_inventory(),
          epoch
        )
        if (is.null(reply)) {
          reply <- list(
            kind = "error",
            epoch = epoch,
            generation = request$generation
          )
        }
        reply$target <- session$ns("filter_grid")
        session$sendCustomMessage("llw-metadata-section", reply)
      },
      ignoreInit = FALSE
    )

    observeEvent(
      input$clear_query,
      {
        updateTextInput(session, "query", value = "")
      },
      ignoreInit = TRUE
    )

    previous_record_source <- NULL
    previous_overlay_revision <- NULL
    observeEvent(
      record(),
      {
        current <- record()
        identity <- list(
          id = current$id,
          source = current$metadata_bundle$source_fingerprint
        )
        overlay_revision <- current$metadata_bundle$overlay$revision
        overlay_change <- identical(identity, previous_record_source) &&
          !identical(overlay_revision, previous_overlay_revision)
        previous_record_source <<- identity
        previous_overlay_revision <<- overlay_revision
        if (overlay_change) {
          requested <- requested_group_id()
          if (!is.null(requested) && !requested %in% index()$groups$group_id) {
            # Keep valid exploration state, but never retain a successful
            # crosslink or replay its pin after that exact field is removed.
            requested_group_id(NULL)
            requested_group_synced(FALSE)
            if (identical(external_focus_status()$state, "resolved")) {
              external_focus_status(NULL)
            }
          }
          return(invisible(NULL))
        }
        updateTextInput(session, "query", value = "")
        record_selection_revision(record_selection_revision() + 1L)
        group_input_revision(-1L)
        active_group_id(NULL)
        focused_node_id(NULL)
        requested_group_id(NULL)
        requested_group_synced(FALSE)
        external_focus_status(NULL)
      },
      ignoreInit = FALSE
    )

    query_result <- reactive({
      value <- index()
      if (is.null(value)) {
        return(structure(
          list(rows = data.frame(), total = 0L, limited = FALSE),
          class = c("llw_metadata_group_query", "list")
        ))
      }
      metadata_index_query_groups_by_section(
        value,
        section_index = section_index(),
        query = input$query %||% "",
        namespace_id = selected_namespace(),
        section_id = selected_section(),
        layer = selected_layer(),
        sort_by = metadata_filter_selection(
          input$sort_by,
          filter_choices()$sort_by
        ),
        descending = isTRUE(input$descending),
        limit = 40L
      )
    })

    output$dataset_state <- renderUI({
      value <- bundle()
      if (is.null(value)) {
        return(llw_status_callout(
          "idle",
          paste(
            "Select a dataset in the library, or import one first.",
            "Metadata-free datasets remain available in every other view."
          ),
          heading = "No dataset selected"
        ))
      }
      metadata_state_callout(value)
    })

    output$dataset_identity <- renderUI({
      metadata_record_identity_ui(record())
    })

    output$layer_summary <- renderUI({
      value <- bundle()
      if (is.null(value)) return(NULL)
      metadata_source_overlay_ui(
        value,
        editable = editable,
        overlay_actions = overlay_actions
      )
    })

    active_group_id <- reactiveVal(NULL)
    focused_node_id <- reactiveVal(NULL)
    requested_group_id <- reactiveVal(NULL)
    requested_group_synced <- reactiveVal(FALSE)
    external_focus_status <- reactiveVal(NULL)
    record_selection_revision <- reactiveVal(0L)
    group_input_revision <- reactiveVal(-1L)

    scroll_to_focus <- function() {
      session$onFlushed(
        function() {
          group_id <- isolate(active_group_id())
          if (is.null(group_id)) return(invisible(NULL))
          session$sendCustomMessage(
            "llw-metadata-focus",
            list(
              target = session$ns("hierarchy_focus"),
              group_id = group_id
            )
          )
        },
        once = TRUE
      )
    }

    output$result_summary <- renderUI({
      value <- bundle()
      if (is.null(value)) return(NULL)
      result <- query_result()
      tags$div(
        class = "llw-metadata-result-summary",
        role = "status",
        `aria-live` = "polite",
        `aria-atomic` = "true",
        metadata_result_summary_message(value, result)
      )
    })

    output$result_list <- renderUI({
      value <- bundle()
      if (is.null(value)) return(NULL)
      result <- query_result()
      requested <- isolate(requested_group_id())
      input_is_current <- identical(
        group_input_revision(),
        record_selection_revision()
      )
      current <- if (
        !is.null(requested) && requested %in% result$rows$group_id
      ) {
        requested
      } else if (input_is_current) {
        isolate(input$group_id %||% "")
      } else {
        ""
      }
      metadata_result_list_ui(
        metadata_present_groups(result$rows, presentation()),
        input_id = session$ns("group_id"),
        selected = current,
        show_source = length(value$sources) > 1L
      )
    })

    output$hierarchy_shell <- renderUI({
      value <- bundle()
      if (
        is.null(value) ||
          !inherits(value$index, "llw_metadata_index") ||
          nrow(value$index$groups) == 0L
      ) {
        return(NULL)
      }
      metadata_hierarchy_shell_ui(session$ns)
    })

    search_group_id <- reactive({
      result <- query_result()
      if (nrow(result$rows) == 0L) return(NULL)
      if (!identical(group_input_revision(), record_selection_revision())) {
        return(NULL)
      }
      group_id <- input$group_id %||% ""
      if (!nzchar(group_id) || !group_id %in% result$rows$group_id) NULL else
        group_id
    })

    selected_group <- reactive({
      value <- index()
      group_id <- active_group_id()
      if (
        is.null(value) ||
          is.null(group_id) ||
          length(group_id) != 1L ||
          is.na(group_id) ||
          !nzchar(group_id)
      ) {
        return(NULL)
      }
      metadata_index_group(value, group_id)
    })

    selected_neighborhood <- reactive({
      value <- index()
      group <- selected_group()
      if (is.null(value) || is.null(group)) return(NULL)
      metadata_index_group_neighborhood(value, group)
    })

    hierarchy_roots <- reactive({
      value <- index()
      group <- selected_group()
      if (is.null(value) || is.null(group)) return(NULL)
      metadata_hierarchy_logical_roots(value, group)
    })

    hierarchy_context <- reactive({
      value <- index()
      group <- selected_group()
      neighborhood <- selected_neighborhood()
      if (is.null(value) || is.null(group) || is.null(neighborhood)) {
        return(NULL)
      }
      node_id <- focused_node_id()
      if (identical(node_id, "metadata_areas")) return(NULL)
      if (
        is.null(node_id) ||
          length(node_id) != 1L ||
          is.na(node_id) ||
          !nzchar(node_id)
      ) {
        node_id <- neighborhood$representative_node_id
      }
      node <- metadata_index_node(value, node_id)
      row <- group$group
      if (
        is.null(node) ||
          !identical(node$namespace_id[[1L]], row$namespace_id[[1L]]) ||
          !identical(node$layer[[1L]], row$layer[[1L]])
      ) {
        node_id <- neighborhood$representative_node_id
      }
      metadata_hierarchy_node_context(value, node_id)
    })

    hierarchy_neighborhood <- reactive({
      metadata_hierarchy_context_neighborhood(hierarchy_context())
    })

    hierarchy_parent_id <- reactive({
      metadata_hierarchy_parent_id(hierarchy_context(), hierarchy_roots())
    })

    observe({
      updateActionButton(
        session,
        "hierarchy_up",
        disabled = is.null(hierarchy_parent_id())
      )
    })

    hierarchy_navigation_nodes <- reactive({
      if (identical(focused_node_id(), "metadata_areas")) {
        hierarchy_roots()
      } else {
        hierarchy_context()$children
      }
    })

    activate_group <- function(group_id, reset_focus = TRUE) {
      value <- index()
      if (
        is.null(value) ||
          is.null(group_id) ||
          length(group_id) != 1L ||
          is.na(group_id) ||
          !nzchar(group_id)
      ) {
        active_group_id(NULL)
        if (isTRUE(reset_focus)) focused_node_id(NULL)
        return(invisible(FALSE))
      }
      group <- metadata_index_group(value, group_id)
      if (is.null(group)) return(invisible(FALSE))
      previous_group_id <- isolate(active_group_id())
      active_group_id(group_id)
      if (
        isTRUE(reset_focus) &&
          (!identical(previous_group_id, group_id) ||
            identical(isolate(focused_node_id()), "metadata_areas"))
      ) {
        neighborhood <- metadata_index_group_neighborhood(value, group)
        focused_node_id(neighborhood$representative_node_id)
      }
      invisible(TRUE)
    }

    set_hierarchy_focus <- function(node_id) {
      value <- index()
      group <- selected_group()
      if (
        is.null(value) ||
          is.null(group) ||
          is.null(node_id) ||
          length(node_id) != 1L ||
          is.na(node_id) ||
          !nzchar(node_id)
      ) {
        return(invisible(FALSE))
      }
      node <- metadata_index_node(value, node_id)
      row <- group$group
      if (
        is.null(node) ||
          !identical(node$namespace_id[[1L]], row$namespace_id[[1L]]) ||
          !identical(node$layer[[1L]], row$layer[[1L]])
      ) {
        return(invisible(FALSE))
      }
      focused_node_id(node_id)
      invisible(TRUE)
    }

    previous_search_selection <- NULL
    observeEvent(
      search_group_id(),
      {
        selection_key <- list(
          record_selection_revision(),
          input$group_id,
          input$query,
          selected_namespace(),
          selected_layer(),
          selected_section()
        )
        if (
          identical(selection_key, previous_search_selection) &&
            isTRUE(isolate(active_group_id()) %in% index()$groups$group_id)
        ) {
          return(invisible(NULL))
        }
        previous_search_selection <<- selection_key
        requested <- requested_group_id()
        if (
          !is.null(requested) &&
            requested %in% index()$groups$group_id
        ) {
          activate_group(requested, reset_focus = FALSE)
        } else {
          activate_group(search_group_id(), reset_focus = TRUE)
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )

    observeEvent(
      input$group_id,
      {
        selected <- input$group_id %||% ""
        group_input_revision(record_selection_revision())
        requested <- isolate(requested_group_id())
        if (!is.null(requested)) {
          if (identical(selected, requested)) {
            requested_group_synced(TRUE)
          } else if (
            nzchar(selected) &&
              isTRUE(isolate(requested_group_synced()))
          ) {
            requested_group_id(NULL)
            requested_group_synced(FALSE)
            external_focus_status(NULL)
          }
        }
        if (selected %in% query_result()$rows$group_id) scroll_to_focus()
      },
      ignoreInit = TRUE
    )

    observeEvent(
      focus_request(),
      {
        request <- tryCatch(
          validate_metadata_focus_request(focus_request()),
          error = identity
        )
        value <- record()
        if (inherits(request, "error") || is.null(value)) {
          external_focus_status(list(
            state = "unavailable",
            label = "Requested metadata context",
            message = "The requested metadata context is not available."
          ))
          return()
        }
        if (!identical(request$dataset_id, value$id)) {
          external_focus_status(list(
            state = "unavailable",
            label = request$label,
            message = paste(
              "The request belongs to another dataset. The current metadata",
              "selection remains unchanged."
            )
          ))
          return()
        }
        resolution <- metadata_resolve_focus_request(bundle(), request)
        if (!identical(resolution$state, "resolved")) {
          external_focus_status(list(
            state = resolution$state,
            label = request$label,
            message = if (identical(resolution$state, "ambiguous")) {
              paste(
                "More than one exact retained location matched this request.",
                "Nothing was selected automatically."
              )
            } else {
              paste(
                "No exact retained location matched this request. The",
                "dataset remains fully available with raw names and values."
              )
            }
          ))
          return()
        }
        row <- bundle()$index$groups[
          bundle()$index$groups$group_id == resolution$group_id,
          ,
          drop = FALSE
        ]
        requested_group_id(resolution$group_id)
        requested_group_synced(identical(
          isolate(input$group_id %||% ""),
          resolution$group_id
        ))
        activate_group(resolution$group_id, reset_focus = TRUE)
        if (!is.null(resolution$node_id)) {
          set_hierarchy_focus(resolution$node_id)
        }
        updateTextInput(
          session,
          "query",
          value = request$record_identity %||% request$field %||% ""
        )
        updateSelectInput(
          session,
          "namespace",
          selected = metadata_filter_selection(
            row$namespace_id[[1L]],
            filter_choices()$namespace
          )
        )
        updateSelectInput(
          session,
          "layer",
          selected = metadata_filter_selection(
            row$layer[[1L]],
            filter_choices()$layer
          )
        )
        refresh_section(selected = metadata_group_section_id(row))
        session$onFlushed(
          function() {
            updateRadioButtons(
              session,
              "group_id",
              selected = resolution$group_id
            )
          },
          once = TRUE
        )
        external_focus_status(list(
          state = "resolved",
          label = request$label,
          message = paste(
            "The exact retained field is selected above. Use the path and",
            "visible explorer to navigate its containing source metadata."
          )
        ))
        scroll_to_focus()
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    output$focus_status <- renderUI({
      status <- external_focus_status()
      if (is.null(status)) return(NULL)
      llw_status_callout(
        if (identical(status$state, "resolved")) "complete" else "warning",
        status$message,
        heading = status$label,
        compact = TRUE,
        live = TRUE
      )
    })

    observeEvent(
      input$neighborhood_group_id,
      {
        neighborhood <- hierarchy_neighborhood()
        selected <- input$neighborhood_group_id %||% ""
        if (
          !is.null(neighborhood) &&
            selected %in% neighborhood$property_groups$group_id
        ) {
          activate_group(selected, reset_focus = FALSE)
        }
      },
      ignoreInit = TRUE
    )

    open_hierarchy_dialog <- function() {
      context <- hierarchy_context()
      if (is.null(context)) return(invisible(NULL))
      showModal(modalDialog(
        title = "Full metadata path",
        metadata_hierarchy_context_ui(
          context,
          hierarchy_roots(),
          input_ids = list(
            path = session$ns("hierarchy_dialog_path_node_id")
          ),
          selected_group_id = active_group_id()
        ),
        footer = modalButton("Close"),
        easyClose = TRUE,
        fade = FALSE,
        size = "l"
      ))
    }
    observeEvent(
      input$hierarchy_full_path,
      {
        open_hierarchy_dialog()
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$hierarchy_up,
      {
        count <- input$hierarchy_up
        if (
          !is.numeric(count) ||
            length(count) != 1L ||
            is.na(count) ||
            !is.finite(count) ||
            count <= 0
        ) {
          return()
        }
        parent_id <- hierarchy_parent_id()
        if (is.null(parent_id)) return()
        if (identical(parent_id, "metadata_areas")) {
          focused_node_id("metadata_areas")
          scroll_to_focus()
        } else if (isTRUE(set_hierarchy_focus(parent_id))) {
          scroll_to_focus()
        }
      },
      ignoreInit = TRUE
    )

    # One bounded observer tracks only the currently visible, stable node IDs.
    # Rebaseline on a scope change so a stale/remounted button cannot open a
    # different branch or replay an earlier activation.
    navigation_scope <- NULL
    navigation_counts <- numeric()
    observe({
      nodes <- hierarchy_navigation_nodes()
      ids <- metadata_hierarchy_action_id(nodes$node_id %||% character())
      counts <- stats::setNames(
        vapply(
          ids,
          function(id) {
            value <- input[[id]]
            if (
              is.numeric(value) &&
                length(value) == 1L &&
                !is.na(value) &&
                is.finite(value) &&
                value >= 0
            ) {
              as.numeric(value)
            } else 0
          },
          numeric(1)
        ),
        ids
      )
      scope <- list(record_selection_revision(), focused_node_id(), ids)
      if (!identical(navigation_scope, scope)) {
        navigation_scope <<- scope
        navigation_counts <<- counts
        return()
      }
      activated <- which(counts > navigation_counts)
      navigation_counts <<- counts
      if (length(activated) != 1L) return()
      selected <- nodes$node_id[[activated]]
      if (isTRUE(set_hierarchy_focus(selected))) scroll_to_focus()
    })

    observeEvent(
      input$hierarchy_path_node_id,
      {
        path <- metadata_hierarchy_full_path(
          hierarchy_context(),
          hierarchy_roots()
        )
        selected <- input$hierarchy_path_node_id %||% ""
        if (selected %in% path$node_id) {
          set_hierarchy_focus(selected)
          scroll_to_focus()
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$hierarchy_dialog_path_node_id,
      {
        path <- metadata_hierarchy_full_path(
          hierarchy_context(),
          hierarchy_roots()
        )
        selected <- input$hierarchy_dialog_path_node_id %||% ""
        if (selected %in% path$node_id) {
          set_hierarchy_focus(selected)
          removeModal()
          scroll_to_focus()
        }
      },
      ignoreInit = TRUE
    )

    selected_entry <- reactive({
      value <- selected_group()
      if (is.null(value) || nrow(value$entries) == 0L) return(NULL)
      value$entries[1L, , drop = FALSE]
    })

    output$hierarchy_summary <- renderUI({
      metadata_hierarchy_summary_ui(
        selected_group(),
        hierarchy_context()
      )
    })

    output$hierarchy_content <- renderUI({
      value <- index()
      if (is.null(value) || nrow(value$groups) == 0L) {
        return(tags$p(
          class = "llw-secondary mb-0",
          "No retained metadata fields to explore yet."
        ))
      }
      metadata_hierarchy_panel_ui(
        selected_group(),
        hierarchy_context(),
        hierarchy_roots(),
        input_ids = list(
          path = session$ns("hierarchy_path_node_id"),
          full_path = session$ns("hierarchy_full_path")
        ),
        show_areas = identical(focused_node_id(), "metadata_areas")
      )
    })

    output$item_count <- renderUI({
      metadata_item_count_ui(hierarchy_neighborhood())
    })

    # Keep the list mounted when only its selected field changes. Native radio
    # updates preserve the browser's list scroll and keyboard focus.
    output$item_properties <- renderUI({
      group <- selected_group()
      if (is.null(group)) return(NULL)
      neighborhood <- hierarchy_neighborhood()
      if (!is.null(neighborhood))
        neighborhood$property_groups <- metadata_present_groups(
          neighborhood$property_groups,
          presentation()
        )
      metadata_item_properties_ui(
        neighborhood,
        session$ns("neighborhood_group_id"),
        group$group$group_id[[1L]],
        children = hierarchy_navigation_nodes(),
        child_input_id = function(node_id) {
          session$ns(metadata_hierarchy_action_id(node_id))
        },
        show_areas = identical(focused_node_id(), "metadata_areas")
      )
    }) |>
      shiny::bindEvent(
        index(),
        focused_node_id(),
        presentation(),
        ignoreNULL = FALSE
      )

    observe({
      selected <- active_group_id()
      groups <- hierarchy_neighborhood()$property_groups$group_id
      updateRadioButtons(
        session,
        "neighborhood_group_id",
        selected = if (!is.null(selected) && selected %in% groups) {
          selected
        } else character()
      )
    })

    output$entry_detail <- renderUI({
      value <- bundle()
      if (is.null(value)) return(NULL)
      result <- query_result()
      if (nrow(value$index$groups) == 0L) return(NULL)
      if (result$total == 0L) {
        return(llw_status_callout(
          "idle",
          paste(
            "No current result is selected. Clear or broaden the search",
            "and filters to inspect a retained field."
          ),
          heading = "No current metadata selection",
          compact = TRUE
        ))
      }
      metadata_group_detail_ui(
        selected_group(),
        value,
        show_source = length(value$sources) > 1L,
        presentation = presentation()
      )
    })

    status <- reactive({
      value <- bundle()
      result <- query_result()
      list(
        ready = !is.null(value),
        source_count = if (is.null(value)) 0L else length(value$sources),
        match_count = result$total,
        shown_count = nrow(result$rows),
        selected_group_id = selected_group()$group$group_id %||% NULL,
        selected_entry_id = selected_entry()$entry_id %||% NULL,
        focused_node_id = hierarchy_context()$node$node_id %||% NULL,
        external_focus = external_focus_status()
      )
    })

    list(
      bundle = bundle,
      query = query_result,
      selected_group = selected_group,
      selected_entry = selected_entry,
      selected_neighborhood = selected_neighborhood,
      hierarchy_roots = hierarchy_roots,
      hierarchy_context = hierarchy_context,
      hierarchy_neighborhood = hierarchy_neighborhood,
      status = status
    )
  })
}

metadata_explorer_showcase_records <- function() {
  data <- data.frame(
    Id = "P01",
    Datetime = as.POSIXct("2026-01-01 08:00:00", tz = "UTC"),
    MEDI = 125,
    stringsAsFactors = FALSE
  )
  manifest <- new_source_manifest(
    "metadata_showcase",
    source_timezone = "UTC"
  )
  complete <- new_dataset_record(
    data,
    "Complete metadata",
    manifest,
    factual_metadata = list(
      study = list(title = "Circadian field study"),
      device = list(manufacturer = "Example", model = "Logger A"),
      variables = list(
        MEDI = list(
          label = "Melanopic EDI",
          unit = "lux"
        )
      )
    ),
    analysis_settings = list(primary_variable = "MEDI"),
    id = "dataset_m7complete"
  )
  complete$metadata_bundle$sources[[1L]]$state <- "complete"
  complete$metadata_bundle$sources[[1L]]$fingerprint <- NULL
  complete$metadata_bundle$sources[[1L]] <- do.call(
    new_metadata_source_namespace,
    complete$metadata_bundle$sources[[1L]][c(
      "namespace_id",
      "label",
      "source_type",
      "records",
      "resources",
      "source_dataset_id",
      "state",
      "source_schema_version",
      "provenance"
    )]
  )
  names(complete$metadata_bundle$sources) <-
    complete$metadata_bundle$sources[[1L]]$namespace_id
  complete$metadata_bundle <- new_metadata_bundle(
    complete$metadata_bundle$sources
  )
  complete <- validate_dataset_record(complete)
  partial <- new_dataset_record(
    data,
    "Partial metadata",
    manifest,
    factual_metadata = list(
      device = list(model = "Logger B"),
      variables = list(MEDI = list(label = "Melanopic EDI"))
    ),
    analysis_settings = list(primary_variable = "MEDI"),
    id = "dataset_m7partial"
  )
  absent <- new_dataset_record(
    data,
    "Metadata absent",
    manifest,
    analysis_settings = list(primary_variable = "MEDI"),
    id = "dataset_m7absent"
  )
  records <- list(complete, partial, absent)
  stats::setNames(records, vapply(records, `[[`, character(1), "id"))
}

metadata_explorer_fixture_records <- function(
  fixture_directory,
  include_absent = TRUE
) {
  assert_scalar_string(fixture_directory, "fixture_directory")
  assert_flag(include_absent, "include_absent")
  files <- c(
    "tum-medi-only.rds",
    "iztech-light-subset.rds",
    "ucr-all-light-variables.rds"
  )
  paths <- file.path(fixture_directory, files)
  if (any(!file.exists(paths))) {
    abort_llw(
      "The three offline Milestone 7 review fixtures are incomplete.",
      type = "validation"
    )
  }
  records <- lapply(paths, function(path) {
    fixture <- readRDS(path)
    required <- c(
      "fixture_schema",
      "fixture_id",
      "label",
      "disclaimer",
      "source",
      "selection",
      "generation",
      "data",
      "metadata_snapshot"
    )
    if (
      !is.list(fixture) ||
        !all(required %in% names(fixture)) ||
        !identical(fixture$fixture_schema, "llw-glc-derived-fixture")
    ) {
      abort_llw("A Milestone 7 review fixture is invalid.", type = "validation")
    }
    snapshot <- validate_glc_source_metadata_snapshot(
      fixture$metadata_snapshot
    )
    current_bundle <- new_metadata_bundle(
      snapshot$bundle$sources,
      overlay = snapshot$bundle$overlay
    )
    new_dataset_record(
      raw_data = fixture$data,
      display_name = fixture$label,
      source_manifest = new_source_manifest(
        source_type = "derived_glc_fixture",
        original_filenames = basename(path),
        hashes = paste0(
          "sha256:",
          digest::digest(path, algo = "sha256", file = TRUE)
        ),
        source_timezone = lubridate::tz(fixture$data$Datetime),
        imported_at = as.POSIXct(
          fixture$generation$generated_at,
          format = "%Y-%m-%dT%H:%M:%SZ",
          tz = "UTC"
        ),
        details = list(
          derived_fixture = TRUE,
          fixture_id = fixture$fixture_id,
          fixture_contract = metadata_fixture_contract_label(
            fixture$selection
          ),
          disclaimer = fixture$disclaimer,
          source = fixture$source,
          selection = fixture$selection,
          generation = fixture$generation
        )
      ),
      factual_metadata = list(
        derived_fixture = fixture$label,
        source_dataset_ids = fixture$selection$dataset_ids %||%
          fixture$selection$dataset_id
      ),
      analysis_settings = list(
        primary_variable = if ("MEDI" %in% names(fixture$data)) {
          "MEDI"
        } else {
          names(fixture$data)[[1L]]
        }
      ),
      metadata_bundle = current_bundle,
      id = metadata_fixture_dataset_record_id(fixture$fixture_id)
    )
  })
  names(records) <- vapply(records, `[[`, character(1), "id")
  appended <- metadata_explorer_append_review_record(records)
  records <- c(records, list(appended))
  if (isTRUE(include_absent)) {
    absent <- metadata_explorer_showcase_records()[[3L]]
    absent$display_name <- "Metadata absent fallback"
    absent <- validate_dataset_record(absent)
    records <- c(records, list(absent))
  }
  stats::setNames(records, vapply(records, `[[`, character(1), "id"))
}

metadata_explorer_app <- function(
  records = metadata_explorer_showcase_records()
) {
  if (!is.list(records) || length(records) == 0L) {
    abort_llw(
      "The metadata showcase requires dataset records.",
      type = "validation"
    )
  }
  records <- lapply(records, validate_dataset_record)
  names(records) <- vapply(records, `[[`, character(1), "id")
  ui <- lightlogweb_page(page_fluid(
    theme = lightlogweb_theme(),
    lightlogweb_head(),
    tags$main(
      class = "llw-main-shell",
      llw_view_header(
        "Milestone 7 showcase",
        "Metadata essentials",
        paste(
          "Exercise retained metadata and the metadata-free fallback without",
          "network access or measurement-file reads."
        )
      ),
      selectInput(
        "showcase_record",
        "Dataset or fixture",
        choices = stats::setNames(
          names(records),
          vapply(records, `[[`, character(1), "display_name")
        )
      ),
      metadata_explorer_ui("metadata"),
      tags$aside(
        class = "llw-showcase-status",
        `aria-label` = "Development status",
        tags$h2("Development status"),
        verbatimTextOutput("status")
      )
    )
  ))
  server <- function(input, output, session) {
    selected <- reactive(records[[
      input$showcase_record %||% names(records)[[1L]]
    ]])
    explorer <- metadata_explorer_server("metadata", selected)
    output$status <- renderPrint(explorer$status())
  }
  shinyApp(ui, server)
}
