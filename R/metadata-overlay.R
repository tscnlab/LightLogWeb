# Overlay authoring owns only user assertions, never source records or payloads.
metadata_overlay_version <- function() "2.0.0"

metadata_overlay_fields <- function() {
  list(
    datasets = c(title = "Dataset title"),
    participants = c(label = "Participant label", sex = "Participant sex"),
    devices = c(manufacturer = "Device manufacturer", model = "Device model"),
    sites = c(
      site = "Site",
      country = "Country",
      source_timezone = "Source timezone annotation",
      latitude = "Latitude",
      longitude = "Longitude"
    ),
    variables = c(label = "Variable label", unit = "Variable unit")
  )
}

metadata_overlay_row_key <- function(table, columns) {
  if (nrow(table) == 0L) return(character())
  do.call(
    paste0,
    lapply(table[columns], function(value) {
      paste0(nchar(value, type = "bytes"), ":", value)
    })
  )
}

metadata_overlay_scope <- function(namespaces) {
  as.character(jsonlite::toJSON(unname(namespaces), auto_unbox = FALSE))
}

metadata_overlay_empty_catalog <- function() {
  dplyr::tibble(
    target_id = character(),
    source_scope = character(),
    source_label = character(),
    table = character(),
    entity_id = character(),
    dataset_id = character(),
    participant_id = character(),
    device_id = character(),
    file_group_id = character(),
    name = character(),
    field = character(),
    field_label = character(),
    cohort = character(),
    source_value = list(),
    value_type = character(),
    variable_type = character(),
    source_path = character()
  )
}

metadata_overlay_source_catalog <- function(bundle) {
  rows <- list()
  fields <- metadata_overlay_fields()
  column_value <- function(table, column, numeric = FALSE) {
    absent <- if (numeric) NA_real_ else NA_character_
    if (!column %in% names(table)) return(rep(absent, nrow(table)))
    value <- table[[column]]
    if (is.list(value)) return(rep(absent, nrow(table)))
    if (numeric) {
      if (!is.numeric(value)) return(rep(absent, nrow(table)))
      as.numeric(value)
    } else as.character(value)
  }
  add_table <- function(
    source,
    namespaces,
    path,
    table_name,
    input,
    key_map,
    value_map,
    path_resource,
    variable_type = NULL
  ) {
    if (
      !is.data.frame(input) ||
        nrow(input) == 0L ||
        !all(unname(key_map) %in% names(input))
    )
      return(invisible(NULL))
    scope <- metadata_overlay_scope(namespaces)
    keys <- lapply(key_map, function(column) as.character(input[[column]]))
    valid <- Reduce(
      `&`,
      lapply(keys, function(value) !is.na(value) & nzchar(value))
    )
    if (!any(valid)) return(invisible(NULL))
    selected <- which(valid)
    entity <- keys[[1L]][selected]
    if (identical(table_name, "variables")) {
      entity <- paste(
        keys$dataset_id[selected],
        keys$file_group_id[selected],
        keys$name[selected],
        sep = " / "
      )
    }
    for (field in names(fields[[table_name]])) {
      type <- if (field %in% c("latitude", "longitude")) "numeric" else
        "character"
      source_field <- value_map[[field]] %||% field
      value <- column_value(
        input,
        source_field,
        numeric = identical(type, "numeric")
      )[selected]
      target_ids <- vapply(
        selected,
        function(row) {
          identity <- c(
            scope,
            table_name,
            vapply(keys, `[[`, character(1), row),
            field
          )
          paste0("overlay_target_", digest::digest(identity, algo = "sha256"))
        },
        character(1)
      )
      key_value <- function(key)
        if (key %in% names(keys)) keys[[key]][selected] else ""
      field_label <- unname(fields[[table_name]][[field]])
      cohort <- paste(
        table_name,
        if (field %in% c("latitude", "longitude")) "coordinates" else field,
        if (table_name == "variables") key_value("name") else "",
        sep = "/"
      )
      rows[[length(rows) + 1L]] <<- dplyr::tibble(
        target_id = target_ids,
        source_scope = scope,
        source_label = source$label,
        table = table_name,
        entity_id = entity,
        dataset_id = key_value("dataset_id"),
        participant_id = key_value("participant_id"),
        device_id = key_value("device_id"),
        file_group_id = key_value("file_group_id"),
        name = key_value("name"),
        field = field,
        field_label = field_label,
        cohort = cohort,
        source_value = as.list(value),
        value_type = type,
        variable_type = if (is.null(variable_type)) NA_character_ else
          column_value(input, variable_type)[selected],
        source_path = paste0(
          path,
          "/",
          path_resource,
          "[",
          selected,
          "]/",
          source_field
        )
      )
    }
    invisible(NULL)
  }
  visit <- function(source, namespaces, path) {
    nested <- source$records$metadata_bundle %||% NULL
    if (inherits(nested, "llw_metadata_bundle")) {
      for (name in names(nested$sources)) {
        visit(
          nested$sources[[name]],
          c(namespaces, name),
          paste0(path, "/metadata_bundle/sources/", name, "/records")
        )
      }
      return(invisible(NULL))
    }
    records <- source$records
    core <- records$core_metadata %||% records
    core_path <- if ("core_metadata" %in% names(records)) "core_metadata/" else
      ""
    add_table(
      source,
      namespaces,
      path,
      "participants",
      core$participants,
      c(participant_id = "participant_internal_id"),
      list(label = "participant_label", sex = "participant_sex"),
      paste0(core_path, "participants")
    )
    add_table(
      source,
      namespaces,
      path,
      "devices",
      core$devices,
      c(device_id = "device_internal_id"),
      list(manufacturer = "device_manufacturer", model = "device_model"),
      paste0(core_path, "devices")
    )
    inventory <- records$dataset_inventory %||% NULL
    add_table(
      source,
      namespaces,
      path,
      "datasets",
      inventory,
      c(dataset_id = "dataset_id"),
      list(title = "title"),
      "dataset_inventory"
    )
    add_table(
      source,
      namespaces,
      path,
      "sites",
      inventory,
      c(dataset_id = "dataset_id"),
      list(
        site = "site",
        country = "country",
        source_timezone = "timezone",
        latitude = "latitude",
        longitude = "longitude"
      ),
      "dataset_inventory"
    )
    add_table(
      source,
      namespaces,
      path,
      "variables",
      records$variable_inventory,
      c(
        dataset_id = "dataset_id",
        file_group_id = "file_group_id",
        name = "name"
      ),
      list(label = "label", unit = "unit"),
      "variable_inventory",
      "type"
    )
    invisible(NULL)
  }
  for (name in names(bundle$sources)) visit(bundle$sources[[name]], name, name)
  if (length(rows) == 0L) return(metadata_overlay_empty_catalog())
  catalog <- dplyr::bind_rows(rows)
  duplicated <- duplicated(catalog$target_id)
  if (any(duplicated)) {
    for (id in unique(catalog$target_id[duplicated])) {
      positions <- which(catalog$target_id == id)
      values <- catalog$source_value[positions]
      if (!all(vapply(values, identical, logical(1), values[[1L]]))) {
        abort_llw(
          "Repeated metadata identities have conflicting retained values.",
          type = "validation"
        )
      }
    }
    catalog <- catalog[!duplicated, ]
  }
  catalog
}

metadata_overlay_upgrade <- function(overlay, catalog) {
  if (identical(overlay$overlay_version, metadata_overlay_version()))
    return(overlay)
  tables <- metadata_empty_overlay_tables(metadata_overlay_version())
  unresolved <- list()
  old_keys <- metadata_overlay_key_contract()
  for (table in names(tables)) {
    old <- overlay$tables[[table]]
    for (row in seq_len(nrow(old))) {
      candidates <- catalog[catalog$table == table, ]
      for (key in old_keys[[table]]) {
        candidates <- candidates[candidates[[key]] == old[[key]][[row]], ]
      }
      scopes <- unique(candidates$source_scope)
      if (length(scopes) != 1L) {
        unresolved[[length(unresolved) + 1L]] <- list(
          table = table,
          row = old[row, , drop = FALSE]
        )
        next
      }
      value <- old[row, , drop = FALSE]
      value$source_scope <- scopes[[1L]]
      if (identical(table, "participants")) value$sex <- NA_character_
      value <- value[, names(tables[[table]]), drop = FALSE]
      tables[[table]] <- rbind(tables[[table]], value)
    }
  }
  new_metadata_overlay(
    overlay$source_fingerprint,
    tables,
    overlay$revision,
    overlay$unknown,
    version = metadata_overlay_version(),
    unresolved = unresolved
  )
}

metadata_overlay_source_index <- function(index) {
  result <- index
  for (name in c("nodes", "entries", "groups")) {
    result[[name]] <- index[[name]][
      index[[name]]$layer != "LightLogWeb overlay",
      ,
      drop = FALSE
    ]
  }
  result$nodes$parent_row <- match(
    result$nodes$parent_node_id,
    result$nodes$node_id,
    nomatch = 0L
  )
  result
}

# Bind core/inventory views only through exact retained identities and agreeing
# source values. Similar labels, semantic terms and calibration units are not
# aliases of an editable variable field.
metadata_overlay_navigation_targets <- function(index, catalog) {
  result <- data.frame(
    path = catalog$source_path,
    target_id = catalog$target_id
  )
  result <- result[!is.na(result$path), , drop = FALSE]
  entries <- index$entries
  fields <- c(
    dataset_file_variables_labels = "label",
    dataset_file_variables_units = "unit"
  )
  aliases <- entries[entries$field %in% names(fields), , drop = FALSE]
  variables <- catalog[catalog$table == "variables", ]
  if (nrow(aliases) > 0L && nrow(variables) > 0L) {
    name_paths <- sub(
      "/dataset_file_variables_(labels|units)$",
      "/dataset_file_variables_name",
      aliases$path
    )
    alias_keys <- data.frame(
      source = sub("/core_metadata/.*$", "", aliases$path),
      file_group_id = aliases$record,
      name = entries$value[match(name_paths, entries$path)],
      field = unname(fields[aliases$field])
    )
    variable_keys <- data.frame(
      source = sub("/variable_inventory\\[.*$", "", variables$source_path),
      file_group_id = variables$file_group_id,
      name = variables$name,
      field = variables$field
    )
    keys <- metadata_overlay_row_key(variable_keys, names(variable_keys))
    ambiguous <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
    keys[ambiguous] <- NA_character_
    position <- match(
      metadata_overlay_row_key(alias_keys, names(alias_keys)),
      keys
    )
    source_value <- vapply(
      variables$source_value,
      metadata_scalar_text,
      character(1)
    )[position]
    agrees <- (!is.na(source_value) &
      !is.na(aliases$value) &
      source_value == aliases$value) |
      (is.na(source_value) & is.na(aliases$value))
    keep <- !is.na(position) & agrees
    result <- rbind(
      result,
      data.frame(
        path = aliases$path[keep],
        target_id = variables$target_id[position[keep]]
      )
    )
  }
  result
}

metadata_overlay_explorer_selection <- function(context, selection, overlay) {
  unavailable <- function(reason) list(editable = FALSE, reason = reason)
  group <- selection$group
  if (is.null(group) || !inherits(group, "llw_metadata_group")) {
    return(unavailable(
      "Choose a metadata field in the explorer to edit its overlay."
    ))
  }
  entries <- group$entries
  if (nrow(entries) == 0L)
    return(unavailable("This field has no retained item to annotate."))
  catalog <- context$catalog
  mapping <- context$navigation_targets
  ids <- mapping$target_id[match(entries$path, mapping$path)]
  # Overlay rows can move after removal. Resolve their current qualified keys,
  # never their row positions as long-lived target identities.
  is_overlay <- entries$namespace_id == metadata_overlay_namespace_id()
  for (table in unique(entries$resource[is_overlay])) {
    supported <- names(metadata_overlay_fields()[[table]])
    selected <- which(
      is_overlay & entries$resource == table & entries$field %in% supported
    )
    if (length(selected) == 0L) next
    prefix <- paste0(metadata_overlay_namespace_id(), "/", table, "[")
    selected <- selected[startsWith(entries$path[selected], prefix)]
    row <- suppressWarnings(as.integer(sub(
      "\\].*$",
      "",
      substring(entries$path[selected], nchar(prefix) + 1L)
    )))
    values <- overlay$tables[[table]]
    valid <- !is.na(row) & row >= 1L & row <= nrow(values)
    selected <- selected[valid]
    row <- row[valid]
    if (length(selected) == 0L) next
    keys <- metadata_overlay_key_contract(overlay$overlay_version)[[table]]
    candidates <- catalog[catalog$table == table, ]
    requested <- values[row, keys, drop = FALSE]
    requested$field <- entries$field[selected]
    keys <- c(keys, "field")
    candidate_keys <- metadata_overlay_row_key(candidates, keys)
    duplicate <- duplicated(candidate_keys) |
      duplicated(candidate_keys, fromLast = TRUE)
    candidate_keys[duplicate] <- NA_character_
    position <- match(metadata_overlay_row_key(requested, keys), candidate_keys)
    ids[selected] <- candidates$target_id[position]
  }
  if (anyNA(ids)) {
    if (any(is_overlay)) {
      return(unavailable(
        if (any(entries$resource == "unresolved_legacy")) {
          "This preserved legacy overlay has no unambiguous source binding and is not applied. It remains retained and searchable; resolving it here is not supported."
        } else {
          "This overlay field is read-only. Only source-bound fields in the typed authoring contract can be edited; overlay identities and unmodeled content stay protected."
        }
      ))
    }
    return(unavailable(
      "This source field is read-only. IDs, relationships, calibration and unmodeled fields are not part of the typed overlay contract."
    ))
  }
  positions <- match(ids, catalog$target_id)
  if (length(unique(catalog$cohort[positions])) != 1L) {
    return(unavailable(
      "This grouped field contains different exact source variables. It cannot be overwritten as one field."
    ))
  }
  preferred <- 1L
  focus_path <- selection$focus_path
  if (
    is.character(focus_path) && length(focus_path) == 1L && !is.na(focus_path)
  ) {
    nearby <- which(startsWith(entries$path, paste0(focus_path, "/")))
    if (length(nearby) == 1L) preferred <- nearby[[1L]]
  }
  target <- catalog[positions[[preferred]], ]
  if (target$field == "longitude") {
    target <- catalog[
      catalog$table == "sites" &
        catalog$field == "latitude" &
        catalog$dataset_id == target$dataset_id &
        catalog$source_scope == target$source_scope,
    ]
  }
  field <- paste(target$table, target$field, sep = "/")
  cohort <- catalog[
    catalog$cohort == target$cohort & catalog$field == target$field,
  ]
  list(
    editable = TRUE,
    field = field,
    matching_id = cohort$target_id[[1L]],
    target_id = target$target_id[[1L]],
    target_ids = unique(ids),
    label = if (target$field == "latitude")
      "Coordinates (latitude and longitude)" else target$field_label[[1L]],
    source_name = if (target$table == "variables") target$name[[1L]] else NULL,
    source_field = group$group$field[[1L]],
    group_id = group$group$group_id[[1L]]
  )
}

# Only missing navigation bindings are offered by the secondary authoring
# route. A similarly named study/contributor field is never a dataset alias.
metadata_overlay_missing_targets <- function(context) {
  mapping <- context$navigation_targets
  available <- mapping$target_id[
    mapping$path %in% context$source_index$entries$path
  ]
  catalog <- context$catalog
  missing <- !catalog$target_id %in% available
  coordinates <- catalog$field %in% c("latitude", "longitude")
  # A coordinate pair already has a contextual route when either component
  # exists. Do not offer a half-pair through the missing-annotation route.
  pair <- metadata_overlay_row_key(catalog, c("source_scope", "dataset_id"))
  missing[coordinates & pair %in% pair[coordinates & !missing]] <- FALSE
  catalog$target_id[missing]
}

# Sparse, disposable presentation of current assertions. Neither retained
# source entries nor their search/group identities are rewritten.
metadata_overlay_presentation <- function(context, overlay) {
  overlay <- metadata_overlay_upgrade(overlay, context$catalog)
  current <- metadata_overlay_values(overlay, context$catalog)
  changed <- which(current$overridden)
  mapping <- context$navigation_targets
  targets <- match(mapping$target_id, context$catalog$target_id[changed])
  positions <- match(mapping$path, context$source_index$entries$path)
  keep <- !is.na(targets) & !is.na(positions)
  positions <- positions[keep]
  values <- current$values[changed[targets[keep]]]
  entries <- dplyr::tibble(
    path = mapping$path[keep],
    value = vapply(values, metadata_scalar_text, character(1)),
    value_type = vapply(values, metadata_value_type, character(1))
  )
  group_ids <- unique(context$source_index$entries$group_id[positions])
  groups <- dplyr::tibble(
    group_id = group_ids,
    value_preview = character(length(group_ids))
  )
  source_entries <- context$source_index$entries
  affected <- which(source_entries$group_id %in% group_ids)
  by_group <- split(
    affected,
    match(source_entries$group_id[affected], group_ids)
  )
  replacement <- match(source_entries$path, entries$path)
  for (i in seq_along(group_ids)) {
    rows <- by_group[[as.character(i)]]
    matched <- replacement[rows]
    values <- source_entries$value[rows]
    hit <- !is.na(matched)
    values[hit] <- entries$value[matched[hit]]
    groups$value_preview[[i]] <- metadata_group_value_preview(values)
  }
  list(entries = entries, groups = groups, revision = overlay$revision)
}

metadata_overlay_context <- function(record) {
  if (!inherits(record, "llw_dataset_record"))
    abort_llw("Select a dataset first.", type = "validation")
  bundle <- record$metadata_bundle
  catalog <- metadata_overlay_source_catalog(bundle)
  if (
    nrow(catalog) == 0L &&
      length(bundle$sources) == 1L &&
      !identical(bundle$sources[[1L]]$source_type, "append_source")
  ) {
    # Initialization only: derive identities from the already imported payload.
    # The module caches this context. No source fact or namespace is replaced.
    observed <- unserialize(record$raw_payload)
    source <- bundle$sources[[1L]]
    variables <- setdiff(
      names(observed),
      c("Id", "Datetime", "llw_source_dataset_id")
    )
    types <- vapply(
      observed[variables],
      function(value) {
        if (is.factor(value)) "factor" else if (is.logical(value))
          "boolean" else if (is.numeric(value)) "numeric" else "string"
      },
      character(1)
    )
    ids <- if ("Id" %in% names(observed)) unique(as.character(observed$Id)) else
      character()
    ids <- ids[!is.na(ids) & nzchar(ids)]
    temporary <- source
    factual <- source$records$factual_metadata %||% record$factual_metadata
    variable_fact <- function(field)
      vapply(
        variables,
        function(name) {
          value <- factual$variables[[name]][[field]] %||%
            if (field == "unit")
              record$source_manifest$details$units[[name]] else NULL
          if (is.character(value) && length(value) == 1L) value else
            NA_character_
        },
        character(1)
      )
    temporary$records <- list(
      core_metadata = list(
        participants = data.frame(participant_internal_id = ids)
      ),
      dataset_inventory = data.frame(
        dataset_id = record$id,
        timezone = record$source_manifest$source_timezone %||% NA_character_
      ),
      variable_inventory = data.frame(
        dataset_id = rep(record$id, length(variables)),
        file_group_id = rep(
          paste0("llw:import:", record$id),
          length(variables)
        ),
        name = variables,
        type = unname(types),
        label = variable_fact("label"),
        unit = variable_fact("unit")
      )
    )
    catalog <- metadata_overlay_source_catalog(list(
      sources = stats::setNames(list(temporary), source$namespace_id)
    ))
    catalog$source_path <- NA_character_
    catalog$source_label <- paste(source$label, "(imported identities)")
  }
  source_index <- metadata_overlay_source_index(bundle$index)
  structure(
    list(
      dataset_id = record$id,
      source_fingerprint = bundle$source_fingerprint,
      catalog = catalog,
      navigation_targets = metadata_overlay_navigation_targets(
        source_index,
        catalog
      ),
      row_targets = Map(
        function(keys, name) {
          positions <- which(catalog$table == name)
          values <- metadata_overlay_row_key(catalog[positions, keys], keys)
          keep <- !duplicated(values)
          list(
            keys = keys,
            values = values[keep],
            rows = catalog[positions[keep], keys],
            ids = catalog$target_id[positions][keep]
          )
        },
        metadata_overlay_key_contract(metadata_overlay_version()),
        names(metadata_overlay_key_contract(metadata_overlay_version()))
      ),
      source_index = source_index,
      overlay_paths_disjoint = !any(startsWith(
        tolower(enc2utf8(source_index$entries$path)),
        paste0(metadata_overlay_namespace_id(), "/")
      )),
      overlay_labels_disjoint = !any(startsWith(
        tolower(enc2utf8(unique(source_index$entries$source_label))),
        "lightlogweb overlay revision "
      ))
    ),
    class = "llw_overlay_context"
  )
}

metadata_overlay_row_targets <- function(context, table, name, keys) {
  cached <- context$row_targets[[name]]
  if (!is.null(cached$rows) && identical(cached$keys, keys)) {
    return(cached$ids[metadata_overlay_match_rows(
      table,
      cached$rows,
      keys,
      cached$values
    )])
  }
  if (is.null(cached) || !identical(cached$keys, keys)) {
    positions <- which(context$catalog$table == name)
    values <- metadata_overlay_row_key(context$catalog[, keys], keys)[positions]
    cached <- list(values = values, ids = context$catalog$target_id[positions])
  }
  cached$ids[match(metadata_overlay_row_key(table, keys), cached$values)]
}

metadata_overlay_assert_context <- function(record, context) {
  if (
    !inherits(context, "llw_overlay_context") ||
      !identical(context$dataset_id, record$id) ||
      !identical(
        context$source_fingerprint,
        record$metadata_bundle$source_fingerprint
      )
  ) {
    abort_llw(
      "The metadata source changed. Reopen the editor for the current dataset.",
      type = "validation"
    )
  }
  invisible(TRUE)
}

metadata_overlay_values <- function(overlay, targets) {
  result <- targets$source_value
  overridden <- rep(FALSE, nrow(targets))
  for (table in unique(targets$table)) {
    positions <- which(targets$table == table)
    values <- overlay$tables[[table]]
    keys <- metadata_overlay_key_contract(overlay$overlay_version)[[table]]
    matches <- match(
      metadata_overlay_row_key(targets[positions, ], keys),
      metadata_overlay_row_key(values, keys)
    )
    for (i in which(!is.na(matches))) {
      position <- positions[[i]]
      field <- targets$field[[position]]
      if (!field %in% names(values)) next
      value <- values[[field]][[matches[[i]]]]
      if (!is.na(value)) {
        result[[position]] <- value
        overridden[[position]] <- TRUE
      }
    }
  }
  list(values = result, overridden = overridden)
}

metadata_overlay_target_scope <- function(
  context,
  target_id,
  scope = c("all", "single"),
  eligible_ids = NULL
) {
  scope <- match.arg(scope)
  if (!is.null(eligible_ids)) {
    if (
      !is.character(eligible_ids) ||
        anyNA(eligible_ids) ||
        !all(eligible_ids %in% context$catalog$target_id) ||
        !target_id %in% eligible_ids
    ) {
      abort_llw(
        "Choose an available missing annotation target.",
        type = "validation"
      )
    }
    context$catalog <- context$catalog[
      context$catalog$target_id %in% eligible_ids,
    ]
  }
  anchor <- match(target_id, context$catalog$target_id)
  if (is.na(anchor))
    abort_llw("Choose an available metadata item.", type = "validation")
  if (scope == "single") {
    if (context$catalog$field[[anchor]] %in% c("latitude", "longitude")) {
      return(context$catalog[
        context$catalog$source_scope == context$catalog$source_scope[[anchor]] &
          context$catalog$table == "sites" &
          context$catalog$dataset_id == context$catalog$dataset_id[[anchor]] &
          context$catalog$field %in% c("latitude", "longitude"),
      ])
    }
    return(context$catalog[anchor, ])
  }
  context$catalog[context$catalog$cohort == context$catalog$cohort[[anchor]], ]
}

metadata_overlay_validate_value <- function(
  targets,
  value,
  action = c("set", "remove")
) {
  action <- match.arg(action)
  numeric <- identical(unique(targets$value_type), "numeric")
  coordinates <- setequal(unique(targets$field), c("latitude", "longitude"))
  if (coordinates) {
    if (action == "remove") return(c(latitude = NA_real_, longitude = NA_real_))
    if (
      !is.numeric(value) ||
        !identical(names(value), c("latitude", "longitude")) ||
        length(value) != 2L ||
        anyNA(value) ||
        any(!is.finite(value)) ||
        abs(value[[1L]]) > 90 ||
        abs(value[[2L]]) > 180
    ) {
      abort_llw(
        "Enter paired finite latitude (-90 to 90) and longitude (-180 to 180).",
        type = "validation"
      )
    }
    return(value)
  }
  if (action == "remove") return(if (numeric) NA_real_ else NA_character_)
  if (numeric) {
    if (
      !is.numeric(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value)
    ) {
      abort_llw("Enter one finite numeric value.", type = "validation")
    }
    limit <- if (identical(unique(targets$field), "latitude")) 90 else 180
    if (abs(value) > limit)
      abort_llw(
        paste0("Enter a value between -", limit, " and ", limit, "."),
        type = "validation"
      )
    return(as.numeric(value))
  }
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    abort_llw(
      "Enter a non-empty value, or choose Remove override.",
      type = "validation"
    )
  }
  value <- enc2utf8(value)
  field <- unique(targets$field)
  if (identical(field, "source_timezone") && !value %in% OlsonNames()) {
    abort_llw(
      "Use an IANA timezone such as Europe/Berlin.",
      type = "validation"
    )
  }
  if (identical(field, "unit")) {
    placeholder <- gsub("[[:space:]_-]+", " ", tolower(trimws(value)))
    if (
      placeholder %in%
        c(
          "n/a",
          "n.a.",
          "na",
          "unknown",
          "unspecified",
          "not specified",
          "unit not specified",
          "not available",
          "not applicable",
          "not recorded",
          "missing",
          "none",
          "null"
        )
    ) {
      abort_llw(
        "Use a UCUM code or precise custom unit, not a placeholder. To stop overriding the source, choose Remove override.",
        type = "validation"
      )
    }
    if (any(targets$variable_type %in% c("string", "boolean", "factor"))) {
      abort_llw(
        "Units do not apply to string, boolean or factor variables.",
        type = "validation"
      )
    }
  }
  value
}

metadata_overlay_legacy_conflicts <- function(overlay, targets) {
  result <- rep(list(character()), nrow(targets))
  for (legacy in overlay$unresolved %||% list()) {
    positions <- which(
      targets$table == legacy$table &
        !targets$target_id %in% (legacy$resolved_targets %||% character())
    )
    if (length(positions) == 0L) next
    for (key in metadata_overlay_key_contract()[[legacy$table]]) {
      positions <- positions[
        targets[[key]][positions] == legacy$row[[key]][[1L]]
      ]
    }
    for (position in positions) {
      field <- targets$field[[position]]
      value <- legacy$row[[field]] %||% NA_character_
      if (length(value) == 1L && !is.na(value)) {
        result[[position]] <- c(result[[position]], metadata_scalar_text(value))
      }
    }
  }
  result
}

metadata_overlay_preview <- function(
  record,
  context,
  target_id,
  value = NULL,
  scope = c("all", "single"),
  action = c("set", "remove"),
  resolve_legacy = FALSE,
  eligible_ids = NULL
) {
  metadata_overlay_assert_context(record, context)
  scope <- match.arg(scope)
  action <- match.arg(action)
  targets <- metadata_overlay_target_scope(
    context,
    target_id,
    scope,
    eligible_ids
  )
  value <- metadata_overlay_validate_value(targets, value, action)
  overlay <- metadata_overlay_upgrade(
    record$metadata_bundle$overlay,
    context$catalog
  )
  assert_flag(resolve_legacy, "resolve_legacy")
  legacy <- metadata_overlay_legacy_conflicts(overlay, targets)
  if (any(lengths(legacy) > 0L) && !resolve_legacy) {
    abort_llw(
      paste(
        "An older overlay value has no unique source.",
        "Review the older values and explicitly resolve this assignment before applying."
      ),
      type = "validation"
    )
  }
  old <- metadata_overlay_values(overlay, targets)
  proposed <- if (action == "remove") targets$source_value else if (
    length(value) == 2L
  ) {
    lapply(targets$field, function(field) unname(value[[field]]))
  } else rep(list(value), nrow(targets))
  changed <- if (action == "remove") old$overridden else
    vapply(
      seq_len(nrow(targets)),
      function(i) {
        !identical(old$values[[i]], proposed[[i]])
      },
      logical(1)
    )
  structure(
    list(
      dataset_id = record$id,
      source_fingerprint = context$source_fingerprint,
      base_revision = record$revision,
      overlay_revision = overlay$revision,
      target_id = target_id,
      target_ids = targets$target_id,
      scope = scope,
      action = action,
      value = value,
      targets = targets,
      before = old$values,
      proposed = proposed,
      overridden = old$overridden,
      changed = changed,
      resolve_legacy = resolve_legacy,
      eligible_ids = eligible_ids,
      legacy = legacy,
      fingerprint = metadata_fingerprint(list(
        record$id,
        record$revision,
        overlay$revision,
        context$source_fingerprint,
        targets$target_id,
        action,
        value,
        resolve_legacy
      ))
    ),
    class = "llw_overlay_preview"
  )
}

metadata_overlay_patch_tables <- function(overlay, preview) {
  tables <- overlay$tables
  targets <- if (all(preview$changed)) preview$targets else
    preview$targets[preview$changed, ]
  proposed <- preview$proposed[preview$changed]
  for (name in unique(targets$table)) {
    positions <- which(targets$table == name)
    target <- if (length(positions) == nrow(targets)) targets else
      targets[positions, ]
    value <- tables[[name]]
    keys <- metadata_overlay_key_contract(metadata_overlay_version())[[name]]
    target_keys <- metadata_overlay_row_key(target, keys)
    current_keys <- metadata_overlay_row_key(value, keys)
    missing <- which(!target_keys %in% current_keys & !duplicated(target_keys))
    if (length(missing) > 0L) {
      additions <- value[rep(NA_integer_, length(missing)), , drop = FALSE]
      for (key in keys) additions[[key]] <- target[[key]][missing]
      value <- rbind(value, additions)
    }
    matches <- match(target_keys, metadata_overlay_row_key(value, keys))
    for (field in unique(target$field)) {
      selected <- which(target$field == field)
      replacement <- if (preview$action == "remove") {
        if (is.numeric(value[[field]])) NA_real_ else NA_character_
      } else unlist(proposed[positions[selected]], use.names = FALSE)
      value[[field]][matches[selected]] <- replacement
    }
    fields <- setdiff(names(value), keys)
    keep <- Reduce(`|`, lapply(value[fields], function(column) !is.na(column)))
    value <- value[keep, , drop = FALSE]
    rownames(value) <- NULL
    tables[[name]] <- metadata_validate_overlay_table(
      value,
      name,
      metadata_overlay_version()
    )
  }
  tables
}

metadata_overlay_index_rows <- function(rows, positions) {
  # Index row names carry no identity. Subset the typed columns directly so base
  # data-frame row-name repair does not copy long tables during a single edit.
  result <- lapply(rows, `[`, positions)
  structure(
    result,
    class = "data.frame",
    row.names = .set_row_names(length(positions))
  )
}

metadata_overlay_bind_index <- function(parts) {
  populated <- vapply(parts, nrow, integer(1)) > 0L
  if (!any(populated)) return(parts[[1L]])
  parts <- parts[populated]
  if (length(parts) == 1L) return(parts[[1L]])
  result <- lapply(names(parts[[1L]]), function(column) {
    unlist(lapply(parts, `[[`, column), use.names = FALSE)
  })
  names(result) <- names(parts[[1L]])
  structure(
    result,
    class = "data.frame",
    row.names = .set_row_names(sum(vapply(parts, nrow, integer(1))))
  )
}

metadata_overlay_typed_index <- function(context, overlay) {
  # Known tables are flat and typed. Build their rows by column, not by walking
  # every scalar through the generic nested-source parser. Unknown content still
  # uses that parser, so its structure is retained without guessing a schema.
  other <- overlay
  other$tables <- metadata_empty_overlay_tables(overlay$overlay_version)
  extra <- new_metadata_index(list(
    sources = list(),
    overlay = other,
    source_fingerprint = context$source_fingerprint
  ))
  prefix <- function(value)
    ifelse(nzchar(value), paste0("overlay_unknown_", value), value)
  extra$entries$entry_id <- prefix(extra$entries$entry_id)
  for (part in c("nodes", "entries")) {
    for (column in c("node_id", "parent_node_id")) {
      extra[[part]][[column]] <- prefix(extra[[part]][[column]])
    }
  }
  if (!any(vapply(overlay$tables, nrow, integer(1)) > 0L)) return(extra)
  namespace <- metadata_overlay_namespace_id()
  label <- paste("LightLogWeb overlay revision", overlay$revision)
  root <- if (nrow(extra$nodes)) extra$nodes$node_id[[1L]] else "overlay_root"
  node <- function(
    id,
    parent,
    resource,
    label_value,
    kind,
    path,
    identity = ""
  ) {
    data.frame(
      node_id = id,
      parent_node_id = parent,
      namespace_id = namespace,
      source_dataset_id = namespace,
      source_label = label,
      state = "complete",
      layer = "LightLogWeb overlay",
      resource = resource,
      label = label_value,
      kind = kind,
      path = path,
      record_identity = identity,
      direct_value_count = 0L,
      child_node_count = 0L,
      parent_row = 0L,
      stringsAsFactors = FALSE
    )
  }
  nodes <- list(extra$nodes)
  entries <- list(extra$entries)
  if (nrow(extra$nodes) == 0L)
    nodes[[1L]] <- node(root, "", "", label, "source", namespace)
  keys <- metadata_overlay_key_contract(overlay$overlay_version)
  for (name in names(overlay$tables)) {
    table <- overlay$tables[[name]]
    n <- nrow(table)
    if (n == 0L) next
    table_id <- paste0("overlay_table_", name)
    table_path <- paste0(namespace, "/", name)
    row_ids <- metadata_overlay_row_targets(context, table, name, keys[[name]])
    if (anyNA(row_ids))
      abort_llw(
        "An overlay row has no retained source-qualified target.",
        type = "validation"
      )
    row_ids <- paste0("overlay_row_", row_ids)
    identity <- table[[setdiff(keys[[name]], "source_scope")[[1L]]]]
    if (name == "variables") identity <- table$file_group_id
    row_labels <- if (name == "variables") table$name else identity
    row_paths <- paste0(table_path, "[", seq_len(n), "]")
    nodes[[length(nodes) + 1L]] <- node(
      table_id,
      root,
      name,
      name,
      "table",
      table_path
    )
    nodes[[length(nodes) + 1L]] <- node(
      row_ids,
      table_id,
      name,
      row_labels,
      "record",
      row_paths,
      identity
    )
    fields <- rep(names(table), each = n)
    positions <- rep(seq_len(n), times = ncol(table))
    paths <- paste0(row_paths[positions], "/", fields)
    group_paths <- paste0(table_path, "/", fields)
    discriminators <- if (name == "variables")
      paste0("name=", table$name[positions]) else rep("", length(fields))
    group_ids <- paste0(
      "overlay_group_",
      name,
      "/",
      fields,
      "/",
      nchar(discriminators),
      ":",
      discriminators
    )
    values <- unlist(
      lapply(table, function(column) {
        missing <- is.na(column)
        if (is.numeric(column))
          column <- format(
            column,
            digits = 15L,
            scientific = FALSE,
            trim = TRUE
          )
        column[missing | !nzchar(column)] <- NA_character_
        as.character(column)
      }),
      use.names = FALSE
    )
    types <- rep(vapply(table, metadata_value_type, character(1)), each = n)
    rows <- data.frame(
      entry_id = paste0(row_ids[positions], "/", fields),
      node_id = row_ids[positions],
      parent_node_id = row_ids[positions],
      namespace_id = namespace,
      source_dataset_id = namespace,
      source_label = label,
      state = "complete",
      layer = "LightLogWeb overlay",
      resource = name,
      record = identity[positions],
      path = paths,
      group_path = group_paths,
      group_discriminator = discriminators,
      group_context = if (name == "variables") table$name[positions] else "",
      group_id = group_ids,
      field = fields,
      value = values,
      value_type = types,
      search_text = tolower(enc2utf8(paste(
        fields,
        gsub("[_-]+", " ", fields),
        ifelse(is.na(values), "", values)
      ))),
      stringsAsFactors = FALSE
    )
    for (column in c(
      "sort_rank_path",
      "sort_rank_field",
      "sort_rank_value",
      "sort_rank_source"
    ))
      rows[[column]] <- 0L
    entries[[length(entries) + 1L]] <- rows
  }
  extra$nodes <- metadata_overlay_bind_index(nodes)
  extra$entries <- metadata_overlay_bind_index(entries)
  extra$groups <- metadata_index_groups(extra$entries)
  extra$nodes$direct_value_count <- tabulate(
    match(extra$entries$node_id, extra$nodes$node_id),
    nrow(extra$nodes)
  )
  extra$nodes$child_node_count <- tabulate(
    match(extra$nodes$parent_node_id, extra$nodes$node_id),
    nrow(extra$nodes)
  )
  extra$nodes$parent_row <- match(
    extra$nodes$parent_node_id,
    extra$nodes$node_id,
    nomatch = 0L
  )
  extra
}

metadata_overlay_rank_entries <- function(
  entries,
  columns = c("path", "field", "value", "source_label")
) {
  if (length(columns) == 0L) return(entries)
  # Every ordering uses the same exact identity tie-break. Rank it once, rather
  # than repeatedly radix-sorting long source-qualified IDs for each property.
  identity_order <- order(entries$entry_id, method = "radix")
  identity_rank <- integer(nrow(entries))
  identity_rank[identity_order] <- seq_along(identity_order)
  for (column in columns) {
    values <- entries[[column]]
    if (column == "path") {
      normalized <- tolower(enc2utf8(values))
      if (anyNA(normalized)) normalized[is.na(normalized)] <- ""
    } else {
      unique_values <- unique(values)
      normalized_values <- tolower(enc2utf8(unique_values))
      if (anyNA(normalized_values))
        normalized_values[is.na(normalized_values)] <- ""
      normalized <- normalized_values[match(values, unique_values)]
    }
    ordering <- order(normalized, identity_rank, method = "radix")
    rank <- integer(length(ordering))
    rank[ordering] <- seq_along(ordering)
    suffix <- if (column == "source_label") "source" else column
    entries[[paste0("sort_rank_", suffix)]] <- rank
  }
  entries
}

metadata_overlay_match_rows <- function(
  after,
  before,
  keys,
  before_keys = NULL
) {
  if (nrow(after) == 0L) return(integer())
  if (
    all(vapply(
      keys,
      function(key) identical(after[[key]], before[[key]]),
      logical(1)
    ))
  ) {
    return(seq_len(nrow(after)))
  }
  # A unique component can locate candidates cheaply, but never establishes
  # identity alone. Verify every source-qualified key before accepting a match.
  for (key in keys) {
    if (anyDuplicated(before[[key]])) next
    positions <- match(after[[key]], before[[key]])
    if (
      !anyNA(positions) &&
        all(vapply(
          keys,
          function(column) {
            identical(after[[column]], before[[column]][positions])
          },
          logical(1)
        ))
    )
      return(positions)
  }
  match(
    metadata_overlay_row_key(after, keys),
    before_keys %||% metadata_overlay_row_key(before, keys)
  )
}

metadata_overlay_patch_existing_index <- function(context, overlay, previous) {
  if (
    is.null(previous) ||
      !identical(previous$overlay$overlay_version, overlay$overlay_version) ||
      !identical(previous$overlay$unknown, overlay$unknown) ||
      !identical(previous$overlay$unresolved, overlay$unresolved)
  )
    return(NULL)
  keys <- metadata_overlay_key_contract(overlay$overlay_version)
  row_matches <- list()
  for (name in names(overlay$tables)) {
    row_matches[[name]] <- metadata_overlay_match_rows(
      overlay$tables[[name]],
      previous$overlay$tables[[name]],
      keys[[name]]
    )
    if (anyNA(row_matches[[name]])) return(NULL)
  }
  index <- previous$index
  changed_entries <- integer()
  affected_groups <- character()
  removed_nodes <- character()
  changed_paths <- FALSE
  for (name in names(overlay$tables)) {
    before <- previous$overlay$tables[[name]]
    after <- overlay$tables[[name]]
    if (identical(before, after)) next
    row_ids <- metadata_overlay_row_targets(context, after, name, keys[[name]])
    if (nrow(before) != nrow(after)) {
      removed <- setdiff(seq_len(nrow(before)), row_matches[[name]])
      removed_ids <- metadata_overlay_row_targets(
        context,
        before[removed, , drop = FALSE],
        name,
        keys[[name]]
      )
      if (anyNA(removed_ids)) return(NULL)
      removed_nodes <- c(removed_nodes, paste0("overlay_row_", removed_ids))
      if (nrow(after) == 0L)
        removed_nodes <- c(removed_nodes, paste0("overlay_table_", name))
      if (nrow(after) > 0L) {
        nodes <- paste0("overlay_row_", row_ids)
        paths <- paste0(
          metadata_overlay_namespace_id(),
          "/",
          name,
          "[",
          seq_len(nrow(after)),
          "]"
        )
        positions <- match(nodes, index$nodes$node_id)
        if (anyNA(positions)) return(NULL)
        index$nodes$path[positions] <- paths
        entry_nodes <- match(index$entries$node_id, nodes)
        keep <- which(!is.na(entry_nodes))
        new_paths <- paste0(
          paths[entry_nodes[keep]],
          "/",
          index$entries$field[keep]
        )
        prior_positions <- match(new_paths, index$entries$path)
        if (isTRUE(context$overlay_paths_disjoint) && !anyNA(prior_positions)) {
          # Compacting a typed table reuses existing physical path slots. Their
          # cached ordering is exact even when row numbers sort lexically (10
          # before 2). Stable entry identities stay attached to their values.
          index$entries$sort_rank_path[keep] <-
            index$entries$sort_rank_path[prior_positions]
        } else {
          changed_paths <- TRUE
        }
        index$entries$path[keep] <- new_paths
      }
    }
    before <- before[row_matches[[name]], , drop = FALSE]
    for (field in setdiff(names(after), keys[[name]])) {
      different <- xor(is.na(before[[field]]), is.na(after[[field]])) |
        (!is.na(before[[field]]) &
          !is.na(after[[field]]) &
          before[[field]] != after[[field]])
      if (!any(different)) next
      ids <- paste0("overlay_row_", row_ids[different], "/", field)
      positions <- match(ids, index$entries$entry_id)
      if (anyNA(positions)) return(NULL)
      values <- after[[field]][different]
      missing <- is.na(values)
      if (is.numeric(values))
        values <- format(values, digits = 15L, scientific = FALSE, trim = TRUE)
      values[missing | !nzchar(values)] <- NA_character_
      index$entries$value[positions] <- values
      index$entries$search_text[positions] <- tolower(enc2utf8(paste(
        field,
        gsub("[_-]+", " ", field),
        ifelse(is.na(values), "", values)
      )))
      changed_entries <- c(changed_entries, positions)
    }
  }
  affected_groups <- unique(c(
    affected_groups,
    index$entries$group_id[changed_entries],
    index$entries$group_id[index$entries$node_id %in% removed_nodes]
  ))
  if (length(removed_nodes)) {
    index$entries <- metadata_overlay_index_rows(
      index$entries,
      which(!index$entries$node_id %in% removed_nodes)
    )
    index$nodes <- metadata_overlay_index_rows(
      index$nodes,
      which(!index$nodes$node_id %in% removed_nodes)
    )
    if (!any(index$entries$layer == "LightLogWeb overlay")) {
      index$nodes <- index$nodes[
        index$nodes$layer != "LightLogWeb overlay",
        ,
        drop = FALSE
      ]
    }
    index$nodes$parent_row <- match(
      index$nodes$parent_node_id,
      index$nodes$node_id,
      nomatch = 0L
    )
    index$nodes$child_node_count <- tabulate(
      match(index$nodes$parent_node_id, index$nodes$node_id),
      nrow(index$nodes)
    )
  }
  label <- paste("LightLogWeb overlay revision", overlay$revision)
  for (part in c("nodes", "entries", "groups")) {
    index[[part]]$source_label[
      index[[part]]$layer == "LightLogWeb overlay"
    ] <- label
  }
  index$nodes$label[
    index$nodes$kind == "source" & index$nodes$layer == "LightLogWeb overlay"
  ] <- label
  group_ids <- affected_groups
  if (length(group_ids)) {
    updated <- metadata_index_groups(
      index$entries,
      which(index$entries$group_id %in% group_ids)
    )
    index$groups <- index$groups[
      !index$groups$group_id %in% setdiff(group_ids, updated$group_id),
      ,
      drop = FALSE
    ]
    positions <- match(updated$group_id, index$groups$group_id)
    for (column in c(
      "value_preview",
      "distinct_value_count",
      "value_type",
      "entry_count",
      "record_count"
    )) {
      index$groups[[column]][positions] <- updated[[column]]
    }
  }
  index$entries <- metadata_overlay_rank_entries(
    index$entries,
    c(
      if (length(changed_entries)) "value",
      if (!isTRUE(context$overlay_labels_disjoint)) "source_label",
      if (changed_paths) "path"
    )
  )
  index$groups$sort_rank_value <- metadata_index_sort_rank(
    index$groups$value_preview,
    index$groups$group_id
  )
  index$groups$sort_rank_source <- metadata_index_sort_rank(
    index$groups$source_label,
    index$groups$group_id
  )
  validate_metadata_index(index, context$source_fingerprint)
}

metadata_overlay_patch_index <- function(context, overlay, previous = NULL) {
  patched <- metadata_overlay_patch_existing_index(context, overlay, previous)
  if (!is.null(patched)) return(patched)
  extra <- metadata_overlay_typed_index(context, overlay)
  index <- context$source_index
  for (part in c("nodes", "entries", "groups")) {
    index[[part]] <- metadata_overlay_bind_index(list(
      index[[part]],
      extra[[part]]
    ))
  }
  index$entries <- metadata_overlay_rank_entries(index$entries)
  ranked <- index$groups
  ranked$path <- ranked$group_path
  ranked$value <- ranked$value_preview
  ranked <- metadata_index_add_sort_ranks(ranked, identity = ranked$group_id)
  for (column in c(
    "sort_rank_path",
    "sort_rank_field",
    "sort_rank_value",
    "sort_rank_source"
  )) {
    index$groups[[column]] <- ranked[[column]]
  }
  index$nodes$parent_row <- match(
    index$nodes$parent_node_id,
    index$nodes$node_id,
    nomatch = 0L
  )
  validate_metadata_index(index, context$source_fingerprint)
}

metadata_overlay_replace <- function(
  record,
  context,
  overlay,
  history = TRUE,
  index_base = record$metadata_bundle
) {
  metadata_overlay_assert_context(record, context)
  bundle <- record$metadata_bundle
  bundle$overlay <- overlay
  bundle$index <- metadata_overlay_patch_index(
    context,
    overlay,
    index_base
  )
  if (as.numeric(utils::object.size(bundle)) > 32 * 1024^2) {
    abort_llw(
      "This metadata update exceeds the 32 MiB bundle budget. Reduce the edit scope.",
      type = "validation"
    )
  }
  if (history) {
    snapshot <- dataset_snapshot(record)
    attr(snapshot, "llw_change_type") <- "metadata_overlay"
    record <- push_dataset_history(record, snapshot)
  }
  record$metadata_bundle <- bundle
  record$revision <- record$revision + 1L
  record
}

# Only patch objects built from the retained catalog are accepted. Measurement
# payloads cannot enter this API and are never decoded by an edit or undo.
apply_metadata_overlay <- function(record, context, preview) {
  metadata_overlay_assert_context(record, context)
  if (
    !inherits(preview, "llw_overlay_preview") ||
      !identical(preview$dataset_id, record$id) ||
      !identical(preview$base_revision, record$revision) ||
      !identical(
        preview$overlay_revision,
        record$metadata_bundle$overlay$revision
      )
  ) {
    abort_llw(
      "The preview is stale. Review changes again before applying.",
      type = "validation"
    )
  }
  if (!is.null(record$draft)) {
    abort_llw(
      "Finish or discard the dataset preparation draft before editing metadata.",
      type = "validation"
    )
  }
  verified <- metadata_overlay_preview(
    record,
    context,
    preview$target_id,
    preview$value,
    preview$scope,
    preview$action,
    preview$resolve_legacy,
    eligible_ids = preview$eligible_ids
  )
  if (!identical(preview, verified)) {
    abort_llw(
      "The edit target or preview changed. Review the current targets again.",
      type = "validation"
    )
  }
  resolving <- isTRUE(preview$resolve_legacy) &&
    any(lengths(preview$legacy) > 0L)
  if (!any(preview$changed) && !resolving) return(record)
  overlay <- metadata_overlay_upgrade(
    record$metadata_bundle$overlay,
    context$catalog
  )
  tables <- metadata_overlay_patch_tables(overlay, preview)
  if (resolving) {
    overlay$unresolved <- lapply(overlay$unresolved, function(legacy) {
      affected <- metadata_overlay_legacy_conflicts(
        list(unresolved = list(legacy)),
        preview$targets
      )
      legacy$resolved_targets <- unique(c(
        legacy$resolved_targets,
        preview$targets$target_id[lengths(affected) > 0L]
      ))
      legacy
    })
  }
  updated <- new_metadata_overlay(
    overlay$source_fingerprint,
    tables,
    overlay$revision + 1L,
    overlay$unknown,
    version = metadata_overlay_version(),
    unresolved = overlay$unresolved
  )
  metadata_overlay_replace(record, context, updated)
}

undo_metadata_overlay <- function(record, context) {
  metadata_overlay_assert_context(record, context)
  snapshot <- if (length(record$history) > 0L) record$history[[1L]] else NULL
  if (
    is.null(snapshot) ||
      !identical(attr(snapshot, "llw_change_type"), "metadata_overlay")
  ) {
    abort_llw(
      "There is no latest metadata edit to undo. Other changes are protected.",
      type = "validation"
    )
  }
  if (!is.null(record$draft)) {
    abort_llw(
      "Finish or discard the dataset preparation draft before undoing metadata.",
      type = "validation"
    )
  }
  previous <- metadata_overlay_upgrade(
    snapshot$metadata_bundle$overlay,
    context$catalog
  )
  previous$revision <- record$metadata_bundle$overlay$revision + 1L
  record$history <- record$history[-1L]
  # The protected history already holds this exact committed index. Reuse it
  # when restoring a removed assertion, rather than rebuilding every table just
  # to reinsert one row. Only the monotonic overlay revision label changes.
  metadata_overlay_replace(
    record,
    context,
    previous,
    history = FALSE,
    index_base = snapshot$metadata_bundle
  )
}

metadata_overlay_preview_table <- function(preview) {
  text <- function(values) vapply(values, metadata_scalar_text, character(1))
  dplyr::tibble(
    Item = preview$targets$entity_id,
    Field = preview$targets$field_label,
    Original = text(preview$targets$source_value),
    Current = text(preview$before),
    Proposed = text(preview$proposed),
    Change = ifelse(preview$changed, "Will change", "Unchanged"),
    Source = preview$targets$source_label,
    Older_unqualified_value = vapply(
      preview$legacy,
      paste,
      character(1),
      collapse = "; "
    ),
    Source_scope = preview$targets$source_scope,
    Target_id = preview$targets$target_id
  )
}
