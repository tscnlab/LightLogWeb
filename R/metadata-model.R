metadata_contract_version <- function() {
  "1.0.0"
}

metadata_index_version <- function() {
  "1.3.1"
}

metadata_overlay_namespace_id <- function() {
  "llw:overlay"
}

metadata_empty_resources <- function() {
  data.frame(
    resource = character(),
    path = character(),
    core = logical(),
    format = character(),
    media_type = character(),
    stringsAsFactors = FALSE
  )
}

metadata_overlay_table_contract <- function() {
  list(
    datasets = c("dataset_id", "title"),
    participants = c("participant_id", "label"),
    devices = c("device_id", "manufacturer", "model"),
    sites = c(
      "dataset_id",
      "site",
      "country",
      "source_timezone",
      "latitude",
      "longitude"
    ),
    variables = c(
      "dataset_id",
      "file_group_id",
      "name",
      "label",
      "unit"
    )
  )
}

metadata_overlay_type_contract <- function() {
  list(
    datasets = c(dataset_id = "character", title = "character"),
    participants = c(participant_id = "character", label = "character"),
    devices = c(
      device_id = "character",
      manufacturer = "character",
      model = "character"
    ),
    sites = c(
      dataset_id = "character",
      site = "character",
      country = "character",
      source_timezone = "character",
      latitude = "numeric",
      longitude = "numeric"
    ),
    variables = c(
      dataset_id = "character",
      file_group_id = "character",
      name = "character",
      label = "character",
      unit = "character"
    )
  )
}

metadata_overlay_key_contract <- function() {
  list(
    datasets = "dataset_id",
    participants = "participant_id",
    devices = "device_id",
    sites = "dataset_id",
    variables = c("dataset_id", "file_group_id", "name")
  )
}

metadata_empty_overlay_table <- function(columns) {
  values <- stats::setNames(
    rep(list(character()), length(columns)),
    columns
  )
  values$latitude <- if ("latitude" %in% columns) numeric() else NULL
  values$longitude <- if ("longitude" %in% columns) numeric() else NULL
  data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

metadata_empty_overlay_tables <- function() {
  lapply(metadata_overlay_table_contract(), metadata_empty_overlay_table)
}

metadata_validate_overlay_table <- function(table, name) {
  columns <- metadata_overlay_table_contract()[[name]]
  types <- metadata_overlay_type_contract()[[name]]
  keys <- metadata_overlay_key_contract()[[name]]
  if (!is.data.frame(table) || !identical(names(table), columns)) {
    abort_llw(
      paste0("Metadata overlay table `", name, "` is invalid."),
      type = "validation"
    )
  }
  table <- as.data.frame(table, stringsAsFactors = FALSE, check.names = FALSE)
  for (column in columns) {
    expected <- types[[column]]
    value <- table[[column]]
    valid <- if (identical(expected, "character")) {
      is.character(value)
    } else {
      is.numeric(value) && !inherits(value, c("Date", "POSIXt", "difftime"))
    }
    if (!valid) {
      abort_llw(
        paste0(
          "Metadata overlay column `",
          name,
          "$",
          column,
          "` must be ",
          expected,
          "."
        ),
        type = "validation"
      )
    }
    if (identical(expected, "character")) {
      table[[column]] <- trimws(enc2utf8(value))
    } else {
      table[[column]] <- as.numeric(value)
    }
  }
  if (nrow(table) == 0L) return(table)
  for (key in keys) {
    value <- table[[key]]
    if (anyNA(value) || any(!nzchar(value))) {
      abort_llw(
        paste0("Metadata overlay key `", name, "$", key, "` is required."),
        type = "validation"
      )
    }
  }
  key_values <- do.call(paste, c(table[keys], sep = "\r"))
  if (anyDuplicated(key_values)) {
    abort_llw(
      paste0("Metadata overlay table `", name, "` has duplicate identities."),
      type = "validation"
    )
  }
  if (identical(name, "sites")) {
    latitude <- table$latitude
    longitude <- table$longitude
    if (
      any(is.infinite(latitude), na.rm = TRUE) ||
        any(is.infinite(longitude), na.rm = TRUE) ||
        any(latitude < -90 | latitude > 90, na.rm = TRUE) ||
        any(longitude < -180 | longitude > 180, na.rm = TRUE) ||
        any(xor(is.na(latitude), is.na(longitude)))
    ) {
      abort_llw(
        "Overlay coordinates must be finite, paired latitude/longitude values.",
        type = "validation"
      )
    }
    timezones <- table$source_timezone
    timezones <- timezones[!is.na(timezones) & nzchar(timezones)]
    if (any(!timezones %in% OlsonNames())) {
      abort_llw(
        "Overlay source timezones must use known IANA timezone names.",
        type = "validation"
      )
    }
  }
  table
}

metadata_fingerprint <- function(value) {
  paste0(
    "sha256:",
    digest::digest(value, algo = "sha256", serialize = TRUE)
  )
}

metadata_nonempty_text <- function(value) {
  if (is.null(value) || length(value) == 0L) return(character())
  value <- trimws(enc2utf8(as.character(value)))
  value[!is.na(value) & nzchar(value)]
}

metadata_value_has_content <- function(value) {
  if (is.null(value) || length(value) == 0L) return(FALSE)
  if (is.data.frame(value)) return(nrow(value) > 0L)
  if (is.list(value)) {
    return(any(vapply(value, metadata_value_has_content, logical(1))))
  }
  if (is.character(value) || is.factor(value)) {
    return(length(metadata_nonempty_text(value)) > 0L)
  }
  any(!is.na(value))
}

metadata_source_state <- function(records, state = NULL) {
  if (!is.null(state)) {
    return(match.arg(state, c("complete", "partial", "absent")))
  }
  if (!metadata_value_has_content(records)) "absent" else "partial"
}

new_metadata_source_namespace <- function(
  namespace_id,
  label,
  source_type,
  records = list(),
  resources = metadata_empty_resources(),
  source_dataset_id = namespace_id,
  state = NULL,
  source_schema_version = NA_character_,
  provenance = list()
) {
  assert_scalar_string(namespace_id, "namespace_id")
  assert_scalar_string(label, "label")
  assert_scalar_string(source_type, "source_type")
  assert_scalar_string(source_dataset_id, "source_dataset_id")
  if (!is.list(records) || (length(records) > 0L && is.null(names(records)))) {
    abort_llw(
      "Source metadata records must be a named list.",
      type = "validation"
    )
  }
  if (!is.data.frame(resources)) {
    abort_llw(
      "Source metadata resources must be a data frame.",
      type = "validation"
    )
  }
  required_resources <- names(metadata_empty_resources())
  for (column in setdiff(required_resources, names(resources))) {
    resources[[column]] <- metadata_empty_resources()[[column]]
  }
  resources <- resources[, required_resources, drop = FALSE]
  resources$resource <- as.character(resources$resource)
  resources$path <- as.character(resources$path)
  resources$core <- as.logical(resources$core)
  resources$format <- as.character(resources$format)
  resources$media_type <- as.character(resources$media_type)
  if (anyNA(resources$resource) || any(!nzchar(resources$resource))) {
    abort_llw(
      "Source metadata resource identities must be non-empty strings.",
      type = "validation"
    )
  }
  if (anyDuplicated(resources$resource)) {
    abort_llw(
      "Source metadata resource identities must be unique.",
      type = "validation"
    )
  }
  if (!is.list(provenance)) {
    abort_llw("Source metadata provenance must be a list.", type = "validation")
  }
  if (
    !is.character(source_schema_version) ||
      length(source_schema_version) != 1L
  ) {
    abort_llw(
      "Source metadata schema version must be one string or `NA`.",
      type = "validation"
    )
  }
  state <- metadata_source_state(records, state)
  source <- structure(
    list(
      source_schema = "llw-source-metadata",
      source_version = metadata_contract_version(),
      namespace_id = namespace_id,
      source_dataset_id = source_dataset_id,
      label = label,
      source_type = source_type,
      state = state,
      source_schema_version = source_schema_version,
      resources = resources,
      records = records,
      provenance = provenance
    ),
    class = c("llw_source_metadata", "list")
  )
  source$fingerprint <- metadata_fingerprint(source)
  assert_serializable_value(source, "source metadata namespace")
  source
}

validate_metadata_source_namespace <- function(source) {
  if (!inherits(source, "llw_source_metadata") || !is.list(source)) {
    abort_llw(
      "Source metadata must be created by `new_metadata_source_namespace()`.",
      type = "validation"
    )
  }
  required <- c(
    "source_schema",
    "source_version",
    "namespace_id",
    "source_dataset_id",
    "label",
    "source_type",
    "state",
    "source_schema_version",
    "resources",
    "records",
    "provenance",
    "fingerprint"
  )
  if (!all(required %in% names(source))) {
    abort_llw("Source metadata fields are incomplete.", type = "validation")
  }
  expected_fingerprint <- source$fingerprint
  source$fingerprint <- NULL
  rebuilt <- new_metadata_source_namespace(
    namespace_id = source$namespace_id,
    label = source$label,
    source_type = source$source_type,
    records = source$records,
    resources = source$resources,
    source_dataset_id = source$source_dataset_id,
    state = source$state,
    source_schema_version = source$source_schema_version,
    provenance = source$provenance
  )
  if (
    !identical(source$source_schema, "llw-source-metadata") ||
      !identical(source$source_version, metadata_contract_version()) ||
      !identical(expected_fingerprint, rebuilt$fingerprint)
  ) {
    abort_llw(
      "Immutable source metadata failed its fingerprint check.",
      type = "validation",
      public_message = paste(
        "The retained source metadata failed an integrity check.",
        "No source or overlay metadata were changed."
      )
    )
  }
  rebuilt
}

new_metadata_overlay <- function(
  source_fingerprint,
  tables = metadata_empty_overlay_tables(),
  revision = 0L,
  unknown = list()
) {
  assert_scalar_string(source_fingerprint, "source_fingerprint")
  if (!grepl("^sha256:[0-9a-f]{64}$", source_fingerprint)) {
    abort_llw(
      "Overlay source fingerprints must use the `sha256:<64 hex>` form.",
      type = "validation"
    )
  }
  if (
    !is.numeric(revision) ||
      length(revision) != 1L ||
      is.na(revision) ||
      revision < 0 ||
      revision != floor(revision)
  ) {
    abort_llw(
      "Metadata overlay revision must be one non-negative whole number.",
      type = "validation"
    )
  }
  contract <- metadata_overlay_table_contract()
  if (!is.list(tables) || !identical(names(tables), names(contract))) {
    abort_llw(
      "Metadata overlay tables do not match the versioned contract.",
      type = "validation"
    )
  }
  tables <- lapply(names(contract), function(name) {
    metadata_validate_overlay_table(tables[[name]], name)
  })
  names(tables) <- names(contract)
  if (!is.list(unknown)) {
    abort_llw("Overlay unknown fields must be a list.", type = "validation")
  }
  overlay <- structure(
    list(
      overlay_schema = "llw-metadata-overlay",
      overlay_version = metadata_contract_version(),
      revision = as.integer(revision),
      source_fingerprint = source_fingerprint,
      tables = tables,
      unknown = unknown
    ),
    class = c("llw_metadata_overlay", "list")
  )
  assert_serializable_value(overlay, "metadata overlay")
  overlay
}

validate_metadata_overlay <- function(overlay, source_fingerprint) {
  if (!inherits(overlay, "llw_metadata_overlay") || !is.list(overlay)) {
    abort_llw(
      "Metadata overlays must be created by `new_metadata_overlay()`.",
      type = "validation"
    )
  }
  if (
    !identical(overlay$overlay_schema, "llw-metadata-overlay") ||
      !identical(overlay$overlay_version, metadata_contract_version()) ||
      !identical(overlay$source_fingerprint, source_fingerprint)
  ) {
    abort_llw(
      "The LightLogWeb overlay does not match its immutable source metadata.",
      type = "validation"
    )
  }
  new_metadata_overlay(
    source_fingerprint = overlay$source_fingerprint,
    tables = overlay$tables,
    revision = overlay$revision,
    unknown = overlay$unknown
  )
}

metadata_scalar_text <- function(value) {
  if (length(value) == 0L || is.null(value)) return(NA_character_)
  if (inherits(value, "POSIXct")) {
    return(format(value[[1L]], tz = lubridate::tz(value), usetz = TRUE))
  }
  if (inherits(value, "Date")) return(format(value[[1L]], "%Y-%m-%d"))
  if (inherits(value, "difftime")) {
    return(paste(as.character(value[[1L]]), attr(value, "units")))
  }
  if (is.factor(value)) value <- as.character(value)
  if (is.logical(value)) {
    if (is.na(value[[1L]])) return(NA_character_)
    return(if (isTRUE(value[[1L]])) "TRUE" else "FALSE")
  }
  if (is.numeric(value)) {
    if (is.na(value[[1L]])) return(NA_character_)
    return(format(
      value[[1L]],
      digits = 15L,
      scientific = FALSE,
      trim = TRUE
    ))
  }
  if (is.raw(value)) return(paste0("raw:", paste(value, collapse = "")))
  text <- enc2utf8(as.character(value[[1L]]))
  if (is.na(text) || !nzchar(text)) NA_character_ else text
}

metadata_value_type <- function(value) {
  if (inherits(value, "POSIXct")) return("datetime")
  if (inherits(value, "Date")) return("date")
  if (is.factor(value)) return("factor")
  if (is.logical(value)) return("logical")
  if (is.integer(value)) return("integer")
  if (is.numeric(value)) return("number")
  if (is.raw(value)) return("raw")
  if (is.character(value)) return("text")
  class(value)[[1L]] %||% typeof(value)
}

metadata_data_frame_cell <- function(data, column, row) {
  value <- data[[column]]
  if (is.data.frame(value)) return(value[row, , drop = FALSE])
  if (is.matrix(value) || length(dim(value)) > 1L) {
    return(value[row, , drop = FALSE])
  }
  if (is.list(value)) return(value[[row]])
  value[row]
}

metadata_record_identity <- function(data, row) {
  internal_ids <- grep(
    "(^|_)internal_id$",
    names(data),
    ignore.case = TRUE,
    value = TRUE
  )
  candidates <- unique(c(
    intersect(c("file_group_id", "dataset_file_group_id"), names(data)),
    internal_ids,
    intersect(
      c(
        "dataset_id",
        "participant_id",
        "device_id",
        "study_id",
        "device_datasheet_id",
        "datasheet_id",
        "id"
      ),
      names(data)
    )
  ))
  candidates <- candidates[!grepl(
    "crossref|reference",
    candidates,
    ignore.case = TRUE
  )]
  for (column in unique(candidates)) {
    cell <- metadata_data_frame_cell(data, column, row)
    if (is.data.frame(cell) || is.list(cell) || length(cell) != 1L) next
    value <- metadata_scalar_text(cell)
    if (!is.na(value) && nzchar(value)) return(value)
  }
  ""
}

metadata_file_group_lookup <- function(records) {
  collect_pairs <- function(value) {
    empty <- data.frame(
      dataset_id = character(),
      file_group_id = character(),
      stringsAsFactors = FALSE
    )
    if (inherits(value, "llw_metadata_index")) return(empty)
    if (inherits(value, "llw_metadata_bundle")) {
      rows <- lapply(value$sources, function(source) {
        collect_pairs(source$records)
      })
    } else if (inherits(value, "llw_source_metadata")) {
      rows <- list(collect_pairs(value$records))
    } else if (is.list(value)) {
      rows <- list()
      inventory <- value$variable_inventory %||% NULL
      if (
        is.data.frame(inventory) &&
          all(c("dataset_id", "file_group_id") %in% names(inventory))
      ) {
        rows[[length(rows) + 1L]] <- data.frame(
          dataset_id = as.character(inventory$dataset_id),
          file_group_id = as.character(inventory$file_group_id),
          stringsAsFactors = FALSE
        )
      }
      nested <- value$metadata_bundle %||% NULL
      if (inherits(nested, "llw_metadata_bundle")) {
        rows[[length(rows) + 1L]] <- collect_pairs(nested)
      }
    } else {
      return(empty)
    }
    rows <- rows[vapply(rows, nrow, integer(1)) > 0L]
    if (length(rows) == 0L) return(empty)
    do.call(rbind, rows)
  }

  pairs <- unique(collect_pairs(records))
  keep <- !is.na(pairs$dataset_id) & nzchar(pairs$dataset_id) &
    !is.na(pairs$file_group_id) & nzchar(pairs$file_group_id)
  pairs <- pairs[keep, , drop = FALSE]
  if (nrow(pairs) == 0L) return(list())
  split(
    pairs$file_group_id,
    factor(pairs$dataset_id, levels = unique(pairs$dataset_id)),
    drop = TRUE
  )
}

metadata_file_group_record <- function(
  file_groups,
  inherited_record,
  data,
  row,
  field
) {
  if (!identical(field, "dataset_file") || !nzchar(inherited_record)) {
    return("")
  }
  candidates <- file_groups[[inherited_record]] %||% character()
  if (length(candidates) == 0L) return("")
  row_key <- row.names(data)[[row]]
  row_candidate <- paste0(inherited_record, ":", row_key)
  if (row_candidate %in% candidates) return(row_candidate)
  if (length(candidates) == nrow(data)) return(candidates[[row]])
  ""
}

metadata_record_label <- function(
  data,
  row,
  inherited_record,
  field,
  file_groups
) {
  file_group <- metadata_file_group_record(
    file_groups,
    inherited_record,
    data,
    row,
    field
  )
  if (nzchar(file_group)) return(file_group)
  identity <- metadata_record_identity(data, row)
  if (nzchar(identity)) return(identity)
  if (!is.na(inherited_record) && nzchar(inherited_record)) {
    return(inherited_record)
  }
  paste("Record", format(row, big.mark = ","))
}

metadata_row_group_identity <- function(data, row) {
  if (!is.data.frame(data) || nrow(data) < row) {
    return(list(key = "", value = ""))
  }
  candidates <- c(
    grep(
      "(_variables|_channel|_characteristic)_name$",
      names(data),
      ignore.case = TRUE,
      value = TRUE
    ),
    intersect(c("parameter_name", "device_sensor_type"), names(data)),
    if (all(c("term", "label") %in% names(data))) "term" else character(),
    if (
      "name" %in% names(data) &&
        any(c("dataset_id", "file_group_id") %in% names(data))
    ) {
      "name"
    } else {
      character()
    },
    if (all(c("value", "label") %in% names(data))) "value" else character()
  )
  for (column in unique(candidates)) {
    cell <- metadata_data_frame_cell(data, column, row)
    if (is.data.frame(cell) || is.list(cell) || length(cell) != 1L) next
    value <- metadata_scalar_text(cell)
    if (!is.na(value) && nzchar(value)) {
      return(list(key = column, value = value))
    }
  }
  list(key = "", value = "")
}

metadata_group_identity_append <- function(existing, value) {
  if (is.na(value) || !nzchar(value)) return(existing)
  if (is.na(existing) || !nzchar(existing)) return(value)
  paste(existing, value, sep = " / ")
}

metadata_group_id <- function(
  namespace_id,
  layer,
  resource,
  group_path,
  field,
  group_discriminator = ""
) {
  paste0(
    "metadata_group_",
    digest::digest(
      paste(
        namespace_id,
        layer,
        resource,
        group_path,
        field,
        group_discriminator,
        sep = "\r"
      ),
      algo = "sha256",
      serialize = FALSE
    )
  )
}

metadata_search_text <- function(field, value) {
  assert_scalar_string(field, "field")
  value <- if (is.na(value)) "" else value
  friendly_field <- gsub("[_-]+", " ", field)
  tolower(enc2utf8(paste(field, friendly_field, value)))
}

metadata_index_buffers <- function() {
  state <- new.env(parent = emptyenv())
  state$entry_rows <- list()
  state$node_rows <- list()
  state$entry_count <- 0L
  state$node_count <- 0L

  state$add_node <- function(
    parent_node_id,
    namespace_id,
    source_dataset_id,
    source_label,
    state_value,
    layer,
    resource,
    label,
    kind,
    path,
    record_identity = ""
  ) {
    state$node_count <- state$node_count + 1L
    node_id <- paste0("metadata_node_", sprintf("%08d", state$node_count))
    state$node_rows[[state$node_count]] <- list(
      node_id = node_id,
      parent_node_id = parent_node_id,
      namespace_id = namespace_id,
      source_dataset_id = source_dataset_id,
      source_label = source_label,
      state = state_value,
      layer = layer,
      resource = resource,
      label = label,
      kind = kind,
      path = path,
      record_identity = record_identity
    )
    node_id
  }

  state$add_entry <- function(
    node_id,
    parent_node_id,
    namespace_id,
    source_dataset_id,
    source_label,
    state_value,
    layer,
    resource,
    record,
    path,
    group_path,
    group_discriminator,
    group_context,
    field,
    value,
    value_type
  ) {
    state$entry_count <- state$entry_count + 1L
    state$entry_rows[[state$entry_count]] <- list(
      entry_id = paste0(
        "metadata_entry_",
        sprintf("%09d", state$entry_count)
      ),
      node_id = node_id,
      parent_node_id = parent_node_id,
      namespace_id = namespace_id,
      source_dataset_id = source_dataset_id,
      source_label = source_label,
      state = state_value,
      layer = layer,
      resource = resource,
      record = record,
      path = path,
      group_path = group_path,
      group_discriminator = group_discriminator,
      group_context = group_context,
      group_id = metadata_group_id(
        namespace_id,
        layer,
        resource,
        group_path,
        field,
        group_discriminator
      ),
      field = field,
      value = value,
      value_type = value_type,
      search_text = metadata_search_text(field, value)
    )
    invisible(NULL)
  }
  state
}

metadata_rows_to_data_frame <- function(rows, columns) {
  if (length(rows) == 0L) {
    values <- stats::setNames(rep(list(character()), length(columns)), columns)
    return(data.frame(values, stringsAsFactors = FALSE, check.names = FALSE))
  }
  values <- lapply(columns, function(column) {
    vapply(rows, `[[`, character(1), column)
  })
  names(values) <- columns
  data.frame(values, stringsAsFactors = FALSE, check.names = FALSE)
}

metadata_index_sort_rank <- function(key, entry_id) {
  if (length(key) == 0L) return(integer())
  key[is.na(key)] <- ""
  ordering <- order(
    tolower(enc2utf8(key)),
    entry_id,
    method = "radix"
  )
  rank <- integer(length(ordering))
  rank[ordering] <- seq_along(ordering)
  rank
}

metadata_index_add_sort_ranks <- function(
  entries,
  identity = entries$entry_id
) {
  entries$sort_rank_path <- metadata_index_sort_rank(
    entries$path,
    identity
  )
  entries$sort_rank_field <- metadata_index_sort_rank(
    entries$field,
    identity
  )
  entries$sort_rank_value <- metadata_index_sort_rank(
    entries$value,
    identity
  )
  entries$sort_rank_source <- metadata_index_sort_rank(
    entries$source_label,
    identity
  )
  entries
}

metadata_group_value_preview <- function(values, max_values = 3L) {
  values[is.na(values) | !nzchar(values)] <- "not specified"
  values <- unique(values)
  shown <- utils::head(values, max_values)
  shown <- vapply(
    shown,
    function(value) {
      if (nchar(value, type = "width") <= 48L) return(value)
      paste0(substr(value, 1L, 47L), "...")
    },
    character(1)
  )
  preview <- paste(shown, collapse = "; ")
  remaining <- length(values) - length(shown)
  if (remaining > 0L) paste0(preview, "; +", remaining, " more") else preview
}

metadata_index_groups <- function(entries) {
  if (nrow(entries) == 0L) {
    groups <- data.frame(
      group_id = character(),
      namespace_id = character(),
      source_dataset_id = character(),
      source_label = character(),
      state = character(),
      layer = character(),
      resource = character(),
      group_path = character(),
      group_discriminator = character(),
      group_context = character(),
      field = character(),
      value_preview = character(),
      value_type = character(),
      entry_count = integer(),
      record_count = integer(),
      distinct_value_count = integer(),
      stringsAsFactors = FALSE
    )
    rank_input <- groups
    rank_input$path <- rank_input$group_path
    rank_input$value <- rank_input$value_preview
    ranked <- metadata_index_add_sort_ranks(
      rank_input,
      identity = groups$group_id
    )
    return(ranked[,
      c(
        names(groups),
        "sort_rank_path",
        "sort_rank_field",
        "sort_rank_value",
        "sort_rank_source"
      ),
      drop = FALSE
    ])
  }

  group_ids <- unique(entries$group_id)
  positions <- split(
    seq_len(nrow(entries)),
    factor(entries$group_id, levels = group_ids),
    drop = TRUE
  )
  rows <- lapply(positions, function(position) {
    first <- entries[position[[1L]], , drop = FALSE]
    values <- entries$value[position]
    normalized_values <- values
    normalized_values[is.na(normalized_values) | !nzchar(normalized_values)] <-
      "not specified"
    records <- entries$parent_node_id[position]
    records <- unique(records[!is.na(records) & nzchar(records)])
    if (length(records) == 0L) {
      records <- entries$record[position]
      records <- unique(records[!is.na(records) & nzchar(records)])
    }
    value_types <- unique(entries$value_type[position])
    data.frame(
      group_id = first$group_id,
      namespace_id = first$namespace_id,
      source_dataset_id = first$source_dataset_id,
      source_label = first$source_label,
      state = first$state,
      layer = first$layer,
      resource = first$resource,
      group_path = first$group_path,
      group_discriminator = first$group_discriminator,
      group_context = first$group_context,
      field = first$field,
      value_preview = metadata_group_value_preview(values),
      value_type = if (length(value_types) == 1L) value_types else "mixed",
      entry_count = as.integer(length(position)),
      record_count = as.integer(length(records)),
      distinct_value_count = as.integer(length(unique(normalized_values))),
      stringsAsFactors = FALSE
    )
  })
  groups <- do.call(rbind, rows)
  rownames(groups) <- NULL
  rank_input <- groups
  rank_input$path <- rank_input$group_path
  rank_input$value <- rank_input$value_preview
  ranked <- metadata_index_add_sort_ranks(
    rank_input,
    identity = groups$group_id
  )
  ranked[,
    c(
      names(groups),
      "sort_rank_path",
      "sort_rank_field",
      "sort_rank_value",
      "sort_rank_source"
    ),
    drop = FALSE
  ]
}

metadata_index_source <- function(source, buffers, layer = "Immutable source") {
  source <- validate_metadata_source_namespace(source)
  file_groups <- metadata_file_group_lookup(source$records)
  namespace_node <- buffers$add_node(
    parent_node_id = "",
    namespace_id = source$namespace_id,
    source_dataset_id = source$source_dataset_id,
    source_label = source$label,
    state_value = source$state,
    layer = layer,
    resource = "",
    label = source$label,
    kind = "source",
    path = source$namespace_id
  )

  walk <- function(
    value,
    parent_node_id,
    resource,
    record,
    path,
    group_path,
    group_discriminator,
    group_context,
    field
  ) {
    if (inherits(value, "llw_metadata_bundle")) {
      bundle_node <- buffers$add_node(
        parent_node_id,
        source$namespace_id,
        source$source_dataset_id,
        source$label,
        source$state,
        layer,
        resource,
        field,
        "group",
        path
      )
      walk(
        value$sources,
        bundle_node,
        resource,
        record,
        paste0(path, "/sources"),
        paste0(group_path, "/sources"),
        group_discriminator,
        group_context,
        "sources"
      )
      walk(
        value$overlay,
        bundle_node,
        resource,
        record,
        paste0(path, "/overlay"),
        paste0(group_path, "/overlay"),
        group_discriminator,
        group_context,
        "overlay"
      )
      return(invisible(NULL))
    }
    if (inherits(value, "llw_metadata_index")) {
      return(invisible(NULL))
    }
    if (is.data.frame(value)) {
      table_node <- buffers$add_node(
        parent_node_id,
        source$namespace_id,
        source$source_dataset_id,
        source$label,
        source$state,
        layer,
        resource,
        field,
        "table",
        path
      )
      if (nrow(value) == 0L) return(invisible(NULL))
      for (row in seq_len(nrow(value))) {
        record_label <- metadata_record_label(
          value,
          row,
          record,
          field,
          file_groups
        )
        row_identity <- metadata_row_group_identity(value, row)
        row_node_label <- if (nzchar(row_identity$value)) {
          row_identity$value
        } else {
          record_label
        }
        row_discriminator <- metadata_group_identity_append(
          group_discriminator,
          if (nzchar(row_identity$key)) {
            paste0(row_identity$key, "=", row_identity$value)
          } else {
            ""
          }
        )
        row_context <- metadata_group_identity_append(
          group_context,
          row_identity$value
        )
        row_path <- paste0(path, "[", row, "]")
        row_node <- buffers$add_node(
          table_node,
          source$namespace_id,
          source$source_dataset_id,
          source$label,
          source$state,
          layer,
          resource,
          row_node_label,
          "record",
          row_path,
          record_identity = record_label
        )
        for (column in names(value)) {
          cell <- metadata_data_frame_cell(value, column, row)
          walk(
            cell,
            row_node,
            resource,
            record_label,
            paste0(row_path, "/", column),
            paste0(group_path, "/", column),
            row_discriminator,
            row_context,
            column
          )
        }
      }
      return(invisible(NULL))
    }
    if (is.list(value) && !inherits(value, c("POSIXct", "Date"))) {
      list_node <- buffers$add_node(
        parent_node_id,
        source$namespace_id,
        source$source_dataset_id,
        source$label,
        source$state,
        layer,
        resource,
        field,
        "group",
        path
      )
      if (length(value) == 0L) return(invisible(NULL))
      labels <- names(value)
      if (is.null(labels)) labels <- paste0("Item ", seq_along(value))
      labels[is.na(labels) | !nzchar(labels)] <- paste0(
        "Item ",
        which(is.na(labels) | !nzchar(labels))
      )
      for (index in seq_along(value)) {
        child_field <- labels[[index]]
        walk(
          value[[index]],
          list_node,
          resource,
          record,
          paste0(path, "/", child_field),
          paste0(group_path, "/", child_field),
          group_discriminator,
          group_context,
          child_field
        )
      }
      return(invisible(NULL))
    }
    if (length(value) == 0L) return(invisible(NULL))
    for (index in seq_along(value)) {
      scalar <- value[index]
      scalar_path <- if (length(value) > 1L) {
        paste0(path, "[", index, "]")
      } else {
        path
      }
      buffers$add_entry(
        node_id = parent_node_id,
        parent_node_id = parent_node_id,
        namespace_id = source$namespace_id,
        source_dataset_id = source$source_dataset_id,
        source_label = source$label,
        state_value = source$state,
        layer = layer,
        resource = resource,
        record = record,
        path = scalar_path,
        group_path = group_path,
        group_discriminator = group_discriminator,
        group_context = group_context,
        field = field,
        value = metadata_scalar_text(scalar),
        value_type = metadata_value_type(scalar)
      )
    }
    invisible(NULL)
  }

  resource_names <- names(source$records)
  for (index in seq_along(source$records)) {
    resource <- resource_names[[index]]
    resource_node <- buffers$add_node(
      namespace_node,
      source$namespace_id,
      source$source_dataset_id,
      source$label,
      source$state,
      layer,
      resource,
      resource,
      "resource",
      paste0(source$namespace_id, "/", resource)
    )
    walk(
      source$records[[index]],
      resource_node,
      resource,
      resource,
      paste0(source$namespace_id, "/", resource),
      paste0(source$namespace_id, "/", resource),
      "",
      "",
      resource
    )
  }
  invisible(NULL)
}

metadata_index_overlay <- function(overlay, buffers) {
  records <- overlay$tables
  if (metadata_value_has_content(overlay$unknown)) {
    records$unknown_fields <- overlay$unknown
  }
  keep <- vapply(records, metadata_value_has_content, logical(1))
  records <- records[keep]
  if (length(records) == 0L) return(invisible(NULL))
  resource_count <- length(records)
  resources <- data.frame(
    resource = names(records),
    path = paste0("llw-overlay/", names(records)),
    core = rep(FALSE, resource_count),
    format = rep("LightLogWeb typed table", resource_count),
    media_type = rep("application/x-lightlogweb-overlay", resource_count),
    stringsAsFactors = FALSE
  )
  source <- new_metadata_source_namespace(
    namespace_id = metadata_overlay_namespace_id(),
    source_dataset_id = metadata_overlay_namespace_id(),
    label = paste("LightLogWeb overlay revision", overlay$revision),
    source_type = "lightlogweb_overlay",
    records = records,
    resources = resources,
    state = "complete",
    source_schema_version = overlay$overlay_version,
    provenance = list(
      overlay_version = overlay$overlay_version,
      overlay_revision = overlay$revision,
      bound_source_fingerprint = overlay$source_fingerprint
    )
  )
  metadata_index_source(source, buffers, layer = "LightLogWeb overlay")
}

new_metadata_index <- function(bundle) {
  if (!is.list(bundle) || !is.list(bundle$sources)) {
    abort_llw(
      "A metadata index requires source namespaces.",
      type = "validation"
    )
  }
  buffers <- metadata_index_buffers()
  for (source in bundle$sources) {
    metadata_index_source(source, buffers)
  }
  metadata_index_overlay(bundle$overlay, buffers)
  entry_columns <- c(
    "entry_id",
    "node_id",
    "parent_node_id",
    "namespace_id",
    "source_dataset_id",
    "source_label",
    "state",
    "layer",
    "resource",
    "record",
    "path",
    "group_path",
    "group_discriminator",
    "group_context",
    "group_id",
    "field",
    "value",
    "value_type",
    "search_text"
  )
  node_columns <- c(
    "node_id",
    "parent_node_id",
    "namespace_id",
    "source_dataset_id",
    "source_label",
    "state",
    "layer",
    "resource",
    "label",
    "kind",
    "path",
    "record_identity"
  )
  entries <- metadata_rows_to_data_frame(
    buffers$entry_rows,
    entry_columns
  )
  entries <- metadata_index_add_sort_ranks(entries)
  groups <- metadata_index_groups(entries)
  nodes <- metadata_rows_to_data_frame(buffers$node_rows, node_columns)
  nodes$direct_value_count <- metadata_index_node_counts(
    entries$node_id,
    nodes$node_id
  )
  nodes$child_node_count <- metadata_index_node_counts(
    nodes$parent_node_id,
    nodes$node_id
  )
  nodes$parent_row <- match(
    nodes$parent_node_id,
    nodes$node_id,
    nomatch = 0L
  )
  index <- structure(
    list(
      index_schema = "llw-metadata-index",
      index_version = metadata_index_version(),
      source_fingerprint = bundle$source_fingerprint,
      nodes = nodes,
      entries = entries,
      groups = groups
    ),
    class = c("llw_metadata_index", "list")
  )
  assert_serializable_value(index, "metadata index")
  index
}

validate_metadata_index <- function(index, source_fingerprint) {
  if (!inherits(index, "llw_metadata_index") || !is.list(index)) {
    abort_llw("The in-memory metadata index is invalid.", type = "validation")
  }
  required <- c(
    "index_schema",
    "index_version",
    "source_fingerprint",
    "nodes",
    "entries",
    "groups"
  )
  if (
    !all(required %in% names(index)) ||
      !identical(index$index_schema, "llw-metadata-index") ||
      !identical(index$index_version, metadata_index_version()) ||
      !identical(index$source_fingerprint, source_fingerprint) ||
      !is.data.frame(index$nodes) ||
      !is.data.frame(index$entries) ||
      !is.data.frame(index$groups) ||
      !all(
        c(
          "node_id",
          "namespace_id",
          "resource",
          "path",
          "record_identity",
          "direct_value_count",
          "child_node_count",
          "parent_row"
        ) %in%
          names(index$nodes)
      ) ||
      !all(
        c(
          "entry_id",
          "group_id",
          "group_path",
          "group_discriminator",
          "group_context",
          "namespace_id",
          "resource",
          "field",
          "value",
          "search_text",
          "sort_rank_path",
          "sort_rank_field",
          "sort_rank_value",
          "sort_rank_source"
        ) %in%
          names(index$entries)
      ) ||
      !all(
        c(
          "group_id",
          "namespace_id",
          "resource",
          "group_path",
          "group_discriminator",
          "group_context",
          "field",
          "value_preview",
          "entry_count",
          "record_count",
          "sort_rank_path",
          "sort_rank_field",
          "sort_rank_value",
          "sort_rank_source"
        ) %in%
          names(index$groups)
      )
  ) {
    abort_llw(
      "The in-memory metadata index is incomplete.",
      type = "validation"
    )
  }
  index
}

new_metadata_bundle <- function(sources, overlay = NULL, index = NULL) {
  if (
    !is.list(sources) ||
      length(sources) == 0L ||
      is.null(names(sources)) ||
      anyNA(names(sources)) ||
      any(!nzchar(names(sources))) ||
      anyDuplicated(names(sources))
  ) {
    abort_llw(
      "Metadata bundles require uniquely named source namespaces.",
      type = "validation"
    )
  }
  if (metadata_overlay_namespace_id() %in% names(sources)) {
    abort_llw(
      "Source metadata cannot use the reserved LightLogWeb overlay namespace.",
      type = "validation"
    )
  }
  sources <- lapply(sources, validate_metadata_source_namespace)
  namespace_ids <- unname(vapply(
    sources,
    `[[`,
    character(1),
    "namespace_id"
  ))
  if (!identical(names(sources), namespace_ids)) {
    abort_llw(
      "Metadata source names must equal their stable namespace identities.",
      type = "validation"
    )
  }
  source_fingerprint <- metadata_fingerprint(lapply(
    sources,
    `[[`,
    "fingerprint"
  ))
  if (is.null(overlay)) {
    overlay <- new_metadata_overlay(source_fingerprint)
  } else {
    overlay <- validate_metadata_overlay(overlay, source_fingerprint)
  }
  bundle <- structure(
    list(
      bundle_schema = "llw-metadata-bundle",
      bundle_version = metadata_contract_version(),
      source_fingerprint = source_fingerprint,
      sources = sources,
      overlay = overlay,
      index = NULL
    ),
    class = c("llw_metadata_bundle", "list")
  )
  bundle$index <- if (
    is.null(index) ||
      !inherits(index, "llw_metadata_index") ||
      !identical(index$index_version %||% NULL, metadata_index_version())
  ) {
    new_metadata_index(bundle)
  } else {
    validate_metadata_index(index, source_fingerprint)
  }
  assert_serializable_value(bundle, "metadata bundle")
  bundle
}

validate_metadata_bundle <- function(bundle) {
  if (!inherits(bundle, "llw_metadata_bundle") || !is.list(bundle)) {
    abort_llw(
      "Dataset metadata must be a versioned LightLogWeb metadata bundle.",
      type = "validation"
    )
  }
  if (
    !identical(bundle$bundle_schema, "llw-metadata-bundle") ||
      !identical(bundle$bundle_version, metadata_contract_version())
  ) {
    abort_llw(
      "The dataset metadata bundle version is unsupported.",
      type = "validation"
    )
  }
  new_metadata_bundle(
    sources = bundle$sources,
    overlay = bundle$overlay,
    index = bundle$index
  )
}

new_metadata_bundle_from_factual <- function(
  factual_metadata,
  namespace_id,
  label,
  source_type,
  source_schema_version = NA_character_,
  provenance = list()
) {
  if (!is.list(factual_metadata)) {
    abort_llw("Factual metadata must be a list.", type = "validation")
  }
  state <- if (metadata_value_has_content(factual_metadata)) {
    "partial"
  } else {
    "absent"
  }
  source <- new_metadata_source_namespace(
    namespace_id = namespace_id,
    source_dataset_id = namespace_id,
    label = label,
    source_type = source_type,
    records = list(factual_metadata = factual_metadata),
    state = state,
    source_schema_version = source_schema_version,
    provenance = provenance
  )
  new_metadata_bundle(stats::setNames(list(source), namespace_id))
}

metadata_bundle_for_record <- function(record) {
  if (!is.list(record)) {
    abort_llw(
      "Metadata exploration requires a dataset record.",
      type = "validation"
    )
  }
  bundle <- record$metadata_bundle %||% NULL
  if (inherits(bundle, "llw_metadata_bundle")) return(bundle)
  new_metadata_bundle_from_factual(
    factual_metadata = record$factual_metadata %||% list(),
    namespace_id = record$id,
    label = record$display_name,
    source_type = record$source_manifest$source_type %||% "legacy_record",
    provenance = list(legacy_record = TRUE)
  )
}

metadata_index_query <- function(
  index,
  query = "",
  namespace_id = NULL,
  resource = NULL,
  layer = NULL,
  sort_by = c("path", "field", "value", "source"),
  descending = FALSE,
  limit = 200L
) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  sort_by <- match.arg(sort_by)
  assert_flag(descending, "descending")
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
  rows <- index$entries
  keep <- rep(TRUE, nrow(rows))
  query <- tolower(trimws(enc2utf8(as.character(query %||% ""))))
  if (length(query) != 1L || is.na(query)) query <- ""
  if (nzchar(query)) keep <- keep & grepl(query, rows$search_text, fixed = TRUE)
  if (!is.null(namespace_id) && !identical(namespace_id, "all")) {
    keep <- keep & rows$namespace_id %in% namespace_id
  }
  if (!is.null(resource) && !identical(resource, "all")) {
    keep <- keep & rows$resource %in% resource
  }
  if (!is.null(layer) && !identical(layer, "all")) {
    keep <- keep & rows$layer %in% layer
  }
  rows <- rows[keep, , drop = FALSE]
  rank_column <- switch(
    sort_by,
    path = "sort_rank_path",
    field = "sort_rank_field",
    value = "sort_rank_value",
    source = "sort_rank_source"
  )
  ordering <- order(rows[[rank_column]], method = "radix")
  if (isTRUE(descending)) ordering <- rev(ordering)
  rows <- rows[ordering, , drop = FALSE]
  total <- nrow(rows)
  rows <- utils::head(rows, as.integer(limit))
  rownames(rows) <- NULL
  structure(
    list(rows = rows, total = total, limited = total > nrow(rows)),
    class = c("llw_metadata_query", "list")
  )
}

metadata_index_query_groups <- function(
  index,
  query = "",
  namespace_id = NULL,
  resource = NULL,
  layer = NULL,
  sort_by = c("path", "field", "value", "source"),
  descending = FALSE,
  limit = 200L
) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  sort_by <- match.arg(sort_by)
  assert_flag(descending, "descending")
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

  groups <- index$groups
  keep <- rep(TRUE, nrow(groups))
  query <- tolower(trimws(enc2utf8(as.character(query %||% ""))))
  if (length(query) != 1L || is.na(query)) query <- ""
  if (nzchar(query)) {
    matching_entries <- grepl(
      query,
      index$entries$search_text,
      fixed = TRUE
    )
    matching_group_ids <- unique(index$entries$group_id[matching_entries])
    keep <- keep & groups$group_id %in% matching_group_ids
  }
  if (!is.null(namespace_id) && !identical(namespace_id, "all")) {
    keep <- keep & groups$namespace_id %in% namespace_id
  }
  if (!is.null(resource) && !identical(resource, "all")) {
    keep <- keep & groups$resource %in% resource
  }
  if (!is.null(layer) && !identical(layer, "all")) {
    keep <- keep & groups$layer %in% layer
  }
  groups <- groups[keep, , drop = FALSE]
  rank_column <- switch(
    sort_by,
    path = "sort_rank_path",
    field = "sort_rank_field",
    value = "sort_rank_value",
    source = "sort_rank_source"
  )
  ordering <- order(groups[[rank_column]], method = "radix")
  if (isTRUE(descending)) ordering <- rev(ordering)
  groups <- groups[ordering, , drop = FALSE]
  total <- nrow(groups)
  groups <- utils::head(groups, as.integer(limit))
  rownames(groups) <- NULL
  structure(
    list(rows = groups, total = total, limited = total > nrow(groups)),
    class = c("llw_metadata_group_query", "list")
  )
}

metadata_index_group <- function(index, group_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  assert_scalar_string(group_id, "group_id")
  row <- match(group_id, index$groups$group_id)
  if (is.na(row)) return(NULL)
  entry_rows <- which(index$entries$group_id == group_id)
  entries <- index$entries[entry_rows, , drop = FALSE]
  ordering <- order(entries$path, entries$entry_id, method = "radix")
  entries <- entries[ordering, , drop = FALSE]
  rownames(entries) <- NULL
  structure(
    list(
      group = index$groups[row, , drop = FALSE],
      entries = entries
    ),
    class = c("llw_metadata_group", "list")
  )
}

metadata_index_entry <- function(index, entry_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  assert_scalar_string(entry_id, "entry_id")
  row <- match(entry_id, index$entries$entry_id)
  if (is.na(row)) return(NULL)
  index$entries[row, , drop = FALSE]
}

metadata_index_node <- function(index, node_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  assert_scalar_string(node_id, "node_id")
  row <- match(node_id, index$nodes$node_id)
  if (is.na(row)) return(NULL)
  index$nodes[row, , drop = FALSE]
}

metadata_index_node_counts <- function(values, node_ids) {
  if (length(node_ids) == 0L) return(integer())
  counts <- table(values, useNA = "no")
  result <- as.integer(counts[node_ids])
  result[is.na(result)] <- 0L
  result
}

metadata_index_decorate_nodes <- function(index, nodes) {
  if (!is.data.frame(nodes)) {
    abort_llw("Metadata hierarchy nodes are invalid.", type = "validation")
  }
  if (all(c("direct_value_count", "child_node_count") %in% names(nodes))) {
    return(nodes)
  }
  nodes$direct_value_count <- metadata_index_node_counts(
    index$entries$node_id,
    nodes$node_id
  )
  nodes$child_node_count <- metadata_index_node_counts(
    index$nodes$parent_node_id,
    nodes$node_id
  )
  nodes
}

metadata_index_node_ancestors <- function(index, node_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  assert_scalar_string(node_id, "node_id")
  current <- match(node_id, index$nodes$node_id)
  if (is.na(current)) return(index$nodes[FALSE, , drop = FALSE])

  rows <- integer()
  seen <- character()
  repeat {
    current_id <- index$nodes$node_id[[current]]
    if (current_id %in% seen) {
      abort_llw(
        "The metadata hierarchy contains a parent cycle.",
        type = "validation"
      )
    }
    seen <- c(seen, current_id)
    rows <- c(rows, current)
    parent_row <- index$nodes$parent_row[[current]]
    if (is.na(parent_row) || parent_row == 0L) break
    if (
      parent_row < 1L ||
        parent_row > nrow(index$nodes) ||
        !identical(
          index$nodes$node_id[[parent_row]],
          index$nodes$parent_node_id[[current]]
        )
    ) {
      abort_llw(
        "The metadata hierarchy contains an unknown parent node.",
        type = "validation"
      )
    }
    current <- parent_row
  }
  result <- index$nodes[rev(rows), , drop = FALSE]
  rownames(result) <- NULL
  result
}

metadata_index_node_children <- function(index, node_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  assert_scalar_string(node_id, "node_id")
  child_rows <- which(index$nodes$parent_node_id == node_id)
  children <- index$nodes[child_rows, , drop = FALSE]
  if (nrow(children) > 0L) {
    ordering <- order(children$path, children$node_id, method = "radix")
    children <- children[ordering, , drop = FALSE]
  }
  rownames(children) <- NULL
  metadata_index_decorate_nodes(index, children)
}

metadata_index_nearby_branches <- function(index, node_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  ancestors <- metadata_index_node_ancestors(index, node_id)
  empty <- metadata_index_decorate_nodes(
    index,
    index$nodes[FALSE, , drop = FALSE]
  )
  empty$anchor_node_id <- character()
  empty$anchor_label <- character()
  empty$distance <- integer()
  if (nrow(ancestors) < 2L) return(empty)

  rows <- list()
  row_count <- 0L
  for (level in seq_len(nrow(ancestors) - 1L)) {
    anchor <- ancestors[level, , drop = FALSE]
    path_child_id <- ancestors$node_id[[level + 1L]]
    sibling_rows <- which(
      index$nodes$parent_node_id == anchor$node_id[[1L]] &
        index$nodes$node_id != path_child_id &
        index$nodes$kind %in% c("resource", "group", "table")
    )
    siblings <- index$nodes[sibling_rows, , drop = FALSE]
    if (nrow(siblings) == 0L) next
    siblings$anchor_node_id <- anchor$node_id[[1L]]
    siblings$anchor_label <- anchor$label[[1L]]
    siblings$distance <- nrow(ancestors) - level
    row_count <- row_count + 1L
    rows[[row_count]] <- siblings
  }
  if (length(rows) == 0L) return(empty)

  result <- do.call(rbind, rows)
  ordering <- order(result$distance, result$path, method = "radix")
  result <- result[ordering, , drop = FALSE]
  result <- result[!duplicated(result$node_id), , drop = FALSE]
  rownames(result) <- NULL
  decorated <- metadata_index_decorate_nodes(
    index,
    result[names(index$nodes)]
  )
  decorated$anchor_node_id <- result$anchor_node_id
  decorated$anchor_label <- result$anchor_label
  decorated$distance <- result$distance
  decorated
}

metadata_index_node_context <- function(index, node_id) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  node <- metadata_index_node(index, node_id)
  if (is.null(node)) return(NULL)
  entry_rows <- which(index$entries$node_id == node_id)
  entries <- index$entries[entry_rows, , drop = FALSE]
  if (nrow(entries) > 0L) {
    ordering <- order(entries$path, entries$entry_id, method = "radix")
    entries <- entries[ordering, , drop = FALSE]
  }
  group_ids <- unique(entries$group_id)
  group_rows <- match(group_ids, index$groups$group_id)
  groups <- index$groups[group_rows[!is.na(group_rows)], , drop = FALSE]
  rownames(entries) <- NULL
  rownames(groups) <- NULL
  structure(
    list(
      node = metadata_index_decorate_nodes(index, node),
      ancestors = metadata_index_decorate_nodes(
        index,
        metadata_index_node_ancestors(index, node_id)
      ),
      entries = entries,
      groups = groups,
      children = metadata_index_node_children(index, node_id),
      nearby = metadata_index_nearby_branches(index, node_id)
    ),
    class = c("llw_metadata_node_context", "list")
  )
}

metadata_index_group_neighborhood <- function(index, group) {
  index <- validate_metadata_index(index, index$source_fingerprint)
  if (is.character(group)) {
    assert_scalar_string(group, "group")
    group <- metadata_index_group(index, group)
  }
  if (is.null(group) || !inherits(group, "llw_metadata_group")) {
    return(NULL)
  }
  node_ids <- unique(group$entries$node_id)
  node_rows <- match(node_ids, index$nodes$node_id)
  item_nodes <- index$nodes[node_rows[!is.na(node_rows)], , drop = FALSE]
  property_rows <- which(index$entries$node_id %in% item_nodes$node_id)
  property_entries <- index$entries[property_rows, , drop = FALSE]
  property_group_ids <- unique(property_entries$group_id)
  property_rows <- match(property_group_ids, index$groups$group_id)
  property_groups <- index$groups[
    property_rows[!is.na(property_rows)],
    ,
    drop = FALSE
  ]
  instance_records <- vapply(item_nodes$node_id, function(node_id) {
    records <- unique(group$entries$record[group$entries$node_id == node_id])
    records <- metadata_nonempty_text(records)
    record <- if (length(records) > 0L) records[[1L]] else ""
    if (
      nzchar(record) &&
        sum(group$entries$record == record, na.rm = TRUE) == 1L
    ) {
      return(record)
    }
    ancestors <- metadata_index_node_ancestors(index, node_id)
    parent_records <- ancestors[
      ancestors$kind == "record" & ancestors$node_id != node_id,
      ,
      drop = FALSE
    ]
    if (nrow(parent_records) > 0L) {
      parent <- utils::tail(parent_records$label, 1L)
      if (nzchar(record) && !identical(record, parent)) {
        return(paste(record, "in", parent))
      }
      return(parent)
    }
    if (nzchar(record)) record else node_id
  }, character(1))
  item_nodes <- metadata_index_decorate_nodes(index, item_nodes)
  item_nodes$instance_record <- instance_records
  rownames(item_nodes) <- NULL
  rownames(property_entries) <- NULL
  rownames(property_groups) <- NULL
  representative_node_id <- if (nrow(item_nodes) > 0L) {
    item_nodes$node_id[[1L]]
  } else {
    ""
  }
  structure(
    list(
      selected_group_id = group$group$group_id[[1L]],
      item_nodes = item_nodes,
      property_entries = property_entries,
      property_groups = property_groups,
      representative_node_id = representative_node_id
    ),
    class = c("llw_metadata_neighborhood", "list")
  )
}

metadata_display_label <- function(raw_name, label = NA_character_) {
  assert_scalar_string(raw_name, "raw_name")
  label <- metadata_nonempty_text(label)
  if (length(label) == 0L) raw_name else label[[1L]]
}

metadata_display_unit <- function(unit = NA_character_) {
  unit <- metadata_nonempty_text(unit)
  if (length(unit) == 0L) "unit not specified" else unit[[1L]]
}

glc_metadata_namespace_id <- function(source) {
  paste0(
    "glc:",
    source$repository,
    "@",
    tolower(source$resolved_revision)
  )
}

new_glc_source_metadata_snapshot <- function(
  source,
  resources,
  metadata,
  datasets = data.frame(),
  variables = data.frame(),
  state = NULL
) {
  if (!is.list(source)) {
    abort_llw(
      "A GLC metadata snapshot requires source provenance.",
      type = "validation"
    )
  }
  required_source <- c(
    "repository",
    "resolved_revision",
    "schema_version",
    "glcdp_version"
  )
  if (!all(required_source %in% names(source))) {
    abort_llw(
      "GLC metadata source provenance is incomplete.",
      type = "validation"
    )
  }
  retained_source_fields <- intersect(
    c(
      "registry_url",
      "registry_generated_at",
      "repository",
      "requested_revision",
      "resolved_revision",
      "validation_state",
      "attestation_verified",
      "schema_version",
      "glcdp_version"
    ),
    names(source)
  )
  source <- as.list(source)[retained_source_fields]
  if (!is.data.frame(resources) || !is.list(metadata)) {
    abort_llw(
      "GLC resources and metadata do not match their exported API contracts.",
      type = "validation"
    )
  }
  if (!is.data.frame(datasets) || !is.data.frame(variables)) {
    abort_llw("GLC metadata inventories must be tabular.", type = "validation")
  }
  namespace_id <- glc_metadata_namespace_id(source)
  source_namespace <- new_metadata_source_namespace(
    namespace_id = namespace_id,
    source_dataset_id = namespace_id,
    label = paste0(
      source$repository,
      " at ",
      substr(source$resolved_revision, 1L, 12L)
    ),
    source_type = "glc_package",
    records = list(
      core_metadata = metadata,
      dataset_inventory = datasets,
      variable_inventory = variables
    ),
    resources = resources,
    state = state %||%
      if (metadata_value_has_content(metadata)) {
        "complete"
      } else {
        "absent"
      },
    source_schema_version = source$schema_version,
    provenance = source
  )
  bundle <- new_metadata_bundle(
    stats::setNames(list(source_namespace), namespace_id)
  )
  snapshot <- structure(
    list(
      snapshot_schema = "llw-glc-source-metadata-snapshot",
      snapshot_version = metadata_contract_version(),
      source = source,
      bundle = bundle
    ),
    class = c("llw_glc_source_metadata_snapshot", "list")
  )
  snapshot$fingerprint <- metadata_fingerprint(snapshot)
  assert_serializable_value(snapshot, "GLC source metadata snapshot")
  snapshot
}

validate_glc_source_metadata_snapshot <- function(snapshot) {
  if (
    !inherits(snapshot, "llw_glc_source_metadata_snapshot") ||
      !is.list(snapshot)
  ) {
    abort_llw(
      "The GLC source metadata snapshot is invalid.",
      type = "validation"
    )
  }
  required <- c(
    "snapshot_schema",
    "snapshot_version",
    "source",
    "bundle",
    "fingerprint"
  )
  if (!all(required %in% names(snapshot))) {
    abort_llw(
      "The GLC source metadata snapshot is incomplete.",
      type = "validation"
    )
  }
  expected <- snapshot$fingerprint
  value <- snapshot
  value$fingerprint <- NULL
  if (
    !identical(snapshot$snapshot_schema, "llw-glc-source-metadata-snapshot") ||
      !identical(snapshot$snapshot_version, metadata_contract_version()) ||
      !identical(expected, metadata_fingerprint(value))
  ) {
    abort_llw(
      "The GLC source metadata snapshot failed its fingerprint check.",
      type = "validation"
    )
  }
  snapshot$bundle <- validate_metadata_bundle(snapshot$bundle)
  snapshot$fingerprint <- NULL
  snapshot$fingerprint <- metadata_fingerprint(snapshot)
  snapshot
}

new_selected_glc_source_metadata_snapshot <- function(
  source,
  metadata,
  selected_groups,
  variables,
  resources = NULL
) {
  if (is.null(resources)) {
    resource_status <- metadata$tables$resource_status %||% data.frame()
    if (
      is.data.frame(resource_status) && "resource" %in% names(resource_status)
    ) {
      resource_count <- nrow(resource_status)
      resources <- data.frame(
        resource = as.character(resource_status$resource),
        path = rep(NA_character_, resource_count),
        core = rep(TRUE, resource_count),
        format = rep(NA_character_, resource_count),
        media_type = rep(NA_character_, resource_count),
        stringsAsFactors = FALSE
      )
    } else {
      resources <- metadata_empty_resources()
    }
  }
  new_glc_source_metadata_snapshot(
    source = source,
    resources = resources,
    metadata = list(
      selected_metadata = metadata,
      selected_groups = selected_groups
    ),
    datasets = metadata$tables$datasets %||% data.frame(),
    variables = variables
  )
}

metadata_nested_bundle_state <- function(bundle, factual_metadata = list()) {
  if (!inherits(bundle, "llw_metadata_bundle")) {
    return(
      if (metadata_value_has_content(factual_metadata)) {
        "partial"
      } else {
        "absent"
      }
    )
  }
  bundle <- validate_metadata_bundle(bundle)
  states <- unname(vapply(bundle$sources, `[[`, character(1), "state"))
  if (all(states == "complete")) return("complete")
  if (
    all(states == "absent") &&
      !metadata_value_has_content(factual_metadata)
  ) {
    return("absent")
  }
  "partial"
}

new_appended_metadata_bundle <- function(source_provenance) {
  if (!is.list(source_provenance) || length(source_provenance) == 0L) {
    abort_llw(
      "Append metadata requires source provenance.",
      type = "validation"
    )
  }
  sources <- lapply(source_provenance, function(source) {
    dataset_id <- source$dataset_id
    new_metadata_source_namespace(
      namespace_id = dataset_id,
      source_dataset_id = dataset_id,
      label = source$display_name,
      source_type = "append_source",
      records = list(
        metadata_bundle = source$metadata_bundle,
        factual_metadata = source$factual_metadata %||% list()
      ),
      state = metadata_nested_bundle_state(
        source$metadata_bundle,
        source$factual_metadata %||% list()
      ),
      provenance = list(
        dataset_id = dataset_id,
        revision = source$revision,
        raw_checksum = source$raw_checksum,
        source_manifest = source$source_manifest
      )
    )
  })
  names(sources) <- vapply(
    source_provenance,
    `[[`,
    character(1),
    "dataset_id"
  )
  new_metadata_bundle(sources)
}
