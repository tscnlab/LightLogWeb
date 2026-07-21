raw_import_minimum_lightlogr_version <- function() {
  "0.10.3"
}

raw_import_default_max_bytes <- function() {
  200 * 1024^2
}

validate_lightlogr_runtime <- function() {
  if (!requireNamespace("LightLogR", quietly = TRUE)) {
    abort_llw(
      "LightLogR is not installed.",
      type = "unavailable_feature",
      public_message = paste(
        "Raw import is unavailable because LightLogR is not installed.",
        "Restore the project environment and retry."
      )
    )
  }
  installed <- utils::packageVersion("LightLogR")
  required <- package_version(raw_import_minimum_lightlogr_version())
  if (installed < required) {
    abort_llw(
      paste0(
        "LightLogR ",
        installed,
        " is installed; raw import requires ",
        required,
        " or newer."
      ),
      type = "unavailable_feature",
      public_message = paste0(
        "Raw import requires LightLogR ",
        required,
        " or newer. Restore the project environment and retry."
      )
    )
  }
  required_exports <- c(
    "import_Dataset",
    "supported_devices",
    "supported_versions",
    "dominant_epoch",
    "gg_overview"
  )
  missing_exports <- setdiff(
    required_exports,
    getNamespaceExports("LightLogR")
  )
  if (length(missing_exports) > 0L) {
    abort_llw(
      paste0(
        "The installed LightLogR namespace is missing required export(s): ",
        paste(missing_exports, collapse = ", "),
        "."
      ),
      type = "unavailable_feature",
      public_message = paste(
        "The installed LightLogR build does not provide the raw-import API",
        "expected by LightLogWeb. Restore the project environment and retry."
      )
    )
  }
  invisible(as.character(installed))
}

raw_import_extension_contract <- function() {
  # LightLogR 0.10.3 exposes devices and versions, but not a machine-readable
  # browser-file extension registry. This adapter follows its exported
  # ll_import_expr() readers and documented device examples. Keep it narrow,
  # fail closed, and test it against every supported_devices() value.
  list(
    Actiwatch_Spectrum = "csv",
    ActLumus = c("txt", "zip"),
    ActTrust = "txt",
    Circadian_Eye = "csv",
    Clouclip = "xls",
    DeLux = "csv",
    GENEActiv_GGIR = character(),
    Kronowise = c("txt", "tsv"),
    LiDo = "csv",
    LightWatcher = c("txt", "tsv"),
    LIMO = "csv",
    LYS = "csv",
    MiEye = "csv",
    MotionWatch8 = "csv",
    nanoLambda = "txt",
    OcuWEAR = "csv",
    Speccy = "csv",
    SpectraWear = "csv",
    VEET = c("csv", "txt", "zip")
  )
}

raw_import_devices <- function() {
  validate_lightlogr_runtime()
  devices <- LightLogR::supported_devices()
  assert_character_vector(
    devices,
    "LightLogR supported devices",
    allow_empty = FALSE
  )
  sort(unique(devices))
}

raw_import_accept_extensions <- function() {
  extensions <- unique(unlist(
    raw_import_extension_contract(),
    use.names = FALSE
  ))
  paste0(".", sort(extensions[nzchar(extensions)]))
}

raw_import_supported_versions <- function(device) {
  assert_scalar_string(device, "device")
  if (!device %in% raw_import_devices()) {
    abort_llw(
      paste0("Unsupported LightLogR device `", device, "`."),
      type = "validation",
      public_message = paste0(
        "`",
        device,
        "` is not supported by the installed LightLogR version. Choose a listed device."
      )
    )
  }
  versions <- LightLogR::supported_versions(device)
  required <- c("Device", "Version", "Default", "Description")
  if (
    !is.data.frame(versions) ||
      nrow(versions) == 0L ||
      !all(required %in% names(versions))
  ) {
    abort_llw(
      paste0(
        "LightLogR returned no usable version registry for `",
        device,
        "`."
      ),
      type = "unavailable_feature",
      public_message = paste0(
        "Version information for `",
        device,
        "` is unavailable in the installed LightLogR build."
      )
    )
  }
  as.data.frame(versions[, required, drop = FALSE], stringsAsFactors = FALSE)
}

normalize_raw_import_version <- function(device, version = NULL) {
  versions <- raw_import_supported_versions(device)
  if (
    is.null(version) ||
      (is.character(version) && length(version) == 1L && !nzchar(version))
  ) {
    return("default")
  }
  assert_scalar_string(version, "version")
  if (!identical(version, "default") && !version %in% versions$Version) {
    abort_llw(
      paste0(
        "Unsupported version `",
        version,
        "` for device `",
        device,
        "`."
      ),
      type = "validation",
      public_message = paste0(
        "Format version `",
        version,
        "` is not supported for `",
        device,
        "` by the installed LightLogR version."
      )
    )
  }
  version
}

raw_filename_extension <- function(filename) {
  tolower(tools::file_ext(basename(filename)))
}

raw_filename_stem <- function(filename) {
  tools::file_path_sans_ext(basename(filename))
}

new_filename_id_mapping <- function(
  original_names,
  id_mode = c("automated", "manual", "extract"),
  manual_id = NULL,
  extract_pattern = NULL
) {
  id_mode <- match.arg(id_mode)
  assert_character_vector(original_names, "original_names", allow_empty = FALSE)
  stems <- raw_filename_stem(original_names)
  if (any(!nzchar(trimws(stems)))) {
    abort_llw(
      "One or more filenames have no usable stem for participant mapping.",
      type = "validation",
      public_message = paste(
        "Every selected file needs a name before its extension.",
        "Rename the file or choose an explicit participant-ID mapping."
      )
    )
  }

  proposed_ids <- switch(
    id_mode,
    automated = stems,
    manual = {
      assert_scalar_string(manual_id, "manual_id")
      rep(manual_id, length(stems))
    },
    extract = {
      assert_scalar_string(extract_pattern, "extract_pattern")
      matched <- tryCatch(
        stringr::str_match(stems, extract_pattern),
        error = function(cnd) {
          abort_llw(
            paste0(
              "Invalid participant-ID expression: ",
              conditionMessage(cnd)
            ),
            type = "validation",
            public_message = paste(
              "The participant-ID expression is not a valid regular expression.",
              "Correct it and review the mapping preview."
            ),
            diagnostics = list(original_message = conditionMessage(cnd)),
            parent = cnd
          )
        }
      )
      if (ncol(matched) > 1L) matched[, 2L] else matched[, 1L]
    }
  )

  proposed_ids <- as.character(proposed_ids)
  invalid <- is.na(proposed_ids) |
    !nzchar(trimws(proposed_ids)) |
    grepl("[[:cntrl:]]", proposed_ids)
  if (any(invalid)) {
    files <- paste(basename(original_names[invalid]), collapse = ", ")
    abort_llw(
      paste0("Participant IDs could not be mapped for file(s): ", files, "."),
      type = "validation",
      public_message = paste0(
        "No usable participant ID was produced for: ",
        files,
        ". Adjust the mapping and review the preview."
      )
    )
  }
  # Several source files may legitimately form one participant record. Keep
  # this flag for the mapping preview and provenance, but do not reject the
  # mapping solely because participant IDs repeat across files.
  duplicate_proposed <- duplicated(proposed_ids) |
    duplicated(proposed_ids, fromLast = TRUE)

  mapping <- data.frame(
    source_index = seq_along(original_names),
    original_name = as.character(original_names),
    filename_stem = stems,
    proposed_id = proposed_ids,
    mapping_source = id_mode,
    duplicate_proposed = duplicate_proposed,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  structure(mapping, class = c("llw_filename_id_mapping", "data.frame"))
}

validate_filename_id_mapping <- function(mapping, original_names) {
  required <- c(
    "source_index",
    "original_name",
    "filename_stem",
    "proposed_id",
    "mapping_source",
    "duplicate_proposed"
  )
  if (
    !inherits(mapping, "llw_filename_id_mapping") ||
      !is.data.frame(mapping) ||
      !all(required %in% names(mapping)) ||
      nrow(mapping) != length(original_names)
  ) {
    abort_llw(
      "`id_mapping` must be created by `new_filename_id_mapping()`.",
      type = "validation"
    )
  }
  if (
    !identical(
      as.character(mapping$original_name),
      as.character(original_names)
    )
  ) {
    abort_llw(
      "The participant-ID mapping does not match the staged file order.",
      type = "validation",
      public_message = paste(
        "The selected files changed after the participant mapping was prepared.",
        "Review the files and mapping again."
      )
    )
  }
  assert_character_vector(mapping$proposed_id, "id_mapping$proposed_id", FALSE)
  mapping
}

zip_entry_is_unsafe <- function(path) {
  normalized <- gsub("\\\\", "/", path)
  pieces <- strsplit(normalized, "/", fixed = TRUE)[[1L]]
  startsWith(normalized, "/") ||
    grepl("^[A-Za-z]:", normalized) ||
    any(pieces == "..")
}

zip_entry_is_metadata <- function(path) {
  normalized <- gsub("\\\\", "/", path)
  filename <- basename(normalized)
  startsWith(normalized, "__MACOSX/") ||
    identical(filename, ".DS_Store") ||
    startsWith(filename, "._")
}

inspect_raw_zip <- function(
  path,
  original_name,
  allowed_extensions,
  max_bytes
) {
  listing <- tryCatch(
    suppressWarnings(utils::unzip(path, list = TRUE)),
    error = function(cnd) {
      abort_llw(
        paste0("Could not inspect ZIP upload `", original_name, "`."),
        type = "validation",
        public_message = paste0(
          "`",
          basename(original_name),
          "` is not a readable ZIP archive. Export it again and retry."
        ),
        diagnostics = list(original_message = conditionMessage(cnd)),
        parent = cnd
      )
    }
  )
  files <- listing[!grepl("/$", listing$Name), , drop = FALSE]
  unsafe <- vapply(files$Name, zip_entry_is_unsafe, logical(1))
  if (any(unsafe)) {
    abort_llw(
      paste0("ZIP upload `", original_name, "` contains an unsafe path."),
      type = "validation",
      public_message = paste0(
        "`",
        basename(original_name),
        "` contains an unsafe archive path and cannot be imported."
      )
    )
  }
  expanded_bytes <- sum(as.numeric(files$Length))
  if (
    !is.finite(expanded_bytes) ||
      expanded_bytes <= 0 ||
      expanded_bytes > max_bytes
  ) {
    abort_llw(
      paste0("ZIP upload `", original_name, "` has an unsafe expanded size."),
      type = "resource",
      public_message = paste0(
        "The expanded contents of `",
        basename(original_name),
        "` are empty or exceed the configured upload limit."
      )
    )
  }
  metadata <- vapply(files$Name, zip_entry_is_metadata, logical(1))
  files <- files[!metadata, , drop = FALSE]
  if (nrow(files) != 1L) {
    abort_llw(
      paste0(
        "ZIP upload `",
        original_name,
        "` contains ",
        nrow(files),
        " files."
      ),
      type = "validation",
      public_message = paste0(
        "`",
        basename(original_name),
        "` must contain exactly one device-export file."
      )
    )
  }
  inner_extension <- raw_filename_extension(files$Name[[1L]])
  allowed_inner <- setdiff(allowed_extensions, "zip")
  if (!inner_extension %in% allowed_inner) {
    abort_llw(
      paste0(
        "ZIP upload `",
        original_name,
        "` contains unsupported extension `.",
        inner_extension,
        "`."
      ),
      type = "validation",
      public_message = paste0(
        "`",
        basename(original_name),
        "` does not contain a supported device-export file."
      )
    )
  }
  list(
    entry_name = files$Name[[1L]],
    inner_extension = inner_extension,
    payload_bytes = as.numeric(files$Length[[1L]]),
    expanded_bytes = expanded_bytes
  )
}

validate_raw_zip <- function(
  path,
  original_name,
  allowed_extensions,
  max_bytes
) {
  inspect_raw_zip(
    path = path,
    original_name = original_name,
    allowed_extensions = allowed_extensions,
    max_bytes = max_bytes
  )$expanded_bytes
}

preflight_raw_import_request <- function(
  staged_files,
  device,
  version,
  timezone,
  id_mapping,
  max_bytes
) {
  lightlogr_version <- validate_lightlogr_runtime()
  staged_files <- validate_staged_uploads(staged_files)
  assert_scalar_string(timezone, "timezone")
  if (!timezone %in% OlsonNames()) {
    abort_llw(
      paste0("Unknown IANA time zone `", timezone, "`."),
      type = "validation",
      public_message = paste0(
        "`",
        timezone,
        "` is not a valid IANA source time zone. Choose a listed time zone."
      )
    )
  }
  version <- normalize_raw_import_version(device, version)
  if (
    !is.numeric(max_bytes) ||
      length(max_bytes) != 1L ||
      is.na(max_bytes) ||
      !is.finite(max_bytes) ||
      max_bytes <= 0
  ) {
    abort_llw(
      "`max_bytes` must be one positive finite number.",
      type = "validation"
    )
  }
  if (anyDuplicated(staged_files$original_name)) {
    duplicates <- unique(staged_files$original_name[
      duplicated(staged_files$original_name) |
        duplicated(staged_files$original_name, fromLast = TRUE)
    ])
    abort_llw(
      paste0(
        "Duplicate original filename(s): ",
        paste(duplicates, collapse = ", "),
        "."
      ),
      type = "validation",
      public_message = paste0(
        "The selection contains repeated filename(s): ",
        paste(basename(duplicates), collapse = ", "),
        ". Rename or import them separately so provenance remains unambiguous."
      )
    )
  }
  id_mapping <- validate_filename_id_mapping(
    id_mapping,
    staged_files$original_name
  )

  extension_contract <- raw_import_extension_contract()
  if (!device %in% names(extension_contract)) {
    abort_llw(
      paste0(
        "No browser extension contract exists for LightLogR device `",
        device,
        "`."
      ),
      type = "unavailable_feature",
      public_message = paste0(
        "`",
        device,
        "` is new to this LightLogR version and has not yet been validated for browser uploads."
      )
    )
  }
  allowed_extensions <- extension_contract[[device]]
  if (length(allowed_extensions) == 0L) {
    abort_llw(
      paste0("Device `", device, "` requires a directory import."),
      type = "unavailable_feature",
      public_message = paste0(
        "`",
        device,
        "` requires a directory-based export that browser file upload cannot preserve. Use LightLogR locally for this format."
      )
    )
  }

  total_bytes <- sum(staged_files$size_bytes)
  if (!is.finite(total_bytes) || total_bytes > max_bytes) {
    abort_llw(
      "The staged upload exceeds the configured request limit.",
      type = "resource",
      public_message = paste0(
        "The selected files exceed the ",
        format(round(max_bytes / 1024^2, 1), trim = TRUE),
        " MB upload limit. Reduce the selection and retry."
      )
    )
  }
  if (any(staged_files$size_bytes <= 0)) {
    empty <- basename(staged_files$original_name[staged_files$size_bytes <= 0])
    abort_llw(
      paste0("Empty upload(s): ", paste(empty, collapse = ", "), "."),
      type = "validation",
      public_message = paste0(
        "The following file is empty: ",
        paste(empty, collapse = ", "),
        ". Export it again and retry."
      )
    )
  }
  if (any(file.access(staged_files$staged_path, mode = 4L) != 0L)) {
    abort_llw(
      "One or more staged uploads are not readable.",
      type = "resource",
      public_message = paste(
        "A selected file cannot be read from private session storage.",
        "Choose the files again and retry."
      )
    )
  }

  extensions <- vapply(
    staged_files$original_name,
    raw_filename_extension,
    character(1)
  )
  unsupported <- !extensions %in% allowed_extensions
  if (any(unsupported)) {
    labels <- paste0(
      basename(staged_files$original_name[unsupported]),
      " (.",
      extensions[unsupported],
      ")"
    )
    abort_llw(
      paste0(
        "Unsupported extension(s) for `",
        device,
        "`: ",
        paste(labels, collapse = ", "),
        "."
      ),
      type = "validation",
      public_message = paste0(
        "The selected extension is not supported for `",
        device,
        "`: ",
        paste(labels, collapse = ", "),
        "."
      )
    )
  }

  expanded_bytes <- staged_files$size_bytes
  zip_rows <- which(extensions == "zip")
  if (length(zip_rows) > 0L) {
    expanded_bytes[zip_rows] <- vapply(
      zip_rows,
      function(index) {
        validate_raw_zip(
          staged_files$staged_path[[index]],
          staged_files$original_name[[index]],
          allowed_extensions,
          max_bytes
        )
      },
      numeric(1)
    )
  }
  if (sum(expanded_bytes) > max_bytes) {
    abort_llw(
      "Expanded ZIP contents exceed the configured request limit.",
      type = "resource",
      public_message = paste0(
        "The selected files expand beyond the ",
        format(round(max_bytes / 1024^2, 1), trim = TRUE),
        " MB upload limit. Reduce the selection and retry."
      )
    )
  }

  files <- data.frame(
    original_name = staged_files$original_name,
    size_bytes = staged_files$size_bytes,
    expanded_bytes = expanded_bytes,
    sha256 = staged_files$sha256,
    extension = extensions,
    proposed_id = id_mapping$proposed_id,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      device = device,
      version = version,
      timezone = timezone,
      LightLogR_version = lightlogr_version,
      total_bytes = total_bytes,
      expanded_bytes = sum(expanded_bytes),
      max_bytes = max_bytes,
      files = files,
      warnings = character()
    ),
    class = c("llw_raw_import_preflight", "list")
  )
}

raw_import_column_kind <- function(x) {
  if (inherits(x, "POSIXct")) {
    return("POSIXct")
  }
  if (inherits(x, "Date")) {
    return("Date")
  }
  if (is.numeric(x)) {
    return("numeric")
  }
  if (is.logical(x) && all(is.na(x))) {
    return("empty")
  }
  if (is.factor(x) || is.character(x)) {
    return("categorical")
  }
  if (is.logical(x)) {
    return("logical")
  }
  if (is.list(x)) {
    return("list")
  }
  paste(class(x), collapse = "/")
}

capture_lightlogr_import <- function(arguments, original_name) {
  imported <- NULL
  warnings <- character()
  output <- tryCatch(
    withCallingHandlers(
      utils::capture.output(
        imported <- do.call(LightLogR::import_Dataset, arguments),
        type = "output"
      ),
      warning = function(cnd) {
        warnings <<- c(warnings, conditionMessage(cnd))
        invokeRestart("muffleWarning")
      },
      message = function(cnd) {
        invokeRestart("muffleMessage")
      }
    ),
    error = function(cnd) {
      abort_llw(
        paste0(
          "LightLogR could not parse `",
          original_name,
          "`: ",
          conditionMessage(cnd)
        ),
        type = "import",
        public_message = paste0(
          "LightLogR could not parse `",
          basename(original_name),
          "` with the selected device, version, and time zone. Review the export and settings, then retry."
        ),
        diagnostics = list(
          phase = "validation",
          file = basename(original_name),
          classes = class(cnd),
          original_message = conditionMessage(cnd)
        ),
        parent = cnd
      )
    }
  )
  if (!is.data.frame(imported) || nrow(imported) == 0L) {
    abort_llw(
      paste0("LightLogR returned no observations for `", original_name, "`."),
      type = "import",
      public_message = paste0(
        "`",
        basename(original_name),
        "` produced no observations with the selected import settings."
      ),
      diagnostics = list(phase = "validation", file = basename(original_name))
    )
  }
  duplicate_result <- "dupes" %in%
    names(imported) &&
    any(grepl("duplicate rows", warnings, ignore.case = TRUE))
  if (duplicate_result) {
    abort_llw(
      paste0("LightLogR stopped on identical rows in `", original_name, "`."),
      type = "validation",
      public_message = paste0(
        "`",
        basename(original_name),
        "` contains identical rows. Remove them in the source or explicitly enable duplicate removal before importing."
      ),
      diagnostics = list(
        phase = "validation",
        file = basename(original_name),
        import_warnings = warnings
      )
    )
  }
  list(data = imported, output = output, warnings = warnings)
}

preflight_raw_import_structure <- function(request, n_max = 25L) {
  if (!inherits(request, "llw_raw_import_request")) {
    abort_llw(
      "`request` must be created by `new_raw_import_request()`.",
      type = "validation"
    )
  }
  if (
    !is.numeric(n_max) ||
      length(n_max) != 1L ||
      is.na(n_max) ||
      n_max < 2 ||
      n_max != floor(n_max)
  ) {
    abort_llw(
      "`n_max` must be a whole number of at least 2.",
      type = "validation"
    )
  }

  previews <- vector("list", nrow(request$source_files))
  for (index in seq_len(nrow(request$source_files))) {
    arguments <- request$import_arguments
    arguments$filename <- request$import_arguments$filename[[index]]
    arguments$n_max <- as.integer(n_max)
    arguments$not.before <- as.Date("1900-01-01")
    arguments$silent <- TRUE
    arguments$print_n <- 0
    arguments$auto.plot <- FALSE
    previews[[index]] <- capture_lightlogr_import(
      arguments,
      request$source_files$original_name[[index]]
    )
  }

  reference_names <- names(previews[[1L]]$data)
  reference_kinds <- vapply(
    previews[[1L]]$data,
    raw_import_column_kind,
    character(1)
  )
  id_sources <- vapply(
    seq_along(previews),
    function(index) {
      observed_ids <- unique(as.character(previews[[index]]$data$Id))
      proposed_id <- request$id_mapping$proposed_id[[index]]

      # The probe uses the same ID arguments as the full import. LightLogR
      # keeps an embedded Id when present and otherwise applies the filename
      # fallback (or shared manual ID) shown by the app.
      if (length(observed_ids) > 0L && all(observed_ids == proposed_id)) {
        "filename_mapping"
      } else {
        "embedded"
      }
    },
    character(1)
  )
  problems <- character()
  signatures <- vector("list", length(previews))
  for (index in seq_along(previews)) {
    data <- previews[[index]]$data
    kinds <- vapply(data, raw_import_column_kind, character(1))
    signatures[[index]] <- data.frame(
      original_name = request$source_files$original_name[[index]],
      variable = names(data),
      kind = kinds,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    if (!identical(names(data), reference_names)) {
      problems <- c(
        problems,
        paste0(
          "`",
          basename(request$source_files$original_name[[index]]),
          "` has different columns"
        )
      )
      next
    }
    compatible <- kinds == reference_kinds |
      kinds == "empty" |
      reference_kinds == "empty"
    if (any(!compatible)) {
      variables <- names(data)[!compatible]
      problems <- c(
        problems,
        paste0(
          "`",
          basename(request$source_files$original_name[[index]]),
          "` has incompatible type(s) for ",
          paste(variables, collapse = ", ")
        )
      )
    }
  }
  if (length(problems) > 0L) {
    abort_llw(
      paste(problems, collapse = "; "),
      type = "validation",
      public_message = paste(
        "The selected files do not share one compatible export structure.",
        "Import files with the same device format together, or split them into separate datasets."
      ),
      diagnostics = list(phase = "validation", structural_problems = problems)
    )
  }

  structure(
    list(
      files = data.frame(
        source_index = seq_along(previews),
        original_name = request$source_files$original_name,
        preview_rows = vapply(previews, function(x) nrow(x$data), integer(1)),
        id_source = id_sources,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      signatures = do.call(rbind, signatures),
      warnings = unique(unlist(lapply(previews, `[[`, "warnings"))),
      messages = unlist(lapply(previews, `[[`, "output"), use.names = FALSE)
    ),
    class = c("llw_raw_import_structure", "list")
  )
}
