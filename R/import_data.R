raw_upload_basename <- function(filename) {
  assert_character_vector(filename, "filename", allow_empty = FALSE)
  normalized <- gsub("\\\\", "/", filename)
  names <- basename(normalized)
  invalid <-
    !nzchar(trimws(names)) |
    names %in% c(".", "..") |
    grepl("[[:cntrl:]]", names)
  if (any(invalid)) {
    abort_llw(
      "One or more uploaded filenames are unsafe.",
      type = "validation",
      public_message = paste(
        "A selected file has an unusable filename.",
        "Rename it and choose the file again."
      )
    )
  }
  names
}

stage_import_files <- function(files, upload_root) {
  if (!is.data.frame(files) || !all(c("name", "datapath") %in% names(files))) {
    abort_llw(
      "Uploaded files must provide `name` and `datapath` columns.",
      type = "validation"
    )
  }
  assert_scalar_string(upload_root, "upload_root")
  if (!dir.exists(upload_root)) {
    created <- dir.create(
      upload_root,
      recursive = TRUE,
      showWarnings = FALSE,
      mode = "0700"
    )
    if (!created && !dir.exists(upload_root)) {
      abort_llw(
        "The session upload root could not be created.",
        type = "resource"
      )
    }
  }
  upload_root <- normalizePath(upload_root, winslash = "/", mustWork = TRUE)
  if (nrow(files) == 0L) {
    abort_llw("At least one uploaded file is required.", type = "validation")
  }
  original_names <- raw_upload_basename(as.character(files$name))
  source_paths <- as.character(files$datapath)
  if (
    anyNA(original_names) ||
      anyNA(source_paths) ||
      any(!nzchar(original_names)) ||
      any(!nzchar(source_paths))
  ) {
    abort_llw(
      "Uploaded file names and paths must not be missing.",
      type = "validation"
    )
  }
  source_info <- file.info(source_paths)
  if (anyNA(source_info$size) || any(source_info$isdir)) {
    abort_llw(
      "One or more uploaded source files are unavailable.",
      type = "resource"
    )
  }
  task_dir <- file.path(upload_root, new_stable_id("upload"))
  created <- dir.create(
    task_dir,
    recursive = TRUE,
    showWarnings = FALSE,
    mode = "0700"
  )
  if (!created && !dir.exists(task_dir)) {
    abort_llw(
      "The session upload directory could not be created.",
      type = "resource"
    )
  }
  staging_complete <- FALSE
  on.exit(
    {
      if (!staging_complete) {
        unlink(task_dir, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  storage_dirs <- file.path(
    task_dir,
    sprintf("%04d", seq_along(original_names))
  )
  directories_created <- vapply(
    storage_dirs,
    dir.create,
    logical(1),
    recursive = FALSE,
    showWarnings = FALSE,
    mode = "0700"
  )
  if (!all(directories_created | dir.exists(storage_dirs))) {
    unlink(task_dir, recursive = TRUE, force = TRUE)
    abort_llw(
      "One or more private upload directories could not be created.",
      type = "resource"
    )
  }
  # Each upload receives its own directory, so LightLogR sees the original
  # basename while repeated stems cannot overwrite one another in storage.
  destinations <- file.path(storage_dirs, original_names)
  copied <- file.copy(
    from = source_paths,
    to = destinations,
    overwrite = FALSE,
    copy.mode = TRUE
  )
  if (!all(copied)) {
    unlink(task_dir, recursive = TRUE, force = TRUE)
    abort_llw(
      "One or more uploaded files could not be staged in session storage.",
      type = "resource"
    )
  }
  destination_info <- file.info(destinations)
  if (!identical(unname(source_info$size), unname(destination_info$size))) {
    unlink(task_dir, recursive = TRUE, force = TRUE)
    abort_llw(
      "Staged uploads failed their byte-size integrity check.",
      type = "resource"
    )
  }
  source_hashes <- vapply(source_paths, sha256_file, character(1))
  hashes <- vapply(destinations, sha256_file, character(1))
  if (!identical(unname(source_hashes), unname(hashes))) {
    unlink(task_dir, recursive = TRUE, force = TRUE)
    abort_llw(
      "Staged uploads failed their SHA-256 integrity check.",
      type = "resource",
      public_message = paste(
        "A selected file changed while it was being copied into session",
        "storage. Choose the files again and retry."
      )
    )
  }
  staged <- data.frame(
    original_name = original_names,
    staged_path = destinations,
    size_bytes = unname(destination_info$size),
    sha256 = unname(hashes),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  staging_complete <- TRUE
  structure(staged, class = c("llw_staged_uploads", "data.frame"))
}

validate_staged_uploads <- function(staged_files, verify_files = TRUE) {
  assert_flag(verify_files, "verify_files")
  required <- c(
    "original_name",
    "staged_path",
    "size_bytes",
    "sha256"
  )
  if (
    !inherits(staged_files, "llw_staged_uploads") ||
      !is.data.frame(staged_files) ||
      !all(required %in% names(staged_files)) ||
      nrow(staged_files) == 0L
  ) {
    abort_llw(
      "`staged_files` must be created by `stage_import_files()`.",
      type = "validation"
    )
  }
  assert_character_vector(
    staged_files$original_name,
    "staged_files$original_name",
    allow_empty = FALSE
  )
  assert_character_vector(
    staged_files$staged_path,
    "staged_files$staged_path",
    allow_empty = FALSE
  )
  assert_character_vector(
    staged_files$sha256,
    "staged_files$sha256",
    allow_empty = FALSE
  )
  if (any(!grepl("^sha256:[0-9a-f]{64}$", staged_files$sha256))) {
    abort_llw(
      "Staged upload hashes must use the SHA-256 manifest form.",
      type = "validation"
    )
  }
  sizes <- staged_files$size_bytes
  if (
    !is.numeric(sizes) ||
      anyNA(sizes) ||
      any(!is.finite(sizes)) ||
      any(sizes < 0) ||
      any(sizes != floor(sizes))
  ) {
    abort_llw(
      "Staged upload sizes must be non-negative whole numbers.",
      type = "validation"
    )
  }
  if (!verify_files) {
    return(staged_files)
  }

  info <- file.info(staged_files$staged_path)
  if (anyNA(info$size) || any(info$isdir)) {
    abort_llw(
      "A staged source file is no longer available.",
      type = "resource"
    )
  }
  if (!identical(unname(info$size), unname(sizes))) {
    abort_llw(
      "A staged source file changed size after it was copied.",
      type = "resource",
      public_message = paste(
        "A staged source file failed its integrity check.",
        "Choose the files again and retry."
      )
    )
  }
  current_hashes <- vapply(
    staged_files$staged_path,
    sha256_file,
    character(1)
  )
  if (!identical(unname(current_hashes), unname(staged_files$sha256))) {
    abort_llw(
      "A staged source file changed after it was copied.",
      type = "resource",
      public_message = paste(
        "A staged source file failed its integrity check.",
        "Choose the files again and retry."
      )
    )
  }
  staged_files
}

prepare_raw_import_files <- function(request) {
  if (!inherits(request, "llw_raw_import_request")) {
    abort_llw(
      "`request` must be created by `new_raw_import_request()`.",
      type = "validation"
    )
  }
  if (!identical(request$import_arguments$device, "VEET")) {
    return(request)
  }

  extensions <- vapply(
    request$source_files$original_name,
    raw_filename_extension,
    character(1)
  )
  zip_rows <- which(extensions == "zip")
  if (length(zip_rows) == 0L) {
    return(request)
  }

  allowed_extensions <- raw_import_extension_contract()[["VEET"]]
  import_filenames <- request$import_arguments$filename
  for (index in zip_rows) {
    archive <- request$source_files$staged_path[[index]]
    original_name <- request$source_files$original_name[[index]]
    payload <- inspect_raw_zip(
      path = archive,
      original_name = original_name,
      allowed_extensions = allowed_extensions,
      max_bytes = request$max_bytes
    )
    derived_dir <- file.path(dirname(archive), "veet-extracted")
    created <- dir.create(
      derived_dir,
      recursive = TRUE,
      showWarnings = FALSE,
      mode = "0700"
    )
    if (!created && !dir.exists(derived_dir)) {
      abort_llw(
        paste0(
          "Could not create a private VEET extraction directory for `",
          original_name,
          "`."
        ),
        type = "resource",
        public_message = paste0(
          "The VEET archive `",
          basename(original_name),
          "` could not be prepared in private session storage. Choose it again and retry."
        )
      )
    }

    extracted <- tryCatch(
      suppressWarnings(utils::unzip(
        archive,
        files = payload$entry_name,
        overwrite = TRUE,
        junkpaths = TRUE,
        exdir = derived_dir
      )),
      error = function(cnd) {
        abort_llw(
          paste0("Could not extract VEET archive `", original_name, "`."),
          type = "import",
          public_message = paste0(
            "The VEET archive `",
            basename(original_name),
            "` passed inspection but could not be unpacked. Export it again and retry."
          ),
          diagnostics = list(original_message = conditionMessage(cnd)),
          parent = cnd
        )
      }
    )
    if (length(extracted) != 1L || !file.exists(extracted[[1L]])) {
      abort_llw(
        paste0(
          "VEET archive `",
          original_name,
          "` did not yield one readable payload."
        ),
        type = "import",
        public_message = paste0(
          "The VEET archive `",
          basename(original_name),
          "` could not be unpacked into one device-export file."
        )
      )
    }

    # Preserve the filename fallback shown before import. For example,
    # `01_VEET_L.csv.zip` is unpacked to an internal `01_VEET_L.csv.csv`;
    # LightLogR removes the final `.csv` and therefore still returns the
    # previewed source key `01_VEET_L.csv`. The derived name is never recorded
    # in the durable source manifest.
    target <- file.path(
      derived_dir,
      paste0(
        raw_filename_stem(original_name),
        ".",
        payload$inner_extension
      )
    )
    extracted <- normalizePath(extracted[[1L]], winslash = "/", mustWork = TRUE)
    target <- normalizePath(target, winslash = "/", mustWork = FALSE)
    if (!identical(extracted, target)) {
      moved <- suppressWarnings(file.rename(extracted, target))
      if (!moved) {
        moved <- file.copy(
          extracted,
          target,
          overwrite = TRUE,
          copy.mode = TRUE
        )
        if (moved) {
          unlink(extracted, force = TRUE)
        }
      }
      if (!moved) {
        abort_llw(
          paste0(
            "Could not prepare the VEET payload from `",
            original_name,
            "`."
          ),
          type = "resource",
          public_message = paste0(
            "The VEET archive `",
            basename(original_name),
            "` could not be prepared in private session storage."
          )
        )
      }
    }
    target_info <- file.info(target)
    if (
      is.na(target_info$size) ||
        target_info$isdir ||
        !identical(as.numeric(target_info$size), payload$payload_bytes)
    ) {
      abort_llw(
        paste0(
          "The extracted VEET payload from `",
          original_name,
          "` failed its size check."
        ),
        type = "resource",
        public_message = paste0(
          "The unpacked contents of `",
          basename(original_name),
          "` failed an integrity check. Export it again and retry."
        )
      )
    }
    Sys.chmod(target, mode = "0600", use_umask = TRUE)
    import_filenames[[index]] <- target
  }

  request$import_arguments$filename <- import_filenames
  request
}

new_raw_import_request <- function(
  device,
  staged_files,
  timezone,
  not_before,
  options = character(),
  version = NULL,
  id_mode = c("automated", "manual", "extract"),
  manual_id = NULL,
  extract_pattern = NULL,
  veet_modality = NULL,
  max_bytes = raw_import_default_max_bytes(),
  progress_path = NULL
) {
  id_mode <- match.arg(id_mode)
  assert_scalar_string(device, "device")
  # The complete preflight below performs the byte-size and hash verification.
  # Validate only the in-memory shape here to avoid hashing large uploads twice
  # while constructing one request.
  staged_files <- validate_staged_uploads(staged_files, verify_files = FALSE)
  assert_character_vector(options, "options")
  if (!is.null(progress_path)) {
    assert_scalar_string(progress_path, "progress_path")
    progress_path <- normalizePath(
      progress_path,
      winslash = "/",
      mustWork = FALSE
    )
  }
  if (
    length(not_before) != 1L ||
      is.na(not_before) ||
      !inherits(not_before, c("Date", "POSIXt"))
  ) {
    abort_llw(
      "`not_before` must be one valid date or date-time.",
      type = "validation"
    )
  }
  id_mapping <- new_filename_id_mapping(
    original_names = staged_files$original_name,
    id_mode = id_mode,
    manual_id = manual_id,
    extract_pattern = extract_pattern
  )
  preflight <- preflight_raw_import_request(
    staged_files = staged_files,
    device = device,
    version = version,
    timezone = timezone,
    id_mapping = id_mapping,
    max_bytes = max_bytes
  )
  import_arguments <- list(
    device = device,
    filename = staged_files$staged_path,
    tz = timezone,
    not.before = not_before,
    dst_adjustment = "dst_jumps" %in% options,
    remove_duplicates = "remove_duplicates" %in% options,
    auto.plot = FALSE,
    version = preflight$version,
    print_n = Inf
  )
  if (identical(device, "VEET")) {
    assert_scalar_string(veet_modality, "veet_modality")
    import_arguments$modality <- veet_modality
  }
  if (identical(id_mode, "manual")) {
    import_arguments$manual.id <- unique(id_mapping$proposed_id)
  } else if (identical(id_mode, "extract")) {
    import_arguments$auto.id <- extract_pattern
  } else {
    # The private staged file keeps the original basename. LightLogR therefore
    # creates the same fallback ID that the app previews before import.
    import_arguments$auto.id <- ".*"
  }
  request <- structure(
    list(
      import_arguments = import_arguments,
      source_files = staged_files,
      id_mode = id_mode,
      id_mapping = id_mapping,
      preflight = preflight,
      max_bytes = max_bytes,
      progress_path = progress_path
    ),
    class = c("llw_raw_import_request", "list")
  )
  assert_serializable_value(request, "raw import request")
  request
}

raw_import_manifest_arguments <- function(request) {
  if (!inherits(request, "llw_raw_import_request")) {
    abort_llw(
      "`request` must be created by `new_raw_import_request()`.",
      type = "validation"
    )
  }
  arguments <- request$import_arguments
  arguments$filename <- request$source_files$original_name
  arguments$id_mode <- request$id_mode
  arguments$filename_to_id <- unclass(request$id_mapping[, c(
    "original_name",
    "proposed_id"
  )])
  arguments$manual.id <- NULL
  arguments$auto.id <- NULL
  arguments
}

normalize_raw_import_data <- function(
  data,
  request,
  structure_preview,
  import_warnings = character()
) {
  if (!is.data.frame(data) || nrow(data) == 0L) {
    abort_llw(
      "LightLogR returned no usable observations.",
      type = "import",
      public_message = "The import produced no usable observations. Review the files and settings, then retry.",
      diagnostics = list(phase = "normalization")
    )
  }
  duplicate_result <- "dupes" %in%
    names(data) &&
    any(grepl("duplicate rows", import_warnings, ignore.case = TRUE))
  if (duplicate_result) {
    abort_llw(
      "LightLogR stopped and returned only its identical-row diagnostic table.",
      type = "validation",
      public_message = paste(
        "Identical rows were found. Remove them in the source or explicitly",
        "enable duplicate removal before importing. No partial dataset was created."
      ),
      diagnostics = list(
        phase = "normalization",
        import_warnings = import_warnings
      )
    )
  }
  required <- c("Id", "Datetime", "file.name")
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    abort_llw(
      paste0(
        "LightLogR output is missing column(s): ",
        paste(missing, collapse = ", "),
        "."
      ),
      type = "import",
      public_message = paste0(
        "LightLogR could not produce required import column(s): ",
        paste(missing, collapse = ", "),
        ". Review the selected device and format version."
      ),
      diagnostics = list(phase = "normalization", missing_columns = missing)
    )
  }

  staged_keys <- raw_filename_stem(request$source_files$original_name)
  imported_keys <- as.character(data$file.name)
  source_index <- match(imported_keys, staged_keys)
  if (anyNA(source_index)) {
    unknown <- unique(imported_keys[is.na(source_index)])
    abort_llw(
      paste0(
        "LightLogR returned unknown staged file key(s): ",
        paste(unknown, collapse = ", "),
        "."
      ),
      type = "import",
      public_message = paste(
        "The imported rows could not be matched back to their original source files.",
        "No dataset was created; choose the files again and retry."
      ),
      diagnostics = list(phase = "normalization", unknown_file_keys = unknown)
    )
  }

  required_structure_columns <- c(
    "source_index",
    "original_name",
    "id_source"
  )
  if (
    !inherits(structure_preview, "llw_raw_import_structure") ||
      !is.data.frame(structure_preview$files) ||
      !all(required_structure_columns %in% names(structure_preview$files)) ||
      !identical(
        as.integer(structure_preview$files$source_index),
        seq_len(nrow(request$source_files))
      ) ||
      !identical(
        as.character(structure_preview$files$original_name),
        as.character(request$source_files$original_name)
      ) ||
      any(
        !structure_preview$files$id_source %in%
          c("embedded", "filename_mapping")
      )
  ) {
    abort_llw(
      "The structural import preview does not match the staged source files.",
      type = "import",
      public_message = paste(
        "The imported rows could not be reconciled with the participant-ID preflight.",
        "No dataset was created; review the files and mapping, then retry."
      ),
      diagnostics = list(phase = "normalization")
    )
  }

  # LightLogR is the authority for the final Id. It retains an embedded Id
  # column when present and otherwise applies manual.id/auto.id to the original
  # basename. Do not rewrite that result after import.
  data$Id <- factor(as.character(data$Id))
  data$file.name <- request$id_mapping$filename_stem[source_index]
  data <- data |>
    dplyr::group_by(Id) |>
    dplyr::arrange(Datetime, .by_group = TRUE)
  data
}

raw_import_phase_row <- function(phase, elapsed_seconds, detail) {
  data.frame(
    phase = phase,
    status = "complete",
    elapsed_seconds = round(elapsed_seconds, 3),
    detail = detail,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

raw_import_progress_snapshot <- function(
  completed_phases,
  active_phase = NULL,
  active_detail = NULL
) {
  completed <- if (length(completed_phases) == 0L) {
    data.frame(
      phase = character(),
      status = character(),
      elapsed_seconds = numeric(),
      detail = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, completed_phases)
  }
  if (is.null(active_phase)) {
    return(completed)
  }
  rbind(
    completed,
    data.frame(
      phase = active_phase,
      status = "running",
      elapsed_seconds = NA_real_,
      detail = active_detail,
      stringsAsFactors = FALSE
    )
  )
}

write_raw_import_progress <- function(
  request,
  completed_phases,
  active_phase = NULL,
  active_detail = NULL
) {
  path <- request$progress_path %||% NULL
  if (is.null(path) || !dir.exists(dirname(path))) {
    return(invisible(FALSE))
  }
  progress <- raw_import_progress_snapshot(
    completed_phases,
    active_phase = active_phase,
    active_detail = active_detail
  )
  written <- tryCatch(
    {
      temporary <- tempfile("raw-import-progress-", tmpdir = dirname(path))
      on.exit(unlink(temporary, force = TRUE), add = TRUE)
      saveRDS(progress, temporary, version = 2)
      if (!file.rename(temporary, path)) {
        file.copy(temporary, path, overwrite = TRUE)
      } else {
        TRUE
      }
    },
    error = function(cnd) FALSE
  )
  invisible(isTRUE(written))
}

read_raw_import_progress <- function(path) {
  if (is.null(path) || !file.exists(path)) {
    return(NULL)
  }
  progress <- tryCatch(readRDS(path), error = function(cnd) NULL)
  required <- c("phase", "status", "elapsed_seconds", "detail")
  if (!is.data.frame(progress) || !all(required %in% names(progress))) {
    return(NULL)
  }
  progress
}

run_raw_import_pipeline <- function(request) {
  if (!inherits(request, "llw_raw_import_request")) {
    abort_llw(
      "`request` must be created by `new_raw_import_request()`.",
      type = "validation"
    )
  }
  phases <- list()

  write_raw_import_progress(
    request,
    phases,
    active_phase = "validation",
    active_detail = "Checking file integrity, formats, and import settings."
  )
  started <- proc.time()[["elapsed"]]
  preflight <- preflight_raw_import_request(
    staged_files = request$source_files,
    device = request$import_arguments$device,
    version = request$import_arguments$version,
    timezone = request$import_arguments$tz,
    id_mapping = request$id_mapping,
    max_bytes = request$max_bytes
  )
  worker_request <- prepare_raw_import_files(request)
  structure_preview <- preflight_raw_import_structure(worker_request)
  phases[[length(phases) + 1L]] <- raw_import_phase_row(
    "validation",
    proc.time()[["elapsed"]] - started,
    paste(
      nrow(preflight$files),
      "file(s) passed byte, mapping, format, and structural checks."
    )
  )

  write_raw_import_progress(
    request,
    phases,
    active_phase = "import",
    active_detail = "LightLogR is reading the private copies of the selected files."
  )
  started <- proc.time()[["elapsed"]]
  imported <- capture_lightlogr_import(
    worker_request$import_arguments,
    paste(basename(request$source_files$original_name), collapse = ", ")
  )
  phases[[length(phases) + 1L]] <- raw_import_phase_row(
    "import",
    proc.time()[["elapsed"]] - started,
    paste(
      format(nrow(imported$data), big.mark = ","),
      "row(s) returned by LightLogR."
    )
  )

  write_raw_import_progress(
    request,
    phases,
    active_phase = "normalization",
    active_detail = paste(
      "Verifying source filenames and participant IDs produced by LightLogR."
    )
  )
  started <- proc.time()[["elapsed"]]
  normalized <- normalize_raw_import_data(
    imported$data,
    worker_request,
    import_warnings = imported$warnings,
    structure_preview = structure_preview
  )
  validate_imported_light_data(
    normalized,
    worker_request$import_arguments$tz
  )
  phases[[length(phases) + 1L]] <- raw_import_phase_row(
    "normalization",
    proc.time()[["elapsed"]] - started,
    paste(
      "Original source labels were verified; participant IDs produced by",
      "LightLogR were kept, and sensor values were not altered."
    )
  )

  write_raw_import_progress(
    request,
    phases,
    active_phase = "quality",
    active_detail = "Checking all rows for time-series and missing-data problems."
  )
  started <- proc.time()[["elapsed"]]
  quality <- summarize_raw_import_quality(
    normalized,
    source_timezone = worker_request$import_arguments$tz
  )
  phases[[length(phases) + 1L]] <- raw_import_phase_row(
    "quality",
    proc.time()[["elapsed"]] - started,
    paste(
      quality$summary$participants,
      "participant(s);",
      quality$summary$implicit_gap_epochs,
      "implicit gap epoch(s);",
      quality$summary$irregular_observations,
      "irregular observation(s)."
    )
  )

  write_raw_import_progress(
    request,
    phases,
    active_phase = "preview",
    active_detail = "Preparing summaries and a small preview of the imported rows."
  )
  started <- proc.time()[["elapsed"]]
  preview <- raw_import_preview_indices(nrow(normalized), max_rows = 100L)
  phases[[length(phases) + 1L]] <- raw_import_phase_row(
    "preview",
    proc.time()[["elapsed"]] - started,
    preview$notice
  )
  write_raw_import_progress(request, phases)

  warnings <- unique(c(
    preflight$warnings,
    structure_preview$warnings,
    imported$warnings,
    quality$warnings
  ))
  warnings <- warnings[nzchar(warnings)]
  result <- structure(
    list(
      data = normalized,
      msg = imported$output,
      preflight = preflight,
      structure = structure_preview,
      quality = quality,
      eligibility = quality$eligibility,
      preview = preview,
      phases = do.call(rbind, phases),
      warnings = warnings
    ),
    class = c("llw_raw_import_result", "list")
  )
  assert_serializable_value(result, "raw import result")
  if (length(warnings) > 0L) {
    for (message in warnings) {
      warning(message, call. = FALSE)
    }
  }
  result
}

raw_import_worker <- function(payload, spec) {
  tryCatch(
    run_raw_import_pipeline(payload),
    error = function(cnd) {
      if (inherits(cnd, "llw_error")) stop(cnd)
      abort_llw(
        conditionMessage(cnd),
        type = "import",
        public_message = "Import failed. Review the files and settings, then retry.",
        diagnostics = list(
          phase = "import",
          classes = class(cnd),
          original_message = conditionMessage(cnd)
        ),
        parent = cnd
      )
    }
  )
}

import_data <- function(request) {
  if (!inherits(request, "llw_raw_import_request")) {
    abort_llw(
      "`request` must be created by `new_raw_import_request()`.",
      type = "validation"
    )
  }
  tryCatch(
    raw_import_worker(request, spec = NULL),
    error = function(cnd) {
      if (inherits(cnd, "llw_error")) stop(cnd)
      abort_llw(
        conditionMessage(cnd),
        type = "import",
        public_message = "Import failed. Review the files and settings, then retry.",
        diagnostics = list(
          classes = class(cnd),
          original_message = conditionMessage(cnd)
        ),
        parent = cnd
      )
    }
  )
}
