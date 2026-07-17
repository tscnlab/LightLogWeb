stage_import_files <- function(files, upload_root) {
  if (!is.data.frame(files) || !all(c("name", "datapath") %in% names(files))) {
    abort_llw(
      "Uploaded files must provide `name` and `datapath` columns.",
      type = "validation"
    )
  }
  assert_scalar_string(upload_root, "upload_root")
  if (!dir.exists(upload_root)) {
    created <- dir.create(upload_root, recursive = TRUE, showWarnings = FALSE)
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
  original_names <- as.character(files$name)
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
  created <- dir.create(task_dir, recursive = TRUE, showWarnings = FALSE)
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
  extensions <- tolower(tools::file_ext(basename(original_names)))
  extensions[!grepl("^[a-z0-9]{1,12}$", extensions)] <- ""
  storage_names <- sprintf(
    "%04d%s",
    seq_along(original_names),
    ifelse(nzchar(extensions), paste0(".", extensions), "")
  )
  destinations <- file.path(task_dir, storage_names)
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

new_raw_import_request <- function(
  device,
  staged_files,
  timezone,
  not_before,
  options = character(),
  version = NULL,
  id_mode = c("automated", "manual", "extract"),
  id_preview = character(),
  manual_id = NULL,
  extract_pattern = NULL,
  veet_modality = NULL
) {
  id_mode <- match.arg(id_mode)
  assert_scalar_string(device, "device")
  staged_files <- validate_staged_uploads(staged_files)
  assert_scalar_string(timezone, "timezone")
  if (!timezone %in% OlsonNames()) {
    abort_llw(
      paste0("Unknown IANA time zone `", timezone, "`."),
      type = "validation"
    )
  }
  assert_character_vector(options, "options")
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
  if (is.character(version) && length(version) == 1L && !nzchar(version)) {
    version <- NULL
  }
  import_arguments <- list(
    device = device,
    filename = staged_files$staged_path,
    tz = timezone,
    not.before = not_before,
    dst_adjustment = "dst_jumps" %in% options,
    remove_duplicates = "remove_duplicates" %in% options,
    auto.plot = FALSE,
    version = version,
    print_n = Inf
  )
  if (identical(device, "VEET")) {
    assert_scalar_string(veet_modality, "veet_modality")
    import_arguments$modality <- veet_modality
  }
  if (identical(id_mode, "manual")) {
    assert_scalar_string(manual_id, "manual_id")
    import_arguments$manual.id <- manual_id
  } else if (identical(id_mode, "extract")) {
    assert_scalar_string(extract_pattern, "extract_pattern")
    import_arguments$auto.id <- extract_pattern
  } else {
    assert_character_vector(id_preview, "id_preview", allow_empty = FALSE)
    prefix_length <- nchar(id_preview[[1L]])
    import_arguments$auto.id <- paste0(".{", prefix_length, "}")
  }
  request <- structure(
    list(
      import_arguments = import_arguments,
      source_files = staged_files
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
  arguments
}

raw_import_worker <- local(
  function(payload, spec) {
    imported_data <- NULL
    import_message <- utils::capture.output({
      imported_data <- do.call(
        LightLogR::import_Dataset,
        payload$import_arguments
      )
    })
    if (length(import_message) > 0L && startsWith(import_message[[1L]], "\r")) {
      import_message <- import_message[-1L]
    }
    list(data = imported_data, msg = import_message)
  },
  envir = baseenv()
)

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
