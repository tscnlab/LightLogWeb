new_stable_id <- function(prefix) {
  if (
    !is.character(prefix) ||
      length(prefix) != 1L ||
      is.na(prefix) ||
      !grepl("^[a-z][a-z0-9_]*$", prefix)
  ) {
    stop(
      "`prefix` must contain lowercase letters, numbers, and underscores and start with a letter.",
      call. = FALSE
    )
  }
  suffix <- gsub("[^A-Za-z0-9]", "", basename(tempfile("id")))
  paste0(prefix, "_", tolower(suffix))
}

assert_scalar_string <- function(x, arg, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    abort_llw(
      paste0("`", arg, "` must be one non-missing string."),
      type = "validation"
    )
  }
  if (!allow_empty && !nzchar(x)) {
    abort_llw(
      paste0("`", arg, "` must not be empty."),
      type = "validation"
    )
  }
  invisible(x)
}

assert_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    abort_llw(
      paste0("`", arg, "` must be `TRUE` or `FALSE`."),
      type = "validation"
    )
  }
  invisible(x)
}

assert_character_vector <- function(x, arg, allow_empty = TRUE) {
  if (!is.character(x) || anyNA(x) || any(!nzchar(x))) {
    abort_llw(
      paste0(
        "`",
        arg,
        "` must contain non-empty strings without missing values."
      ),
      type = "validation"
    )
  }
  if (!allow_empty && length(x) == 0L) {
    abort_llw(
      paste0("`", arg, "` must contain at least one value."),
      type = "validation"
    )
  }
  invisible(x)
}

sha256_raw <- function(x) {
  if (!is.raw(x)) {
    abort_llw("`x` must be a raw vector.", type = "validation")
  }
  paste0(
    "sha256:",
    digest::digest(x, algo = "sha256", serialize = FALSE)
  )
}

sha256_file <- function(path) {
  assert_scalar_string(path, "path")
  if (!file.exists(path) || dir.exists(path)) {
    abort_llw(
      paste0("Cannot fingerprint missing file `", basename(path), "`."),
      type = "resource"
    )
  }
  paste0(
    "sha256:",
    digest::digest(file = path, algo = "sha256", serialize = FALSE)
  )
}

nonserializable_path <- function(x, path = "value") {
  if (inherits(x, "glc_package")) {
    return(paste0(path, " (live `glc_package` handle)"))
  }
  if (
    is.environment(x) ||
      is.function(x) ||
      typeof(x) %in% c("externalptr", "weakref") ||
      inherits(x, "connection") ||
      is.language(x)
  ) {
    return(paste0(path, " (", typeof(x), ")"))
  }
  if (is.list(x)) {
    labels <- names(x)
    if (is.null(labels)) {
      labels <- as.character(seq_along(x))
    }
    for (index in seq_along(x)) {
      child <- nonserializable_path(
        x[[index]],
        paste0(path, "[[", labels[[index]], "]] ")
      )
      if (!is.null(child)) {
        return(trimws(child))
      }
    }
  }
  NULL
}

assert_serializable_value <- function(x, arg = "value") {
  problem <- nonserializable_path(x, path = arg)
  if (!is.null(problem)) {
    abort_llw(
      paste0("`", arg, "` is not worker-serializable at ", problem, "."),
      type = "validation",
      public_message = paste(
        "This operation contains session-only state that cannot be sent to a",
        "background worker. Recreate the request from serializable selections."
      )
    )
  }
  serialized <- tryCatch(
    serialize(x, connection = NULL, version = 3),
    error = identity
  )
  if (inherits(serialized, "error")) {
    abort_llw(
      paste0(
        "`",
        arg,
        "` could not be serialized: ",
        conditionMessage(serialized)
      ),
      type = "validation",
      parent = serialized
    )
  }
  invisible(x)
}
