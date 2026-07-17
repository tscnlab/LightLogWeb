resolve_runtime_profile <- function(
  profile = c("auto", "hosted", "local"),
  max_upload_mb = 200,
  workers = NULL,
  env_profile = Sys.getenv("LIGHTLOGWEB_PROFILE", unset = ""),
  is_interactive = interactive(),
  available_cores = parallel::detectCores(logical = FALSE)
) {
  profile <- match.arg(profile)
  if (
    !is.numeric(max_upload_mb) ||
      length(max_upload_mb) != 1L ||
      is.na(max_upload_mb) ||
      !is.finite(max_upload_mb) ||
      max_upload_mb <= 0
  ) {
    abort_llw(
      "`max_upload_mb` must be one positive finite number.",
      type = "validation"
    )
  }
  if (!is.logical(is_interactive) || length(is_interactive) != 1L) {
    abort_llw(
      "`is_interactive` must be one logical value.",
      type = "validation"
    )
  }
  if (
    !is.numeric(available_cores) ||
      length(available_cores) != 1L ||
      is.na(available_cores) ||
      available_cores < 1
  ) {
    available_cores <- 1L
  }
  available_cores <- as.integer(floor(available_cores))

  if (identical(profile, "auto")) {
    if (nzchar(env_profile)) {
      if (!env_profile %in% c("hosted", "local")) {
        abort_llw(
          "`LIGHTLOGWEB_PROFILE` must be `hosted` or `local`.",
          type = "validation"
        )
      }
      profile <- env_profile
    } else {
      profile <- if (isTRUE(is_interactive)) "local" else "hosted"
    }
  }

  worker_cap <- if (identical(profile, "hosted")) {
    1L
  } else {
    min(2L, available_cores)
  }
  if (is.null(workers)) {
    resolved_workers <- worker_cap
  } else {
    if (
      !is.numeric(workers) ||
        length(workers) != 1L ||
        is.na(workers) ||
        !is.finite(workers) ||
        workers < 0 ||
        workers != floor(workers)
    ) {
      abort_llw(
        "`workers` must be `NULL` or one non-negative whole number.",
        type = "validation"
      )
    }
    resolved_workers <- min(as.integer(workers), worker_cap)
  }

  hosted <- identical(profile, "hosted")
  structure(
    list(
      name = profile,
      max_upload_bytes = as.numeric(max_upload_mb) * 1024^2,
      workers = resolved_workers,
      max_concurrent_tasks = if (hosted) {
        1L
      } else {
        max(1L, resolved_workers)
      },
      cache_max_bytes = if (hosted) 256 * 1024^2 else 1024 * 1024^2,
      session_temp_max_bytes = if (hosted) 1024 * 1024^2 else 4096 * 1024^2,
      task_timeout_ms = if (hosted) 15 * 60 * 1000L else 60 * 60 * 1000L,
      synchronous = identical(resolved_workers, 0L)
    ),
    class = c("llw_runtime_profile", "list")
  )
}

create_session_paths <- function(root = tempfile("lightlogweb-session-")) {
  assert_scalar_string(root, "root")
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  if (file.exists(root) || dir.exists(root)) {
    abort_llw(
      paste0("Session root `", root, "` already exists."),
      type = "resource",
      public_message = paste(
        "A private session directory could not be initialized safely.",
        "Start a new session and retry."
      )
    )
  }
  created_root <- dir.create(root, recursive = TRUE, showWarnings = FALSE)
  if (!created_root || !dir.exists(root)) {
    abort_llw(
      paste0("Could not create session directory `", root, "`."),
      type = "resource"
    )
  }
  initialized <- FALSE
  on.exit(
    {
      if (!initialized) {
        unlink(root, recursive = TRUE, force = TRUE)
      }
    },
    add = TRUE
  )
  paths <- list(
    root = root,
    uploads = file.path(root, "uploads"),
    cache = file.path(root, "cache"),
    tasks = file.path(root, "tasks")
  )
  created <- vapply(
    paths,
    dir.create,
    logical(1),
    recursive = TRUE,
    showWarnings = FALSE
  )
  exists <- vapply(paths, dir.exists, logical(1))
  if (!all(created | exists)) {
    abort_llw(
      paste0("Could not create session directory `", root, "`."),
      type = "resource"
    )
  }
  marker <- file.path(root, ".lightlogweb-session")
  writeLines("LightLogWeb session directory", marker, useBytes = TRUE)
  if (!file.exists(marker)) {
    abort_llw(
      "The LightLogWeb session marker could not be created.",
      type = "resource"
    )
  }
  initialized <- TRUE
  structure(
    c(paths, list(marker = marker)),
    class = c("llw_session_paths", "list")
  )
}

cleanup_session_paths <- function(paths) {
  if (
    !inherits(paths, "llw_session_paths") ||
      !is.list(paths) ||
      !is.character(paths$root) ||
      !is.character(paths$marker)
  ) {
    abort_llw(
      "Cleanup requires paths created by `create_session_paths()`.",
      type = "validation"
    )
  }
  if (!dir.exists(paths$root)) {
    return(invisible(TRUE))
  }
  if (!file.exists(paths$marker)) {
    abort_llw(
      "Refusing to remove a directory without a LightLogWeb session marker.",
      type = "resource"
    )
  }
  unlink(paths$root, recursive = TRUE, force = TRUE)
  invisible(!dir.exists(paths$root))
}

directory_size <- function(path) {
  assert_scalar_string(path, "path")
  if (!dir.exists(path)) {
    return(0)
  }
  entries <- list.files(
    path,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    no.. = TRUE
  )
  if (length(entries) == 0L) {
    return(0)
  }
  info <- file.info(entries)
  sum(info$size[!info$isdir], na.rm = TRUE)
}

assert_cache_key <- function(key) {
  assert_scalar_string(key, "key")
  if (
    !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", key) ||
      grepl("\\.\\.", key, fixed = FALSE)
  ) {
    abort_llw(
      "Cache keys may contain letters, numbers, dots, underscores, and hyphens without `..`.",
      type = "validation"
    )
  }
  invisible(key)
}

new_session_cache <- function(path, max_bytes) {
  assert_scalar_string(path, "path")
  if (
    !is.numeric(max_bytes) ||
      length(max_bytes) != 1L ||
      is.na(max_bytes) ||
      !is.finite(max_bytes) ||
      max_bytes < 1
  ) {
    abort_llw(
      "`max_bytes` must be one positive finite number.",
      type = "validation"
    )
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  files <- function() {
    list.files(path, pattern = "\\.rds$", full.names = TRUE)
  }
  size <- function() {
    current <- files()
    if (length(current) == 0L) {
      return(0)
    }
    sum(file.info(current)$size, na.rm = TRUE)
  }
  target <- function(key) {
    assert_cache_key(key)
    file.path(path, paste0(key, ".rds"))
  }
  prune_for <- function(required_bytes, exclude = character()) {
    current <- setdiff(files(), exclude)
    if (length(current) == 0L) {
      return(invisible(NULL))
    }
    info <- file.info(current)
    order_oldest <- order(info$mtime, na.last = TRUE)
    for (index in order_oldest) {
      if (size() + required_bytes <= max_bytes) {
        break
      }
      unlink(current[[index]], force = TRUE)
    }
    invisible(NULL)
  }

  set <- function(key, value) {
    assert_serializable_value(value, "cache value")
    destination <- target(key)
    temporary <- tempfile(".cache-", tmpdir = path, fileext = ".tmp")
    on.exit(unlink(temporary, force = TRUE), add = TRUE)
    saveRDS(value, temporary, version = 3, compress = FALSE)
    item_bytes <- unname(file.info(temporary)$size)
    if (is.na(item_bytes) || item_bytes > max_bytes) {
      abort_llw(
        paste0(
          "Cache item `",
          key,
          "` needs ",
          item_bytes,
          " bytes; the session cache limit is ",
          max_bytes,
          " bytes."
        ),
        type = "resource",
        public_message = "This result is too large for the session cache. Reduce the selection and retry."
      )
    }
    if (file.exists(destination)) {
      unlink(destination, force = TRUE)
    }
    prune_for(item_bytes, exclude = destination)
    if (!file.rename(temporary, destination)) {
      abort_llw(
        paste0("Could not finalize cache item `", key, "`."),
        type = "resource"
      )
    }
    invisible(value)
  }
  get <- function(key, default = NULL) {
    destination <- target(key)
    if (!file.exists(destination)) {
      return(default)
    }
    Sys.setFileTime(destination, Sys.time())
    readRDS(destination)
  }
  remove <- function(key) {
    unlink(target(key), force = TRUE)
    invisible(NULL)
  }
  clear <- function() {
    unlink(files(), force = TRUE)
    invisible(NULL)
  }

  list(
    path = path,
    max_bytes = max_bytes,
    set = set,
    get = get,
    remove = remove,
    clear = clear,
    size = size,
    keys = function() tools::file_path_sans_ext(basename(files()))
  )
}

new_session_runtime <- function(
  profile,
  session = shiny::getDefaultReactiveDomain(),
  paths = create_session_paths(),
  start_workers = TRUE,
  daemon_start = mirai::daemons,
  daemon_stop = mirai::daemons
) {
  if (!inherits(profile, "llw_runtime_profile")) {
    abort_llw(
      "`profile` must be created by `resolve_runtime_profile()`.",
      type = "validation"
    )
  }
  assert_flag(start_workers, "start_workers")
  if (!is.function(daemon_start) || !is.function(daemon_stop)) {
    abort_llw(
      "`daemon_start` and `daemon_stop` must be functions.",
      type = "validation"
    )
  }
  cache <- new_session_cache(paths$cache, profile$cache_max_bytes)
  compute_profile <- new_stable_id("compute")
  active_ids <- character()
  queue <- list()
  cancel_handlers <- list()
  closed <- FALSE
  workers_started <- FALSE
  startup_error <- NULL

  if (profile$workers > 0L && start_workers) {
    worker_start <- tryCatch(
      {
        daemon_start(
          profile$workers,
          dispatcher = TRUE,
          .compute = compute_profile
        )
        TRUE
      },
      error = identity
    )
    if (inherits(worker_start, "error")) {
      startup_error <- new_llw_error(
        message = conditionMessage(worker_start),
        type = "resource",
        public_message = paste(
          "Background workers could not be started.",
          "This session is using the synchronous fallback."
        ),
        diagnostics = list(
          classes = class(worker_start),
          original_message = conditionMessage(worker_start)
        ),
        parent = worker_start
      )
      profile$workers <- 0L
      profile$max_concurrent_tasks <- 1L
      profile$synchronous <- TRUE
    } else {
      workers_started <- TRUE
    }
  }

  notify_start_error <- function(entry, condition) {
    try(entry$on_error(condition), silent = TRUE)
    invisible(NULL)
  }
  start_entry <- function(id, entry) {
    active_ids <<- c(active_ids, id)
    started <- tryCatch(
      {
        entry$start()
        TRUE
      },
      error = identity
    )
    if (inherits(started, "error")) {
      active_ids <<- setdiff(active_ids, id)
      cancel_handlers[[id]] <<- NULL
      notify_start_error(entry, started)
      return(FALSE)
    }
    TRUE
  }
  start_next <- function() {
    started_ids <- character()
    while (
      !closed &&
        length(queue) > 0L &&
        length(active_ids) < profile$max_concurrent_tasks
    ) {
      id <- names(queue)[[1L]]
      entry <- queue[[id]]
      queue[[id]] <<- NULL
      if (start_entry(id, entry)) {
        started_ids <- c(started_ids, id)
      }
    }
    invisible(started_ids)
  }
  submit <- function(
    id,
    start,
    on_error = function(condition) invisible(NULL)
  ) {
    assert_scalar_string(id, "id")
    if (!is.function(start) || !is.function(on_error)) {
      abort_llw(
        "`start` and `on_error` must be functions.",
        type = "validation"
      )
    }
    if (closed) {
      abort_llw(
        "The session runtime is already closed.",
        type = "resource"
      )
    }
    if (id %in% c(active_ids, names(queue))) {
      abort_llw(
        paste0("Task `", id, "` is already active or queued."),
        type = "validation"
      )
    }
    entry <- list(start = start, on_error = on_error)
    if (length(active_ids) < profile$max_concurrent_tasks) {
      return(if (start_entry(id, entry)) "running" else "error")
    }
    queue[[id]] <<- entry
    "queued"
  }
  release <- function(id) {
    active_ids <<- setdiff(active_ids, id)
    cancel_handlers[[id]] <<- NULL
    if (!closed) {
      start_next()
    }
    invisible(NULL)
  }
  cancel_queued <- function(id) {
    if (!id %in% names(queue)) {
      return(FALSE)
    }
    queue[[id]] <<- NULL
    cancel_handlers[[id]] <<- NULL
    TRUE
  }
  register_cancel <- function(id, handler) {
    assert_scalar_string(id, "id")
    if (!is.function(handler)) {
      abort_llw("`handler` must be a function.", type = "validation")
    }
    cancel_handlers[[id]] <<- handler
    invisible(NULL)
  }
  cleanup <- function() {
    if (closed) {
      return(invisible(TRUE))
    }
    closed <<- TRUE
    handlers <- cancel_handlers
    queue <<- list()
    for (handler in handlers) {
      try(handler(), silent = TRUE)
    }
    active_ids <<- character()
    cancel_handlers <<- list()
    if (workers_started) {
      try(
        daemon_stop(0L, .compute = compute_profile),
        silent = TRUE
      )
      workers_started <<- FALSE
    }
    cleanup_session_paths(paths)
    invisible(TRUE)
  }

  if (!is.null(session) && is.function(session$onSessionEnded)) {
    session$onSessionEnded(cleanup)
  }

  assert_capacity <- function(additional_bytes = 0) {
    if (
      !is.numeric(additional_bytes) ||
        length(additional_bytes) != 1L ||
        is.na(additional_bytes) ||
        !is.finite(additional_bytes) ||
        additional_bytes < 0
    ) {
      abort_llw(
        "`additional_bytes` must be one non-negative finite number.",
        type = "validation"
      )
    }
    used <- directory_size(paths$root)
    if (used + additional_bytes > profile$session_temp_max_bytes) {
      abort_llw(
        paste0(
          "The session needs ",
          used + additional_bytes,
          " bytes but its temporary-storage limit is ",
          profile$session_temp_max_bytes,
          " bytes."
        ),
        type = "resource",
        public_message = paste(
          "This operation would exceed the session storage limit.",
          "Reduce the selection or remove another session artifact."
        )
      )
    }
    invisible(profile$session_temp_max_bytes - used - additional_bytes)
  }
  stage_uploads <- function(files) {
    if (!is.data.frame(files) || !"datapath" %in% names(files)) {
      abort_llw("Uploaded file metadata are invalid.", type = "validation")
    }
    info <- file.info(as.character(files$datapath))
    incoming <- sum(info$size, na.rm = FALSE)
    if (length(incoming) != 1L || is.na(incoming)) {
      abort_llw(
        "One or more uploaded files are unavailable.",
        type = "resource"
      )
    }
    if (incoming > profile$max_upload_bytes) {
      abort_llw(
        "The upload exceeds the configured request limit.",
        type = "resource",
        public_message = paste0(
          "The selected files exceed this app's ",
          format(round(profile$max_upload_bytes / 1024^2, 1), trim = TRUE),
          " MB upload limit."
        )
      )
    }
    assert_capacity(incoming)
    staged <- stage_import_files(files, paths$uploads)
    capacity <- tryCatch(assert_capacity(), error = identity)
    if (inherits(capacity, "error")) {
      unlink(
        unique(dirname(staged$staged_path)),
        recursive = TRUE,
        force = TRUE
      )
      stop(capacity)
    }
    staged
  }

  list(
    profile = profile,
    paths = paths,
    cache = cache,
    compute_profile = compute_profile,
    startup_error = startup_error,
    submit = submit,
    release = release,
    cancel_queued = cancel_queued,
    register_cancel = register_cancel,
    active_count = function() length(active_ids),
    queued_count = function() length(queue),
    session_size = function() directory_size(paths$root),
    assert_capacity = assert_capacity,
    stage_uploads = stage_uploads,
    cleanup = cleanup
  )
}
