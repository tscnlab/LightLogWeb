test_that("runtime profiles bound workers and hosted concurrency", {
  hosted <- resolve_runtime_profile(
    "auto",
    workers = 8,
    env_profile = "hosted",
    is_interactive = TRUE,
    available_cores = 8
  )
  local <- resolve_runtime_profile(
    "local",
    workers = NULL,
    is_interactive = TRUE,
    available_cores = 8
  )
  sync <- resolve_runtime_profile("hosted", workers = 0)

  expect_identical(hosted$name, "hosted")
  expect_identical(hosted$workers, 1L)
  expect_identical(hosted$max_concurrent_tasks, 1L)
  expect_identical(local$workers, 2L)
  expect_identical(local$max_concurrent_tasks, 2L)
  expect_true(sync$synchronous)
  expect_equal(hosted$max_upload_bytes, 200 * 1024^2)
  expect_error(
    resolve_runtime_profile("hosted", max_upload_mb = 200.01, workers = 0),
    class = "llw_validation_error",
    regexp = "Hosted.*cannot exceed"
  )
  expect_equal(
    resolve_runtime_profile(
      "local",
      max_upload_mb = 512,
      workers = 0
    )$max_upload_bytes,
    512 * 1024^2
  )
})

test_that("session caches evict old values and reject oversized items", {
  path <- tempfile("llw-cache-test-")
  cache <- new_session_cache(path, max_bytes = 2500)
  on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

  cache$set("first", as.raw(rep(1L, 1400L)))
  Sys.sleep(0.01)
  cache$set("second", as.raw(rep(2L, 1400L)))

  expect_lte(cache$size(), cache$max_bytes)
  expect_true("second" %in% cache$keys())
  expect_error(
    cache$set("too-large", as.raw(rep(3L, 3000L))),
    class = "llw_resource_error",
    regexp = "needs"
  )
})

test_that("the runtime queues above its profile limit and cleans all temp data", {
  paths <- create_session_paths()
  if (.Platform$OS.type != "windows") {
    expect_identical(as.integer(file.info(paths$root)$mode), 448L)
    expect_true(all(vapply(
      paths[c("uploads", "cache", "tasks")],
      function(path) as.integer(file.info(path)$mode) == 448L,
      logical(1)
    )))
  }
  profile <- resolve_runtime_profile("hosted", workers = 0)
  runtime <- new_session_runtime(
    profile,
    session = NULL,
    paths = paths,
    start_workers = FALSE
  )
  events <- character()

  expect_identical(
    runtime$submit("first", function() events <<- c(events, "first")),
    "running"
  )
  expect_identical(
    runtime$submit("second", function() events <<- c(events, "second")),
    "queued"
  )
  expect_identical(events, "first")
  expect_equal(runtime$active_count(), 1L)
  expect_equal(runtime$queued_count(), 1L)

  runtime$release("first")
  expect_identical(events, c("first", "second"))
  expect_equal(runtime$active_count(), 1L)
  runtime$release("second")
  expect_equal(runtime$active_count(), 0L)

  root <- paths$root
  writeLines("temporary", file.path(paths$uploads, "source.txt"))
  expect_true(dir.exists(root))
  runtime$cleanup()
  expect_false(dir.exists(root))
  expect_true(runtime$cleanup())
})

test_that("runtime start failures are contained and leave capacity reusable", {
  paths <- create_session_paths()
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 0),
    session = NULL,
    paths = paths,
    start_workers = FALSE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  captured <- NULL

  state <- runtime$submit(
    "broken",
    function() stop("private start failure", call. = FALSE),
    on_error = function(cnd) captured <<- cnd
  )
  expect_identical(state, "error")
  expect_equal(runtime$active_count(), 0L)
  expect_s3_class(captured, "error")

  expect_identical(
    runtime$submit("next", function() invisible(NULL)),
    "running"
  )
  runtime$release("next")
  expect_equal(runtime$active_count(), 0L)
})

test_that("worker startup falls back synchronously without losing the session", {
  paths <- create_session_paths()
  runtime <- new_session_runtime(
    resolve_runtime_profile("hosted", workers = 1),
    session = NULL,
    paths = paths,
    daemon_start = function(...) stop("daemon unavailable", call. = FALSE)
  )
  on.exit(runtime$cleanup(), add = TRUE)

  expect_true(runtime$profile$synchronous)
  expect_identical(runtime$profile$workers, 0L)
  expect_s3_class(runtime$startup_error, "llw_resource_error")
  expect_match(
    llw_public_message(runtime$startup_error),
    "synchronous fallback"
  )
})

test_that("ending a Shiny session removes its complete temporary tree", {
  root <- NULL
  server <- function(input, output, session) {
    runtime <- new_session_runtime(
      resolve_runtime_profile("hosted", workers = 0),
      session = session
    )
    root <<- runtime$paths$root
    writeLines("session-only", file.path(runtime$paths$uploads, "source.txt"))
  }

  shiny::testServer(server, {
    expect_true(dir.exists(root))
  })
  expect_false(dir.exists(root))
})

test_that("session initialization never takes ownership of an existing directory", {
  root <- tempfile("llw-existing-root-")
  dir.create(root, recursive = TRUE)
  sentinel <- file.path(root, "keep-me.txt")
  writeLines("owner data", sentinel)
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)

  expect_error(
    create_session_paths(root),
    class = "llw_resource_error",
    regexp = "already exists"
  )
  expect_true(file.exists(sentinel))
  expect_false(file.exists(file.path(root, ".lightlogweb-session")))
})
