# Run from the project root with its renv library. All numerical verification
# is in R. Large preparation is timed separately from ordinary module controls.
source("dev/milestone-8-prepare-app.R")
output_path <- Sys.getenv(
  "LLW_M8_BENCHMARK",
  "dev/milestone-8-preparation-benchmark.json"
)
measure <- function(code, repeats = 40L) {
  timings <- numeric(repeats)
  for (i in seq_len(repeats)) {
    elapsed <- system.time(force(code(i)))[["elapsed"]]
    timings[[i]] <- elapsed * 1000
  }
  list(
    p95_ms = unname(stats::quantile(timings, 0.95)),
    maximum_ms = max(timings),
    repetitions = repeats
  )
}

run_case <- function(n, profile) {
  record <- m8_scale_record(n)
  raw <- unserialize(record$raw_payload)
  context <- preparation_context(record, raw)
  recipe <- new_recipe(list(new_preparation_step(
    "clock",
    list(
      start = "22:00:00",
      end = "06:00:00",
      timezone = "UTC",
      anchor = "start_date"
    ),
    context
  )))
  budget <- resolve_runtime_profile(profile, workers = 0L)$cache_max_bytes
  prepared <- NULL
  gc(reset = TRUE)
  log <- tempfile("m8-allocation-")
  utils::Rprofmem(log)
  elapsed <- system.time({
    prepared <- preparation_task_worker(list(
      action = "preview",
      record = record,
      recipe = recipe,
      budget = budget
    ))
  })[["elapsed"]]
  utils::Rprofmem(NULL)
  peak <- gc()
  allocation <- readLines(log, warn = FALSE)
  unlink(log)
  allocations <- suppressWarnings(as.numeric(sub(" .*", "", allocation)))
  # Independent direct public-R reference, starting from immutable raw.
  expected <- raw |>
    dplyr::filter(
      format(Datetime, "%H:%M:%S", tz = "UTC") >= "22:00:00" |
        format(Datetime, "%H:%M:%S", tz = "UTC") < "06:00:00"
    )
  stopifnot(
    identical(unserialize(prepared$candidate$prepared_payload), expected),
    identical(prepared$candidate$raw_payload, record$raw_payload)
  )

  value <- shiny::reactiveVal(record)
  runtime <- new_session_runtime(
    resolve_runtime_profile(profile, workers = 1L),
    start_workers = TRUE
  )
  on.exit(runtime$cleanup(), add = TRUE)
  control_results <- NULL
  decoded <- 0L
  raw_size <- length(record$raw_payload)
  # The trace observes only deserializations the size of this full raw payload.
  # It does not change decoded values or adjudicate results outside R.
  monitor <- new.env(parent = emptyenv())
  monitor$decoded <- 0L
  monitor$raw_size <- raw_size
  options(llw.m8.monitor = monitor)
  on.exit(options(llw.m8.monitor = NULL), add = TRUE)
  shiny::testServer(
    prepare_server,
    args = list(
      dataset = shiny::reactive(value()),
      runtime = runtime,
      on_commit = m8_commit(value)
    ),
    {
      result <- session$getReturned()
      m8_wait(session, function() !is.null(result$context()))
      m8_set_form(session, result$context())
      session$setInputs(ack_cost = TRUE)
      session$setInputs(step_type = "clock", save_step = 1L)
      session$setInputs(new_step = 1L)
      m8_set_form(session, result$context())
      session$setInputs(
        step_type = "clock",
        clock_start = "01:00:00",
        clock_end = "08:00:00",
        save_step = 2L
      )
      m8_wait(
        session,
        function()
          !result$active() && !result$pending() && !is.null(result$preview()),
        seconds = 45
      )
      step_id <- result$recipe()$steps[[2L]]$id
      session$setInputs(selected_step = step_id)
      trace(
        "unserialize",
        where = baseenv(),
        print = FALSE,
        tracer = quote({
          monitor <- getOption("llw.m8.monitor")
          if (is.raw(connection) && length(connection) == monitor$raw_size)
            monitor$decoded <- monitor$decoded + 1L
        })
      )
      on.exit(untrace("unserialize", where = baseenv()), add = TRUE)
      edited <- measure(function(i) {
        session$setInputs(
          clock_end = if (i %% 2L) "07:00:00" else "06:00:00",
          save_step = i + 2L
        )
        session$flushReact()
        invisible(output$recipe_list)
      })
      toggled <- measure(function(i) {
        session$setInputs(toggle = i)
        session$flushReact()
        invisible(output$recipe_list)
      })
      ordered <- measure(function(i) {
        if (i %% 2L) session$setInputs(up = i) else session$setInputs(down = i)
        session$flushReact()
        invisible(output$recipe_list)
      })
      control_results <<- list(
        edit = edited,
        enable_disable = toggled,
        reorder = ordered
      )
      decoded <<- monitor$decoded
      untrace("unserialize", where = baseenv())
    }
  )
  ordinary_p95 <- max(vapply(control_results, `[[`, numeric(1), "p95_ms"))
  list(
    rows = n,
    profile = profile,
    preview_seconds = elapsed,
    raw_serialized_bytes = raw_size,
    estimated_working_bytes = prepared$estimated_working_bytes,
    retained_candidate_bytes = as.numeric(object.size(prepared$candidate)),
    transient_total_allocated_bytes = sum(allocations, na.rm = TRUE),
    maximum_r_vector_heap_MiB = unname(peak["Vcells", "max used"]) * 8 / 1024^2,
    budget_bytes = budget,
    ordinary = control_results,
    ordinary_p95_ms = ordinary_p95,
    full_payload_decodes_during_controls = decoded,
    independent_raw_pipeline_identical = TRUE,
    controls_hard_gate_pass = ordinary_p95 <= 100 && decoded == 0L,
    controls_design_target_pass = ordinary_p95 <= 50
  )
}

cases <- list(run_case(250000L, "hosted"), run_case(1000000L, "local"))
packages <- c(
  "LightLogR",
  "glcdp",
  "shiny",
  "bslib",
  "dplyr",
  "lubridate",
  "mirai",
  "promises",
  "testthat"
)
result <- list(
  report_version = "1.2.0",
  generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  R = R.version.string,
  source_sha256 = stats::setNames(
    lapply(
      c(
        "R/preparation.R",
        "R/preparation-filters.R",
        "R/preparation-preview.R",
        "R/preparation-ui.R",
        "R/preparation-summary.R",
        "R/m_prepare.R",
        "dev/milestone-8-prepare-app.R",
        "dev/run-milestone-8-acceptance.R",
        "tests/testthat/helper-milestone-8.R"
      ),
      function(path) digest::digest(file = path, algo = "sha256")
    ),
    c(
      "R/preparation.R",
      "R/preparation-filters.R",
      "R/preparation-preview.R",
      "R/preparation-ui.R",
      "R/preparation-summary.R",
      "R/m_prepare.R",
      "dev/milestone-8-prepare-app.R",
      "dev/run-milestone-8-acceptance.R",
      "tests/testthat/helper-milestone-8.R"
    )
  ),
  packages = stats::setNames(
    lapply(packages, function(p) as.character(packageVersion(p))),
    packages
  ),
  command = "RENV_CONFIG_SANDBOX_ENABLED=FALSE Rscript dev/run-milestone-8-acceptance.R",
  inputs = "Explicitly synthetic R fixtures from dev/milestone-8-prepare-app.R; no published measurements",
  measurement_scope = "Main-session control latency with one real background worker and automatic preview enabled; preparation computation timed separately. Not browser or deployed concurrency latency. Total allocations are not simultaneous RSS.",
  target_ms = 50,
  hard_gate_ms = 100,
  cases = cases,
  all_hard_gates_pass = all(vapply(
    cases,
    `[[`,
    logical(1),
    "controls_hard_gate_pass"
  ))
)
jsonlite::write_json(result, output_path, pretty = TRUE, auto_unbox = TRUE)
print(result)
if (!result$all_hard_gates_pass)
  stop("An unchanged preparation control gate failed.")
