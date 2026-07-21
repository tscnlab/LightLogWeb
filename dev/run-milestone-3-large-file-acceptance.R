pkgload::load_all(quiet = TRUE)

mebibyte <- 1024^2
limit_bytes <- 200 * mebibyte
near_limit_bytes <- limit_bytes - 64 * 1024
over_limit_bytes <- limit_bytes + 1

make_sparse_file <- function(path, bytes) {
  connection <- file(path, open = "wb")
  on.exit(close(connection), add = TRUE)
  seek(connection, where = bytes - 1, origin = "start", rw = "write")
  writeBin(as.raw(0), connection)
  invisible(path)
}

near_limit <- tempfile("lightlogweb-near-200mb-", fileext = ".txt")
over_limit <- tempfile("lightlogweb-over-200mb-", fileext = ".txt")
on.exit(unlink(c(near_limit, over_limit), force = TRUE), add = TRUE)
make_sparse_file(near_limit, near_limit_bytes)
make_sparse_file(over_limit, over_limit_bytes)

profile <- resolve_runtime_profile(
  "hosted",
  max_upload_mb = 200,
  workers = 0
)
runtime <- new_session_runtime(profile, session = NULL)
on.exit(runtime$cleanup(), add = TRUE)

started <- proc.time()[["elapsed"]]
staged <- runtime$stage_uploads(data.frame(
  name = "near-limit-malformed.txt",
  datapath = near_limit,
  stringsAsFactors = FALSE
))
request <- new_raw_import_request(
  device = "ActLumus",
  staged_files = staged,
  timezone = "UTC",
  not_before = as.Date("2001-01-01"),
  version = "default",
  id_mode = "automated",
  max_bytes = limit_bytes
)
outcome <- tryCatch(
  suppressWarnings(import_data(request)),
  error = identity
)
elapsed <- proc.time()[["elapsed"]] - started

stopifnot(
  identical(staged$size_bytes[[1L]], as.numeric(near_limit_bytes)),
  grepl("^sha256:[0-9a-f]{64}$", staged$sha256[[1L]]),
  identical(sha256_file(near_limit), staged$sha256[[1L]]),
  inherits(outcome, "llw_import_error"),
  nzchar(llw_public_message(outcome))
)

session_size_before_over_limit <- runtime$session_size()
over_limit_outcome <- tryCatch(
  runtime$stage_uploads(data.frame(
    name = "over-limit.txt",
    datapath = over_limit,
    stringsAsFactors = FALSE
  )),
  error = identity
)
stopifnot(
  inherits(over_limit_outcome, "llw_resource_error"),
  identical(runtime$session_size(), session_size_before_over_limit),
  grepl(
    "200 MB upload limit",
    llw_public_message(over_limit_outcome),
    fixed = TRUE
  )
)

cat(
  "near_limit_bytes",
  near_limit_bytes,
  "staged_sha256",
  staged$sha256[[1L]],
  "outcome_class",
  class(outcome)[[1L]],
  "public_message",
  llw_public_message(outcome),
  "elapsed_seconds",
  round(elapsed, 3),
  "over_limit_class",
  class(over_limit_outcome)[[1L]],
  "over_limit_message",
  llw_public_message(over_limit_outcome),
  sep = "\t"
)
cat("\n")
