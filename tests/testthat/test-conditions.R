test_that("every Milestone 1 failure domain has a typed condition", {
  for (type in llw_error_types()) {
    condition <- new_llw_error(
      message = paste("private", type, "diagnostic"),
      type = type,
      public_message = paste("Public", type, "message"),
      diagnostics = list(secret = "retained privately")
    )
    expect_s3_class(condition, paste0("llw_", type, "_error"))
    expect_s3_class(condition, "llw_error")
    expect_identical(condition$diagnostics$secret, "retained privately")
    expect_false(grepl(
      "diagnostic",
      llw_public_message(condition),
      fixed = TRUE
    ))
  }
})

test_that("task workloads map to their recoverable failure domains", {
  expected <- c(
    raw_import = "import",
    glc_discovery = "network",
    glc_read = "import",
    glc_download = "network",
    preparation = "preparation",
    append_merge = "preparation",
    metrics = "metric",
    report = "export"
  )

  expect_identical(
    unname(vapply(names(expected), task_error_type, character(1))),
    unname(expected)
  )
})
