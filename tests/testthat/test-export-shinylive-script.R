local({
  source("scripts/export-shinylive.R", local = environment())

  test_that("resolve_repo_dir uses script path when available", {
    temp_repo <- tempfile("lightlogweb-repo-")
    dir.create(temp_repo)
    dir.create(file.path(temp_repo, "R"))
    dir.create(file.path(temp_repo, "inst", "app", "www"), recursive = TRUE)
    file.create(file.path(temp_repo, "DESCRIPTION"))

    script_path <- file.path(temp_repo, "scripts", "export-shinylive.R")
    dir.create(dirname(script_path), recursive = TRUE)
    file.create(script_path)

    expect_equal(
      resolve_repo_dir(script_path = script_path, working_dir = tempdir()),
      normalizePath(temp_repo, winslash = "/", mustWork = TRUE)
    )
  })

  test_that("resolve_repo_dir falls back to working directory", {
    temp_repo <- tempfile("lightlogweb-wd-")
    dir.create(temp_repo)
    dir.create(file.path(temp_repo, "R"))
    dir.create(file.path(temp_repo, "inst", "app", "www"), recursive = TRUE)
    file.create(file.path(temp_repo, "DESCRIPTION"))

    expect_equal(
      resolve_repo_dir(script_path = NA_character_, working_dir = temp_repo),
      normalizePath(temp_repo, winslash = "/", mustWork = TRUE)
    )
  })

  test_that("resolve_repo_dir errors when no repository can be found", {
    temp_dir <- tempfile("lightlogweb-empty-")
    dir.create(temp_dir)

    expect_error(
      resolve_repo_dir(script_path = NA_character_, working_dir = temp_dir),
      "Could not determine the package root directory"
    )
  })
})
