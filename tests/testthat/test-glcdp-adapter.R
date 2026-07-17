fake_glcdp_api <- function(call_log = new.env(parent = emptyenv())) {
  call_log$opened_ref <- NULL
  api <- list(
    glc_packages = function(...) data.frame(),
    glc_search_packages = function(...) data.frame(),
    glc_open = function(source, ref, cache_dir, registry, quiet) {
      call_log$opened_ref <- ref
      structure(
        list(
          source = source,
          transport = new.env(parent = emptyenv())
        ),
        class = "glc_package"
      )
    },
    glc_schema_versions = function() data.frame(),
    glc_summary = function(x) data.frame(dataset_count = 1L),
    glc_datasets = function(x, dataset_id = NULL) {
      data.frame(dataset_id = "dataset-a")
    },
    glc_files = function(x, dataset_id = NULL, file_group = NULL) {
      data.frame(
        path = "data.csv",
        expected_size = 100,
        available = TRUE
      )
    },
    glc_variables = function(
      x,
      dataset_id = NULL,
      file_group = NULL,
      primary = NULL
    ) {
      data.frame(variable = "MEDI")
    },
    glc_resources = function(x) data.frame(resource = "datasets"),
    glc_metadata = function(x, resources = NULL) list(study = "Fixture"),
    glc_search_metadata = function(...) data.frame(),
    glc_read = function(...) structure(list(), class = "glc_data_collection"),
    glc_collect = function(x, standardize) m1_fixture_data(),
    glc_download = function(...) data.frame(path = "data.csv")
  )
  list(api = api, calls = call_log)
}

test_that("GLC source and selection objects are serializable data only", {
  cache_dir <- tempfile("glc-cache-")
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "latest_pass",
    resolved_revision = paste(rep("a", 40L), collapse = ""),
    validation_state = "passed",
    attestation_verified = TRUE,
    schema_version = "2.0.0",
    glcdp_version = "0.90.0",
    cache_dir = cache_dir
  )
  selection <- new_glc_selection(
    dataset_ids = "dataset-a",
    source_variables = "MEDI"
  )

  expect_s3_class(source, "llw_glc_source_spec")
  expect_s3_class(selection, "llw_glc_selection")
  expect_null(nonserializable_path(source))
  expect_null(nonserializable_path(selection))
  expect_type(serialize(source, NULL), "raw")

  live_handle <- structure(
    list(transport = new.env(parent = emptyenv())),
    class = "glc_package"
  )
  expect_error(
    assert_serializable_value(live_handle, "handle"),
    class = "llw_validation_error",
    regexp = "live `glc_package` handle"
  )
})

test_that("the GLC worker reopens the exact revision and returns no live handle", {
  fake <- fake_glcdp_api()
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = paste(rep("b", 40L), collapse = ""),
    validation_state = "passed",
    schema_version = "2.0.0",
    glcdp_version = "0.90.0",
    cache_dir = tempfile("glc-cache-")
  )
  selection <- new_glc_selection(dataset_ids = "dataset-a")

  preview <- glcdp_worker_execute(
    "discovery",
    source = source,
    selection = selection,
    api = fake$api
  )

  expect_identical(fake$calls$opened_ref, source$resolved_revision)
  expect_s3_class(preview, "llw_glc_preview")
  expect_equal(preview$transfer_bytes, 100)
  expect_null(nonserializable_path(preview))
  expect_type(serialize(preview, NULL), "raw")
})

test_that("the worker boundary revalidates modified GLC value objects", {
  fake <- fake_glcdp_api()
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("d", 40L),
    validation_state = "passed",
    schema_version = "2.0.0",
    glcdp_version = "0.90.0",
    cache_dir = tempfile("glc-cache-")
  )
  selection <- new_glc_selection(dataset_ids = "dataset-a")

  source$resolved_revision <- "moving-branch"
  expect_error(
    glcdp_worker_execute(
      "discovery",
      source = source,
      selection = selection,
      api = fake$api
    ),
    class = "llw_validation_error",
    regexp = "exact 40-character commit SHA"
  )
  expect_null(fake$calls$opened_ref)

  source$resolved_revision <- strrep("d", 40L)
  selection$preview_n_max <- -1L
  expect_error(
    new_glcdp_task_payload(
      "read",
      source = source,
      selection = selection
    ),
    class = "llw_validation_error",
    regexp = "non-negative whole number"
  )
})

test_that("documented glcdp conditions map to recoverable public errors", {
  cases <- list(
    discovery = "glcdp_registry_error",
    validation = "glcdp_schema_error",
    transport = "glcdp_http_error",
    lfs = "glcdp_lfs_hash_mismatch",
    import = "glcdp_datetime_parse",
    compatibility = "glcdp_incompatible_collection",
    resource = "glcdp_file_exists"
  )

  for (category in names(cases)) {
    source <- structure(
      list(message = paste("private", category, "detail"), call = NULL),
      class = c(cases[[category]], "error", "condition")
    )
    mapped <- map_glcdp_condition(source)
    expect_s3_class(mapped, paste0("llw_glcdp_", category, "_error"))
    expect_identical(mapped$diagnostics$category, category)
    expect_match(mapped$diagnostics$original_message, "private")
    expect_false(grepl("private", llw_public_message(mapped), fixed = TRUE))
  }
})

test_that("the installed glcdp seam matches the reviewed public export contract", {
  skip_if_not_installed("glcdp", minimum_version = "0.90.0")
  exports <- getNamespaceExports("glcdp")

  expect_true(all(glcdp_export_contract() %in% exports))
  expect_identical(
    names(formals(glcdp::glc_open)),
    c("source", "ref", "token", "cache_dir", "registry", "quiet")
  )
  expect_identical(
    names(formals(glcdp::glc_read)),
    c(
      "x",
      "dataset_id",
      "file_group",
      "files",
      "variables",
      "terms",
      "primary_only",
      "n_max",
      "problems",
      "progress"
    )
  )
})

test_that("task payload construction rejects a live glcdp package handle", {
  source <- new_glc_source_spec(
    registry_url = "https://example.org/registry.json",
    repository = "owner/repository",
    requested_revision = "exact",
    resolved_revision = strrep("c", 40L),
    validation_state = "passed",
    schema_version = "2.0.0",
    glcdp_version = "0.90.0",
    cache_dir = tempfile("glc-cache-")
  )
  selection <- new_glc_selection(dataset_ids = "dataset-a")
  handle <- structure(
    list(transport = new.env(parent = emptyenv())),
    class = "glc_package"
  )

  expect_error(
    new_glcdp_task_payload(
      "read",
      source = source,
      selection = selection,
      destination = handle
    ),
    class = "llw_validation_error",
    regexp = "live `glc_package` handle"
  )
})
