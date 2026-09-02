test_that("Milestone 7 uses the exact pinned exported glcdp metadata API", {
  description <- utils::packageDescription("glcdp")
  expected_sha <- "e49fcf153a114916ce2680f8b40eb475e694e10d"
  expected_formals <- list(
    glc_resources = "x",
    glc_metadata = c("x", "resources"),
    glc_search_metadata = c(
      "x",
      "query",
      "resources",
      "fields",
      "fixed",
      "ignore_case",
      "search_in"
    ),
    extract_metadata = c("dataset", "metadata", "fields", "by", "resource"),
    add_metadata = c(
      "dataset",
      "metadata",
      "fields",
      "by",
      "resource",
      "overwrite"
    )
  )

  expect_identical(description$Version, "1.0.0.9000")
  expect_identical(description$RemoteSha, expected_sha)
  for (name in names(expected_formals)) {
    fun <- getExportedValue("glcdp", name)
    expect_true(is.function(fun), info = name)
    expect_identical(names(formals(fun)), expected_formals[[name]], info = name)
  }
})

test_that("exported metadata extraction preserves keys, order, and grouping", {
  dataset <- data.frame(
    Id = c("D1", "D1", "D2"),
    file_group_id = factor(
      c("D1:1", "D1:1", "D2:1"),
      levels = c("D2:1", "D1:1")
    ),
    value = 1:3,
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(Id, .drop = FALSE)
  metadata <- data.frame(
    file_group_id = c("D1:1", "D2:1"),
    condition = c("control", "intervention"),
    unit = c("lx", "lx"),
    stringsAsFactors = FALSE
  )

  extracted <- glcdp::extract_metadata(
    dataset,
    metadata,
    fields = c("condition", "unit")
  )
  joined <- glcdp::add_metadata(
    dataset,
    metadata,
    fields = c("condition", "unit")
  )

  expect_identical(as.character(extracted$file_group_id), c("D1:1", "D2:1"))
  expect_s3_class(extracted$file_group_id, "factor")
  expect_identical(joined$value, dataset$value)
  expect_identical(
    as.character(joined$file_group_id),
    as.character(
      dataset$file_group_id
    )
  )
  expect_identical(dplyr::group_vars(joined), "Id")
  expect_false(attr(dplyr::group_data(joined), ".drop"))
  expect_identical(nrow(joined), nrow(dataset))
})

test_that("exported metadata ambiguity and conflicts remain recoverable", {
  dataset <- data.frame(
    file_group_id = c("D1:1", "D1:1", "D2:1"),
    value = 1:3,
    stringsAsFactors = FALSE
  )
  metadata <- data.frame(
    file_group_id = c("D1:1", "D2:1"),
    condition = c("control", "intervention"),
    stringsAsFactors = FALSE
  )
  conflict <- dataset
  conflict$condition <- "existing"
  duplicated <- dplyr::bind_rows(metadata, metadata[1L, ])

  expect_error(
    glcdp::add_metadata(conflict, metadata, "condition"),
    class = "glcdp_metadata_column_conflict",
    regexp = "overwrite"
  )
  expect_error(
    glcdp::extract_metadata(dataset, duplicated, "condition"),
    class = "glcdp_metadata_id_error",
    regexp = "must be unique"
  )
  replaced <- glcdp::add_metadata(
    conflict,
    metadata,
    "condition",
    overwrite = TRUE
  )
  expect_identical(
    replaced$condition,
    c("control", "control", "intervention")
  )
})
