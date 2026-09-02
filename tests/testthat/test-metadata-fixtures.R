test_that("the three-source fixture manifest freezes exact public provenance", {
  manifest <- m7_fixture_manifest()
  entries <- manifest$fixtures
  ids <- vapply(entries, function(entry) entry$fixture_id, character(1))

  expect_identical(manifest$manifest_schema, "llw-glc-derived-fixture-manifest")
  expect_identical(manifest$manifest_version, "2.0.0")
  expect_identical(
    ids,
    c(
      "tum-medi-only",
      "iztech-light-subset",
      "ucr-all-light-variables"
    )
  )
  expect_identical(
    vapply(entries, function(entry) entry$source$exact_revision, character(1)),
    c(
      "1187d19b7f6145b73b8d4dfee13af711c1cb6180",
      "abc456bdb418e6250b9c973813a190dc062c6e76",
      "1d1309c969b9ca754a5c9696382c39d4b58ccdfd"
    )
  )
  expect_true(all(vapply(
    entries,
    function(entry) identical(entry$source$license$id, "CC-BY-4.0"),
    logical(1)
  )))
  expect_equal(
    manifest$total_fixture_bytes,
    sum(vapply(entries, function(entry) entry$file_bytes, numeric(1)))
  )
  expect_lte(manifest$total_fixture_bytes, 3 * 1024^2)
  expect_true(manifest$generation_policy$exported_glcdp_apis_only)
  expect_true(manifest$generation_policy$retained_measurement_values)
  expect_false(manifest$generation_policy$privacy_filtering)
  expect_identical(
    unlist(manifest$generation_policy$selected_files_per_fixture),
    c(5L, 6L, 5L)
  )
  expect_identical(
    manifest$generation_policy$selected_participants_per_fixture,
    5L
  )
  expect_identical(
    manifest$generation_policy$public_measurement_read_row_limit_per_file,
    32L
  )
  expect_false(manifest$generation_policy$source_package_reopen_in_tests)

  for (entry in entries) {
    path <- file.path(m7_fixture_directory(), entry$file)
    expect_true(file.exists(path), info = entry$fixture_id)
    expect_equal(unname(file.info(path)$size), entry$file_bytes)
    expect_identical(
      paste0("sha256:", digest::digest(path, algo = "sha256", file = TRUE)),
      entry$file_sha256,
      info = entry$fixture_id
    )
    expect_false(entry$public_value_retention$filtering_applied)
    expect_true(entry$public_value_retention$retention$public_source_values_retained)
  }
})

test_that("fixture manifest freezes the offline appended review state", {
  manifest <- m7_fixture_manifest()
  expect_length(manifest$offline_review_states, 1L)
  review <- manifest$offline_review_states[[1L]]
  sources <- review$sources

  expect_identical(review$review_state_id, "tum-iztech-appended")
  expect_true(review$derived_fixture)
  expect_true("generated_data_file" %in% names(review))
  expect_null(review$generated_data_file)
  expect_match(review$construction, "No source package is reopened", fixed = TRUE)
  expect_identical(
    vapply(sources, function(source) source$fixture_id, character(1)),
    c("tum-medi-only", "iztech-light-subset")
  )
  expect_identical(
    vapply(sources, function(source) source$dataset_record_id, character(1)),
    c("dataset_m7tummedionly", "dataset_m7iztechlightsubset")
  )
  expect_identical(
    vapply(sources, function(source) source$license, character(1)),
    rep("CC-BY-4.0", 2L)
  )
  expect_identical(
    vapply(sources, function(source) source$exact_revision, character(1)),
    c(
      "1187d19b7f6145b73b8d4dfee13af711c1cb6180",
      "abc456bdb418e6250b9c973813a190dc062c6e76"
    )
  )
  expect_identical(
    vapply(sources, function(source) length(source$package_dataset_ids), integer(1)),
    c(5L, 5L)
  )
  expect_identical(
    vapply(sources, function(source) source$retained_rows, integer(1)),
    c(160L, 192L)
  )
  expect_identical(review$lineage_column, "llw_source_dataset_id")
  expect_identical(review$retained_rows, 352L)
  expect_identical(review$overlay$revision, 1L)
  expect_true(review$overlay$source_bound)
  expect_false(review$overlay$source_mutated)
  expect_false(review$offline_test_policy$source_package_reopen)
  expect_true(review$offline_test_policy$retained_public_measurement_values)
  expect_false(review$offline_test_policy$privacy_filtering)
})

test_that("fixtures retain exact five-participant variable and plan contracts", {
  all_ucr_variables <- c(
    "Id", "file.name", "Datetime", "is.implicit", "EVENT", "TEMPERATURE",
    "ORIENTATION", "PIM", "PIMn", "TAT", "TATn", "ZCM", "ZCMn", "LIGHT",
    "IR.LIGHT", "CAP_SENS_1", "CAP_SENS_2", "F1", "F2", "F3", "F4", "F5",
    "F6", "F7", "F8", "MEDI", "CLEAR", "STATE", "position"
  )
  expected <- list(
    "tum-medi-only" = list(
      datasets = sprintf("MELIDOS_TUM_S%03d", 1:5),
      groups = c(
        "MELIDOS_TUM_S001:18", "MELIDOS_TUM_S002:17",
        "MELIDOS_TUM_S003:18", "MELIDOS_TUM_S004:18",
        "MELIDOS_TUM_S005:18"
      ),
      requested = "MEDI",
      result = c(
        "MEDI", "Id", "file_group_id", "participant_Id", "Datetime", "file.name"
      ),
      rows = 160L,
      compatibility = paste0(
        "glcc_",
        "5f35f72ffbea9c9ad5a49710ae4d88b69d0d9b1f9c8ab0ad075ee4e5a47b1620"
      ),
      unit = paste0(
        "glcu_",
        "c1175af6fb135f2f7c1eaff471e2607bcba997abcd603ee394661131d87cddb3"
      )
    ),
    "iztech-light-subset" = list(
      datasets = sprintf("MELIDOS_IZTECH_S%03d", 1:5),
      groups = c(
        "MELIDOS_IZTECH_S001:18", "MELIDOS_IZTECH_S002:18",
        "MELIDOS_IZTECH_S003:18", "MELIDOS_IZTECH_S004:19",
        "MELIDOS_IZTECH_S004:20", "MELIDOS_IZTECH_S005:18"
      ),
      requested = c("LIGHT", "MEDI"),
      result = c(
        "LIGHT", "MEDI", "Id", "file_group_id", "participant_Id",
        "Datetime", "file.name"
      ),
      rows = 192L,
      compatibility = paste0(
        "glcc_",
        "8c714caba48d4c2885d1c9f0dfeb34bcad088c829dc420e6a005cc4c9dec07b8"
      ),
      unit = paste0(
        "glcu_",
        "5c96116e9497b48ade74e50520666d741431be11184760642be44b63627734e9"
      )
    ),
    "ucr-all-light-variables" = list(
      datasets = sprintf("MELIDOS_UCR_S%03d", 1:5),
      groups = sprintf("MELIDOS_UCR_S%03d:18", 1:5),
      requested = all_ucr_variables,
      result = c(all_ucr_variables, "file_group_id", "participant_Id"),
      rows = 160L,
      compatibility = paste0(
        "glcc_",
        "a6aa68bac79e3236d9f1d46788dbd6bcaec6c4994fa4737cbf22a0f21f8ede12"
      ),
      unit = paste0(
        "glcu_",
        "f6d259c843347e3d10d784cd1d26e4b74d8651d21529d39222213c59bbd3e853"
      )
    )
  )

  for (fixture_id in names(expected)) {
    fixture <- m7_fixture(fixture_id)
    contract <- expected[[fixture_id]]
    expect_identical(fixture$selection$dataset_ids, contract$datasets)
    expect_identical(fixture$selection$file_group_ids, contract$groups)
    expect_identical(fixture$selection$requested_variables, contract$requested)
    expect_identical(names(fixture$data), contract$result)
    expect_identical(fixture$selection$result_columns, contract$result)
    expect_identical(nrow(fixture$data), contract$rows)
    expect_identical(fixture$selection$compatibility_id, contract$compatibility)
    expect_identical(fixture$selection$unit_id, contract$unit)
    expect_identical(fixture$selection$final_unit_count, 1L)
  }
})

test_that("offline fixtures retain exact bounded public values", {
  expected_states <- stats::setNames(
    c("complete", "partial", "complete"),
    c("tum-medi-only", "iztech-light-subset", "ucr-all-light-variables")
  )
  for (fixture_id in names(expected_states)) {
    fixture <- m7_fixture(fixture_id)
    expect_s3_class(fixture, "llw_glc_derived_fixture")
    expect_false(fixture$generation$synthetic_cells)
    expect_false(fixture$generation$privacy_filtering)
    expect_setequal(
      unique(as.character(fixture$data$Id)),
      fixture$selection$dataset_ids
    )
    retained_file_names <- unique(fixture$data$file.name)
    if (all(is.na(retained_file_names))) {
      expect_identical(fixture_id, "ucr-all-light-variables")
    } else {
      expect_setequal(
        retained_file_names,
        basename(fixture$selection$files)
      )
    }
    expect_identical(
      unique(fixture$data$file_group_id),
      fixture$selection$file_group_ids
    )
    expect_setequal(
      unique(fixture$data$participant_Id),
      fixture$selection$participant_ids
    )
    expect_s3_class(fixture$data$Datetime, "POSIXct")
    expect_identical(
      names(fixture$generation$source_read_sha256),
      fixture$selection$file_group_ids
    )
    expect_identical(
      names(fixture$generation$source_file_sha256),
      fixture$selection$file_group_ids
    )
    expect_true(all(grepl(
      "^sha256:[0-9a-f]{64}$",
      fixture$generation$source_read_sha256
    )))
    expect_true(all(grepl(
      "^sha256:[0-9a-f]{64}$",
      fixture$generation$source_file_sha256
    )))

    snapshot <- validate_glc_source_metadata_snapshot(fixture$metadata_snapshot)
    source <- snapshot$bundle$sources[[1L]]
    expect_identical(source$state, expected_states[[fixture_id]])
    expect_match(source$fingerprint, "^sha256:[0-9a-f]{64}$")
    expect_gt(nrow(snapshot$bundle$index$entries), 100L)
    expect_gt(nrow(snapshot$bundle$index$groups), 50L)
    expect_type(serialize(fixture, NULL, version = 3), "raw")
  }
})

test_that("fixture metadata is scoped to all represented relationships", {
  expected_variable_rows <- c(
    "tum-medi-only" = 20L,
    "iztech-light-subset" = 30L,
    "ucr-all-light-variables" = 145L
  )
  expected_device_rows <- c(
    "tum-medi-only" = 1L,
    "iztech-light-subset" = 4L,
    "ucr-all-light-variables" = 3L
  )
  expected_file_rows <- c(
    "tum-medi-only" = 5L,
    "iztech-light-subset" = 6L,
    "ucr-all-light-variables" = 5L
  )

  for (fixture_id in names(expected_variable_rows)) {
    fixture <- m7_fixture(fixture_id)
    bundle <- fixture$metadata_snapshot$bundle
    records <- bundle$sources[[1L]]$records
    core <- records$core_metadata
    inventory <- records$dataset_inventory
    file_parts <- lapply(core$datasets$dataset_file, function(value) {
      rownames(value) <- NULL
      value
    })
    file_row_count <- sum(vapply(file_parts, nrow, integer(1)))

    expect_identical(nrow(core$participants), 5L, info = fixture_id)
    expect_identical(
      nrow(core$devices), unname(expected_device_rows[[fixture_id]]),
      info = fixture_id
    )
    expect_identical(
      file_row_count, unname(expected_file_rows[[fixture_id]]),
      info = fixture_id
    )
    expect_identical(nrow(inventory), 5L, info = fixture_id)
    expect_identical(
      sum(inventory$file_count), unname(expected_file_rows[[fixture_id]]),
      info = fixture_id
    )
    expect_identical(
      sum(inventory$file_group_count), unname(expected_file_rows[[fixture_id]]),
      info = fixture_id
    )
    expect_identical(
      nrow(records$variable_inventory),
      unname(expected_variable_rows[[fixture_id]]),
      info = fixture_id
    )
    expect_setequal(
      as.character(core$participants$participant_internal_id),
      unique(as.character(fixture$data$participant_Id))
    )
    expect_setequal(
      unique(unlist(
        lapply(file_parts, function(value) value$dataset_file_names),
        recursive = TRUE,
        use.names = FALSE
      )),
      fixture$selection$files
    )

    age <- metadata_index_query_groups(bundle$index, "participant_age", limit = 100L)
    sex <- metadata_index_query_groups(bundle$index, "participant_sex", limit = 100L)
    expect_identical(age$total, 1L, info = fixture_id)
    expect_identical(sex$total, 1L, info = fixture_id)
    expect_identical(age$rows$resource, "core_metadata", info = fixture_id)
    expect_identical(sex$rows$resource, "core_metadata", info = fixture_id)
    expect_identical(age$rows$record_count, 5L, info = fixture_id)
    expect_identical(sex$rows$record_count, 5L, info = fixture_id)
    expect_identical(age$rows$entry_count, 5L, info = fixture_id)
    expect_identical(sex$rows$entry_count, 5L, info = fixture_id)
  }
})

test_that("participant metadata values are exact public package values", {
  expected <- list(
    "tum-medi-only" = list(
      ids = sprintf("TUM_S%03d", 1:5),
      ages = c(28, 26, 25, 29, 28),
      sex = c("2", "1", "1", "1", "1")
    ),
    "iztech-light-subset" = list(
      ids = sprintf("IZTECH_S%03d", 1:5),
      ages = c(27, 24, 23, 25, 25),
      sex = rep("Female", 5L)
    ),
    "ucr-all-light-variables" = list(
      ids = sprintf("UCR_S%03d", 1:5),
      ages = c(42, 27, 23, 23, 22),
      sex = c("1", "2", "2", "1", "1")
    )
  )
  manifest <- m7_fixture_manifest()

  for (entry in manifest$fixtures) {
    fixture <- m7_fixture(entry$fixture_id)
    participants <- fixture$metadata_snapshot$bundle$sources[[
      1L
    ]]$records$core_metadata$participants
    retention <- entry$public_value_retention$retention
    values <- expected[[entry$fixture_id]]

    expect_identical(as.character(participants$participant_internal_id), values$ids)
    expect_identical(as.numeric(participants$participant_age), values$ages)
    expect_identical(as.character(participants$participant_sex), values$sex)
    expect_false(entry$public_value_retention$filtering_applied)
    expect_false(retention$participant_values_replaced)
    expect_false(retention$device_serial_numbers_replaced)
    expect_false(retention$source_measurement_values_removed)
    expect_identical(retention$selected_participants_retained, 5L)
  }
})

test_that("IZTECH retains one participant measured by two devices", {
  fixture <- m7_fixture("iztech-light-subset")
  cases <- fixture$selection$cases
  s004 <- cases[vapply(
    cases,
    function(case) identical(case$participant_id, "IZTECH_S004"),
    logical(1)
  )]
  bundle <- fixture$metadata_snapshot$bundle
  device_group <- metadata_index_query_groups(
    bundle$index,
    "dataset_file_crossref_device_id",
    limit = 100L
  )
  selected <- metadata_index_group(
    bundle$index,
    device_group$rows$group_id[[1L]]
  )

  expect_length(s004, 2L)
  expect_setequal(
    vapply(s004, function(case) case$device_id, character(1)),
    c("IZTECH_AL03", "IZTECH_AL04")
  )
  expect_setequal(
    vapply(s004, function(case) case$file_group_id, character(1)),
    c("MELIDOS_IZTECH_S004:19", "MELIDOS_IZTECH_S004:20")
  )
  expect_identical(unname(table(fixture$data$participant_Id)[["IZTECH_S004"]]), 64L)
  expect_identical(device_group$total, 1L)
  expect_identical(device_group$rows$entry_count, 6L)
  expect_identical(device_group$rows$record_count, 6L)
  expect_identical(device_group$rows$distinct_value_count, 4L)
  expect_true(all(c("IZTECH_AL03", "IZTECH_AL04") %in% selected$entries$value))
  expect_identical(
    selected$entries$record,
    c(
      "MELIDOS_IZTECH_S001:18",
      "MELIDOS_IZTECH_S002:18",
      "MELIDOS_IZTECH_S003:18",
      "MELIDOS_IZTECH_S004:19",
      "MELIDOS_IZTECH_S004:20",
      "MELIDOS_IZTECH_S005:18"
    )
  )
  s004_entries <- selected$entries[
    selected$entries$record %in% c(
      "MELIDOS_IZTECH_S004:19",
      "MELIDOS_IZTECH_S004:20"
    ),
    ,
    drop = FALSE
  ]
  expect_identical(
    stats::setNames(s004_entries$value, s004_entries$record),
    c(
      "MELIDOS_IZTECH_S004:19" = "IZTECH_AL03",
      "MELIDOS_IZTECH_S004:20" = "IZTECH_AL04"
    )
  )
})

test_that("melanopic grouping never includes sibling variables", {
  for (fixture_id in c(
    "tum-medi-only",
    "iztech-light-subset",
    "ucr-all-light-variables"
  )) {
    bundle <- m7_fixture(fixture_id)$metadata_snapshot$bundle
    result <- metadata_index_query_groups(
      bundle$index,
      "melanopic EDI",
      limit = 100L
    )
    entries <- unlist(lapply(
      result$rows$group_id,
      function(group_id) {
        metadata_index_group(bundle$index, group_id)$entries$value
      }
    ), use.names = FALSE)

    expect_gt(result$total, 0L)
    expect_true(all(nzchar(result$rows$group_context)), info = fixture_id)
    expect_false(any(grepl("Photopic", entries, ignore.case = TRUE)))
    expect_false(any(entries == "LIGHT"))
  }
})

test_that("IZTECH unavailability is retained as a partial transport fact", {
  entry <- m7_fixture_manifest_entry("iztech-light-subset")
  exclusions <- entry$selection$metadata_resource_exclusions

  expect_named(exclusions, c("study", "participant_characteristics"))
  expect_match(exclusions$study, "HTTP 400", fixed = TRUE)
  expect_match(exclusions$participant_characteristics, "HTTP 400", fixed = TRUE)
  expect_match(exclusions$study, "not silently", fixed = TRUE)
  expect_false("study" %in% entry$selection$metadata_resources)
  expect_false(
    "participant_characteristics" %in% entry$selection$metadata_resources
  )
})
