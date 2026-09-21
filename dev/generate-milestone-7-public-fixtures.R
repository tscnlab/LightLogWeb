# Generate three bounded, derived, offline Milestone 7 GLC fixtures.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required to generate the fixtures.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The `jsonlite` package is required to write the fixture manifest.")
}

pkgload::load_all(".", export_all = TRUE, quiet = TRUE)

expected_glcdp_sha <- "e49fcf153a114916ce2680f8b40eb475e694e10d"
installed_glcdp <- utils::packageDescription("glcdp")
if (!identical(installed_glcdp$RemoteSha, expected_glcdp_sha)) {
  stop(
    "Fixture generation requires the exact pinned glcdp revision ",
    expected_glcdp_sha,
    "."
  )
}

generated_at <- Sys.getenv("LLW_M7_GENERATED_AT", unset = NA_character_)
if (
  is.na(generated_at) ||
    !grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      generated_at
    )
) {
  stop(
    "Set LLW_M7_GENERATED_AT to one explicit UTC timestamp, for example ",
    "2026-08-31T12:00:00Z."
  )
}

registry_url <- "https://tscnlab.github.io/glc-registry/registry.json"
fixture_directory <- file.path(
  "tests",
  "testthat",
  "fixtures",
  "milestone-7"
)
dir.create(fixture_directory, recursive = TRUE, showWarnings = FALSE)
staging_directory <- tempfile("m7-fixture-staging-", tmpdir = tempdir())
dir.create(staging_directory, recursive = TRUE)
on.exit(unlink(staging_directory, recursive = TRUE, force = TRUE), add = TRUE)

all_ucr_variables <- c(
  "Id",
  "file.name",
  "Datetime",
  "is.implicit",
  "EVENT",
  "TEMPERATURE",
  "ORIENTATION",
  "PIM",
  "PIMn",
  "TAT",
  "TATn",
  "ZCM",
  "ZCMn",
  "LIGHT",
  "IR.LIGHT",
  "CAP_SENS_1",
  "CAP_SENS_2",
  "F1",
  "F2",
  "F3",
  "F4",
  "F5",
  "F6",
  "F7",
  "F8",
  "MEDI",
  "CLEAR",
  "STATE",
  "position"
)

core_metadata_resources <- c(
  "study",
  "participants",
  "participant_characteristics",
  "datasets",
  "devices",
  "device_datasheets"
)

sources <- list(
  list(
    fixture_id = "tum-medi-only",
    output_file = "tum-medi-only.rds",
    label = "TUM MEDI-only derived fixture",
    repository = "tscnlab/melidos-tum-glc-dataset",
    revision = "1187d19b7f6145b73b8d4dfee13af711c1cb6180",
    citation_doi = "10.5281/zenodo.16893901",
    publication_version = paste(
      "Repository README identifies source version 1.0.1;",
      "CITATION.cff identifies 1.0.0. The discrepancy is retained."
    ),
    registry_errors = 0L,
    registry_warnings = 1228L,
    cases = data.frame(
      dataset_id = sprintf("MELIDOS_TUM_S%03d", 1:5),
      participant_id = sprintf("TUM_S%03d", 1:5),
      file_group_id = c(
        "MELIDOS_TUM_S001:18",
        "MELIDOS_TUM_S002:17",
        "MELIDOS_TUM_S003:18",
        "MELIDOS_TUM_S004:18",
        "MELIDOS_TUM_S005:18"
      ),
      file_path = sprintf(
        "data/files/sensor/TUM_S%03d_light_glasses.csv",
        1:5
      ),
      device_id = rep("TUM_AL01", 5L),
      declared_bytes = c(12743300, 12397779, 12249004, 12446232, 11526112),
      git_blob_sha1 = c(
        "c6ac02fa1d2c779a149388db53c9335546c8a1f2",
        "f6d7d27a0d302a636066a96432935f4baf0a0f13",
        "e8242e14c2f55ea673adea8c6485a6b9ad1f71d1",
        "bc7eb2114524d3b744651710068fa54bee50a316",
        "a700d74735b57254bd8e32b85d385726b89c0eca"
      ),
      stringsAsFactors = FALSE
    ),
    compatibility_id = paste0(
      "glcc_",
      "5f35f72ffbea9c9ad5a49710ae4d88b69d0d9b1f9c8ab0ad075ee4e5a47b1620"
    ),
    unit_id = paste0(
      "glcu_",
      "c1175af6fb135f2f7c1eaff471e2607bcba997abcd603ee394661131d87cddb3"
    ),
    request_mode = "explicit",
    requested_variables = "MEDI",
    metadata_resources = core_metadata_resources,
    metadata_resource_exclusions = list(),
    expected_result_columns = c(
      "Id",
      "file.name",
      "Datetime",
      "MEDI",
      "file_group_id",
      "participant_Id"
    ),
    metadata_state = "complete",
    rationale = paste(
      "Melanopic equivalent daylight illuminance is the sole requested",
      "measurement; automatic identity, time, key, and result columns remain."
    )
  ),
  list(
    fixture_id = "iztech-light-subset",
    output_file = "iztech-light-subset.rds",
    label = "IZTECH photopic and melanopic subset derived fixture",
    repository = "tscnlab/melidos-iztech-glc-dataset",
    revision = "abc456bdb418e6250b9c973813a190dc062c6e76",
    citation_doi = "10.5281/zenodo.16568109",
    publication_version = "1.0.0 at the pinned repository revision",
    registry_errors = 0L,
    registry_warnings = 2175L,
    cases = data.frame(
      dataset_id = c(
        "MELIDOS_IZTECH_S001",
        "MELIDOS_IZTECH_S002",
        "MELIDOS_IZTECH_S003",
        "MELIDOS_IZTECH_S004",
        "MELIDOS_IZTECH_S004",
        "MELIDOS_IZTECH_S005"
      ),
      participant_id = c(
        "IZTECH_S001",
        "IZTECH_S002",
        "IZTECH_S003",
        "IZTECH_S004",
        "IZTECH_S004",
        "IZTECH_S005"
      ),
      file_group_id = c(
        "MELIDOS_IZTECH_S001:18",
        "MELIDOS_IZTECH_S002:18",
        "MELIDOS_IZTECH_S003:18",
        "MELIDOS_IZTECH_S004:19",
        "MELIDOS_IZTECH_S004:20",
        "MELIDOS_IZTECH_S005:18"
      ),
      file_path = c(
        "data/files/sensor/IZTECH_S001_light_glasses.csv",
        "data/files/sensor/IZTECH_S002_light_glasses.csv",
        "data/files/sensor/IZTECH_S003_light_glasses.csv",
        "data/files/sensor/IZTECH_S004_light_glasses_AL03.csv",
        "data/files/sensor/IZTECH_S004_light_glasses_AL04.csv",
        "data/files/sensor/IZTECH_S005_light_glasses.csv"
      ),
      device_id = c(
        "IZTECH_AL03",
        "IZTECH_AL03",
        "IZTECH_AL05_FW1_7",
        "IZTECH_AL03",
        "IZTECH_AL04",
        "IZTECH_AL06_FW1_7"
      ),
      declared_bytes = c(
        11868229,
        10882377,
        10967292,
        1691503,
        8956228,
        10801890
      ),
      git_blob_sha1 = c(
        "74ca26574ba04fdedf06222ff776293b2055b020",
        "c7a3781740644a032e239e779b35599514fa3131",
        "6b748fcc56d6102e793da9a95c4f2960cd6fae3a",
        "9ebf9eb1f548ec452672d8531c9279ae607355ef",
        "8b004ec53c17d6530646162be6f445c154d5b858",
        "21b8bfa573b35958bef09363e17b4b5bd1c3d928"
      ),
      stringsAsFactors = FALSE
    ),
    compatibility_id = paste0(
      "glcc_",
      "8c714caba48d4c2885d1c9f0dfeb34bcad088c829dc420e6a005cc4c9dec07b8"
    ),
    unit_id = paste0(
      "glcu_",
      "5c96116e9497b48ade74e50520666d741431be11184760642be44b63627734e9"
    ),
    request_mode = "explicit",
    requested_variables = c("LIGHT", "MEDI"),
    metadata_resources = setdiff(
      core_metadata_resources,
      c("study", "participant_characteristics")
    ),
    metadata_resource_exclusions = list(
      study = paste(
        "Declared core resource data/study.json returned HTTP 400 from the",
        "GitHub raw endpoint at the exact revision on 2026-08-31. The object",
        "exists in Git and through GitHub's contents API. It is not silently",
        "substituted or copied around glcdp; this fixture remains partial."
      ),
      participant_characteristics = paste(
        "Declared optional resource data/participant_characteristics.csv",
        "returned HTTP 400 through exported glc_metadata() at the exact",
        "revision on 2026-08-31. It is retained as unavailable, not absent",
        "from or reinterpreted on behalf of the published package."
      )
    ),
    expected_result_columns = c(
      "Id",
      "file.name",
      "Datetime",
      "LIGHT",
      "MEDI",
      "file_group_id",
      "participant_Id"
    ),
    metadata_state = "partial",
    rationale = paste(
      "Photopic illuminance and melanopic EDI form a plausible paired light",
      "subset. Other available channels are deliberately not requested."
    )
  ),
  list(
    fixture_id = "ucr-all-light-variables",
    output_file = "ucr-all-light-variables.rds",
    label = "UCR all-variable derived fixture",
    repository = "tscnlab/melidos-ucr-glc-dataset",
    revision = "1d1309c969b9ca754a5c9696382c39d4b58ccdfd",
    citation_doi = "10.5281/zenodo.17289456",
    publication_version = "1.0.0 at the pinned repository revision",
    registry_errors = 0L,
    registry_warnings = 3095L,
    cases = data.frame(
      dataset_id = sprintf("MELIDOS_UCR_S%03d", 1:5),
      participant_id = sprintf("UCR_S%03d", 1:5),
      file_group_id = sprintf("MELIDOS_UCR_S%03d:18", 1:5),
      file_path = sprintf(
        "data/files/sensor/UCR_S%03d_light_wrist.csv",
        1:5
      ),
      device_id = c("UCR_AL03", "UCR_AL06", "UCR_AL09", "UCR_AL03", "UCR_AL09"),
      declared_bytes = c(11033699, 10819615, 11069984, 10462952, 10495760),
      git_blob_sha1 = c(
        "b6ffea655f0673a94f5b39808c9af925201c216c",
        "333daf2ec7c1ac3b1117a277d1fd4711d646afb5",
        "d8df8c5017ce8286090af94a67ecf11cd9f84ff0",
        "4c93dc6b7093a419e1b96c497f6a7cc5b2214f45",
        "3fd884ef264376c06f8c0851b5b8960db4ce0690"
      ),
      stringsAsFactors = FALSE
    ),
    compatibility_id = paste0(
      "glcc_",
      "a6aa68bac79e3236d9f1d46788dbd6bcaec6c4994fa4737cbf22a0f21f8ede12"
    ),
    unit_id = paste0(
      "glcu_",
      "f6d259c843347e3d10d784cd1d26e4b74d8651d21529d39222213c59bbd3e853"
    ),
    request_mode = "all",
    requested_variables = all_ucr_variables,
    metadata_resources = core_metadata_resources,
    metadata_resource_exclusions = list(),
    expected_result_columns = c(
      all_ucr_variables,
      "file_group_id",
      "participant_Id"
    ),
    metadata_state = "complete",
    rationale = paste(
      "Every variable available in the compatible wrist light unit is",
      "requested. This is the widest metadata and output contract."
    )
  )
)

recursive_atomic_values <- function(value) {
  if (is.data.frame(value)) value <- as.list(value)
  if (is.list(value)) {
    return(unlist(lapply(value, recursive_atomic_values), use.names = FALSE))
  }
  as.character(value)
}

fixture_nonempty_values <- function(value) {
  values <- recursive_atomic_values(value)
  unique(values[!is.na(values) & nzchar(values)])
}

fixture_bind_rows <- function(values) {
  values <- values[vapply(values, is.data.frame, logical(1))]
  if (length(values) == 0L) return(data.frame())
  result <- do.call(rbind, values)
  rownames(result) <- NULL
  as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE)
}

fixture_file_row_indices <- function(file_rows, paths, fixture_id) {
  if (
    !is.data.frame(file_rows) ||
      nrow(file_rows) == 0L ||
      !"dataset_file_names" %in% names(file_rows)
  ) {
    stop("Selected dataset file metadata are unavailable for ", fixture_id)
  }
  row_paths <- lapply(seq_len(nrow(file_rows)), function(row) {
    fixture_nonempty_values(file_rows$dataset_file_names[row])
  })
  vapply(
    paths,
    function(path) {
      matches <- which(vapply(
        row_paths,
        function(values) path %in% values,
        logical(1)
      ))
      if (length(matches) != 1L) {
        stop(
          "Selected file metadata are not unique for ",
          fixture_id,
          ": ",
          path
        )
      }
      matches[[1L]]
    },
    integer(1)
  )
}

fixture_scope_file_variables <- function(selected_files, retained_names) {
  if (!"dataset_file_variables" %in% names(selected_files)) {
    return(selected_files)
  }
  for (row in seq_len(nrow(selected_files))) {
    variables <- selected_files$dataset_file_variables[[row]]
    if (
      !is.data.frame(variables) ||
        nrow(variables) == 0L ||
        !"dataset_file_variables_name" %in% names(variables)
    ) {
      next
    }
    keep <- as.character(variables$dataset_file_variables_name) %in%
      retained_names
    selected_files$dataset_file_variables[[row]] <-
      variables[keep, , drop = FALSE]
  }
  selected_files
}

fixture_file_terms <- function(selected_files) {
  if (!"dataset_file_variables" %in% names(selected_files)) {
    return(character())
  }
  unique(unlist(
    lapply(
      selected_files$dataset_file_variables,
      function(variables) {
        if (
          !is.data.frame(variables) ||
            !"dataset_file_variables_term" %in% names(variables)
        ) {
          return(character())
        }
        fixture_nonempty_values(variables$dataset_file_variables_term)
      }
    ),
    use.names = FALSE
  ))
}

fixture_scope_study <- function(study, study_ids, dataset_ids) {
  if (!is.data.frame(study) || nrow(study) == 0L) return(study)
  if ("study_internal_id" %in% names(study) && length(study_ids) > 0L) {
    study <- study[
      as.character(study$study_internal_id) %in% study_ids,
      ,
      drop = FALSE
    ]
  }
  if ("study_datasets" %in% names(study)) {
    study$study_datasets <- lapply(study$study_datasets, function(value) {
      intersect(fixture_nonempty_values(value), dataset_ids)
    })
  }
  study
}

fixture_nrow <- function(value) {
  if (is.data.frame(value)) nrow(value) else 0L
}

prepare_public_case_metadata <- function(
  metadata,
  datasets,
  variables,
  source,
  result_columns
) {
  cases <- source$cases
  required_case_columns <- c(
    "dataset_id",
    "participant_id",
    "file_group_id",
    "file_path",
    "device_id",
    "declared_bytes",
    "git_blob_sha1"
  )
  if (
    !is.data.frame(cases) ||
      !identical(names(cases), required_case_columns) ||
      anyDuplicated(cases$file_group_id) ||
      anyDuplicated(cases$file_path)
  ) {
    stop("The five-case source contract is invalid for ", source$fixture_id)
  }
  dataset_ids <- unique(as.character(cases$dataset_id))
  participant_ids <- unique(as.character(cases$participant_id))
  if (length(dataset_ids) != 5L || length(participant_ids) != 5L) {
    stop(
      "Exactly five datasets and participants are required for ",
      source$fixture_id
    )
  }

  metadata_dataset_ids <- as.character(metadata$datasets$dataset_internal_id)
  dataset_positions <- match(dataset_ids, metadata_dataset_ids)
  if (anyNA(dataset_positions) || anyDuplicated(dataset_positions)) {
    stop("Selected dataset metadata are incomplete for ", source$fixture_id)
  }
  selected_datasets <- metadata$datasets[dataset_positions, , drop = FALSE]

  inventory_ids <- as.character(datasets$dataset_id)
  inventory_positions <- match(dataset_ids, inventory_ids)
  if (anyNA(inventory_positions) || anyDuplicated(inventory_positions)) {
    stop("Selected dataset inventory is incomplete for ", source$fixture_id)
  }
  selected_inventory <- datasets[inventory_positions, , drop = FALSE]
  inventory_participants <- fixture_nonempty_values(
    selected_inventory$participant_id
  )
  if (!setequal(inventory_participants, participant_ids)) {
    stop("Participant relationships changed for ", source$fixture_id)
  }

  retained_names <- unique(c(
    intersect(as.character(variables$name), result_columns),
    source$requested_variables
  ))
  selected_files <- vector("list", length(dataset_ids))
  device_ids_by_dataset <- vector("list", length(dataset_ids))
  for (index in seq_along(dataset_ids)) {
    dataset_id <- dataset_ids[[index]]
    case_rows <- cases[cases$dataset_id == dataset_id, , drop = FALSE]
    file_rows <- selected_datasets$dataset_file[[index]]
    file_indices <- fixture_file_row_indices(
      file_rows,
      case_rows$file_path,
      source$fixture_id
    )
    file_rows <- file_rows[file_indices, , drop = FALSE]
    file_rows <- fixture_scope_file_variables(file_rows, retained_names)
    selected_datasets$dataset_file[[index]] <- file_rows
    selected_files[[index]] <- file_rows

    selected_terms <- fixture_file_terms(file_rows)
    if (
      "dataset_variable_terms" %in%
        names(selected_datasets) &&
        is.data.frame(selected_datasets$dataset_variable_terms[[index]])
    ) {
      term_table <- selected_datasets$dataset_variable_terms[[index]]
      selected_datasets$dataset_variable_terms[[index]] <- term_table[
        as.character(term_table$term) %in% selected_terms,
        ,
        drop = FALSE
      ]
    }

    device_ids <- if ("dataset_file_crossref_device_id" %in% names(file_rows)) {
      fixture_nonempty_values(file_rows$dataset_file_crossref_device_id)
    } else {
      character()
    }
    if (!setequal(device_ids, unique(case_rows$device_id))) {
      stop("Selected device relationships changed for ", source$fixture_id)
    }
    device_ids_by_dataset[[index]] <- device_ids

    selected_inventory$file_group_count[[index]] <- nrow(file_rows)
    selected_inventory$file_count[[index]] <- nrow(file_rows)
    if ("modalities" %in% names(selected_inventory)) {
      selected_inventory$modalities[[index]] <- fixture_nonempty_values(
        file_rows$dataset_file_modality
      )
    }
    if ("device_ids" %in% names(selected_inventory)) {
      selected_inventory$device_ids[[index]] <- device_ids
    }
    if ("primary_variables" %in% names(selected_inventory)) {
      selected_inventory$primary_variables[[index]] <- intersect(
        fixture_nonempty_values(file_rows$primary_variables),
        retained_names
      )
    }
  }

  selected_device_ids <- unique(unlist(
    device_ids_by_dataset,
    use.names = FALSE
  ))
  devices_source <- metadata$devices
  devices <- if (is.data.frame(devices_source)) {
    devices_source[
      as.character(devices_source$device_internal_id) %in% selected_device_ids,
      ,
      drop = FALSE
    ]
  } else {
    devices_source
  }
  datasheet_ids <- if (is.data.frame(devices)) {
    fixture_nonempty_values(devices$device_datasheet_id)
  } else {
    character()
  }
  datasheets_source <- metadata$device_datasheets
  datasheets <- if (is.data.frame(datasheets_source)) {
    datasheets_source[
      as.character(datasheets_source$datasheet_id) %in% datasheet_ids,
      ,
      drop = FALSE
    ]
  } else {
    datasheets_source
  }

  study_ids <- fixture_nonempty_values(selected_inventory$study_id)
  participants_source <- metadata$participants
  participants <- if (is.data.frame(participants_source)) {
    participants_source[
      as.character(participants_source$participant_internal_id) %in%
        participant_ids,
      ,
      drop = FALSE
    ]
  } else {
    participants_source
  }
  if (
    !is.data.frame(participants) ||
      !setequal(
        as.character(participants$participant_internal_id),
        participant_ids
      )
  ) {
    stop(
      "Five participant metadata records are required for ",
      source$fixture_id
    )
  }

  characteristics_source <- metadata$participant_characteristics
  characteristics <- if (is.data.frame(characteristics_source)) {
    characteristics_source[
      as.character(characteristics_source$participant_internal_id) %in%
        participant_ids,
      ,
      drop = FALSE
    ]
  } else {
    characteristics_source
  }

  retained_metadata <- list(
    study = fixture_scope_study(
      metadata$study,
      study_ids,
      dataset_ids
    ),
    participants = participants,
    participant_characteristics = characteristics,
    datasets = selected_datasets,
    devices = devices,
    device_datasheets = datasheets
  )
  selected_variables <- variables[
    as.character(variables$file_group_id) %in%
      cases$file_group_id &
      as.character(variables$name) %in% retained_names,
    ,
    drop = FALSE
  ]
  represented_groups <- unique(as.character(selected_variables$file_group_id))
  if (!setequal(represented_groups, cases$file_group_id)) {
    stop("Selected variable inventories are incomplete for ", source$fixture_id)
  }

  list(
    metadata = retained_metadata,
    datasets = selected_inventory,
    variables = selected_variables,
    retention = list(
      privacy_filtering = FALSE,
      public_source_values_retained = TRUE,
      participant_ids_replaced = 0L,
      participant_values_replaced = FALSE,
      device_serial_numbers_replaced = FALSE,
      source_measurement_values_removed = FALSE,
      selected_studies_retained = fixture_nrow(retained_metadata$study),
      selected_participants_retained = fixture_nrow(participants),
      selected_devices_retained = fixture_nrow(devices),
      selected_files_retained = sum(vapply(selected_files, nrow, integer(1))),
      imported_variable_rows_retained = nrow(selected_variables)
    )
  )
}

file_sha256 <- function(path) {
  paste0("sha256:", digest::digest(path, algo = "sha256", file = TRUE))
}
generate_fixture <- function(source, cache_root) {
  message("Generating ", source$fixture_id, " from ", source$revision)
  source_cache <- file.path(cache_root, source$fixture_id)
  dir.create(source_cache, recursive = TRUE)
  handle <- glcdp::glc_open(
    source = source$repository,
    ref = source$revision,
    cache_dir = source_cache,
    registry = registry_url,
    quiet = TRUE
  )
  resources <- glcdp::glc_resources(handle)
  available_resources <- unique(as.character(resources$resource))
  if (!all(source$metadata_resources %in% available_resources)) {
    stop("A retained metadata resource is unavailable for ", source$fixture_id)
  }
  metadata <- glcdp::glc_metadata(
    handle,
    resources = source$metadata_resources
  )
  all_datasets <- glcdp::glc_datasets(handle)
  cases <- source$cases

  file_rows <- lapply(seq_len(nrow(cases)), function(index) {
    case <- cases[index, , drop = FALSE]
    files <- glcdp::glc_files(
      handle,
      dataset_id = case$dataset_id,
      file_group = case$file_group_id,
      role = NULL,
      modality = "light",
      available = TRUE
    )
    files <- files[as.character(files$path) == case$file_path, , drop = FALSE]
    if (nrow(files) != 1L) {
      stop("The exact selected file is unavailable for ", source$fixture_id)
    }
    if (
      !identical(as.character(files$participant_id), case$participant_id) ||
        !identical(as.character(files$device_id), case$device_id) ||
        !identical(as.numeric(files$expected_bytes), case$declared_bytes) ||
        !identical(as.character(files$blob_sha), case$git_blob_sha1)
    ) {
      stop("The selected public-file contract changed for ", source$fixture_id)
    }
    files
  })
  selected_files <- fixture_bind_rows(file_rows)

  variable_rows <- lapply(seq_len(nrow(cases)), function(index) {
    case <- cases[index, , drop = FALSE]
    variables <- glcdp::glc_variables(
      handle,
      dataset_id = case$dataset_id,
      file_group = case$file_group_id,
      term = NULL,
      primary = NULL
    )
    available_names <- unique(as.character(variables$name))
    if (identical(source$request_mode, "all")) {
      if (!setequal(available_names, source$requested_variables)) {
        stop("The all-variable contract changed for ", source$fixture_id)
      }
    } else if (!all(source$requested_variables %in% available_names)) {
      stop("A requested variable is unavailable for ", source$fixture_id)
    }
    variables
  })
  variables <- fixture_bind_rows(variable_rows)

  requested <- if (identical(source$request_mode, "all")) {
    NULL
  } else {
    source$requested_variables
  }
  variable_scope <- if (identical(source$request_mode, "all")) {
    "all"
  } else {
    "selected"
  }
  plan <- glcdp::glc_collection_plan(
    handle,
    variables = requested,
    variable_scope = variable_scope,
    dataset_id = unique(cases$dataset_id),
    file_group = cases$file_group_id,
    standardize = "lightlogr"
  )
  if (nrow(plan$units) != 1L) {
    stop("The selected fixture cases no longer form one final unit.")
  }
  plan_unit <- plan$units[1L, , drop = FALSE]
  planned_groups <- fixture_nonempty_values(plan_unit$file_group_ids)
  if (
    !identical(
      as.character(plan_unit$compatibility_id),
      source$compatibility_id
    ) ||
      !identical(as.character(plan_unit$unit_id), source$unit_id) ||
      !setequal(planned_groups, cases$file_group_id)
  ) {
    stop(
      "The exported collection-plan identity changed for ",
      source$fixture_id
    )
  }

  download_directory <- file.path(source_cache, "verified-download")
  downloads <- glcdp::glc_download(
    handle,
    dest_dir = download_directory,
    include = "data",
    dataset_id = unique(cases$dataset_id),
    file_group = cases$file_group_id,
    files = cases$file_path,
    overwrite = FALSE
  )
  selected_downloads <- downloads[
    match(cases$file_path, as.character(downloads$path)),
    ,
    drop = FALSE
  ]
  if (
    nrow(selected_downloads) != nrow(cases) ||
      anyNA(selected_downloads$path) ||
      !identical(as.numeric(selected_downloads$bytes), cases$declared_bytes) ||
      anyNA(selected_downloads$sha256) ||
      any(!nzchar(selected_downloads$sha256))
  ) {
    stop("Downloaded source-file hashes are incomplete for ", source$fixture_id)
  }
  source_file_hashes <- paste0("sha256:", selected_downloads$sha256)
  names(source_file_hashes) <- cases$file_group_id

  case_results <- lapply(seq_len(nrow(cases)), function(index) {
    case <- cases[index, , drop = FALSE]
    collection <- glcdp::glc_read(
      x = handle,
      dataset_id = case$dataset_id,
      file_group = case$file_group_id,
      files = case$file_path,
      variables = requested,
      terms = NULL,
      primary_only = FALSE,
      n_max = 32L,
      problems = "error",
      progress = FALSE
    )
    observed <- glcdp::glc_collect(collection, standardize = "lightlogr")
    if (!is.data.frame(observed) || nrow(observed) != 32L) {
      stop("Each bounded public case must retain exactly 32 rows.")
    }
    if (
      anyDuplicated(names(observed)) > 0L ||
        !setequal(names(observed), source$expected_result_columns)
    ) {
      stop(
        "The collected result-column contract changed for ",
        source$fixture_id
      )
    }
    if (
      !identical(
        unique(as.character(observed$file_group_id)),
        case$file_group_id
      ) ||
        !identical(
          unique(as.character(observed$participant_Id)),
          case$participant_id
        )
    ) {
      stop(
        "Collected participant or file-group lineage changed for ",
        source$fixture_id
      )
    }

    read_columns <- if (
      is.data.frame(collection) &&
        "data" %in% names(collection) &&
        length(collection$data) > 0L
    ) {
      names(collection$data[[1L]])
    } else {
      character()
    }
    list(
      data = observed,
      read_columns = read_columns,
      source_read_sha256 = paste0(
        "sha256:",
        digest::digest(observed, algo = "sha256", serialize = TRUE)
      )
    )
  })
  observed_cases <- lapply(case_results, function(result) result$data)
  result_columns <- names(observed_cases[[1L]])
  if (
    !all(vapply(
      observed_cases,
      function(observed) identical(names(observed), result_columns),
      logical(1)
    ))
  ) {
    stop("Collected result-column order differs between fixture cases.")
  }
  class_signatures <- lapply(observed_cases, function(observed) {
    vapply(
      observed,
      function(column) paste(class(column), collapse = "/"),
      character(1)
    )
  })
  if (
    !all(vapply(
      class_signatures,
      identical,
      logical(1),
      class_signatures[[1L]]
    ))
  ) {
    stop("Collected result-column classes differ between fixture cases.")
  }
  observed <- fixture_bind_rows(observed_cases)
  read_columns <- lapply(case_results, function(result) result$read_columns)
  names(read_columns) <- cases$file_group_id
  source_read_hashes <- vapply(
    case_results,
    function(result) result$source_read_sha256,
    character(1)
  )
  names(source_read_hashes) <- cases$file_group_id

  prepared_metadata <- prepare_public_case_metadata(
    metadata,
    all_datasets,
    variables,
    source,
    result_columns
  )
  relationship_files <- unique(data.frame(
    dataset_id = as.character(observed$Id),
    file_group_id = as.character(observed$file_group_id),
    stringsAsFactors = FALSE
  ))
  relationships <- glcdp_extract_relationship_context(
    handle = handle,
    files = relationship_files,
    metadata = prepared_metadata$metadata,
    api = glcdp_api()
  )
  snapshot <- new_glc_source_metadata_snapshot(
    source = list(
      registry_url = registry_url,
      registry_generated_at = NA_character_,
      repository = source$repository,
      requested_revision = "exact",
      resolved_revision = source$revision,
      validation_state = "passed",
      attestation_verified = TRUE,
      schema_version = "3.0.2",
      glcdp_version = as.character(utils::packageVersion("glcdp"))
    ),
    resources = resources,
    metadata = prepared_metadata$metadata,
    datasets = prepared_metadata$datasets,
    variables = prepared_metadata$variables,
    relationships = relationships,
    state = source$metadata_state
  )

  fixture <- structure(
    list(
      fixture_schema = "llw-glc-derived-fixture",
      fixture_version = "2.1.0",
      fixture_id = source$fixture_id,
      label = source$label,
      disclaimer = paste(
        "Derived, bounded public-data test fixture containing exact retained",
        "source values. It is not the complete published dataset and is not",
        "fresh or live measurement evidence."
      ),
      source = list(
        repository = source$repository,
        revision = source$revision,
        citation_doi = source$citation_doi,
        license = "CC-BY-4.0",
        schema_version = "3.0.2"
      ),
      selection = list(
        dataset_ids = unique(cases$dataset_id),
        participant_ids = unique(cases$participant_id),
        device_ids = unique(cases$device_id),
        compatibility_id = as.character(plan_unit$compatibility_id),
        unit_id = as.character(plan_unit$unit_id),
        final_unit_count = nrow(plan$units),
        file_group_ids = cases$file_group_id,
        files = cases$file_path,
        cases = lapply(seq_len(nrow(cases)), function(row) {
          as.list(cases[row, , drop = FALSE])
        }),
        requested_variables = source$requested_variables,
        request_mode = source$request_mode,
        metadata_resources = source$metadata_resources,
        metadata_resource_exclusions = source$metadata_resource_exclusions,
        read_columns = read_columns,
        result_columns = result_columns,
        n_max_per_file = 32L,
        standardize = "lightlogr",
        problems = "error"
      ),
      generation = list(
        generated_at = generated_at,
        retained_rows = nrow(observed),
        source_rows_parsed = nrow(observed),
        source_read_sha256 = source_read_hashes,
        source_file_sha256 = source_file_hashes,
        source_column_classes = class_signatures[[1L]],
        synthetic_cells = FALSE,
        privacy_filtering = FALSE,
        retention = prepared_metadata$retention,
        glcdp_plan_schema = plan$plan_schema,
        glcdp_plan_version = plan$plan_version,
        relationship_extraction = list(
          extractor = "glcdp::extract_metadata",
          glcdp_revision = expected_glcdp_sha,
          source_package_open_count = 1L,
          public_measurement_reads = 0L,
          anchor = relationships$anchor,
          dataset_anchor = relationships$dataset_anchor,
          requested_fields = relationships$requested_fields,
          resolved_file_group_fields = relationships$resolved_file_group_fields,
          resolved_dataset_fields = relationships$resolved_dataset_fields,
          warnings = relationships$warnings,
          file_group_rows = nrow(relationships$file_groups),
          dataset_rows = nrow(relationships$datasets),
          relationship_fingerprint = relationships$fingerprint
        )
      ),
      data = observed,
      metadata_snapshot = snapshot
    ),
    class = c("llw_glc_derived_fixture", "list")
  )
  assert_serializable_value(fixture, "Milestone 7 fixture")
  fixture
}
cache_root <- tempfile("m7-public-source-cache-")
dir.create(cache_root, recursive = TRUE)
on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

fixtures <- lapply(sources, generate_fixture, cache_root = cache_root)
names(fixtures) <- vapply(sources, `[[`, character(1), "fixture_id")

fixture_manifest_entries <- lapply(seq_along(sources), function(index) {
  source <- sources[[index]]
  fixture <- fixtures[[index]]
  output_path <- file.path(staging_directory, source$output_file)
  saveRDS(fixture, output_path, version = 3, compress = "xz")
  file_info <- file.info(output_path)
  if (is.na(file_info$size) || file_info$size > 1024^2) {
    stop(
      "Fixture exceeds the approved 1 MiB per-file limit: ",
      source$output_file
    )
  }
  list(
    fixture_id = source$fixture_id,
    label = source$label,
    derived_fixture = TRUE,
    disclaimer = fixture$disclaimer,
    file = source$output_file,
    file_bytes = unname(file_info$size),
    file_sha256 = file_sha256(output_path),
    source = list(
      citation = paste0("https://doi.org/", source$citation_doi),
      repository = paste0("https://github.com/", source$repository),
      exact_revision = source$revision,
      publication_status = "Published and publicly accessible",
      publication_version = source$publication_version,
      package_schema_version = "3.0.2",
      registry_validation = list(
        state = "latest_pass at audit",
        attested = TRUE,
        validator_version = "0.5.2",
        errors = source$registry_errors,
        warnings = source$registry_warnings
      ),
      license = list(
        id = "CC-BY-4.0",
        redistribution_assessment = paste(
          "Suitable for redistribution with attribution. This retained",
          "snapshot is derived, bounded, preserves public source values,",
          "and is clearly labelled."
        )
      ),
      public_accessibility = "Public GitHub repository and DOI landing page"
    ),
    selection = c(
      fixture$selection,
      list(
        source_file_declared_bytes = source$cases$declared_bytes,
        source_file_git_blob_sha1 = source$cases$git_blob_sha1,
        rationale = source$rationale
      )
    ),
    public_value_retention = list(
      assessment = paste(
        "Exact bounded values from the published public package are retained",
        "under the owner-approved no-filtering contract. No identifiers,",
        "participant metadata, device values, timestamps, or measurements are",
        "synthetically replaced."
      ),
      filtering_applied = FALSE,
      retention = fixture$generation$retention
    ),
    generation = c(
      fixture$generation[c(
        "generated_at",
        "retained_rows",
        "source_rows_parsed",
        "source_read_sha256",
        "source_file_sha256",
        "source_column_classes",
        "synthetic_cells",
        "privacy_filtering",
        "retention",
        "glcdp_plan_schema",
        "glcdp_plan_version",
        "relationship_extraction"
      )],
      list(
        r_version = R.version.string,
        glcdp_version = as.character(utils::packageVersion("glcdp")),
        glcdp_revision = expected_glcdp_sha,
        LightLogWeb_baseline = "b704d24ee6eab066163bb88e7125faf19e09543d"
      )
    )
  )
})

total_fixture_bytes <- sum(vapply(
  fixture_manifest_entries,
  `[[`,
  numeric(1),
  "file_bytes"
))
if (total_fixture_bytes > 3 * 1024^2) {
  stop("The fixture set exceeds the approved 3 MiB total limit.")
}

append_review_fixture_ids <- c("tum-medi-only", "iztech-light-subset")
append_review_sources <- sources[match(
  append_review_fixture_ids,
  vapply(sources, `[[`, character(1), "fixture_id")
)]
offline_review_states <- list(metadata_append_review_contract(
  lapply(append_review_sources, function(source) {
    list(
      fixture_id = source$fixture_id,
      label = source$label,
      dataset_record_id = metadata_fixture_dataset_record_id(
        source$fixture_id
      ),
      package_dataset_ids = unique(source$cases$dataset_id),
      retained_rows = fixture_manifest_entries[[match(
        source$fixture_id,
        vapply(sources, `[[`, character(1), "fixture_id")
      )]]$generation$retained_rows,
      citation = paste0("https://doi.org/", source$citation_doi),
      repository = paste0("https://github.com/", source$repository),
      exact_revision = source$revision,
      license = "CC-BY-4.0",
      package_schema_version = "3.0.2"
    )
  })
))

manifest <- list(
  manifest_schema = "llw-glc-derived-fixture-manifest",
  manifest_version = "2.1.0",
  generated_at = generated_at,
  derived_fixture_notice = paste(
    "Every entry is a bounded derived test fixture retaining exact public",
    "source values. No entry is a complete published dataset or current",
    "measurement evidence. Offline tests must not reopen a source package."
  ),
  generation_policy = list(
    exported_glcdp_apis_only = TRUE,
    public_measurement_read_row_limit_per_file = 32L,
    retained_measurement_values = TRUE,
    privacy_filtering = FALSE,
    retained_rows_per_source_file = 32L,
    selected_participants_per_fixture = 5L,
    selected_files_per_fixture = vapply(
      sources,
      function(source) nrow(source$cases),
      integer(1)
    ),
    standardize = "lightlogr",
    problems = "error",
    source_package_reopen_in_tests = FALSE,
    relationship_extraction = list(
      exported_glcdp_api = "extract_metadata",
      package_open_count_per_fixture = 1L,
      public_measurement_reads = 0L,
      default_anchor = "file_group_id",
      explicit_dataset_anchor = "Id",
      offline_tests_reopen_packages = FALSE
    ),
    per_fixture_byte_limit = 1024^2,
    total_byte_limit = 3 * 1024^2
  ),
  total_fixture_bytes = total_fixture_bytes,
  offline_review_states = offline_review_states,
  fixtures = fixture_manifest_entries
)

manifest_path <- file.path(staging_directory, "manifest.json")
jsonlite::write_json(
  manifest,
  manifest_path,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA,
  null = "null"
)

generated_files <- c(
  vapply(sources, `[[`, character(1), "output_file"),
  "manifest.json"
)
for (file in generated_files) {
  source_path <- file.path(staging_directory, file)
  destination <- file.path(fixture_directory, file)
  if (!file.copy(source_path, destination, overwrite = TRUE)) {
    stop("Could not place generated fixture file: ", file)
  }
}

cat("Milestone 7 public fixtures generated.\n")
cat("Generated at:", generated_at, "\n")
cat("Fixture bytes:", total_fixture_bytes, "\n")
for (entry in fixture_manifest_entries) {
  cat(entry$file, entry$file_bytes, entry$file_sha256, "\n")
}
