# Add exported, metadata-only relationship outputs to the approved fixtures.

if (!requireNamespace("pkgload", quietly = TRUE)) {
  stop("The `pkgload` package is required.")
}
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("The `jsonlite` package is required.")
}

pkgload::load_all(".", export_all = TRUE, quiet = TRUE)

expected_glcdp_sha <- "e49fcf153a114916ce2680f8b40eb475e694e10d"
installed_glcdp <- utils::packageDescription("glcdp")
if (!identical(installed_glcdp$RemoteSha, expected_glcdp_sha)) {
  stop("Relationship extraction requires exact pinned glcdp revision.")
}

generated_at <- Sys.getenv(
  "LLW_M7_RELATIONSHIPS_GENERATED_AT",
  unset = NA_character_
)
if (
  is.na(generated_at) ||
    !grepl(
      "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$",
      generated_at
    )
) {
  stop("Set LLW_M7_RELATIONSHIPS_GENERATED_AT to one explicit UTC timestamp.")
}

fixture_directory <- file.path("tests", "testthat", "fixtures", "milestone-7")
manifest_path <- file.path(fixture_directory, "manifest.json")
manifest <- jsonlite::read_json(manifest_path, simplifyVector = FALSE)
sources <- list(
  list(
    fixture_id = "tum-medi-only",
    file = "tum-medi-only.rds",
    repository = "tscnlab/melidos-tum-glc-dataset",
    revision = "1187d19b7f6145b73b8d4dfee13af711c1cb6180"
  ),
  list(
    fixture_id = "iztech-light-subset",
    file = "iztech-light-subset.rds",
    repository = "tscnlab/melidos-iztech-glc-dataset",
    revision = "abc456bdb418e6250b9c973813a190dc062c6e76"
  ),
  list(
    fixture_id = "ucr-all-light-variables",
    file = "ucr-all-light-variables.rds",
    repository = "tscnlab/melidos-ucr-glc-dataset",
    revision = "1d1309c969b9ca754a5c9696382c39d4b58ccdfd"
  )
)

file_sha256 <- function(path) {
  paste0("sha256:", digest::digest(path, algo = "sha256", file = TRUE))
}

manifest_entry <- function(fixture_id) {
  ids <- vapply(manifest$fixtures, `[[`, character(1), "fixture_id")
  position <- match(fixture_id, ids)
  if (is.na(position)) stop("Fixture is absent from manifest: ", fixture_id)
  position
}

staging_directory <- tempfile("m7-relationship-staging-", tmpdir = tempdir())
dir.create(staging_directory, recursive = TRUE)
on.exit(unlink(staging_directory, recursive = TRUE, force = TRUE), add = TRUE)
cache_root <- tempfile("m7-relationship-cache-", tmpdir = tempdir())
dir.create(cache_root, recursive = TRUE)
on.exit(unlink(cache_root, recursive = TRUE, force = TRUE), add = TRUE)

api <- glcdp_api()
updated <- vector("list", length(sources))

for (index in seq_along(sources)) {
  source <- sources[[index]]
  source_path <- file.path(fixture_directory, source$file)
  entry_index <- manifest_entry(source$fixture_id)
  old_hash <- manifest$fixtures[[entry_index]]$file_sha256
  if (!identical(file_sha256(source_path), old_hash)) {
    stop("Existing fixture hash does not match manifest: ", source$file)
  }
  fixture <- readRDS(source_path)
  if (
    !identical(fixture$fixture_id, source$fixture_id) ||
      !identical(fixture$source$repository, source$repository) ||
      !identical(fixture$source$revision, source$revision)
  ) {
    stop("Fixture source identity changed: ", source$file)
  }
  data_before <- fixture$data
  data_hash_before <- metadata_fingerprint(data_before)
  snapshot <- validate_glc_source_metadata_snapshot(fixture$metadata_snapshot)
  namespace <- snapshot$bundle$sources[[1L]]
  files <- unique(data.frame(
    dataset_id = as.character(fixture$data$Id),
    file_group_id = as.character(fixture$data$file_group_id),
    stringsAsFactors = FALSE
  ))

  message("Opening metadata only: ", source$repository, "@", source$revision)
  handle <- api$glc_open(
    source = source$repository,
    ref = source$revision,
    cache_dir = file.path(cache_root, source$fixture_id),
    registry = "https://tscnlab.github.io/glc-registry/registry.json",
    quiet = TRUE
  )
  relationships <- glcdp_extract_relationship_context(
    handle = handle,
    files = files,
    metadata = namespace$records$core_metadata,
    api = api
  )
  relationships <- new_metadata_relationship_context(
    file_groups = relationships$file_groups,
    datasets = relationships$datasets,
    requested_fields = relationships$requested_fields,
    warnings = relationships$warnings,
    provenance = c(
      relationships$provenance,
      list(
        relationship_fixture_generated_at = generated_at,
        glcdp_version = as.character(utils::packageVersion("glcdp")),
        glcdp_revision = expected_glcdp_sha,
        exact_repository = source$repository,
        exact_revision = source$revision,
        package_open_count = 1L,
        existing_fixture_measurements_reused = TRUE
      )
    )
  )
  fixture$fixture_version <- "2.1.0"
  fixture$metadata_snapshot <- new_glc_source_metadata_snapshot(
    source = snapshot$source,
    resources = namespace$resources,
    metadata = namespace$records$core_metadata,
    datasets = namespace$records$dataset_inventory,
    variables = namespace$records$variable_inventory,
    relationships = relationships,
    state = namespace$state
  )
  fixture$generation$relationship_extraction <- list(
    generated_at = generated_at,
    extractor = "glcdp::extract_metadata",
    glcdp_revision = expected_glcdp_sha,
    source_package_open_count = 1L,
    public_measurement_reads = 0L,
    existing_measurement_snapshot_reused = TRUE,
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
  if (
    !identical(fixture$data, data_before) ||
      !identical(metadata_fingerprint(fixture$data), data_hash_before)
  ) {
    stop("Measurement snapshot changed during metadata augmentation.")
  }
  assert_serializable_value(fixture, "augmented Milestone 7 fixture")
  output_path <- file.path(staging_directory, source$file)
  saveRDS(fixture, output_path, version = 3, compress = "xz")
  info <- file.info(output_path)
  if (is.na(info$size) || info$size > 1024^2) {
    stop("Augmented fixture exceeds 1 MiB: ", source$file)
  }
  updated[[index]] <- list(
    fixture = fixture,
    staging_path = output_path,
    bytes = unname(info$size),
    sha256 = file_sha256(output_path),
    manifest_index = entry_index
  )
}

for (index in seq_along(sources)) {
  source <- sources[[index]]
  value <- updated[[index]]
  entry <- manifest$fixtures[[value$manifest_index]]
  entry$file_bytes <- value$bytes
  entry$file_sha256 <- value$sha256
  entry$generation$relationship_extraction <-
    value$fixture$generation$relationship_extraction
  manifest$fixtures[[value$manifest_index]] <- entry
}

manifest$manifest_version <- "2.1.0"
manifest$relationships_generated_at <- generated_at
manifest$generation_policy$relationship_extraction <- list(
  exported_glcdp_api = "extract_metadata",
  package_open_count_per_fixture = 1L,
  public_measurement_reads = 0L,
  existing_measurement_snapshots_reused = TRUE,
  default_anchor = "file_group_id",
  explicit_dataset_anchor = "Id",
  offline_tests_reopen_packages = FALSE
)
manifest$total_fixture_bytes <- sum(vapply(
  updated,
  `[[`,
  numeric(1),
  "bytes"
))
if (manifest$total_fixture_bytes > 3 * 1024^2) {
  stop("Augmented fixture set exceeds 3 MiB.")
}

staging_manifest <- file.path(staging_directory, "manifest.json")
jsonlite::write_json(
  manifest,
  staging_manifest,
  auto_unbox = TRUE,
  pretty = TRUE,
  digits = NA,
  null = "null"
)

for (value in updated) {
  destination <- file.path(fixture_directory, basename(value$staging_path))
  if (!file.copy(value$staging_path, destination, overwrite = TRUE)) {
    stop("Could not replace fixture: ", destination)
  }
}
if (!file.copy(staging_manifest, manifest_path, overwrite = TRUE)) {
  stop("Could not replace fixture manifest.")
}

cat("Milestone 7 relationship fixture augmentation complete.\n")
cat("Metadata-only timestamp:", generated_at, "\n")
for (index in seq_along(sources)) {
  cat(
    sources[[index]]$file,
    updated[[index]]$bytes,
    updated[[index]]$sha256,
    "\n"
  )
}
