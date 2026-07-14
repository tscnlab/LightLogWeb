# Local project bundles -----------------------------------------------------

llw_project_manifest <- function(dataset, recipe, include_data, files = list()) {
  list(
    format = "LightLogWeb project",
    schema_version = .llw_schema_version,
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    dataset_name = dataset$name,
    includes_raw_data = isTRUE(include_data),
    source_fingerprints = dataset$provenance$files %||% list(),
    recipe_steps = length(recipe$steps),
    modules = lapply(Filter(function(step) identical(step$type, "module"), recipe$steps), function(step) {
      list(id = step$module_id, version = step$module_version, enabled = step$enabled)
    }),
    versions = list(
      LightLogWeb = tryCatch(as.character(utils::packageVersion("LightLogWeb")), error = function(e) "development"),
      LightLogR = as.character(utils::packageVersion("LightLogR")),
      R = as.character(getRversion())
    ),
    citations = list(
      LightLogR = "https://tscnlab.github.io/LightLogR/",
      LightLogWeb = "https://github.com/tscnlab/LightLogWeb"
    ),
    files = files
  )
}

llw_write_json <- function(x, path) {
  make_jsonable <- function(value) {
    if (inherits(value, c("llw_recipe_step", "llw_recipe", "llw_result", "llw_dataset", "llw_analysis"))) {
      value <- unclass(value)
    }
    if (is.list(value)) value <- lapply(value, make_jsonable)
    value
  }
  jsonlite::write_json(
    make_jsonable(x),
    path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    POSIXt = "ISO8601"
  )
}

#' Save a local LightLogWeb project bundle
#'
#' @param x An `llw_dataset` or `llw_analysis`.
#' @param recipe Recipe to save when `x` is a dataset.
#' @param path Output `.llw` ZIP path.
#' @param analysis Optional analysis result when `x` is a dataset.
#' @param include_data Include raw and prepared participant data. Defaults to
#'   `FALSE` for privacy and portability.
#'
#' @return The normalized project path, invisibly.
#' @export
llw_save_project <- function(x,
                             recipe = NULL,
                             path,
                             analysis = NULL,
                             include_data = FALSE) {
  if (inherits(x, "llw_analysis")) {
    analysis <- x
    dataset <- x$dataset
    recipe <- x$recipe
  } else {
    if (!inherits(x, "llw_dataset")) llw_abort("`x` must be an `llw_dataset` or `llw_analysis`.")
    dataset <- x
    recipe <- recipe %||% llw_recipe()
  }
  llw_validate_recipe(recipe)
  if (missing(path) || !nzchar(path)) llw_abort("Provide a project output `path`.")

  staging <- tempfile("lightlogweb-project-")
  dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)

  recipe_portable <- recipe
  recipe_portable$history <- list()
  recipe_portable$future <- list()
  saveRDS(recipe_portable, file.path(staging, "recipe.rds"), version = 3)
  llw_write_json(recipe_portable$steps, file.path(staging, "recipe.json"))
  llw_write_json(dataset$metadata, file.path(staging, "metadata.json"))
  saveRDS(dataset$annotations, file.path(staging, "annotations.rds"), version = 3)

  if (length(dataset$annotations)) {
    annotation_dir <- file.path(staging, "annotations")
    dir.create(annotation_dir)
    for (name in names(dataset$annotations)) {
      utils::write.csv(dataset$annotations[[name]], file.path(annotation_dir, paste0(name, ".csv")), row.names = FALSE)
    }
  }

  dataset_shell <- list(
    schema_version = dataset$schema_version,
    name = dataset$name,
    metadata = dataset$metadata,
    provenance = dataset$provenance,
    annotations = dataset$annotations,
    groups = dataset$groups,
    preparation = dataset$preparation
  )
  saveRDS(dataset_shell, file.path(staging, "dataset-shell.rds"), version = 3)

  if (isTRUE(include_data)) {
    saveRDS(dataset$raw_data, file.path(staging, "raw-data.rds"), version = 3)
    saveRDS(dataset$prepared_data, file.path(staging, "prepared-data.rds"), version = 3)
  }
  if (!is.null(analysis)) {
    result_shell <- analysis
    result_shell$dataset <- NULL
    saveRDS(result_shell, file.path(staging, "analysis.rds"), version = 3)
    metrics <- unlist(lapply(names(analysis$metrics), function(step_id) {
      value <- analysis$metrics[[step_id]]$value
      stats::setNames(value, paste(step_id, names(value), sep = "__"))
    }), recursive = FALSE)
    if (length(metrics)) {
      result_dir <- file.path(staging, "results")
      dir.create(result_dir)
      for (name in names(metrics)) llw_export(metrics[[name]], file.path(result_dir, paste0(name, ".csv")), "csv")
    }
    figures <- c(analysis$plots, Filter(function(result) inherits(result, "llw_result") && identical(result$type, "plot"), analysis$modules))
    if (length(figures)) {
      figure_dir <- file.path(staging, "figures")
      dir.create(figure_dir)
      for (name in names(figures)) {
        safe_name <- gsub("[^A-Za-z0-9_.-]", "_", name)
        llw_export(figures[[name]], file.path(figure_dir, paste0(safe_name, ".png")), "png")
        llw_export(figures[[name]], file.path(figure_dir, paste0(safe_name, "-data.csv")), "csv")
      }
    }
  }

  payload <- list.files(staging, recursive = TRUE, full.names = TRUE)
  relative <- substring(payload, nchar(staging) + 2L)
  hashes <- as.list(unname(tools::md5sum(payload)))
  names(hashes) <- relative
  manifest <- llw_project_manifest(dataset, recipe_portable, include_data, hashes)
  llw_write_json(manifest, file.path(staging, "manifest.json"))

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  zip::zipr(zipfile = path, files = list.files(staging, all.files = TRUE, no.. = TRUE), root = staging)
  invisible(normalizePath(path, mustWork = TRUE))
}

llw_verify_project <- function(directory, manifest) {
  hashes <- manifest$files %||% list()
  if (!length(hashes)) return(invisible(TRUE))
  paths <- file.path(directory, names(hashes))
  missing <- names(hashes)[!file.exists(paths)]
  if (length(missing)) llw_abort(paste0("Project bundle is missing files: ", paste(missing, collapse = ", "), "."), "lightlogweb_invalid_project")
  actual <- unname(tools::md5sum(paths))
  expected <- unlist(hashes, use.names = FALSE)
  bad <- names(hashes)[actual != expected]
  if (length(bad)) llw_abort(paste0("Project checksum mismatch: ", paste(bad, collapse = ", "), "."), "lightlogweb_invalid_project")
  invisible(TRUE)
}

#' Load a LightLogWeb project bundle
#'
#' @param path A `.llw` project bundle.
#' @param modules Available startup module specifications.
#'
#' @return An `llw_project` object. `dataset` is `NULL` when the bundle did not
#'   include participant data; use `llw_relink_project()`.
#' @export
llw_load_project <- function(path, modules = list()) {
  if (!file.exists(path)) llw_abort(paste0("Project file does not exist: ", path))
  modules <- llw_validate_modules(modules)
  directory <- tempfile("lightlogweb-load-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = directory)
  manifest_path <- file.path(directory, "manifest.json")
  if (!file.exists(manifest_path)) llw_abort("This is not a valid LightLogWeb project bundle.", "lightlogweb_invalid_project")
  manifest <- jsonlite::read_json(manifest_path, simplifyVector = TRUE)
  if (!identical(manifest$schema_version, .llw_schema_version)) {
    llw_abort(
      paste0("Project schema ", manifest$schema_version, " is not supported by schema ", .llw_schema_version, "."),
      "lightlogweb_project_version"
    )
  }
  llw_verify_project(directory, manifest)
  recipe <- readRDS(file.path(directory, "recipe.rds"))
  shell <- readRDS(file.path(directory, "dataset-shell.rds"))
  dataset <- NULL
  if (isTRUE(manifest$includes_raw_data)) {
    dataset <- llw_dataset(
      readRDS(file.path(directory, "raw-data.rds")),
      metadata = shell$metadata,
      name = shell$name,
      provenance = shell$provenance,
      annotations = shell$annotations
    )
    dataset$prepared_data <- readRDS(file.path(directory, "prepared-data.rds"))
    dataset$groups <- shell$groups
    dataset$preparation <- shell$preparation
  }

  available <- names(modules)
  warnings <- character()
  for (i in seq_along(recipe$steps)) {
    step <- recipe$steps[[i]]
    if (step$type != "module") next
    module <- modules[[step$module_id]]
    if (is.null(module) || (!is.null(step$module_version) && !identical(step$module_version, module$version))) {
      recipe$steps[[i]]$enabled <- FALSE
      warnings <- c(warnings, paste0("Module step `", step$label, "` was disabled because `", step$module_id, "` at version ", step$module_version, " is unavailable."))
    }
  }
  analysis_path <- file.path(directory, "analysis.rds")
  analysis <- if (file.exists(analysis_path)) readRDS(analysis_path) else NULL
  structure(
    list(
      manifest = manifest,
      dataset = dataset,
      dataset_shell = shell,
      recipe = recipe,
      analysis = analysis,
      warnings = warnings,
      path = normalizePath(path)
    ),
    class = "llw_project"
  )
}

#' Relink a data-free project to its source dataset
#'
#' @param project An `llw_project`.
#' @param dataset An `llw_dataset` with matching source fingerprints where they
#'   are available.
#'
#' @return The project with its dataset attached.
#' @export
llw_relink_project <- function(project, dataset) {
  if (!inherits(project, "llw_project")) llw_abort("`project` must be an `llw_project`.")
  if (!inherits(dataset, "llw_dataset")) llw_abort("`dataset` must be an `llw_dataset`.")
  expected <- project$dataset_shell$provenance$files$md5 %||% character()
  actual <- dataset$provenance$files$md5 %||% character()
  if (length(expected) && !identical(unname(expected), unname(actual))) {
    llw_abort("The selected source files do not match the project fingerprints.", "lightlogweb_project_source_mismatch")
  }
  shell <- project$dataset_shell
  dataset$name <- shell$name
  dataset$metadata <- utils::modifyList(dataset$metadata, shell$metadata)
  dataset$annotations <- shell$annotations
  project$dataset <- dataset
  project
}
