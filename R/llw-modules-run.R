# Custom modules and recipe execution --------------------------------------

#' Define a startup-provided LightLogWeb analysis module
#'
#' @param id Stable module ID using letters, numbers, and underscores.
#' @param title Human-readable title.
#' @param version Module interface version.
#' @param ui Shiny module UI function with signature `function(id)`.
#' @param server Shiny module server function with signature
#'   `function(id, context)`.
#' @param validate Plain function with arguments `data`, `metadata`, and
#'   `params`; it should return `TRUE` or throw an informative error.
#' @param run Plain analysis function with arguments `data`, `metadata`, and
#'   `params`.
#' @param code Function of `params` returning parseable R code.
#' @param required_columns Columns required in prepared data.
#' @param docs_url Optional module documentation URL.
#'
#' @return An `llw_module` specification.
#' @export
llw_module <- function(id,
                       title,
                       version = "1.0.0",
                       ui,
                       server,
                       validate = function(data, metadata, params) TRUE,
                       run,
                       code,
                       required_columns = c("Id", "Datetime"),
                       docs_url = NULL) {
  if (!is.character(id) || length(id) != 1L || !grepl("^[A-Za-z][A-Za-z0-9_]*$", id)) {
    llw_abort("Module `id` must start with a letter and contain only letters, numbers, and underscores.", "lightlogweb_invalid_module")
  }
  functions <- list(ui = ui, server = server, validate = validate, run = run, code = code)
  invalid <- names(functions)[!vapply(functions, is.function, logical(1))]
  if (length(invalid)) llw_abort(paste0("Module fields must be functions: ", paste(invalid, collapse = ", "), "."))
  structure(
    list(
      id = id,
      title = title,
      version = version,
      ui = ui,
      server = server,
      validate = validate,
      run = run,
      code = code,
      required_columns = unique(required_columns),
      docs_url = docs_url
    ),
    class = "llw_module"
  )
}

llw_validate_modules <- function(modules) {
  if (is.null(modules)) modules <- list()
  if (!is.list(modules)) llw_abort("`modules` must be a list of `llw_module` objects.")
  if (length(modules) && !all(vapply(modules, inherits, logical(1), "llw_module"))) {
    llw_abort("Every custom module must be created with `llw_module()`.", "lightlogweb_invalid_module")
  }
  ids <- vapply(modules, `[[`, character(1), "id")
  if (anyDuplicated(ids)) llw_abort("Custom module IDs must be unique.", "lightlogweb_invalid_module")
  if (length(modules) && is.null(names(modules))) names(modules) <- ids
  if (length(modules)) names(modules) <- ids
  modules
}

#' Run a custom module from the console
#'
#' @param x An `llw_dataset`.
#' @param module An `llw_module` specification.
#' @param params Named module parameters.
#'
#' @return An `llw_result`.
#' @export
llw_run_module <- function(x, module, params = list()) {
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x)
  if (!inherits(module, "llw_module")) llw_abort("`module` must be an `llw_module`.")
  missing <- setdiff(module$required_columns, names(x$prepared_data))
  if (length(missing)) {
    llw_abort(
      paste0("Module `", module$id, "` requires columns: ", paste(missing, collapse = ", "), "."),
      "lightlogweb_module_requirements"
    )
  }
  validation <- module$validate(data = x$prepared_data, metadata = x$metadata, params = params)
  if (!isTRUE(validation)) llw_abort(paste0("Module `", module$id, "` validation did not return TRUE."))
  code <- module$code(params)
  if (!is.character(code)) llw_abort(paste0("Module `", module$id, "` code renderer must return character R code."))
  parse(text = paste(code, collapse = "\n"))
  value <- module$run(data = x$prepared_data, metadata = x$metadata, params = params)
  if (!inherits(value, "llw_result")) {
    llw_abort(paste0("Module `", module$id, "` must return an `llw_result`."), "lightlogweb_invalid_module_result")
  }
  value$provenance <- utils::modifyList(
    list(module_id = module$id, module_version = module$version, code = code, docs_url = module$docs_url),
    value$provenance
  )
  value
}

#' Execute a complete LightLogWeb recipe
#'
#' @param x An `llw_dataset` or compatible data frame.
#' @param recipe An `llw_recipe`.
#' @param modules Startup-provided module specifications.
#'
#' @return An `llw_analysis`.
#' @export
llw_run <- function(x, recipe = llw_recipe(), modules = list()) {
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x)
  llw_validate_recipe(recipe)
  modules <- llw_validate_modules(modules)
  state <- list(dataset = x, metrics = list(), plots = list(), modules = list(), messages = character())
  registry <- llw_step_registry()

  for (step in recipe$steps) {
    if (!isTRUE(step$enabled)) next
    state <- registry[[step$type]]$execute(state, step, modules)
  }
  llw_analysis(
    dataset = state$dataset,
    recipe = recipe,
    quality = llw_quality(state$dataset),
    metrics = state$metrics,
    plots = state$plots,
    modules = state$modules,
    messages = state$messages
  )
}
