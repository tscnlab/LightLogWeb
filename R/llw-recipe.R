# Analysis recipes ----------------------------------------------------------

llw_step_id <- function(type) {
  paste0(type, "_", format(Sys.time(), "%Y%m%d%H%M%OS6"), "_", sample.int(9999, 1))
}

llw_recipe_step <- function(type,
                            parameters = list(),
                            id = NULL,
                            label = NULL,
                            enabled = TRUE,
                            module_id = NULL,
                            module_version = NULL) {
  allowed <- c("preparation", "grouping", "metric", "visualization", "annotation", "module")
  if (!type %in% allowed) {
    llw_abort(paste0("Unknown recipe step type `", type, "`."), "lightlogweb_invalid_recipe")
  }
  if (!is.list(parameters)) llw_abort("Recipe step `parameters` must be a list.")
  if (length(parameters) && (is.null(names(parameters)) || any(!nzchar(names(parameters))))) {
    llw_abort("Recipe step `parameters` must be a fully named list.", "lightlogweb_invalid_recipe")
  }
  structure(
    list(
      id = id %||% llw_step_id(type),
      type = type,
      label = label %||% tools::toTitleCase(type),
      enabled = isTRUE(enabled),
      parameters = parameters,
      module_id = module_id,
      module_version = module_version,
      created_at = Sys.time()
    ),
    class = "llw_recipe_step"
  )
}

llw_validate_recipe <- function(recipe) {
  if (!inherits(recipe, "llw_recipe")) llw_abort("Expected an `llw_recipe` object.")
  ids <- vapply(recipe$steps, `[[`, character(1), "id")
  if (anyDuplicated(ids)) llw_abort("Recipe step IDs must be unique.", "lightlogweb_invalid_recipe")
  invisible(lapply(recipe$steps, llw_validate_recipe_step))
  invisible(recipe)
}

#' Create a LightLogWeb analysis recipe
#'
#' @param steps A list of recipe steps. Most users build recipes with
#'   `llw_recipe_add()`.
#'
#' @return An object of class `llw_recipe`.
#' @export
llw_recipe <- function(steps = list()) {
  if (!is.list(steps)) llw_abort("`steps` must be a list.")
  recipe <- structure(
    list(
      schema_version = .llw_schema_version,
      steps = steps,
      history = list(),
      future = list()
    ),
    class = "llw_recipe"
  )
  llw_validate_recipe(recipe)
  recipe
}

#' @export
print.llw_recipe <- function(x, ...) {
  cat("<llw_recipe>", length(x$steps), "steps\n")
  if (length(x$steps)) {
    tab <- data.frame(
      id = vapply(x$steps, `[[`, character(1), "id"),
      type = vapply(x$steps, `[[`, character(1), "type"),
      enabled = vapply(x$steps, `[[`, logical(1), "enabled"),
      label = vapply(x$steps, `[[`, character(1), "label")
    )
    print(tab, row.names = FALSE)
  }
  invisible(x)
}

llw_recipe_checkpoint <- function(recipe) {
  recipe$history <- c(recipe$history, list(recipe$steps))
  recipe$future <- list()
  recipe
}

#' Add a step to a recipe
#'
#' @param recipe An `llw_recipe`.
#' @param type Step type.
#' @param parameters Named parameter list passed to the corresponding console
#'   function.
#' @param label,id Optional label and stable ID.
#' @param enabled Whether the step should run.
#' @param module_id,module_version Identifiers for module steps.
#'
#' @return An updated `llw_recipe`.
#' @export
llw_recipe_add <- function(recipe,
                           type,
                           parameters = list(),
                           label = NULL,
                           id = NULL,
                           enabled = TRUE,
                           module_id = NULL,
                           module_version = NULL) {
  llw_validate_recipe(recipe)
  recipe <- llw_recipe_checkpoint(recipe)
  recipe$steps <- c(
    recipe$steps,
    list(llw_recipe_step(type, parameters, id, label, enabled, module_id, module_version))
  )
  llw_validate_recipe(recipe)
  recipe
}

#' Modify recipe steps
#'
#' @param recipe An `llw_recipe`.
#' @param id Step ID.
#' @param parameters Replacement parameter list.
#' @param enabled Replacement enabled state.
#' @param label Replacement label.
#'
#' @return An updated recipe.
#' @export
llw_recipe_update <- function(recipe, id, parameters = NULL, enabled = NULL, label = NULL) {
  llw_validate_recipe(recipe)
  idx <- match(id, vapply(recipe$steps, `[[`, character(1), "id"))
  if (is.na(idx)) llw_abort(paste0("Recipe step `", id, "` was not found."))
  recipe <- llw_recipe_checkpoint(recipe)
  if (!is.null(parameters)) recipe$steps[[idx]]$parameters <- parameters
  if (!is.null(enabled)) recipe$steps[[idx]]$enabled <- isTRUE(enabled)
  if (!is.null(label)) recipe$steps[[idx]]$label <- label
  recipe
}

#' @rdname llw_recipe_update
#' @export
llw_recipe_remove <- function(recipe, id) {
  llw_validate_recipe(recipe)
  idx <- match(id, vapply(recipe$steps, `[[`, character(1), "id"))
  if (is.na(idx)) llw_abort(paste0("Recipe step `", id, "` was not found."))
  recipe <- llw_recipe_checkpoint(recipe)
  recipe$steps <- recipe$steps[-idx]
  recipe
}

#' Move a recipe step
#'
#' @param recipe An `llw_recipe`.
#' @param id Step ID.
#' @param direction Either `"up"` or `"down"`.
#'
#' @return An updated recipe.
#' @export
llw_recipe_move <- function(recipe, id, direction = c("up", "down")) {
  direction <- match.arg(direction)
  ids <- vapply(recipe$steps, `[[`, character(1), "id")
  idx <- match(id, ids)
  if (is.na(idx)) llw_abort(paste0("Recipe step `", id, "` was not found."))
  target <- idx + if (direction == "up") -1L else 1L
  if (target < 1L || target > length(recipe$steps)) return(recipe)
  recipe <- llw_recipe_checkpoint(recipe)
  recipe$steps[c(idx, target)] <- recipe$steps[c(target, idx)]
  recipe
}

#' Undo or redo recipe changes
#'
#' @param recipe An `llw_recipe`.
#'
#' @return An updated recipe.
#' @export
llw_recipe_undo <- function(recipe) {
  llw_validate_recipe(recipe)
  if (!length(recipe$history)) return(recipe)
  previous <- recipe$history[[length(recipe$history)]]
  recipe$history <- recipe$history[-length(recipe$history)]
  recipe$future <- c(list(recipe$steps), recipe$future)
  recipe$steps <- previous
  recipe
}

#' @rdname llw_recipe_undo
#' @export
llw_recipe_redo <- function(recipe) {
  llw_validate_recipe(recipe)
  if (!length(recipe$future)) return(recipe)
  next_steps <- recipe$future[[1]]
  recipe$future <- recipe$future[-1]
  recipe$history <- c(recipe$history, list(recipe$steps))
  recipe$steps <- next_steps
  recipe
}

llw_dput <- function(x) paste(utils::capture.output(dput(x)), collapse = "\n")

llw_function_schema <- function(fn, required = character()) {
  arguments <- setdiff(names(formals(fn)), "x")
  list(required = required, allowed = arguments)
}

llw_validate_parameter_schema <- function(parameters, schema, type) {
  if (identical(schema$allowed, "module-defined")) return(invisible(TRUE))
  unknown <- setdiff(names(parameters), schema$allowed)
  missing <- setdiff(schema$required, names(parameters))
  if (length(unknown)) llw_abort(paste0("Unknown parameters for ", type, " step: ", paste(unknown, collapse = ", "), "."), "lightlogweb_invalid_recipe")
  if (length(missing)) llw_abort(paste0("Missing parameters for ", type, " step: ", paste(missing, collapse = ", "), "."), "lightlogweb_invalid_recipe")
  invisible(TRUE)
}

llw_step_registry <- function() {
  list(
    preparation = list(
      schema = llw_function_schema(llw_prepare),
      execute = function(state, step, modules) { state$dataset <- rlang::exec(llw_prepare, x = state$dataset, !!!step$parameters); state },
      code = function(step, object) sprintf("%s <- llw_prepare(%s%s)", object, object, llw_code_arguments(step$parameters))
    ),
    grouping = list(
      schema = llw_function_schema(llw_group),
      execute = function(state, step, modules) { state$dataset <- rlang::exec(llw_group, x = state$dataset, !!!step$parameters); state },
      code = function(step, object) sprintf("%s <- llw_group(%s%s)", object, object, llw_code_arguments(step$parameters))
    ),
    annotation = list(
      schema = llw_function_schema(llw_annotate, "intervals"),
      execute = function(state, step, modules) { state$dataset <- rlang::exec(llw_annotate, x = state$dataset, !!!step$parameters); state },
      code = function(step, object) sprintf("%s <- llw_annotate(%s%s)", object, object, llw_code_arguments(step$parameters))
    ),
    metric = list(
      schema = llw_function_schema(llw_metrics, "metrics"),
      execute = function(state, step, modules) { state$metrics[[step$id]] <- rlang::exec(llw_metrics, x = state$dataset, !!!step$parameters); state },
      code = function(step, object) sprintf("metric_results[[%s]] <- llw_metrics(%s%s)", llw_dput(step$id), object, llw_code_arguments(step$parameters))
    ),
    visualization = list(
      schema = llw_function_schema(llw_plot),
      execute = function(state, step, modules) { state$plots[[step$id]] <- rlang::exec(llw_plot, x = state$dataset, !!!step$parameters); state },
      code = function(step, object) sprintf("plot_results[[%s]] <- llw_plot(%s%s)", llw_dput(step$id), object, llw_code_arguments(step$parameters))
    ),
    module = list(
      schema = list(required = character(), allowed = "module-defined"),
      execute = function(state, step, modules) {
        module <- modules[[step$module_id]]
        if (is.null(module)) {
          state$messages <- c(state$messages, paste0("Module `", step$module_id, "` is unavailable; step `", step$label, "` was skipped."))
        } else if (!is.null(step$module_version) && !identical(step$module_version, module$version)) {
          state$messages <- c(state$messages, paste0("Module `", step$module_id, "` version ", module$version, " does not match stored version ", step$module_version, "; the step was skipped."))
        } else {
          state$modules[[step$id]] <- llw_run_module(state$dataset, module, step$parameters)
        }
        state
      },
      code = function(step, object) sprintf(
        "module_results[[%s]] <- llw_run_module(%s, modules[[%s]], %s)",
        llw_dput(step$id), object, llw_dput(step$module_id), llw_dput(step$parameters)
      )
    )
  )
}

#' Inspect recipe step parameter schemas
#'
#' @param type Optional recipe step type.
#'
#' @return A named list of required and allowed parameters. Module parameters
#'   are defined by the startup-provided module.
#' @export
llw_recipe_schema <- function(type = NULL) {
  schemas <- lapply(llw_step_registry(), `[[`, "schema")
  if (is.null(type)) return(schemas)
  if (!type %in% names(schemas)) llw_abort(paste0("Unknown recipe step type `", type, "`."))
  schemas[[type]]
}

llw_validate_recipe_step <- function(step) {
  if (!inherits(step, "llw_recipe_step")) llw_abort("Every recipe step must be created by `llw_recipe_add()`.", "lightlogweb_invalid_recipe")
  registry <- llw_step_registry()
  if (!step$type %in% names(registry)) llw_abort(paste0("Unknown recipe step type `", step$type, "`."), "lightlogweb_invalid_recipe")
  if (step$type == "module" && (is.null(step$module_id) || !nzchar(step$module_id))) llw_abort("Module recipe steps require `module_id`.", "lightlogweb_invalid_recipe")
  llw_validate_parameter_schema(step$parameters, registry[[step$type]]$schema, step$type)
  invisible(step)
}

llw_code_arguments <- function(parameters) {
  if (!length(parameters)) return("")
  paste0(", ", paste(names(parameters), vapply(parameters, llw_dput, character(1)), sep = " = ", collapse = ", "))
}

llw_step_code <- function(step, object = "dataset") {
  llw_step_registry()[[step$type]]$code(step, object)
}

#' Build a reproducible R script from a recipe
#'
#' @param recipe An `llw_recipe`.
#' @param data_expression R code that produces an `llw_dataset`, or `NULL` for a
#'   documented placeholder.
#' @param include_session_info Include `sessionInfo()`.
#'
#' @return A character vector containing an executable R script.
#' @export
llw_build_script <- function(recipe,
                             data_expression = NULL,
                             include_session_info = TRUE) {
  llw_validate_recipe(recipe)
  lines <- c(
    "# LightLogWeb reproducible analysis",
    paste0("# Recipe schema: ", recipe$schema_version),
    "library(LightLogWeb)",
    "library(LightLogR)",
    "",
    if (is.null(data_expression)) {
      c(
        "# Replace this line with the project-relative source for your data.",
        "# data <- readRDS(\"data/light-data.rds\")",
        "stop(\"Define `data` as a data frame or llw_dataset before running this script.\")"
      )
    } else {
      paste0("data <- ", data_expression)
    },
    "if (!inherits(data, \"llw_dataset\")) data <- llw_dataset(data)",
    "dataset <- data",
    "metric_results <- list()",
    "plot_results <- list()",
    "module_results <- list()",
    "if (!exists(\"modules\")) modules <- list()",
    ""
  )

  enabled <- Filter(function(step) isTRUE(step$enabled), recipe$steps)
  for (step in enabled) {
    lines <- c(lines, paste0("# ", step$label), llw_step_code(step), "")
  }
  lines <- c(
    lines,
    "analysis <- list(dataset = dataset, metrics = metric_results, plots = plot_results, modules = module_results)",
    "analysis"
  )
  if (isTRUE(include_session_info)) lines <- c(lines, "", "sessionInfo()")
  parse(text = paste(lines, collapse = "\n"))
  lines
}
