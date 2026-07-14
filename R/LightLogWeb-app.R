#' Launch LightLogWeb
#'
#' `LightLogWeb()` launches the production Shiny application. Custom analyses
#' can be supplied at startup as validated [llw_module()] specifications.
#'
#' @param modules A list of custom modules created with [llw_module()].
#' @param project Optional `.llw` project path or an object returned by
#'   [llw_load_project()].
#' @param ... Arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object.
#' @export
#'
#' @examples
#' if (interactive()) {
#'   LightLogWeb()
#' }
LightLogWeb <- function(modules = list(), project = NULL, ...) {
  old <- options(
    shiny.maxRequestSize = 100 * 1024^2,
    spinner.caption = "Calculating..."
  )
  on.exit(options(old), add = TRUE)
  llw_shiny_app(modules = modules, project = project, ...)
}
