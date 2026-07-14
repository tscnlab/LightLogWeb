# Attributed package test fixtures ------------------------------------------

#' Load a bundled MeLiDos test fixture
#'
#' @param site One of TUM, KNUST, UCR, or RISE.
#' @param intervals Return the corresponding wear and sleep intervals instead
#'   of light measurements.
#'
#' @return A deidentified tibble. Source attribution and checksums are stored
#'   in `inst/extdata/melidos/manifest.csv`.
#' @export
llw_fixture <- function(site = c("TUM", "KNUST", "UCR", "RISE"), intervals = FALSE) {
  site <- match.arg(site)
  suffix <- if (isTRUE(intervals)) "intervals" else "light"
  path <- system.file("extdata", "melidos", paste0(tolower(site), "-", suffix, ".rds"), package = "LightLogWeb")
  if (!nzchar(path)) {
    path <- file.path("inst", "extdata", "melidos", paste0(tolower(site), "-", suffix, ".rds"))
  }
  if (!file.exists(path)) llw_abort("Bundled MeLiDos fixtures are unavailable in this installation.")
  readRDS(path)
}
