# Offline full-app integration harness. The production parent composes the same
# modules; only initial records, the registry loader and worker fixtures differ.
source("dev/milestone-8-prepare-app.R", local = TRUE)

milestone_8_integration_app <- function(profile = "local", workers = 1L) {
  lightlogweb_app(
    profile = profile,
    workers = workers,
    initial_records = m8_showcase_records(),
    initial_panel = "dashboard",
    registry_loader = function(...)
      stop("Live GLC discovery is disabled in this offline showcase."),
    preparation_worker = m8_showcase_worker
  )
}
