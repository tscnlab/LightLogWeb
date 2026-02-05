#' Resolve the path to static web resources
#'
#' Tries package-installed resources first and then falls back to local
#' development/export layouts used by Shinylive builds.
#'
#' @param base_path Base path used to resolve local fallback directories.
#'   Defaults to the working directory.
#'
#' @returns A single string with the directory path.
resolve_www_path <- function(base_path = getwd()) {
  pkg_path <- system.file("app/www", package = "LightLogWeb")

  if (nzchar(pkg_path) && dir.exists(pkg_path)) {
    return(pkg_path)
  }

  candidate_paths <- c(
    file.path(base_path, "inst", "app", "www"),
    file.path(base_path, "app", "www"),
    file.path(base_path, "www")
  )

  found_path <- candidate_paths[dir.exists(candidate_paths)][1]

  if (is.na(found_path)) {
    rlang::abort(
      c(
        "Could not find LightLogWeb web resources.",
        i = "Checked package installation and local fallback paths."
      )
    )
  }

  found_path
}
