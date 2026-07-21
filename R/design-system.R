llw_asset_dependency_version <- "0.2.0"

lightlogweb_tokens <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)

  if (identical(mode, "light")) {
    return(c(
      bg_page = "#F6F8F4",
      bg_surface = "#FFFFFF",
      bg_subtle = "#E6EFEB",
      text = "#102A33",
      text_muted = "#52666D",
      border = "#B8C7C9",
      border_control = "#60767B",
      action = "#0B6675",
      on_action = "#FFFFFF",
      accent = "#C36A1D",
      focus = "#006D7A",
      success = "#1E6F4A",
      warning = "#8A4B09",
      danger = "#A33B3B",
      grid = "#B9C9CA"
    ))
  }

  c(
    bg_page = "#081A20",
    bg_surface = "#10282F",
    bg_subtle = "#16343A",
    text = "#EEF7F5",
    text_muted = "#B7C7C5",
    border = "#46636A",
    border_control = "#74939A",
    action = "#76D2DD",
    on_action = "#07171C",
    accent = "#F2B66D",
    focus = "#9CEBF2",
    success = "#83D5AA",
    warning = "#FFC47D",
    danger = "#FFAAAA",
    grid = "#46636A"
  )
}

lightlogweb_asset_dir <- function() {
  path <- system.file("app/www", package = "LightLogWeb")
  if (!nzchar(path) || !dir.exists(path)) {
    abort_llw(
      "LightLogWeb runtime assets are unavailable.",
      type = "resource",
      public_message = paste(
        "The LightLogWeb interface assets could not be loaded.",
        "Reinstall the package and try again."
      )
    )
  }
  path
}

lightlogweb_sass_file <- function() {
  path <- system.file(
    "app/scss/lightlogweb.scss",
    package = "LightLogWeb"
  )
  if (!nzchar(path) || !file.exists(path)) {
    abort_llw(
      "The LightLogWeb Sass source is unavailable.",
      type = "resource",
      public_message = paste(
        "The LightLogWeb interface theme could not be loaded.",
        "Reinstall the package and try again."
      )
    )
  }
  path
}

lightlogweb_asset_url <- function(path) {
  assert_scalar_string(path, "path")
  if (
    startsWith(path, "/") ||
      grepl("(^|/)\\.\\.(/|$)", path) ||
      grepl("[?#]", path)
  ) {
    abort_llw(
      "`path` must be a package-relative asset path without traversal.",
      type = "validation"
    )
  }

  paste0(
    "lightlogweb-assets-",
    llw_asset_dependency_version,
    "/",
    path
  )
}

lightlogweb_dependency <- function() {
  htmltools::htmlDependency(
    name = "lightlogweb-assets",
    version = llw_asset_dependency_version,
    src = c(file = lightlogweb_asset_dir()),
    stylesheet = "css/lightlogweb-fonts.css",
    all_files = TRUE
  )
}

lightlogweb_theme <- function() {
  tokens <- lightlogweb_tokens("light")
  font_stack <- bslib::font_collection(
    "Source Sans 3",
    "system-ui",
    "-apple-system",
    "BlinkMacSystemFont",
    "Segoe UI",
    "sans-serif"
  )

  theme <- bslib::bs_theme(
    version = 5,
    bg = unname(tokens[["bg_page"]]),
    fg = unname(tokens[["text"]]),
    primary = unname(tokens[["action"]]),
    secondary = unname(tokens[["text_muted"]]),
    success = unname(tokens[["success"]]),
    info = unname(tokens[["action"]]),
    warning = unname(tokens[["warning"]]),
    danger = unname(tokens[["danger"]]),
    base_font = font_stack,
    heading_font = font_stack,
    code_font = font_stack
  )

  theme <- bslib::bs_add_variables(
    theme,
    "body-bg" = unname(tokens[["bg_page"]]),
    "body-color" = unname(tokens[["text"]]),
    "border-color" = unname(tokens[["border"]]),
    "input-border-color" = unname(tokens[["border_control"]]),
    "card-bg" = unname(tokens[["bg_surface"]]),
    "card-border-color" = unname(tokens[["border"]]),
    "card-cap-bg" = "transparent",
    "link-color" = unname(tokens[["action"]]),
    "focus-ring-width" = "0.1875rem",
    "focus-ring-opacity" = "1",
    "focus-ring-color" = unname(tokens[["focus"]]),
    "border-radius" = "0.375rem",
    "border-radius-lg" = "0.75rem",
    "border-radius-xl" = "1.125rem",
    "box-shadow" = "0 0.75rem 2.5rem rgba(16, 42, 51, 0.12)"
  )

  bslib::bs_add_rules(theme, sass::sass_file(lightlogweb_sass_file()))
}

lightlogweb_head <- function() {
  head <- tags$head(
    tags$meta(name = "color-scheme", content = "light dark"),
    tags$meta(name = "theme-color", content = "#F6F8F4"),
    tags$link(
      rel = "icon",
      type = "image/svg+xml",
      href = lightlogweb_asset_url("brand/favicon.svg")
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = lightlogweb_asset_url("brand/favicon-32.png")
    ),
    tags$link(
      rel = "apple-touch-icon",
      sizes = "180x180",
      href = lightlogweb_asset_url("brand/apple-touch-icon.png")
    )
  )

  htmltools::attachDependencies(head, lightlogweb_dependency())
}

lightlogweb_page <- function(page) {
  wrapper <- tags$div(class = "llw-app", page)
  htmltools::attachDependencies(wrapper, lightlogweb_dependency())
}

lightlogweb_skip_link <- function(target = "llw-main-content") {
  assert_scalar_string(target, "target")
  tags$a(
    "Skip to main content",
    class = "llw-skip-link",
    href = paste0("#", target)
  )
}

lightlogweb_mark <- function(
  variant = c("adaptive", "dark", "monochrome", "reversed"),
  size = 40,
  decorative = FALSE,
  class = NULL
) {
  variant <- match.arg(variant)
  if (
    !is.numeric(size) ||
      length(size) != 1L ||
      is.na(size) ||
      size <= 0
  ) {
    abort_llw("`size` must be one positive number.", type = "validation")
  }

  filename <- switch(
    variant,
    adaptive = "lightlogweb-mark.svg",
    dark = "lightlogweb-mark-dark.svg",
    monochrome = "lightlogweb-mark-monochrome.svg",
    reversed = "lightlogweb-mark-reversed.svg"
  )
  attrs <- list(
    src = lightlogweb_asset_url(file.path("brand", filename)),
    class = paste(c("llw-mark", class), collapse = " "),
    width = size,
    height = round(size * 1.1),
    draggable = "false"
  )
  if (isTRUE(decorative)) {
    attrs$alt <- ""
    attrs$`aria-hidden` <- "true"
  } else {
    attrs$alt <- "LightLogWeb measured day arc mark"
  }

  htmltools::attachDependencies(
    do.call(tags$img, attrs),
    lightlogweb_dependency()
  )
}

lightlogweb_wordmark <- function(compact = FALSE) {
  tags$span(
    class = paste(
      "llw-wordmark",
      if (isTRUE(compact)) "llw-wordmark--compact" else NULL
    ),
    `aria-label` = "LightLogWeb",
    lightlogweb_mark(size = if (isTRUE(compact)) 28 else 34, decorative = TRUE),
    tags$span(class = "llw-wordmark__text", "LightLogWeb")
  )
}

llw_view_header <- function(eyebrow, title, description = NULL) {
  assert_scalar_string(eyebrow, "eyebrow")
  assert_scalar_string(title, "title")
  if (!is.null(description)) {
    assert_scalar_string(description, "description")
  }

  tags$header(
    class = "llw-view-header",
    tags$p(class = "llw-eyebrow", eyebrow),
    tags$h1(title),
    if (!is.null(description)) tags$p(class = "llw-view-lede", description)
  )
}

llw_status_spec <- function(state) {
  assert_scalar_string(state, "state")
  specs <- list(
    idle = list(label = "Ready", icon = "circle", tone = "muted"),
    queued = list(
      label = "Queued",
      icon = "hourglass-split",
      tone = "action"
    ),
    running = list(
      label = "Running",
      icon = "arrow-repeat",
      tone = "action"
    ),
    finalizing = list(
      label = "Finalizing",
      icon = "three-dots",
      tone = "action"
    ),
    complete = list(
      label = "Complete",
      icon = "check-circle",
      tone = "success"
    ),
    warning = list(
      label = "Warning",
      icon = "exclamation-triangle",
      tone = "warning"
    ),
    error = list(label = "Error", icon = "x-octagon", tone = "danger"),
    cancelled = list(
      label = "Cancelled",
      icon = "slash-circle",
      tone = "muted"
    ),
    stale = list(
      label = "Stale result",
      icon = "clock-history",
      tone = "warning"
    )
  )
  spec <- specs[[state]]
  if (is.null(spec)) {
    abort_llw(
      paste0("Unknown task state `", state, "`."),
      type = "validation"
    )
  }
  spec
}

llw_status_callout <- function(
  state,
  message,
  heading = NULL,
  action = NULL,
  live = FALSE,
  compact = FALSE
) {
  spec <- llw_status_spec(state)
  if (is.null(heading)) {
    heading <- spec$label
  }
  assert_scalar_string(heading, "heading")

  attrs <- list(
    class = paste(
      "llw-status",
      paste0("llw-status--", spec$tone),
      if (isTRUE(compact)) "llw-status--compact" else NULL
    )
  )
  if (isTRUE(live)) {
    attrs$role <- "status"
    attrs$`aria-live` <- "polite"
    attrs$`aria-atomic` <- "true"
  }

  do.call(
    tags$div,
    c(
      attrs,
      list(
        tags$div(
          class = "llw-status__icon",
          `aria-hidden` = "true",
          bsicons::bs_icon(spec$icon)
        ),
        tags$div(
          class = "llw-status__body",
          tags$div(class = "llw-status__heading", heading),
          tags$div(class = "llw-status__message", message),
          if (!is.null(action)) {
            tags$div(class = "llw-status__action", action)
          }
        )
      )
    )
  )
}

llw_task_status <- function(status, context = "Task") {
  if (!is.list(status) || is.null(status$state)) {
    abort_llw("`status` must be a task-status value.", type = "validation")
  }
  assert_scalar_string(context, "context")
  spec <- llw_status_spec(status$state)
  message <- status$message %||%
    switch(
      status$state,
      idle = "The action is available.",
      queued = "The request is waiting to start.",
      running = "Work is in progress.",
      finalizing = "Results are being assembled.",
      complete = "The result was applied.",
      warning = "The result needs attention.",
      error = "The operation failed; no unsafe result was applied.",
      cancelled = "Cancelled; no changes were applied.",
      stale = "Stale result not applied."
    )

  llw_status_callout(
    state = status$state,
    heading = paste(context, spec$label),
    message = message,
    live = TRUE,
    compact = TRUE
  )
}

lightlogweb_plot_colors <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)
  tokens <- lightlogweb_tokens(mode)
  c(
    primary = tokens[["action"]],
    comparison = tokens[["accent"]],
    start = tokens[["accent"]],
    end = tokens[["success"]],
    reference = if (identical(mode, "light")) {
      tokens[["border_control"]]
    } else {
      tokens[["text_muted"]]
    },
    gap = tokens[["danger"]],
    text = tokens[["text"]],
    grid = tokens[["grid"]],
    interval = tokens[["bg_subtle"]],
    surface = tokens[["bg_surface"]]
  )
}

lightlogweb_plot_theme <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)
  colors <- lightlogweb_plot_colors(mode)

  ggplot2::theme_minimal(base_family = "Source Sans 3", base_size = 12) +
    ggplot2::theme(
      text = ggplot2::element_text(color = colors[["text"]]),
      plot.background = ggplot2::element_rect(
        fill = colors[["surface"]],
        color = NA
      ),
      panel.background = ggplot2::element_rect(
        fill = colors[["surface"]],
        color = NA
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        color = colors[["grid"]],
        linewidth = 0.35
      ),
      axis.text = ggplot2::element_text(color = colors[["text"]]),
      axis.title = ggplot2::element_text(
        color = colors[["text"]],
        face = "plain"
      ),
      legend.background = ggplot2::element_rect(
        fill = colors[["surface"]],
        color = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = colors[["surface"]],
        color = NA
      ),
      strip.background = ggplot2::element_rect(
        fill = colors[["interval"]],
        color = NA
      ),
      strip.text = ggplot2::element_text(color = colors[["text"]]),
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(size = 10, hjust = 0),
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

llw_relative_luminance <- function(color) {
  if (
    !is.character(color) ||
      length(color) != 1L ||
      is.na(color) ||
      !grepl("^#[0-9A-Fa-f]{6}$", color)
  ) {
    abort_llw(
      "`color` must be one six-digit hexadecimal color.",
      type = "validation"
    )
  }
  rgb <- grDevices::col2rgb(color)[, 1L] / 255
  linear <- ifelse(
    rgb <= 0.04045,
    rgb / 12.92,
    ((rgb + 0.055) / 1.055)^2.4
  )
  sum(linear * c(0.2126, 0.7152, 0.0722))
}

llw_contrast_ratio <- function(foreground, background) {
  luminance <- c(
    llw_relative_luminance(foreground),
    llw_relative_luminance(background)
  )
  (max(luminance) + 0.05) / (min(luminance) + 0.05)
}
