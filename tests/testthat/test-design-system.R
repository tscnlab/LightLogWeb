read_png_dimensions <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  bytes <- readBin(connection, what = "raw", n = 24L)
  if (length(bytes) != 24L) {
    stop("PNG header is incomplete.", call. = FALSE)
  }
  signature <- as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))
  if (!identical(bytes[seq_along(signature)], signature)) {
    stop("File does not have a PNG signature.", call. = FALSE)
  }
  decode_uint32 <- function(value) {
    sum(as.integer(value) * 256^(3:0))
  }
  c(
    width = decode_uint32(bytes[17:20]),
    height = decode_uint32(bytes[21:24])
  )
}

test_that("semantic design tokens are complete and stable", {
  expected_names <- c(
    "bg_page",
    "bg_surface",
    "bg_subtle",
    "text",
    "text_muted",
    "border",
    "border_control",
    "action",
    "on_action",
    "accent",
    "focus",
    "success",
    "warning",
    "danger",
    "grid"
  )

  light <- lightlogweb_tokens("light")
  dark <- lightlogweb_tokens("dark")
  expect_named(light, expected_names)
  expect_named(dark, expected_names)
  expect_identical(unname(light[["action"]]), "#0B6675")
  expect_identical(unname(dark[["action"]]), "#76D2DD")
  expect_identical(unname(light[["accent"]]), "#C36A1D")
  expect_identical(unname(dark[["accent"]]), "#F2B66D")
})

test_that("critical light and dark contrast pairs pass their thresholds", {
  text_pairs <- list(
    c("text", "bg_page"),
    c("text_muted", "bg_surface"),
    c("action", "bg_surface"),
    c("on_action", "action")
  )
  graphical_pairs <- list(
    c("border_control", "bg_surface"),
    c("focus", "bg_page"),
    c("success", "bg_surface"),
    c("warning", "bg_surface"),
    c("danger", "bg_surface")
  )

  for (mode in c("light", "dark")) {
    tokens <- lightlogweb_tokens(mode)
    for (pair in text_pairs) {
      expect_gte(
        llw_contrast_ratio(tokens[[pair[[1L]]]], tokens[[pair[[2L]]]]),
        4.5
      )
    }
    for (pair in graphical_pairs) {
      expect_gte(
        llw_contrast_ratio(tokens[[pair[[1L]]]], tokens[[pair[[2L]]]]),
        3
      )
    }
  }
})

test_that("the bslib theme compiles from the packaged Sass source", {
  theme <- lightlogweb_theme()
  expect_s3_class(theme, "bs_theme")
  expect_identical(
    unname(bslib::bs_get_variables(theme, "primary")),
    "#0B6675"
  )
  expect_true(file.exists(lightlogweb_sass_file()))

  dependencies <- bslib::bs_theme_dependencies(
    theme,
    precompiled = FALSE
  )
  expect_true(length(dependencies) > 0L)
})

test_that("wide dashboard table headers stay horizontally readable", {
  source <- paste(readLines(lightlogweb_sass_file()), collapse = "\n")
  expected_rule <- paste(
    ".llw-dashboard-data-view table.dataTable thead th {",
    "  overflow-wrap: normal;",
    "  white-space: nowrap;",
    "}",
    sep = "\n"
  )

  expect_match(source, expected_rule, fixed = TRUE)
})

test_that("dashboard layout and quality tones retain owner-review fixes", {
  source <- paste(readLines(lightlogweb_sass_file()), collapse = "\n")

  expect_match(
    source,
    ".llw-main-shell.llw-dashboard-shell {\n  width: calc(100% - 2rem);\n  max-width: none;",
    fixed = TRUE
  )
  expect_match(
    source,
    ".llw-app .llw-dashboard-value-box--success",
    fixed = TRUE
  )
  expect_match(
    source,
    ".llw-app .llw-dashboard-value-box--warning",
    fixed = TRUE
  )
  expect_match(
    source,
    ".llw-dashboard-data-view .dataTables_length {\n  display: none;",
    fixed = TRUE
  )
})

test_that("brand helpers retain accessible semantics", {
  page <- htmltools::renderTags(
    lightlogweb_page(tags$main(id = "test-main", "Content"))
  )$html
  expect_match(page, 'class="llw-app"', fixed = TRUE)
  expect_match(page, '<main id="test-main">', fixed = TRUE)

  wordmark <- htmltools::renderTags(lightlogweb_wordmark())$html
  expect_match(wordmark, 'aria-label="LightLogWeb"', fixed = TRUE)
  expect_match(wordmark, 'alt=""', fixed = TRUE)
  expect_match(wordmark, "lightlogweb-mark.svg", fixed = TRUE)

  status <- htmltools::renderTags(llw_status_callout(
    "error",
    "Review the input and retry.",
    live = TRUE
  ))$html
  expect_match(status, 'role="status"', fixed = TRUE)
  expect_match(status, 'aria-live="polite"', fixed = TRUE)
  expect_match(status, "llw-status--danger", fixed = TRUE)
  expect_match(status, "Error", fixed = TRUE)
  expect_match(status, "Review the input and retry.", fixed = TRUE)

  heading <- htmltools::renderTags(llw_view_header(
    "Evidence",
    "Inspect the source",
    "Review provenance first."
  ))$html
  h1_matches <- regmatches(
    heading,
    gregexpr("<h1", heading, fixed = TRUE)
  )[[1L]]
  expect_length(h1_matches, 1L)
  expect_match(heading, "Review provenance first.", fixed = TRUE)
})

test_that("all task states include text and an icon contract", {
  states <- c(
    "idle",
    "queued",
    "running",
    "finalizing",
    "complete",
    "warning",
    "error",
    "cancelled",
    "stale"
  )
  specs <- lapply(states, llw_status_spec)
  expect_true(all(vapply(specs, function(x) nzchar(x$label), logical(1))))
  expect_true(all(vapply(specs, function(x) nzchar(x$icon), logical(1))))
  expect_true(all(vapply(specs, function(x) nzchar(x$tone), logical(1))))
  expect_error(llw_status_spec("unknown"), class = "llw_validation_error")
})

test_that("packaged identity assets are present and correctly reduced", {
  asset <- function(name) {
    file.path(lightlogweb_asset_dir(), "brand", name)
  }
  required_svg <- c(
    "lightlogweb-mark.svg",
    "lightlogweb-mark-dark.svg",
    "lightlogweb-mark-monochrome.svg",
    "lightlogweb-mark-reversed.svg",
    "lightlogweb-wordmark-horizontal.svg",
    "lightlogweb-wordmark-stacked.svg",
    "favicon.svg"
  )
  expect_true(all(file.exists(vapply(required_svg, asset, character(1)))))

  dimensions <- list(
    "favicon-16.png" = c(width = 16, height = 16),
    "favicon-32.png" = c(width = 32, height = 32),
    "favicon-48.png" = c(width = 48, height = 48),
    "apple-touch-icon.png" = c(width = 180, height = 180),
    "icon-192.png" = c(width = 192, height = 192),
    "icon-512.png" = c(width = 512, height = 512),
    "lightlogweb-mark-1024.png" = c(width = 1024, height = 1024),
    "lightlogweb-mark-2048.png" = c(width = 2048, height = 2048)
  )
  for (name in names(dimensions)) {
    expect_identical(read_png_dimensions(asset(name)), dimensions[[name]])
  }

  mark <- paste(
    readLines(asset("lightlogweb-mark.svg"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(mark, "llw-mark__field", fixed = TRUE)
  expect_match(mark, "llw-mark__arc", fixed = TRUE)
  expect_match(mark, "llw-mark__sun", fixed = TRUE)
  expect_identical(
    lengths(regmatches(mark, gregexpr("llw-mark__tick ", mark, fixed = TRUE))),
    6L
  )
})

test_that("Source Sans 3 is self-hosted with its license", {
  font_dir <- file.path(lightlogweb_asset_dir(), "fonts")
  fonts <- list.files(font_dir, pattern = "\\.woff2$", full.names = TRUE)
  expect_length(fonts, 6L)
  expect_true(all(file.info(fonts)$size > 0L))

  license <- paste(
    readLines(file.path(font_dir, "OFL.txt"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(license, "SIL OPEN FONT LICENSE Version 1.1", fixed = TRUE)

  dependency <- lightlogweb_dependency()
  expect_identical(dependency$name, "lightlogweb-assets")
  expect_identical(dependency$version, llw_asset_dependency_version)
  expect_identical(dependency$stylesheet, "css/lightlogweb-fonts.css")
})

test_that("asset URLs reject traversal and remain dependency relative", {
  expect_identical(
    lightlogweb_asset_url("brand/favicon.svg"),
    paste0(
      "lightlogweb-assets-",
      llw_asset_dependency_version,
      "/brand/favicon.svg"
    )
  )
  expect_error(
    lightlogweb_asset_url("../private.txt"),
    class = "llw_validation_error"
  )
  expect_error(
    lightlogweb_asset_url("brand/favicon.svg?cache=off"),
    class = "llw_validation_error"
  )
})

test_that("plot styling uses the same semantic palette", {
  light <- lightlogweb_plot_colors("light")
  dark <- lightlogweb_plot_colors("dark")
  expect_identical(unname(light[["primary"]]), "#0B6675")
  expect_identical(unname(dark[["primary"]]), "#76D2DD")
  expect_identical(unname(light[["comparison"]]), "#C36A1D")
  expect_identical(unname(light[["gap"]]), "#A33B3B")
  expect_identical(unname(dark[["gap"]]), "#FFAAAA")
  expect_s3_class(lightlogweb_plot_theme("light"), "theme")
  expect_s3_class(lightlogweb_plot_theme("dark"), "theme")
})
