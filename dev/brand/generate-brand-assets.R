script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) != 1L) {
    stop(
      "Run this file with `Rscript dev/brand/generate-brand-assets.R`.",
      call. = FALSE
    )
  }
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
}

project_root <- normalizePath(
  file.path(dirname(script_path()), "..", ".."),
  mustWork = TRUE
)
master_path <- file.path(
  project_root,
  "dev",
  "brand",
  "lightlogweb-logo-master.svg"
)
brand_dir <- file.path(project_root, "inst", "app", "www", "brand")
font_dir <- file.path(project_root, "inst", "app", "www", "fonts")
pkgdown_dir <- file.path(project_root, "man", "figures")

dir.create(brand_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pkgdown_dir, recursive = TRUE, showWarnings = FALSE)

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

extract_segment <- function(text, name) {
  pattern <- paste0(
    "(?s).*<!-- BEGIN ",
    name,
    " -->\\s*(.*?)\\s*<!-- END ",
    name,
    " -->.*"
  )
  value <- sub(pattern, "\\1", text, perl = TRUE)
  if (identical(value, text)) {
    stop("Could not extract `", name, "` from the logo master.", call. = FALSE)
  }
  value
}

write_text <- function(path, text) {
  writeLines(sub("\\n+$", "", text), path, useBytes = TRUE)
}

master <- read_text(master_path)
mark <- extract_segment(master, "MARK")
wordmark <- extract_segment(master, "WORDMARK")
mark <- gsub(' inkscape:label="[^"]*"', "", mark, perl = TRUE)

palette_css <- function(
  variant = c("adaptive", "light", "dark", "monochrome", "reversed")
) {
  variant <- match.arg(variant)
  rules <- function(surface, field, horizon, action, sun, text) {
    paste0(
      ".llw-mark__surface{fill:",
      surface,
      "}",
      ".llw-mark__field{fill:",
      field,
      "}",
      ".llw-mark__horizon{stroke:",
      horizon,
      "}",
      ".llw-mark__outline,.llw-mark__arc{stroke:",
      action,
      "}",
      ".llw-mark__tick{stroke:",
      action,
      "}",
      ".llw-mark__sun{fill:",
      sun,
      "}",
      ".llw-wordmark-source{fill:",
      text,
      "}"
    )
  }
  light <- rules(
    "#FFFFFF",
    "#E6EFEB",
    "#B8C7C9",
    "#0B6675",
    "#C36A1D",
    "#102A33"
  )
  dark <- rules(
    "#10282F",
    "#16343A",
    "#46636A",
    "#76D2DD",
    "#F2B66D",
    "#EEF7F5"
  )
  mono <- paste(
    rules(
      "transparent",
      "currentColor",
      "currentColor",
      "currentColor",
      "currentColor",
      "currentColor"
    ),
    ".llw-mark__field{opacity:.12}.llw-mark__horizon{opacity:.45}"
  )
  reversed <- paste(
    ":root{color:#EEF7F5}",
    rules(
      "transparent",
      "#EEF7F5",
      "#EEF7F5",
      "#EEF7F5",
      "#EEF7F5",
      "#EEF7F5"
    ),
    ".llw-mark__field{opacity:.12}.llw-mark__horizon{opacity:.45}"
  )
  if (identical(variant, "adaptive")) {
    return(paste(
      light,
      "@media (prefers-color-scheme: dark) {",
      dark,
      "}"
    ))
  }
  switch(
    variant,
    light = light,
    dark = dark,
    monochrome = mono,
    reversed = reversed
  )
}

base_css <- paste(
  ".llw-mark__horizon{fill:none;stroke-width:2}",
  ".llw-mark__outline,.llw-mark__arc{fill:none;stroke-linecap:round;stroke-linejoin:round;stroke-width:5}",
  ".llw-mark__tick{stroke-linecap:round;stroke-width:2}",
  ".llw-wordmark-source{font-family:'Source Sans 3',sans-serif;font-size:64px;font-weight:600;letter-spacing:-1px}"
)

font_css <- function() {
  font_path <- file.path(font_dir, "source-sans-3-latin-600-normal.woff2")
  if (
    !requireNamespace("base64enc", quietly = TRUE) || !file.exists(font_path)
  ) {
    return("")
  }
  encoded <- base64enc::base64encode(font_path)
  paste0(
    "@font-face{font-family:'Source Sans 3';font-style:normal;font-weight:600;",
    "src:url(data:font/woff2;base64,",
    encoded,
    ") format('woff2')}"
  )
}

svg_document <- function(
  view_box,
  content,
  title,
  description,
  variant,
  extra_css = "",
  embed_font = FALSE
) {
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"",
    view_box,
    "\" role=\"img\" aria-labelledby=\"asset-title asset-description\">\n",
    "<title id=\"asset-title\">",
    title,
    "</title>\n",
    "<desc id=\"asset-description\">",
    description,
    "</desc>\n",
    "<metadata>Derived from dev/brand/lightlogweb-logo-master.svg; schema 1.0.0; generated 2026-07-20.</metadata>\n",
    "<style>",
    palette_css(variant),
    base_css,
    if (isTRUE(embed_font)) font_css() else "",
    extra_css,
    "</style>\n",
    content,
    "\n</svg>\n"
  )
}

write_mark <- function(filename, variant) {
  write_text(
    file.path(brand_dir, filename),
    svg_document(
      "0 0 160 176",
      mark,
      "LightLogWeb measured day arc mark",
      "A hexagonal day and night window with a measured sun path and solar marker.",
      variant
    )
  )
}

mark_variants <- c(
  "lightlogweb-mark.svg" = "adaptive",
  "lightlogweb-mark-light.svg" = "light",
  "lightlogweb-mark-dark.svg" = "dark",
  "lightlogweb-mark-monochrome.svg" = "monochrome",
  "lightlogweb-mark-reversed.svg" = "reversed"
)
invisible(Map(write_mark, names(mark_variants), unname(mark_variants)))

horizontal_content <- paste(mark, wordmark, sep = "\n")
stacked_wordmark <- wordmark
stacked_wordmark <- sub('x="190"', 'x="180"', stacked_wordmark, fixed = TRUE)
stacked_wordmark <- sub('y="112"', 'y="272"', stacked_wordmark, fixed = TRUE)
stacked_wordmark <- sub(
  'font-size:64px',
  'font-size:54px',
  stacked_wordmark,
  fixed = TRUE
)
stacked_wordmark <- sub(
  'class="llw-wordmark-source"',
  'class="llw-wordmark-source" text-anchor="middle"',
  stacked_wordmark,
  fixed = TRUE
)
stacked_content <- paste0(
  '<g transform="translate(100 0)">',
  mark,
  "</g>\n",
  stacked_wordmark
)

write_lockup <- function(stem, content, view_box, variant, extra_css = "") {
  write_text(
    file.path(brand_dir, paste0(stem, ".svg")),
    svg_document(
      view_box,
      content,
      "LightLogWeb wordmark",
      "The measured day arc mark paired with the LightLogWeb name.",
      variant,
      extra_css = extra_css,
      embed_font = TRUE
    )
  )
}

for (variant in c("adaptive", "light", "dark", "monochrome", "reversed")) {
  suffix <- if (identical(variant, "adaptive")) "" else paste0("-", variant)
  write_lockup(
    paste0("lightlogweb-wordmark-horizontal", suffix),
    horizontal_content,
    "0 0 640 176",
    variant
  )
  write_lockup(
    paste0("lightlogweb-wordmark-stacked", suffix),
    stacked_content,
    "0 0 360 300",
    variant,
    extra_css = ".llw-wordmark-source{font-size:54px}"
  )
}

invisible(file.copy(
  file.path(brand_dir, "lightlogweb-wordmark-horizontal.svg"),
  file.path(pkgdown_dir, "logo.svg"),
  overwrite = TRUE
))

minor_tick_pattern <- paste0(
  '<line class="llw-mark__tick llw-mark__tick--minor"[^>]*/>\\s*'
)
medium_mark <- gsub(minor_tick_pattern, "", mark, perl = TRUE)
simplified_mark <- gsub(
  '<path class="llw-mark__field"[^>]*/>\\s*|<path class="llw-mark__horizon"[^>]*/>\\s*|<g id="llw-mark-ticks".*?</g>\\s*',
  "",
  mark,
  perl = TRUE
)

square_content <- function(mark_content, background = NULL) {
  background_rect <- if (is.null(background)) {
    ""
  } else {
    paste0('<rect width="176" height="176" fill="', background, '"/>')
  }
  paste0(
    background_rect,
    '<g transform="translate(8 0)">',
    mark_content,
    "</g>"
  )
}

favicon_svg <- svg_document(
  "0 0 176 176",
  square_content(medium_mark),
  "LightLogWeb favicon",
  "A reduced measured day arc mark for small browser contexts.",
  "adaptive"
)
write_text(file.path(brand_dir, "favicon.svg"), favicon_svg)

square_light_path <- file.path(brand_dir, "lightlogweb-mark-square.svg")
square_dark_path <- file.path(brand_dir, "lightlogweb-mark-square-dark.svg")
write_text(
  square_light_path,
  svg_document(
    "0 0 176 176",
    square_content(mark),
    "LightLogWeb square mark",
    "The full measured day arc mark on a transparent square artboard.",
    "light"
  )
)
write_text(
  square_dark_path,
  svg_document(
    "0 0 176 176",
    square_content(mark, "#081A20"),
    "LightLogWeb square dark mark",
    "The full measured day arc mark on the approved night background.",
    "dark"
  )
)

render_png <- function(svg, output, size) {
  if (
    requireNamespace("magick", quietly = TRUE) &&
      requireNamespace("rsvg", quietly = TRUE)
  ) {
    image <- magick::image_read_svg(svg, width = size, height = size)
    magick::image_write(image, path = output, format = "png", density = 144)
    info <- magick::image_info(image)
    if (info$width[[1L]] != size || info$height[[1L]] != size) {
      stop("Unexpected dimensions for `", output, "`.", call. = FALSE)
    }
    return(invisible(output))
  }

  node <- Sys.getenv("LIGHTLOGWEB_NODE", unset = Sys.which("node"))
  renderer <- file.path(project_root, "dev", "brand", "render-brand-png.cjs")
  if (!nzchar(node) || !file.exists(renderer)) {
    stop(
      "PNG derivation requires R packages `magick` + `rsvg`, or Node.js + `sharp`.",
      call. = FALSE
    )
  }
  status <- system2(
    node,
    c(renderer, svg, output, as.character(size)),
    stdout = TRUE,
    stderr = TRUE
  )
  if (!identical(attr(status, "status"), NULL) || !file.exists(output)) {
    stop(
      "Node PNG renderer failed for `",
      output,
      "`: ",
      paste(status, collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(output)
}

temp_svg <- function(content, variant) {
  path <- tempfile(fileext = ".svg")
  write_text(
    path,
    svg_document(
      "0 0 176 176",
      square_content(content),
      "LightLogWeb reduced mark",
      "A size-specific reduction of the measured day arc mark.",
      variant
    )
  )
  path
}

favicon_16_svg <- temp_svg(simplified_mark, "light")
favicon_32_svg <- temp_svg(medium_mark, "light")
favicon_48_svg <- temp_svg(mark, "light")
on.exit(unlink(c(favicon_16_svg, favicon_32_svg, favicon_48_svg)), add = TRUE)

render_png(favicon_16_svg, file.path(brand_dir, "favicon-16.png"), 16)
render_png(favicon_32_svg, file.path(brand_dir, "favicon-32.png"), 32)
render_png(favicon_48_svg, file.path(brand_dir, "favicon-48.png"), 48)
render_png(square_light_path, file.path(brand_dir, "apple-touch-icon.png"), 180)
render_png(square_light_path, file.path(brand_dir, "icon-192.png"), 192)
render_png(square_light_path, file.path(brand_dir, "icon-512.png"), 512)
render_png(
  square_light_path,
  file.path(brand_dir, "lightlogweb-mark-1024.png"),
  1024
)
render_png(
  square_light_path,
  file.path(brand_dir, "lightlogweb-mark-2048.png"),
  2048
)
render_png(
  square_dark_path,
  file.path(brand_dir, "lightlogweb-mark-dark-1024.png"),
  1024
)
render_png(
  square_dark_path,
  file.path(brand_dir, "lightlogweb-mark-dark-2048.png"),
  2048
)

asset_paths <- c(
  master_path,
  list.files(brand_dir, full.names = TRUE),
  file.path(pkgdown_dir, "logo.svg"),
  list.files(font_dir, full.names = TRUE)
)
asset_paths <- sort(unique(asset_paths[file.exists(asset_paths)]))
relative_paths <- substring(asset_paths, nchar(project_root) + 2L)
checksums <- vapply(
  asset_paths,
  digest::digest,
  character(1),
  file = TRUE,
  algo = "sha256"
)
checksum_lines <- paste(checksums, relative_paths, sep = "  ")
writeLines(
  checksum_lines,
  file.path(project_root, "dev", "brand", "brand-checksums.sha256"),
  useBytes = TRUE
)

provenance <- c(
  "# LightLogWeb brand provenance",
  "",
  "- Identity: **Measured Day Arc**, selected from the Circadian Field direction.",
  "- Canonical editable artwork: `dev/brand/lightlogweb-logo-master.svg`.",
  "- Derivation command: `Rscript --vanilla dev/brand/generate-brand-assets.R`.",
  "- Generator: `dev/brand/generate-brand-assets.R`.",
  "- Font: Source Sans 3, pinned to `@fontsource/source-sans-3` 5.2.9.",
  "- Font license: SIL Open Font License 1.1, shipped as `inst/app/www/fonts/OFL.txt`.",
  "- LightLogR heritage reference: `dev/design-workshop/public/reference-lightlogr-logo.png`, used only to maintain family kinship through a hexagonal silhouette and the light-exposure subject.",
  "- Institutional and funder marks are not part of the product identity.",
  "- Generated on 2026-07-20; all shipping derivatives are recreated from the master by the generator.",
  "",
  "## Asset integrity",
  "",
  "SHA-256 checksums are recorded in `dev/brand/brand-checksums.sha256`:",
  "",
  "```text",
  checksum_lines,
  "```",
  "",
  "## Review boundary",
  "",
  "The mark was compared with the current LightLogR identity for deliberate separation: it contains no room perspective, barcode, extruded planes, or construction geometry.",
  "",
  "A preliminary scan on 2026-07-20 found no exact `LightLogWeb` result in general web searches or exact-name searches targeted at WIPO, EUIPO, and DPMA pages. Broader searches found unrelated `LightLog` products in film photography, software logging, and light-logger research, so the shared root is not exclusive. A visual-concept scan also found that hexagon-and-sunrise marks are common; LightLogWeb's differentiation therefore depends on the measured arc, perpendicular tick rhythm, flat day/night field, and full wordmark.",
  "",
  "This scan reduces obvious collision risk only. Search engines do not establish register availability, registry coverage changes, and WIPO recommends checking relevant national and regional offices. Obtain a professional clearance before a trademark filing or consequential public launch."
)
writeLines(
  provenance,
  file.path(project_root, "dev", "brand", "brand-provenance.md"),
  useBytes = TRUE
)

message("Generated ", length(asset_paths), " brand and font assets.")
