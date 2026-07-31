refinement_output_html <- function(output_value) {
  if (is.list(output_value) && !is.null(output_value$html)) {
    return(as.character(output_value$html))
  }
  paste(as.character(output_value), collapse = "")
}

refinement_nav_panel_html <- function(html, value) {
  marker <- paste0('data-value="', value, '"')
  positions <- gregexpr(marker, html, fixed = TRUE)[[1L]]
  positions <- positions[positions > 0L]
  if (length(positions) == 0L) {
    stop("Dashboard nav panel was not found: ", value, call. = FALSE)
  }
  start <- positions[[length(positions)]]
  remainder <- substring(html, start)
  next_panel <- regexpr('<div class="tab-pane', remainder, fixed = TRUE)
  if (next_panel[[1L]] < 0L) {
    return(remainder)
  }
  substring(remainder, 1L, next_panel[[1L]] - 1L)
}

refinement_open_tags_with_class <- function(html, class_name) {
  pattern <- paste0(
    '<[^>]+class="[^"]*(?<![[:alnum:]_-])',
    class_name,
    '(?![[:alnum:]_-])[^"]*"[^>]*>'
  )
  matches <- gregexpr(pattern, html, perl = TRUE)[[1L]]
  if (identical(matches[[1L]], -1L)) {
    return(character())
  }
  regmatches(html, list(matches))[[1L]]
}

refinement_html_between <- function(html, start_marker, end_marker = NULL) {
  start <- regexpr(start_marker, html, fixed = TRUE)[[1L]]
  if (start < 0L) {
    stop("Dashboard HTML marker was not found: ", start_marker, call. = FALSE)
  }
  remainder <- substring(html, start)
  if (is.null(end_marker)) {
    return(remainder)
  }
  finish <- regexpr(end_marker, remainder, fixed = TRUE)[[1L]]
  if (finish < 0L) {
    stop("Dashboard HTML marker was not found: ", end_marker, call. = FALSE)
  }
  substring(remainder, 1L, finish - 1L)
}

refinement_record <- function(data, display_name = "Dashboard refinement fixture") {
  quality <- summarize_raw_import_quality(data, "UTC")
  new_dataset_record(
    raw_data = data,
    display_name = display_name,
    source_manifest = new_source_manifest(
      source_type = "dashboard_refinement_test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    ),
    provenance = list(
      raw_import_quality = quality,
      primary_variable_eligibility = quality$eligibility
    )
  )
}

refinement_detailed_preview <- function(
  snapshot,
  days = 2L,
  days_per_page = days,
  time_basis = "participant"
) {
  selection <- dashboard_plot_selection(
    snapshot,
    participants = snapshot$participants[[1L]],
    time_basis = time_basis,
    measurement_window = c(1L, days),
    view_mode = "detailed",
    days_per_page = days_per_page
  )
  dashboard_plot_preview(snapshot, selection)
}

test_that("focus missingness keeps all three denominators auditable", {
  datetimes <- as.POSIXct(
    c(
      "2026-01-01 00:00:00",
      "2026-01-01 00:00:00",
      "2026-01-01 00:01:00",
      "2026-01-01 00:03:00"
    ),
    tz = "UTC"
  )
  data <- data.frame(
    Id = "P01",
    Datetime = datetimes,
    MEDI = c(NA_real_, 10, NA_real_, 30),
    stringsAsFactors = FALSE
  )
  quality <- summarize_raw_import_quality(data, "UTC")
  coverage <- dashboard_daily_coverage(data, quality, "UTC", "MEDI")
  missingness <- dashboard_focus_missingness(
    data,
    quality,
    "UTC",
    "MEDI",
    coverage = coverage
  )

  expect_s3_class(missingness, "llw_dashboard_focus_missingness")
  expect_identical(
    missingness$scope,
    c("recorded", "regular", "full_days")
  )
  expect_identical(
    missingness$label,
    c(
      "Within recorded time series",
      "Within regular series",
      "Within full local days"
    )
  )

  recorded <- missingness[missingness$scope == "recorded", , drop = FALSE]
  expect_equal(recorded$expected_count, 3)
  expect_equal(recorded$observed_count, 2)
  expect_equal(recorded$explicit_missing_count, 1)
  expect_equal(recorded$implicit_gap_count, 0)
  expect_equal(recorded$missing_percent, 100 / 3)

  regular <- missingness[missingness$scope == "regular", , drop = FALSE]
  expect_equal(regular$expected_count, 4)
  expect_equal(regular$recorded_count, 3)
  expect_equal(regular$observed_count, 2)
  expect_equal(regular$explicit_missing_count, 1)
  expect_equal(regular$implicit_gap_count, 1)
  expect_equal(regular$missing_count, 2)

  full_days <- missingness[
    missingness$scope == "full_days",
    ,
    drop = FALSE
  ]
  expect_equal(full_days$expected_count, 1440)
  expect_equal(full_days$recorded_count, 3)
  expect_equal(full_days$observed_count, 2)
  expect_equal(full_days$explicit_missing_count, 1)
  expect_equal(full_days$implicit_gap_count, 1437)
  expect_equal(full_days$missing_count, 1438)
})

test_that("ordinary timestamp phase shifts do not manufacture gaps", {
  data <- data.frame(
    Id = "P01",
    Datetime = as.POSIXct("2026-01-01 00:00:00", tz = "UTC") +
      c(0, 2, 5, 7),
    MEDI = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  quality <- summarize_raw_import_quality(data, "UTC")
  missingness <- dashboard_focus_missingness(
    data,
    quality,
    "UTC",
    "MEDI"
  )
  regular <- missingness[missingness$scope == "regular", , drop = FALSE]

  expect_equal(quality$participants$dominant_epoch_seconds, 2)
  expect_gt(regular$off_grid_count, 0)
  expect_equal(regular$recorded_count, 4)
  expect_equal(regular$observed_count, 4)
  expect_equal(regular$implicit_gap_count, 0)
  expect_equal(regular$missing_count, 0)
  expect_match(
    regular$denominator,
    "do not by themselves create missing epochs"
  )
})

test_that("manual y-axis limits validate and remain a display-only zoom", {
  automatic <- dashboard_y_axis_limits(scale = "linear")
  expect_false(automatic$active)
  expect_true(all(is.na(automatic$limits)))
  expect_null(automatic$message)

  partial <- dashboard_y_axis_limits("0", "", scale = "linear")
  expect_true(partial$active)
  expect_equal(partial$limits, c(0, NA_real_))
  expect_null(partial$message)

  expect_match(
    dashboard_y_axis_limits("not a number", "10")$message,
    "finite number"
  )
  expect_match(
    dashboard_y_axis_limits("10", "10")$message,
    "smaller"
  )
  expect_match(
    dashboard_y_axis_limits("0", "10", scale = "log")$message,
    "greater than zero"
  )

  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 1L,
    days = 2L,
    epoch_seconds = 3600
  ))
  preview <- refinement_detailed_preview(snapshot)
  plot <- plot_dashboard_timeline(
    snapshot,
    preview,
    mode = "light",
    scale = "linear",
    y_limits = c(10, 180)
  )

  expect_equal(attr(plot, "llw_y_limits"), c(10, 180))
  expect_s3_class(plot$coordinates, "CoordCartesian")
  expect_equal(plot$coordinates$limits$y, c(10, 180))
  expect_no_error(ggplot2::ggplot_build(plot))
  expect_error(
    plot_dashboard_timeline(
      snapshot,
      preview,
      mode = "light",
      scale = "log",
      y_limits = c(0, 180)
    ),
    "greater than zero"
  )
})

test_that("fourteen-day detailed pages build without minor-break errors", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 1L,
    days = 14L,
    epoch_seconds = 3600
  ))
  preview <- refinement_detailed_preview(
    snapshot,
    days = 14L,
    days_per_page = 14L,
    time_basis = "elapsed"
  )
  plot <- plot_dashboard_timeline(
    snapshot,
    preview,
    mode = "light",
    scale = "linear"
  )

  expect_no_error(ggplot2::ggplot_build(plot))
  expect_identical(attr(plot, "llw_dashboard_plot"), "detailed_timeline")
})

test_that("participant timelines use exact day-aligned page limits", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 1L,
    days = 2L,
    epoch_seconds = 3600
  ))
  preview <- refinement_detailed_preview(snapshot, days = 2L)
  plot <- plot_dashboard_timeline(
    snapshot,
    preview,
    mode = "light",
    scale = "linear"
  )
  built <- ggplot2::ggplot_build(plot)
  panel_range <- built$layout$panel_params[[1L]]$x.range
  page_start <- lubridate::floor_date(
    min(preview$data[["Plot time"]]),
    unit = "1 day"
  )
  expected_range <- as.numeric(c(
    page_start,
    page_start + lubridate::days(2L)
  ))

  expect_equal(panel_range, expected_range, tolerance = 1e-7)
  expect_lt(
    panel_range[[2L]],
    as.numeric(max(preview$data[["Plot time"]]) + 86400)
  )
})

test_that("logarithmic plots distinguish omitted zeros and negatives", {
  data <- dataset_raw_data(dashboard_showcase_record(
    participants = 1L,
    days = 2L,
    epoch_seconds = 3600
  ))
  data$MEDI[seq_len(3L)] <- c(0, -2, NA_real_)
  snapshot <- dashboard_dataset_snapshot(refinement_record(data))
  preview <- refinement_detailed_preview(snapshot)
  plot <- plot_dashboard_timeline(
    snapshot,
    preview,
    mode = "light",
    scale = "log"
  )
  notice <- dashboard_scale_notice(preview, "log")

  expect_identical(attr(plot, "llw_log_zero_excluded"), 1L)
  expect_identical(attr(plot, "llw_log_negative_excluded"), 1L)
  expect_identical(attr(plot, "llw_log_excluded"), 2L)
  expect_match(notice, "1 zero", fixed = TRUE)
  expect_match(notice, "1 negative", fixed = TRUE)
})

test_that("compact availability views print rounded coverage percentages", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 2L,
    days = 3L,
    epoch_seconds = 3600
  ))
  selection <- dashboard_plot_selection(
    snapshot,
    participants = snapshot$participants,
    time_basis = "participant",
    measurement_window = c(1L, 3L),
    view_mode = "availability",
    days_per_page = 3L
  )
  preview <- dashboard_plot_preview(snapshot, selection)
  plot <- plot_dashboard_availability(snapshot, preview, mode = "light")
  text_layers <- which(vapply(
    plot$layers,
    function(layer) inherits(layer$geom, "GeomText"),
    logical(1)
  ))

  expect_gt(length(text_layers), 0L)
  built <- ggplot2::ggplot_build(plot)
  labels <- unlist(lapply(
    built$data[text_layers],
    function(layer_data) layer_data$label
  ))
  expect_true(any(grepl("^[0-9]+%$", labels)))
})

test_that("dashboard refinement controls have stable labels and placement hooks", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )

  expect_match(html, "Max. days per time page", fixed = TRUE)
  expect_match(html, "llw-dashboard-value-box--focus", fixed = TRUE)
  expect_match(html, "fa-bullseye", fixed = TRUE)
  expect_false(grepl("llw-dashboard-focus-bar", html, fixed = TRUE))
  expect_match(html, "dashboard-view_recommendation", fixed = TRUE)
  expect_match(html, "dashboard-y_range_control", fixed = TRUE)
  expect_match(html, "llw-participant-rail--previous", fixed = TRUE)
  expect_match(html, "llw-participant-rail--next", fixed = TRUE)
  expect_match(html, "dashboard-preprocessed_column_selector", fixed = TRUE)
  expect_match(html, "dashboard-source_column_selector", fixed = TRUE)
  expect_match(html, "llw-dashboard-table-toolbar", fixed = TRUE)
  expect_match(sass, "flex-basis: 9rem;", fixed = TRUE)
  expect_match(sass, "flex-basis: 10rem;", fixed = TRUE)
  expect_match(
    sass,
    ".llw-dashboard-value-slot--missingness",
    fixed = TRUE
  )
  expect_match(sass, "transform: translateY(-50%);", fixed = TRUE)
  expect_match(
    sass,
    ".llw-dashboard-value-box__icon i::before",
    fixed = TRUE
  )
})

test_that("module exposes recommendation, missingness, table, and range controls", {
  data <- dataset_raw_data(dashboard_showcase_record())
  data$Extra <- data$MEDI * 2
  dataset <- shiny::reactiveVal(refinement_record(data))

  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = shiny::reactive("dashboard"),
      color_mode = shiny::reactive("light")
    ),
    {
      session$setInputs(
        focus_variable = "MEDI",
        missingness_scope = "full_days",
        data_stage = "preprocessed",
        participants = c("P01", "P02"),
        show_all_participants = FALSE,
        time_basis = "participant",
        measurement_window = c(1L, 2L),
        view_mode = "auto",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1",
        plot_scale = "symlog",
        symlog_threshold = "1",
        preprocessed_main_only = FALSE,
        source_main_only = FALSE
      )
      session$flushReact()

      recommendation_html <- refinement_output_html(
        output$view_recommendation
      )
      expect_match(recommendation_html, "Recommended view", fixed = TRUE)
      expect_match(
        recommendation_html,
        "llw-recommended-view-action",
        fixed = TRUE
      )
      expect_match(recommendation_html, "disabled", fixed = TRUE)

      missingness_html <- refinement_output_html(output$summary_missingness)
      expect_match(
        missingness_html,
        "missingness_scope",
        fixed = TRUE
      )
      expect_match(
        missingness_html,
        "Recorded times",
        fixed = TRUE
      )
      expect_match(
        missingness_html,
        "Regular span",
        fixed = TRUE
      )
      expect_match(
        missingness_html,
        "Full days",
        fixed = TRUE
      )
      expect_match(
        missingness_html,
        'value="full_days" checked="checked"',
        fixed = TRUE
      )
      expect_match(
        missingness_html,
        "llw-dashboard-missingness-trigger",
        fixed = TRUE
      )
      expect_identical(
        session$getReturned()$status()$missingness_scope,
        "full_days"
      )

      y_range_html <- refinement_output_html(output$y_range_control)
      expect_match(y_range_html, "Y range", fixed = TRUE)
      expect_match(y_range_html, "Minimum (optional)", fixed = TRUE)
      expect_match(y_range_html, "Maximum (optional)", fixed = TRUE)

      prepared_columns <- refinement_output_html(
        output$preprocessed_column_selector
      )
      source_columns <- refinement_output_html(output$source_column_selector)
      expect_match(prepared_columns, "Choose columns", fixed = TRUE)
      expect_match(prepared_columns, "Additional columns", fixed = TRUE)
      expect_match(source_columns, "Choose columns", fixed = TRUE)
      expect_match(source_columns, "Additional columns", fixed = TRUE)

      inventory <- jsonlite::fromJSON(
        as.character(output$variable_inventory),
        simplifyVector = FALSE
      )
      expect_true(inventory$x$options$paging)
      expect_identical(inventory$x$options$dom, "ftip")
      expect_identical(
        inventory$x$options$pageLength,
        10L
      )
    }
  )
})

test_that("source diagnostics render status-specific colored icons", {
  diagnostics <- data.frame(
    check = c("Required identity", "Ordering", "Date range"),
    status = c("pass", "warning", "information"),
    value = c("2 participants", "1 unsorted", "2 days"),
    detail = c("Valid identity.", "Review ordering.", "Range information."),
    stringsAsFactors = FALSE
  )
  html <- htmltools::renderTags(
    raw_import_diagnostics_table_ui(diagnostics)
  )$html

  expect_match(html, "llw-diagnostic-status--success", fixed = TRUE)
  expect_match(html, "llw-diagnostic-status--warning", fixed = TRUE)
  expect_match(html, "llw-diagnostic-status--information", fixed = TRUE)
  expect_match(html, ">Pass<", fixed = TRUE)
  expect_match(html, ">Warning<", fixed = TRUE)
  expect_match(html, ">Info<", fixed = TRUE)
  expect_gte(length(gregexpr("<svg", html, fixed = TRUE)[[1L]]), 3L)
})

test_that("log omission badge reports the total and explains its components", {
  data <- dataset_raw_data(dashboard_showcase_record(
    participants = 1L,
    days = 2L,
    epoch_seconds = 3600
  ))
  data$MEDI[seq_len(2L)] <- c(0, -2)
  dataset <- shiny::reactiveVal(refinement_record(data))

  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = shiny::reactive("dashboard"),
      color_mode = shiny::reactive("light")
    ),
    {
      session$setInputs(
        focus_variable = "MEDI",
        data_stage = "preprocessed",
        participants = "P01",
        show_all_participants = FALSE,
        time_basis = "participant",
        measurement_window = c(1L, 2L),
        view_mode = "detailed",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1",
        preprocessed_main_only = TRUE,
        source_main_only = TRUE
      )
      session$flushReact()
      session$setInputs(plot_scale = "log")
      session$flushReact()

      badge <- refinement_output_html(output$log_omission_badge)
      expect_match(badge, "llw-log-omission-badge", fixed = TRUE)
      expect_match(
        badge,
        ">2 value(?:\\(s\\)|s)? omitted<",
        perl = TRUE
      )
      expect_match(badge, "1\\s+exact zero", perl = TRUE)
      expect_match(badge, "1\\s+negative", perl = TRUE)
    }
  )
})

test_that("explorer scope disclosures are removed from the UI", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("dashboard-plot_notice", html, fixed = TRUE))
  expect_false(grepl("llw-dashboard-scope-disclosure", html, fixed = TRUE))
  expect_false(grepl("llw-dashboard-scope-disclosure", sass, fixed = TRUE))
})

test_that("provenance and state content lives with its corresponding data", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  explore <- refinement_nav_panel_html(html, "explore")
  quality <- refinement_nav_panel_html(html, "quality")
  preprocessed <- refinement_nav_panel_html(html, "prepared")
  source <- refinement_nav_panel_html(html, "raw")

  expect_false(grepl("provenance_table", explore, fixed = TRUE))
  expect_false(grepl("state_table", explore, fixed = TRUE))
  expect_false(grepl("Source provenance", explore, fixed = TRUE))
  expect_false(grepl("Pre-processed-data state", explore, fixed = TRUE))

  expect_false(grepl("import_checks", quality, fixed = TRUE))
  expect_false(grepl("quality_diagnostics", quality, fixed = TRUE))
  expect_false(grepl("Source import checks", quality, fixed = TRUE))

  expect_match(
    preprocessed,
    "llw-dashboard-preprocessed-state",
    fixed = TRUE
  )
  expect_match(preprocessed, "state_table", fixed = TRUE)
  expect_match(preprocessed, "prepared_state_note", fixed = TRUE)
  expect_match(
    preprocessed,
    "llw-dashboard-preprocessed-integrity",
    fixed = TRUE
  )
  expect_match(preprocessed, "preprocessed_integrity", fixed = TRUE)

  expect_match(source, "Source provenance", fixed = TRUE)
  expect_match(source, "provenance_table", fixed = TRUE)
  expect_match(source, "Source import checks", fixed = TRUE)
  expect_match(source, "import_checks", fixed = TRUE)
  expect_match(source, "quality_diagnostics", fixed = TRUE)
})

test_that("Explore uses the full nav-card body without an inset wrapper", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  explore <- refinement_nav_panel_html(html, "explore")

  expect_match(explore, "llw-dashboard-layout", fixed = TRUE)
  expect_match(
    explore,
    paste0(
      '<div class="llw-explorer-workbench">[[:space:]]*',
      '<div class="[^"]*',
      '(?:bslib-sidebar-layout[^"]*llw-dashboard-layout|',
      'llw-dashboard-layout[^"]*bslib-sidebar-layout)',
      '[^"]*"'
    ),
    perl = TRUE
  )
  expect_false(grepl(
    paste0(
      '<div class="llw-dashboard-layout">[[:space:]]*',
      '<div class="bslib-sidebar-layout'
    ),
    explore,
    perl = TRUE
  ))

  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )
  full_bleed_selector <- paste0(
    "(?s)\\.llw-dashboard-tabs[^\\{]*",
    "\\.tab-pane\\[data-value=[\"']explore[\"']\\]",
    "[^\\{]*>\\s*\\.card-body\\s*\\{"
  )
  expect_match(
    sass,
    paste0(full_bleed_selector, "[^}]*padding:\\s*0;"),
    perl = TRUE
  )
  expect_match(
    sass,
    paste0(full_bleed_selector, "[^}]*gap:\\s*0;"),
    perl = TRUE
  )
  expect_match(
    sass,
    paste0(
      "(?s)\\.llw-dashboard-layout\\s*\\{",
      "[^}]*margin-bottom:\\s*0;"
    ),
    perl = TRUE
  )
})

test_that("pre-processed support cards stretch to one row height", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  preprocessed <- refinement_nav_panel_html(html, "prepared")

  expect_match(
    preprocessed,
    "llw-dashboard-preprocessed-support",
    fixed = TRUE
  )
  expect_match(
    preprocessed,
    "llw-dashboard-preprocessed-state",
    fixed = TRUE
  )
  expect_match(
    preprocessed,
    "llw-dashboard-preprocessed-integrity",
    fixed = TRUE
  )

  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )
  support_grid_rule <- paste0(
    "(?s)\\.llw-dashboard-preprocessed-support\\s*\\{",
    "[^}]*align-items:\\s*stretch;"
  )
  support_card_rule <- paste0(
    "(?s)\\.llw-dashboard-preprocessed-support",
    "[^\\{]*:where\\(",
    "[^)]*\\.llw-dashboard-preprocessed-state",
    "[^)]*\\.llw-dashboard-preprocessed-integrity",
    "[^)]*\\)\\s*\\{",
    "[^}]*height:\\s*100%;"
  )
  expect_match(sass, support_grid_rule, perl = TRUE)
  expect_match(sass, support_card_rule, perl = TRUE)
})

test_that("dashboard support content is flat inside its owning cards", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  preprocessed <- refinement_nav_panel_html(html, "prepared")
  source <- refinement_nav_panel_html(html, "raw")
  support_fragments <- list(
    state = refinement_html_between(
      preprocessed,
      "llw-dashboard-preprocessed-state",
      "llw-dashboard-preprocessed-integrity"
    ),
    integrity = refinement_html_between(
      preprocessed,
      "llw-dashboard-preprocessed-integrity"
    ),
    provenance = refinement_html_between(
      source,
      "llw-dashboard-source-provenance",
      "llw-dashboard-source-import-checks"
    ),
    diagnostics = refinement_html_between(
      source,
      "llw-dashboard-source-import-checks"
    )
  )
  support_tags <- c(
    refinement_open_tags_with_class(
      preprocessed,
      "llw-dashboard-support-region"
    ),
    refinement_open_tags_with_class(
      source,
      "llw-dashboard-support-region"
    )
  )

  expect_length(support_tags, 4L)
  expect_true(all(vapply(
    support_fragments,
    function(fragment) {
      grepl("llw-dashboard-support-region", fragment, fixed = TRUE)
    },
    logical(1)
  )))
  expect_false(any(vapply(
    support_fragments,
    function(fragment) {
      grepl("llw-data-region", fragment, fixed = TRUE)
    },
    logical(1)
  )))
  expect_false(any(grepl("llw-data-region", support_tags, fixed = TRUE)))
  expect_false(any(grepl(
    'class="[^"]*(?:^|[[:space:]])card(?:[[:space:]]|$)',
    support_tags,
    perl = TRUE
  )))
  expect_match(preprocessed, "dashboard-state_table", fixed = TRUE)
  expect_match(preprocessed, "dashboard-preprocessed_integrity", fixed = TRUE)
  expect_match(source, "dashboard-provenance_table", fixed = TRUE)
  expect_match(source, "dashboard-quality_diagnostics", fixed = TRUE)

  quality <- refinement_nav_panel_html(html, "quality")
  flat_table_tags <- refinement_open_tags_with_class(
    quality,
    "llw-dashboard-flat-table-region"
  )
  expect_length(flat_table_tags, 2L)
  expect_false(any(grepl("llw-data-region", flat_table_tags, fixed = TRUE)))
  auto_height_cards <- refinement_open_tags_with_class(
    quality,
    "llw-dashboard-auto-height-card"
  )
  expect_length(auto_height_cards, 2L)
  expect_false(any(grepl("html-fill-item", auto_height_cards, fixed = TRUE)))
})

test_that("participant-day coverage preserves horizontal table readability", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  quality <- refinement_nav_panel_html(html, "quality")
  expect_match(quality, "llw-dashboard-coverage-table", fixed = TRUE)

  dataset <- shiny::reactiveVal(dashboard_showcase_record())
  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = shiny::reactive("dashboard"),
      color_mode = shiny::reactive("light")
    ),
    {
      session$setInputs(
        focus_variable = "MEDI",
        data_stage = "preprocessed",
        participants = c("P01", "P02"),
        show_all_participants = FALSE,
        time_basis = "participant",
        measurement_window = c(1L, 2L),
        view_mode = "auto",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1",
        preprocessed_main_only = TRUE,
        source_main_only = TRUE
      )
      session$flushReact()

      coverage <- jsonlite::fromJSON(
        as.character(output$coverage_table),
        simplifyVector = FALSE
      )
      expect_true(coverage$x$options$scrollX)
      expect_true(coverage$x$options$autoWidth)
    }
  )

  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )
  expect_match(
    sass,
    ".llw-dashboard-coverage-table table.dataTable thead th",
    fixed = TRUE
  )
  coverage_header_rule <- paste0(
    "(?s)\\.llw-dashboard-coverage-table table\\.dataTable thead th",
    "[^{]*\\{[^}]*white-space:\\s*nowrap;"
  )
  expect_match(sass, coverage_header_rule, perl = TRUE)
})

test_that("dashboard styling has scoped hooks for compact form alignment", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  expect_match(html, "llw-dashboard-checkbox", fixed = TRUE)

  dataset <- shiny::reactiveVal(dashboard_showcase_record())
  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = shiny::reactive("dashboard"),
      color_mode = shiny::reactive("light")
    ),
    {
      session$setInputs(
        focus_variable = "MEDI",
        missingness_scope = "full_days",
        data_stage = "preprocessed",
        participants = "P01",
        show_all_participants = FALSE,
        time_basis = "participant",
        measurement_window = c(1L, 2L),
        view_mode = "detailed",
        days_per_page = "7",
        participant_page = "1",
        time_page = "1",
        preprocessed_main_only = TRUE,
        source_main_only = TRUE
      )
      session$flushReact()

      y_range <- refinement_output_html(output$y_range_control)
      scale <- refinement_output_html(output$scale_control)
      missingness <- refinement_output_html(output$summary_missingness)
      expect_match(y_range, "llw-dashboard-y-range", fixed = TRUE)
      expect_match(
        y_range,
        "symlog_threshold_control",
        fixed = TRUE
      )
      expect_match(scale, ">Symlog<", fixed = TRUE)
      expect_match(
        missingness,
        "llw-dashboard-missingness-scope",
        fixed = TRUE
      )
    }
  )

  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )
  expect_match(sass, ".llw-dashboard-y-range", fixed = TRUE)
  expect_match(
    sass,
    '.llw-dashboard-y-range input[type="radio"]',
    fixed = TRUE
  )
  expect_false(grepl(
    ".llw-dashboard-y-range :where(.form-control, input)",
    sass,
    fixed = TRUE
  ))
  expect_match(sass, ".llw-dashboard-missingness-scope", fixed = TRUE)
  expect_match(sass, ".llw-dashboard-checkbox", fixed = TRUE)
  expect_match(
    sass,
    paste0(
      "(?s)\\.llw-dashboard-checkbox[^\\{]*\\{?[^}]*",
      "align-items:\\s*center;"
    ),
    perl = TRUE
  )
})
