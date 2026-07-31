test_that("participant page size controls bounded participant slices", {
  snapshot <- dashboard_dataset_snapshot(dashboard_showcase_record(
    participants = 10L,
    days = 14L,
    epoch_seconds = 3600
  ))

  selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    facet_page = 2L,
    participants_per_page = 6L,
    time_basis = "participant",
    measurement_window = c(1L, 14L),
    time_page = 2L,
    days_per_page = 7L
  )

  expect_identical(selection$participants_per_page, 6L)
  expect_identical(selection$participant_pages, 2L)
  expect_identical(selection$participant_page, 2L)
  expect_identical(selection$page_participants, sprintf("P%02d", 7:10))
  expect_identical(nrow(selection$facet_slots), 6L)
  expect_identical(
    selection$facet_slots$Label,
    c(sprintf("P%02d", 7:10), "", "")
  )
  expect_identical(
    selection$facet_slots$Empty,
    c(rep(FALSE, 4L), TRUE, TRUE)
  )
  expect_identical(selection$time_pages, 2L)
  expect_identical(selection$time_page, 2L)
  expect_identical(dashboard_participants_per_page("6"), 6L)
  expect_identical(
    dashboard_participants_per_page("invalid", fallback = 4L),
    4L
  )
})

test_that("page navigator counts finite focus values including exact zero", {
  instants <- as.POSIXct(
    c(
      "2026-01-01 00:00:00",
      "2026-01-01 01:00:00",
      "2026-01-02 00:00:00",
      "2026-01-02 01:00:00"
    ),
    tz = "UTC"
  )
  data <- data.frame(
    Id = rep(c("P01", "P02"), each = length(instants)),
    Datetime = rep(instants, 2L),
    MEDI = c(0, -1, NA, Inf, NA, -Inf, 5, NA),
    stringsAsFactors = FALSE
  )
  record <- new_dataset_record(
    raw_data = data,
    display_name = "Navigator focus fixture",
    source_manifest = new_source_manifest(
      source_type = "test",
      source_timezone = "UTC"
    ),
    analysis_settings = list(
      primary_variable = "MEDI",
      analysis_timezone = "UTC"
    )
  )
  snapshot <- dashboard_dataset_snapshot(record)
  focus_view <- dashboard_focus_view(snapshot, "MEDI", "preprocessed")
  selection <- dashboard_plot_selection(
    snapshot,
    show_all = TRUE,
    participants_per_page = 1L,
    time_basis = "participant",
    measurement_window = c(1L, 2L),
    days_per_page = 1L,
    focus_view = focus_view
  )
  navigator <- dashboard_page_navigator(focus_view, selection)

  expect_equal(
    navigator[c("participant_page", "time_page")],
    data.frame(
      participant_page = c(1L, 1L, 2L, 2L),
      time_page = c(1L, 2L, 1L, 2L)
    )
  )
  expect_identical(navigator$observed_focus_values, c(2L, 0L, 0L, 1L))
  expect_identical(navigator$has_data, c(TRUE, FALSE, FALSE, TRUE))
})

test_that("dashboard exposes the compact navigation control contracts", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )

  expect_match(html, "llw-explorer-workbench", fixed = TRUE)
  expect_match(html, "llw-explorer-pager-strip", fixed = TRUE)
  expect_match(html, "llw-explorer-time-reference", fixed = TRUE)
  expect_match(html, "llw-explorer-time-actions__label", fixed = TRUE)
  expect_match(html, "llw-explorer-fit-time__label", fixed = TRUE)
  expect_match(
    html,
    "Fit the time window to the selected participants",
    fixed = TRUE
  )
  expect_match(html, "llw-explorer-sidebar", fixed = TRUE)
  expect_match(html, "llw-explorer-canvas", fixed = TRUE)
  expect_match(
    sass,
    ".llw-explorer-workbench__layout\n  > .main",
    fixed = TRUE
  )
  expect_match(sass, "padding: 0 !important;", fixed = TRUE)
  expect_false(grepl("llw-dashboard-plot-card", html, fixed = TRUE))
  expect_false(grepl(
    "llw-dashboard-navigation-help",
    html,
    fixed = TRUE
  ))

  core_ids <- c(
    "focus_variable",
    "view_recommendation",
    "data_stage",
    "show_all_participants",
    "participants",
    "participants_per_page",
    "time_basis",
    "date_window_ui",
    "fit_time_scope",
    "days_per_page",
    "view_mode",
    "scale_control",
    "y_range_control",
    "dashboard_plot",
    "participant_pagination",
    "facet_pagination"
  )
  for (id in core_ids) {
    expect_match(html, paste0("dashboard-", id), fixed = TRUE)
  }

  expect_match(html, "Max. participants per page", fixed = TRUE)
  expect_match(html, "llw-dashboard-view-mode", fixed = TRUE)
  expect_match(html, 'data-view-mode="auto"', fixed = TRUE)
  expect_match(html, 'data-view-mode="detailed"', fixed = TRUE)
  expect_match(html, 'data-view-mode="availability"', fixed = TRUE)
  expect_match(html, "fa-circle-half-stroke", fixed = TRUE)
  expect_match(html, "dashboard-facet_pagination", fixed = TRUE)
  expect_match(html, "dashboard-participant_pagination", fixed = TRUE)
  expect_false(grepl(
    "Day of month (calendar months)",
    html,
    fixed = TRUE
  ))
  expect_false(grepl(
    "Day of year (calendar years)",
    html,
    fixed = TRUE
  ))
  expect_false(grepl("dashboard-plot_notice", html, fixed = TRUE))
  expect_false(grepl(
    "llw-explorer-control-group__title",
    html,
    fixed = TRUE
  ))
  group_positions <- vapply(
    c("display", "data", "time", "participants"),
    function(group) {
      regexpr(
        paste0("llw-explorer-control-group--", group),
        html,
        fixed = TRUE
      )[[1L]]
    },
    integer(1L)
  )
  expect_true(all(group_positions > 0L))
  expect_true(all(diff(group_positions) > 0L))
  expect_match(sass, ".llw-dashboard-page-navigator", fixed = TRUE)
  expect_match(
    sass,
    ".llw-dashboard-page-navigator__cell--empty",
    fixed = TRUE
  )
  expect_match(sass, ".llw-participant-rail__slot", fixed = TRUE)
  expect_match(sass, "flex: 1 1 50%;", fixed = TRUE)
  expect_match(sass, "border-block-start: 1px solid", fixed = TRUE)
  expect_match(sass, "column-gap: var(--llw-space-6);", fixed = TRUE)
  expect_match(
    sass,
    "--llw-explorer-strip-padding: var(--llw-space-2);",
    fixed = TRUE
  )
  expect_match(
    sass,
    ".llw-explorer-participant-pagination",
    fixed = TRUE
  )
  expect_match(html, ">1 participant<", fixed = TRUE)
  expect_false(grepl("\U0001f4c4", html, fixed = TRUE))
  expect_false(grepl(
    "llw-dashboard-participants-per-page-control__icon",
    html,
    fixed = TRUE
  ))
  expect_false(grepl(
    ".llw-explorer-participant-page-control__icon",
    sass,
    fixed = TRUE
  ))
  expect_false(grepl(
    ".llw-dashboard-time-page-control__icon",
    sass,
    fixed = TRUE
  ))
  expect_match(
    sass,
    "grid-template-columns: 4.25rem minmax(0, 1fr);",
    fixed = TRUE
  )
  expect_match(html, "Plot all", fixed = TRUE)
  expect_match(
    sass,
    "grid-template-columns: minmax(6.75rem, 1fr) max-content;",
    fixed = TRUE
  )
  expect_match(
    sass,
    ".llw-explorer-show-all__label",
    fixed = TRUE
  )
  expect_false(grepl(
    ".llw-explorer-pager__participant-dock",
    sass,
    fixed = TRUE
  ))
  expect_false(grepl(
    "grid-template-areas: \"participant-dock main\";",
    sass,
    fixed = TRUE
  ))
  expect_match(sass, ".llw-explorer-pager__main", fixed = TRUE)
  expect_match(
    sass,
    "min-height: calc(2.75rem + 1.3rem);",
    fixed = TRUE
  )
  expect_match(
    sass,
    "grid-template-columns: 6.5rem minmax(12.5rem, 16.5rem) 6.5rem;",
    fixed = TRUE
  )
  expect_match(
    sass,
    "border-bottom-left-radius: var(--llw-explorer-corner-radius);",
    fixed = TRUE
  )
  expect_match(
    sass,
    ".llw-dashboard-page-axis .control-label",
    fixed = TRUE
  )
  expect_false(grepl(
    "grid-template-areas: \"participant navigator time .\";",
    sass,
    fixed = TRUE
  ))
  expect_match(sass, ".popover:has(.llw-explorer-time-help)", fixed = TRUE)
  expect_match(
    sass,
    "> .llw-explorer-info-trigger",
    fixed = TRUE
  )
  expect_match(
    sass,
    "container-name: llw-explorer-time-controls;",
    fixed = TRUE
  )
  expect_match(
    sass,
    "@container llw-explorer-time-controls (max-width: 14rem)",
    fixed = TRUE
  )
})

test_that("server pager keeps its page map compact and icon-only", {
  dataset <- shiny::reactiveVal(dashboard_showcase_record(
    participants = 10L,
    days = 14L,
    epoch_seconds = 3600
  ))

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
        participants = character(),
        show_all_participants = TRUE,
        participants_per_page = "4",
        time_basis = "participant",
        measurement_window = c(1L, 14L),
        view_mode = "auto",
        days_per_page = "7",
        plot_scale = "symlog",
        symlog_threshold = "1",
        preprocessed_main_only = TRUE,
        source_main_only = TRUE
      )
      session$flushReact()

      participant_pagination <- output$participant_pagination
      participant_pagination_html <- if (
        is.list(participant_pagination) &&
          !is.null(participant_pagination$html)
      ) {
        participant_pagination$html
      } else {
        paste(as.character(participant_pagination), collapse = "")
      }
      pagination <- output$facet_pagination
      pagination_html <- if (
        is.list(pagination) && !is.null(pagination$html)
      ) {
        pagination$html
      } else {
        paste(as.character(pagination), collapse = "")
      }

      expect_match(
        pagination_html,
        '<span class="visually-hidden">Participant and time page map</span>',
        fixed = TRUE
      )
      expect_false(grepl(
        ">[[:space:]]*Pages[[:space:]]*<",
        pagination_html,
        perl = TRUE
      ))
      expect_false(grepl("fa-border-all", pagination_html, fixed = TRUE))
      expect_match(
        participant_pagination_html,
        "-participant_page",
        fixed = TRUE
      )
      expect_match(
        participant_pagination_html,
        "Participants 1\u20134 of 10",
        fixed = TRUE
      )
      expect_match(
        participant_pagination_html,
        "llw-dashboard-page-option__icon",
        fixed = TRUE
      )
      expect_match(
        pagination_html,
        "llw-dashboard-page-option__icon",
        fixed = TRUE
      )
      expect_match(
        pagination_html,
        "llw-dashboard-page-option__label",
        fixed = TRUE
      )
      expect_false(grepl("\U0001f4c4", pagination_html, fixed = TRUE))
      expect_false(grepl("\u25a4", pagination_html, fixed = TRUE))
      expect_false(grepl(
        "\\b(?:Earlier|Later)\\b",
        pagination_html,
        perl = TRUE
      ))
      expect_match(
        pagination_html,
        "--llw-time-page-count:2",
        fixed = TRUE
      )
      expect_match(
        pagination_html,
        "--llw-participant-page-count:3",
        fixed = TRUE
      )
      expect_match(
        pagination_html,
        "--llw-navigator-width:",
        fixed = TRUE
      )
      expect_match(
        pagination_html,
        "--llw-navigator-row-size:",
        fixed = TRUE
      )
      expect_match(
        pagination_html,
        "llw-dashboard-time-page-button__icons",
        fixed = TRUE
      )
      expect_false(grepl(
        "llw-explorer-pager__participant-dock",
        pagination_html,
        fixed = TRUE
      ))
      expect_false(grepl(
        "-participant_page",
        pagination_html,
        fixed = TRUE
      ))
      expect_match(
        pagination_html,
        "llw-explorer-pager__main",
        fixed = TRUE
      )
      expect_match(pagination_html, "fa-arrow-left", fixed = TRUE)
      expect_match(pagination_html, "fa-calendar", fixed = TRUE)
      expect_match(pagination_html, "fa-arrow-right", fixed = TRUE)
    }
  )
})

test_that("explorer omits the redundant scope disclosure", {
  html <- htmltools::renderTags(datasetDashboardUI("dashboard"))$html
  sass <- paste(
    readLines(lightlogweb_sass_file(), warn = FALSE),
    collapse = "\n"
  )

  expect_false(grepl("dashboard-plot_notice", html, fixed = TRUE))
  expect_false(grepl("Detailed timeline scope", html, fixed = TRUE))
  expect_false(grepl("Focus coverage overview scope", html, fixed = TRUE))
  expect_false(grepl("llw-dashboard-scope-disclosure", sass, fixed = TRUE))
})

test_that("initialization and recommendation apply restore participant page size", {
  messages <- list()
  test_session <- shiny::MockShinySession$new()
  test_session$sendInputMessage <- function(inputId, message) {
    messages[[length(messages) + 1L]] <<- list(
      inputId = inputId,
      message = message
    )
    invisible()
  }
  participant_page_updates <- function() {
    Filter(
      function(message) {
        identical(message$inputId, "participants_per_page")
      },
      messages
    )
  }

  dataset <- shiny::reactiveVal(dashboard_showcase_record(
    participants = 10L,
    days = 14L,
    epoch_seconds = 3600
  ))

  shiny::testServer(
    datasetDashboardServer,
    args = list(
      dataset = dataset,
      active_panel = shiny::reactive("dashboard"),
      color_mode = shiny::reactive("light")
    ),
    session = test_session,
    {
      session$flushReact()
      initialization_updates <- participant_page_updates()
      expect_true(length(initialization_updates) >= 1L)
      expect_identical(
        initialization_updates[[length(initialization_updates)]]$message$value,
        "4"
      )

      session$setInputs(
        focus_variable = "MEDI",
        data_stage = "preprocessed",
        participants = sprintf("P%02d", 1:4),
        show_all_participants = FALSE,
        participants_per_page = "8",
        time_basis = "participant",
        measurement_window = c(1L, 14L),
        view_mode = "auto",
        days_per_page = "7",
        plot_scale = "symlog",
        symlog_threshold = "1",
        preprocessed_main_only = TRUE,
        source_main_only = TRUE
      )
      session$flushReact()

      messages <<- list()
      session$setInputs(apply_recommendation = 1)
      session$flushReact()
      apply_updates <- participant_page_updates()
      expect_length(apply_updates, 1L)
      expect_identical(apply_updates[[1L]]$message$value, "4")
    }
  )
})

test_that("navigator changes participant and time pages atomically", {
  dataset <- shiny::reactiveVal(dashboard_showcase_record(
    participants = 10L,
    days = 14L,
    epoch_seconds = 3600
  ))

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
        participants = character(),
        show_all_participants = TRUE,
        participants_per_page = "4",
        time_basis = "participant",
        measurement_window = c(1L, 14L),
        view_mode = "auto",
        days_per_page = "7",
        plot_scale = "symlog",
        symlog_threshold = "1",
        preprocessed_main_only = TRUE,
        source_main_only = TRUE
      )
      session$flushReact()

      returned <- session$getReturned()
      expect_identical(returned$status()$participant_pages, 3L)
      expect_identical(returned$status()$time_pages, 2L)
      expect_identical(returned$status()$participant_page, 1L)
      expect_identical(returned$status()$time_page, 1L)

      recommendation <- output$view_recommendation
      recommendation_html <- if (
        is.list(recommendation) && !is.null(recommendation$html)
      ) {
        recommendation$html
      } else {
        paste(as.character(recommendation), collapse = "")
      }
      expect_match(
        recommendation_html,
        "The recommended view is already active.",
        fixed = TRUE
      )
      expect_match(recommendation_html, 'aria-disabled="true"', fixed = TRUE)
      expect_match(recommendation_html, 'tabindex="0"', fixed = TRUE)

      session$setInputs(page_navigator = "3:2")
      session$flushReact()
      expect_identical(returned$status()$participant_page, 3L)
      expect_identical(returned$status()$time_page, 2L)

      session$setInputs(participant_page = "2")
      session$flushReact()
      expect_identical(returned$status()$participant_page, 2L)
      expect_identical(returned$status()$time_page, 2L)

      session$setInputs(time_page = "1")
      session$flushReact()
      expect_identical(returned$status()$participant_page, 2L)
      expect_identical(returned$status()$time_page, 1L)
    }
  )
})

test_that("time-window handles expose the boundary they change", {
  dataset <- shiny::reactiveVal(dashboard_showcase_record(
    participants = 2L,
    days = 30L,
    epoch_seconds = 3600
  ))

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
        participants_per_page = "2",
        time_basis = "participant",
        measurement_window = c(1L, 14L),
        view_mode = "detailed",
        days_per_page = "7",
        plot_scale = "symlog",
        symlog_threshold = "1"
      )
      session$flushReact()
      returned <- session$getReturned()
      expect_identical(returned$status()$time_pages, 2L)

      session$setInputs(measurement_window = c(1L, 21L))
      session$flushReact()
      expect_identical(returned$status()$time_pages, 3L)
      expect_identical(returned$status()$time_page, 3L)

      session$setInputs(measurement_window = c(8L, 21L))
      session$flushReact()
      expect_identical(returned$status()$time_pages, 2L)
      expect_identical(returned$status()$time_page, 1L)

      session$setInputs(
        time_basis = "calendar",
        date_window = as.Date(c("2026-01-01", "2026-01-14"))
      )
      session$flushReact()
      expect_identical(returned$status()$time_pages, 2L)

      session$setInputs(
        date_window = as.Date(c("2026-01-01", "2026-01-21"))
      )
      session$flushReact()
      expect_identical(returned$status()$time_pages, 3L)
      expect_identical(returned$status()$time_page, 3L)

      session$setInputs(
        date_window = as.Date(c("2026-01-08", "2026-01-21"))
      )
      session$flushReact()
      expect_identical(returned$status()$time_pages, 2L)
      expect_identical(returned$status()$time_page, 1L)
    }
  )
})
