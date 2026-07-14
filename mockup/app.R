#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(bsicons)
  library(ggplot2)
  library(LightLogR)
})

`%or%` <- function(x, fallback) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) fallback else x
}

r_quote <- function(x) encodeString(as.character(x), quote = "\"")

demo_data <- LightLogR::sample.data.environment
demo_5min <- LightLogR::aggregate_Datetime(demo_data, unit = "5 min") |>
  as.data.frame()

demo_ids <- levels(demo_data$Id)
demo_dates <- seq.Date(
  as.Date(min(demo_data$Datetime), tz = "Europe/Berlin"),
  as.Date(max(demo_data$Datetime), tz = "Europe/Berlin"),
  by = "day"
)

metric_definitions <- data.frame(
  id = c("tat", "dose", "centroid", "bright_dark", "iv", "is", "ema", "spectral"),
  name = c(
    "Duration above threshold",
    "Dose",
    "Centroid of light exposure",
    "Brightest / darkest period",
    "Intradaily variability",
    "Interdaily stability",
    "Exponential moving average",
    "Spectral integration"
  ),
  family = c(
    "Duration & threshold",
    "Level & dose",
    "Timing",
    "Timing",
    "Rhythmicity",
    "Rhythmicity",
    "Prior history",
    "Spectral"
  ),
  function_name = c(
    "duration_above_threshold()",
    "dose()",
    "centroidLE()",
    "bright_dark_period()",
    "intradaily_variability()",
    "interdaily_stability()",
    "exponential_moving_average()",
    "spectral_integration()"
  ),
  summary = c(
    "Total time satisfying a configurable exposure threshold.",
    "Time-integrated exposure over the selected analysis groups.",
    "Time at which half of cumulative exposure has occurred.",
    "Mean, timing, onset and offset of the brightest or darkest window.",
    "Fragmentation of the within-day exposure rhythm.",
    "Stability of the exposure pattern across consecutive days.",
    "A time-varying light estimate weighted by recent exposure history.",
    "Integrate spectral irradiance across a declared wavelength range."
  ),
  requirement = c(
    "Light + timestamps",
    "Light + timestamps",
    "Light + timestamps",
    "Light + timestamps",
    "Regular timestamps",
    "Hourly means recommended",
    "Regular light + timestamps",
    "Spectral irradiance + wavelengths"
  ),
  unit = c(
    "Output: time",
    "Output: value·hours",
    "Output: local date/time",
    "Output: value + local time",
    "Output: unitless",
    "Output: unitless",
    "Output: same as light variable",
    "Output: weighted spectral integral"
  ),
  stringsAsFactors = FALSE
)

viz_definitions <- data.frame(
  id = c("overview", "day", "timeline", "heatmap", "doubleplot", "aggregate", "histogram", "cdf"),
  name = c(
    "Availability", "Day plot", "Timeline", "Heatmap",
    "Doubleplot", "Daily profile", "Histogram", "Cumulative distribution"
  ),
  description = c(
    "Coverage by participant and date",
    "Overlay dates on a 24-hour clock",
    "Follow exposure across the study",
    "Scan recurring temporal patterns",
    "Inspect continuity across midnight",
    "Median and interquartile daily rhythm",
    "Inspect range, zeros and outliers",
    "Compare exposure distributions"
  ),
  stringsAsFactors = FALSE
)

default_script_state <- function() {
  list(
    device = "ActLumus",
    device_version = "Auto-detect",
    timezone = "Europe/Berlin",
    dst = TRUE,
    id_method = "automated",
    id_value = ".*",
    date_start = "2023-08-29",
    date_end = "2023-09-03",
    value_min = 0,
    value_max = 100000,
    flag_range = TRUE,
    nonwear = "none",
    aggregate = TRUE,
    interval = "5 min",
    aggregate_function = "mean",
    gap_handling = "explicit_na",
    remove_partial = TRUE,
    missing_threshold = 20,
    group_dimensions = c("participant", "date", "photoperiod"),
    clock_window = FALSE,
    clock_start = 6,
    clock_end = 12,
    latitude = 48.52,
    longitude = 9.06,
    solar_depression = 6,
    metric = "tat",
    metric_threshold = 250,
    metric_direction = "above",
    metric_period = "brightest",
    metric_timespan = "10 hours",
    metric_loop = TRUE,
    metric_decay = "90 min",
    spectral_min = 380,
    spectral_max = 780,
    visualization = "timeline",
    visualization_photoperiod = TRUE,
    include_source = TRUE,
    include_timezone = TRUE,
    include_range = TRUE,
    include_sampling = TRUE,
    include_gaps = TRUE,
    include_inclusion = TRUE,
    include_grouping = TRUE,
    include_metric = TRUE,
    include_visualization = TRUE
  )
}

build_script <- function(state = default_script_state()) {
  lines <- c(
    "# LightLogWeb analysis recipe",
    sprintf("# Generated with LightLogR %s and R %s", as.character(packageVersion("LightLogR")), getRversion()),
    "# Place the original device exports in a project-relative data/ folder.",
    "",
    "library(LightLogR)",
    "library(dplyr)",
    "library(ggplot2)",
    "library(lubridate)",
    ""
  )

  if (isTRUE(state$include_source)) {
    id_argument <- switch(
      state$id_method,
      manual = sprintf(", manual.id = %s", r_quote(state$id_value %or% "Participant")),
      extract = sprintf(", auto.id = %s", r_quote(state$id_value %or% ".*")),
      ", auto.id = \".*\""
    )
    lines <- c(
      lines,
      "# 1. Import",
      sprintf("# File-format version: %s", state$device_version),
      "files <- list.files(\"data\", pattern = \"\\\\.(csv|txt)$\", full.names = TRUE)",
      sprintf(
        "data_raw <- import$%s(files, tz = %s, dst_adjustment = %s%s)",
        state$device,
        r_quote(state$timezone),
        if (isTRUE(state$dst)) "TRUE" else "FALSE",
        id_argument
      ),
      "",
      "data <- data_raw"
    )
  } else {
    lines <- c(lines, "# Import step disabled: provide an object named data.")
  }

  if (isTRUE(state$include_timezone)) {
    lines <- c(
      lines,
      "",
      "# 2. Make the intended local time zone explicit",
      sprintf(
        "data <- data |> mutate(Datetime = force_tz(Datetime, tzone = %s))",
        r_quote(state$timezone)
      ),
      sprintf(
        "data <- data |> filter(Datetime >= as.POSIXct(%s, tz = %s), Datetime < as.POSIXct(%s, tz = %s) + days(1))",
        r_quote(state$date_start), r_quote(state$timezone),
        r_quote(state$date_end), r_quote(state$timezone)
      )
    )
  }

  if (isTRUE(state$include_range) && isTRUE(state$flag_range)) {
    lines <- c(
      lines,
      "",
      "# 3. Retain timestamps while marking out-of-range readings as missing",
      sprintf(
        "data <- data |> mutate(MEDI = ifelse(between(MEDI, %s, %s), MEDI, NA_real_))",
        format(state$value_min, scientific = FALSE),
        format(state$value_max, scientific = FALSE)
      )
    )
  }

  if (isTRUE(state$include_range) && !identical(state$nonwear, "none")) {
    nonwear_note <- switch(
      state$nonwear,
      device = "# Non-wear source selected: map the device wear flag to an explicit validity column.",
      log = "# Non-wear source selected: join the external event log before invalidating readings.",
      variance = "# Non-wear source selected: validate the study-specific low-variance rule before applying it.",
      "# No automatic non-wear rule selected."
    )
    lines <- c(lines, "", nonwear_note)
  }

  if (isTRUE(state$include_gaps) && identical(state$gap_handling, "explicit_na")) {
    lines <- c(
      lines,
      "",
      "# 4. Convert implicit gaps to explicit NA observations; do not impute values",
      "data <- data |> gap_handler()"
    )
  } else if (isTRUE(state$include_gaps)) {
    lines <- c(
      lines,
      "",
      "# 4. Gap policy: leave timestamps unchanged; implicit gaps remain absent"
    )
  }

  if (isTRUE(state$include_sampling) && isTRUE(state$aggregate)) {
    handler <- switch(
      state$aggregate_function,
      median = "\\(x) median(x, na.rm = TRUE)",
      max = "\\(x) max(x, na.rm = TRUE)",
      "\\(x) mean(x, na.rm = TRUE)"
    )
    lines <- c(
      lines,
      "",
      "# 5. Regularize to the selected analysis interval",
      sprintf(
        "data <- data |> aggregate_Datetime(unit = %s, numeric.handler = %s)",
        r_quote(state$interval),
        handler
      )
    )
  } else if (isTRUE(state$include_sampling)) {
    lines <- c(
      lines,
      "",
      "# 5. Sampling policy: retain each stream's recording epoch"
    )
  }

  if (isTRUE(state$include_inclusion) && isTRUE(state$remove_partial)) {
    lines <- c(
      lines,
      "",
      "# 6. Apply the declared daily completeness rule",
      sprintf(
        "data <- data |> remove_partial_data(MEDI, threshold.missing = %.2f, by.date = TRUE, handle.gaps = TRUE)",
        state$missing_threshold / 100
      )
    )
  }

  dimensions <- state$group_dimensions %or% character(0)
  if (isTRUE(state$include_grouping)) {
    if ("photoperiod" %in% dimensions) {
      lines <- c(
        lines,
        "",
        "# 7. Add location-specific photoperiod context",
        sprintf(
          "data <- data |> add_photoperiod(coordinates = c(%.4f, %.4f), solarDep = %s, overwrite = TRUE)",
          state$latitude,
          state$longitude,
          state$solar_depression
        )
      )
    }

    grouping_terms <- character(0)
    if ("participant" %in% dimensions) grouping_terms <- c(grouping_terms, "Id")
    if ("date" %in% dimensions) grouping_terms <- c(grouping_terms, "Day = date(Datetime)")
    if ("weekday" %in% dimensions) {
      grouping_terms <- c(grouping_terms, "Day_type = if_else(wday(Datetime) %in% c(1, 7), \"Weekend\", \"Weekday\")")
    }
    if ("photoperiod" %in% dimensions) grouping_terms <- c(grouping_terms, "photoperiod")
    if (isTRUE(state$clock_window)) {
      grouping_terms <- c(
        grouping_terms,
        sprintf(
          "Clock_window = hour(Datetime) >= %s & hour(Datetime) < %s",
          state$clock_start,
          state$clock_end
        )
      )
    }
    if (length(grouping_terms)) {
      lines <- c(
        lines,
        "",
        "# 8. Define analysis groups",
        sprintf("data_grouped <- data |> group_by(%s)", paste(grouping_terms, collapse = ", "))
      )
    } else {
      lines <- c(lines, "", "data_grouped <- data")
    }
  } else {
    lines <- c(lines, "", "data_grouped <- data")
  }

  metric_call <- switch(
    state$metric,
    dose = "dose(MEDI, Datetime, as.df = TRUE)",
    centroid = "centroidLE(MEDI, Datetime, as.df = TRUE)",
    bright_dark = sprintf(
      "bright_dark_period(MEDI, Datetime, period = %s, timespan = %s, loop = %s, as.df = TRUE)",
      r_quote(state$metric_period),
      r_quote(state$metric_timespan),
      if (isTRUE(state$metric_loop)) "TRUE" else "FALSE"
    ),
    iv = "intradaily_variability(MEDI, Datetime, as.df = TRUE)",
    is = "interdaily_stability(MEDI, Datetime, as.df = TRUE)",
    sprintf(
      "duration_above_threshold(MEDI, Datetime, comparison = %s, threshold = %s, as.df = TRUE)",
      r_quote(state$metric_direction), state$metric_threshold
    )
  )

  if (isTRUE(state$include_metric)) {
    if (identical(state$metric, "ema")) {
      lines <- c(
        lines,
        "",
        "# 9. Add a time-varying prior-history metric within each declared group",
        sprintf(
          "metrics <- data_grouped |> mutate(MEDI_EMA = exponential_moving_average(MEDI, Datetime, decay = %s)) |> ungroup()",
          r_quote(state$metric_decay)
        )
      )
    } else if (identical(state$metric, "spectral")) {
      lines <- c(
        lines,
        "",
        "# 9. Calculate a spectral metric from a project-relative spectral file",
        "# Expected format: one row per spectrum and numeric wavelength columns.",
        "spectrum <- read.csv(\"data/spectrum.csv\", check.names = FALSE)",
        sprintf(
          "metrics <- spectral_integration(spectrum, wavelength.range = c(%s, %s))",
          state$spectral_min,
          state$spectral_max
        )
      )
    } else {
      lines <- c(
        lines,
        "",
        "# 9. Calculate the selected metric for every declared group",
        sprintf("metrics <- data_grouped |> summarize(%s, .groups = \"drop\")", metric_call)
      )
    }
  }

  visualization_call <- switch(
    state$visualization,
    overview = "gg_overview(data)",
    day = if (isTRUE(state$visualization_photoperiod)) sprintf("gg_day(data) |> gg_photoperiod(coordinates = c(%.4f, %.4f), solarDep = %s)", state$latitude, state$longitude, state$solar_depression) else "gg_day(data)",
    heatmap = "gg_heatmap(data)",
    doubleplot = "gg_heatmap(data, doubleplot = \"next\")",
    aggregate = "gg_day(data, geom = \"ribbon\")",
    histogram = "ggplot(data, aes(MEDI)) + geom_histogram(bins = 60) + scale_x_continuous(trans = scales::pseudo_log_trans(base = 10))",
    cdf = "ggplot(data, aes(MEDI)) + stat_ecdf() + scale_x_continuous(trans = scales::pseudo_log_trans(base = 10))",
    if (isTRUE(state$visualization_photoperiod)) sprintf("gg_days(data) |> gg_photoperiod(coordinates = c(%.4f, %.4f), solarDep = %s)", state$latitude, state$longitude, state$solar_depression) else "gg_days(data)"
  )

  if (isTRUE(state$include_visualization)) {
    lines <- c(
      lines,
      "",
      "# 10. Recreate the selected visualization",
      sprintf("exposure_plot <- %s", visualization_call),
      "exposure_plot",
      ""
    )
  }

  c(
    lines,
    "# Reproducibility",
    "sessionInfo()"
  )
}

app_theme <- bslib::bs_theme(
  version = 5,
  bg = "#F5F8FA",
  fg = "#0B3041",
  primary = "#0B789C",
  secondary = "#5E7480",
  success = "#26735B",
  warning = "#A86118",
  danger = "#A9413D",
  base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif"),
  heading_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "sans-serif")
)

brand_mark <- function(compact = FALSE) {
  tags$div(
    class = paste("brand-mark", if (compact) "brand-mark-compact" else ""),
    tags$span(class = "brand-icon", bsicons::bs_icon("sunrise")),
    tags$span(
      class = "brand-copy",
      tags$strong("LightLog"),
      tags$span("Web")
    )
  )
}

screen_heading <- function(eyebrow, title, description, actions = NULL) {
  tags$header(
    class = "screen-heading",
    tags$div(
      tags$p(class = "eyebrow", eyebrow),
      tags$h1(title),
      tags$p(class = "screen-description", description)
    ),
    if (!is.null(actions)) tags$div(class = "heading-actions", actions)
  )
}

quality_item <- function(icon_name, label, value, detail, status = "good", action_id = NULL) {
  content <- tags$div(
    class = paste("quality-item", paste0("quality-", status)),
    tags$span(class = "quality-icon", bsicons::bs_icon(icon_name)),
    tags$div(
      tags$span(class = "quality-label", label),
      tags$strong(value),
      tags$span(class = "quality-detail", detail)
    ),
    if (!is.null(action_id)) actionLink(
      action_id, "Review", class = "quality-link",
      `aria-label` = paste("Review", tolower(label))
    )
  )
  content
}

landing_ui <- tags$div(
  class = "landing-page",
  tags$header(
    class = "landing-nav",
    brand_mark(),
    tags$nav(
      `aria-label` = "Resources",
      tags$a("Documentation", href = "https://tscnlab.github.io/LightLogR/", target = "_blank", rel = "noopener"),
      tags$a("Course", href = "https://tscnlab.github.io/LightLogR_webinar/", target = "_blank", rel = "noopener"),
      actionButton("landing_import", "Start import", class = "btn btn-outline-primary")
    )
  ),
  tags$main(
    class = "landing-main",
    tags$section(
      class = "hero-section",
      tags$div(
        class = "hero-copy",
        tags$p(class = "eyebrow", "Wearable light data, clearly understood"),
        tags$h1("From logger files to a transparent analysis."),
        tags$p(
          class = "hero-lede",
          "Explore, prepare and summarize personal light exposure without writing code—then take the complete recipe with you into R."
        ),
        tags$div(
          class = "hero-actions",
          actionButton(
            "explore_demo",
            tagList(bsicons::bs_icon("play-circle"), "Explore the demo"),
            class = "btn btn-primary btn-lg"
          ),
          actionButton(
            "start_import",
            tagList(bsicons::bs_icon("upload"), "Start an import"),
            class = "btn btn-light btn-lg"
          )
        ),
        tags$p(
          class = "privacy-note",
          bsicons::bs_icon("shield-check"),
          "This prototype keeps the demonstration inside your current R session."
        )
      ),
      tags$div(
        class = "hero-visual",
        tags$div(class = "sun-orbit", tags$span(class = "sun-core")),
        tags$div(
          class = "hero-chart",
          tags$div(
            class = "hero-chart-header",
            tags$span("Personal light exposure"),
            tags$span(class = "demo-badge", "DEMO")
          ),
          tags$div(
            class = "hero-chart-plot",
            tags$div(class = "night-zone night-zone-left"),
            tags$div(class = "night-zone night-zone-right"),
            tags$svg(
              viewBox = "0 0 640 230",
              role = "img",
              `aria-label` = "Illustrative daily personal light exposure curve",
              tags$path(class = "chart-grid", d = "M20 55H620 M20 115H620 M20 175H620"),
              tags$path(class = "chart-area", d = "M20 196 C65 194 82 188 110 174 C138 160 150 148 180 143 C220 136 225 66 270 82 C307 96 320 50 362 62 C407 76 420 132 460 139 C505 147 520 176 558 184 C580 190 600 194 620 196 L620 210 L20 210 Z"),
              tags$path(class = "chart-line", d = "M20 196 C65 194 82 188 110 174 C138 160 150 148 180 143 C220 136 225 66 270 82 C307 96 320 50 362 62 C407 76 420 132 460 139 C505 147 520 176 558 184 C580 190 600 194 620 196"),
              tags$circle(class = "chart-point", cx = "362", cy = "62", r = "6")
            )
          ),
          tags$div(
            class = "hero-chart-footer",
            tags$span("00"), tags$span("06"), tags$span("12"), tags$span("18"), tags$span("24")
          )
        )
      )
    ),
    tags$section(
      class = "journey-section",
      tags$p(class = "eyebrow", "One continuous workflow"),
      tags$h2("Stay close to the data. Keep every decision."),
      tags$div(
        class = "journey-grid",
        tags$article(
          class = "journey-step",
          tags$span(class = "step-number", "01"),
          tags$span(class = "step-icon", bsicons::bs_icon("file-earmark-arrow-up")),
          tags$h3("Import"),
          tags$p("Identify devices, participants, timestamps and measured quantities.")
        ),
        tags$article(
          class = "journey-step",
          tags$span(class = "step-number", "02"),
          tags$span(class = "step-icon", bsicons::bs_icon("activity")),
          tags$h3("Inspect"),
          tags$p("See coverage, sampling regularity, missingness and temporal shape first.")
        ),
        tags$article(
          class = "journey-step",
          tags$span(class = "step-number", "03"),
          tags$span(class = "step-icon", bsicons::bs_icon("sliders")),
          tags$h3("Analyse"),
          tags$p("Prepare, group, calculate metrics and build purposeful visualizations.")
        ),
        tags$article(
          class = "journey-step journey-step-accent",
          tags$span(class = "step-number", "04"),
          tags$span(class = "step-icon", bsicons::bs_icon("code-slash")),
          tags$h3("Continue in R"),
          tags$p("Download a readable LightLogR script containing every choice made so far.")
        )
      )
    ),
    tags$section(
      class = "evidence-strip",
      tags$div(
        tags$span(class = "evidence-icon", bsicons::bs_icon("check2-circle")),
        tags$div(tags$strong("Research-centred by design"), tags$p("Quality, timing and provenance stay visible throughout the workflow."))
      ),
      tags$a(
        "Read the LightLogR workflow",
        href = "https://tscnlab.github.io/LightLogR/articles/Import.html",
        target = "_blank",
        rel = "noopener"
      )
    )
  ),
  tags$footer(
    class = "landing-footer",
    tags$p(tags$strong("LightLogWeb"), " · A code-less companion to LightLogR"),
    tags$p("Translational Sensory & Circadian Neuroscience Unit · TUM / MPS / TUMCREATE"),
    tags$nav(
      class = "footer-links", `aria-label` = "LightLogR resources",
      tags$a("Documentation", href = "https://tscnlab.github.io/LightLogR/", target = "_blank", rel = "noopener"),
      tags$a("Course", href = "https://tscnlab.github.io/LightLogR_webinar/", target = "_blank", rel = "noopener")
    )
  )
)

overview_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Dataset overview",
    "sample.data.environment",
    "A participant and concurrent environmental recording in Tübingen, Germany · 29 Aug–3 Sep 2023",
    tags$span(class = "status-pill status-ready", bsicons::bs_icon("check-circle"), "Ready with checks")
  ),
  bslib::layout_column_wrap(
    width = 1 / 4,
    class = "summary-grid",
    bslib::value_box(
      title = "IDs",
      value = "2",
      showcase = bsicons::bs_icon("people"),
      p("Participant + environment"),
      class = "summary-card"
    ),
    bslib::value_box(
      title = "Participant-days",
      value = "6",
      showcase = bsicons::bs_icon("calendar3"),
      p("29 Aug–3 Sep"),
      class = "summary-card"
    ),
    bslib::value_box(
      title = "Usable coverage",
      value = "100%",
      showcase = bsicons::bs_icon("circle-half"),
      p("69,120 observations"),
      class = "summary-card"
    ),
    bslib::value_box(
      title = "Recording epochs",
      value = "10 s / 30 s",
      showcase = bsicons::bs_icon("stopwatch"),
      p("Participant / environment"),
      class = "summary-card"
    )
  ),
  tags$section(
    class = "quality-section",
    tags$div(
      class = "section-title-row",
      tags$div(tags$p(class = "eyebrow", "Data integrity"), tags$h2("The checks that affect every result")),
      actionLink("quality_prepare", "Open preparation", icon = bsicons::bs_icon("arrow-right"))
    ),
    tags$div(
      class = "quality-grid",
      quality_item("check2", "Implicit gaps", "None detected", "Regular series within each stream", "good", "review_gaps"),
      quality_item("check2", "Irregular timestamps", "None detected", "Dominant epochs are consistent", "good", "review_irregular"),
      quality_item("check2", "Explicit missingness", "0%", "No NA readings in the demo", "good", "review_missing"),
      quality_item("check2", "Valid days", "12 of 12", "Using ≥80% daily coverage", "good", "review_days")
    )
  ),
  bslib::layout_columns(
    col_widths = c(8, 4),
    class = "overview-columns",
    bslib::card(
      class = "plot-card",
      bslib::card_header(
        tags$div(
          class = "card-title-block",
          tags$h2("Exposure across the study"),
          tags$p("Night shading provides temporal context; values remain untransformed.")
        ),
        tags$div(
          class = "inline-controls",
          selectInput(
            "overview_id", "Stream",
            choices = c("All streams" = "All", "Participant", "Environment"),
            selected = "Participant",
            width = "170px"
          ),
          selectInput(
            "overview_variable", "Variable",
            choices = c("Melanopic EDI (MEDI)" = "MEDI"),
            selected = "MEDI",
            width = "190px"
          ),
          radioButtons(
            "overview_scale", "Scale",
            choices = c("Symlog" = "symlog", "Linear" = "linear"),
            selected = "symlog", inline = TRUE
          )
        )
      ),
      plotOutput("overview_plot", height = "390px")
    ),
    bslib::card(
      class = "availability-card",
      bslib::card_header(
        tags$div(
          class = "card-title-block",
          tags$h2("Daily availability"),
          tags$p("Coverage against each stream's own epoch.")
        )
      ),
      plotOutput("availability_plot", height = "280px"),
      tags$div(
        class = "availability-note",
        bsicons::bs_icon("info-circle"),
        tags$p("Coverage is complete in this demonstration dataset. Production views should reveal missing and irregular intervals here.")
      )
    )
  )
)

prepare_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Prepare",
    "Make analysis decisions in a deliberate order.",
    "Preview each change against the original data. Regularization can create explicit gaps; it never implies that missing light was measured."
  ),
  bslib::layout_columns(
    col_widths = c(7, 5),
    class = "prepare-layout",
    tags$div(
      class = "prepare-controls",
      bslib::accordion(
        id = "prepare_accordion",
        multiple = TRUE,
        open = c("time", "sampling"),
        bslib::accordion_panel(
          title = tagList(tags$span(class = "control-step", "1"), "Time and study window"),
          value = "time",
          tags$p(class = "control-intro", "Confirm the local interpretation before making clock-time or photoperiod comparisons."),
          bslib::layout_columns(
            col_widths = c(6, 6),
            selectInput(
              "prep_tz", "Analysis time zone",
              choices = c("Europe/Berlin", "UTC", "Europe/London", "America/New_York", "Asia/Singapore"),
              selected = "Europe/Berlin"
            ),
            dateRangeInput(
              "prep_range", "Study window",
              start = min(demo_dates), end = max(demo_dates),
              min = min(demo_dates), max = max(demo_dates)
            )
          ),
          bslib::input_switch("prep_dst", "Apply device-specific daylight-saving correction at import", value = TRUE),
          tags$p(class = "field-help", "DST correction belongs to import only when the source device did not handle the transition itself.")
        ),
        bslib::accordion_panel(
          title = tagList(tags$span(class = "control-step", "2"), "Measurement range and wear"),
          value = "range",
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            numericInput("prep_min", "Plausible minimum", value = 0, min = 0),
            numericInput("prep_max", "Plausible maximum", value = 100000, min = 1),
            selectInput(
              "prep_nonwear", "Non-wear / occlusion evidence",
              choices = c(
                "Do not infer" = "none",
                "Device wear flag" = "device",
                "External event log" = "log",
                "Low-variance rule" = "variance"
              )
            )
          ),
          bslib::input_switch("prep_flag_range", "Mark out-of-range readings as explicit NA", value = TRUE),
          tags$p(class = "field-help", "Keeping timestamps preserves duration accounting and makes invalidated observations auditable.")
        ),
        bslib::accordion_panel(
          title = tagList(tags$span(class = "control-step", "3"), "Sampling and gaps"),
          value = "sampling",
          bslib::input_switch("prep_aggregate", "Regularize to a common analysis interval", value = TRUE),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shinyWidgets::sliderTextInput(
              "prep_interval", "Analysis interval",
              choices = c("30 sec", "1 min", "5 min", "15 min", "1 hour"),
              selected = "5 min", grid = TRUE
            ),
            selectInput(
              "prep_function", "Numeric aggregation",
              choices = c("Mean" = "mean", "Median" = "median", "Maximum" = "max")
            )
          ),
          radioButtons(
            "prep_gaps", "Implicit gap handling",
            choices = c(
              "Convert to explicit NA observations" = "explicit_na",
              "Leave timestamps unchanged" = "leave"
            ),
            selected = "explicit_na"
          ),
          tags$div(
            class = "principle-note",
            bsicons::bs_icon("ban"),
            tags$p(tags$strong("No automatic imputation."), "Missing measurements remain missing in the generated analysis recipe.")
          )
        ),
        bslib::accordion_panel(
          title = tagList(tags$span(class = "control-step", "4"), "Daily inclusion"),
          value = "inclusion",
          bslib::input_switch("prep_remove_partial", "Exclude days that fail the completeness rule", value = TRUE),
          sliderInput(
            "prep_missing", "Maximum missing time per day",
            min = 0, max = 50, value = 20, step = 5, post = "%"
          ),
          tags$p(class = "field-help", "The threshold must be justified for the study duration and selected metrics; it is not a universal standard.")
        )
      ),
      actionButton(
        "apply_preparation", tagList(bsicons::bs_icon("check2"), "Add preparation to recipe"),
        class = "btn btn-primary apply-button"
      )
    ),
    tags$aside(
      class = "preview-column",
      bslib::card(
        class = "impact-card",
        bslib::card_header(tags$div(class = "card-title-block", tags$h2("Impact preview"), tags$p("Original data are never overwritten in this prototype."))),
        tags$div(
          class = "impact-stats",
          tags$div(tags$span("Observations"), tags$strong(textOutput("prep_rows", inline = TRUE)), tags$small("after regularization")),
          tags$div(tags$span("Daily coverage"), tags$strong("100%"), tags$small("unchanged in demo")),
          tags$div(tags$span("Invalidated values"), tags$strong(textOutput("prep_invalid", inline = TRUE)), tags$small("outside declared range"))
        ),
        plotOutput("prepare_plot", height = "280px"),
        uiOutput("preparation_state")
      )
    )
  )
)

group_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Group",
    "Define the units that every metric will describe.",
    "Grouping decisions are kept separate from metric selection so participant, day and context comparisons remain explicit."
  ),
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      class = "group-builder",
      bslib::card_header(tags$div(class = "card-title-block", tags$h2("Grouping recipe"), tags$p("Choose one or more nested dimensions."))),
      checkboxGroupInput(
        "group_dimensions", "Group results by",
        choiceNames = list(
          tags$span(tags$strong("Participant / stream"), tags$small("Preserve the source Id grouping")),
          tags$span(tags$strong("Calendar date"), tags$small("One result per local day")),
          tags$span(tags$strong("Weekday / weekend"), tags$small("Compare behavioural day types")),
          tags$span(tags$strong("Photoperiod"), tags$small("Day and night from coordinates"))
        ),
        choiceValues = c("participant", "date", "weekday", "photoperiod"),
        selected = c("participant", "date", "photoperiod")
      ),
      tags$hr(),
      bslib::input_switch("group_clock", "Add a clock-time window", value = FALSE),
      conditionalPanel(
        "input.group_clock",
        bslib::layout_columns(
          col_widths = c(6, 6),
          numericInput("group_clock_start", "Start hour", value = 6, min = 0, max = 23),
          numericInput("group_clock_end", "End hour", value = 12, min = 1, max = 24)
        )
      ),
      conditionalPanel(
        "input.group_dimensions && input.group_dimensions.indexOf('photoperiod') >= 0",
        tags$div(
          class = "context-fields",
          tags$div(class = "context-heading", bsicons::bs_icon("geo-alt"), tags$strong("Photoperiod context"), tags$span("Required")),
          bslib::layout_columns(
            col_widths = c(3, 3, 3, 3),
            numericInput("group_lat", "Latitude", value = 48.52, min = -90, max = 90),
            numericInput("group_lon", "Longitude", value = 9.06, min = -180, max = 180),
            selectInput("group_tz", "Time zone", choices = c("Europe/Berlin", "UTC"), selected = "Europe/Berlin"),
            selectInput(
              "group_solar_depression", "Solar depression",
              choices = c("Sunrise / sunset · 0°" = 0, "Civil twilight · 6°" = 6, "Nautical twilight · 12°" = 12),
              selected = 6
            )
          ),
          tags$p(class = "field-help", "Photoperiod is derived from date, time zone, coordinates and the selected solar-depression definition.")
        )
      ),
      actionButton("apply_grouping", tagList(bsicons::bs_icon("check2"), "Use these groups"), class = "btn btn-primary apply-button")
    ),
    bslib::card(
      class = "group-preview-card",
      bslib::card_header(tags$div(class = "card-title-block", tags$h2("Resulting grain"), tags$p("A readable preview before metrics are calculated."))),
      uiOutput("group_preview"),
      tags$div(
        class = "sample-table",
        tags$div(class = "sample-table-head", tags$span("Id"), tags$span("Day"), tags$span("Context")),
        tags$div(tags$span("Participant"), tags$span("29 Aug"), tags$span("day")),
        tags$div(tags$span("Participant"), tags$span("29 Aug"), tags$span("night")),
        tags$div(tags$span("Environment"), tags$span("29 Aug"), tags$span("day"))
      ),
      tags$p(class = "preview-footnote", "Preview rows are illustrative; the generated R script computes the full grouping from data.")
    )
  )
)

metrics_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Metrics",
    "Choose measures by the question they answer.",
    "Definitions, requirements and parameters remain visible. Defaults are starting points for analysis—not universal exposure targets."
  ),
  tags$div(
    class = "metric-toolbar",
    textInput(
      "metric_search", tags$span(class = "visually-hidden", "Search metrics"),
      placeholder = "Search metric or function…", width = "320px"
    ),
    selectInput(
      "metric_family", tags$span(class = "visually-hidden", "Metric family"),
      choices = c("All families", sort(unique(metric_definitions$family))),
      selected = "All families", width = "220px"
    ),
    tags$span(class = "selection-context", bsicons::bs_icon("diagram-3"), textOutput("metric_group_context", inline = TRUE))
  ),
  bslib::layout_columns(
    col_widths = c(8, 4),
    tags$div(class = "metric-catalog", uiOutput("metric_catalog")),
    tags$aside(
      class = "metric-config",
      bslib::card(
        bslib::card_header(tags$div(class = "card-title-block", tags$h2(textOutput("metric_config_title", inline = TRUE)), tags$p(textOutput("metric_config_function", inline = TRUE)))),
        uiOutput("metric_parameters"),
        tags$p(
          class = "metric-provenance",
          bsicons::bs_icon("journal-check"),
          "Parameter source: researcher-specified and recorded in the R recipe."
        ),
        tags$div(
          class = "metric-result",
          tags$span("Participant · all selected days"),
          tags$strong(textOutput("metric_value", inline = TRUE)),
          tags$p(textOutput("metric_detail", inline = TRUE))
        ),
        actionButton("add_metric", tagList(bsicons::bs_icon("plus-lg"), "Add metric to recipe"), class = "btn btn-primary apply-button"),
        uiOutput("metric_recipe_state")
      ),
      tags$div(
        class = "principle-note metric-caution",
        bsicons::bs_icon("exclamation-triangle"),
        tags$p(tags$strong("Interpret with context."), "Thresholds and metric families should be chosen for the protocol, sensor and research question.")
      )
    )
  )
)

visualize_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Visualize",
    "Choose a view for the pattern you need to inspect.",
    "The gallery mirrors common LightLogR views while keeping configuration and export intent close to the figure."
  ),
  uiOutput("viz_gallery"),
  bslib::layout_columns(
    col_widths = c(9, 3),
    bslib::card(
      class = "visual-card",
      bslib::card_header(
        tags$div(class = "card-title-block", tags$h2(textOutput("viz_title", inline = TRUE)), tags$p(textOutput("viz_description", inline = TRUE))),
        tags$span(class = "plot-provenance", "LightLogR sample data")
      ),
      plotOutput("visual_plot", height = "470px")
    ),
    tags$aside(
      class = "visual-controls",
      bslib::card(
        bslib::card_header(tags$h2("Figure settings")),
        selectInput("viz_id", "Stream", choices = c("Participant", "Environment"), selected = "Participant"),
        selectInput("viz_interval", "Display interval", choices = c("1 min", "5 min", "15 min", "1 hour"), selected = "5 min"),
        radioButtons("viz_scale", "Exposure scale", choices = c("Symlog" = "symlog", "Linear" = "linear"), selected = "symlog", inline = TRUE),
        bslib::input_switch("viz_photoperiod", "Show night context where applicable", value = TRUE),
        tags$hr(),
        actionButton("export_plot", tagList(bsicons::bs_icon("image"), "Export plot"), class = "btn btn-outline-primary w-100"),
        actionButton("export_data", tagList(bsicons::bs_icon("table"), "Export plotted data"), class = "btn btn-light w-100"),
        tags$p(class = "field-help", "Plot and data export controls demonstrate placement; the R recipe download is functional.")
      )
    )
  )
)

recipe_checkbox <- function(id, number, title, description, checked = TRUE) {
  tags$div(
    class = "recipe-step",
    checkboxInput(
      id,
      label = tags$span(
        class = "recipe-label",
        tags$span(class = "recipe-number", number),
        tags$span(tags$strong(title), tags$small(description))
      ),
      value = checked
    )
  )
}

recipe_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Analysis recipe",
    "A transparent bridge from interface choices to R.",
    "Disable representative steps to inspect how the generated script changes. Download the result when the recipe matches the intended analysis."
  ),
  bslib::layout_columns(
    col_widths = c(5, 7),
    bslib::card(
      class = "recipe-card",
      bslib::card_header(tags$div(class = "card-title-block", tags$h2("Included steps"), tags$p("Chronological and reversible in the prototype."))),
      recipe_checkbox("recipe_source", "01", "Import source files", "Device, time zone and participant identifiers"),
      recipe_checkbox("recipe_timezone", "02", "Confirm time zone", "Preserve local clock interpretation"),
      recipe_checkbox("recipe_range", "03", "Validate measurement range", "Keep timestamps; mark invalid readings NA"),
      recipe_checkbox("recipe_gaps", "04", "Make gaps explicit", "No automatic imputation"),
      recipe_checkbox("recipe_sampling", "05", "Regularize sampling", "Selected epoch and numeric handler"),
      recipe_checkbox("recipe_inclusion", "06", "Apply daily inclusion", "Declared missingness threshold"),
      recipe_checkbox("recipe_grouping", "07", "Create analysis groups", "Participant, day and temporal context"),
      recipe_checkbox("recipe_metric", "08", "Calculate metric", "Selected family and parameters"),
      recipe_checkbox("recipe_visual", "09", "Build visualization", "Selected LightLogR view"),
      downloadButton("download_script_recipe", tagList(bsicons::bs_icon("download"), "Download R script"), class = "btn btn-primary w-100")
    ),
    bslib::card(
      class = "code-card",
      bslib::card_header(
        tags$div(class = "card-title-block", tags$h2("R preview"), tags$p("Readable, relative-path code without interface identifiers.")),
        tags$span(class = "status-pill", bsicons::bs_icon("file-earmark-code"), "analysis.R")
      ),
      verbatimTextOutput("script_preview")
    )
  )
)

metadata_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Dataset metadata",
    "Keep measurement context beside the data.",
    "The production product should expand this lightweight view into study-, participant-, device- and dataset-level metadata completeness."
  ),
  bslib::layout_columns(
    col_widths = c(8, 4),
    bslib::card(
      bslib::card_header(tags$div(class = "card-title-block", tags$h2("Recorded metadata"), tags$p("Values used by plots, photoperiod and reporting."))),
      gt::gt_output("metadata_table")
    ),
    bslib::card(
      class = "metadata-completeness",
      bslib::card_header(tags$h2("Reporting readiness")),
      tags$div(class = "completeness-ring", tags$strong("8/12"), tags$span("fields")),
      tags$ul(
        tags$li(class = "complete", bsicons::bs_icon("check2"), "Device and sampling"),
        tags$li(class = "complete", bsicons::bs_icon("check2"), "Location and time zone"),
        tags$li(class = "incomplete", bsicons::bs_icon("dash"), "Wearing location"),
        tags$li(class = "incomplete", bsicons::bs_icon("dash"), "Calibration and firmware")
      )
    )
  )
)

raw_ui <- tags$section(
  class = "workspace-screen",
  screen_heading(
    "Raw data preview",
    "Inspect source values without making the table the centre of the workflow.",
    "The first 500 observations are shown. The production import should keep original and processed data as separate objects."
  ),
  bslib::card(
    class = "raw-card",
    bslib::card_header(
      tags$div(class = "card-title-block", tags$h2("sample.data.environment"), tags$p("69,120 rows · 3 columns · grouped by Id")),
      tags$span(class = "status-pill", "Original")
    ),
    DT::DTOutput("raw_table")
  )
)

workspace_ui <- tags$div(
  class = "workspace-shell",
  tags$header(
    class = "workspace-header",
    actionButton(
      "brand_home", brand_mark(compact = TRUE),
      class = "brand-home btn btn-link",
      `aria-label` = "Return to LightLogWeb home"
    ),
    tags$div(
      class = "dataset-switcher",
      selectInput(
        "active_dataset", "Active dataset",
        choices = c("sample.data.environment"),
        selected = "sample.data.environment", width = "250px"
      ),
      tags$span(class = "dataset-state", bsicons::bs_icon("check-circle-fill"), "Saved in session")
    ),
    tags$div(
      class = "header-actions",
      tags$a(
        class = "header-help",
        href = "https://tscnlab.github.io/LightLogR/",
        target = "_blank", rel = "noopener",
        bsicons::bs_icon("question-circle"), tags$span("Help")
      ),
      actionButton("open_recipe", tagList(bsicons::bs_icon("list-check"), "Analysis recipe"), class = "btn btn-light"),
      downloadButton(
        "download_script", tagList(bsicons::bs_icon("download"), "Download analysis"),
        class = "btn btn-primary", `aria-label` = "Download analysis as an R script"
      )
    )
  ),
  bslib::layout_sidebar(
    class = "workspace-layout",
    sidebar = bslib::sidebar(
      width = 260,
      open = "desktop",
      tags$p(class = "sidebar-label", "Workflow"),
      radioButtons(
        "workspace_section", tags$span(class = "visually-hidden", "Workflow section"),
        choiceNames = list(
          tags$span(bsicons::bs_icon("grid-1x2"), tags$span(tags$strong("Overview"), tags$small("Shape & quality"))),
          tags$span(bsicons::bs_icon("sliders"), tags$span(tags$strong("Prepare"), tags$small("Clean & regularize"))),
          tags$span(bsicons::bs_icon("diagram-3"), tags$span(tags$strong("Group"), tags$small("Define analysis units"))),
          tags$span(bsicons::bs_icon("calculator"), tags$span(tags$strong("Metrics"), tags$small("Calculate summaries"))),
          tags$span(bsicons::bs_icon("bar-chart-line"), tags$span(tags$strong("Visualize"), tags$small("Explore patterns")))
        ),
        choiceValues = c("overview", "prepare", "group", "metrics", "visualize"),
        selected = "overview"
      ),
      tags$div(class = "sidebar-separator"),
      tags$p(class = "sidebar-label", "Dataset"),
      actionButton("show_metadata", tagList(bsicons::bs_icon("card-list"), "Metadata"), class = "sidebar-action btn btn-link"),
      actionButton("show_raw", tagList(bsicons::bs_icon("table"), "Raw table"), class = "sidebar-action btn btn-link"),
      actionButton("show_recipe", tagList(bsicons::bs_icon("code-square"), "Analysis recipe"), class = "sidebar-action btn btn-link"),
      actionButton("add_dataset", tagList(bsicons::bs_icon("plus-circle"), "Add another dataset"), class = "sidebar-action btn btn-link"),
      tags$div(
        class = "sidebar-footnote",
        bsicons::bs_icon("info-circle"),
        tags$p("This design prototype uses the bundled LightLogR sample dataset.")
      )
    ),
    bslib::navset_hidden(
      id = "workspace_view",
      selected = "overview",
      bslib::nav_panel("Overview", value = "overview", overview_ui),
      bslib::nav_panel("Prepare", value = "prepare", prepare_ui),
      bslib::nav_panel("Group", value = "group", group_ui),
      bslib::nav_panel("Metrics", value = "metrics", metrics_ui),
      bslib::nav_panel("Visualize", value = "visualize", visualize_ui),
      bslib::nav_panel("Recipe", value = "recipe", recipe_ui),
      bslib::nav_panel("Metadata", value = "metadata", metadata_ui),
      bslib::nav_panel("Raw", value = "raw", raw_ui)
    )
  )
)

ui <- bslib::page_fillable(
  theme = app_theme,
  title = "LightLogWeb · Clickable mockup",
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "description", content = "A clickable design prototype for transparent wearable light exposure analysis."),
    includeCSS("www/styles.css")
  ),
  bslib::navset_hidden(
    id = "app_screen",
    selected = "landing",
    bslib::nav_panel("Landing", value = "landing", landing_ui),
    bslib::nav_panel("Workspace", value = "workspace", workspace_ui)
  )
)

server <- function(input, output, session) {
  preparation_applied <- reactiveVal(FALSE)
  grouping_applied <- reactiveVal(FALSE)
  metric_added <- reactiveVal(FALSE)

  go_to_workspace <- function() {
    bslib::nav_select("app_screen", selected = "workspace", session = session)
  }

  open_import_modal <- function() {
    showModal(
      modalDialog(
        class = "import-modal",
        size = "l",
        easyClose = TRUE,
        footer = NULL,
        title = NULL,
        tags$div(
          class = "import-modal-header",
          tags$div(class = "import-close-action", actionButton("close_import", "Close import", class = "btn btn-light")),
          tags$p(class = "eyebrow", "Import concept"),
          tags$h2("Bring a wearable dataset into focus"),
          tags$p("This wizard demonstrates the intended configuration. In the mockup, previewing continues with the bundled sample data.")
        ),
        bslib::navset_hidden(
          id = "import_steps",
          selected = "files",
          bslib::nav_panel(
            "Files", value = "files",
            tags$div(
              class = "import-stepper",
              tags$span(class = "active", "1 · Source"), tags$span("2 · Time & IDs"), tags$span("3 · Preview")
            ),
            bslib::layout_columns(
              col_widths = c(7, 5),
              tags$div(
                fileInput(
                  "import_files", "Choose device export files",
                  multiple = TRUE, accept = c(".csv", ".txt", ".zip")
                ),
                tags$p(class = "field-help", "Files are displayed but not parsed by this design prototype.")
              ),
              tags$div(
                selectInput(
                  "import_device", "Device",
                  choices = c("ActLumus", "Actiwatch_Spectrum", "Clouclip", "LiDo", "LYS", "VEET"),
                  selected = "ActLumus"
                ),
                selectInput("import_version", "File-format version", choices = c("Auto-detect", "Current export"))
              )
            ),
            tags$div(class = "modal-actions", actionButton("import_next_time", "Continue to time & IDs", class = "btn btn-primary"))
          ),
          bslib::nav_panel(
            "Time", value = "time",
            tags$div(
              class = "import-stepper",
              tags$span(class = "complete", "1 · Source"), tags$span(class = "active", "2 · Time & IDs"), tags$span("3 · Preview")
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              selectInput(
                "import_tz", "Time zone of collection",
                choices = c("Europe/Berlin", "UTC", "Europe/London", "America/New_York", "Asia/Singapore"),
                selected = "Europe/Berlin"
              ),
              selectInput(
                "import_id_method", "Participant identifiers",
                choices = c(
                  "Derive from filenames" = "automated",
                  "Use one manual ID" = "manual",
                  "Extract with a pattern" = "extract"
                ),
                selected = "automated"
              ),
              bslib::input_switch("import_dst", "Correct device DST jumps", value = TRUE),
              textInput("import_id_value", "ID or extraction pattern", value = ".*", placeholder = "e.g. P[0-9]+")
            ),
            tags$div(
              class = "modal-actions modal-actions-split",
              actionButton("import_back_files", "Back", class = "btn btn-light"),
              actionButton("import_next_preview", "Preview import", class = "btn btn-primary")
            )
          ),
          bslib::nav_panel(
            "Preview", value = "preview",
            tags$div(
              class = "import-stepper",
              tags$span(class = "complete", "1 · Source"), tags$span(class = "complete", "2 · Time & IDs"), tags$span(class = "active", "3 · Preview")
            ),
            tags$div(
              class = "import-preview",
              tags$span(class = "import-preview-icon", bsicons::bs_icon("check2-circle")),
              tags$div(
                tags$h3("Configuration is ready to inspect"),
                uiOutput("import_file_summary"),
                tags$p("The prototype will open sample.data.environment so the complete workspace can be explored without uploading research data.")
              )
            ),
            tags$div(
              class = "preview-facts",
              tags$div(tags$span("Device / format"), tags$strong(textOutput("import_device_summary", inline = TRUE))),
              tags$div(tags$span("Time zone"), tags$strong(textOutput("import_tz_summary", inline = TRUE))),
              tags$div(tags$span("ID handling"), tags$strong(textOutput("import_id_summary", inline = TRUE)))
            ),
            tags$div(
              class = "modal-actions modal-actions-split",
              actionButton("import_back_time", "Back", class = "btn btn-light"),
              actionButton("use_demo_import", "Open configured demo", class = "btn btn-primary")
            )
          )
        )
      )
    )
  }

  observeEvent(input$explore_demo, go_to_workspace())
  observeEvent(input$brand_home, bslib::nav_select("app_screen", selected = "landing", session = session))
  observeEvent(input$start_import, open_import_modal())
  observeEvent(input$landing_import, open_import_modal())
  observeEvent(input$add_dataset, open_import_modal())
  observeEvent(input$close_import, removeModal())

  observeEvent(input$import_next_time, bslib::nav_select("import_steps", selected = "time", session = session))
  observeEvent(input$import_back_files, bslib::nav_select("import_steps", selected = "files", session = session))
  observeEvent(input$import_next_preview, bslib::nav_select("import_steps", selected = "preview", session = session))
  observeEvent(input$import_back_time, bslib::nav_select("import_steps", selected = "time", session = session))
  observeEvent(input$use_demo_import, {
    selected_tz <- input$import_tz %or% "Europe/Berlin"
    updateSelectInput(session, "prep_tz", selected = selected_tz)
    updateSelectInput(session, "group_tz", selected = selected_tz)
    updateCheckboxInput(session, "prep_dst", value = isTRUE(input$import_dst %or% TRUE))
    removeModal()
    go_to_workspace()
    showNotification("The configured demonstration dataset is ready.", type = "message")
  })

  output$import_file_summary <- renderUI({
    if (is.null(input$import_files)) {
      tags$p(tags$strong("No local files selected."), " The demo source will be used.")
    } else {
      tags$p(tags$strong(nrow(input$import_files), " file(s) selected:"), paste(input$import_files$name, collapse = ", "))
    }
  })
  output$import_device_summary <- renderText(paste(input$import_device %or% "ActLumus", "·", input$import_version %or% "Auto-detect"))
  output$import_tz_summary <- renderText(input$import_tz %or% "Europe/Berlin")
  output$import_id_summary <- renderText({
    switch(input$import_id_method %or% "automated", manual = "Manual ID", extract = "Filename pattern", "From filenames")
  })

  observeEvent(input$workspace_section, {
    bslib::nav_select("workspace_view", selected = input$workspace_section, session = session)
  }, ignoreInit = TRUE)

  navigate_workspace <- function(value) {
    bslib::nav_select("workspace_view", selected = value, session = session)
    if (value %in% c("overview", "prepare", "group", "metrics", "visualize")) {
      updateRadioButtons(session, "workspace_section", selected = value)
    }
  }

  observeEvent(input$quality_prepare, navigate_workspace("prepare"))
  observeEvent(input$review_gaps, navigate_workspace("prepare"))
  observeEvent(input$review_irregular, navigate_workspace("prepare"))
  observeEvent(input$review_missing, navigate_workspace("prepare"))
  observeEvent(input$review_days, navigate_workspace("prepare"))
  observeEvent(input$show_metadata, navigate_workspace("metadata"))
  observeEvent(input$show_raw, navigate_workspace("raw"))
  observeEvent(input$show_recipe, navigate_workspace("recipe"))
  observeEvent(input$open_recipe, navigate_workspace("recipe"))

  plot_theme <- function() {
    theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#FFFFFF", colour = NA),
        panel.background = element_rect(fill = "#FFFFFF", colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E6EDF1", linewidth = 0.45),
        axis.title = element_text(colour = "#49616D"),
        axis.text = element_text(colour = "#5E7480"),
        strip.text = element_text(face = "bold", colour = "#0B3041"),
        strip.background = element_rect(fill = "#EDF4F6", colour = NA),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(12, 18, 10, 12)
      )
  }

  add_symlog_y <- function(plot, selected_scale) {
    if (identical(selected_scale, "symlog")) {
      plot + scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), labels = scales::label_number())
    } else {
      plot + scale_y_continuous(labels = scales::label_number())
    }
  }

  night_rectangles <- function(data) {
    days <- seq.Date(
      as.Date(min(data$Datetime), tz = "Europe/Berlin"),
      as.Date(max(data$Datetime), tz = "Europe/Berlin"), by = "day"
    )
    rbind(
      data.frame(
        xmin = as.POSIXct(paste(days, "00:00:00"), tz = "Europe/Berlin"),
        xmax = as.POSIXct(paste(days, "06:00:00"), tz = "Europe/Berlin")
      ),
      data.frame(
        xmin = as.POSIXct(paste(days, "20:00:00"), tz = "Europe/Berlin"),
        xmax = as.POSIXct(paste(days + 1, "00:00:00"), tz = "Europe/Berlin")
      )
    )
  }

  output$overview_plot <- renderPlot({
    selected <- input$overview_id %or% "Participant"
    data <- demo_5min
    if (!identical(selected, "All")) data <- data[as.character(data$Id) == selected, , drop = FALSE]
    night <- night_rectangles(data)

    p <- ggplot(data, aes(Datetime, MEDI, colour = Id)) +
      geom_rect(
        data = night,
        aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
        inherit.aes = FALSE, fill = "#DDE8EE", alpha = 0.65
      ) +
      geom_line(linewidth = 0.6, alpha = 0.9) +
      scale_color_manual(values = c("Environment" = "#D8873D", "Participant" = "#0B789C")) +
      labs(x = NULL, y = "Melanopic EDI (lx)") +
      plot_theme()
    if (identical(selected, "All")) p <- p + facet_wrap(~Id, ncol = 1, scales = "free_y")
    add_symlog_y(p, input$overview_scale %or% "symlog")
  }, res = 110, alt = "Light-exposure timeline for the selected stream, variable and scale")

  availability_data <- reactive({
    full <- expand.grid(Id = demo_ids, Date = demo_dates, stringsAsFactors = FALSE)
    full$Coverage <- 1
    full
  })

  output$availability_plot <- renderPlot({
    ggplot(availability_data(), aes(Date, Id, fill = Coverage)) +
      geom_tile(width = 0.88, height = 0.55, colour = "#FFFFFF", linewidth = 1.5) +
      scale_fill_gradient(low = "#E7EEF1", high = "#84C8D8", limits = c(0, 1), guide = "none") +
      scale_x_date(date_labels = "%d %b", date_breaks = "2 days", expand = expansion(add = 0.35)) +
      labs(x = NULL, y = NULL) +
      plot_theme() +
      theme(
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(8, 8, 4, 8)
      )
  }, res = 110, alt = "Daily data availability by stream and calendar date")

  preparation_rows <- reactive({
    if (!isTRUE(input$prep_aggregate %or% TRUE)) return(nrow(demo_data))
    switch(
      input$prep_interval %or% "5 min",
      "30 sec" = 34560,
      "1 min" = 17280,
      "5 min" = 3458,
      "15 min" = 1154,
      "1 hour" = 290,
      3458
    )
  })

  output$prep_rows <- renderText(format(preparation_rows(), big.mark = ","))
  output$prep_invalid <- renderText({
    if (!isTRUE(input$prep_flag_range %or% TRUE)) return("Not checked")
    low <- input$prep_min %or% 0
    high <- input$prep_max %or% 100000
    invalid <- sum(demo_data$MEDI < low | demo_data$MEDI > high, na.rm = TRUE)
    sprintf("%s (%s)", format(invalid, big.mark = ","), scales::percent(invalid / nrow(demo_data), accuracy = 0.1))
  })

  output$prepare_plot <- renderPlot({
    interval <- input$prep_interval %or% "5 min"
    regular <- if (isTRUE(input$prep_aggregate %or% TRUE)) {
      LightLogR::aggregate_Datetime(demo_data, unit = interval) |> as.data.frame()
    } else {
      demo_data |> as.data.frame()
    }
    first_day <- min(demo_dates)
    raw <- demo_data[
      as.character(demo_data$Id) == "Participant" & as.Date(demo_data$Datetime, tz = "Europe/Berlin") == first_day,
      , drop = FALSE
    ]
    after <- regular[
      as.character(regular$Id) == "Participant" & as.Date(regular$Datetime, tz = "Europe/Berlin") == first_day,
      , drop = FALSE
    ]
    ggplot() +
      geom_line(data = raw, aes(Datetime, MEDI), colour = "#A8B8C0", linewidth = 0.35, alpha = 0.7) +
      geom_line(data = after, aes(Datetime, MEDI), colour = "#0B789C", linewidth = 0.8) +
      scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), labels = scales::label_number()) +
      labs(x = NULL, y = "Melanopic EDI (lx)", subtitle = "Grey: recording epoch · Blue: selected analysis interval") +
      plot_theme() +
      theme(legend.position = "none", plot.subtitle = element_text(colour = "#5E7480", size = 9))
  }, res = 110, alt = "Before-and-after preview of the selected sampling interval")

  output$preparation_state <- renderUI({
    if (preparation_applied()) {
      tags$div(class = "applied-state", bsicons::bs_icon("check-circle-fill"), tags$p(tags$strong("Added to recipe"), "The R handoff now reflects these preparation choices."))
    } else {
      tags$div(class = "pending-state", bsicons::bs_icon("circle"), tags$p(tags$strong("Preview only"), "Add the configuration when it represents the intended analysis."))
    }
  })

  observeEvent(input$apply_preparation, {
    preparation_applied(TRUE)
    showNotification("Preparation choices added to the analysis recipe.", type = "message")
  })

  output$group_preview <- renderUI({
    dimensions <- input$group_dimensions %or% character(0)
    has_photo <- "photoperiod" %in% dimensions
    has_day <- "date" %in% dimensions
    groups <- 1
    if ("participant" %in% dimensions) groups <- groups * 2
    if (has_day) groups <- groups * 6
    if ("weekday" %in% dimensions) groups <- groups * 2
    if (has_photo) groups <- groups * 2
    if (isTRUE(input$group_clock %or% FALSE)) groups <- groups * 2
    tags$div(
      class = "group-grain",
      tags$span("Expected result groups"),
      tags$strong(groups),
      tags$p(if (has_photo) "Each selected day is separated into daylight and night context." else "No photoperiod context is currently included."),
      if (has_photo) tags$p(class = "preview-context", sprintf("Photoperiod uses %s and a %s° solar-depression definition.", input$group_tz %or% "Europe/Berlin", input$group_solar_depression %or% 6)),
      tags$div(
        class = "group-badges",
        lapply(dimensions, function(x) tags$span(class = "group-badge", switch(x, participant = "Id", date = "Calendar day", weekday = "Day type", photoperiod = "Photoperiod", x))),
        if (isTRUE(input$group_clock %or% FALSE)) tags$span(class = "group-badge", sprintf("%02d:00–%02d:00", input$group_clock_start, input$group_clock_end))
      ),
      if (grouping_applied()) tags$span(class = "applied-inline", bsicons::bs_icon("check2"), "In recipe")
    )
  })

  observeEvent(input$apply_grouping, {
    dimensions <- input$group_dimensions %or% character(0)
    if ("photoperiod" %in% dimensions) {
      has_context <- isTRUE(is.finite(input$group_lat)) &&
        isTRUE(is.finite(input$group_lon)) &&
        nzchar(input$group_tz %or% "")
      if (!has_context) {
        showNotification("Coordinates and a time zone are required before photoperiod can be added.", type = "error")
        return()
      }
      updateSelectInput(session, "prep_tz", selected = input$group_tz)
    }
    grouping_applied(TRUE)
    showNotification("Grouping choices added to the analysis recipe.", type = "message")
  })

  output$metric_group_context <- renderText({
    dimensions <- input$group_dimensions %or% c("participant", "date", "photoperiod")
    labels <- vapply(
      dimensions,
      function(x) switch(x, participant = "Id", date = "day", weekday = "day type", photoperiod = "photoperiod", x),
      character(1)
    )
    paste("Grouped by", paste(labels, collapse = " · "))
  })

  output$metric_catalog <- renderUI({
    family <- input$metric_family %or% "All families"
    query <- tolower(trimws(input$metric_search %or% ""))
    visible <- metric_definitions
    if (!identical(family, "All families")) visible <- visible[visible$family == family, , drop = FALSE]
    if (nzchar(query)) {
      haystack <- tolower(paste(visible$name, visible$function_name, visible$summary, visible$family))
      visible <- visible[grepl(query, haystack, fixed = TRUE), , drop = FALSE]
    }
    if (!nrow(visible)) return(tags$div(class = "empty-state", bsicons::bs_icon("search"), tags$p("No metrics match this filter.")))

    selected <- input$metric_selected %or% "tat"
    if (!selected %in% visible$id) selected <- visible$id[[1]]
    radioButtons(
      "metric_selected", tags$span(class = "visually-hidden", "Choose a metric"),
      choiceNames = lapply(seq_len(nrow(visible)), function(i) {
        tags$span(
          class = "metric-choice-content",
          tags$span(class = "metric-family", visible$family[[i]]),
          tags$strong(visible$name[[i]]),
          tags$p(visible$summary[[i]]),
          tags$span(
            class = "metric-meta",
            tags$code(visible$function_name[[i]]),
            tags$span(visible$requirement[[i]]),
            tags$span(class = "metric-unit", visible$unit[[i]])
          )
        )
      }),
      choiceValues = visible$id,
      selected = selected
    )
  })

  selected_metric_row <- reactive({
    id <- input$metric_selected %or% "tat"
    row <- metric_definitions[metric_definitions$id == id, , drop = FALSE]
    if (!nrow(row)) row <- metric_definitions[1, , drop = FALSE]
    row
  })

  output$metric_config_title <- renderText(selected_metric_row()$name[[1]])
  output$metric_config_function <- renderText(selected_metric_row()$function_name[[1]])

  output$metric_parameters <- renderUI({
    id <- selected_metric_row()$id[[1]]
    switch(
      id,
      tat = tagList(
        numericInput("metric_threshold", "Threshold", value = 250, min = 0),
        selectInput("metric_direction", "Condition", choices = c("Above threshold" = "above", "Below threshold" = "below")),
        tags$p(class = "field-help", "The value and unit come from the selected primary variable; they are not a health recommendation.")
      ),
      bright_dark = tagList(
        selectInput("metric_period", "Period", choices = c("Brightest" = "brightest", "Darkest" = "darkest")),
        selectInput("metric_timespan", "Window length", choices = c("5 hours", "10 hours", "12 hours"), selected = "10 hours"),
        bslib::input_switch("metric_loop", "Allow the period to cross midnight", value = TRUE)
      ),
      ema = tagList(
        selectInput(
          "metric_decay", "Decay half-life",
          choices = c("30 minutes" = "30 min", "90 minutes" = "90 min", "3 hours" = "3 hours", "12 hours" = "12 hours"),
          selected = "90 min"
        ),
        tags$p(class = "field-help", "The output is a time-varying exposure-history series. Missing light values are treated as zero by this LightLogR function and should be reviewed first.")
      ),
      spectral = tagList(
        bslib::layout_columns(
          col_widths = c(6, 6),
          numericInput("spectral_min", "Minimum wavelength", value = 380, min = 200, max = 1000, step = 1),
          numericInput("spectral_max", "Maximum wavelength", value = 780, min = 200, max = 1000, step = 1)
        ),
        tags$div(
          class = "parameter-note",
          bsicons::bs_icon("info-circle"),
          tags$p("The bundled demo contains melanopic EDI rather than spectral irradiance. The R handoff therefore adds an explicit data/spectrum.csv placeholder.")
        )
      ),
      iv = tags$div(class = "parameter-note", bsicons::bs_icon("info-circle"), tags$p("Uses the current regular sampling interval. Inspect gaps before interpretation.")),
      is = tags$div(class = "parameter-note", bsicons::bs_icon("info-circle"), tags$p("LightLogR calculates IS from hourly means; the generated script records this metric choice.")),
      tags$div(class = "parameter-note", bsicons::bs_icon("check2-circle"), tags$p("No additional parameters are required for this metric."))
    )
  })

  metric_result <- reactive({
    id <- selected_metric_row()$id[[1]]
    data <- demo_data[as.character(demo_data$Id) == "Participant", , drop = FALSE]
    switch(
      id,
      dose = {
        value <- LightLogR::dose(data$MEDI, data$Datetime, as.df = TRUE)[[1]][[1]]
        list(value = paste0(scales::number(value, accuracy = 1), " lx·h"), detail = "Integrated melanopic EDI exposure over six days")
      },
      centroid = {
        value <- LightLogR::centroidLE(data$MEDI, data$Datetime, as.df = TRUE)[[1]][[1]]
        list(value = format(value, "%d %b · %H:%M"), detail = "Half of cumulative exposure occurred by this moment")
      },
      bright_dark = {
        period <- input$metric_period %or% "brightest"
        span <- input$metric_timespan %or% "10 hours"
        result <- LightLogR::bright_dark_period(data$MEDI, data$Datetime, period = period, timespan = span, loop = isTRUE(input$metric_loop %or% TRUE), as.df = TRUE)
        list(value = paste0(scales::number(result[[1]][[1]], accuracy = 1), " lx"), detail = paste(tools::toTitleCase(period), span, "mean"))
      },
      iv = {
        value <- LightLogR::intradaily_variability(data$MEDI, data$Datetime, as.df = TRUE)[[1]][[1]]
        list(value = scales::number(value, accuracy = 0.001), detail = "Higher values indicate greater within-day fragmentation")
      },
      is = {
        value <- LightLogR::interdaily_stability(data$MEDI, data$Datetime, as.df = TRUE)[[1]][[1]]
        list(value = scales::number(value, accuracy = 0.001), detail = "Similarity of the light pattern across study days")
      },
      ema = {
        history_data <- demo_5min[as.character(demo_5min$Id) == "Participant", , drop = FALSE]
        decay <- input$metric_decay %or% "90 min"
        history <- LightLogR::exponential_moving_average(history_data$MEDI, history_data$Datetime, decay = decay)
        list(
          value = paste0(scales::number(tail(history, 1), accuracy = 0.1), " lx"),
          detail = paste("Exposure-history-weighted value at the final sample · decay", decay)
        )
      },
      spectral = list(
        value = "Input required",
        detail = "Select a spectral irradiance file to calculate this metric in production"
      ),
      {
        threshold <- input$metric_threshold %or% 250
        direction <- input$metric_direction %or% "above"
        value <- LightLogR::duration_above_threshold(data$MEDI, data$Datetime, comparison = direction, threshold = threshold, as.df = TRUE)[[1]][[1]]
        hours <- as.numeric(value, units = "hours")
        list(value = paste0(scales::number(hours, accuracy = 0.1), " h"), detail = paste("Total time", direction, threshold, "lx melanopic EDI"))
      }
    )
  })

  output$metric_value <- renderText(metric_result()$value)
  output$metric_detail <- renderText(metric_result()$detail)

  output$metric_recipe_state <- renderUI({
    if (metric_added()) {
      tags$div(class = "applied-state metric-added-state", bsicons::bs_icon("check-circle-fill"), tags$p(tags$strong("Added to recipe"), "The selected function and parameters are present in the R handoff."))
    } else {
      tags$div(class = "pending-state metric-added-state", bsicons::bs_icon("circle"), tags$p(tags$strong("Not yet confirmed"), "Review the definition and parameters before adding this metric."))
    }
  })

  observeEvent(input$metric_selected, metric_added(FALSE), ignoreInit = TRUE)

  observeEvent(input$add_metric, {
    metric_added(TRUE)
    showNotification(paste(selected_metric_row()$name[[1]], "added to the analysis recipe."), type = "message")
  })

  output$viz_gallery <- renderUI({
    radioButtons(
      "viz_type", tags$span(class = "visually-hidden", "Choose a visualization"),
      choiceNames = lapply(seq_len(nrow(viz_definitions)), function(i) {
        tags$span(
          class = "viz-choice-content",
          tags$span(class = "viz-choice-icon", bsicons::bs_icon(c("calendar-check", "clock", "activity", "grid-3x3", "arrow-left-right", "cloud-sun", "bar-chart", "graph-up")[[i]])),
          tags$span(tags$strong(viz_definitions$name[[i]]), tags$small(viz_definitions$description[[i]]))
        )
      }),
      choiceValues = viz_definitions$id,
      selected = "timeline",
      inline = TRUE
    )
  })

  selected_viz_row <- reactive({
    id <- input$viz_type %or% "timeline"
    viz_definitions[viz_definitions$id == id, , drop = FALSE]
  })
  output$viz_title <- renderText(selected_viz_row()$name[[1]])
  output$viz_description <- renderText(selected_viz_row()$description[[1]])

  visualization_data <- reactive({
    interval <- input$viz_interval %or% "5 min"
    LightLogR::aggregate_Datetime(demo_data, unit = interval) |>
      as.data.frame()
  })

  output$visual_plot <- renderPlot({
    type <- input$viz_type %or% "timeline"
    selected_id <- input$viz_id %or% "Participant"
    data <- visualization_data()
    data <- data[as.character(data$Id) == selected_id, , drop = FALSE]
    data$Date <- as.Date(data$Datetime, tz = "Europe/Berlin")
    data$Hour <- as.numeric(format(data$Datetime, "%H", tz = "Europe/Berlin")) + as.numeric(format(data$Datetime, "%M", tz = "Europe/Berlin")) / 60

    if (identical(type, "overview")) {
      coverage <- data.frame(Date = demo_dates, Coverage = 1)
      p <- ggplot(coverage, aes(Date, 1, fill = Coverage)) +
        geom_tile(width = 0.88, height = 0.62, colour = "#FFFFFF", linewidth = 2) +
        geom_text(aes(label = scales::percent(Coverage)), colour = "#0B3041", size = 4) +
        scale_fill_gradient(low = "#E7EEF1", high = "#84C8D8", limits = c(0, 1), guide = "none") +
        scale_y_continuous(breaks = 1, labels = selected_id) +
        scale_x_date(date_labels = "%d %b", date_breaks = "1 day") +
        labs(x = NULL, y = NULL) + plot_theme() + theme(panel.grid = element_blank())
    } else if (identical(type, "day")) {
      p <- ggplot(data, aes(Hour, MEDI, group = Date)) +
        annotate("rect", xmin = 0, xmax = 6, ymin = -Inf, ymax = Inf, fill = "#DDE8EE", alpha = 0.7) +
        annotate("rect", xmin = 20, xmax = 24, ymin = -Inf, ymax = Inf, fill = "#DDE8EE", alpha = 0.7) +
        geom_line(colour = "#0B789C", alpha = 0.5, linewidth = 0.55) +
        scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
        labs(x = "Local clock time", y = "Melanopic EDI (lx)") + plot_theme()
      p <- add_symlog_y(p, input$viz_scale %or% "symlog")
    } else if (identical(type, "timeline")) {
      p <- ggplot(data, aes(Datetime, MEDI))
      if (isTRUE(input$viz_photoperiod %or% TRUE)) {
        p <- p + geom_rect(data = night_rectangles(data), aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), inherit.aes = FALSE, fill = "#DDE8EE", alpha = 0.7)
      }
      p <- p + geom_line(colour = "#0B789C", linewidth = 0.65) + labs(x = NULL, y = "Melanopic EDI (lx)") + plot_theme()
      p <- add_symlog_y(p, input$viz_scale %or% "symlog")
    } else if (identical(type, "heatmap")) {
      p <- ggplot(data, aes(Hour, factor(Date), fill = log10(pmax(MEDI, 0) + 1))) +
        geom_tile() +
        scale_fill_gradientn(colours = c("#E7EEF1", "#8AC9D7", "#E9B878", "#C96A3D"), labels = function(x) scales::number(10^x - 1), name = "MEDI (lx)") +
        scale_x_continuous(breaks = seq(0, 24, 4), expand = expansion(mult = 0)) +
        coord_cartesian(xlim = c(0, 24)) +
        labs(x = "Local clock time", y = NULL) + plot_theme() + theme(panel.grid = element_blank())
    } else if (identical(type, "doubleplot")) {
      dates <- sort(unique(data$Date))
      panels <- lapply(seq_len(max(1, length(dates) - 1)), function(i) {
        first <- data[data$Date == dates[[i]], , drop = FALSE]
        second <- data[data$Date == dates[[min(i + 1, length(dates))]], , drop = FALSE]
        first$DoubleHour <- first$Hour
        second$DoubleHour <- second$Hour + 24
        first$Panel <- format(dates[[i]], "%d %b")
        second$Panel <- format(dates[[i]], "%d %b")
        rbind(first, second)
      })
      double <- do.call(rbind, panels)
      p <- ggplot(double, aes(DoubleHour, factor(Panel, levels = rev(unique(Panel))), fill = log10(pmax(MEDI, 0) + 1))) +
        geom_tile() +
        geom_vline(xintercept = 24, colour = "#FFFFFF", linewidth = 0.8) +
        scale_fill_gradientn(colours = c("#E7EEF1", "#8AC9D7", "#E9B878", "#C96A3D"), labels = function(x) scales::number(10^x - 1), name = "MEDI (lx)") +
        scale_x_continuous(breaks = seq(0, 48, 6), labels = sprintf("%02d", seq(0, 48, 6) %% 24)) +
        labs(x = "48-hour clock", y = "Starting date") + plot_theme() + theme(panel.grid = element_blank())
    } else if (identical(type, "aggregate")) {
      profile <- data |>
        dplyr::group_by(Hour) |>
        dplyr::summarise(
          median = median(MEDI, na.rm = TRUE),
          q25 = quantile(MEDI, 0.25, na.rm = TRUE),
          q75 = quantile(MEDI, 0.75, na.rm = TRUE),
          .groups = "drop"
        )
      p <- ggplot(profile, aes(Hour, median)) +
        geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#85C7D6", alpha = 0.35) +
        geom_line(colour = "#0B789C", linewidth = 0.85) +
        scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
        labs(x = "Local clock time", y = "Melanopic EDI (lx)") + plot_theme()
      p <- add_symlog_y(p, input$viz_scale %or% "symlog")
    } else if (identical(type, "histogram")) {
      p <- ggplot(data, aes(MEDI)) +
        geom_histogram(bins = 55, fill = "#65B5C8", colour = "#FFFFFF", linewidth = 0.25) +
        scale_x_continuous(trans = scales::pseudo_log_trans(base = 10), labels = scales::label_number()) +
        labs(x = "Melanopic EDI (lx, symlog)", y = "Observations") + plot_theme()
    } else {
      p <- ggplot(data, aes(MEDI)) +
        stat_ecdf(geom = "step", colour = "#0B789C", linewidth = 0.9) +
        scale_x_continuous(trans = scales::pseudo_log_trans(base = 10), labels = scales::label_number()) +
        scale_y_continuous(labels = scales::label_percent()) +
        labs(x = "Melanopic EDI (lx, symlog)", y = "Cumulative observations") + plot_theme()
    }
    p
  }, res = 110, alt = "Selected LightLogR-style visualization of the demonstration dataset")

  observeEvent(input$export_plot, showNotification("Plot export is represented here; use the generated R script to reproduce the figure.", type = "message"))
  observeEvent(input$export_data, showNotification("Plotted-data export is a recommended production feature and is not enabled in this prototype.", type = "message"))

  script_state <- reactive({
    defaults <- default_script_state()
    list(
      device = input$import_device %or% defaults$device,
      device_version = input$import_version %or% defaults$device_version,
      timezone = input$prep_tz %or% input$import_tz %or% defaults$timezone,
      dst = input$prep_dst %or% input$import_dst %or% defaults$dst,
      id_method = input$import_id_method %or% defaults$id_method,
      id_value = input$import_id_value %or% defaults$id_value,
      date_start = as.character((input$prep_range %or% c(defaults$date_start, defaults$date_end))[[1]]),
      date_end = as.character((input$prep_range %or% c(defaults$date_start, defaults$date_end))[[2]]),
      value_min = input$prep_min %or% defaults$value_min,
      value_max = input$prep_max %or% defaults$value_max,
      flag_range = input$prep_flag_range %or% defaults$flag_range,
      nonwear = input$prep_nonwear %or% defaults$nonwear,
      aggregate = input$prep_aggregate %or% defaults$aggregate,
      interval = input$prep_interval %or% defaults$interval,
      aggregate_function = input$prep_function %or% defaults$aggregate_function,
      gap_handling = input$prep_gaps %or% defaults$gap_handling,
      remove_partial = input$prep_remove_partial %or% defaults$remove_partial,
      missing_threshold = input$prep_missing %or% defaults$missing_threshold,
      group_dimensions = input$group_dimensions %or% defaults$group_dimensions,
      clock_window = input$group_clock %or% defaults$clock_window,
      clock_start = input$group_clock_start %or% defaults$clock_start,
      clock_end = input$group_clock_end %or% defaults$clock_end,
      latitude = input$group_lat %or% defaults$latitude,
      longitude = input$group_lon %or% defaults$longitude,
      solar_depression = input$group_solar_depression %or% defaults$solar_depression,
      metric = input$metric_selected %or% defaults$metric,
      metric_threshold = input$metric_threshold %or% defaults$metric_threshold,
      metric_direction = input$metric_direction %or% defaults$metric_direction,
      metric_period = input$metric_period %or% defaults$metric_period,
      metric_timespan = input$metric_timespan %or% defaults$metric_timespan,
      metric_loop = input$metric_loop %or% defaults$metric_loop,
      metric_decay = input$metric_decay %or% defaults$metric_decay,
      spectral_min = input$spectral_min %or% defaults$spectral_min,
      spectral_max = input$spectral_max %or% defaults$spectral_max,
      visualization = input$viz_type %or% defaults$visualization,
      visualization_photoperiod = input$viz_photoperiod %or% defaults$visualization_photoperiod,
      include_source = input$recipe_source %or% defaults$include_source,
      include_timezone = input$recipe_timezone %or% defaults$include_timezone,
      include_range = input$recipe_range %or% defaults$include_range,
      include_sampling = input$recipe_sampling %or% defaults$include_sampling,
      include_gaps = input$recipe_gaps %or% defaults$include_gaps,
      include_inclusion = input$recipe_inclusion %or% defaults$include_inclusion,
      include_grouping = input$recipe_grouping %or% defaults$include_grouping,
      include_metric = input$recipe_metric %or% defaults$include_metric,
      include_visualization = input$recipe_visual %or% defaults$include_visualization
    )
  })

  generated_script <- reactive(paste(build_script(script_state()), collapse = "\n"))
  output$script_preview <- renderText(generated_script())

  script_download <- function(filename) {
    downloadHandler(
      filename = function() filename,
      contentType = "text/plain",
      content = function(file) writeLines(build_script(script_state()), file, useBytes = TRUE)
    )
  }
  output$download_script <- script_download("lightlogweb-analysis.R")
  output$download_script_recipe <- script_download("lightlogweb-analysis.R")

  output$metadata_table <- gt::render_gt({
    tibble::tibble(
      Field = c(
        "Dataset", "Device", "Primary variable", "Variable unit", "Time zone",
        "Latitude", "Longitude", "Site", "Participant epoch", "Environment epoch"
      ),
      Value = c(
        "sample.data.environment", "ActLumus", "Melanopic EDI", "lx", "Europe/Berlin",
        "48.52", "9.06", "Tübingen, Germany", "10 seconds", "30 seconds"
      ),
      Purpose = c(
        "Internal reference", "Import and reporting", "Plots and metrics", "Interpretation", "Clock-time analysis",
        "Photoperiod", "Photoperiod", "Labelling", "Regularity checks", "Regularity checks"
      )
    ) |>
      gt::gt() |>
      gt::tab_options(table.font.size = gt::px(13), data_row.padding = gt::px(8))
  })

  output$raw_table <- DT::renderDT({
    preview <- as.data.frame(demo_data[seq_len(500), ])
    DT::datatable(
      preview,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 12, lengthMenu = c(12, 25, 50), scrollX = TRUE),
      class = "stripe hover"
    )
  })
}

shinyApp(ui, server)
