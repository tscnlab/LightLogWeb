# These are observed differences from the source, not an inferred regular grid.
# LightLogR's implicit/explicit distinction concerns absent timestamps versus
# present rows with NA values. is.implicit on an expanded row records its origin;
# once that row exists, its NA is an explicit gap in the preview.
preparation_gap_marks <- function(
  raw,
  prepared,
  removed,
  variable,
  id,
  day,
  timezone
) {
  columns <- union(preparation_identity_columns(raw), dplyr::group_vars(raw))
  columns <- union(columns, "Datetime")
  on_day <- function(data) {
    as.character(data$Id) == id &
      format(data$Datetime, "%Y-%m-%d", tz = timezone) == day
  }
  before <- as.data.frame(raw[removed & on_day(raw), columns, drop = FALSE])
  after <- prepared %||% raw
  present <- if (is.null(prepared)) !removed else rep(TRUE, nrow(after))
  rows <- which(present & on_day(after))
  # Match within the same recording and current group. A retained duplicate or
  # an explicit replacement row means this timestamp is still present.
  absent <- dplyr::anti_join(
    before,
    as.data.frame(after[rows, columns, drop = FALSE]),
    by = columns
  )
  missing <- if (is.null(variable)) integer() else
    rows[is.na(after[[variable]][rows])]
  unique(data.frame(
    Datetime = as.POSIXct(
      c(as.numeric(absent$Datetime), as.numeric(after$Datetime[missing])),
      origin = "1970-01-01",
      tz = timezone
    ),
    Kind = c(rep("implicit", nrow(absent)), rep("explicit", length(missing)))
  ))
}

preparation_gap_caption <- function() {
  paste(
    "Bottom marks: amber = source timestamps absent from the preview (implicit gaps);",
    "blue = preview readings that are NA (explicit gaps), including rows added by Gaps.",
    "Gap marks use every observation, even when lines are sampled."
  )
}

# Row lineage records the first step that removed each source observation.
# It keeps duplicate timestamps distinct without changing stored source columns.
preparation_day_view <- function(
  raw,
  removed_at,
  context,
  selection = list(),
  prepared = NULL,
  source_rows = NULL
) {
  stopifnot(length(removed_at) == nrow(raw))
  if (
    nrow(raw) &&
      !is.null(prepared) &&
      (anyNA(source_rows) ||
        !identical(
          lubridate::tz(raw$Datetime),
          lubridate::tz(prepared$Datetime)
        ) ||
        !identical(
          as.numeric(raw$Datetime[source_rows]),
          as.numeric(prepared$Datetime)
        ))
  )
    return(preparation_time_day_view(
      raw,
      prepared,
      source_rows,
      removed_at,
      context,
      selection
    ))
  through <- selection$step %||% Inf
  if (identical(through, "all")) through <- Inf
  through <- as.numeric(through)
  kept <- removed_at == 0L | removed_at > through
  value <- if (is.null(context$variable)) rep(NA_real_, nrow(raw)) else
    raw[[context$source_variable %||% context$variable]]
  preview_value <- value
  if (!is.null(prepared) && !is.null(context$variable)) {
    preview_value <- rep(NA_real_, nrow(raw))
    original <- !is.na(source_rows)
    preview_value[source_rows[original]] <- prepared[[context$variable]][
      original
    ]
  }
  changed <- kept & preparation_values_differ(value, preview_value)
  day <- format(raw$Datetime, "%Y-%m-%d", tz = context$timezone)
  id <- as.character(raw$Id)
  .data <- NULL
  catalog <- data.frame(
    Id = id,
    Day = day,
    Removed = !kept,
    Changed = changed,
    Affected = !kept | changed,
    Finite = is.finite(value)
  ) |>
    dplyr::group_by(.data$Id, .data$Day) |>
    dplyr::summarise(
      Rows = dplyr::n(),
      Removed = sum(.data$Removed),
      Changed = sum(.data$Changed),
      Affected = sum(.data$Affected),
      Finite = sum(.data$Finite),
      .groups = "drop"
    )
  if (!nrow(catalog))
    return(list(
      plot = data.frame(),
      days = catalog,
      id = NULL,
      day = NULL,
      step = through,
      caption = "The source contains no observations."
    ))
  catalog$Fraction <- catalog$Affected / catalog$Rows
  candidates <- which(
    catalog$Finite > 0L &
      catalog$Affected > 0L &
      catalog$Affected < catalog$Rows
  )
  if (!length(candidates))
    candidates <- which(catalog$Finite > 0L & catalog$Affected > 0L)
  if (!length(candidates)) candidates <- which(catalog$Finite > 0L)
  if (!length(candidates)) candidates <- seq_len(nrow(catalog))
  chosen <- candidates[order(
    abs(catalog$Fraction[candidates] - 0.51),
    -catalog$Rows[candidates],
    catalog$Id[candidates],
    catalog$Day[candidates]
  )][[1L]]
  if (!is.null(selection$id) && selection$id %in% catalog$Id) {
    available <- which(catalog$Id == selection$id)
    requested <- available[catalog$Day[available] == selection$day]
    chosen <- if (length(requested)) requested[[1L]] else
      available[which.min(abs(catalog$Fraction[available] - 0.51))]
  }
  selected <- catalog[chosen, ]
  index <- which(id == selected$Id & day == selected$Day)
  keys <- as.data.frame(raw[index, context$identity_columns, drop = FALSE])
  groups <- dplyr::group_indices(dplyr::group_by(
    keys,
    dplyr::across(dplyr::everything())
  ))
  ordered <- order(groups, raw$Datetime[index], index)
  index <- index[ordered]
  groups <- groups[ordered]
  plot <- data.frame(
    Datetime = raw$Datetime[index],
    Value = value[index],
    Preview_value = preview_value[index],
    Kept = kept[index],
    Recording = groups
  )
  plot$Source_segment <- integer(nrow(plot))
  plot$Preview_segment <- integer(nrow(plot))
  for (group in unique(groups)) {
    at <- which(groups == group)
    differences <- diff(as.numeric(plot$Datetime[at]))
    positive <- differences[differences > 0]
    epoch <- if (length(positive)) stats::median(positive) else Inf
    missing <- !is.finite(plot$Value[at])
    gap <- c(TRUE, differences > 1.5 * epoch) |
      missing |
      c(FALSE, utils::head(missing, -1L))
    plot$Source_segment[at] <- cumsum(gap)
    plot$Preview_segment[at] <- cumsum(
      gap |
        !plot$Kept[at] |
        c(FALSE, !utils::head(plot$Kept[at], -1L)) |
        !is.finite(plot$Preview_value[at]) |
        c(FALSE, !is.finite(utils::head(plot$Preview_value[at], -1L)))
    )
  }
  total <- nrow(plot)
  if (total > 2000L)
    plot <- plot[
      unique(round(seq(1, total, length.out = 2000L))),
      ,
      drop = FALSE
    ]
  reason <- if (!is.null(selection$id)) "Chosen participant and day." else if (
    selected$Affected > 0L && selected$Affected < selected$Rows
  )
    "Automatic: a day closest to 51% of source rows affected." else if (
    selected$Affected == selected$Rows
  )
    "Automatic: a fully affected day; no partly changed day is available." else
    "No day changes under this comparison; showing an observed source day."
  list(
    plot = plot,
    gaps = preparation_gap_marks(
      raw,
      prepared,
      !kept,
      context$variable,
      selected$Id,
      selected$Day,
      context$timezone
    ),
    source_gaps = preparation_gap_marks(
      raw,
      raw,
      rep(FALSE, nrow(raw)),
      context$source_variable %||% context$variable,
      selected$Id,
      selected$Day,
      context$timezone
    ),
    source_timezone = context$timezone,
    days = as.data.frame(catalog),
    id = selected$Id,
    day = selected$Day,
    step = through,
    variable = context$variable,
    variable_label = context$variable_label,
    transformation_note = context$transformation_note,
    caption = paste(
      reason,
      sprintf(
        "%s of %s source rows removed; %s retained %s values changed on this day (%.1f%% of rows affected).",
        selected$Removed,
        selected$Rows,
        selected$Changed,
        context$variable,
        100 * selected$Fraction
      ),
      if (total > 2000L) "Plot uses 2,000 evenly spaced source rows." else
        "All source rows for this day are shown.",
      "Grey: source. Teal: preview. Lines break at removed observations, missing values and recording gaps.",
      preparation_gap_caption(),
      context$transformation_note %||% ""
    ),
    participants = stats::aggregate(
      cbind(Rows, Removed, Changed) ~ Id,
      data = catalog,
      FUN = sum
    )
  )
}

preparation_recipe_status <- function(recipe, applied, index) {
  if (!recipe$steps[[index]]$enabled) return("Disabled")
  same_prefix <- length(applied$steps) >= index &&
    identical(recipe$steps[seq_len(index)], applied$steps[seq_len(index)])
  if (same_prefix) "Applied" else "Preview"
}

preparation_pending_removals <- function(recipe, applied) {
  ids <- vapply(recipe$steps, `[[`, character(1), "id")
  which(vapply(
    applied$steps,
    function(step) {
      isTRUE(step$enabled) && !step$id %in% ids
    },
    logical(1)
  ))
}

# The parent can pass dashboard$plot_settings directly. The standalone module
# uses the same dataset preferences and defaults until it is wired to a dashboard.
preparation_axis_settings <- function(record = NULL, settings = NULL) {
  settings <- settings %||% list()
  if (!is.list(settings))
    abort_llw("Invalid dashboard plot settings.", "validation")
  preferences <- record$analysis_settings %||% list()
  scale <- dashboard_plot_scale_value(
    settings$scale %||% preferences$display_scale
  )
  threshold <- dashboard_symlog_threshold_value(
    settings$symlog_threshold %||% preferences$symlog_threshold
  )
  limits <- settings$y_limits
  if (is.list(limits)) {
    if (!is.null(limits$message)) abort_llw(limits$message, "validation")
    limits <- limits$limits
  }
  if (is.null(limits)) limits <- c(NA_real_, NA_real_)
  if (length(limits) != 2L) abort_llw("Use two y-axis bounds.", "validation")
  checked <- dashboard_y_axis_limits(limits[[1]], limits[[2]], scale)
  if (!is.null(checked$message)) abort_llw(checked$message, "validation")
  list(
    scale = scale,
    symlog_threshold = threshold,
    y_limits = if (checked$active) checked$limits else NULL,
    y_scope = if (identical(settings$y_scope, "page")) "page" else "shared",
    domain = settings$domain,
    positive_domain = settings$positive_domain
  )
}

preparation_axis_label <- function(settings) {
  paste0(
    dashboard_plot_scale_label(settings$scale),
    if (settings$scale == "symlog")
      paste0(", threshold ", settings$symlog_threshold),
    if (settings$scale == "log") " (positive values only)",
    if (!is.null(settings$y_limits)) " \u00b7 dashboard y range"
  )
}

preparation_plot_domain <- function(..., positive = FALSE) {
  bounds <- unlist(
    lapply(list(...), function(values) {
      values <- values[is.finite(values)]
      if (positive) values <- values[values > 0]
      if (length(values)) range(values) else NULL
    }),
    use.names = FALSE
  )
  if (length(bounds)) range(bounds) else NULL
}

preparation_preview_plot <- function(
  view,
  context,
  settings = preparation_axis_settings()
) {
  data <- view$plot
  context$timezone <- view$timezone %||% context$timezone
  if (settings$scale == "log") {
    data$Value[data$Value <= 0 & !is.na(data$Value)] <- NA_real_
    data$Preview_value[
      data$Preview_value <= 0 & !is.na(data$Preview_value)
    ] <- NA_real_
  }
  values <- c(data$Value, data$Preview_value)
  domain <- if (settings$y_scope == "shared") {
    # A dashboard's automatic source range must not censor a transformed result.
    # Worker-computed bounds cover the complete comparison, keeping shared
    # limits stable across days and across both split panels.
    if (settings$scale == "log")
      preparation_plot_domain(
        settings$positive_domain,
        view$positive_domain,
        values,
        positive = TRUE
      ) else preparation_plot_domain(settings$domain, view$domain, values)
  } else NULL
  manual <- settings$y_limits
  source_data <- if ("Source" %in% names(data))
    data[data$Source, , drop = FALSE] else data
  .data <- NULL
  plot <- ggplot2::ggplot(
    data,
    ggplot2::aes(x = .data$Datetime, y = .data$Value)
  ) +
    ggplot2::geom_line(
      data = source_data,
      ggplot2::aes(
        group = interaction(.data$Recording, .data$Source_segment)
      ),
      colour = "#B8C8CD",
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = source_data,
      colour = "#B8C8CD",
      size = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      data = data[data$Kept, , drop = FALSE],
      ggplot2::aes(
        y = .data$Preview_value,
        group = interaction(.data$Recording, .data$Preview_segment)
      ),
      colour = "#087D99",
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      data = data[data$Kept, , drop = FALSE],
      ggplot2::aes(y = .data$Preview_value),
      colour = "#087D99",
      size = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::scale_x_datetime(
      date_labels = "%H:%M",
      timezone = context$timezone,
      limits = as.POSIXct(
        paste(
          c(view$day, as.character(as.Date(view$day) + 1)),
          "00:00:00"
        ),
        tz = context$timezone
      )
    ) +
    ggplot2::labs(
      x = paste(view$id, view$day, context$timezone, sep = "\n"),
      y = paste(
        preparation_plot_label(view$variable_label %||% context$variable_label),
        "\u00b7",
        settings$scale
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(8, 8, 8, 8)
    )
  if (!is.null(view$gaps) && nrow(view$gaps)) {
    for (kind in c("implicit", "explicit")) {
      plot <- plot +
        ggplot2::geom_rug(
          data = view$gaps[view$gaps$Kind == kind, , drop = FALSE],
          ggplot2::aes(x = .data$Datetime, colour = .data$Kind),
          inherit.aes = FALSE,
          sides = "b",
          # Different lengths keep both colours visible when recordings overlap.
          length = ggplot2::unit(if (kind == "implicit") 0.04 else 0.02, "npc"),
          linewidth = 0.8,
          alpha = 0.8,
          show.legend = TRUE
        )
    }
    plot <- plot +
      ggplot2::scale_colour_manual(
        name = NULL,
        values = c(implicit = "#A95F13", explicit = "#3675C5"),
        limits = intersect(c("implicit", "explicit"), view$gaps$Kind),
        labels = c(implicit = "Absent timestamps", explicit = "NA readings")
      ) +
      ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.text = ggplot2::element_text(size = 9),
        legend.key.height = ggplot2::unit(8, "pt"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        legend.box.spacing = ggplot2::unit(1, "pt")
      )
  }
  if (settings$scale == "symlog") {
    axis <- dashboard_symlog_axis(
      c(domain %||% values, manual[is.finite(manual)]),
      threshold = settings$symlog_threshold
    )
    plot <- plot +
      ggplot2::scale_y_continuous(
        transform = LightLogR::symlog_trans(
          base = axis$base,
          thr = settings$symlog_threshold,
          scale = settings$symlog_threshold
        ),
        limits = axis$limits,
        breaks = axis$breaks,
        labels = axis$labels,
        minor_breaks = axis$minor_breaks,
        guide = ggplot2::guide_axis(minor.ticks = TRUE),
        expand = ggplot2::expansion(mult = c(0.03, 0.06))
      )
  } else {
    plot <- plot +
      ggplot2::scale_y_continuous(
        transform = if (settings$scale == "log") "log10" else "identity",
        labels = dashboard_axis_number,
        limits = if (is.null(manual)) domain else NULL,
        minor_breaks = NULL
      )
  }
  if (!is.null(manual)) plot <- plot + ggplot2::coord_cartesian(ylim = manual)
  attr(plot, "llw_dashboard_scale") <- settings$scale
  attr(plot, "llw_symlog_threshold") <- settings$symlog_threshold
  plot
}

# Rotated axis labels need short lines even when metadata has a long name.
# The complete metadata label remains in the controls and comparison note.
preparation_plot_label <- function(label, width = 42L) {
  paste(strwrap(label, width = width), collapse = "\n")
}

preparation_split_view <- function(view, stage) {
  if (stage == "source") {
    view$plot <- view$split_source %||% view$plot
    if ("Source" %in% names(view$plot))
      view$plot <- view$plot[view$plot$Source, , drop = FALSE]
    view$plot$Kept <- rep(FALSE, nrow(view$plot))
    view$plot$Preview_value <- rep(NA_real_, nrow(view$plot))
    view$timezone <- view$source_timezone %||% view$timezone
    view$gaps <- view$source_gaps
    view$panel_title <- view$before_label %||% "Source"
  } else {
    view$plot <- view$plot[view$plot$Kept, , drop = FALSE]
    view$plot$Value <- rep(NA_real_, nrow(view$plot))
    view$plot$Source <- rep(FALSE, nrow(view$plot))
    view$panel_title <- view$after_label %||% "Preview"
  }
  view$empty_note <- if (!nrow(view$plot))
    paste(
      "No observations in",
      tolower(view$panel_title),
      "on",
      view$day,
      "in",
      view$timezone,
      ". Choose another date to inspect this stage."
    ) else NULL
  view
}

preparation_plot_theme <- function(color_mode = "light") {
  lightlogweb_plot_theme(color_mode) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 9),
      legend.key.height = ggplot2::unit(8, "pt"),
      legend.box.spacing = ggplot2::unit(1, "pt")
    )
}
