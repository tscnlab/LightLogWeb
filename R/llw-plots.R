# Visualization registry and plot construction ------------------------------

llw_plot_types <- function() {
  tibble::tribble(
    ~id, ~name, ~description,
    "availability", "Availability", "Inspect observation coverage and gaps by stream.",
    "day", "Day overlay", "Compare dates on a 24-hour clock.",
    "timeline", "Timeline", "Follow exposure over the study period.",
    "gap", "Gap review", "Locate implicit gaps and irregular timestamps.",
    "heatmap", "Heatmap", "Scan recurring date-by-time patterns.",
    "doubleplot", "Double plot", "Inspect continuity across midnight.",
    "daily_profile", "Daily profile", "Median and interquartile exposure by clock time.",
    "histogram", "Histogram", "Inspect zeros, range, and outliers.",
    "cdf", "Cumulative distribution", "Compare the exposure distribution.",
    "photoperiod", "Photoperiod", "Review exposure against astronomical day and twilight.",
    "state", "Annotated states", "Overlay sleep, wear, protocol, or event states."
  )
}

llw_plot_subset <- function(data, id) {
  if (is.null(id) || !length(id) || identical(id, "All")) return(data)
  keep <- as.character(data$Id) %in% id
  if (!any(keep)) llw_abort(paste0("No rows match selected Id: ", paste(id, collapse = ", "), "."))
  data[keep, , drop = FALSE]
}

llw_empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = message, colour = "#5E7480", size = 4.2) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void(base_size = 12)
}

llw_log_breaks <- function(limits) {
  limits <- limits[is.finite(limits)]
  if (!length(limits)) return(0)
  largest <- max(abs(limits))
  if (!is.finite(largest) || largest < 1) return(0)
  powers <- 10^seq.int(0, ceiling(log10(largest)))
  c(if (min(limits) < 0) -rev(powers), 0, if (max(limits) > 0) powers)
}

llw_axis_labels <- function() {
  scales::label_number(scale_cut = scales::cut_short_scale(), trim = TRUE)
}

llw_with_attached_lightlogr <- function(expr) {
  was_attached <- "package:LightLogR" %in% search()
  if (!was_attached) {
    attachNamespace("LightLogR")
    on.exit(detach("package:LightLogR", character.only = TRUE), add = TRUE)
  }
  force(expr)
}

llw_lightlog_plot <- function(type, data, variable, scale, units, coordinates, solar_depression, state) {
  variable_symbol <- rlang::sym(variable)
  label <- paste0(variable, if (!is.null(units) && !is.na(units) && nzchar(units)) paste0(" (", units, ")") else "")
  plot_scale <- if (identical(scale, "symlog")) LightLogR::symlog_trans() else scales::identity_trans()
  plot <- llw_with_attached_lightlogr(switch(
    type,
    availability = LightLogR::gg_overview(data),
    day = rlang::inject(LightLogR::gg_day(data, y.axis = !!variable_symbol, y.scale = plot_scale, y.axis.label = label)),
    timeline = rlang::inject(LightLogR::gg_days(data, y.axis = !!variable_symbol, y.scale = plot_scale, y.axis.label = label)),
    gap = llw_quiet_value(rlang::inject(LightLogR::gg_gaps(data, Variable.colname = !!variable_symbol, show.irregulars = TRUE))),
    heatmap = rlang::inject(LightLogR::gg_heatmap(data, Variable.colname = !!variable_symbol, fill.title = label, fill.scale = plot_scale)),
    doubleplot = LightLogR::gg_doubleplot(data),
    photoperiod = rlang::inject(LightLogR::gg_days(data, y.axis = !!variable_symbol, y.scale = plot_scale, y.axis.label = label)),
    state = {
      if (is.null(state) || !state %in% names(data)) llw_abort("State visualization requires a valid annotation column.")
      base <- rlang::inject(LightLogR::gg_days(data, y.axis = !!variable_symbol, y.scale = plot_scale, y.axis.label = label))
      rlang::inject(LightLogR::gg_states(base, !!rlang::sym(state), aes_fill = !!rlang::sym(state)))
    }
  ))
  if (type == "photoperiod" && is.null(coordinates)) {
    llw_abort("Photoperiod visualization requires latitude and longitude metadata or `coordinates`.")
  }
  if (type %in% c("day", "timeline", "doubleplot", "photoperiod") && !is.null(coordinates)) {
    plot <- LightLogR::gg_photoperiod(plot, coordinates, solarDep = solar_depression)
  }
  if (!inherits(plot, "ggplot")) {
    plot <- llw_empty_plot(if (type == "gap") "No gaps or irregular timestamps were detected." else "No plottable observations are available for this view.")
  }
  plot
}

llw_custom_plot <- function(type, data, variable, scale, units, timezone) {
  label <- paste0(variable, if (!is.null(units) && !is.na(units) && nzchar(units)) paste0(" (", units, ")") else "")
  if (type == "daily_profile") {
    data$.llw_hour <- as.numeric(format(data$Datetime, "%H", tz = timezone)) +
      as.numeric(format(data$Datetime, "%M", tz = timezone)) / 60
    profile <- data |>
      dplyr::group_by(.data$.llw_hour) |>
      dplyr::summarise(
        median = stats::median(.data[[variable]], na.rm = TRUE),
        q25 = stats::quantile(.data[[variable]], 0.25, na.rm = TRUE),
        q75 = stats::quantile(.data[[variable]], 0.75, na.rm = TRUE),
        .groups = "drop"
      )
    plot <- ggplot2::ggplot(profile, ggplot2::aes(x = .data$.llw_hour, y = .data$median)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$q25, ymax = .data$q75), fill = "#85C7D6", alpha = 0.4) +
      ggplot2::geom_line(colour = "#0B789C", linewidth = 0.8) +
      ggplot2::scale_x_continuous(breaks = seq(0, 24, 4), limits = c(0, 24)) +
      ggplot2::labs(x = "Local clock time", y = label)
    if (scale == "symlog") {
      plot <- plot + ggplot2::scale_y_continuous(
        trans = LightLogR::symlog_trans(),
        breaks = llw_log_breaks(c(profile$median, profile$q25, profile$q75)),
        labels = llw_axis_labels(),
        minor_breaks = NULL
      )
    } else {
      plot <- plot + ggplot2::scale_y_continuous(labels = llw_axis_labels())
    }
    return(list(plot = plot + ggplot2::theme_minimal(base_size = 12), data = profile))
  }
  if (type == "histogram") {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]])) +
      ggplot2::geom_histogram(bins = 55, fill = "#65B5C8", colour = "white", linewidth = 0.25) +
      ggplot2::labs(x = label, y = "Observations")
    if (scale == "symlog") {
      plot <- plot + ggplot2::scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        breaks = llw_log_breaks(data[[variable]]),
        labels = llw_axis_labels(),
        minor_breaks = NULL
      )
    } else {
      plot <- plot + ggplot2::scale_x_continuous(labels = llw_axis_labels())
    }
    return(list(plot = plot + ggplot2::theme_minimal(base_size = 12), data = data))
  }
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[variable]])) +
    ggplot2::stat_ecdf(geom = "step", colour = "#0B789C", linewidth = 0.9) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::labs(x = label, y = "Cumulative observations")
  if (scale == "symlog") {
    plot <- plot + ggplot2::scale_x_continuous(
      trans = scales::pseudo_log_trans(base = 10),
      breaks = llw_log_breaks(data[[variable]]),
      labels = llw_axis_labels(),
      minor_breaks = NULL
    )
  } else {
    plot <- plot + ggplot2::scale_x_continuous(labels = llw_axis_labels())
  }
  list(plot = plot + ggplot2::theme_minimal(base_size = 12), data = data)
}

#' Build a LightLogWeb visualization
#'
#' @param x An `llw_dataset` or compatible data frame.
#' @param type Visualization ID. See the application gallery.
#' @param variable Primary measurement variable.
#' @param id Optional participant or stream IDs.
#' @param scale Either `"symlog"` or `"identity"`.
#' @param coordinates Optional latitude and longitude for photoperiod shading.
#' @param solar_depression Solar depression angle.
#' @param state Annotation column for a state plot.
#'
#' @return An `llw_result` of type `plot`, including the plotted data.
#' @export
llw_plot <- function(x,
                     type = "timeline",
                     variable = NULL,
                     id = NULL,
                     scale = c("symlog", "identity"),
                     coordinates = NULL,
                     solar_depression = 6,
                     state = NULL) {
  if (!inherits(x, "llw_dataset")) x <- llw_dataset(x, metadata = list(variable = variable))
  variable <- variable %||% llw_primary_variable(x)
  scale <- match.arg(scale)
  known <- llw_plot_types()
  if (!type %in% known$id) llw_abort(paste0("Unknown plot type `", type, "`."))
  data <- llw_plot_subset(x$prepared_data, id)
  units <- x$metadata$variable_unit
  coordinates <- coordinates %||% x$metadata$coordinates

  if (type %in% c("daily_profile", "histogram", "cdf")) {
    built <- llw_custom_plot(type, data, variable, scale, units, x$metadata$timezone)
    plot <- built$plot
    plotted_data <- built$data
  } else {
    plot <- llw_lightlog_plot(type, data, variable, scale, units, coordinates, solar_depression, state)
    plotted_data <- data
  }
  name <- known$name[match(type, known$id)]
  llw_result(
    "plot",
    plot,
    label = name,
    units = units,
    parameters = list(type = type, variable = variable, id = id, scale = scale, coordinates = coordinates, solar_depression = solar_depression, state = state),
    grouping = x$groups,
    source = if (type %in% c("daily_profile", "histogram", "cdf")) "LightLogWeb" else "LightLogR",
    data = plotted_data,
    provenance = list(description = known$description[match(type, known$id)])
  )
}
