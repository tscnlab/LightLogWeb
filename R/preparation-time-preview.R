preparation_time_day_view <- function(
  raw,
  prepared,
  source_rows,
  removed_at,
  context,
  selection,
  source_membership = NULL,
  aggregated = FALSE
) {
  zone <- lubridate::tz(prepared$Datetime)
  source_variable <- context$source_variable %||% context$variable
  source_value <- raw[[source_variable]] %||% rep(NA_real_, nrow(raw))
  preview_value <- prepared[[context$variable]] %||%
    rep(NA_real_, nrow(prepared))
  original <- !is.na(source_rows)
  affected <- removed_at != 0L
  if (aggregated) {
    represented <- which(!is.na(source_membership))
    affected[represented] <- TRUE
  } else
    affected[source_rows[original]] <- preparation_values_differ(
      source_value[source_rows[original]],
      preview_value[original]
    ) |
      as.numeric(raw$Datetime[source_rows[original]]) !=
        as.numeric(prepared$Datetime[original])
  source <- data.frame(
    Id = as.character(raw$Id),
    Day = format(raw$Datetime, "%Y-%m-%d", tz = zone),
    Rows = 1L,
    Removed = as.integer(removed_at != 0L),
    Changed = as.integer(affected & removed_at == 0L),
    Affected = as.integer(affected),
    Added = 0L,
    Preview = 0L
  )
  after <- data.frame(
    Id = as.character(prepared$Id),
    Day = format(prepared$Datetime, "%Y-%m-%d", tz = zone),
    Rows = rep(0L, nrow(prepared)),
    Removed = rep(0L, nrow(prepared)),
    Changed = rep(0L, nrow(prepared)),
    Affected = rep(0L, nrow(prepared)),
    Added = if (aggregated) rep(0L, nrow(prepared)) else as.integer(!original),
    Preview = rep(1L, nrow(prepared))
  )
  catalog <- stats::aggregate(
    cbind(Rows, Removed, Changed, Affected, Added, Preview) ~ Id + Day,
    rbind(source, after),
    sum
  )
  catalog$Fraction <- (catalog$Affected + catalog$Added) /
    pmax(1, catalog$Rows + catalog$Added)
  candidates <- which(catalog$Fraction > 0 & catalog$Fraction < 1)
  if (!length(candidates)) candidates <- which(catalog$Fraction > 0)
  if (!length(candidates)) candidates <- seq_len(nrow(catalog))
  chosen <- candidates[order(
    abs(catalog$Fraction[candidates] - 0.51),
    -catalog$Rows[candidates]
  )][[1L]]
  # Split panels use their own local dates. A date observed only in the source
  # zone must remain selectable even when every preview instant lies yesterday.
  # These additional choices carry no overlay counts, so totals and automatic
  # day selection retain their preview-zone meaning.
  source_dates <- unique(data.frame(
    Id = as.character(raw$Id),
    Day = format(raw$Datetime, "%Y-%m-%d", tz = lubridate::tz(raw$Datetime))
  ))
  extra_dates <- dplyr::anti_join(
    source_dates,
    catalog[c("Id", "Day")],
    by = c("Id", "Day")
  )
  if (nrow(extra_dates)) {
    for (name in setdiff(names(catalog), c("Id", "Day")))
      extra_dates[[name]] <- 0
    catalog <- rbind(catalog, extra_dates[names(catalog)])
  }
  if (!is.null(selection$id) && selection$id %in% catalog$Id) {
    available <- which(catalog$Id == selection$id)
    requested <- available[catalog$Day[available] == selection$day]
    chosen <- if (length(requested)) requested[[1L]] else available[[1L]]
  }
  selected <- catalog[chosen, ]
  frame <- function(data, values, stage, frame_zone = zone) {
    keep <- as.character(data$Id) == selected$Id &
      format(data$Datetime, "%Y-%m-%d", tz = frame_zone) == selected$Day
    data <- data[keep, , drop = FALSE]
    values <- values[keep]
    group <- dplyr::group_indices(preparation_time_groups(data))
    order <- order(group, data$Datetime)
    times <- data$Datetime[order]
    values <- values[order]
    group <- group[order]
    segment <- integer(length(times))
    for (g in unique(group)) {
      at <- which(group == g)
      delta <- diff(as.numeric(times[at]))
      epoch <- lightlogr_dominant_epoch_seconds(times[at])
      if (!is.finite(epoch)) epoch <- Inf
      missing <- !is.finite(values[at])
      segment[at] <- cumsum(
        c(TRUE, delta > 1.5 * epoch) |
          missing |
          c(FALSE, utils::head(missing, -1L))
      )
    }
    result <- data.frame(
      Datetime = times,
      Value = if (stage == "source") values else rep(NA_real_, length(values)),
      Preview_value = if (stage == "preview") values else
        rep(NA_real_, length(values)),
      Kept = rep(stage == "preview", length(values)),
      Source = rep(stage == "source", length(values)),
      Recording = group,
      Source_segment = segment,
      Preview_segment = segment,
      Implicit = if (stage == "preview" && "is.implicit" %in% names(data))
        data$is.implicit[order] else rep(FALSE, length(values))
    )
    if (nrow(result) > 2000L)
      result <- result[
        unique(round(seq(1, nrow(result), length.out = 2000))),
        ,
        drop = FALSE
      ]
    result
  }
  list(
    plot = rbind(
      frame(raw, source_value, "source"),
      frame(prepared, preview_value, "preview")
    ),
    gaps = preparation_gap_marks(
      raw,
      prepared,
      removed_at != 0L,
      context$variable,
      selected$Id,
      selected$Day,
      zone
    ),
    split_source = frame(
      raw,
      source_value,
      "source",
      lubridate::tz(raw$Datetime)
    ),
    source_timezone = lubridate::tz(raw$Datetime),
    source_gaps = preparation_gap_marks(
      raw,
      raw,
      rep(FALSE, nrow(raw)),
      source_variable,
      selected$Id,
      selected$Day,
      lubridate::tz(raw$Datetime)
    ),
    days = catalog,
    id = selected$Id,
    day = selected$Day,
    step = if (is.null(selection$step) || identical(selection$step, "all"))
      Inf else as.numeric(selection$step),
    variable = context$variable,
    variable_label = context$variable_label,
    timezone = zone,
    transformation_note = paste(
      context$transformation_note %||% "",
      if (aggregated)
        "Teal shows aggregates at bin starts. Combining readings is not a filtering gap." else
        "",
      "Source time zone:",
      lubridate::tz(raw$Datetime),
      "\u00b7 Preview time zone:",
      zone,
      if (!identical(lubridate::tz(raw$Datetime), zone))
        paste0(
          ". Overlay uses ",
          zone,
          " for both curves. Split view shows the selected date in each stage's own time zone; the date menu includes dates from both zones."
        )
    ),
    caption = if (aggregated)
      paste(
        if (is.null(selection$id)) "Automatic informative day." else
          "Chosen participant and day.",
        selected$Rows - selected$Removed,
        "source rows represented;",
        selected$Removed,
        "source rows excluded by filtering;",
        selected$Preview,
        "preview rows on this day.",
        "Teal shows aggregates at bin starts. At most 2,000 evenly spaced rows per stage are drawn; lines break at gaps and missing values.",
        preparation_gap_caption()
      ) else
      paste(
        if (is.null(selection$id)) "Automatic informative day." else
          "Chosen participant and day.",
        selected$Removed,
        "source rows removed;",
        selected$Changed,
        "source rows changed;",
        selected$Added,
        "explicit gap rows added on this day. Grey source and teal preview use their actual instants on the preview-zone axis.",
        "At most 2,000 evenly spaced rows per stage are drawn; lines break at gaps and missing values.",
        preparation_gap_caption()
      ),
    participants = stats::aggregate(
      cbind(Rows, Removed, Changed) ~ Id,
      catalog,
      sum
    )
  )
}
