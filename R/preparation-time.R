# Time operations preserve measurements and recording identities. Gap rows are
# explicit missing observations, never interpolated measurements.
preparation_time_types <- function() c("timezone", "dst", "gaps", "aggregate")

preparation_time_arguments <- function(type, args) {
  if (type == "aggregate") return(preparation_aggregation_arguments(args))
  expected <- switch(
    type,
    timezone = c("timezone", "method", "ambiguous"),
    dst = "method",
    gaps = c("epoch", "seconds", "bounds")
  )
  if (
    !is.list(args) ||
      anyDuplicated(names(args)) ||
      !setequal(names(args), expected)
  )
    abort_llw(
      "The time-step arguments are incomplete or unsupported.",
      "preparation"
    )
  args <- args[expected]
  choices <- switch(
    type,
    timezone = list(
      method = c("with", "force"),
      ambiguous = c("error", "earlier", "later")
    ),
    dst = list(method = "fixed_start_offset"),
    gaps = list(
      epoch = c("dominant", "custom"),
      bounds = c("recording", "days")
    )
  )
  for (name in names(choices)) {
    assert_scalar_string(args[[name]], name)
    if (!args[[name]] %in% choices[[name]])
      abort_llw(paste("Choose a supported", name, "."), "preparation")
  }
  if (type == "timezone" && !preparation_valid_timezone(args$timezone))
    abort_llw("Choose a time zone from OlsonNames.", "preparation")
  if (
    type == "gaps" &&
      (!is.numeric(args$seconds) ||
        length(args$seconds) != 1L ||
        !is.finite(args$seconds) ||
        args$seconds < 1 ||
        args$seconds > 86400)
  )
    abort_llw("Use an interval from 1 second to 24 hours.", "preparation")
  args
}

preparation_time_groups <- function(data) {
  columns <- union(dplyr::group_vars(data), preparation_identity_columns(data))
  dplyr::group_by(data, dplyr::across(dplyr::all_of(columns)))
}

preparation_assert_times <- function(data) {
  if (any(!is.finite(as.numeric(data$Datetime))))
    abort_llw(
      "Time operations require valid timestamps. Resolve missing timestamps at import.",
      "preparation"
    )
}

# Difference between the local wall clock interpreted as UTC and its instant.
# Unlike a fixed one-hour adjustment, this supports half-hour DST transitions.
preparation_utc_offset <- function(time) {
  wall <- as.POSIXct(
    format(time, "%Y-%m-%d %H:%M:%OS6", tz = lubridate::tz(time)),
    tz = "UTC",
    format = "%Y-%m-%d %H:%M:%OS"
  )
  round(as.numeric(wall) - as.numeric(time))
}

preparation_time_diagnostics <- function(data) {
  preparation_assert_times(data)
  grouped <- preparation_time_groups(data)
  rows <- dplyr::group_rows(grouped)
  if (!nrow(data))
    return(data.frame(
      Recording = integer(),
      Rows = integer(),
      Offset_changes = integer(),
      Duplicate_instants = integer(),
      Repeated_clocks = integer(),
      First = character(),
      Last = character()
    ))
  dplyr::bind_rows(lapply(seq_along(rows), function(i) {
    time <- sort(data$Datetime[rows[[i]]])
    offsets <- preparation_utc_offset(time)
    data.frame(
      Recording = i,
      Rows = length(time),
      Offset_changes = sum(diff(offsets) != 0),
      Duplicate_instants = sum(duplicated(time)),
      Repeated_clocks = sum(duplicated(format(time, "%Y-%m-%d %H:%M:%OS6"))),
      First = if (length(time)) format(time[[1]], usetz = TRUE) else "",
      Last = if (length(time)) format(utils::tail(time, 1), usetz = TRUE) else
        ""
    )
  }))
}

preparation_change_timezone <- function(data, args) {
  preparation_assert_times(data)
  if (args$method == "with") {
    data$Datetime <- lubridate::with_tz(data$Datetime, args$timezone)
  } else {
    # Skipped local times never acquire invented instants. Ambiguous times need
    # an explicit earlier/later choice; the default rejects both situations.
    policy <- switch(
      args$ambiguous,
      error = "NA",
      earlier = "pre",
      later = "post"
    )
    time <- lubridate::force_tz(
      data$Datetime,
      args$timezone,
      roll_dst = c("NA", policy)
    )
    if (anyNA(time))
      abort_llw(
        "Some clock times are skipped or repeated in the chosen zone. Skipped times cannot be reinterpreted; for repeated times choose the earlier or later occurrence.",
        "preparation"
      )
    data$Datetime <- time
  }
  data
}

preparation_correct_dst <- function(data) {
  preparation_assert_times(data)
  for (rows in dplyr::group_rows(preparation_time_groups(data))) {
    if (!length(rows)) next
    time <- data$Datetime[rows]
    offset <- preparation_utc_offset(time)
    first <- which.min(as.numeric(time))
    data$Datetime[rows] <- time + offset - offset[[first]]
  }
  data
}

# Plan without constructing a sequence. Complete days include the date of a
# final midnight observation, unlike ceiling_date's unchanged-midnight default.
preparation_gap_plan <- function(data, args, budget = 1024^3) {
  preparation_assert_times(data)
  grouped <- preparation_time_groups(data)
  rows <- dplyr::group_rows(grouped)
  plans <- lapply(seq_along(rows), function(i) {
    time <- data$Datetime[rows[[i]]]
    epoch <- if (args$epoch == "custom") args$seconds else
      lightlogr_dominant_epoch_seconds(time)
    n <- length(time)
    if (n < 2L)
      return(data.frame(
        group = i,
        epoch = epoch,
        grid = 0,
        added = 0,
        from = NA_real_
      ))
    if (!is.finite(epoch) || epoch <= 0)
      abort_llw(
        "A recording has no estimable interval. Choose a custom interval.",
        "preparation"
      )
    first <- min(time)
    last <- max(time)
    before <- if (args$bounds == "days")
      floor(
        as.numeric(difftime(
          first,
          lubridate::floor_date(first, "day"),
          units = "secs"
        )) /
          epoch
      ) else 0
    end <- if (args$bounds == "days")
      as.POSIXct(
        paste(as.Date(last, tz = lubridate::tz(time)) + 1, "00:00:00"),
        tz = lubridate::tz(time)
      ) else last
    if (!is.finite(as.numeric(end)) || !is.finite(before))
      abort_llw(
        "A local-day boundary is unavailable in this time zone. Expand within the recording instead.",
        "preparation"
      )
    span <- as.numeric(difftime(end, first, units = "secs")) / epoch
    grid <- before +
      if (args$bounds == "days") ceiling(span) else floor(span) + 1
    position <- (as.numeric(time) - as.numeric(first)) / epoch
    on_grid <- as.numeric(first) + round(position) * epoch == as.numeric(time) &
      position >= -before &
      position <= grid - before - 1
    added <- max(0, grid - length(unique(round(position[on_grid]))))
    data.frame(
      group = i,
      epoch = epoch,
      grid = grid,
      added = added,
      from = as.numeric(first) - before * epoch
    )
  })
  plan <- if (length(plans)) dplyr::bind_rows(plans) else
    data.frame(
      group = integer(),
      epoch = numeric(),
      grid = numeric(),
      added = numeric(),
      from = numeric()
    )
  added <- sum(plan$added)
  total <- nrow(data) + added
  fixed <- as.numeric(utils::object.size(data[0, , drop = FALSE]))
  per_row <- max(
    64,
    (as.numeric(utils::object.size(data)) - fixed) / max(1L, nrow(data))
  )
  memory <- 64 * 1024^2 + 16 * (fixed + total * per_row)
  if (!is.finite(memory) || memory > budget || total > 5000000)
    abort_llw(
      paste0(
        "Gap expansion would produce approximately ",
        format(total, scientific = FALSE, big.mark = ","),
        " rows and require ",
        round(memory / 1024^2),
        " MiB working memory. Use a coarser interval or a shorter recording window."
      ),
      "resource"
    )
  list(
    groups = plan,
    added = added,
    total = total,
    working_bytes = memory,
    singletons = sum(lengths(rows) < 2L)
  )
}

preparation_expand_gaps <- function(data, args, budget = 1024^3) {
  plan <- preparation_gap_plan(data, args, budget)
  groups <- dplyr::group_vars(data)
  grouped <- preparation_time_groups(data)
  rows <- dplyr::group_rows(grouped)
  marker <- utils::tail(make.unique(c(names(data), ".llw_gap_row")), 1L)
  data[[marker]] <- seq_len(nrow(data))
  old_mask <- data[["is.implicit"]]
  if (!is.null(old_mask) && (!is.logical(old_mask) || anyNA(old_mask)))
    abort_llw(
      "The existing is.implicit marker must contain TRUE or FALSE.",
      "preparation"
    )
  pieces <- lapply(seq_along(rows), function(i) {
    part <- preparation_time_groups(data[rows[[i]], , drop = FALSE])
    if (nrow(part) < 2L) {
      part$is.implicit <- FALSE
      return(part)
    }
    if (args$bounds == "days") {
      # Use the preflight grid to include all observed local dates, including
      # a final midnight. A join keeps duplicates and off-grid measurements.
      spec <- plan$groups[i, ]
      grid <- dplyr::group_keys(part)[rep(1L, spec$grid), , drop = FALSE]
      grid$Datetime <- as.POSIXct(
        spec$from + seq.int(0, length.out = spec$grid) * spec$epoch,
        origin = "1970-01-01",
        tz = lubridate::tz(part$Datetime)
      )
      result <- dplyr::full_join(
        part,
        grid,
        by = union(dplyr::group_vars(part), "Datetime")
      )
      return(dplyr::arrange(result, .data$Datetime))
    }
    LightLogR::gap_handler(
      part,
      epoch = lubridate::dseconds(plan$groups$epoch[[i]] %||% args$seconds),
      behavior = "full_sequence",
      full.days = FALSE
    )
  })
  result <- if (length(pieces)) dplyr::bind_rows(pieces) else data
  created <- is.na(result[[marker]])
  result$is.implicit <- created
  if (!is.null(old_mask))
    result$is.implicit[!created] <- old_mask[result[[marker]][!created]]
  result[[marker]] <- NULL
  result <- dplyr::ungroup(result)
  if (is.null(old_mask))
    result <- dplyr::relocate(result, "is.implicit", .after = "Datetime")
  if (length(groups))
    result <- dplyr::group_by(result, dplyr::across(dplyr::all_of(groups)))
  list(data = result, plan = plan)
}
