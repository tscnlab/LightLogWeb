# Rolling variability is calculated in elapsed time, separately within each
# current group and retained participant/file identity. Sampling is estimated
# from the immutable source, so a preceding filter cannot redefine a gap.
preparation_variability_reference <- function(raw) {
  columns <- union(dplyr::group_vars(raw), preparation_identity_columns(raw))
  grouped <- dplyr::group_by(raw, dplyr::across(dplyr::all_of(columns)))
  keys <- dplyr::group_keys(grouped)
  keys$.llw_variability_epoch <- vapply(
    dplyr::group_rows(grouped),
    function(rows) lightlogr_dominant_epoch_seconds(raw$Datetime[rows]),
    numeric(1)
  )
  list(columns = columns, keys = keys)
}

preparation_variability_arguments <- function(args) {
  assert_scalar_string(args$source, "variability source variable")
  for (field in c("length", "threshold", "coverage")) {
    x <- args[[field]]
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x))
      abort_llw(paste("Enter a finite number for", field, "."), "preparation")
  }
  choices <- list(
    unit = c("seconds", "minutes", "hours"),
    alignment = c("centered", "trailing"),
    statistic = c("sd", "cv"),
    direction = c("below", "above")
  )
  if ("unevaluable" %in% names(args))
    choices$unevaluable <- c("keep", "missing")
  for (field in names(choices)) {
    value <- args[[field]]
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !value %in% choices[[field]]
    )
      abort_llw(
        paste("Choose a supported variability", field, "."),
        "preparation"
      )
  }
  if (
    args$length <= 0 ||
      args$length * switch(args$unit, seconds = 1, minutes = 60, hours = 3600) >
        366 * 86400
  )
    abort_llw(
      "Use a positive window length of at most 366 days.",
      "preparation"
    )
  if (args$threshold < 0)
    abort_llw(
      "The variability threshold must be zero or greater.",
      "preparation"
    )
  if (args$coverage < 0 || args$coverage > 100)
    abort_llw(
      "Minimum window coverage must be between 0 and 100%.",
      "preparation"
    )
  args
}

# Each observation accounts for at most one native epoch, centred on its
# timestamp. Neighbour midpoints cap overlapping epochs; missing values and
# implicit gaps contribute no covered time. No values are filled or interpolated.
preparation_window_coverage <- function(time, finite, epoch, lower, upper) {
  n <- length(time)
  midpoint <- if (n > 1L) time[-n] + diff(time) / 2 else numeric()
  left <- pmax(time - epoch / 2, c(-Inf, midpoint))
  right <- pmin(time + epoch / 2, c(midpoint, Inf))
  left <- left[finite]
  right <- right[finite]
  if (!length(left)) return(rep(0, length(lower)))
  cumulative <- c(0, cumsum(right - left))
  area <- function(bound) {
    index <- findInterval(bound, left)
    active <- pmax(1L, index)
    cumulative[index + 1L] -
      ifelse(index > 0L, pmax(0, right[active] - bound), 0)
  }
  pmin(100, pmax(0, 100 * (area(upper) - area(lower)) / (upper - lower)))
}

preparation_rolling_variability <- function(
  time,
  value,
  width,
  alignment,
  epoch
) {
  n <- length(time)
  # Fill preallocated vectors, then assemble once. Assigning individual cells
  # through a data frame repeatedly copies its columns during the rolling loop.
  window_sd <- rep(NA_real_, n)
  window_mean <- rep(NA_real_, n)
  coverage <- rep(0, n)
  observations <- integer(n)
  window_first <- integer(n)
  window_last <- integer(n)
  duplicate <- rep(FALSE, n)
  if (!n || !is.finite(epoch) || epoch <= 0)
    return(data.frame(
      statistic = window_sd,
      mean = window_mean,
      coverage = coverage,
      observations = observations,
      first = window_first,
      last = window_last,
      duplicate = duplicate
    ))
  # Both forms include the current observation. Centred windows are [a, b);
  # trailing windows are (a, b], with b at the current timestamp.
  lower <- time - if (alignment == "centered") width / 2 else width
  upper <- time + if (alignment == "centered") width / 2 else 0
  finite <- is.finite(value)
  coverage <- preparation_window_coverage(
    time,
    finite,
    epoch,
    lower,
    upper
  )
  duplicates <- duplicated(time) | duplicated(time, fromLast = TRUE)
  duplicate_count <- c(0L, cumsum(duplicates))
  finite_count <- c(0L, cumsum(finite))
  observed <- value[finite]
  changes <- cumsum(c(FALSE, diff(observed) != 0))
  offset <- if (any(finite)) value[which(finite)[[1L]]] else 0
  left <- 1L
  right <- 0L
  count <- 0L
  mean <- 0
  m2 <- 0
  for (i in seq_len(n)) {
    rebase <- FALSE
    while (
      right < n &&
        if (alignment == "centered") time[right + 1L] < upper[[i]] else
          time[right + 1L] <= upper[[i]]
    ) {
      right <- right + 1L
      if (finite[[right]]) {
        count <- count + 1L
        x <- value[[right]] - offset
        delta <- x - mean
        mean <- mean + delta / count
        m2 <- m2 + delta * (x - mean)
      }
    }
    while (
      left <= right &&
        if (alignment == "centered") time[[left]] < lower[[i]] else
          time[[left]] <= lower[[i]]
    ) {
      if (finite[[left]]) {
        if (count <= 1L) {
          count <- 0L
          mean <- 0
          m2 <- 0
        } else {
          x <- value[[left]] - offset
          next_mean <- mean - (x - mean) / (count - 1L)
          old_m2 <- m2
          m2 <- m2 - (x - mean) * (x - next_mean)
          if (
            !is.finite(m2) ||
              m2 < 0 ||
              m2 < sqrt(.Machine$double.eps) * abs(old_m2)
          )
            rebase <- TRUE
          mean <- next_mean
          count <- count - 1L
        }
      }
      left <- left + 1L
    }
    observations[[i]] <- count
    window_first[[i]] <- left
    window_last[[i]] <- right
    duplicate[[i]] <- duplicate_count[[right + 1L]] >
      duplicate_count[[left]]
    if (count >= 2L) {
      first <- finite_count[[left]] + 1L
      last <- finite_count[[right + 1L]]
      # Exact equality, not a numerical tolerance, establishes constant data.
      # This also resets accumulated removal roundoff without erasing small SD.
      if (changes[[first]] == changes[[last]]) {
        offset <- observed[[first]]
        mean <- 0
        m2 <- 0
      } else if (rebase || !is.finite(m2) || m2 < 0) {
        # Cancellation triggers a fresh calculation using the original readings
        # and a new local origin. It never forces a small variance to zero.
        x <- value[seq.int(left, right)]
        x <- x[is.finite(x)]
        offset <- x[[1L]]
        x <- x - offset
        mean <- base::mean(x)
        m2 <- sum((x - mean)^2)
      }
      window_sd[[i]] <- sqrt(m2 / (count - 1L))
      window_mean[[i]] <- offset + mean
      if (
        is.finite(window_sd[[i]]) &&
          window_sd[[i]] > 0 &&
          is.finite(window_mean[[i]]) &&
          abs(window_mean[[i]]) <=
            sqrt(.Machine$double.eps) * window_sd[[i]]
      ) {
        # CV is undefined at an exactly zero mean. Recompute a cancellation-
        # sensitive mean from its actual readings instead of rounding it to zero.
        x <- value[seq.int(left, right)]
        window_mean[[i]] <- base::mean(x[is.finite(x)])
      }
    }
  }
  data.frame(
    statistic = window_sd,
    mean = window_mean,
    coverage = coverage,
    observations = observations,
    first = window_first,
    last = window_last,
    duplicate = duplicate
  )
}

preparation_mask_variability <- function(data, args, reference) {
  if (
    !all(
      c(args$variable, args$source) %in% preparation_measurement_columns(data)
    )
  )
    abort_llw(
      "Choose numeric target and source measurements available at this step.",
      "preparation"
    )
  if (!is.null(args$output) && args$output %in% names(data))
    abort_llw(
      "The output variable already exists. Choose a new name.",
      "preparation"
    )
  grouped <- dplyr::group_by(
    data,
    dplyr::across(dplyr::all_of(reference$columns))
  )
  keys <- dplyr::left_join(
    dplyr::group_keys(grouped),
    reference$keys,
    by = reference$columns
  )
  rows <- dplyr::group_rows(grouped)
  diagnostics <- data.frame(
    statistic = rep(NA_real_, nrow(data)),
    coverage = rep(0, nrow(data)),
    evaluable = rep(FALSE, nrow(data)),
    reason = rep("insufficient data", nrow(data))
  )
  width <- args$length *
    switch(args$unit, seconds = 1, minutes = 60, hours = 3600)
  for (g in seq_along(rows)) {
    index <- rows[[g]]
    index <- index[order(data$Datetime[index], na.last = TRUE)]
    time <- as.numeric(data$Datetime[index])
    valid <- is.finite(time)
    index <- index[valid]
    time <- time[valid]
    epoch <- keys$.llw_variability_epoch[[g]]
    rolling <- preparation_rolling_variability(
      time,
      data[[args$source]][index],
      width,
      args$alignment,
      epoch
    )
    statistic <- rolling$statistic
    zero_mean <- args$statistic == "cv" &
      (!is.finite(rolling$mean) | rolling$mean == 0)
    if (args$statistic == "cv") statistic <- 100 * statistic / abs(rolling$mean)
    # Re-evaluate only comparisons at machine precision of the cutoff. The
    # tolerance selects a direct check, never changes above/below semantics.
    borderline <- which(
      is.finite(statistic) &
        statistic > 0 &
        abs(statistic - args$threshold) <=
          64 * .Machine$double.eps * pmax(abs(statistic), abs(args$threshold)) &
        rolling$observations >= 2L &
        !rolling$duplicate &
        rolling$coverage + 1e-8 >= args$coverage
    )
    previous_bounds <- integer()
    checked <- NA_real_
    for (i in borderline) {
      bounds <- c(rolling$first[[i]], rolling$last[[i]])
      if (!identical(bounds, previous_bounds)) {
        x <- data[[args$source]][index[seq.int(bounds[[1L]], bounds[[2L]])]]
        x <- x[is.finite(x)]
        checked <- stats::sd(x)
        if (args$statistic == "cv")
          checked <- 100 * checked / abs(base::mean(x))
        previous_bounds <- bounds
      }
      statistic[[i]] <- checked
    }
    reason <- rep("assessed", length(index))
    reason[!is.finite(statistic)] <- "nonfinite statistic"
    reason[zero_mean] <- "undefined CV"
    reason[rolling$observations < 2L] <- "fewer than two finite observations"
    reason[rolling$coverage + 1e-8 < args$coverage] <- "insufficient coverage"
    reason[rolling$duplicate] <- "duplicate timestamps"
    if (!is.finite(epoch) || epoch <= 0) reason[] <- "unknown sampling interval"
    diagnostics$statistic[index] <- statistic
    diagnostics$coverage[index] <- rolling$coverage
    diagnostics$evaluable[index] <- reason == "assessed"
    diagnostics$reason[index] <- reason
  }
  hit <- diagnostics$evaluable &
    if (args$direction == "above") diagnostics$statistic > args$threshold else
      diagnostics$statistic < args$threshold
  hit[is.na(hit)] <- FALSE
  if (identical(args$unevaluable, "missing"))
    hit <- hit | !diagnostics$evaluable
  value <- data[[args$variable]]
  value[hit] <- NA
  data[[args$output %||% args$variable]] <- value
  list(data = data, diagnostics = diagnostics)
}
