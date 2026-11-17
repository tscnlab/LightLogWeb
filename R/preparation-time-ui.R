preparation_time_ui <- function(ns) {
  shiny::tagList(
    preparation_choice_buttons(
      ns("sampling_type"),
      "Adjust time and sampling",
      c("Time zone", "Device DST", "Gaps", "Aggregate"),
      c("timezone", "dst", "gaps", "aggregate"),
      c("globe", "clock", "grip-lines", "layer-group"),
      "timezone"
    ),
    shiny::conditionalPanel(
      "input.sampling_type === 'aggregate'",
      ns = ns,
      shiny::tags$div(
        class = "llw-prepare-field-pair",
        shiny::selectInput(
          ns("aggregate_interval"),
          "Output interval",
          preparation_aggregation_intervals(),
          selected = "15 minutes",
          width = "100%"
        ),
        shiny::selectInput(
          ns("aggregate_numeric"),
          "Numeric summary",
          c(
            "Mean" = "mean",
            "Median" = "median",
            "Minimum" = "min",
            "Maximum" = "max",
            "Sum" = "sum"
          ),
          width = "100%"
        )
      ),
      shiny::selectInput(
        ns("aggregate_missing"),
        "Missing readings (all summary types)",
        c(
          "Any NA makes the bin NA" = "propagate",
          "Use available readings; all missing stays NA" = "available",
          "Set NA above a missingness limit" = "threshold"
        ),
        width = "100%"
      ),
      shiny::conditionalPanel(
        "input.aggregate_missing === 'threshold'",
        ns = ns,
        shiny::numericInput(
          ns("aggregate_max_missing"),
          "Maximum missing readings per bin (%)",
          value = 20,
          min = 0,
          max = 100,
          step = 1,
          width = "100%"
        ),
        shiny::tags$p(
          class = "llw-prepare-hint",
          "For each numeric, categorical or logical variable, set its summary to NA when its missing readings exceed this limit. At the limit, use available readings. All missing stays NA."
        ),
        shiny::tags$p(
          class = "llw-prepare-hint",
          shiny::tags$strong("Absent timestamps are not counted. "),
          "Add a Gaps step before Aggregate to include them as missing readings. Choose complete local days in Gaps if recording-edge gaps must count too."
        )
      ),
      preparation_help(
        "Other data types",
        shiny::tags$div(
          class = "llw-prepare-field-pair",
          shiny::selectInput(
            ns("aggregate_categorical"),
            "Categories (text / factor)",
            c(
              "Most frequent" = "mode",
              "First reading" = "first",
              "Last reading" = "last"
            ),
            width = "100%"
          ),
          shiny::selectInput(
            ns("aggregate_logical"),
            "Logical (TRUE / FALSE)",
            c("Majority" = "majority", "Any TRUE" = "any", "All TRUE" = "all"),
            width = "100%"
          )
        ),
        shiny::tags$p(
          class = "llw-prepare-hint",
          "The missing-readings rule applies separately to each numeric, categorical and logical variable. When available readings are allowed, first/last skip NA and use timestamp order, retaining row order for ties. Most frequent uses LightLogR's tie rule: the first table category wins. A logical majority tie is TRUE. All-missing summaries stay NA."
        )
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        "Summarise readings within each current group and participant/file recording. Bins start on clock boundaries and include their start, but exclude their end. Choose an interval at least as long as the sampling interval entering this step."
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        "Time coverage is reported separately and does not discard an aggregate. Empty intervals stay absent. Use Gaps explicitly if you need missing rows."
      ),
      preparation_help(
        "Coverage and aggregation details",
        shiny::tags$p(
          "Coverage uses finite readings entering this step. Each timestamp represents at most one entering sampling interval, capped at the next timestamp and the bin boundary. Duplicate timestamps count once; missing values and gaps contribute no observed time. An unknown sampling interval gives unavailable coverage."
        ),
        shiny::tags$p(
          "The denominator is the full bin, including partial first and last bins. Local days have 23 or 25 hours at daylight-saving changes. Coverage is descriptive; means and medians are calculated from readings, without duration weighting."
        ),
        shiny::tags$p(
          "Duration columns follow the selected numeric summary and missing-value policy, retaining their duration type. Other datetime and time columns use their default means. Current grouping and recording identifiers remain unchanged. Unsupported column types stop the preview. The is.implicit marker is TRUE only when every contributing row was a synthetic gap row."
        ),
        shiny::tags$p(
          "Each aggregation uses the rows entering that step. After an earlier aggregation, coverage uses the actual duration of available input bins, not the original readings within them. If a time correction shifts those bins, coverage is unavailable. Sequential means give each input bin equal weight. Every preview starts from the original source."
        )
      )
    ),
    shiny::conditionalPanel(
      "input.sampling_type === 'timezone'",
      ns = ns,
      shiny::tags$div(
        class = "llw-prepare-field-pair llw-prepare-zone-pair",
        shiny::tags$dl(
          class = "llw-prepare-zone-current",
          shiny::tags$dt(
            class = "control-label",
            "Time zone entering this step"
          ),
          htmltools::tagAppendAttributes(
            shiny::textOutput(
              ns("time_entering_timezone"),
              container = shiny::tags$dd
            ),
            class = "form-control"
          )
        ),
        shiny::selectizeInput(
          ns("time_timezone"),
          "Change to",
          preparation_timezones(),
          selected = "UTC",
          width = "100%"
        )
      ),
      shiny::selectInput(
        ns("time_method"),
        "Change timestamps by",
        c(
          "Keep the instant; change the clock (with_tz)" = "with",
          "Keep the clock; change the instant (force_tz)" = "force"
        ),
        width = "100%"
      ),
      shiny::conditionalPanel(
        "input.time_method === 'with'",
        ns = ns,
        shiny::tags$div(
          class = "llw-prepare-zone-summary llw-prepare-hint",
          shiny::tags$p(
            "Shows the same moments in the selected time zone. Local clock times and dates may change; the time between readings stays the same."
          ),
          shiny::tags$p(
            "Daylight saving: the clock difference follows both zones' rules for each date. The displayed clock may skip forward or repeat, but no readings are added or removed."
          )
        )
      ),
      shiny::conditionalPanel(
        "input.time_method === 'force'",
        ns = ns,
        shiny::selectInput(
          ns("time_ambiguous"),
          "Repeated local times",
          c(
            "Stop and ask me to resolve them" = "error",
            "Use the earlier occurrence" = "earlier",
            "Use the later occurrence" = "later"
          ),
          width = "100%"
        ),
        shiny::tags$div(
          class = "llw-prepare-zone-summary llw-prepare-hint",
          shiny::tags$p(
            "Keeps each local date and clock time, but reads it in the selected time zone. This can shift readings to different moments in time."
          ),
          shiny::tags$p(
            "Daylight saving: the time shift can vary by date. Skipped local times stop the preview. Repeated local times use your choice above: stop, use the earlier occurrence, or use the later one."
          )
        )
      )
    ),
    shiny::conditionalPanel(
      "input.sampling_type === 'dst'",
      ns = ns,
      shiny::tags$p(
        "Correct a device that kept the UTC offset from the start of its recording."
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        "Use only when the device did not adjust for daylight saving. The first timestamp in each current group and participant/file recording is assumed correct. Each later timestamp is shifted by its change in UTC offset."
      ),
      shiny::uiOutput(ns("time_diagnostics")),
      preparation_help(
        "About DST diagnosis",
        shiny::tags$p(
          "An offset transition shows that the recording spans a clock change. It does not establish a device error. Already-correct timestamps need no correction. Missing or misparsed timestamps must be resolved at import."
        )
      )
    ),
    shiny::conditionalPanel(
      "input.sampling_type === 'gaps'",
      ns = ns,
      shiny::selectInput(
        ns("gap_epoch"),
        "Sampling interval",
        c(
          "Dominant interval in each recording" = "dominant",
          "Choose an interval" = "custom"
        ),
        width = "100%"
      ),
      shiny::conditionalPanel(
        "input.gap_epoch === 'custom'",
        ns = ns,
        shiny::numericInput(
          ns("gap_seconds"),
          "Interval (seconds)",
          60,
          min = 1,
          max = 86400
        )
      ),
      shiny::selectInput(
        ns("gap_bounds"),
        "Expand within",
        c(
          "First to last timestamp in each recording" = "recording",
          "Complete local days" = "days"
        ),
        width = "100%"
      ),
      shiny::tags$p(
        class = "llw-prepare-hint",
        "Add missing timestamps separately within current groups and participant/file recordings. Added rows have missing measurements and is.implicit = TRUE. Existing readings, including off-grid and duplicate timestamps, remain unchanged."
      ),
      preparation_help(
        "Expansion and resource limits",
        shiny::tags$p(
          "The grid is anchored to the first timestamp, at the chosen interval. Complete days extend that grid towards the local day boundaries. Single-observation recordings are kept without expansion. Row and working-memory estimates are checked before allocation; previews exceeding the session budget or 5 million rows stop. Aggregation is a later step."
        )
      )
    )
  )
}
